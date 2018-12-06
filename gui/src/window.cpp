/****************************************************************
 * Copyright 2018 Trenton Schulz
 *
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 ******************************************************************/

#include "window.h"
#include <QtCore/QSettings>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QPushButton>
#include <ros/console.h>
#include <algorithm>

static const char* const Experiment_Log_Name = "Experiment";

Window::Window(QWidget* parent)
  : QWidget(parent)
  , locations_({ QString(QLatin1String("kitchen1")), QString(QLatin1String("kitchen2")),
		 QString(QLatin1String("dining_table1")), QString(QLatin1String("dining_table2")),
		 QString(QLatin1String("sofa1")), QString(QLatin1String("sofa2")) })
  , current_curve_index_(-1)
  , robot_path_index_(-1)
  , nodeHandle_(ros::NodeHandle())
  , fetch_controller_(this)
{
  next_locations_.reserve(2); // Most of the time, it will be two items max.
  buildPath();
  setupUi();
  connect(&fetch_controller_, SIGNAL(moveFinished()), this, SLOT(moveFinished()));
  connect(&fetch_controller_, SIGNAL(torsoFinished()), this, SLOT(torsoFinished()));
  connect(&fetch_controller_, SIGNAL(stopFinished()), this, SLOT(stopFinished()));
  connect(&fetch_controller_, SIGNAL(velocityCurveChanged()), this, SLOT(velocityCurveChanged()));
  // Make sure that we have info level, since we are dependent on that for logging.
  if (ros::console::set_logger_level(ROSCONSOLE_DEFAULT_NAME, ros::console::levels::Info))
  {
    ros::console::notifyLoggerLevelsChanged();
  }
  // Read the settings and sync the GUI
  readSettings();
  checkLockButtons();
}

static QLabel* createOrderingLabel()
{
  QLabel* label = new QLabel(Window::tr("-"));
  label->setAlignment(Qt::AlignCenter);
  auto font = label->font();
  font.setWeight(QFont::Bold);
  const QFontMetrics fm(font);
  const auto sisoWidth = fm.width(Window::tr("Slow in, Slow Out"));
  label->setMinimumWidth(sisoWidth);
  return label;
}

static const int TorsoUpId = 1;
static const int TorsoDownId = 2;
static const double TorsoUpHeight = 0.10;
static const double TorsoDownHeight = 0.00;

static QPushButton* createLocationButton(const QString& buttonText, QButtonGroup* buttonGroup, int id = -1)
{
  QPushButton* button = new QPushButton(buttonText);
  button->setCheckable(true);
  buttonGroup->addButton(button, id);
  return button;
}

void Window::setupUi()
{
  auto idLabel = new QLabel(tr("ID:"));
  id_line_edit_ = new QLineEdit();
  connect(id_line_edit_, SIGNAL(editingFinished()), this, SLOT(checkLockButtons()));
  connect(id_line_edit_, SIGNAL(editingFinished()), this, SLOT(logIdChanged()));

  button_group_ = new QButtonGroup(this);
  button_group_->setExclusive(true);
  torso_button_group_ = new QButtonGroup(this);
  torso_button_group_->setExclusive(true);
  ordering_label_1_ = createOrderingLabel();
  ordering_label_2_ = createOrderingLabel();
  ordering_label_3_ = createOrderingLabel();
  ordering_label_4_ = createOrderingLabel();
  kitchen_1_button_ = createLocationButton(locationToUser(locations_.at(0)), button_group_);
  kitchen_2_button_ = createLocationButton(locationToUser(locations_.at(1)), button_group_);
  dining_table_1_button_ = createLocationButton(locationToUser(locations_.at(2)), button_group_);
  dining_table_2_button_ = createLocationButton(locationToUser(locations_.at(3)), button_group_);
  sofa_1_button_ = createLocationButton(locationToUser(locations_.at(4)), button_group_);
  sofa_2_button_ = createLocationButton(locationToUser(locations_.at(5)), button_group_);
  torso_up_button_ = createLocationButton(tr("Torso Up"), torso_button_group_, TorsoUpId);
  torso_down_button_ = createLocationButton(tr("Torso Down"), torso_button_group_, TorsoDownId);
  connect(button_group_, SIGNAL(buttonClicked(int)), SLOT(locationClicked(int)));
  connect(torso_button_group_, SIGNAL(buttonClicked(int)), SLOT(changeTorso(int)));

  emergency_stop_button_ = new QPushButton(tr("Emergency &Stop"));
  auto widget_font = emergency_stop_button_->font();
  widget_font.setWeight(QFont::Bold);
  emergency_stop_button_->setFont(widget_font);
  emergency_stop_button_->setSizePolicy(QSizePolicy(QSizePolicy::Minimum, QSizePolicy::MinimumExpanding));
  emergency_stop_button_->setMinimumWidth(emergency_stop_button_->sizeHint().width());
  connect(emergency_stop_button_, SIGNAL(clicked()), SLOT(emergencyStop()));

  next_curve_button_ = new QPushButton();
  lockNextCurveButton(true);
  next_curve_button_->setMinimumWidth(next_curve_button_->sizeHint().width());
  next_curve_button_->setSizePolicy(QSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Maximum));
  connect(next_curve_button_, SIGNAL(clicked()), this, SLOT(advanceToNextCurve()));

  next_path_button_ = new QPushButton(tr("Next Stop"));
  next_path_button_->setSizePolicy(QSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Maximum));
  connect(next_path_button_, SIGNAL(clicked()), this, SLOT(advanceToNextStop()));

  buildPathLabels();

  static const int IdRow = 0;
  static const int CurveRow = IdRow + 1;
  static const int PathRow = CurveRow + 1;
  static const int PathButtonRow = PathRow + 1;
  static const int SpacerRow = PathButtonRow + 1;
  static const int Location1Row = SpacerRow + 2;
  static const int Location2Row = Location1Row + 1;

  auto* idLayout = new QHBoxLayout();
  idLayout->addWidget(idLabel);
  idLayout->addWidget(id_line_edit_);

  auto* orderingLayout = new QHBoxLayout();
  orderingLayout->addWidget(ordering_label_1_);
  orderingLayout->addWidget(ordering_label_2_);
  orderingLayout->addWidget(ordering_label_3_);
  orderingLayout->addWidget(ordering_label_4_);

  auto* layout = new QGridLayout();
  layout->addLayout(idLayout, IdRow, 0, 1, 3);
  layout->addLayout(orderingLayout, IdRow, 4, 1, -1);
  layout->addWidget(next_curve_button_, CurveRow, 6, 1, -1);

  auto *pathLayout = new QHBoxLayout();
  for (auto *label : path_labels_) {
    pathLayout->addWidget(label);
  }
  layout->addLayout(pathLayout, PathRow, 0, 1, 10);
  layout->addWidget(next_path_button_, PathButtonRow, 0, 1, 3);

  // Add some space
  layout->addWidget(new QLabel(), SpacerRow, 0, 1, -1);
  layout->setRowStretch(SpacerRow, 1);

  layout->addWidget(kitchen_1_button_, Location1Row, 0);
  layout->addWidget(kitchen_2_button_, Location2Row, 0);
  layout->addWidget(dining_table_1_button_, Location1Row, 1);
  layout->addWidget(dining_table_2_button_, Location2Row, 1);
  layout->addWidget(sofa_1_button_, Location1Row, 2);
  layout->addWidget(sofa_2_button_, Location2Row, 2);
  layout->addWidget(torso_up_button_, Location1Row, 3);
  layout->addWidget(torso_down_button_, Location2Row, 3);
  layout->addWidget(emergency_stop_button_, Location1Row, 4, -1, -1);
  setLayout(layout);
}

class WriteSettingsArrayWrapper
{
public:
  WriteSettingsArrayWrapper(QSettings* settings, const QString& arrayName) : m_settings(settings)
  {
    m_settings->beginWriteArray(arrayName);
  }
  ~WriteSettingsArrayWrapper()
  {
    m_settings->endArray();
  }

private:
  QSettings* m_settings;
};

class ReadSettingsArrayWrapper
{
public:
  ReadSettingsArrayWrapper(QSettings* settings, const QString& arrayName) : m_settings(settings)
  {
    m_settings->beginReadArray(arrayName);
  }
  ~ReadSettingsArrayWrapper()
  {
    m_settings->endArray();
  }

private:
  QSettings* m_settings;
};

void Window::readSettings()
{
  QSettings settings;
  current_curves_.clear();
  current_curves_.reserve(4);

  current_id_ = settings.value(QLatin1String("current_id"), QString()).toString();
  current_curve_index_ = settings.value(QLatin1String("current_curve_index"), -1).toInt();
  if (current_curve_index_ >= 0)
  {
    ReadSettingsArrayWrapper arrayStart(&settings, QLatin1String("current_curves"));
    const int curves_size = current_curves_.capacity();
    for (int index = 0; index < curves_size; ++index)
    {
      settings.setArrayIndex(index);
      current_curves_.push_back(settings.value(QLatin1String("current_curve_element"), -1).toInt());
    }
  }
  id_line_edit_->setText(current_id_);
  if (!current_curves_.empty())
  {
    syncLabelsToCurves();
    advanceToCurve();
  }

  robot_path_index_ = settings.value(QLatin1String("robot_path_index"), 0).toInt();
  if (robot_path_index_ >= 0) {
    syncPath();
  }
}

void Window::writeSettings()
{
  QSettings settings;
  settings.setValue(QLatin1String("current_id"), current_id_);
  settings.setValue(QLatin1String("current_curve_index"), current_curve_index_);
  if (current_curve_index_ >= 0)
  {
    const int curves_size = current_curves_.size();
    WriteSettingsArrayWrapper arrayStart(&settings, QLatin1String("current_curves"));
    for (int index = 0; index < curves_size; ++index)
    {
      settings.setArrayIndex(index);
      settings.setValue(QLatin1String("current_curve_element"), current_curves_.at(index));
    }
  }
  settings.setValue(QLatin1String("robot_path_index"), robot_path_index_);
}

Window::~Window()
{
  writeSettings();
}

void Window::disableLocationButtons(bool disable)
{
  for (const auto& button : button_group_->buttons())
  {
    button->setDisabled(disable);
  }
  next_path_button_->setDisabled(disable);
  for (const auto &button : torso_button_group_->buttons())
  {
    button->setDisabled(disable);
  }
}

void Window::torsoFinished()
{
  move_stopwatch_.start();
  if (!next_locations_.empty())
  {
    fetch_controller_.travelToLocations(next_locations_);
  } else {
    // No movements about to happen, so re-enable the buttons.
    disableLocationButtons(false);
  }
}

void Window::locationClicked(int negative_id)
{
  disableLocationButtons(true);
  next_locations_.push_back(locationForButtonId(negative_id));
  fetch_controller_.moveTorso(TorsoDownHeight);
}

void Window::emergencyStop()
{
  emergency_stop_button_->setText(tr("Stopping…"));
  emergency_stop_button_->setDisabled(true);
  fetch_controller_.emergencyStop();
}

static QString textForVariable(int i)
{
  return (i == 1) ? Window::tr("Slow in, Slow out") : Window::tr("Linear");
}

void Window::velocityCurveChanged()
{
  lockNextCurveButton(current_curve_index_ == current_curves_.size() - 1);
  const auto& variableName = textForVariable(current_curves_.at(current_curve_index_));

  ROS_INFO_NAMED(Experiment_Log_Name, "ID %s now using %s curve (index %d)", current_id_.toUtf8().constData(),
		 variableName.toUtf8().constData(), current_curve_index_);
}

static int maxVariableNameLength()
{
  const auto& s0 = textForVariable(0);
  const auto& s1 = textForVariable(1);
  return std::max(s0.toUtf8().size(), s1.toUtf8().size());
}

void Window::newVariables()
{
  current_curve_index_ = 0;
  current_curves_ = order_generator_.newOrder();
  syncLabelsToCurves();
}

void Window::syncLabelsToCurves()
{
  const std::vector<QLabel*> labels({
      ordering_label_1_, ordering_label_2_, ordering_label_3_, ordering_label_4_,
  });
  Q_ASSERT(labels.size() == current_curves_.size());
  const int TotalVariables = labels.size();
  // Build a long enough string
  std::string logString;
  logString.reserve(4 * maxVariableNameLength() + 6);

  for (int place = 0; place < TotalVariables; ++place)
  {
    const auto& name = textForVariable(current_curves_.at(place));
    labels.at(place)->setText(name);
    logString.append(name.toStdString());
    if (place != TotalVariables - 1)
    {
      logString.append(", ");
    }
  }
  ROS_INFO_NAMED(Experiment_Log_Name, "ID: %s has new variables: %s", current_id_.toUtf8().constData(),
		 logString.c_str());
}

void Window::syncLabelsToIndex()
{
  const std::vector<QLabel*> labels({
      ordering_label_1_, ordering_label_2_, ordering_label_3_, ordering_label_4_,
  });
  const int TotalVariables = labels.size();
  for (int index = 0; index < TotalVariables; ++index)
  {
    auto* label = labels.at(index);
    auto font = label->font();
    font.setWeight(index == current_curve_index_ ? QFont::Bold : QFont::Normal);
    label->setFont(font);
  }
}

QString Window::locationForButtonId(int buttonId) const
{
  const auto id = abs(buttonId) - 2;  // automatic numbering starts at -2
  Q_ASSERT_X(id >= 0 && id < locations_.size(), "locationClicked", "index out of range");
  return locations_.at(id);
}

QString Window::locationToUser(const QString& location) const
{
  QString returnString;
  if (location == locations_.at(0))
  {
    returnString = tr("Kitchen 1");
  }
  else if (location == locations_.at(1))
  {
    returnString = tr("Kitchen 2");
  }
  else if (location == locations_.at(2))
  {
    returnString = tr("Dining Table 1");
  }
  else if (location == locations_.at(3))
  {
    returnString = tr("Dining Table 2");
  }
  else if (location == locations_.at(4))
  {
    returnString = tr("Sofa 1");
  }
  else if (location == locations_.at(5))
  {
    returnString = tr("Sofa 2");
  }
  else
  {
    returnString = tr("Unknown");
  }
  return returnString;
}

void Window::advanceToNextCurve()
{
  ++current_curve_index_;
  if (current_curve_index_ >= current_curves_.size())
  {
    newVariables();
  }
  advanceToCurve();
}

void Window::advanceToNextStop()
{
  disableLocationButtons(true);
  ++robot_path_index_;
  if (robot_path_index_ >= robot_path_.size()) {
    robot_path_index_ = 0;
  }
  advancePath();
}

void Window::advancePath()
{
  const auto &nextStep = robot_path_.at(robot_path_index_);
  const auto listSteps = nextStep.split(QLatin1Char(','));
  for (const auto &step : listSteps) {
    next_locations_.push_back(step);
  }
  fetch_controller_.moveTorso(TorsoDownHeight);
}

void Window::advanceToCurve()
{
  const QString new_curve = QLatin1String(current_curves_.at(current_curve_index_) == 0 ? "linear" : "siso");

  next_curve_button_->setDisabled(true);
  fetch_controller_.changeVelocityCurve(new_curve);
  syncLabelsToIndex();
}

void Window::moveFinished()
{
  next_locations_.clear();
  ROS_INFO_NAMED(Experiment_Log_Name, "Move finished time to complete: %d ms", move_stopwatch_.elapsed());
  fetch_controller_.moveTorso(TorsoUpHeight);
  syncPath();
}

void Window::stopFinished()
{
  emergency_stop_button_->setText(tr("Emergency Stop"));
  emergency_stop_button_->setDisabled(false);
}

void Window::checkLockButtons()
{
  const auto& editText = id_line_edit_->text();
  if (current_id_ != editText)
  {
    // Turn off everything unless we have a basic id.
    const auto lockDown = id_line_edit_->text().isEmpty();
    disableLocationButtons(lockDown);
    lockNextCurveButton(lockDown);
  }
}

void Window::logIdChanged()
{
  const auto& editText = id_line_edit_->text();
  if (current_id_ != editText)
  {
    current_id_ = editText;
    ROS_INFO_NAMED(Experiment_Log_Name, "ID changed to %s", current_id_.toUtf8().constData());
    advanceToNextCurve();
  }
}

void Window::lockNextCurveButton(bool disable)
{
  next_curve_button_->setDisabled(disable);
  next_curve_button_->setText(disable ? tr("Enter new ID") : tr("Next Curve"));
}

void Window::buildPath()
{
  // Build our paths out of the locations we built.
  // I'm assuming we aren't change the order of them in the constructor.
  const int TotalStops = 11;
  const QString Comma(QLatin1Char(','));
  robot_path_.clear();
  robot_path_.reserve(TotalStops);
  robot_path_.push_back(locations_[1]);
  robot_path_.push_back(locations_[2]);
  QString twosteps;
  const auto &loc3 = locations_[3];
  const auto &loc4 = locations_[4];
  twosteps.reserve(loc3.size() + loc4.size() + 1);
  twosteps.append(loc3);
  twosteps.append(Comma);
  twosteps.append(loc4);
  robot_path_.push_back(twosteps);
  twosteps.clear();
  const auto &loc5 = locations_[5];
  const auto &loc0 = locations_[0];
  twosteps.append(loc5);
  twosteps.append(Comma);
  twosteps.append(loc0);
  robot_path_.push_back(twosteps);
  const auto &loc1 = locations_[1];
  const auto &loc2 = locations_[2];
  twosteps.clear();
  twosteps.append(loc1);
  twosteps.append(Comma);
  twosteps.append(loc2);
  robot_path_.push_back(twosteps);
  twosteps.clear();
  twosteps.append(loc3);
  twosteps.append(Comma);
  twosteps.append(loc0);
  robot_path_.push_back(twosteps);
  robot_path_.push_back(locations_[1]);
}

void Window::changeTorso(int torso_id)
{
  fetch_controller_.moveTorso(torso_id == TorsoUpId ? TorsoUpHeight : TorsoDownHeight);
}

void Window::buildPathLabels()
{
  path_labels_.clear();
  path_labels_.reserve(robot_path_.size());

  for (const auto &stop : robot_path_) {
    const auto stopList = stop.split(QLatin1Char(','));
    auto *label = new QLabel(locationToUser(stopList.back()));
    path_labels_.push_back(label);
  }
}

void Window::syncPath()
{
  int index = 0;
  for (auto *label : path_labels_) {
    auto font = label->font();
    font.setWeight(index == robot_path_index_ ? QFont::Bold : QFont::Normal);
    label->setFont(font);
    ++index;
  }
}
