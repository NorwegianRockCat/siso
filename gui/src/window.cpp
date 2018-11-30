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
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QPushButton>
#include <QtCore/QDebug>
#include <algorithm>

Window::Window(QWidget* parent)
  : QWidget(parent)
  , locations_({ QString(QLatin1String("kitchen1")), QString(QLatin1String("kitchen2")),
                 QString(QLatin1String("dining_table1")), QString(QLatin1String("dining_table2")),
                 QString(QLatin1String("sofa1")), QString(QLatin1String("sofa2")) })
  , current_curve_index_(-1)
  , nodeHandle_(ros::NodeHandle())
  , fetch_controller_(this)
  , going_to_move(false)
{
  setupUi();
  connect(&fetch_controller_, SIGNAL(moveFinished()), this, SLOT(moveFinished()));
  connect(&fetch_controller_, SIGNAL(torsoFinished()), this, SLOT(torsoFinished()));
  connect(&fetch_controller_, SIGNAL(stopFinished()), this, SLOT(stopFinished()));
  connect(&fetch_controller_, SIGNAL(velocityCurveChanged()), this, SLOT(velocityCurveChanged()));
  advanceToNextCurve();
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

static QPushButton* createLocationButton(const QString& buttonText, QButtonGroup* buttonGroup)
{
  QPushButton* button = new QPushButton(buttonText);
  button->setCheckable(true);
  buttonGroup->addButton(button);
  return button;
}

void Window::setupUi()
{
  button_group_ = new QButtonGroup(this);
  button_group_->setExclusive(true);
  ordering_label_1_ = createOrderingLabel();
  ordering_label_2_ = createOrderingLabel();
  ordering_label_3_ = createOrderingLabel();
  ordering_label_4_ = createOrderingLabel();
  current_location_label_ = new QLabel(tr("Unknown"));
  current_location_label_->setAlignment(Qt::AlignCenter);
  kitchen_1_button_ = createLocationButton(locationToUser(locations_.at(0)), button_group_);
  kitchen_2_button_ = createLocationButton(locationToUser(locations_.at(1)), button_group_);
  dining_table_1_button_ = createLocationButton(locationToUser(locations_.at(2)), button_group_);
  dining_table_2_button_ = createLocationButton(locationToUser(locations_.at(3)), button_group_);
  sofa_1_button_ = createLocationButton(locationToUser(locations_.at(4)), button_group_);
  sofa_2_button_ = createLocationButton(locationToUser(locations_.at(5)), button_group_);

  connect(button_group_, SIGNAL(buttonClicked(int)), SLOT(locationClicked(int)));
  emergency_stop_button_ = new QPushButton(tr("Emergency &Stop"));
  auto button_font = emergency_stop_button_->font();
  button_font.setWeight(QFont::Bold);
  emergency_stop_button_->setFont(button_font);
  emergency_stop_button_->setSizePolicy(QSizePolicy(QSizePolicy::Minimum, QSizePolicy::MinimumExpanding));
  connect(emergency_stop_button_, SIGNAL(clicked()), SLOT(emergencyStop()));
  next_curve_button_ = new QPushButton(tr("Next Curve"));
  next_curve_button_->setSizePolicy(QSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Maximum));
  connect(next_curve_button_, SIGNAL(clicked()), this, SLOT(advanceToNextCurve()));

  auto* layout = new QGridLayout();
  layout->addWidget(ordering_label_1_, 0, 0);
  layout->addWidget(ordering_label_2_, 0, 1);
  layout->addWidget(ordering_label_3_, 0, 2);
  layout->addWidget(ordering_label_4_, 0, 3);
  layout->addWidget(kitchen_1_button_, 1, 0);
  layout->addWidget(kitchen_2_button_, 2, 0);
  layout->addWidget(dining_table_1_button_, 1, 1);
  layout->addWidget(dining_table_2_button_, 2, 1);
  layout->addWidget(sofa_1_button_, 1, 2);
  layout->addWidget(sofa_2_button_, 2, 2);
  layout->addWidget(emergency_stop_button_, 0, 4, -1, -1);
  layout->addWidget(next_curve_button_, 3, 0, 1, 2);
  auto label = new QLabel(tr("Location"));
  label->setAlignment(Qt::AlignCenter);
  button_font = label->font();
  button_font.setWeight(QFont::Bold);
  label->setFont(button_font);
  label->setFont(button_font);
  layout->addWidget(label, 3, 2);
  layout->addWidget(current_location_label_, 3, 3);
  setLayout(layout);
}

Window::~Window()
{
}

void Window::disableLocationButtons(bool disable)
{
  for (const auto& button : button_group_->buttons())
  {
    button->setDisabled(disable);
  }
}

void Window::torsoFinished()
{
  if (going_to_move) {
    const auto negative_id = button_group_->checkedId();
    const auto &location = locationForButtonId(negative_id);
    fetch_controller_.travelToLocation(location);
  }
}

void Window::locationClicked(int negative_id)
{
  disableLocationButtons(true);
  going_to_move = true;
  fetch_controller_.moveTorso(0.0);
}

void Window::emergencyStop()
{
  emergency_stop_button_->setText(tr("Stopping…"));
  emergency_stop_button_->setDisabled(true);
  fetch_controller_.emergencyStop();
}

void Window::velocityCurveChanged()
{
  next_curve_button_->setDisabled(false);
}

static QString textForVariable(int i)
{
  return (i == 1) ? Window::tr("Slow in, Slow out") : Window::tr("Linear");
}

void Window::newVariables()
{
  current_curve_index_ = 0;
  current_curves_ = order_generator_.newOrder();
  const std::vector<QLabel*> labels({
      ordering_label_1_, ordering_label_2_, ordering_label_3_, ordering_label_4_,
  });
  Q_ASSERT(labels.size() == current_curves_.size());
  const int TotalVariables = labels.size();
  for (int place = 0; place < TotalVariables; ++place)
  {
    labels.at(place)->setText(textForVariable(current_curves_.at(place)));
  }
  current_location_label_->setText(locationToUser("Start"));
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
  current_curve_index_++;
  if (current_curve_index_ >= current_curves_.size())
  {
    newVariables();
  }
  const QString new_curve = QLatin1String(current_curves_.at(current_curve_index_) == 0 ? "linear" : "siso");

  next_curve_button_->setDisabled(true);
  fetch_controller_.changeVelocityCurve(new_curve);
  syncLabelsToIndex();
}

void Window::moveFinished()
{
  disableLocationButtons(false);
  going_to_move = false;
  fetch_controller_.moveTorso(0.25);
  current_location_label_->setText(locationToUser(locationForButtonId(button_group_->checkedId())));
}

void Window::stopFinished()
{
  emergency_stop_button_->setText(tr("Emergency Stop"));
  emergency_stop_button_->setDisabled(false);
}
