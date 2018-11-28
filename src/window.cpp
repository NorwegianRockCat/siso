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
#include <QtWidgets/QLabel>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QDoubleSpinBox>
#include <QtWidgets/QFormLayout>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QRadioButton>
#include <QtWidgets/QVBoxLayout>
#include <algorithm>

Window::Window(QWidget* parent)
  : QWidget(parent)
  , nodeHandle_(ros::NodeHandle())
{
  setupUi();
}

static QLabel *createOrderingLabel()
{
  QLabel *label = new QLabel(Window::tr("-"));
  label->setAlignment(Qt::AlignCenter);
  auto font = label->font();
  font.setWeight(QFont::Bold);
  const QFontMetrics fm(font);
  const auto sisoWidth = fm.width(Window::tr("Slow in, Slow Out"));
  label->setMinimumWidth(sisoWidth);
  return label;
}

static QPushButton *createLocationButton(const QString &buttonText, QButtonGroup *buttonGroup)
{
  QPushButton *button = new QPushButton(buttonText);
  button->setCheckable(true);
  buttonGroup->addButton(button);
  return button;
}

void Window::setupUi()
{
  auto *layout = new QGridLayout();
  auto *buttonGroup = new QButtonGroup(this);
  buttonGroup->setExclusive(true);
  ordering_label_1_ = createOrderingLabel();
  ordering_label_2_ = createOrderingLabel();
  ordering_label_3_ = createOrderingLabel();
  ordering_label_4_ = createOrderingLabel();
  current_location_label_ = new QLabel(tr("Unknown"));
  current_location_label_->setAlignment(Qt::AlignCenter);
  kitchen_1_button_ = createLocationButton(tr("Kitchen 1"), buttonGroup);
  kitchen_2_button_ = createLocationButton(tr("Kitchen 2"), buttonGroup);
  dining_table_1_button_ = createLocationButton(tr("Dining Table 1"), buttonGroup);
  dining_table_2_button_ = createLocationButton(tr("Dining Table 2"), buttonGroup);
  sofa_1_button_ = createLocationButton(tr("Sofa 1"), buttonGroup);
  sofa_2_button_ = createLocationButton(tr("Sofa 2"), buttonGroup);
  emergency_stop_button_ = new QPushButton(tr("Emergency &Stop"));
  auto button_font = emergency_stop_button_->font();
  button_font.setWeight(QFont::Bold);
  emergency_stop_button_->setFont(button_font);
  emergency_stop_button_->setSizePolicy(QSizePolicy(QSizePolicy::Minimum, QSizePolicy::MinimumExpanding));
  next_location_button_ = new QPushButton(tr("Next Location"));
  next_location_button_->setSizePolicy(QSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Maximum));
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
  layout->addWidget(next_location_button_, 3, 0, 1, 2);
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

