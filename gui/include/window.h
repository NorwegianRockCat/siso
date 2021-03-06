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
// -*- mode: c++ -*-
#ifndef WINDOW_H
#define WINDOW_H

#include <QtWidgets/QWidget>
#include <QtCore/QTime>
#include <ros/ros.h>
#include <vector>
#include "fetch_process_controller.h"
#include "ordergenerator.h"

class QLabel;
class QLineEdit;
class QPushButton;
class QButtonGroup;

class Window : public QWidget
{
  Q_OBJECT
public:
  Window(QWidget* parent = 0);
  ~Window();

private slots:
  void locationClicked(int negative_id);
  void emergencyStop();
  void advanceToNextCurve();
  void advanceToNextStop();
  void newVariables();
  void moveFinished();
  void torsoFinished();
  void stopFinished();
  void velocityCurveChanged();
  void checkLockButtons();
  void logIdChanged();
  void changeTorso(int torso_id);

private:
  void setupUi();
  void buildPath();
  void buildPathLabels();
  void readSettings();
  void writeSettings();
  void disableLocationButtons(bool disable);
  void syncLabelsToCurves();
  void syncLabelsToIndex();
  void syncPath();
  void advanceToCurve();
  void advancePath();
  void lockNextCurveButton(bool disable);
  QString locationToUser(const QString &location) const;
  QString locationForButtonId(int buttonId) const;

  QLineEdit *id_line_edit_;
  QButtonGroup *button_group_;
  QPushButton *kitchen_1_button_;
  QPushButton *kitchen_2_button_;
  QPushButton *dining_table_1_button_;
  QPushButton *dining_table_2_button_;
  QPushButton *sofa_1_button_;
  QPushButton *sofa_2_button_;
  QPushButton *emergency_stop_button_;
  QPushButton *next_curve_button_;
  QPushButton *next_path_button_;
  QButtonGroup *torso_button_group_;
  QPushButton *torso_up_button_;
  QPushButton *torso_down_button_;
  QLabel *ordering_label_1_;
  QLabel *ordering_label_2_;
  QLabel *ordering_label_3_;
  QLabel *ordering_label_4_;
  std::vector<QString> locations_;
  std::vector<QString> robot_path_;
  std::vector<QLabel *> path_labels_;
  std::vector<int> current_curves_;
  uint current_curve_index_;
  uint robot_path_index_;
  OrderGenerator order_generator_;
  ros::NodeHandle nodeHandle_;
  FetchProcessController fetch_controller_;
  QString current_id_;
  std::vector<QString> next_locations_;
  QTime move_stopwatch_;
};

#endif
