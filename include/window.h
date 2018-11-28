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
#include <QtCore/QProcess>
#include <ros/ros.h>
#include <vector>
class QLabel;
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
  void pythonProcessStateChanged(QProcess::ProcessState newState) const;
  void stopProcessStateChanged(QProcess::ProcessState newState) const;
  void pythonProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);
  void stopProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);
  void emergencyStop();

private:
  void setupUi();
  void disableLocationButtons(bool disable);
  QButtonGroup *button_group_;
  QPushButton *kitchen_1_button_;
  QPushButton *kitchen_2_button_;
  QPushButton *dining_table_1_button_;
  QPushButton *dining_table_2_button_;
  QPushButton *sofa_1_button_;
  QPushButton *sofa_2_button_;
  QPushButton *emergency_stop_button_;
  QPushButton *next_location_button_;
  QLabel *ordering_label_1_;
  QLabel *ordering_label_2_;
  QLabel *ordering_label_3_;
  QLabel *ordering_label_4_;
  QLabel *current_location_label_;
  QProcess python_process_;
  QProcess stop_process_;
  std::vector<QString> locations_;
  ros::NodeHandle nodeHandle_;
};

#endif
