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

#ifndef FETCHPROCESSCONTROLLER_H
#define FETCHPROCESSCONTROLLER_H
#include <QtCore/QProcess>

class FetchProcessController : public QObject
{
  Q_OBJECT
public:
  FetchProcessController(QObject* parent);
  ~FetchProcessController();

signals:
  void torsoFinished();
  void moveFinished();
  void stopFinished();
  void velocityCurveChanged();

public slots:
  void moveTorso(double heightInMeters);
  void travelToLocation(const QString& location);
  void emergencyStop();
  void changeVelocityCurve(const QString& newCurve);

private slots:
  void baseProcessStateChanged(QProcess::ProcessState newState) const;
  void stopProcessStateChanged(QProcess::ProcessState newState) const;
  void torsoProcessStateChanged(QProcess::ProcessState newState) const;
  void baseProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);
  void stopProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);
  void torsoProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);
  void reconfigureProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);

private:
  QProcess base_process_;
  QProcess torso_process_;
  QProcess stop_process_;
  QProcess reconfigure_process_;
};

#endif  // FETCHPROCESSCONTROLLER_H
