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

#include "fetch_process_controller.h"
#include <QtCore/QDebug>

FetchProcessController::FetchProcessController(QObject *parent)
  : QObject(parent)
  , base_process_(this)
  , torso_process_(this)
  , stop_process_(this)
  , reconfigure_process_(this)
{
  connect(&base_process_, SIGNAL(stateChanged(QProcess::ProcessState)), this,
          SLOT(baseProcessStateChanged(QProcess::ProcessState)));
  connect(&torso_process_, SIGNAL(stateChanged(QProcess::ProcessState)), this,
          SLOT(baseProcessStateChanged(QProcess::ProcessState)));
  connect(&stop_process_, SIGNAL(stateChanged(QProcess::ProcessState)), this,
          SLOT(stopProcessStateChanged(QProcess::ProcessState)));
  connect(&base_process_, SIGNAL(finished(int, QProcess::ExitStatus)), this,
          SLOT(baseProcessFinished(int, QProcess::ExitStatus)));
  connect(&torso_process_, SIGNAL(finished(int, QProcess::ExitStatus)), this,
          SLOT(torsoProcessFinished(int, QProcess::ExitStatus)));
  connect(&stop_process_, SIGNAL(finished(int, QProcess::ExitStatus)), this,
          SLOT(stopProcessFinished(int, QProcess::ExitStatus)));
  connect(&reconfigure_process_, SIGNAL(finished(int, QProcess::ExitStatus)), this,
          SLOT(reconfigureProcessFinished(int, QProcess::ExitStatus)));
}

static void terminateIfRunning(QProcess &process)
{
  process.terminate();
  process.waitForFinished();
}

FetchProcessController::~FetchProcessController()
{
  // Let everything terminate without a cascade.
  // We are going down anyway, but we don't need to crash.
  QSignalBlocker blocker(this); 
  terminateIfRunning(torso_process_);
  terminateIfRunning(base_process_);
  terminateIfRunning(stop_process_);
  terminateIfRunning(reconfigure_process_);
}

void FetchProcessController::baseProcessStateChanged(QProcess::ProcessState newState) const
{
  qDebug() << "base process Got new state" << newState;
}

void FetchProcessController::torsoProcessStateChanged(QProcess::ProcessState newState) const
{
  qDebug() << "torso process Got new state" << newState;
}

void FetchProcessController::stopProcessStateChanged(QProcess::ProcessState newState) const
{
  qDebug() << "stop got new state" << newState;
}

void FetchProcessController::baseProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
  qDebug() << "Base finished" << exitCode << exitStatus;
  emit moveFinished();
}

void FetchProcessController::torsoProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
  qDebug() << "Torso finished" << exitCode << exitStatus;
  emit torsoFinished();
}

void FetchProcessController::stopProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
  qDebug() << "Stop finished" << exitCode << exitStatus;
  emit stopFinished();
}

void FetchProcessController::travelToLocation(const QString &location)
{
  Q_ASSERT_X(base_process_.state() == QProcess::NotRunning, "travelToLocation", "Process is still running");
  const auto command = QLatin1String("rosrun");
  const QStringList arguments(
    { QLatin1String("uh_robots"), QLatin1String("move_base.py"), QLatin1String("base"), QLatin1String("-n"), location });
  qDebug() << "Runnining" << command << arguments;
  base_process_.start(command, arguments);
}

void FetchProcessController::emergencyStop()
{
  Q_ASSERT_X(stop_process_.state() == QProcess::NotRunning, "emergencyStop", "Process is still running");
  const QString command(QLatin1String("rostopic"));
  const QStringList arguments({ QLatin1String("pub"), QLatin1String("-1"), QLatin1String("/enable_software_runstop"),
                                QLatin1String("std_msgs/Bool"), QLatin1String("data: true") });
  qDebug() << "Runnining" << command << arguments;
  stop_process_.start(command, arguments);
}

void FetchProcessController::moveTorso(double heightInMeters)
{
  const auto command = QLatin1String("rosrun");
  const QStringList arguments(
    { QLatin1String("uh_robots"), QLatin1String("move_base.py"), QLatin1String("torso"), QLatin1String("-p"),
      QString::number(heightInMeters) });
  qDebug() << "Runnining" << command << arguments;
  torso_process_.start(command, arguments);
}

void FetchProcessController::changeVelocityCurve(const QString& newCurve)
{
  Q_ASSERT_X(reconfigure_process_.state() == QProcess::NotRunning, "locationClicked", "Process is still running");
  const QString command = QLatin1String("rosrun");
  const QStringList arguments({QLatin1String("dynamic_reconfigure"),
			       QLatin1String("dynparam"),
			       QLatin1String("set"),
			       QLatin1String("/move_base/SisoLocalPlanner"),
			       QLatin1String("velocity_curve"),
			       newCurve});

  qDebug() << "Runnining" << command << arguments;
  reconfigure_process_.start(command, arguments);
			       
}


void FetchProcessController::reconfigureProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
  qDebug() << "Reconfigure finished" << exitCode << exitStatus;
  emit velocityCurveChanged();
}
