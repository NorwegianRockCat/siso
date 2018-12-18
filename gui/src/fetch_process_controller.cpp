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
#include <ros/console.h>

FetchProcessController::FetchProcessController(QObject* parent)
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

static void terminateIfRunning(QProcess& process)
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
  ROS_DEBUG("base process Got new state: %d", newState);
}

void FetchProcessController::torsoProcessStateChanged(QProcess::ProcessState newState) const
{
  ROS_DEBUG("torso process Got new state: %d", newState);
}

void FetchProcessController::stopProcessStateChanged(QProcess::ProcessState newState) const
{
  ROS_DEBUG("stop process Got new state: %d", newState);
}

void FetchProcessController::baseProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
  ROS_DEBUG("Base finished exit code: %d ExitStatus: %d", exitCode, exitStatus);
  if (!location_queue_.empty())
  {
    travelToNextStop();
  }
  else
  {
    emit moveFinished();
  }
}

void FetchProcessController::torsoProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
  ROS_DEBUG("Torso finished exit code: %d ExitStatus: %d", exitCode, exitStatus);
  emit torsoFinished();
}

void FetchProcessController::stopProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
  ROS_DEBUG("Stop finished exit code: %d ExitStatus: %d", exitCode, exitStatus);
  emit stopFinished();
}

void FetchProcessController::travelToLocation(const QString& location)
{
  Q_ASSERT_X(base_process_.state() == QProcess::NotRunning, "travelToLocation", "Process is still "
                                                                                "running");
  location_queue_.push_back(location);
  travelToNextStop();
}

void FetchProcessController::travelToLocations(const std::vector<QString>& locations)
{
  for (const auto& location : locations)
  {
    location_queue_.push_back(location);
  }
  travelToNextStop();
}

void FetchProcessController::travelToNextStop()
{
  Q_ASSERT_X(!location_queue_.empty(), "FetchProcesscontroller::travelToNextstop()",
             "location queue was empty, but we wanted to go to another location");
  const auto location = location_queue_.front();
  location_queue_.pop_front();
  const QString command(QStringLiteral(u"rosrun"));
  const QStringList arguments({ QStringLiteral(u"uh_robots"), QStringLiteral(u"move_base.py"),
                                QStringLiteral(u"base"), QStringLiteral(u"-n"), location,
                                QStringLiteral(u"--timeout"), QStringLiteral(u"60") });
  ROS_DEBUG("Running %s %s", command.toUtf8().constData(),
            arguments.join(QLatin1String(", ")).toUtf8().constData());
  base_process_.start(command, arguments);
}

void FetchProcessController::emergencyStop()
{
  Q_ASSERT_X(stop_process_.state() == QProcess::NotRunning, "emergencyStop", "Process is still "
                                                                             "running");
  const QString command(QStringLiteral(u"rostopic"));
  const QStringList arguments({ QStringLiteral(u"pub"), QStringLiteral(u"-1"),
                                QStringLiteral(u"/enable_software_runstop"),
                                QStringLiteral(u"std_msgs/Bool"), QStringLiteral(u"data: true") });
  ROS_DEBUG("Running %s %s", command.toUtf8().constData(),
            arguments.join(QLatin1String(", ")).toUtf8().constData());
  stop_process_.start(command, arguments);
}

void FetchProcessController::moveTorso(double heightInMeters)
{
  if (torso_process_.state() != QProcess::NotRunning)
  {
    ROS_DEBUG("Torso process still running, waiting for it to finish");
    torso_process_.waitForFinished();
  }
  const QString command(QStringLiteral(u"rosrun"));
  const QStringList arguments({ QStringLiteral(u"uh_robots"), QStringLiteral(u"move_base.py"),
                                QStringLiteral(u"torso"), QStringLiteral(u"-p"),
                                QString::number(heightInMeters), QStringLiteral(u"--timeout"),
                                QStringLiteral(u"5") });
  ROS_DEBUG("Running %s %s", command.toUtf8().constData(),
            arguments.join(QLatin1String(", ")).toUtf8().constData());
  torso_process_.start(command, arguments);
}

void FetchProcessController::changeVelocityCurve(const QString& newCurve)
{
  if (torso_process_.state() != QProcess::NotRunning)
  {
    ROS_DEBUG("base process still running, waiting for it to finish");
    torso_process_.waitForFinished();
  }
  const QString command(QStringLiteral(u"rosrun"));
  const QStringList arguments({ QStringLiteral(u"dynamic_reconfigure"), QStringLiteral(u"dynparam"),
                                QStringLiteral(u"set"),
                                QStringLiteral(u"/move_base/SisoLocalPlanner"),
                                QStringLiteral(u"velocity_curve"), newCurve });

  ROS_DEBUG("Running %s %s", command.toUtf8().constData(),
            arguments.join(QLatin1String(", ")).toUtf8().constData());
  reconfigure_process_.start(command, arguments);
}

void FetchProcessController::reconfigureProcessFinished(int exitCode,
                                                        QProcess::ExitStatus exitStatus)
{
  ROS_DEBUG("Reconfigure finished exit code: %d ExitStatus: %d", exitCode, exitStatus);
  emit velocityCurveChanged();
}
