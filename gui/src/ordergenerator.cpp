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
#include "ordergenerator.h"
#include <chrono>
#include <functional>
#include <random>

#include <ros/console.h>
#include <QtCore/QFile>
#include <QtCore/QTextStream>
#include <QtCore/QDebug>
#include <QtCore/QCoreApplication>

OrderGenerator::OrderGenerator()
{
  /* Here are the possible orders for 2 siso and 2 linear
   * I'm adding them explicitly below so that I'm sure I get them right
   * s s l l
   * l l s s
   * s l s l
   * l s l s
   * l s s l
   * s l l s
   */
  static const int TotalOrders = 6;
  velocity_curve_pool_.reserve(TotalOrders);
  const std::vector<int> ssll({1, 1, 0, 0});
  velocity_curve_pool_.push_back(ssll);
  const std::vector<int> llss({0, 0, 1, 1});
  velocity_curve_pool_.push_back(llss);
  const std::vector<int> slsl({1, 0, 1, 0});
  velocity_curve_pool_.push_back(slsl);
  const std::vector<int> lsls({0, 1, 0, 1});
  velocity_curve_pool_.push_back(lsls);
  const std::vector<int> lssl({0, 1, 1, 0});
  velocity_curve_pool_.push_back(lssl);
  const std::vector<int> slls({1, 0, 0, 1});
  velocity_curve_pool_.push_back(slls);

  // brain dead argument parsing at the worst spot, but honestly I don't want
  // to build a command line parser and this will be running through roslaunch anyway
  const auto arguments = QCoreApplication::instance()->arguments();
  const auto argPosition  = arguments.indexOf(QLatin1String("--diepool"));
  if (argPosition != -1 && argPosition + 1 < arguments.size()) {
    die_pool_file_name_ = arguments.at(argPosition + 1);
  } else {
    die_pool_file_name_ = "diepool.txt"; // That's right, drop it in CWD.
  }
  readDiePool();
}

OrderGenerator::~OrderGenerator()
{
  writeDiePool();
}

void OrderGenerator::fillDiePool()
{
  // Generate random things based on the current time.
  const auto seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::mt19937 generator(seed);
  std::uniform_int_distribution<int> distribution(1, velocity_curve_pool_.size());
  auto dice = std::bind(distribution, generator);
  prepareDiePool();
  auto rolls = 0;
  const auto total = die_pool_.capacity();
  while (rolls < total) {
    die_pool_.push_back(dice());
    ++rolls;
  }
}

std::vector<int> OrderGenerator::newOrder()
{
  if (die_pool_.empty()) {
    fillDiePool();
  }
  const auto index = die_pool_.back() - 1;
  die_pool_.pop_back();
  return velocity_curve_pool_.at(index);
}

void OrderGenerator::readDiePool()
{
  // We are just going to use standard text streams to make this easier.
  QFile file(die_pool_file_name_);
  if (!file.open(QIODevice::ReadOnly)) {
    // This won't be seen because we are still in initialization
    ROS_DEBUG("Could not open the die pool (%s). It will be generated later.", die_pool_file_name_.toUtf8().constData());
    return;
  }
  prepareDiePool();
  QTextStream in(&file);
  in.setCodec("UTF-8"); // Not necessary, but let's be correct.
  while (!in.atEnd()) {
    int roll;
    in >> roll;
    die_pool_.push_back(roll);
  }
  // It seems that me reading the EOF adds a zero, so throw it away.
  // this happens when you go with a very, very simple file.
  die_pool_.pop_back();
}

void OrderGenerator::prepareDiePool()
{
  die_pool_.clear();
  die_pool_.reserve(1024);
}

void OrderGenerator::writeDiePool()
{
  QFile file(die_pool_file_name_);
  if (!file.open(QIODevice::WriteOnly)) {
    ROS_DEBUG("Could not open the die pool (%s) for saving. The rest of your pool is gone.", die_pool_file_name_.toUtf8().constData());
    return;
  }
  QTextStream out(&file);
  out.setCodec("UTF-8"); // Not necessary, but let's be correct.
  for (const auto roll : die_pool_) {
    out << roll << endl;
  }
}
