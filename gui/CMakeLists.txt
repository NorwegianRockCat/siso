cmake_minimum_required(VERSION 2.8.11)
project(uh_siso_experiment)
set (CMAKE_CXX_STANDARD 14)
add_definitions(-Wall -Wextra -DQT_NO_CAST_FROM_ASCII)

find_package(catkin REQUIRED COMPONENTS
  roscpp
  roslaunch
  std_msgs
)

catkin_package()

find_package(Qt5 COMPONENTS Core Gui Widgets REQUIRED)

roslaunch_add_file_check(launch)

#set(CMAKE_AUTOMOC ON)
#set(CMAKE_INCLUDE_CURRENT_DIR ON)

qt5_wrap_cpp(QT_MOC_HPP
  include/window.h
  include/fetch_process_controller.h
)

include_directories(
  include
  ${catkin_INCLUDE_DIRS}
  ${Qt5Core_INCLUDE_DIRS}
  ${Qt5Gui_INCLUDE_DIRS}
  ${Qt5Widgets_INCLUDE_DIRS}
)

add_executable(uh_fetch_driver_gui
  ${RESOURCES}
  ${QT_MOC_HPP}
  src/main.cpp
  src/window.cpp
  src/ordergenerator.cpp
  src/fetch_process_controller.cpp
)

target_link_libraries(uh_fetch_driver_gui
  Qt5::Widgets
  Qt5::Core
  Qt5::Gui
  ${catkin_LIBRARIES}
)
