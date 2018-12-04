cmake_minimum_required(VERSION 2.8.3)
project(fetch_siso_launch)
find_package(catkin REQUIRED 
    roslaunch)
catkin_package()

roslaunch_add_file_check(launch)
