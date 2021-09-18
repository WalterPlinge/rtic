cmake_minimum_required(VERSION 3.21)
project(rtic LANGUAGES C)
add_executable(rtic code/main.c)
set_target_properties(rtic PROPERTIES C_STANDARD 17)
