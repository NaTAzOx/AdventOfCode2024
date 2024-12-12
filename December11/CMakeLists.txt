cmake_minimum_required(VERSION 3.10)
project(December11)

set(CMAKE_CXX_STANDARD 17)

find_package(ZLIB REQUIRED)
include_directories(${ZLIB_INCLUDE_DIRS})

add_executable(December11 december11.cpp)
target_link_libraries(December11 ${ZLIB_LIBRARIES})