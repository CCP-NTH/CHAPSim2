#!/bin/sh
FC=mpif90 

find . -mindepth 1 ! -name "build_cmake.sh" -delete

#cmake .-DCMAKE_PREFIX_PATH=/usr/bin/mpif90
#cmake -DCMAKE_FORTRAN_COMPILER=/usr/bin/mpif90 .
#cmake -DBUILD_TARGET=gpu
cmake -S ../ -B ./
cmake --build ./
cmake --install ./
