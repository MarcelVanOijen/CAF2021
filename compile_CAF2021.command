#!/bin/bash

cd "$(dirname "$BASH_SOURCE")"

gfortran -O3 -c -fdefault-real-8 model/declare_parameters.f90 model/environment.f90 model/abovegroundres.f90 model/management.f90 model/tree.f90 model/shade.f90 model/scaling.f90 model/coffee.f90 model/belowgroundres.f90 model/soil.f90 model/set_params.f90 model/CAF2021.f90

gfortran -shared -o CAF2021.DLL declare_parameters.o environment.o abovegroundres.o management.o tree.o shade.o scaling.o coffee.o belowgroundres.o soil.o set_params.o CAF2021.o

rm -rf *.o
rm -rf *.mod
