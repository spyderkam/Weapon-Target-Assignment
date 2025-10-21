#!/usr/bin/env bash

filename=test_rice_simple

cd src/rice/ada
gnatmake ${filename}.adb

clear
printf "Running ${filename}.adb\n\n"

./$filename
rm -f $filename 
rm -f *.ali *.o

#cd ~
