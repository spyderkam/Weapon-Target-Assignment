#!/usr/bin/env bash

filename=particle_field

cd src
gnatmake ${filename}.adb

clear
printf "Running ${filename}.adb\n\n"

./$filename
rm -f $filename $filename.ali $filename.o
#cd ~
