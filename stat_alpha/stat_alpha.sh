#!/bin/bash

#  This script is used to calculate slope of mass function with help of modified version of statpl code
#  (Thomas Maschberger & Pavel Kroupa, 2009, MNRAS).

# S.Mohammad Hoseini Rad
# smhr313@gmail.com
# Last modification: 12 Mar, 2013

## Usage:
#  Please put this script next to "***all.txt" files. Also you need the "original_inputstatpl" file 
#  and modified version of statpl program to compute slope of mass function.
#  In the parent directory you need "***.new" file for substituting some values to "original_inputstatpl" file.

# M_TOT=`head -n 1 ../*.new | awk '{print $2}'`
# echo "$M_TOT""  ! Total mass" >> original_inputstatpl
rm ./alpha* ./low* ./high* ./*.ps

{
for file in `ls M50*all.txt` ; do 
	echo $file
	read line
	sed 's/CCCCC/'$file'/g' original_inputstatpl > inputstatpl
	############################
	M_TOT=`head -n 1 ../*.new | awk '{print $2}'`
	echo "$M_TOT""  ! Total mass" >> inputstatpl
	############################
	MNow=`echo $line | awk '{print $2}'`
	echo "$MNow""  ! Current mass." >> inputstatpl
	############################
	Rt=`echo $line | awk '{print $26}'`
	echo "$Rt""  ! Tidal radius." >> inputstatpl
	############################
	Rh=`echo $line | awk '{print $23}'`
	echo "$Rh""  ! half-mass radius." >> inputstatpl
	############################
	Time=`echo $line | awk '{print $1}'`
	echo "$Time""  ! Time." >> inputstatpl
	############################
	./statpl < inputstatpl
	
	done
	} < ../*.new

#echo "Making mml.ps and constbin.ps plots."
#gnuplot ./plot.p
#if [ $? -ne 0 ]; then
#      			echo "!!!  Error making plots  !!!!"
#      			exit 1
#fi
echo "Done successfully!"
