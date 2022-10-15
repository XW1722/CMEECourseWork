#!/bin/sh
# Author: Xuan Wang xuan.wang22@imperial.ac.uk
# Script: tabtocsv.sh
# Description: substitute the tabs in the files with commas
#
# Saves the output into a .csv file
# Arguments: 1 -> tab delimited file
# Date: Oct 2019

if [ !-z "$1" ];then # check if the input is not empty
    cat $1 | tr -s "\t" "," >> $1.csv # Creating a comma delimited version of $1 ...
    echo "Done!"
else
    echo "Please enter an input"
fi
exit