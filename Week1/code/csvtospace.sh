#!/bin/sh
# Author: Xuan Wang xuan.wang22@imperial.ac.uk
# Script: csvtospace.sh
# Desc: This script takes a comma separated values and converts it to a space separated values file
# Date: Oct 2019

if [ -z "$1" ]; then # checks if the input is empty
    echo "Please enter a valid input."
else
    if [ -e "$1" ]; then # checks if the file exists
        if [ -d "$1"]; then # checks if the input is a directory
            for i in $1/*.csv
            do
            echo "Converting the comma separated values to space separated ..."
            newoutput = "${i#csv}.txt" # creating a new output to prevent pollution to the original file data
            cat $i | tr -s ',' '\b' > newoutput.csv # replacing the comma with spaces instead
            echo "Done!"
            done
        else
            if [ -f "$1" ]; then # checks if the object is a file
                for i in $1
                do
                    echo "Converting the comma separated values to space separated ..."
                    newoutput = "${i#csv}.txt"
                    cat $i | tr -s ',' '\b' > newoutput.csv
                    echo "Done!"
                done
            else
                echo "Please enter a valid directory or file"
            fi
        fi
    else      
        echo "The input does not exist."        
    fi
fi
exit
