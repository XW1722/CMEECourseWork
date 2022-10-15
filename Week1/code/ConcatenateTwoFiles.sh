#!/bin/bash

if [ !z "$1" ] && [ !-z "$2"] && [ !-z "$3"]; then #checks if the inputs are not empty
    cat $1 > $3
    cat $2 >> $3
    echo "Merged File is"
    cat $3
else
    echo "Please enter valid input"
fi
exit
