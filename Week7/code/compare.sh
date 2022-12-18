#!/bin/sh
# Author:Shengge Tong shengge.tong22@imperial.ac.uk
# Script: compare.sh
# Desc: compare the time for four scrits 
# Arguments: none
# Date: Dec 2022

echo "Running Vectorize1.py..."
python3 Vectorize1.py

echo "Running Vectorize2.py..."
python3 Vectorize2.py

echo "Running Vectorize1.R..."
Rscript Vectorize1.R

echo "Running Vectorize2.R..."
Rscript Vectorize2.R
