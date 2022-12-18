#!/bin/sh
# Author: Xuan Wang xuan.wang22@imperial.ac.uk
# Date: 11 Dec 2022
# Description: This script runs both the get_TreeHeight files for R and python.

# Testing of R file
echo "R script started."
Rscript get_TreeHeight.R trees.csv
echo "R script done!"

# Testing of python file
echo "Python script started."
ipython3 get_TreeHeight.py trees.csv
echo "Python script done!"