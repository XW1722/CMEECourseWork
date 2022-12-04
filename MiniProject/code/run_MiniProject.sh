#!/bin/bash
# Author: Xuan Wang
# Contact: xuan.wang22@imperial.ac.uk

# runs the python script of data preparation
ipython3 dataprocess.py

# runs the R script of model defining, model fitting and comparison
Rscript miniproject.R

# generates the report pdf
bash compilation.sh
echo "Process completed!"