#!/bin/bash
# Author: Xuan Wang
# Contact: xuan.wang22@imperial.ac.uk

ipython3 dataprocess.py

Rscript miniproject.R

bash compilation.sh

echo "Process completed!"