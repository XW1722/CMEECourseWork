#!/bin/bash
# Author: Xuan Wang
# Contact: xuan.wang22@imperial.ac.uk
# Date: 4 Dec 2022
# Description: This script runs the LaTeX report file, and the reference file. The final pdf is saved in the results directory.

pdflatex report.tex
bibtex report
pdflatex report.tex
pdflatex report.tex

# clean up
rm *.aux
rm *.bbl
rm *.blg
rm *.log

# setting the path
mv *.pdf ../results/