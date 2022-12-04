#!/bin/bash
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