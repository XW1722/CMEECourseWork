#!/bin/bash
pdflatex $1
bibtex $1.tex
pdflatex $1
pdflatex $1
evince $1.tex.pdf &

## Cleanup
rm *.aux
rm *.log
rm *.bbl
rm *.blg
rm *.fls
rm *.bib
rm *.fdb_latexmk

# move to the results directory
mv *.pdf ../results/

