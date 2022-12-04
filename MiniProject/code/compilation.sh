#!/bin/bash
pdflatex report.tex
bibtex report
pdflatex report.tex
pdflatex report.tex

# clean up
rm *.aux
rm *.bbl
rm *.blg
rm *.bib
rm *.log
rm *.fdb_latexmk
rm *.fls

mv *.pdf ../results/