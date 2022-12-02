#!/bin/bash
pdflatex report.tex

# clean up
rm *.aux
rm *.log
rm *.fdb_latexmk
rm *.fls