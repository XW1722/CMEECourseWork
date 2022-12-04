# CMEE Miniproject

## Description:

This folder contains the coding and analysis of the CMEE miniproject, which investigates the performance of different models on the evaluation of biological population growth.

## Languages:

IPython version 7.31.1

R version 4.2.2

Shell

## Dependencies:

To fully run the scripts in this directory, the following packages needs to be installed:

**R:**

install.packages("ggplot2")

install.packages("stats")

install.packages("minpack.lm")

**Python:**

pip install pandas

The packages are used in the following ways:

*ggplot2* - for the plotting of graphs, including the model plots and the AIC/BIC comparison plots;

*stats* - for the linear model fittings;

*minpack.lm* - for the non-linear model fittings;

*pandas* - for the reading of the original dataset.

## Structure:

***1. dataprocess.py***

This is the first script to be run. This script includes the preparation of data to be used in the further modelling and analysis. The raw data is processed in this script.

***2. miniproject.R***

This R script includes the modelling and the data visualisation. The dataset processed by the first step is used for this script.

***3. report.tex & report.bib***

The report is written in LaTeX in the report.tex file. The file report.bib includes the reference used for the report.

***4. compilation.sh***

This script runs the report.

***5. run_MiniProject.sh***

This is the final shell script, which runs the entire project including both coding and reports.

## Author name:

Xuan Wang

## Contact:

xuan.wang22@imperial.ac.uk