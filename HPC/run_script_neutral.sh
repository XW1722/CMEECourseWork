#!/bin/bash
#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
echo "R about the neutral model is about to run."
R --vanilla < $HOME/xw1722_HPC_2022_neutral_cluster.R
mv xw1722_HPC_neutral_2022* $HOME
echo "R (neutral model) has finished running."
