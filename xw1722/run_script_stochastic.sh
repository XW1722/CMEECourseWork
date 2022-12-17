#!/bin/bash
#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
echo "R about the stochastic model is about to run."
R --vanilla < $HOME/xw1722_HPC_2022_demographic_cluster.R
mv demographic_cluster_* $HOME
echo "R (stochastic model) has finished running."