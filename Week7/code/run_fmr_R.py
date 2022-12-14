"""
This script runs an R script with the use of subprocess in Python.
The output and results are printed on screen.

Author: Xuan Wang
Date: Nov 2022
Contact: xuan.wang22@imperial.ac.uk
"""

__author__ = 'Xuan Wang xuan.wang22@imperial.ac.uk'
__version__ = '0.0.1'

# importing package
import subprocess

# running the script to generate the result
subprocess.Popen("Rscript --verbose fmr.R > ../results/fmr.Rout 2> ../results/fmr_errFile.Rout", shell=True).wait()
p = subprocess.Popen(["Rscript", "fmr.R"], stdout = subprocess.PIPE, stderr = subprocess.PIPE)

# printing the contents of the R console output
stdout, stderr = p.communicate()
print(stdout.decode())

# check whether the run is successful
if stderr:
    """checks whether the run is successful"""
    print("The process raised an error: \n", stderr.decode())
else:
    print("The run was successful.")