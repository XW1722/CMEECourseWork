"""
This script opens the R file TestR.R in Python using subprocess.

Name: Xuan Wang
Contact: xuan.wang22@imperial.ac.uk
Date: Nov 2022
"""

__appname__ = "TestR.py"

import subprocess
p = subprocess.Popen("Rscript --verbose TestR.R > ../results/TestR.Rout 2> ../results/TestR_errFile.Rout", shell = True).wait
