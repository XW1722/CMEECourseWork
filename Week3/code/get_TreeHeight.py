"""
Description:
This is the script for the week3 Groupwork on Tree Heights.

Required package for this script:
sys - used for taking argument from the command line
os - used for the elimination of extension from the input name when saving the output file
pandas - used to read the dataset
numpy - used for the mathematical computation

Date:
11 Dec 2022
"""

__author__ = "Xuan Wang xuan.wang22@imperial.ac.uk"
__appname__ = "get_TreeHeight.py"
__package__ = "sys, os, pandas, numpy"

# importing the required package
import sys
import os
import pandas as pd
import numpy as np

# taking the argument from command line
argv = str(sys.argv[1])

# loading dataset
path = ["../data/",argv]
treedata = pd.read_csv("".join(path))
# tree height definition
def TreeHeight(degrees, distance):
    """
    This function defines the function of tree height.
    """
    radians = degrees * np.pi / 180
    height = distance * np.tan(radians)
    return height

# adding the column of tree height to the data
treedata["Tree.Height.m"] = TreeHeight(treedata["Angle.degrees"], treedata["Distance.m"])

# select the first two rows of data
treedata = treedata.head(2)
treedata["Input_Name"] = argv

# saving the output file
path_results = ["../results/",os.path.splitext(argv)[0],"_treeheights.csv"]
treedata.to_csv("".join(path_results))

