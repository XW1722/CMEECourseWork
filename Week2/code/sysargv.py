#!/usr/bin/env python3
# Author: Xuan Wang
# Contact: xuan.wang22@imperial.ac.uk

"""An in-class exercise which prints the input parameters"""

__appname__ = "sysargv.py"

import sys
print("This is the name of the script: ", sys.argv[0])
print("Number of arguments: ", len(sys.argv))
print("The arguments are: ", str(sys.argv))
