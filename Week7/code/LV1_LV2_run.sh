#!/bin/bash
# Author: Xuan Wang
# Contact: xuan.wang22@imperial.ac.uk
# Date: 19 Nov 2022
# Description: This script runs LV1.py and LV2.py, and tests the speed of the scripts at the end.

# LV1.py
echo "Running LV1.py."
ipython3 LV1.py
echo "Process completed. The results are saved in the results directory."

# LV2.py
echo "Running LV2.py with parameters: r = 1.0, a = 0.2, z = 1.5, e = 1.5, K = 60."
ipython3 LV2.py 1 0.2 1.5 1.5 60
echo "Process completed. The results are saved in the results directory."

# profiling
ipython3 -m cProfile LV1.py
ipython3 -m cProfile LV2.py





