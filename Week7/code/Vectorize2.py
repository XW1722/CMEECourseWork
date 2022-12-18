"""This script is a Python version of vectorize2."""

__appname__ = 'Vectorize2.py'
__author__ = 'shengge.tong22@imperial.ac.uk'
__version__= '0.0.1'

import time
import numpy as np


def stochrick(p0=np.random.uniform(0.5, 1.5, 1000),
              r=1.2,
              K=1,
              sigma=0.2,
              numyears=100):
    """
    Description: Runs the stochastic Ricker equation with gaussian fluctuations
    Args: p0, r, K, sigma, numyears
    output: np.array
    """
    N = np.zeros((numyears, len(p0)))
    N[0, ] = p0
    for pop in range(0, len(p0)):
        for yr in range(1, numyears):
            N[yr, pop] = N[yr - 1, pop] * np.exp(r * (1 - N[yr - 1, pop] / K) + np.random.normal(0, sigma, 1))[0] # add one fluctuation from normal distribution
    return N


def stochrickvect(p0=np.random.uniform(0.5, 1.5, 1000),
              r=1.2,
              K=1,
              sigma=0.2,
              numyears=100):
    """
    Description: Write another function called stochrickvect that vectorizes the above to the extent possible, with improved performance
    Args: p0, r, K, sigma, numyears
    output: np.array
    """         
    N = np.zeros((numyears, len(p0)))
    N[0,] = p0
    for yr in range(1, numyears):
        N[yr, ] = N[yr - 1, ] * np.exp(r * (1 - N[yr - 1, ] / K) + np.random.normal(0, sigma, 1))[0]
    return N


def timing(fun, *args):
    """
    Description: This function is used to calculate the time cost for the script
    Args: fun, *args
    output: float
    """
    start = time.time()
    res = fun(*args)
    end = time.time()
    return end - start


print("Vectorized Stochastic Ricker takes:")
print(timing(stochrickvect))
