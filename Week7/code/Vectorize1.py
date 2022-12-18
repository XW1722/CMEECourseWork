"""
Author:Shengge Tong (shengge.tong22@imperial.ac.uk)
Date: Dec.2022
Description: Python version of Vectorize1
"""
import numpy as np
import time

M = np.random.uniform(0, 1, (1000, 1000))


def SumAllElements(M):
    """
    Description: Using functions to sum all the elements
    Args: M
    output: int
    """
    Dimensions = M.shape
    Tol = 0
    for i in range(Dimensions[0]):
        for j in range(Dimensions[1]):
            Tol += M[i, j]
    return Tol


def timing(fun, *args):
    """
    Description: Using timing function to calculate the time cost of the script
    Args: fun, *args
    output: float
    """
    start = time.time()
    res = fun(*args)
    end = time.time()
    return end - start

print("Using loops, the time taken is:")
print(timing(SumAllElements, M))

print("Using the in-built vectorized function, the time taken is:")
print(timing(np.sum, M))
