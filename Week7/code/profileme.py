"""
This is the script for CMEE Week 7 - profiling content.
The script is saved from Lecture notes - TheMulQuaBio.
"""

__appname__ = "profileme.py"

def my_squares(iters):
    """defines an appending loop"""
    out = []
    for i in range(iters):
        out.append(i ** 2)
    return out

def my_join(iters, string):
    """defines a function repeating the input string for determined
    number of times"""
    out = ''
    for i in range(iters):
        out += string.join(", ")
    return out

def run_my_funcs(x, y):
    """printing and processing the input"""
    print(x, y)
    my_squares(x)
    my_join(x, y)
    return 0

run_my_funcs(10000000, "My string")
