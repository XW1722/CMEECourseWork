"""
This is an alternative approach to profileme.py.
This approach converted the loop to a list comprehension, and replaced the .join with an explicit string concatenation.
"""

__appname__ = "profileme2.py"

def my_squares(iters):
    """the previous loop has been converted to a list comprehension"""
    out = [i ** 2 for i in range(iters)]
    return out

def my_join(iters, string):
    """the .join is replaced with an string concatenation"""
    out = ''
    for i in range(iters):
        out += ", " + string
    return out

def run_my_funcs(x,y):
    """the inputs are printed and processed"""
    print(x,y)
    my_squares(x)
    my_join(x,y)
    return 0

run_my_funcs(10000000,"My string")