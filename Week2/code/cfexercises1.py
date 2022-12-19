"""Defines several functions"""

__appname__ = "cfexercises1.py"

from re import L
import sys

def foo_1(x):
    """returns the square root of the input"""
    return x ** 0.5

def foo_2(x, y):
    """returns the input with larger value"""
    if x > y:
        return x
    return y

def foo_3(x, y, z):
    """exchanges the value"""
    if x > y:
        tmp = y
        y = x
        x = tmp
    if y > z:
        tmp = z
        z = y
        y = tmp
    return [x,y,z]

def foo_4(x):
    """returns the '!' of the input number"""
    result = 1
    for i in range(1,x+1):
        result = result * i
    return result

def foo_5(x):
    """same as the previous function"""
    if x == 1:
        return 1
    return x * foo_5(x - 1)

def foo_6(x):
    """same as the previous function"""
    facto = 1
    while x >= 1:
        facto = facto * x
        x = x - 1
    return facto

def main(argv):
    """defines the main entrance, which prints the results of the functions"""
    print(foo_1(1))
    print(foo_2(2,1))
    print(foo_2(1,2))
    print(foo_3(3,2,1))
    print(foo_3(2,3,1))
    print(foo_3(3,1,2))
    print(foo_3(1,2,3))
    print(foo_4(1))
    print(foo_5(1))
    print(foo_5(10))
    print(foo_6(1))
    return 0

if __name__ == "__main__":
    # checks whether it is the main entrance
    status = main(sys.argv)
    sys.exit(status)