#!/usr/bin/env python3
# Filename: using_name.py

"""Checks if the entrance is the main entrance."""

__appname__ = "using_name.py"

if __name__ == '__main__':
    print('This program is being run by itself')
else:
    print('I am being imported from another module')

print("This module's name is: " + __name__)
