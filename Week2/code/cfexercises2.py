"""Defines various functions"""

__appname__ = "cfexercise2.py"

def hello_1(x):
    """Print hello every three times"""
    for j in range(x):
        if j % 3 == 0:
            print('hello')
    print(' ')
hello_1(12)

#######

def hello_2(x):
    """prints hello every three times"""
    for j in range(x):
        if j % 5 == 3:
            print('hello')
        elif j % 4 == 3:
            print('hello')
    print(' ')
hello_2(12)

######

def hello_3(x,y):
    """prints space and hello"""
    for i in range(x, y):
        print('hello')
    print(' ')
hello_3(3, 17)

######

def hello_4(x):
    """Prints hello as long as the number is not 15"""
    while x != 15:
        print('hello')
        x = x + 3
    print(' ')

hello_4(0)

######

def hello_5(x):
    """only print if x is 31 or 18"""
    while x < 100:
        if x == 31:
            for k in range(7):
                print('hello')
        elif x == 18:
            print('hello')
        x = x + 1
    print(' ')

hello_5(12)

#####
def hello_6(x,y):
    """prints while x is true and y not equal to 6"""
    while x: # while x is True
        print("hello!" + str(y))
        y += 1
        if y == 6:
            break
    print(' ')
hello_6(True, 0)