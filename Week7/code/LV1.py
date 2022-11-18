"""
This script demonstrates the use of 
"""

from scipy import stats
import numpy as np
import scipy.integrate as integrate
import matplotlib.pylab as p
import matplotlib.pyplot as plt
import sys

def dCR_dt(pops, t=0):
    """
    This function generates the growth rate of consumer and resource population at any given time
    """
    R = pops[0]
    C = pops[1]
    dRdt = r * R - a * R * C 
    dCdt = -z * C + e * a * R * C
    return np.array([dRdt, dCdt])

r = 1.
a = 0.1 
z = 1.5
e = 0.75
t = np.linspace(0, 15, 1000)

R0 = 10
C0 = 5 
RC0 = np.array([R0, C0])
pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)

f1 = p.figure()
p.subplot(2, 1, 1)
p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')

# generating the graph for resource density and consumer density
p.subplot(2, 1, 2)
p.plot(pops[:,0], pops[:,1], 'r-')
p.grid()
p.xlabel('Resource density')
p.ylabel('Consumer density')
p.title('Consumer-Resource population dynamics')

# adjusting the space between subplots
p.subplots_adjust(hspace = 1)

def main():
    """Define the main entrance of the function"""
    f1.savefig('../results/LV_model.pdf') #Save figure

if __name__ == "__main__":
    """check if the script is run from main"""
    status = main()
    sys.exit(status)