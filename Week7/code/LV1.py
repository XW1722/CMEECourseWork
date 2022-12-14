"""
This script applies the methods used in the lecture notes and extend
it to generate a figure at the end.
"""
__author__ = "Xuan Wang xuan.wang22@imperial.ac.uk"
__date__ = "Dec 2022"
__appname__ = "LV1.py"

# import the required packages
from scipy import stats
import numpy as np
import scipy.integrate as integrate
import matplotlib.pylab as p
import matplotlib.pyplot as plt
import sys
from matplotlib.backends.backend_pdf import PdfPages

def dCR_dt(pops, t=0):
    """
    This function defines the Lotka-Volterra model.
    In this model, it generates the growth rate of consumer and resource population at any given time.

    The parameters used includes:
    R & C: consumer and resource population abundances
    r: the intrinsic growth rate of the resource population
    a: per-capita "search rate" for the resource
    z: mortality rate
    e: consumer efficiency in converting resource to consumer biomass
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

def main():
    """Define the main entrance of the function"""
    with PdfPages("../results/LV_model.pdf") as pdf:

        p.figure()
        p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
        p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
        p.grid()
        p.legend(loc='best')
        p.xlabel('Time')
        p.ylabel('Population density')
        p.title('Consumer-Resource population dynamics')
        pdf.savefig()
        p.close()

        # generating the graph for resource density and consumer density
        p.figure()
        p.plot(pops[:,0], pops[:,1], 'r-')
        p.grid()
        p.xlabel('Resource density')
        p.ylabel('Consumer density')
        p.title('Consumer-Resource population dynamics')
        pdf.savefig()
        p.close()

if __name__ == "__main__":
    """check if the script is run from main"""
    status = main()
    sys.exit(status)