"""
This script contains the use of Lotka-Volterra model with 
the density dependence to the resource population (1 - R/K).

The graphs are plotted at the end.
Parameters for this script are taken from the command line.
"""

__appname__ = "LV2.py"
__author__ = "Xuan Wang xuan.wang22@imperial.ac.uk"
__package__ = "numpy, sympy, scipy, matplotlib.pylab, sys"

# importing packages
from scipy import stats
import numpy as np
import sympy as sp
import scipy.integrate as integrate
import matplotlib.pylab as p
from matplotlib.backends.backend_pdf import PdfPages
import sys

def dCR_dt(pops, t=0):
    """
    This function generates the growth rate of consumer and resource population at any given time.
    The model contains the resource density dependence.
    
    Parameters include:
    R & C: consumer and resource population abundances
    r: the intrinsic growth rate of the resource population
    a: per-capita "search rate" for the resource
    z: mortality rate
    e: consumer efficiency in converting resource to consumer biomass
    K: the resource population's carrying capacity
    """
    R = np.array(pops[0], dtype = int)
    C = pops[1]
    dRdt = r * R * (1 - R / K) - a * R * C 
    dCdt = -z * C + e * a * R * C
    return np.array([dRdt, dCdt])

# initial values
t = np.linspace(0, 10, 1000)
R0 = 10
C0 = 5 
RC0 = np.array([R0, C0])

def main(r = 1.0, a = 0.2, z = 1.5, e = 1.5, K = 60): 
    """
    Once the main entrance is confirmed, this function will be operated.
    The plots will be generated and saved in the results directory.
    """
    pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)
    infodict['message']

    # plotting the graph for resource & consumer density and time
    f1 = p.figure()
    p.plot(t, pops[:,0], 'g-', label='Resource density') 
    p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
    p.grid()
    p.legend(loc='best')
    p.xlabel('Time')
    p.ylabel('Population density')
    p.annotate(pops[-1,0], xy = (max(t), pops[-1,0]))
    p.annotate(pops[-1,1], xy = (max(t), pops[-1,1]))
    p.title('Consumer-Resource population dynamics')

    # generating the graph for resource density and consumer density
    f2 = p.figure()
    p.plot(pops[:,0], pops[:,1], 'r-')
    p.grid()
    p.xlabel('Resource density')
    p.ylabel('Consumer density')
    p.title('Consumer-Resource population dynamics')

    # adding the annotation of values of parameters
    p.text(5, 5, "Parameters used in this model:")
    p.text(5, 4, f"r = {r}")
    p.text(5, 3.5, f"a = {a}")
    p.text(5, 3, f"z = {z}")
    p.text(5, 2.5, f"e = {e}")
    p.text(5, 2, f"K = {K}")

    # saving the figure
    fig = PdfPages('../results/LV_model_1.pdf')
    fig.savefig(f1)
    fig.savefig(f2)
    fig.close()

    # printing the final (non-zero) population value
    print(f'The final population value for resource density is {pops[-1,0]}')
    print(f'The final population value for consumer density is {pops[-1,1]}')

if __name__ == "__main__":
    if len(sys.argv) == 6:
        # taking arguments from command line
        r = float(sys.argv[1])
        a = float(sys.argv[2])
        z = float(sys.argv[3])
        e = float(sys.argv[4])
        K = float(sys.argv[5])
        main(r, a, z, e, K)
        sys.exit()
    else:
        print("The default values of parameters are used.")
        main()
        sys.exit()


