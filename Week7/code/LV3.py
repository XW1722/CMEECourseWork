"""
Author:Shengge Tong (shengge.tong22@imperial.ac.uk)
Date: Dec,2022
Description: Write a discrete-time version of the LV model called LV3.py
"""
import sys
import numpy as np
import scipy.integrate as integrate
import matplotlib.pylab as p


try:
    r = float(sys.argv[1])
    a = float(sys.argv[2])
    z = float(sys.argv[3])
    e = float(sys.argv[4])
    K = float(sys.argv[5])

except: #use default value
    r = 1.
    a = 0.1 
    z = 1.5
    e = 0.75
    K = 10000


#Define dCR_dt function
def dCR_dt(pops, t=0):
    """
    Description: Using functions above to practical
    Args: pops, t
    output: np.array
    """
    R = pops[0]
    C = pops[1]
    dRdt = R * (1 + r * (1 - R/K) - a * C)
    dCdt = C * (1 - z + e * a * R)
    
    return np.array([dRdt, dCdt])


t = np.linspace(0, 15, 1000)

R0 = 10
C0 = 5 
RC0 = np.array([R0, C0])

pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)

f1 = p.figure()

p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
p.plot(t, pops[:,1], 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')

#p.show()# To display the figure

f1.savefig('../results/LV_model.pdf') #Save figure

f2 = p.figure()
p.plot(pops[:,0], pops[:,1], "r-")
p.grid()
p.xlabel("Resource density")
p.ylabel("Consumer density")
p.title("Consumer-Resource population dynamics")
f2.savefig("../results/LV_model2.pdf")
