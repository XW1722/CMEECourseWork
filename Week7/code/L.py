"""Fitting the Lotka-Volterra model and generating population dynamics graphs with input values from the command line"""

import scipy as sc
import scipy.integrate as integrate
import matplotlib.pylab as p
from matplotlib.backends.backend_pdf import PdfPages
import sys


def plot_f1(pops, t, r, K, a, z, e): 
    """Generating the Consumer-Resource population dynamics graph with time"""

    f1 = p.figure()
    p.plot(t, pops[:,0], 'g-', label='Resourse density')
    p.plot(t, pops[:,1], 'b-', label='Consumer density')
    p.grid()
    p.legend(loc='best')
    p.xlabel('Time')
    p.ylabel('Population density')
    p.title('Consumer-Resource population dynamics')
    box = dict(boxstyle = "Round", facecolor = "white", alpha = 0.7)
    p.text(15, max(pops[:, 0]), 'r=%s\nK=%s\na=%s\nz=%s\ne=%s' % (str(round(r,2)), str(round(K,2)), str(round(a,2)), str(round(z,2)), str(round(e,2))), horizontalalignment='right', verticalalignment = "top", bbox = box)
    return f1


def plot_f2(pops, r, K, a, z, e):
    """Generating the Consumer-Resource population dynamics graph"""

    f2 = p.figure()
    p.grid()
    p.plot(pops[:,0], pops[:,1],'r-')
    p.xlabel('Resource density')
    p.ylabel('Consumer density')
    p.title('Consumer-Resource population dynamics')
    box = dict(boxstyle = "Round", facecolor = "white", alpha = 0.7)
    p.text(max(pops[:, 0]), max(pops[:, 1]), 'r=%s\nK=%s\na=%s\nz=%s\ne=%s' % (str(round(r,2)), str(round(K,2)), str(round(a,2)), str(round(z,2)), str(round(e,2))), horizontalalignment='right', verticalalignment = "top", bbox = box)
    return f2


def save_figs(f1, f2): 
    """Saving figures """
    figs = PdfPages('../results/LV_model_2.pdf')
    figs.savefig(f1)
    figs.savefig(f2)
    figs.close()
    return 0


def main(argv): 
    """Main entry point of the program"""
    try:
        r = float(sys.argv[1])
        K = float(sys.argv[2])
        a = float(sys.argv[3])
        z = float(sys.argv[4])
        e = float(sys.argv[5])
    except:
        r = 1
        K = 100
        a = 0.5 
        z = 1.5
        e = 0.75
    
    def dCR_dt(pops, t=0):
        """Returns the growth rate of consumer and resource population at any given time step"""
        R = pops[0]
        C = pops[1]
        dRdt = r * R * (1 - R / K) - a * R * C
        dCdt = -z * C + e * a * R * C
        return sc.array([dRdt, dCdt])

    t = sc.linspace(0, 15, 1000)
    R0 = 10
    C0 = 5
    RC0 = sc.array([R0, C0])
    
    pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)

    f1 = plot_f1(pops, t, r, K, a, z, e)
    f2 = plot_f2(pops, r, K, a, z, e)
    save_figs(f1, f2)
    return 0


if __name__ == "__main__": 
    """Makes sure the "main" function is called from the command line"""
    status = main(sys.argv)
    sys.exit(status)