import numpy as np
import matplotlib.pyplot as plt
import math

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

data = np.loadtxt('dscsmp.dat')
plt.xlabel('energy(eV)')
plt.ylabel('Intensity(counts)')
plt.hist(data, bins=114)
plt.savefig('fig_2.eps')
