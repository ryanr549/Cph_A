import numpy as np
import matplotlib.pyplot as plt
import math

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

data = np.loadtxt('data.TXT')
ex = np.loadtxt('smp.dat')

plt.xlabel('energy(eV)')
plt.ylabel('Intensity(counts)')
plt.hist(ex, bins=113)
plt.savefig('fig_1.eps')
