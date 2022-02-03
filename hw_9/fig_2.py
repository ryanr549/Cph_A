import numpy as np
import matplotlib.pyplot as plt
import math

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

x = np.arange(-3, 3, 0.01)
y = np.exp(-pow(x, 2) / 2) / np.sqrt(2 * np.pi)
data = np.loadtxt('myctn_2_6.dat')
plt.xlabel('x')
plt.ylabel('frequency')
plt.plot(x, y)
plt.hist(data, bins=16, density=True)
plt.savefig('myctn_2_6.eps')
plt.show()
