import numpy as np
import matplotlib.pyplot as plt
import math

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

x = np.arange(-3, 3, 0.01)
y1 = 1/(1+4 * pow(x, 4))
y2 = np.exp(- math.pi * pow(x, 2))
plt.axis([-5, 5, 0, 1.1])
plt.xlabel('x')
plt.ylabel('y')
plt.plot(x, y1, x, y2)
plt.legend(['Lorentzian like', 'Gaussian'])
plt.savefig('funcfig.eps')
