import numpy as np
import matplotlib.pyplot as plt
import math

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

x = np.arange(-3, 3, 0.01)
y = np.log((2 * pow(x, 2) + 2 * x + 1)/(2 * pow(x, 2) - 2 * x + 1))/8 - 0.25 * np.arctan(1 - 2 * x) + 0.25 * np.arctan(2 * x + 1) 
plt.xlabel('x')
plt.ylabel('y')
plt.plot(x, y)
plt.show()

