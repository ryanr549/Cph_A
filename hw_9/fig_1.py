import numpy as np
import matplotlib.pyplot as plt
import math

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

plt.ylim(0, 1)
plt.xlim(0, 8)
x = np.arange(0, 8, 0.01)
y = np.exp(-x) 
plt.xlabel('x')
plt.ylabel('f(x)')
plt.plot(x, y)
plt.savefig('fig_1.eps')
