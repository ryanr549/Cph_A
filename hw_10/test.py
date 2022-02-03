import numpy as np
import matplotlib.pyplot as plt
import math

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

x = np.arange(6, 10, 0.1)
y = x 
plt.xlabel('x')
plt.ylabel('y')
plt.plot(x, y)
plt.show()
