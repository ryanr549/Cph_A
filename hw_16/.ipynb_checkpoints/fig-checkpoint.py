"""figure program"""

import numpy as np
import matplotlib.pyplot as plt

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

data = np.loadtxt('test.dat')
print(data)
x = np.transpose(data)[0]
y = np.transpose(data)[1]
plt.xlabel('x')
plt.ylabel('y')
plt.grid()
plt.scatter(x, y, marker=',', s=1)
ax = plt.gca()
ax.set_aspect(1)
plt.savefig('test.eps')
plt.show()
