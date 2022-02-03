import numpy as np
import matplotlib.pyplot as plt

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

y01 = [3.549, 4.465, 4.618, 4.583]
x1 = np.arange(2, 6, 1)
y1 = np.ones(np.size(x1)) * 4.669201
x2 = np.arange(3, 6, 1)
y2 = np.ones(np.size(x2)) * 2.502908
y02 = [2.588, 2.525, 2.500]
plt.xlabel('m')
plt.ylabel(r'$\delta$')
plt.scatter(x1, y01, label="Computational")
plt.plot(x1, y1, color='coral', label="Theoretical")
plt.legend(loc=0, ncol=1)
plt.savefig('err1.eps')
plt.show()

plt.xlabel('m')
plt.ylabel(r'$\alpha$')
plt.scatter(x2, y02, label="Computational")
plt.plot(x2, y2, color='coral', label="Theoretical")
plt.legend(loc=0, ncol=1)
plt.savefig('err2.eps')
plt.show()
