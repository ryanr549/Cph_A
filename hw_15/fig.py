"""plot module"""

import numpy as np
import matplotlib.pyplot as plt
from IPython.core.pylabtools import figsize

figsize(18, 12)
plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300
plt.rcParams['font.size'] = 26

data = np.loadtxt('sol.dat')
size = np.size(data)
x = data[0:int(size/2)]
y = data[int(size/2):size]
x1 = np.arange(0, 0.87, 0.01)
y1 = - np.ones(np.size(x1)) / 2

plt.xlabel(r'$\lambda$')
plt.ylabel(r'$x_n$')
plt.scatter(x, y, s=1, marker='.')
plt.grid(True)
plt.plot(x1, y1, color='coral', label="-1/2")
plt.savefig('alpha.png')
plt.legend(loc=0, ncol=1)
plt.show()

plt.xlabel(r'$\lambda$')
plt.ylabel(r'$x_n$')
plt.scatter(x, y, s=0.7, marker='.')
plt.xlim(0, 1)
plt.ylim(-1.0, 0)
plt.text(0.3185, 0, r'$\lambda_1 = 0.3185$')
plt.text(0.7198, -0.644, r'$\lambda_2 = 0.7198$')
plt.savefig('half.png')
plt.show()

plt.xlabel(r'$\lambda$')
plt.ylabel(r'$x_n$')
plt.xlim(0.82, 0.88)
plt.ylim(-0.9, -0.3)
plt.text(0.8332, -0.445, r'$\lambda_3 = 0.8332$')
plt.scatter(x, y, s=10, marker='.')
plt.savefig('qua.png')
plt.xlim(0.857, 0.866)
plt.ylim(-0.58, -0.35)
plt.text(0.859, -0.374, r'$\lambda_4 = 0.8586$')
plt.text(0.863, -0.382, r'$\lambda_5 = 0.8641$')
plt.text(0.865, -0.40, r'$\lambda_6 = 0.8653$')
plt.savefig('oct.png')
plt.show()
