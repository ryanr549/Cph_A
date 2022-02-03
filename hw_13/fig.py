import numpy as np
import matplotlib.pyplot as plt
import math
import matplotlib as mpl
mpl.use('TkAgg')

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

#积分1
x = np.arange(0, 25, 0.1)
y = np.loadtxt('integral.dat')
y1 = np.abs(y - 12) / 12
plt.xlabel('$\gamma$')
plt.ylabel('Integral')
plt.plot(x, y)
y0 = 12 * np.ones(250)
plt.plot(x, y0)
plt.legend(['Computational value', 'Theroretical value'])
plt.savefig('integral1_1.eps')

plt.ylim(-5, 30)
plt.xlim(5, 25)
plt.savefig('integral1_2.eps')
plt.show()

plt.xlim(5, 25)
plt.ylim(-0.2, 1)
plt.plot(x, y1)
plt.xlabel('$\gamma$')
plt.ylabel('relative error')
plt.savefig('error1.eps')
plt.show()
