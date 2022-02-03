import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
import math

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

dat = np.loadtxt('0_2.dat')
x = dat[0:100000]
y = dat[100001:200001]
plt.xlabel('x')
plt.ylabel('y')
plt.plot(x, y, linewidth=0.1)
plt.savefig('0_2.eps')
plt.show()
plt.scatter(x, y, c=range(100000), cmap=mpl.cm.jet, s=0.1)
plt.colorbar(label="Counts", orientation='vertical')
plt.xlabel('x')
plt.ylabel('y')
plt.savefig('0_2_1.eps')
plt.show()

dat = np.loadtxt('1_0.dat')
x = dat[0:100000]
y = dat[100001:200001]
plt.xlabel('x')
plt.ylabel('y')
plt.plot(x, y, linewidth=0.1)
plt.savefig('1_0.eps')
plt.show()
plt.scatter(x, y, c=range(100000), cmap=mpl.cm.jet, s=0.1)
plt.colorbar(label="Counts", orientation='vertical')
plt.xlabel('x')
plt.ylabel('y')
plt.savefig('1_0_1.eps')
plt.show()

dat = np.loadtxt('5_0.dat')
x = dat[0:100000]
y = dat[100001:200001]
plt.xlabel('x')
plt.ylabel('y')
plt.plot(x, y, linewidth=0.1)
plt.savefig('5_0.eps')
plt.show()
plt.scatter(x, y, c=range(100000), cmap=mpl.cm.jet, s=0.1)
plt.colorbar(label="Counts", orientation='vertical')
plt.xlabel('x')
plt.ylabel('y')
plt.savefig('5_0_1.eps')
plt.show()
