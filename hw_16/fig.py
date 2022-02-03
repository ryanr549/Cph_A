"""figure program"""

import numpy as np
import matplotlib.pyplot as plt
from scipy import optimize
import matplotlib as mpl

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

# print DLA color image
data = np.loadtxt('test.dat')
print(data)
x = np.transpose(data)[0]
y = np.transpose(data)[1]
plt.xlabel('x')
plt.ylabel('y')
plt.grid()
plt.xlim(-256, 256)
plt.ylim(-256, 256)
plt.scatter(x, y, marker=',', s=1, c=range(np.size(x)), cmap=mpl.cm.jet)
plt.colorbar(label="Counts", orientation='vertical')
ax = plt.gca()
ax.set_aspect(1)
plt.savefig('color.eps')
plt.show()

# plot plain image
plt.xlabel('x')
plt.ylabel('y')
plt.grid()
plt.xlim(-256, 256)
plt.ylim(-256, 256)
plt.scatter(x, y, marker=',', s=1)
ax = plt.gca()
ax.set_aspect(1)
plt.savefig('plain.eps')
plt.show()


def func(x, k, b):
    """curve fitting function"""
    return k * x + b


# box curve fitting
data1 = np.transpose(np.loadtxt('box.dat'))
eps1 = data1[0]
N1 = data1[1]
popt, pcov = optimize.curve_fit(func, np.log(1/eps1), np.log(N1))
x1 = np.arange(-5, 0.5, 0.1)
y1 = popt[0] * x1 + popt[1]
plt.scatter(np.log(1/eps1), np.log(N1), label='Computation result')
plt.plot(x1, y1,
         label='Fit curve: k=%5.3f, b=%5.3f' % tuple(popt), color='coral')
plt.legend()
plt.xlabel(r'$\log 1/ \varepsilon$')
plt.ylabel(r'$\log N(\varepsilon)$')
plt.savefig('box.eps')
plt.show()

# sandbox curve fitting
data2 = np.transpose(np.loadtxt('sandbox.dat'))
r2 = data2[0]
N2 = data2[1]
popt, pcov = optimize.curve_fit(func, np.log(r2), np.log(N2))
x2 = np.arange(1, 6, 0.1)
y2 = popt[0] * x2 + popt[1]
plt.scatter(np.log(r2), np.log(N2), label='Computation result')
plt.plot(x2, y2,
         label='Fit curve: k=%5.3f, b=%5.3f' % tuple(popt), color='coral')
plt.legend()
plt.xlabel(r'$\log r$')
plt.ylabel(r'$\log N(r)$')
plt.savefig('sand.eps')
plt.show()
