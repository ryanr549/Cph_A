import numpy as np
import matplotlib.pyplot as plt
import math

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

t1 = 30
omega1 = 0.3
k1 = 0.3
y = np.loadtxt('scor1.dat')
x = np.arange(t1 + 2, t1 + 2 + np.size(y))
x1 = np.arange(0, 200, 0.1)
y1 = 4 * pow(k1, 2) * np.sin(omega1 * x1) * np.sin(omega1 * t1)
plt.xlim(t1, 200)
plt.xlabel('t')
plt.ylabel('Autocorrelation Function')
plt.subplots_adjust(bottom=0.15)
plt.plot(x1, y1)
plt.plot(x, y)
plt.savefig('fig1.eps')
plt.show()

t2 = 2
omega2 = 0.5
k2 = 0.2
plt.xlim(t2, 200)
y = np.loadtxt('scor2.dat')
x = np.arange(t2 + 2, t2 + 2 + np.size(y))
x2 = np.arange(0, 200, 0.1)
y2 = 4 * pow(k2, 2) * np.sin(omega2 * t2) * np.sin(omega2 * x2)
plt.xlabel('t')
plt.ylabel('Autocorrelation Function')
plt.subplots_adjust(bottom=0.15)
plt.plot(x2, y2)
plt.plot(x, y)
plt.savefig('fig2.eps')
plt.show()

t3 = 50
omega3 = 0.15
k3 = 0.3
plt.xlim(t3, 200)
y = np.loadtxt('scor3.dat')
x = np.arange(t3 + 2, t3 + 2 + np.size(y))
x3 = np.arange(0, 200, 0.1)
y3 = 4 * pow(k3, 2) * np.sin(omega3 * t3) * np.sin(omega3 * x3)
plt.xlabel('t')
plt.ylabel('Autocorrelation Function')
plt.subplots_adjust(bottom=0.15)
plt.plot(x3, y3)
plt.plot(x, y)
plt.savefig('fig3.eps')
plt.show()
