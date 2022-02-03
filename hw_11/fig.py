import numpy as np
import matplotlib.pyplot as plt
import math

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

plt.subplots_adjust(bottom=0.15)
x = np.arange(50, 201, 1)
y = np.loadtxt('nu.dat')
plt.xlabel('steps')
plt.ylabel('index nu')
plt.plot(x, y)
plt.savefig('fig1.eps')
plt.show()

plt.subplots_adjust(bottom=0.15)
y1 = np.loadtxt('gamma.dat')
plt.xlabel('steps')
plt.ylabel('index gamma')
plt.plot(x, y1)
plt.savefig('fig2.eps')
plt.show()


