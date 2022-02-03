import matplotlib.pyplot as plt
import numpy as np
import matplotlib as mpl
plt.rcParams['figure.dpi'] = 600
plt.rcParams['savefig.dpi'] = 600

fig, ax = plt.subplots()
ax.set_aspect(1)
data = np.loadtxt("rand6.out")
LENGTH = 40000  # 绘制4000个点的散点图
x = np.zeros(LENGTH)
y = np.zeros(LENGTH)
for num in range(1, LENGTH):
    x[num] = data[num]
    y[num] = data[num + 3]  # 取l值为3  
plt.scatter(x, y, s=1)
ax.set_xlabel('x')
ax.set_ylabel('y')
plt.savefig("randdd.eps")
