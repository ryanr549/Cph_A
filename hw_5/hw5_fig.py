import matplotlib.pyplot as plt
import mpl_toolkits.mplot3d.axes3d as ax
import numpy as np

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

fig = plt.figure()
ax1 = plt.axes(projection='3d')

x = np.loadtxt("x4.dat")
y = np.loadtxt("y4.dat")
z = np.loadtxt("z4.dat")

ax1.scatter3D(x, y, z, s=1)
plt.gca().set_box_aspect(aspect=(1, 1, 1))  # 调整比例尺均匀
ax1.set_xlabel('x')
ax1.set_ylabel('y')
ax1.set_zlabel('z')
plt.savefig("fig4.eps")
