import matplotlib.pyplot as plt
import mpl_toolkits.mplot3d.axes3d as ax
import numpy as np

plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.dpi'] = 300

fig = plt.figure()
ax1 = plt.axes(projection='3d')

theta = np.loadtxt("theta4.dat")
phi = np.loadtxt("phi4.dat")
x = np.sin(theta) * np.cos(phi)  # 换算为直角座标绘图
y = np.sin(theta) * np.sin(phi)
z = np.cos(theta)

ax1.scatter3D(x, y, z, s=1)
plt.gca().set_box_aspect(aspect=(1, 1, 0.5))  # 调整xyz轴比例尺相同
ax1.set_xlabel('x')
ax1.set_ylabel('y')
ax1.set_zlabel('z')
plt.savefig("fig4.png")
