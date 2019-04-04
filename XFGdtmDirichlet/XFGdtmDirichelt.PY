

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import axes3d

n=2000

# Plot 1
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

a=(1,1,1)

s = np.random.dirichlet(a, n)
x = s[:,0].tolist()
y = s[:,1].tolist()
z = s[:,2].tolist()

ax.scatter(x, y, z, c='r', marker='o')

ax.set_xlabel('X Label')
ax.set_ylabel('Y Label')
ax.set_zlabel('Z Label')

ax.view_init(elev=40., azim=40)

plt.show()
#### STOP##########


# Plot 2
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

a=(0.1,0.1,0.1)

s = np.random.dirichlet(a, n)
x = s[:,0].tolist()
y = s[:,1].tolist()
z = s[:,2].tolist()

ax.scatter(x, y, z, c='r', marker='o')

ax.set_xlabel('X Label')
ax.set_ylabel('Y Label')
ax.set_zlabel('Z Label')

ax.view_init(elev=40., azim=40)

plt.show()
#### STOP##########


# Plot 3
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

a=(10,10,10)

s = np.random.dirichlet(a, n)
x = s[:,0].tolist()
y = s[:,1].tolist()
z = s[:,2].tolist()

ax.scatter(x, y, z, c='r', marker='o')

ax.set_xlabel('X Label')
ax.set_ylabel('Y Label')
ax.set_zlabel('Z Label')

ax.view_init(elev=40., azim=40)

plt.show()
#### STOP##########


# Plot 4
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

a=(2,5,15)

s = np.random.dirichlet(a, n)
x = s[:,0].tolist()
y = s[:,1].tolist()
z = s[:,2].tolist()

ax.scatter(x, y, z, c='r', marker='o')

ax.set_xlabel('X Label')
ax.set_ylabel('Y Label')
ax.set_zlabel('Z Label')

ax.view_init(elev=40., azim=40)

plt.show()
