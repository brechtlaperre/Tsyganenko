# -*- coding: utf-8 -*-
"""
Created on Fri Nov  6 14:49:05 2015

@author: murcielago
"""

import numpy as np
import h5py
import matplotlib.pyplot as plt

# Load Tsyganenko data
NX=319
NY=2
NZ=209

# Load positions

print(" Read file")
x, y, z, Bx, By, Bz = np.genfromtxt('/home/jamaya/Dropbox/CPA/Magnetosphere/Tsyganenko/BXZ.T96.DAT', unpack=True)

x  = x.reshape((NZ,NY,NX))
y  = y.reshape((NZ,NY,NX))
z  = z.reshape((NZ,NY,NX))
Bx = Bx.reshape((NZ,NY,NX))
By = By.reshape((NZ,NY,NX))
Bz = Bz.reshape((NZ,NY,NX))

# Scale external B field
print(" Scale external field")
L = 2.0e-6
Bx = L*Bx
By = L*By
Bz = L*Bz

# Add Background B field (If not included in Tsyganenko)
#B0x = 0.0
#B0y = 0.0
#B0z = 0.0001
#Bx = B0x + Bx
#By = B0y + By
#Bz = B0z + Bz

# Calculate dipolar field using the dipolar moment
print(" Compute dipole")
Mx = 0.0
My = 0.0
Mz = -2.0e-4

r  = np.sqrt(x**2+y**2+z**2)
r3 = r*r*r

rhx = x/r
rhy = y/r
rhz = z/r

BDx = (1/r3) * (3 * (Mx*rhx+My*rhy+Mz*rhz) * rhx - Mx)
BDy = (1/r3) * (3 * (Mx*rhx+My*rhy+Mz*rhz) * rhy - My)
BDz = (1/r3) * (3 * (Mx*rhx+My*rhy+Mz*rhz) * rhz - Mz)

# Add external + internal (dipolar) B fields
print(" Add dipole")
Btx = Bx + BDx
Bty = By + BDy
Btz = Bz + BDz

Btm  = np.sqrt(Btx*Btx + Bty*Bty + Btz*Btz)

plt.figure()
plt.plot(x[104,1,:],np.log(Btm[104,1,:]))
plt.show()

Vsw = -302.893

Ex = np.zeros(np.shape(Bx))
Ey = np.zeros(np.shape(By))
Ez = np.zeros(np.shape(Bz))

rho = np.ones(np.shape(Bx))/(4*np.pi)

# Open and write the hdf5 file

print(" Open hdf5 file")
h5filename = "/home/jamaya/Dropbox/CPA/Magnetosphere/Tsyganenko/Earth-Tsyg-Fields_000000.h5"
file = h5py.File(h5filename,'w')

print(" Add dataset: Bx_ext")
file.create_dataset('/Step#0/Block/Bx/0',(NZ,NY,NX),data=Bx)
print(" Add dataset: By_ext")
file.create_dataset('/Step#0/Block/By/0',(NZ,NY,NX),data=By)
print(" Add dataset: Bz_ext")
file.create_dataset('/Step#0/Block/Bz/0',(NZ,NY,NX),data=Bz)
print(" Add dataset: Bx")
file.create_dataset('/Step#0/Block/Btx/0',(NZ,NY,NX),data=Btx)
print(" Add dataset: By")
file.create_dataset('/Step#0/Block/Bty/0',(NZ,NY,NX),data=Bty)
print(" Add dataset: Bz")
file.create_dataset('/Step#0/Block/Btz/0',(NZ,NY,NX),data=Btz)
print(" Add dataset: Ex")
file.create_dataset('/Step#0/Block/Ex/0',(NZ,NY,NX),data=Ex)
print(" Add dataset: Ey")
file.create_dataset('/Step#0/Block/Ey/0',(NZ,NY,NX),data=Ey)
print(" Add dataset: Ez")
file.create_dataset('/Step#0/Block/Ez/0',(NZ,NY,NX),data=Ez)

grp = file['/Step#0']
grp.attrs['nspec'] = np.int32(2)

print(" Add dataset: rho_0")
file.create_dataset('/Step#0/Block/rho_0/0',(NZ,NY,NX),data=-rho)
print(" Add dataset: rho_1")
file.create_dataset('/Step#0/Block/rho_1/0',(NZ,NY,NX),data=rho)

file.close()
