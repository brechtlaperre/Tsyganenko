# -*- coding: utf-8 -*-
"""
Created on Wed Nov  4 12:33:00 2015

@author: murcielago
"""
import numpy as np
import matplotlib.pyplot as plt

plt.close("all")

NX=319
NY=2
NZ=209
di=83081.8
R_sim=12742.0

# Load positions

print(" Read file")
filepath = '/home/brecht/Documents/PhD/Tsyganenko_brecht/model/T96/'
filename = ['T96.FD.01.DAT', 'T96.FD.02.DAT', 'T96.FD.03.DAT']

for f in filename:

    plt.close("all")
    source = filepath + f
    x, y, z, Btx, Bty, Btz = np.genfromtxt(source, unpack=True)

    x   = x.reshape((NZ,NY,NX))
    y   = y.reshape((NZ,NY,NX))
    z   = z.reshape((NZ,NY,NX))
    Btx = Btx.reshape((NZ,NY,NX))
    Bty = Bty.reshape((NZ,NY,NX))
    Btz = Btz.reshape((NZ,NY,NX))

    x   = x[:-1,1,:-1] * (R_sim/di)
    y   = y[:-1,1,:-1] * (R_sim/di)
    z   = z[:-1,1,:-1] * (R_sim/di)
    Btx = Btx[:-1,1,:-1]
    Bty = Bty[:-1,1,:-1]
    Btz = Btz[:-1,1,:-1]

    # Scale external B field
    print(" Scale external field")
    L = 2.0e-6
    Btx = L*Btx
    Bty = L*Bty
    Btz = L*Btz

    # Add Background B field (Verify that it is not already given in Tsyganenko)
    B0x = np.zeros(np.shape(x))
    B0y = np.zeros(np.shape(y))
    #B0z = 7.444642e-5  #Mercury
    B0z = 0.0001 * np.ones(np.shape(z))#Earth
    B0z[x<1.98]=0.0

    # Calculate dipolar field using the dipolar moment
    print(" Compute dipole")
    Mx = 0.0
    My = 0.0
    Mz = -2e-4

    r  = np.sqrt(x**2+y**2+z**2)
    r3 = r*r*r

    rhx = x/r
    rhy = y/r
    rhz = z/r

    BDx = (1/r3) * (3 * (Mx*rhx+My*rhy+Mz*rhz) * rhx - Mx)
    BDy = (1/r3) * (3 * (Mx*rhx+My*rhy+Mz*rhz) * rhy - My)
    BDz = (1/r3) * (3 * (Mx*rhx+My*rhy+Mz*rhz) * rhz - Mz)

    # Add external + internal (dipolar) B fields (+ IMF if not given in Tsyganenko)
    print(" Add dipole")
    Bx = Btx + BDx + B0x
    By = Bty + BDy + B0y
    Bz = Btz + BDz + B0z

    # Calculate B fields magniude
    Btm = np.sqrt(Btx*Btx + Bty*Bty + Btz*Btz)
    BDm = np.sqrt(BDx*BDx + BDy*BDy + BDz*BDz)
    Bm  = np.sqrt(Bx*Bx   + By*By   + Bz*Bz)

    print(" Plot figures")

    # Plot the total B field
    plt.figure(1)
    ax1 = plt.subplot(111)
    ax1.streamplot(x, z, Btx, Btz, density=1.2, linewidth=2, color='k')
    #p1 = ax1.pcolormesh(x, z, np.ma.log10(Btm/L/L))
    ax1.set(aspect=1, title='Tsyganenko magnetic field')

    #plt.colorbar(p1, ax=ax1)
    """
    plt.figure(2)
    ax2 = plt.subplot(111)
    ax2.streamplot(x, z, BDx+B0x, BDz+B0z, density=1.2, color='k')
    #p2 = ax2.pcolormesh(x, z, np.ma.log10(BDm))
    ax2.set(aspect=1, title='Dipole magnetic field')

    #plt.colorbar(p2, ax=ax2)
    """
    plt.figure(3)
    ax3 = plt.subplot(111)
    p3 = ax3.pcolormesh(x, z, np.ma.log10(Bm), vmin=-5,vmax=1)
    ax3.streamplot(x, z, Bx, Bz, density=2, color='k')
    ax3.set(aspect=1, title='Total magnetic field')

    #plt.colorbar(p3, ax=ax3)

    """
    plt.figure(4)
    plt.plot(x[96,:],np.log10(Bm[96,:]))
    plt.plot(x[2,:],np.log10(Bm[2,:]))
    plt.title("Magnetic field strength")
    """
    print(" and show")
    plt.show()