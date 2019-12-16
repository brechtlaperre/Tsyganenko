# -*- coding: utf-8 -*-
"""
Created on Wed Nov  4 12:33:00 2015

@author: murcielago
"""
#%%
import numpy as np
import matplotlib.pyplot as plt
from DA.preprocess import read_and_parse

#%%

def plotMagneticField(x, y, Bx, By, Bm=None):
   
    
    # Plot the total B field
    plt.figure()
    ax1 = plt.subplot(111)
    ax1.streamplot(x, y, Bx, By, density=.7, linewidth=1, color='gray', arrowsize=.5)
    plt.show()
    #p1 = ax1.pcolormesh(x, z, np.ma.log10(Btm/L/L))
    #ax1.set(aspect=1, title='Tsyganenko magnetic field')
    
    #plt.colorbar(p1, ax=ax1)
    """
    plt.figure(2)
    ax2 = plt.subplot(111)
    ax2.streamplot(x, z, BDx+B0x, BDz+B0z, density=1.2, color='k')
    #p2 = ax2.pcolormesh(x, z, np.ma.log10(BDm))
    ax2.set(aspect=1, title='Dipole magnetic field')

    #plt.colorbar(p2, ax=ax2)
    """

    
    #plt.figure(3)
    #ax3 = plt.subplot(111)
    #if Bm is not None:
    #    p3 = ax3.pcolormesh(x, y, np.ma.log10(Bm), vmin=-5,vmax=1)
    #ax1.streamplot(x, y, Bx, By, density=.7, linewidth=1, color='gray', arrowsize=.5)
    #ax3.set(aspect=1, title='Total magnetic field')
    #plt.show()
    #plt.colorbar(p3, ax=ax3)

    """
    plt.figure(4)
    plt.plot(x[96,:],np.log10(Bm[96,:]))
    plt.plot(x[2,:],np.log10(Bm[2,:]))
    plt.title("Magnetic field strength")
    """
#%%
%pwd
#%%
#if __name__ == '__main__':
import seaborn as sns
sns.set(context='paper', style='dark')
filepath = '/home/brecht/Documents/PhD/Tsyganenko_brecht/model/TA15/output/'
filepath = 'model/TA15/output/'
filename = ['OUT00.DAT']#, 'OUT01.DAT', 'OUT02.DAT', 'OUT03.DAT']
for files in filename:
    grid, _, _, total, size = read_and_parse(filepath+files)
    #print(size[0].shape)
    fig, ax = plt.subplots()
    gf = ax.imshow(np.ma.log10(size[0]), extent=(grid[0][0,0], grid[0][0,-1], grid[2][0,0], grid[2][-1, 0]))
    ax.streamplot(grid[0], grid[2], total[0], total[2], density=.7, linewidth=1, color='gray', arrowsize=.5)
    plt.colorbar(gf)
    plt.savefig('Tsyg_example.png', format='png', dpi=500)
    #plotMagneticField(grid[0], grid[2], total[0], total[2], size[0])

   

#%%
