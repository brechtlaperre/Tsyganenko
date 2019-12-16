import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

def read_inputs(filename='DA/input/TA15_input'):
    return pd.read_csv(filename, index_col=0)


def plot_distribution(df, parameters, names):
    sns.set(style='whitegrid')
    for par in parameters:
        sns.distplot(df[par])
    plt.xlabel('Bz (nT) / PDYN (nP)')
    plt.ylabel('frequency')
    plt.legend(names)
    plt.title('Distribution of Bz and PDYN')
    plt.savefig('dist_all.png', format='png', transparent=True)

    if False:
        #fig, axes = plt.subplots(1, 2, sharey=True, figsize=(10,4))
        sns.distplot(df['B0z'], ax=axes[0])
        axes[0].set_xlabel('Bz (nT)')
        axes[0].set_ylabel('frequency')
        axes[0].set_title('Distribution of Bz in the ensemble')

        sns.distplot(df['PDYN'], ax=axes[1])
        axes[1].set_xlabel('PDYN (nP)')
        axes[1].set_ylabel('frequency')
        axes[1].set_title('Distribution of PDYN in the ensemble')

        plt.savefig('dist_all.png', format='png', transparent=False)

def read_output(source):
    x, y, z, Btx, Bty, Btz = np.genfromtxt(source, unpack=True)
    
    x = x.astype(np.float64)
    y = y.astype(np.float64)
    z = z.astype(np.float64)

    nx = np.where(x[1:] == x[0])[0][0] + 1
    ny = np.where(y[1:] == y[0])[0][0] + 1
    nz = int(len(z) / nx)
    # Reshape results back to the dimensions used in the original program
    # Create and scale the grid, reverse x and z axis

    x   = x.reshape((nz,ny,nx))
    y   = y.reshape((nz,ny,nx))
    z   = z.reshape((nz,ny,nx))
    x   = x[:-1,0,:-1] #* (R_sim/di)
    y   = y[:-1,0,:-1] #* (R_sim/di)
    z   = z[:-1,0,:-1] #* (R_sim/di)

    # Read and scale the magnetic field
    Btx = Btx.reshape((nz,ny,nx))
    Bty = Bty.reshape((nz,ny,nx))
    Btz = Btz.reshape((nz,ny,nx))
    Btx = Btx[:-1,0,:-1]
    Bty = Bty[:-1,0,:-1]
    Btz = Btz[:-1,0,:-1]
    
    ccol = np.argwhere(x[0, :] == 0)
    crow = np.argwhere(z[:, 0] == 0)
    print(ccol, crow)
    v = 5
    for r in [crow-v, crow, crow+v]:
        Btx[r, ccol] = 0
        Bty[r, ccol] = 0
        Btz[r, ccol] = 0

    return (x, y, z), (Btx, Bty, Btz)


if __name__ == '__main__':
    #df = read_inputs('model/TA15/TA15_input')
    #parameters = ['B0z', 'PDYN']
    #plot_distribution(df, parameters, ['Bz', 'PDYN'])

    grid, B = read_output('model/TA15/output/OUT01.DAT')
    fig, axes = plt.subplots(3,3)
    
    for i in range(3):
        rs = axes[i, 0].imshow(B[i][:, :350])
        #axes[i, 0].set_colorbar(rs)

        rs = axes[i, 1].imshow(B[i][150:200,350:450])
        #axes[i, 0].set_colorbar(rs)

        rs = axes[i, 2].imshow(B[i][:, 450:])
        #axes[i, 0].set_colorbar(rs)

    plt.savefig('testout.png', format='png')
