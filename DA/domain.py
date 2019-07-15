import os
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import click
from preprocess import read_and_parse

import sys


def accumulate_values(result, ux, uxx, uxy, x, y):
    
    if ux is None:
        ux = result
        uxx = np.multiply(result, result)
        uxy = np.zeros((ux.shape[0], ux.shape[1], len(x)))
        for i, locx in enumerate(x):
            uxy[:, :, i] = result*result[locx, y[i]]
    else:
        ux += result
        uxx += np.multiply(result, result)
        for i, locx in enumerate(x):
            uxy[:, :, i] += result*result[locx, y[i]]

    return ux, uxx, uxy


def get_results(folder, nx, ny, nz, x, y):
    total = 0
    ext_ux = [None, None, None]
    ext_uxx = [None, None, None]
    ext_uxy = [None, None, None]
    magn_ux, magn_uxx, magn_uxy = None, None, None
    for root, _, files in os.walk(folder):
        print(root)
        for file_ in files:
            if 'OUT' in file_:
                total += 1   
                grid, ext_B, _, _, magn = read_and_parse(root+'/' + file_, nx, ny, nz)
                for i, comp in enumerate(ext_B):
                    ext_ux[i], ext_uxx[i], ext_uxy[i] = accumulate_values(comp, ext_ux[i], ext_uxx[i], ext_uxy[i], x, y)
                magn_ux, magn_uxx, magn_uxy = accumulate_values(magn[2], magn_ux, magn_uxx, magn_uxy, x, y)

    # normalize
    for i in range(3):
        ext_ux[i] = ext_ux[i]/total
        ext_uxx[i] = ext_uxx[i]/total
        ext_uxy[i] = ext_uxy[i]/total
    magn_ux, magn_uxx, magn_uxy = magn_ux/total, magn_uxx/total, magn_uxy/total

    cor_ext = {'Bx': None, 'By': None, 'Bz': None}
    for i, key in enumerate(cor_ext.keys()):
        cor_ext[key] = compute_domain(ext_ux[i], ext_uxx[i], ext_uxy[i], x, y)
    cor_magn = compute_domain(magn_ux, magn_uxx, magn_uxy, x, y)

    return grid, cor_ext, cor_magn


def compute_domain(ux, uxx, uxy, x, y):
    var_x = uxx - np.multiply(ux, ux)
    var_y = np.zeros((len(x)))
    cov = np.zeros((var_x.shape[0], var_x.shape[1], len(x)))
    for i, _ in enumerate(x):
        var_y[i] = var_x[x[i], y[i]]
        cov[:, :, i] = uxy[:, :, i] - ux*(ux[x[i], y[i]])
        cov[:, :, i] = cov[:, :, i] / np.sqrt(np.abs(var_x*var_y[i]))
    return cov


def show_and_save(cor, grid, loc, note=''):
    fig, ax = plt.subplots(1, loc.shape[0], figsize=(12, 6))
    fig.suptitle('Domain of influence {}'.format(note))
    for i in range(loc.shape[0]):
        x, y = loc[i, :]
        surf = ax[i].contourf(grid[0], grid[2], cor[:, :, i])
        ax[i].plot(x, y, 'ro')
        fig.colorbar(surf, ax=ax[i])
        ax[i].set_title('Coords ({};{})'.format(x, y))
    plt.show(block=False)
    a = input(' > Save image?: [no]')
    if a == '' or a == 'No' or a == 'NO' or a == 'N' or a == 'n' or a == 'no':
        plt.close()            
    else:
        plt.savefig(a, dpi=1000, format='png')
        plt.close()


@click.command()
@click.argument('source', type=click.Path(exists=True))
@click.argument('coords', type=(int, int))
@click.option('--extra', type=(int, int), multiple=True)
#@click.argument('z_coord', type=int)
def main(source, coords, extra):
    NX, NY, NZ = 192, 1, 192 # Dim is hard-coded for now
    
    # parse input
    x_coords = [coords[0]]
    z_coords = [coords[1]]
    if len(extra) > 0:
        for pair in extra:
            x_coords.append(pair[0])
            z_coords.append(pair[1])

    x_coords = np.array(x_coords)
    z_coords = np.array(z_coords)
    
    grid, cor_ext, cor_magn = get_results(source, NX, NY, NZ, x_coords, z_coords)

    # Plot results    

    loc = np.zeros((x_coords.shape[0], 2))
    for i in range(len(extra) + 1):
        loc[i, 0] = round(100*grid[0][x_coords[i], z_coords[i]])/100
        loc[i, 1] = round(100*grid[2][x_coords[i], z_coords[i]])/100

    for key in cor_ext:
        show_and_save(cor_ext[key], grid, loc, note=key)
    show_and_save(cor_magn, grid, loc, note='|B|')


if __name__ == '__main__':
    main()
