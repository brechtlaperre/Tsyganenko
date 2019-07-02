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
        uxy = result*result[x, y]
    else:
        ux += result
        uxx += np.multiply(result, result)
        uxy += result*result[x, y]

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
    var_y = var_x[x, y]
    cov = uxy - ux*(ux[x, y])
    return cov / np.sqrt(np.abs(var_x*var_y))


def show_and_save(cor, grid, x, y, note=''):
    fig, ax = plt.subplots(1, 1)
    surf = ax.contourf(grid[0], grid[2], cor)
    ax.plot(x, y, 'ro')
    fig.colorbar(surf)
    plt.title('{} Domain of influence on ({};{})'.format(note, x, y))
    plt.show(block=False)
    a = input(' > Save image?: [no]')
    if a == '' or a == 'No' or a == 'NO' or a == 'N' or a == 'n' or a == 'no':
        plt.close()
        return
    else:
        plt.savefig(a, dpi=1000, format='png')
        plt.close()


@click.command()
@click.argument('source', type=click.Path(exists=True))
@click.argument('x_coord', type=int)
@click.argument('z_coord', type=int)
def main(source, x_coord, z_coord):
    NX, NY, NZ = 192, 1, 192 # Dim is hard-coded for now
    grid, cor_ext, cor_magn = get_results(source, NX, NY, NZ, x_coord, z_coord)

    # Plot results
    loc_x = round(100*grid[0][x_coord, z_coord])/100
    loc_z = round(100*grid[2][x_coord, z_coord])/100
    
    for key in cor_ext.keys():
        show_and_save(cor_ext[key], grid, loc_x, loc_z, note=key)
    show_and_save(cor_magn, grid, loc_x, loc_z)


if __name__ == '__main__':
    main()
