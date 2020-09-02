from math import ceil
import os
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable
import numpy as np
import pandas as pd
import seaborn as sns
import click
import sys

sys.path.append('.')

from src.data.preprocess import read_and_parse

def accumulate_values(result, ux, uxx, uxz, x, z):

    if ux is None:
        ux = result # The mean E(X)
        uxx = np.multiply(result, result) # The squared mean, or E(X^2)
        uxz = np.zeros((ux.shape[0], ux.shape[1], len(x))) 
        for i, locx in enumerate(x):
            uxz[:, :, i] = result*result[locx, z[i]]
    else:
        ux += result
        uxx += np.multiply(result, result)
        for i, locx in enumerate(x):
            uxz[:, :, i] += result*result[locx, z[i]]

    return ux.astype(np.float64), uxx.astype(np.float64), uxz.astype(np.float64)


def compute_domain(ux, uxx, uxy, x, z):
    var_x = uxx - np.multiply(ux, ux)
    var_z = np.zeros((len(x)))
    cov = np.zeros((var_x.shape[0], var_x.shape[1], len(x)))
    for i, _ in enumerate(x):
        var_z[i] = var_x[x[i], z[i]]
        cov[:, :, i] = uxy[:, :, i] - ux*(ux[x[i], z[i]])
        dv_ = np.sqrt(var_x*var_z[i])
        
        div = np.where(dv_ > 0, 1/dv_, 0)
        cov[:, :, i] = np.multiply(cov[:, :, i], div)
    return cov


def get_results(folder, x, z):
    total = 0
    ext_ux = [None, None, None]
    ext_uxx = [None, None, None]
    ext_uxz = [None, None, None]    
    magn_ux, magn_uxx, magn_uxz = None, None, None
    for root, _, files in os.walk(folder):
        print(root)
        for file_ in files:
            if 'OUT0' in file_:
                total += 1   
                grid, ext_B, _, field, magn = read_and_parse(root+'/' + file_, True)                
                for i, comp in enumerate(ext_B):
                    ext_ux[i], ext_uxx[i], ext_uxz[i] = accumulate_values(comp, ext_ux[i], ext_uxx[i], ext_uxz[i], x, z)
                magn_ux, magn_uxx, magn_uxz = accumulate_values(magn[0], magn_ux, magn_uxx, magn_uxz, x, z)

    # normalize
    for i in range(3):
        ext_ux[i] = ext_ux[i]/total # Mean field result
        ext_uxx[i] = ext_uxx[i]/total 
        ext_uxz[i] = ext_uxz[i]/total
    magn_ux, magn_uxx, magn_uxz = magn_ux/total, magn_uxx/total, magn_uxz/total

    cor_ext = {'Bx': None, 'By': None, 'Bz': None}
    var_ext = {'Bx': None, 'By': None, 'Bz': None}
    mean_ext = {'Bx': ext_ux[0], 'By': ext_ux[1], 'Bz': ext_ux[2]}
    for i, key in enumerate(cor_ext.keys()):
        var_ext[key] = ext_uxx[i] - np.multiply(ext_ux[i], ext_ux[i])
        cor_ext[key] = compute_domain(ext_ux[i], ext_uxx[i], ext_uxz[i], x, z)
    cor_magn = compute_domain(magn_ux, magn_uxx, magn_uxz, x, z)

    return grid, cor_ext, cor_magn, ext_ux, var_ext, mean_ext

def compute_matrix_coords(folder, x, z):
    xloc = np.zeros(x.shape)
    zloc = np.zeros(z.shape)
    grid, _, _, _, _ = read_and_parse(folder + '/OUT00.DAT')

    if (abs(x) > 1).any() or (abs(z) > 1).any():
        # in case the coordinates are absolute
        xmin = round(grid[0][0, 0])
        xmax = round(grid[0][0, -1])
        zmin = round(grid[2][0, 0])
        zmax = round(grid[2][-1, 0])
        if (x > xmin).all() & (x < xmax).all() & (z > zmin).all() & (z < zmax).all():
            x = x - xmin
            z = z - zmin
            x = x/(xmax - xmin)
            z = z/(zmax - zmin)
        else:
            raise ValueError('x and z are out of bounds')

    for i in range(len(x)):
        zloc[i] = int(grid[0].shape[1]*x[i]) # Translate x-pos to columnnumber
        xloc[i] = int(grid[0].shape[0]*z[i]) # Translate y-pos to rownumber
    xloc = xloc.astype(np.int)
    zloc = zloc.astype(np.int)

    loc = np.zeros((len(xloc), 2))
    for i in range(len(xloc)):
        loc[i, 0] = round(100*grid[0][xloc[i], zloc[i]])/100
        loc[i, 1] = round(100*grid[2][xloc[i], zloc[i]])/100

    return xloc, zloc, loc

@click.command()
@click.argument('source', type=click.Path(exists=True))
@click.argument('varying', type=str, nargs=-1)
@click.argument('coords', type=(float, float))
@click.option('--extra', type=(float, float), multiple=True)
@click.option('--identifier', type=str, default='')
@click.option('--folder', type=str, default='figures/')
@click.option('--model', type=str, default='T89')
@click.option('--datafile', type=str, default='model/input/inputVx1.csv')
def main(source, varying, coords, extra, identifier, folder, model, datafile):
    sns.set(context='paper', style='white', palette='deep', font_scale=1.5)
    sns.set_style('ticks')
    id = source.split('/')[-1].split('x')[-1]

    x_coords = [coords[0]]
    z_coords = [coords[1]]
    if len(extra) > 0:
        for pair in extra:
            x_coords.append(pair[0])
            z_coords.append(pair[1])

    x_coords = np.array(x_coords)
    z_coords = np.array(z_coords)
    x, z, pos = compute_matrix_coords(source, x_coords, z_coords)

    grid, cor_ext, cor_magn, field, variance, mean = get_results(source, x, z)

    base = pd.Timestamp('2004-05-08 09:00:00')
    src = pd.read_csv(datafile, index_col=0)
    ref = src.iloc[0,:]
    td = (ref.DOY - base.dayofyear).astype('timedelta64[m]')*60*60 + (ref.Hour - base.hour).astype('timedelta64[m]')*60 + (ref.Minute - base.minute).astype('timedelta64[m]')
    strTime = str(td).split(' ')[0]

    # Do this if you do not need to review the files
    filename=folder + model + '_'
    for w in varying:
        filename = filename + w + '_'

    f = filename + 'mean_t+{}'.format(strTime)
    plot_mean(mean, grid, field, f, model, strTime)

    f = filename + 'diff_mean_t+{}'.format(strTime)
    plot_mean_vs_ref(mean, model, id, strTime, f)

    for ind, (xs, zs) in enumerate(pos):
        f = filename + '{}_{}_t+{}'.format(int(x_coords[ind]),int(z_coords[ind]), strTime)
        plot_DOI(cor_ext, grid, field, xs, zs, ind, f, model, strTime)

def plot_mean(mean, grid, field, filename, model, time):
    k = list(mean.keys())
    del(k[1])

    for i in range(field[0].shape[0]):
        for j in range(field[2].shape[1]):
            if grid[0][i,j]**2 + grid[2][i,j]**2 <= 1.5:
                mean[k[0]][i,j] = 0
                mean[k[1]][i,j] = 0

    fig, axes = plt.subplots(1, len(k), figsize=(10,6), squeeze=True)
    labels = ['(a)', '(b)', '(c)']
    for i, key in enumerate(k):
        surf = axes[i].imshow(mean[key], origin='lower', cmap=plt.get_cmap('coolwarm'), extent=(grid[0][0,0], grid[0][0,-1], grid[2][0,0], grid[2][-1, 0]), vmin=-100, vmax=100)
        axes[i].streamplot(grid[0], grid[2], field[0], field[2], density=.85, linewidth=1, color='k', arrowsize=.5)
        axes[i].plot(0, 0, 'ko')
        axes[i].set_title(r'{}, Mean, {}[nT], $t_0$ + {} min'.format(model, key, time))
        axes[i].set_xticks(np.arange(-40, 21, 10.0))
        axes[i].tick_params(direction='out')
        axes[i].set_xlim(np.min(grid[0]), np.max(grid[0]))
        axes[i].set_ylim(np.min(grid[2]),np.max(grid[2]))
        axes[i].set_xlabel(r'x/$R_E$')
        axes[i].set_ylabel(r'z/$R_E$')
        # axes[i].text(0.05, 1.1, labels[i], transform=axes[i].transAxes, fontsize=12, va='top', ha='right')
        divider = make_axes_locatable(axes[i])
        cax = divider.append_axes("right", size="5%", pad=0.05)
        plt.colorbar(surf, cax=cax)
        
    plt.tight_layout()
    plt.savefig(filename+'.png', dpi=300, format='png', transparent=False, bbox_inches='tight', pad_inches=0)
    plt.close()

def plot_DOI(doi, grid, field, x, z, ind, filename, model, time):
    k = list(doi.keys())
    del(k[1])
    # k = k[1:] # remove first element of the keys
    fig, axes = plt.subplots(1, len(k), figsize=(8,6), sharey=True)
    labels = ['(a)', '(b)', '(c)']
    for i, key in enumerate(k):
        surf = axes[i].imshow(doi[key][:, :, ind],  origin='lower', cmap=plt.get_cmap('coolwarm'), extent=(grid[0][0,0], grid[0][0,-1], grid[2][0,0], grid[2][-1, 0]), vmin=-1, vmax=1)
        axes[i].set_title(r'{}, DoI, {}, $t_0$ + {} min'.format(model, key, time))
        #axes[i].text(0.00, 1.1, labels[i], transform=axes[i].transAxes, fontsize=12, va='top', ha='right')
        axes[i].set_xticks(np.arange(-40, 21, 10.0))
        axes[i].tick_params(direction='out')
        #else:
        axes[i].set_xlim(np.min(grid[0]), np.max(grid[0]))
        axes[i].set_ylim(np.min(grid[2]),np.max(grid[2]))
        axes[i].set_xlabel(r'x/$R_E$')
        axes[i].set_ylabel(r'z/$R_E$')
        axes[i].plot(0, 0, 'ko')
        divider = make_axes_locatable(axes[i])
        cax = divider.append_axes("right", size="5%", pad=0.05)

        plt.colorbar(surf, cax=cax)
        axes[i].streamplot(grid[0], grid[2], field[0], field[2], density=1, linewidth=1, color='k', arrowsize=.5)

        axes[i].plot(x, z, 'g*', markersize=7)

    plt.tight_layout()
    plt.savefig(filename+'.png', dpi=300, format='png', transparent=False, bbox_inches='tight', pad_inches=0)
    plt.close()

def plot_mean_vs_ref(mean, model, id, time, filename):
    grid, field, _ ,_ ,_  = read_and_parse('model/{}/outputref2/OUT{:02d}.DAT'.format(model, int(id)-1))
    dict_field = {'Bx': field[0], 'By': field[1], 'Bz': field[2]}
    keys = list(dict_field.keys())
    del(keys[1])
    
    for i in range(field[0].shape[0]):
        for j in range(field[2].shape[1]):
            if grid[0][i,j]**2 + grid[2][i,j]**2 <= 1.8:
                for k in keys:
                    dict_field[k][i,j] = 0
                    mean[k][i,j] = 0

    fig, axes = plt.subplots(1, len(keys), figsize=(10,6), squeeze=True)
    labels = ['(a)', '(b)', '(c)']

    for i, key in enumerate(keys):
        surf = axes[i].imshow(np.log(np.abs(dict_field[key] - mean[key])+0.001), origin='lower', cmap=plt.get_cmap('coolwarm'), extent=(grid[0][0,0], grid[0][0,-1], grid[2][0,0], grid[2][-1, 0]))
        axes[i].streamplot(grid[0], grid[2], field[0], field[2], density=1.1, linewidth=1, color='k', arrowsize=.5)
        axes[i].plot(0, 0, 'ko')
        axes[i].set_title(r'{}, Difference, {}[nT], $t_0$ + {} min'.format(model, key, time))
        axes[i].set_xticks(np.arange(-40, 21, 10.0))
        axes[i].tick_params(direction='out')
        axes[i].set_xlim(np.min(grid[0]), np.max(grid[0]))
        axes[i].set_ylim(np.min(grid[2]),np.max(grid[2]))
        axes[i].set_xlabel(r'x/$R_E$')
        axes[i].set_ylabel(r'z/$R_E$')
        # axes[i].text(0.05, 1.1, labels[i], transform=axes[i].transAxes, fontsize=12, va='top', ha='right')
        divider = make_axes_locatable(axes[i])
        cax = divider.append_axes("right", size="5%", pad=0.05)
        if i < 1:
            plt.colorbar(surf, cax=cax)
        else:
            plt.colorbar(surf, cax=cax, label='Log(|Reference - Mean|)')
        
    plt.tight_layout()
    plt.savefig(filename+'.png', dpi=300, format='png', transparent=False, bbox_inches='tight', pad_inches=0)
    plt.close()



if __name__ == '__main__':
    main()
