from math import ceil
import os
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable
import numpy as np
import seaborn as sns
import click
import sys

sys.path.append('.')

from DA.preprocess import read_and_parse

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
            if 'OUT' in file_:
                total += 1   
                grid, ext_B, _, field, magn = read_and_parse(root+'/' + file_, True)                
                for i, comp in enumerate(ext_B):
                    ext_ux[i], ext_uxx[i], ext_uxz[i] = accumulate_values(comp, ext_ux[i], ext_uxx[i], ext_uxz[i], x, z)
                magn_ux, magn_uxx, magn_uxz = accumulate_values(magn[0], magn_ux, magn_uxx, magn_uxz, x, z)

    # normalize
    for i in range(3):
        ext_ux[i] = ext_ux[i]/total # Average result
        ext_uxx[i] = ext_uxx[i]/total 
        ext_uxz[i] = ext_uxz[i]/total
    magn_ux, magn_uxx, magn_uxz = magn_ux/total, magn_uxx/total, magn_uxz/total

    cor_ext = {'Bx': None, 'By': None, 'Bz': None}
    var_ext = {'Bx': None, 'By': None, 'Bz': None}
    for i, key in enumerate(cor_ext.keys()):
        var_ext[key] = ext_uxx[i] - np.multiply(ext_ux[i], ext_ux[i])
        cor_ext[key] = compute_domain(ext_ux[i], ext_uxx[i], ext_uxz[i], x, z)
    cor_magn = compute_domain(magn_ux, magn_uxx, magn_uxz, x, z)

    return grid, cor_ext, cor_magn, ext_ux, var_ext


def show_and_save(cor, grid, loc, field, varying, note, filename=None):
#    if cor.shape[2] < 4:
#        fig, ax = plt.subplots(1, loc.shape[0], figsize=(12, 6))
    fig, ax = plt.subplots(2, ceil(loc.shape[0]/2), figsize=(10, 5))
    axes = np.ndenumerate(ax)
    fig.suptitle('Varying {}, DoI of {}'.format(varying, note))
    for i in range(loc.shape[0]):
        _, axi = axes.next()
        x, y = loc[i, :]
        surf = axi.contourf(grid[0], grid[2], cor[:, :, i], cmap=plt.get_cmap('seismic'), vmin=-1)
        axi.set_xlim(np.min(grid[0]), np.max(grid[0]))
        axi.set_ylim(np.min(grid[2]),np.max(grid[2]))
        axi.set_xlabel(r'x/$R_E$')
        axi.set_ylabel(r'z/$R_E$')
        axi.plot(x, y, 'k*')
        axi.plot(0, 0, 'go')
        fig.colorbar(surf, ax=axi)
        #axi.set_title('({}, {})'.format(x, y))
        if field is not None:
            axi.streamplot(grid[0], grid[2], field[0], field[2], density=.88888888, linewidth=1, color='gray')
    #fig.suptitle('Domain of influence {}'.format(note))
    #plt.tight_layout()
    plt.subplots_adjust(left=0.125, right=0.9, bottom=0.1, top=0.9, wspace=0.26, hspace=0.25)
    #plt.show(block=False)
    
    if filename is None:
        a = input(' > Save image?: [no]')
        if a == '' or a == 'No' or a == 'NO' or a == 'N' or a == 'n' or a == 'no':
            plt.close()            
        else:
            plt.savefig(a+'.png', dpi=800, format='png')
            plt.close()
    else:
        plt.savefig(filename+'.png', dpi=600, format='png')
        plt.close()


def compute_matrix_coords(folder, x, z):
    xloc = np.zeros(x.shape)
    zloc = np.zeros(z.shape)
    grid, _, _, _, _ = read_and_parse(folder + '/OUT00.DAT')

    if (abs(x) > 1).any() or (abs(z) > 1).any():
        # in case the coordinates are absolute
        xmin = round(grid[0][0, 0])
        xmax = round(grid[0][0, -1])
        print(xmin, xmax)
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
def main(source, varying, coords, extra, identifier, folder):
    sns.set()
    sns.set_style('white')
    autosave = True 

    x_coords = [coords[0]]
    z_coords = [coords[1]]
    if len(extra) > 0:
        for pair in extra:
            x_coords.append(pair[0])
            z_coords.append(pair[1])

    x_coords = np.array(x_coords)
    z_coords = np.array(z_coords)

    x, z, pos = compute_matrix_coords(source, x_coords, z_coords)

    grid, cor_ext, cor_magn, field, variance = get_results(source, x, z)
    
    text=varying[0]
    if len(varying) > 1:
        for w in varying[1:]:
            text = text + ', ' + w
    
    # Do this if you do not need to review the files
    if autosave:
        filename=folder + identifier + '_influence_'
        for w in varying:
            filename = filename + w + '_'

    # This plots all DoI of same parameter for different coordinates
    """ for key in cor_ext:
        if autosave:
            f = filename + 'on_' + key
        else:
            f=None    
        if field is not None:
            if autosave:
                f = f + '_f'
        show_and_save(cor_ext[key], grid, loc, field, text, key, f)
    if autosave:
        f = filename + 'on_B'
        if field is not None:
            f = f + '_f'
    else:
        f = None
    show_and_save(cor_magn, grid, loc, field, text, '|B|', f)
    """

    #if autosave:
    #    f = filename + 'variance'
    #plot_variance(variance, grid, field, f)    

    for ind, (xs, zs) in enumerate(pos):
        if autosave:
            f = filename + 'coordinate_' + str(ind)
        create_image(cor_ext, cor_magn, grid, field, xs, zs, ind, f) 

def plot_variance(variance, grid, field, filename):
    k = list(variance.keys())
    fig, axes = plt.subplots(1, len(k), figsize=(10,6), squeeze=True)
    labels = ['(a)', '(b)', '(c)']
    for i, axi in enumerate(axes.flatten()):
        surf = axi.imshow(np.log(variance[k[i]]), extent=(grid[0][0,0], grid[0][0,-1], grid[2][0,0], grid[2][-1, 0]))
        axi.set_title('DoI of {}'.format(k[i]))
        axi.text(0.05, 1.1, labels[i], transform=axi.transAxes, fontsize=12, va='top', ha='right')
        divider = make_axes_locatable(axi)
        cax = divider.append_axes("right", size="5%", pad=0.05)
        plt.colorbar(surf, cax=cax)
        if field is not None:
            axi.streamplot(grid[0], grid[2], field[0], field[2], density=.7, linewidth=1, color='gray', arrowsize=.5)
    plt.tight_layout()
    if filename is None:
        a = input(' > Save image?: [no]')
        if a == '' or a == 'No' or a == 'NO' or a == 'N' or a == 'n' or a == 'no':
            plt.close()            
        else:
            plt.savefig(a+'.png', dpi=800, format='png')
            plt.close()
    else:
        plt.savefig(filename+'.png', dpi=800, format='png', transparent=False, bbox_inches='tight', pad_inches=0)
        plt.close()

def create_image(cor_ext, cor_magn, grid, field, x, z, ind, filename):
    k = list(cor_ext.keys())
    # k = k[1:] # remove first element of the keys
    fig, axes = plt.subplots(1, len(k), figsize=(10,6), sharey=True)
    labels = ['(a)', '(b)', '(c)']
    for i, axi in enumerate(axes.flatten()):
        if i < len(k):
            surf = axi.imshow(cor_ext[k[i]][::-1, :, ind], cmap=plt.get_cmap('coolwarm'), extent=(grid[0][0,0], grid[0][0,-1], grid[2][0,0], grid[2][-1, 0]), vmin=-1, vmax=1)
            axi.set_title('DoI of {}'.format(k[i]))
            axi.text(0.05, 1.1, labels[i], transform=axi.transAxes, fontsize=12, va='top', ha='right')
            axi.set_xticks(np.arange(-40, 21, 10.0))
        #else:
        #    surf = axi.imshow(cor_magn[::-1, :, ind], vmin=-1, vmax=1, extent=(grid[0][0,0], grid[0][0,-1], grid[2][0,0], grid[2][-1, 0]))
        #    axi.set_title('DoI of |B|')
        axi.set_xlim(np.min(grid[0]), np.max(grid[0]))
        axi.set_ylim(np.min(grid[2]),np.max(grid[2]))
        axi.set_xlabel(r'x/$R_E$')
        axi.set_ylabel(r'z/$R_E$')
        axi.plot(x, z, 'g*')
        axi.plot(0, 0, 'ko')
        divider = make_axes_locatable(axi)
        cax = divider.append_axes("right", size="5%", pad=0.05)

        plt.colorbar(surf, cax=cax)
        if field is not None:
            axi.streamplot(grid[0], grid[2], field[0], field[2], density=.7, linewidth=1, color='gray', arrowsize=.5)
    plt.tight_layout()
    #plt.subplots_adjust(left=0, right=1, bottom=0, top=1, wspace=0.3, hspace=0)
    #plt.subplots_adjust(left=0.125, right=0.9, bottom=0.1, top=0.9, wspace=0.26, hspace=0.24)
    if filename is None:
        a = input(' > Save image?: [no]')
        if a == '' or a == 'No' or a == 'NO' or a == 'N' or a == 'n' or a == 'no':
            plt.close()
        else:
            plt.savefig(a+'.png', dpi=800, format='png')
            plt.close()
    else:
        plt.savefig(filename+'.png', dpi=800, format='png', transparent=False, bbox_inches='tight', pad_inches=0)
        plt.close()

if __name__ == '__main__':
    main()
    #x = np.array([0.5, 0.917, 0.333, 0.5])
    #z = np.array([0.583, 0.999, 0.417, 0.833])
    #source = 'model/TA15/output'

    #x = np.array([-15, 10, 19, -30])
    #z = np.array([0, -15, 15, 10])

    #coords = compute_matrix_coords(source, x, z)
    #print(coords)


