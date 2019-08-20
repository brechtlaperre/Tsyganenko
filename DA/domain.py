from math import ceil
import os
import matplotlib.pyplot as plt
import numpy as np
import click
from preprocess import read_and_parse

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


def compute_domain(ux, uxx, uxy, x, y):
    var_x = uxx - np.multiply(ux, ux)
    var_y = np.zeros((len(x)))
    cov = np.zeros((var_x.shape[0], var_x.shape[1], len(x)))
    for i, _ in enumerate(x):
        var_y[i] = var_x[x[i], y[i]]
        cov[:, :, i] = uxy[:, :, i] - ux*(ux[x[i], y[i]])
        dv_ = np.sqrt(np.abs(var_x*var_y[i]))
        # print(np.nonzero(dv_ <= 0))
        div = np.where(dv_ > 0, 1/dv_, 0)
        cov[:, :, i] = np.multiply(cov[:, :, i], div)
    return cov


def get_results(folder, x, y):
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
                grid, ext_B, _, field, magn = read_and_parse(root+'/' + file_)
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

    return grid, cor_ext, cor_magn, field


def show_and_save(cor, grid, loc, field, varying, note, filename=None):
    if cor.shape[2] < 4:
        fig, ax = plt.subplots(1, loc.shape[0], figsize=(12, 6))
    else:
        fig, ax = plt.subplots(2, ceil(loc.shape[0]/2))
    axes = np.ndenumerate(ax)
    fig.suptitle('Varying {}, DoI of {}'.format(varying, note))
    for i in range(loc.shape[0]):
        _, axi = axes.next()
        x, y = loc[i, :]
        surf = axi.contourf(grid[0], grid[2], cor[:, :, i])
        axi.set_xlim(np.min(grid[0]), np.max(grid[0]))
        axi.set_ylim(np.min(grid[2]),np.max(grid[2]))
        axi.plot(x, y, 'ro')
        fig.colorbar(surf, ax=axi)
        axi.set_title('({}, {})'.format(x, y))
        if field is not None:
            axi.streamplot(grid[0], grid[2], field[0], field[2], density=.88888888, linewidth=1, color='white')
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



@click.command()
@click.argument('source', type=click.Path(exists=True))
@click.argument('varying', type=str, nargs=-1)
@click.argument('coords', type=(int, int))
@click.option('--extra', type=(int, int), multiple=True)
@click.option('--identifier', type=str, default='')
def main(source, varying, coords, extra, identifier):
    
    autosave = True 
    # parse input
    x_coords = [coords[0]]
    z_coords = [coords[1]]
    if len(extra) > 0:
        for pair in extra:
            x_coords.append(pair[0])
            z_coords.append(pair[1])

    x_coords = np.array(x_coords)
    z_coords = np.array(z_coords)
    
    grid, cor_ext, cor_magn, field = get_results(source, x_coords, z_coords)
    # field = None
    # Plot results
    loc = np.zeros((x_coords.shape[0], 2))
    for i in range(len(extra) + 1):
        loc[i, 0] = round(100*grid[0][x_coords[i], z_coords[i]])/100
        loc[i, 1] = round(100*grid[2][x_coords[i], z_coords[i]])/100
    
    text=varying[0]
    if len(varying) > 1:    
        for w in varying[1:]:
            text = text + ', ' + w
    
    # Do this if you do not need to review the files
    if autosave:
        filename='figures/' + identifier + '_influence_'
        for w in varying:
            filename = filename + w + '_'
    for key in cor_ext:
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


if __name__ == '__main__':
    main()

