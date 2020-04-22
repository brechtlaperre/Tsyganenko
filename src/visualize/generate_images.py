import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable
import seaborn as sns
import click

sys.path.append('.')

from src.data.preprocess import read_and_parse

def create_image(grid, field, dict_field, time, model, save_to):
    keys = list(dict_field.keys())
    del(keys[1]) # Ignore By

    for i in range(field[0].shape[0]):
        for j in range(field[2].shape[1]):
            if grid[0][i,j]**2 + grid[2][i,j]**2 <= 1.2:
                for k in keys:
                    dict_field[k][i,j] = 0

    fig, axes = plt.subplots(1, len(k), figsize=(10,6), squeeze=True)
    labels = ['(a)', '(b)', '(c)']
    for i, key in enumerate(keys):
        surf = axes[i].imshow(dict_field[key], origin='lower', cmap=plt.get_cmap('coolwarm'), extent=(grid[0][0,0], grid[0][0,-1], grid[2][0,0], grid[2][-1, 0]), vmin=-50, vmax=50)
        axes[i].streamplot(grid[0], grid[2], field[0], field[2], density=1.1, linewidth=1, color='k', arrowsize=.5)
        axes[i].plot(0, 0, 'ko')
        axes[i].set_title(r'{}, Reference, {}[nT], $t_0$ + {} min'.format(model, key, time))
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
    plt.savefig(save_to, dpi=300, format='png', transparent=False, bbox_inches='tight', pad_inches=0)
    plt.close()

@click.command()
@click.argument('folder', type=str, default='input1')
@click.argument('model', type=str, default='test')
@click.argument('location', type=str, default='output')
@click.argument('output', type=str, default='results')
def main(folder, model, location, output):
    sns.set(context='paper', style='white', palette='deep', font_scale=1.5)
    sns.set_style('ticks')

    input_file = folder + '.csv'
    data = pd.read_csv('model/input/{}'.format(input_file))
    for i in range(len(data)):
        if i < 10:
            i = '0{}'.format(i)
        grid, field, _ ,_ ,_  = read_and_parse('model/{}/{}/OUT{}.DAT'.format(model, location, i))
        dict_field = {'Bx': field[0], 'By': field[1], 'Bz': field[2]}
        save_to = '{}/{}/{}/frame{}.png'.format(output, model, location, i)
        time = compute_rel_time(data.iloc[int(i)])
        create_image(grid, field, dict_field, time, model, save_to)

def compute_rel_time(data):
    base = pd.Timestamp('2004-05-08 09:00:00')
    td = (data.DOY - base.dayofyear).astype('timedelta64[m]')*60*60 + (data.Hour - base.hour).astype('timedelta64[m]')*60 + (data.Minute - base.minute).astype('timedelta64[m]')
    strTime = str(td).split(' ')[0]
    return strTime


if __name__ == '__main__':
    main()

