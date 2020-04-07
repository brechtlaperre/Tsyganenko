import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import click

sys.path.append('.')

from src.data.preprocess import read_and_parse

def create_image(grid, field, filename, data):
    
    plt.streamplot(grid[0], grid[2], field[0], field[2], density=.9, linewidth=1, color='gray', arrowsize=.5)
    plt.plot(0, 0, 'ko')
    plt.xlabel(r'x/$R_E$')
    plt.ylabel(r'z/$R_E$')
    plt.title('{:.0f}-{:.0f} {:02.0f}:{:02.0f}:00'.format(data.Year, data.DOY, data.Hour, data.Minute))
    plt.savefig(filename)
    plt.close()

@click.command()
@click.argument('folder', type=str, default='input1')
@click.argument('model', type=str, default='test')
def main(folder, model):
    input_file = folder + '.csv'
    data = pd.read_csv('model/input/{}'.format(input_file))
    for i in range(len(data)):
        if i < 10:
            i = '0{}'.format(i)
        grid, field, _ ,_ ,_  = read_and_parse('model/{}/output/OUT{}.DAT'.format(model, i))
        create_image(grid, field, 'movies/{}/{}/frame{}.png'.format(model, folder, i), data.iloc[int(i)])

if __name__ == '__main__':
    main()

