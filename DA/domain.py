import os
import matplotlib.pyplot as plt
import numpy as np

from preprocess import read_and_parse

def compute_values(folder, nx, ny, nz, i, x, y):
    ux, uxx, uxy = None, None, None
    
    total = 0
    for root, _, files in os.walk(folder):
        print(root)
        for file_ in files:
            if 'OUT' in file_:
                _, _, _, _, magn = read_and_parse(root+file_, nx, ny, nz)
                if ux is None:
                    ux = magn[i]
                    uxx = magn[i]*magn[i]
                    uxy = magn[i]*magn[i][x, y]
                else:
                    ux += magn[i]
                    uxx += magn[i]*magn[i]
                    uxy += magn[i]*magn[i][x, y]
                total += 1

    return ux/total, uxx/total, uxy/total


def compute_domain(ux, uxx, uxy, x, y):
    var_x = uxx - ux*ux
    var_y = var_x[x, y]
    cov = uxy - ux*ux[x, y]
    return cov / np.sqrt(var_x*var_y)


if __name__ == '__main__':
    NX, NY, NZ = 192, 1, 192
    x, y = 110, 125 
    
    # Read values
    ux, uxx, uxy = compute_values('model/TA15/output/', NX, NY, NZ, 2, x, y)
    cor = compute_domain(ux, uxx, uxy, x, y)
    
    
    # plot results
    cr = plt.imshow(cor)
    plt.plot(x, y, 'ro')
    plt.colorbar(cr)
    plt.show()
