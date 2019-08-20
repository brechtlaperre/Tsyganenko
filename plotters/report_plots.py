import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

def read_inputs(filename='DA/input/TA15_input'):
    return pd.read_csv(filename, index_col=0)


if __name__ == '__main__':

    sns.set(style='whitegrid')
    df = read_inputs()
    sns.distplot(df.B0z)
    plt.xlabel('Bz')
    plt.ylabel('frequency')
    plt.title('Distribution of Bz in the ensemble')
    plt.savefig('result.png', format='png')