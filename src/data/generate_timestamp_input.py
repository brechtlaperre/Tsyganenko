import sys
import pandas as pd
import numpy as np
import click

sys.path.append('.')

from src.data.preprocess_input import read_omni_data, read_tsyg_data, prepare_dataset

def gen_init_states(base, parameters, mu, sigma, num=100, sign=None):
    init = base.loc[np.repeat(base.index.values, num)].reset_index(drop=True)
    for i, p in enumerate(parameters):
        if sign is not None:
            s = sign[i]
        else:
            s = ''
        init[p] = np.random.normal(mu[i], sigma[i], size=init.shape[0])
        if p == 'PDYN':
            s = 'pos'
        if s == 'pos':
            init[p] = init[p].apply(lambda x: abs(round(x*100)/100)+0.1)
        elif s == 'neg':
            init[p] = init[p].apply(lambda x: -abs(round(x*100)/100)+0.1)
        else:
            init[p] = init[p].apply(lambda x: round(x*100)/100)
                
    return init

def generate_perturbed_input(column, mu, sigma, amount, sign):
    '''Generates Tsyganenko inputfile
    Generates inputfile useable by model TA15. 
    It varies the value in column given, by picking randomly from a gaussian distribution
    The other values remain fixed.
    Input:
        column: name of variable to be varied 
        mu: mean value of the gaussian distribution
        sigma: standard deviation of gaussian distribution
        amount: Total number of inputs - size of the ensemble
        sign: Set all random values to sign of mu if true
    Output:
        file named TA15_output, useable by model TA15.
    '''
    allowed_columns = ['PDYN', 'B0y', 'B0z', 'XIND', 'VGSEX', 'VGSEY', 'VGSEZ']
    for c in column:
        assert c in allowed_columns, "Error, unknown variable {}. Known variables: {}".format(c, allowed_columns)

    mu = [mu]*len(column)
    sigma = [sigma]*len(column)
    if len(column) > 1:
        deny = ['', 'yes', 'y', 'Y', 'YES', 'Yes', 'YEs', 'yes']
        for i in range(1, len(column)):
            ans = input('> Keep same mu and sigma for the parameter {}? (Current value mu: {}, sigma: {}) [yes]'.format(c, mu[i], sigma[i]))
            if ans not in deny:
                try:
                    nmu = input('Please add the desired value of mu for {}: '.format(c))
                    nmu = float(nmu)
                    mu[i] = nmu
                    nsg = input('Please add the desired value of sigma for {}: '.format(c))
                    nsg = float(nsg)
                    sigma[i] = nsg
                except ValueError:
                    print("Error, that is not a number.")

    base = pd.DataFrame(columns=allowed_columns, data=[[2.0, 1, 8, 0, -400.0, 0.0, 0.0]])
    base.index.name = 'ID'

    if sign:
        sign = []
        for m in mu:
            if m < 0:
                sign.append('neg')
            else:
                sign.append('pos')
    else:
        sign=None

    init = gen_init_states(base, column, mu, sigma, amount, sign)
    return init

@click.command()
@click.argument('column', type=str, nargs=-1)
@click.argument('mu', type=float, default=0)
@click.argument('sigma', type=float, default=0.05)
@click.argument('amount', type=int, default=100)
@click.option('--sign', type=bool, default=False)
def main(column, mu, sigma, amount, sign):
    states = generate_perturbed_input(column, mu, sigma, amount, sign)
    states.to_csv('DA/input/TA15_input.csv')

if __name__ == '__main__':
    main()
    