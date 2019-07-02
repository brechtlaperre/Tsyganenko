import pandas as pd
import numpy as np
import click

def gen_init_states(base, parameter, mu, sigma, num=100):

    init = base.loc[np.repeat(base.index.values, num)].reset_index(drop=True)
    init[parameter] = np.random.normal(mu, sigma, size=init.shape[0])
    init[parameter] = init[parameter].apply(lambda x: round(x*100)/100)
    
    return init

@click.command()
@click.argument('column', type=str)
@click.argument('mu', type=float, default=0)
@click.argument('sigma', type=float, default=0.05)
@click.argument('amount', type=int, default=100)
def main(column, mu, sigma, amount):
    '''Generates Tsyganenko inputfile named TA15_input
    Generates inputfile useable by model TA15. 
    It varies the value in column given, by picking randomly from a gaussian distribution
    The other values remain fixed.
    Input:
        column: name of variable to be varied 
        mu: mean value of the gaussian distribution
        sigma: standard deviation of gaussian distribution
        amount: Total number of inputs - size of the ensemble
    Output:
        file named TA15_output, useable by model TA15.
    '''
    columns = ['ID', 'PDYN', 'B0y', 'B0z', 'XIND']

    assert column in columns, "Error, unknown variable {}. Known variables: {}".format(column, columns)

    base = pd.DataFrame(columns=columns, 
                        data=[[0, 3.0, 1, 8, 1.0]])
    base = base.set_index(['ID'])
    init = gen_init_states(base, column, mu, sigma, amount)

    init.to_csv('DA/input/TA15_input')

if __name__ == '__main__':
    main()