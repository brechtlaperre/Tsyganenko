import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
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
    base = pd.DataFrame(columns=['ID', 'PDYN', 'B0y', 'B0z', 'XIND'], 
                        data=[[0, 3.0, 1, 8, 1.0]])
    base = base.set_index(['ID'])
    init = gen_init_states(base, column, mu, sigma, amount)

    init.to_csv('DA/input/TA15_input')

if __name__ == '__main__':
    main()