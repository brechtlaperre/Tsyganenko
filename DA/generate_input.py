import pandas as pd
import numpy as np




if __name__ == '__main__':


    base = pd.DataFrame(data=[[0, 1.0, 0.0, 3.8, 1.0]], columns=['ID', 'PDYN','B0y','B0z','XIND'])
    base = base.set_index(['ID'])

    init = base.loc[np.repeat(base.index.values, 100)].reset_index(drop=True)
    init['PDYN'] = init['PDYN'].apply(lambda x: np.random.normal(loc=6, scale=3, size=None)*x)
    init['PDYN'] = init['PDYN'].apply(lambda x: round(x*100)/100 if x > 0 else 1)
    
    init.to_csv('DA/input/TA15_input')
