import sys
import click
import pandas as pd
import numpy as np

sys.path.append('.')

from src.data.preprocess_input import read_omni_data, read_tsyg_data, prepare_dataset

def extract_times(startdate, enddate, freq, data):
    rnge = pd.date_range(startdate, enddate, freq=freq)
    rows = []
    for r in rnge:
        rows.append(data[(data['Year'] == r.year) & (data['DOY'] == r.dayofyear) & (data['Hour'] == r.hour) & (data['Minute'] == r.minute)])

    new = pd.concat(rows, ignore_index=True)
    return new

def update_value(name, value, data):
    data[name] = value
    data = prepare_dataset(data)
    data = data.drop(columns=['Np'])
    return data

def gaussian_dist(base, mu, sigma, maxv, minv, size, seed=4255):
    np.random.seed(seed)
    rand = np.random.normal(mu, sigma, size=size)
    values = np.round(rand*base, 2)

    # cut of values
    for i, val in enumerate(values):
        if val > maxv:
            values[i] = maxv
        if val < minv:
            values[i] = minv
    
    return values

def main():
    data = read_tsyg_data()
    param = 'Vx'
    vmax = -363.0
    vmin = -583.0
    size = 50
    mu = 1
    sigma = 0.1

    frames = extract_times('2004-05-08 09:05:00', '2004-05-08 13:05:00', '20min', data)
    values = gaussian_dist(frames[param].mean(), mu, sigma, vmax, vmin, size)

    ref = frames.drop(columns=['Np'])
    ref.to_csv('model/input/input_ref.csv')
    print(values)
    for i, vx in enumerate(values):
        new_frame = update_value(param, vx, frames)
        new_frame.to_csv('model/input/input{}.csv'.format(i+1))


if __name__ == '__main__':
    main()
    



