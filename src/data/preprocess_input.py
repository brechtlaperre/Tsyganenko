import pandas as pd
import numpy as np

def read_omni_data(fname='data/omni_5min_features.lst'):
    columns = ['Year', 'DOY', 'Hour', 'Minute', 'By', 'Bz', 'Vx', 'Vy', 'Vz', 'Np', 'PDYN', 'SYMH']
    data = pd.read_csv(fname, delim_whitespace=True, header=None, names=columns)
    return data

def read_tsyg_data(fname='data/2004_OMNI_5m_with_TA15_drivers.dat'):
    columns = ['Year', 'DOY', 'Hour', 'Minute', 'Bx', 'By', 'Bz', 'Vx', 'Vy', 'Vz', 'Np', 'T', 'SYMH', 'IMFFlag', 'SWFlag', 'Tilt', 'PDYN', 'N', 'B']
    data = pd.read_csv(fname, delim_whitespace=True, header=None, names=columns)
    data = data.drop(columns=['Bx', 'T', 'IMFFlag', 'SWFlag', 'Tilt'])
    neworder = ['Year', 'DOY', 'Hour', 'Minute', 'By', 'Bz', 'Vx', 'Vy', 'Vz', 'Np', 'PDYN', 'SYMH', 'N', 'B']
    data = data[neworder]
    return data

def compute_N(Vx, Vy, Vz, By, Bz):
    V = np.sqrt(Vx**2 + Vy**2 + Vz**2)
    Bt = np.sqrt(By**2  + Bz**2)
    th = np.arctan2(By, Bz)
    N = 0.86*(V/400)**(4/3) * (Bt/5)**(2/3) * (np.sin(th/2) ** 8) ** (1/3)
    return np.round(N.to_numpy()[0],3)

def compute_B(Vx, Vy, Vz, By, Bz, Np):
    V = np.sqrt(Vx**2 + Vy**2 + Vz**2)
    Bt = np.sqrt(By**2  + Bz**2)
    th = np.arctan2(By, Bz)
    B = (Np/5)**0.5 * (V / 400)**2.5 * Bt / 5 * np.sin(th/2) ** 6
    return np.round(B.to_numpy()[0],3)

def prepare_dataset(data):
    data['PDYN'] = round((data.Np / 1000000) * (data.Vx**2 + data.Vy**2 + data.Vz**2) * 2, 2)
    data['N'] = compute_N(data.Vx, data.Vy, data.Vz, data.By, data.Bz)
    data['B'] = compute_B(data.Vx, data.Vy, data.Vz, data.By, data.Bz, data.Np)
    return data

if __name__ == '__main__':
    print(read_tsyg_data())