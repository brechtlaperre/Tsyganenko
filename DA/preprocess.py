import sys 
import numpy as np

def compute_dipolar(x,y,z):
    # Calculate dipolar field using the dipolar moment
    Mx = 0.0
    My = 0.0
    Mz = -2e-4

    r  = np.sqrt(x**2+y**2+z**2)
    r3 = r*r*r

    rhx = x/r
    rhy = y/r
    rhz = z/r

    BDx = (1/r3) * (3 * (Mx*rhx+My*rhy+Mz*rhz) * rhx - Mx)
    BDy = (1/r3) * (3 * (Mx*rhx+My*rhy+Mz*rhz) * rhy - My)
    BDz = (1/r3) * (3 * (Mx*rhx+My*rhy+Mz*rhz) * rhz - Mz)
    return BDx, BDy, BDz


def compute_background(x, y, z, planet='Earth'):
    '''r Add Background B field (Verify that it is not already given in Tsyganenko)'''
    #TODO Check which models do this
    B0x = np.zeros(x.shape)
    B0y = np.zeros(y.shape)
    if planet == 'Mercury':
        B0z = 7.444642e-5 * np.ones(z.shape) #Mercury
    elif planet == 'Earth':
        B0z = 0.0001 * np.ones(z.shape) #Earth
        B0z[x<1.98]=0.0
    else:
        B0z = np.zeros(z.shape)
    
    return B0x, B0y, B0z


def read_and_parse(source, background=False):
    """Read and process output of Tsyganenko

    INPUT:
        source: path and filename of source file
        nx: number of steps in x-direction used in the model
        ny: number of steps in y-direction used in the model
        nz: number of steps in z-direction used in the model
        background: boolean indicating if the earths background magnetic field is included in the results

    OUTPUT:
        1. tuple of the grid (x, y, z)
        2. tuple of external B (x, y, z)
        3. tuple of dipole B (x, y, z)
        4. tuple of background B (x, y, z)
        5. tuple of total B (x, y, z)
        6. tuple of magnitude of B (B_tot, B_dip, B_ext)
    """
    #Arbitrary values used for rescaling
    di=83081.8 
    R_sim=12742.0   # Diameter of earth in km
    x, y, z, Btx, Bty, Btz = np.genfromtxt(source, unpack=True)

    x = x.astype(np.float64)
    y = y.astype(np.float64)
    z = z.astype(np.float64)

    nx = np.where(x[1:] == x[0])[0][0] + 1
    ny = np.where(y[1:] == y[0])[0][0] + 1
    nz = int(len(z) / nx)
    # Reshape results back to the dimensions used in the original program
    # Create and scale the grid, reverse x and z axis

    x   = x.reshape((nz,ny,nx))
    y   = y.reshape((nz,ny,nx))
    z   = z.reshape((nz,ny,nx))
    x   = x[:-1,0,:-1] #* (R_sim/di)
    y   = y[:-1,0,:-1] #* (R_sim/di)
    z   = z[:-1,0,:-1] #* (R_sim/di)

    # Read and scale the magnetic field
    Btx = Btx.reshape((nz,ny,nx))
    Bty = Bty.reshape((nz,ny,nx))
    Btz = Btz.reshape((nz,ny,nx))
    Btx = Btx[:-1,0,:-1]
    Bty = Bty[:-1,0,:-1]
    Btz = Btz[:-1,0,:-1]


    # fix larg dipole at points (0,0), (0+Delta x, 0), (0-Delta x, 0)

    ccol = np.argwhere(x[0, :] == 0)
    crow = np.argwhere(z[:, 0] == 0)
    for r in [crow-1, crow, crow+1]:
        Btx[r, ccol] = 0
        Bty[r, ccol] = 0
        Btz[r, ccol] = 0


    # Scale external B field
    #TODO This needs explanation of why this must happen
    L = 2.0e-6
    Btx = L*Btx
    Bty = L*Bty
    Btz = L*Btz

    if ~background:
        B0x, B0y, B0z = compute_background(x, y, z, planet='Earth')
    else:
        B0x, B0y, B0z = np.zeros(np.shape(x)), np.zeros(np.shape(y)), np.zeros(np.shape(z))    

    
    # BDx, BDy, BDz = compute_dipolar(x, y, z)

    # Add external + internal (dipolar) B fields (+ IMF if not given in Tsyganenko)
    Bx = Btx #+ BDx + B0x
    By = Bty #+ BDy + B0y
    Bz = Btz #+ BDz + B0z


    # Calculate B fields magnitude
    Btm = np.sqrt(Btx*Btx + Bty*Bty + Btz*Btz)
    BDm = None # np.sqrt(BDx*BDx + BDy*BDy + BDz*BDz)
    Bm  = None # np.sqrt(Bx*Bx   + By*By   + Bz*Bz)

    print('Parsed {}'.format(source))
    return (x, y, z), (Btx, Bty, Btz), (B0x, B0y, B0z), (Bx, By, Bz), (Btm, BDm, Bm)

if __name__ == '__main__':
    print('Preprocessing.py')
