# Tsyganenko models

## T96
__Files containing subroutines__
Geopack-2008.for
T96.for

__Files with the actual program. Edit your parameters here__
MAIN_T96.for

__Code for compiling:__

```bash
gfortran T96.for Geopack-2008.for MAIN_T96.for -o T96.e
```
__Input files. The program reads input parameters from here:__
T96_input

__Output files__
BXZ.T96.DAT

## TA15

__Files containing subroutines__
Geopack-2008.for
TA_2015_B.for

Another file named TA_2015_N.for which does the same calculation as TA_2015_B.for, but with slightly different parameters_

__Files with the actual program. Edit your parameters here__
MAIN_TA15.for

__Code for compiling:__

```bash
gfortran Geopack-2008.for TA_2015_B.for MAIN_TA15.for -o TA15.e
```

__Input files. The program reads input parameters from here:__ TA15_input

__Output files:__
BXZ.TA15.DAT

## Tried

- Try to use ```f2py``` to run the model in python => ```f2py``` gives errors that seem difficult to fix
- Install python package containing Tsyganenko: <https://github.com/vtsuperdarn/davitpy>
    - Still runs on python 2.7
    - Asks to compile a piece of matplotlib code which seems to fail on my machine

## TODO

- [x] Changed Fortran code to read a specific input file.
- [ ] Use python to generate this csv input file
- [ ] Give this file to fortran code, which generates a result for each line
- [ ] Read results via python and use them to perform data assimilation
