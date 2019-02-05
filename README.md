# Tsyganenko model file explanation

## Models

### T96

__Files containing subroutines__
Geopack-2008.for
T96.for

__Files with the actual program. Edit your parameters here__
MAIN_T96.for

__Code for compiling:__

```bash
gfortran T96.for Geopack-2008.for MAIN_T96.for -o T96.e
```

__Output files__
BXZ.T96.DAT

### TA15

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

__Output files:__
BXZ.TA15.DAT

## TODO

* ~~Try to use f2py to run the model in python~~ => F2py gives errors that seem difficult to fix
* Install python package containing Tsyganenko: <https://github.com/vtsuperdarn/davitpy>
    * Still runs on python 2.7
    * Asks to compile a piece of matplotlib code which seems to fail on my machine
* Try to use a Fortran for loop to run the experiments
