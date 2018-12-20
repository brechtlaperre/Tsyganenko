# Tsyganenko model file explanation

## Models

### T96

__Files containing subroutines__
Geopack-2008.for
T96.for

__Files with the actual program. Edit your parameters here__
MAIN_T96.for

__Code for compiling:__
gfortran T96.for Geopack-2008.for MAIN_T96.for -o T96.e

__Output files__
BXZ.T96.DAT

### TA15

__Files containing subroutines__
Geopack-2008.for
TA_2015_B.for 
# Another file named TA_2015_N.for which does the same calculation as TA_2015_B.for, but with slightly different parameters

__Files with the actual program. Edit your parameters here__
MAIN_TA15.for

__Code for compiling:__
gfortran Geopack-2008.for TA_2015_B.for MAIN_TA15.for -o TA15.e

__Output files__
BXZ.TA15.DAT
