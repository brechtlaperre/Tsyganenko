# Tsyganenko model file explanation

## Models

### T96

% Files containing subroutines
Geopack-2008.for
T96.for

% Files with the actual program. Edit your parameters here
MAIN_T96.for

% Code for compiling:
gfortran T96.for Geopack-2008.for MAIN_T96.for -o T96.e

% Output files
BXZ.T96.DAT

### TA15

% Files containing subroutines
Geopack-2008.for
TA_2015_B.for 
% Another file named TA_2015_N.for which does the same calculation as TA_2015_B.for, but with slightly different parameters

% Files with the actual program. Edit your parameters here
MAIN_TA15.for

% Code for compiling:
gfortran Geopack-2008.for TA_2015_B.for MAIN_TA15.for -o TA15.e

% Output files
BXZ.TA15.DAT
