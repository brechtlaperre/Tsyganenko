# Tsyganenko Data Assimilation

__author:__ Brecht Laperre  
__supervisor:__ Giovanni Lapenta  
__collaborator:__ Maria-Elena Innocenti

## Status

Currently, the TA15 model is modified to read its initial conditions from a file named 'TA15_input'. This file is generated from the python file 'generate_input.py'. 

In order to run an experiment, set the wanted conditions in the Makefile and run 
```
make all
```


## TODO

- [x] Changed Fortran code to read a specific input file.
- [x] Use python to generate this csv input file
- [x] Give this file to fortran code, which generates a result for each line
- [x] Read results via python and use them to perform data assimilation
- [x] Automate process using Makefile
- [ ] Allow multiple variables to be varied
- [ ] Allow basevalues to be determined by input

## Tsyganenko model - file info

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
__Input files. The program reads input parameters from here:__
T96_input

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

__Input files. The program reads input parameters from here:__ TA15_input

__Output files:__
BXZ.TA15.DAT

