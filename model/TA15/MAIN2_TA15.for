C**********************************************************************

      PROGRAM TA15FROMFILE

C
C The goal of this program is to read all of the TA15 input parameters
C from a given input file. Because Fortran 95 and earlier do not support
C command line arguments, the name of the input file will be fixed.
C
      CHARACTER(len=*), PARAMETER :: file = "TA15_data.csv"
C
C Input file structure:
C 1. The input file is of CSV-format.
C 2. The first line of the file is always the header and will be skipped.
C 3. First column is the ID. Used to create a unique file name
C 4. Column 2 to 5 are the solar wind conditions. These are
C     1) PDYN, the solar wind dynamic pressure in nPa
C     2) IMF BY in nT (average over 30-min trailing interval)
C     3) IMF BZ in nT (average over 30-min trailing interval)
C     4) XIND, the solar-wind-magnetosphere driving index, based on Newell et al. [2007]
C           Typical values of XIND: between 0 (quiet) and 2 (strongly disturbed)
C 5. Column 6 to 15 depict the boxpoints + the size of the grid. Point (0,0,0) corresponds to earth
C     1) Xbeg: Initial x position (GSM). Solar wind travels from positive x to negative x
C     2) Ybeg: Initial y position (GSM)
C     3) Zbeg: Initial z position (GSM)
C     4) Xend: Final x position (GSM)
C     5) Yend: Final y position (GSM)
C     6) Zend: Final z position (GSM)
C     7) xstep: Number of steps in x-direction. Essential for the python program
C     8) ystep: Number of steps in y-direction. Essential for the python program
C     9) zstep: Number of steps in z-direction. Essential for the python program
C 6. Column 16 depicts if the earth magnetic internal field should be added to the calculation
C     1) background: boolean
C GSM: This system has its X axis towards the Sun and its Z axis is the projection of the 
C      Earth's magnetic dipole axis (positive North) on to the plane perpendicular to the X axis. 
C      The direction of the geomagnetic field near the nose of the magnetosphere is well-ordered by this system.
C

      REAL ::           AA(10),SPS,CPS,BB(3),PSI,CC(18)
      COMMON /GEOPACK1/ AA,SPS,CPS,BB,PSI,CC

C 
C be sure to include an EXTERNAL statement with the names of 
C     (i) a magnetospheric external field model 
C     (ii) Earth's internal field model.
C
      EXTERNAL DIP_08
      EXTERNAL TA_2015_B

C
C Define all necessary variables
C

      REAL *8, DIMENSION(10) :: PARMOD

      INTEGER, PARAMETER :: ounit = 20
      INTEGER, PARAMETER :: iunit=21

      INTEGER :: IOPT
      INTEGER :: i, j, k
      REAL*8  :: PS

      ! These parameters hold the external magnetic field values
      REAL*8 :: BXGSW
      REAL*8 :: BYGSW
      REAL*8 :: BZGSW

      ! These hold the internal magnetic field values
      REAL   :: HXGSW = 0.D0
      REAL   :: HYGSW = 0.D0
      REAL   :: HZGSW = 0.D0

      REAL*8 :: Xbeg
      REAL*8 :: Ybeg
      REAL*8 :: Zbeg

      REAL*8 :: Xend
      REAL*8 :: Yend
      REAL*8 :: Zend










      END