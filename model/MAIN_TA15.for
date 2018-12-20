C******************************************************************************
C
      PROGRAM EXAMPLE2

      IMPLICIT NONE
c be sure to include an EXTERNAL statement with the names of (i) a magnetospheric
c external field model and (ii) Earth's internal field model.
c
C
      REAL ::           AA(10),SPS,CPS,BB(3),PSI,CC(18)
      COMMON /GEOPACK1/ AA,SPS,CPS,BB,PSI,CC
C 
c be sure to include an EXTERNAL statement with the names of (i) a magnetospheric
c external field model and (ii) Earth's internal field model.
c
      EXTERNAL DIP_08
      EXTERNAL TA_2015_B
C
C  X,Y,Z Locations
C

      REAL*8, DIMENSION(10) :: PARMOD

      INTEGER, PARAMETER :: ounit=20
      INTEGER :: IOPT
      INTEGER :: i, k

      REAL*8 :: PS    = 0.D0
      REAL*8 :: BXGSW = 0.D0
      REAL*8 :: BYGSW = 0.D0
      REAL*8 :: BZGSW = 0.D0
      REAL   :: HXGSW = 0.D0
      REAL   :: HYGSW = 0.D0
      REAL   :: HZGSW = 0.D0

      REAL*8, DIMENSION(192,192) :: XGSW
      REAL*8, DIMENSION(192,192) :: ZGSW

      REAL*8 :: Xbeg = -19.2D0
      REAL*8 :: Zbeg = -19.2D0
      REAL*8 :: dx = 0.2D0
      REAL*8 :: dz = 0.2D0

      DO k = 1, 192
        DO i = 1, 192
          XGSW (i, k) = Xbeg + (i-1)*dx
          ZGSW (i, k) = Zbeg + (k-1)*dz
        ENDDO
      ENDDO

C
C   First, call RECALC_08, to define the main field coefficients and, hence, the magnetic
C      moment of the geodipole for IYEAR=1997 and IDAY=350.
C   The universal time and solar wind direction does not matter in this example, 
C   because here we explicitly specify the tilt angle (hence, the orientation of 
C   dipole in the GSW coordinates), so we arbitrarily set IHOUR=MIN=ISEC=0 and 
C   VGSEX=-400.0, VGSEY=VGSEZ=0 (any other values would be equally OK):
C
      CALL RECALC_08 (1997,350,0,0,0,-400.0,0.0,0.0)
C
C  Specify the dipole tilt angle PS, its sine SPS and cosine CPS, entering
c    in the common block /GEOPACK1/:
C
      PSI=0.
      SPS=SIN(PSI)
      CPS=COS(PSI)

      IOPT=0
C           (IN THIS EXAMPLE IOPT IS JUST A DUMMY PARAMETER,
C                 WHOSE VALUE DOES NOT MATTER)

      PARMOD = (/ 1.0, 0.0, 3.8, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
      PS = 0.D0

      IOPT=0
C           (IN THIS EXAMPLE IOPT IS JUST A DUMMY PARAMETER,
C                 WHOSE VALUE DOES NOT MATTER)
c

      OPEN (UNIT=ounit,FILE="BXZ.TA15.DAT",ACTION="write",
     *      STATUS="replace")

      DO k = 1, 192
        DO i = 1, 192
            CALL TA_2015_B (IOPT,PARMOD,PS,
     *                     XGSW(i,k),0.D0,ZGSW(i,k),
     *                     BXGSW,BYGSW,BZGSW)
C -- Routines to include internal B field:
c            CALL DIP_08 (REAL(XGSW(i,k)),0.0,REAL(ZGSW(i,k)),
c     *                   HXGSW,HYGSW,HZGSW)
c            CALL IGRF_GSW_08 (REAL(XGSW(i,k)),0.0,REAL(ZGSW(i,k)),
c     *                   HXGSW,HYGSW,HZGSW)
C --
          WRITE(ounit,*) XGSW(i,k),0.D0,ZGSW(i,k),
     *                   BXGSW+DBLE(HXGSW),BYGSW+DBLE(HYGSW),
     *                   BZGSW+DBLE(HZGSW)
        ENDDO
      ENDDO

      CLOSE(ounit)

      END

