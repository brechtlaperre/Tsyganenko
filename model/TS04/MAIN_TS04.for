C******************************************************************************
C
      PROGRAM EXAMPLE2
      IMPLICIT NONE
      REAL ::           AA(10),SPS,CPS,BB(3),PSI,CC(18)
      COMMON /GEOPACK1/ AA,SPS,CPS,BB,PSI,CC
C 
c be sure to include an EXTERNAL statement with the names of (i) a magnetospheric
c external field model and (ii) Earth's internal field model.
c
      EXTERNAL DIP_08
      EXTERNAL TS04
C
C  X,Y,Z Locations
C
      REAL*8, DIMENSION(10) :: PARMOD

      INTEGER, PARAMETER :: ounit=20
      INTEGER, PARAMETER :: iunit=21
      INTEGER :: IOPT
      INTEGER :: i, k
      INTEGER, PARAMETER :: DIMX=600
      INTEGER, PARAMETER :: DIMZ=350

      REAL*8, DIMENSION(DIMX,DIMZ) :: XGSW
      REAL*8, DIMENSION(DIMX,DIMZ) :: ZGSW

      REAL*8 :: PS    = 0.D0
      REAL*8 :: BXGSW = 0.D0
      REAL*8 :: BYGSW = 0.D0
      REAL*8 :: BZGSW = 0.D0
      REAL   :: HXGSW = 0.D0
      REAL   :: HYGSW = 0.D0
      REAL   :: HZGSW = 0.D0

      REAL*8 :: Xbeg = -40.D0
      REAL*8 :: Zbeg = -35.D0
      REAL*8 :: dx = 0.1D0
      REAL*8 :: dz = 0.2D0

      REAL :: PDYN
      REAL :: B0y
      REAL :: B0z
      REAL :: XIND
      REAL :: VGSEX
      REAL :: VGSEY
      REAL :: VGSEZ
    
      INTEGER :: Status 
      INTEGER :: ID
      CHARACTER(len=10) :: filename
C XIND: solar-wind-magnetosphere driving index, 
C Typical values of XIND: between 0 (quiet) and 2 (strongly disturbed)      
      DO k = 1, DIMZ
        DO i = 1, DIMX
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

      OPEN (UNIT=iunit,FILE="TA15_input",ACTION="read")

C Skip first line with column names
      read(iunit,*) 

      DO
      read(iunit,*,IOSTAT=Status) ID,PDYN,B0y,B0z,XIND,VGSEX,VGSEY,VGSEZ
        
        write(*, *) 'generating file', ID

C       CALL RECALC_08 (1997,350,0,0,0,VGSEX,VGSEY,VGSEZ)

        PS    = 0.D0
        BXGSW = 0.D0
        BYGSW = 0.D0
        BZGSW = 0.D0    

        IF (Status < 0) THEN
          ! In case end of file is reached
          EXIT
        END IF
C Specify name of output file
        IF (ID < 10) THEN
          write (filename, "(A3,I1,I1,A4)") "OUT",0,ID,".DAT"
        ELSE
          write (filename, "(A3,I2,A4)") "OUT",ID,".DAT"
        ENDIF

        PSI=0.
        SPS=SIN(PSI)
        CPS=COS(PSI)

        IOPT=0
C           (IN THIS EXAMPLE IOPT IS JUST A DUMMY PARAMETER,
C                 WHOSE VALUE DOES NOT MATTER)
        PARMOD(1) = PDYN
        PARMOD(2) = B0y
        PARMOD(3) = B0z
        PARMOD(4) = XIND
        PARMOD(5:10) = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
        PS = 0.D0

        IOPT=0
C           (IN THIS EXAMPLE IOPT IS JUST A DUMMY PARAMETER,
C                 WHOSE VALUE DOES NOT MATTER)
c

        OPEN (UNIT=ounit,FILE="output/"//filename,ACTION="write",
     *        STATUS="replace")

        DO k = 1, DIMZ
          DO i = 1, DIMX
            CALL T04_s (IOPT,PARMOD,PS,
     *                     XGSW(i,k),0.D0,ZGSW(i,k),
     *                     BXGSW,BYGSW,BZGSW)
C -- Routines to include internal B field:
C            CALL DIP_08 (REAL(XGSW(i,k)),0.0,REAL(ZGSW(i,k)),
C     *                   HXGSW,HYGSW,HZGSW)
            CALL IGRF_GSW_08 (REAL(XGSW(i,k)),0.0,REAL(ZGSW(i,k)),
     *                   HXGSW,HYGSW,HZGSW)
C --
          WRITE(ounit,*) XGSW(i,k),0.D0,ZGSW(i,k),
     *                   BXGSW+DBLE(HXGSW),BYGSW+DBLE(HYGSW),
     *                   BZGSW+DBLE(HZGSW)
        ENDDO
      ENDDO

      CLOSE(ounit)
      ENDDO
      END

