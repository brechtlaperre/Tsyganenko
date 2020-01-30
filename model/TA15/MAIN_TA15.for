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
      EXTERNAL IGRF_GSW_08
      EXTERNAL TA_2015_B
C
C  X,Y,Z Locations
C
      REAL*8, DIMENSION(10) :: PARMOD

      INTEGER, PARAMETER :: ounit=20
      INTEGER, PARAMETER :: iunit=21
      INTEGER :: i, j, k
      INTEGER, PARAMETER :: DIMX=600
      INTEGER, PARAMETER :: DIMY=1
      INTEGER, PARAMETER :: DIMZ=600
      
      INTEGER, PARAMETER :: LIMR=1.5
      REAL*8 :: D2 = 0.D0
      REAL*8 :: R2 = 0.D0
      REAL*8 :: Z = 0.D0

      REAL*8, DIMENSION(DIMX) :: XGSW
      REAL*8, DIMENSION(DIMY) :: YGSW
      REAL*8, DIMENSION(DIMZ) :: ZGSW

      REAL*8 :: PS    = 0.D0
      REAL*8 :: BXGSW = 0.D0
      REAL*8 :: BYGSW = 0.D0
      REAL*8 :: BZGSW = 0.D0
      REAL   :: HXGSW = 0.D0
      REAL   :: HYGSW = 0.D0
      REAL   :: HZGSW = 0.D0
      REAL   :: IXGSW = 0.D0
      REAL   :: IYGSW = 0.D0
      REAL   :: IZGSW = 0.D0

      REAL*8 :: Xbeg = -40.D0
      REAL*8 :: Ybeg = 0.D0
      REAL*8 :: Zbeg = -30.D0
      REAL*8 :: dx = 0.1D0
      REAL*8 :: dy = 0.D0
      REAL*8 :: dz = 0.1D0

      INTEGER :: IOPT
      REAL :: PDYN
      REAL :: DST
      REAL :: TI
      REAL :: B0y
      REAL :: B0z
      REAL :: XIND
      REAL :: VGSEX
      REAL :: VGSEY
      REAL :: VGSEZ
    
      INTEGER :: status 
      INTEGER :: ID
      CHARACTER(len=10) :: filename
      CHARACTER(len=10) :: inputfile
      CHARACTER(len=6) :: outfolder

      print *, '  enter filename of input'
      read*, inputfile
C Automatic output folder choosing for reference case
      IF (inputfile == 'reference') THEN
            outfolder = 'refout'
      ELSE
            outfolder = 'output'
      ENDIF

C XIND: solar-wind-magnetosphere driving index, 
C Typical values of XIND: between 0 (quiet) and 2 (strongly disturbed)      
      DO k = 1, DIMZ
        ZGSW (k) = Zbeg + (k-1)*dz
      ENDDO
      DO j = 1, DIMY
        YGSW (j) = Ybeg + (j-1)*dy
      ENDDO
      DO i = 1, DIMX
        XGSW (i) = Xbeg + (i-1)*dx
      ENDDO
C
C   First, call RECALC_08, to define the main field coefficients and, hence, the magnetic
C      moment of the geodipole for IYEAR=1997 and IDAY=350.
C   The universal time and solar wind direction does not matter in this example, 
C   because here we explicitly specify the tilt angle (hence, the orientation of 
C   dipole in the GSW coordinates), so we arbitrarily set IHOUR=MIN=ISEC=0 and 
C   VGSEX=-400.0, VGSEY=VGSEZ=0 (any other values would be equally OK):
C
C      CALL RECALC_08 (2004,129,9,0,0,-400.0,0.0,0.0)
C
C  Specify the dipole tilt angle PS, its sine SPS and cosine CPS, entering
c    in the common block /GEOPACK1/:
C

      OPEN (UNIT=iunit,FILE=inputfile,ACTION="read")

C Skip first line with column names
      read(iunit,*) 

      DO
      read(iunit,*,IOSTAT=status) ID,VGSEX,VGSEY,VGSEZ,
     *  PDYN,DST,B0y,B0z,XIND,TI,IOPT
        
        write(*, *) 'generating file', ID

       CALL RECALC_08 (2004,129,9,0,0,VGSEX,VGSEY,VGSEZ)

        PS    = 0.D0
        BXGSW = 0.D0
        BYGSW = 0.D0
        BZGSW = 0.D0

        IF (status < 0) THEN
          ! In case end of file is reached
          EXIT
        END IF
C Specify name of output file
        IF (ID < 10) THEN
          write (filename, "(A3,I1,I1,A4)") "OUT",0,ID,".DAT"
        ELSE
          write (filename, "(A3,I2,A4)") "OUT",ID,".DAT"
        ENDIF

        PSI=TI
        SPS=SIN(PSI)
        CPS=COS(PSI)

        PARMOD(1) = PDYN
        PARMOD(2) = B0y
        PARMOD(3) = B0z
        PARMOD(4) = XIND
        PARMOD(5:10) = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
        PS = TI

        OPEN (UNIT=ounit,FILE=outfolder//"/"//filename,ACTION="write",
     *        STATUS="replace")

        DO k = 1, DIMZ
          DO j = 1, DIMY
            DO i = 1, DIMX

              R2 = XGSW(i)*XGSW(i)+YGSW(j)*YGSW(j)+ZGSW(k)*ZGSW(k)

              IF (R2.LT.LIMR) THEN
                D2 = 1 - XGSW(i)*XGSW(i)-YGSW(j)*YGSW(j)
                Z = SQRT(D2)
              ELSE
                Z = ZGSW(k)
              ENDIF

              CALL TA_2015_B (IOPT,PARMOD,PS,
     *                     XGSW(i),YGSW(j),Z,
     *                     BXGSW,BYGSW,BZGSW)
C -- Routines to include internal B field:
              CALL DIP_08 (REAL(XGSW(i)),REAL(YGSW(j)),
     *          REAL(Z),HXGSW,HYGSW,HZGSW)
C            CALL IGRF_GSW_08 (REAL(XGSW(i,j,k)),0.D0,
C     *                   REAL(ZGSW(i,k)),IXGSW,IYGSW,IZGSW)
C -- Save output to file
                WRITE(ounit,*) XGSW(i),YGSW(j),ZGSW(k),
C     *                   BXGSW, BYGSW, BZGSW
     *                   BXGSW+DBLE(HXGSW),!+DBLE(IXGSW), 
     *                   BYGSW+DBLE(HYGSW),!+DBLE(IYGSW),
     *                   BZGSW+DBLE(HZGSW)!+DBLE(IZGSW)
          ENDDO
        ENDDO
      ENDDO

      CLOSE(ounit)
      ENDDO
      END

