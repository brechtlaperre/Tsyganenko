C******************************************************************************
C
      PROGRAM T96FROMFILE

C  Unlike in the EXAMPLE1, here we "manually" specify the tilt angle and its sine/cosine.
c  To forward them to the coordinate transformation subroutines, we need to explicitly
c  include the common block /GEOPACK1/:

C
      COMMON /GEOPACK1/ AA(10),SPS,CPS,BB(3),PSI,CC(18)
C 
c be sure to include an EXTERNAL statement with the names of (i) a magnetospheric
c external field model and (ii) Earth's internal field model.
c
      EXTERNAL T96_01, DIP_08
C
C  X,Y,Z Locations
C

      DIMENSION PARMOD(10)

      INTEGER, PARAMETER :: iunit = 20
      INTEGER, PARAMETER :: ounit = 21
      INTEGER, PARAMETER :: NX    = 319
      INTEGER, PARAMETER :: NY    = 2
      INTEGER, PARAMETER :: NZ    = 209

C  Parameters T96_01
C  Old param
C     REAL, PARAMETER :: PDYN = 1.15
C     REAL, PARAMETER :: Dst  = -16.0 
C     REAL, PARAMETER :: B0y  = 0.0
C     REAL, PARAMETER :: B0z  = 3.8
c      REAL, PARAMETER :: B0z  = 0.0

      REAL :: BXGSW = 0.0
      REAL :: BYGSW = 0.0
      REAL :: BZGSW = 0.0
      REAL :: HXGSW = 0.0
      REAL :: HYGSW = 0.0
      REAL :: HZGSW = 0.0

      REAL :: XGSW
      REAL :: YGSW
      REAL :: ZGSW
      REAL :: X, Y, Z
      REAL :: D2

      REAL, PARAMETER :: RE2 = 1.0
      REAL, PARAMETER :: LX = 82.9383622034
      REAL, PARAMETER :: LY = 0.0
      REAL, PARAMETER :: LZ = 54.24899163
      REAL, PARAMETER :: Xbeg =-55.338362203412586
      REAL, PARAMETER :: Ybeg = 0.0
      REAL, PARAMETER :: Zbeg =-27.124495815

      REAL :: dx = LX/(NX-1)
      REAL :: dy = 0.0
c      REAL :: dy = LY/(NY-1)
      REAL :: dz = LZ/(NZ-1)

      REAL :: PDYN
      REAL :: Dst
      REAL :: B0y
      REAL :: B0z

      INTEGER :: ID
      CHARACTER(len=10) :: filename
      INTEGER :: Status

C
C   First, call RECALC_08, to define the main field coefficients and, hence, the magnetic
C      moment of the geodipole for IYEAR=1997 and IDAY=350.
C   The universal time and solar wind direction does not matter in this example, 
C   because here we explicitly specify the tilt angle (hence, the orientation of 
C   dipole in the GSW coordinates), so we arbitrarily set IHOUR=MIN=ISEC=0 and 
C   VGSEX=-400.0, VGSEY=VGSEZ=0 (any other values would be equally OK):
C
      CALL RECALC_08 (1997,350,0,0,0,-302.893,0.0,0.0)
c
c   Enter input parameters for T96_01:
c

      OPEN (UNIT=iunit,FILE="T96_input",ACTION="read")
      read(iunit,*) ! skip header of the file

      DO 
        read(iunit,*,IOSTAT=Status) ID, PDYN, DST, B0y, B0z

        IF (Status < 0) THEN
! In case end of file is reached
          EXIT
        END IF

        IF (ID < 10) THEN
          write (filename, "(A3,I1,I1,A4)") "OUT",0,ID,".DAT"
        ELSE
          write (filename, "(A3,I2,A4)") "OUT",ID,".DAT"
        ENDIF

        PARMOD(1) = PDYN
        PARMOD(2) = Dst
        PARMOD(3) = B0y
        PARMOD(4) = B0z
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
c
c  Trace the field line:
c
        OPEN (UNIT=ounit,FILE=filename,ACTION="write",
     *        STATUS="replace")

        DO k = 1, NZ
          DO j = 1, NY
            DO i = 1, NX

              XGSW = Xbeg + (i-1)*dx
              YGSW = Ybeg + (i-1)*dy
              ZGSW = Zbeg + (k-1)*dz

              R2 = XGSW*XGSW 
     *         + YGSW*YGSW 
     *         + ZGSW*ZGSW

              X = XGSW
              Y = YGSW
              IF (R2.LT.RE2) THEN
                D2 = XGSW*XGSW+YGSW*YGSW
                Z  = SQRT(D2 + RE2)
              ELSE
                Z = ZGSW
              ENDIF

              CALL T96_01 (IOPT,PARMOD,PSI,X,Y,Z,BXGSW,BYGSW,BZGSW)

C -- Routines to include internal B field:
c            CALL IGRF_GSW_08 (XGSW,YGSW,ZGSW,
c     *                   HXGSW,HYGSW,HZGSW)
c            CALL DIP_08 (XGSW,YGSW,ZGSW,
c     *                   HXGSW,HYGSW,HZGSW)
C --
              WRITE(ounit,*) XGSW,YGSW,ZGSW,
     *                     BXGSW+HXGSW,BYGSW+HYGSW,BZGSW+HZGSW
            ENDDO
          ENDDO
        ENDDO
        CLOSE(ounit)
      ENDDO

      END

