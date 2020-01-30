C**************************************************
C
      PROGRAM T96_RUNNER
C      IMPLICIT NONE
      COMMON /GEOPACK1/ AA(10),SPS,CPS,BB(3),PSI,CC(18)
C

      EXTERNAL T96_01, DIP_08

C Define IO parameters
      INTEGER, PARAMETER :: iunit = 21
      INTEGER, PARAMETER :: ounit = 20
      INTEGER :: i, j, k
C
C DEFINE GRID
      INTEGER, PARAMETER :: DIMX = 600
      INTEGER, PARAMETER :: DIMY = 1
      INTEGER, PARAMETER :: DIMZ = 600
C
      REAL*8, DIMENSION(DIMX) :: XGSW
      REAL*8, DIMENSION(DIMY) :: YGSW
      REAL*8, DIMENSION(DIMZ) :: ZGSW
C
C Standard start: Xbeg = -40.D0, Zbeg = -30.D0
      REAL*8 :: Xbeg = -40.D0
      REAL*8 :: Ybeg = 0.D0
      REAL*8 :: Zbeg = -30.D0
      REAL*8 :: dx = 0.1D0
      REAL*8 :: dy = 0.1D0
      REAL*8 :: dz = 0.1D0
C
C Define parameters
C      
      REAL, DIMENSION(10) :: PARMOD
      REAL*8 :: PS    = 0.D0
      REAL :: BXGSW = 0.D0
      REAL :: BYGSW = 0.D0
      REAL :: BZGSW = 0.D0
      REAL :: DXGSW = 0.D0
      REAL :: DYGSW = 0.D0
      REAL :: DZGSW = 0.D0
      REAL :: IXGSW = 0.D0
      REAL :: IYGSW = 0.D0
      REAL :: IZGSW = 0.D0
C
C Define inputparameters
C
      INTEGER :: IOPT = 0
      REAL :: PDYN
      REAL :: TI
      REAL :: B0y
      REAL :: B0z
      REAL :: DST
      REAL :: XIND
      REAL :: VGSEX
      REAL :: VGSEY
      REAL :: VGSEZ
C
      INTEGER :: status 
      INTEGER :: ID
      CHARACTER(len=10) :: filename
      CHARACTER(len=10) :: inputfile
      CHARACTER(len=6) :: outfolder

      print *, '  enter filename of input'
      read*, inputfile
C
      IF (inputfile == 'reference') THEN
            outfolder = 'refout'
      ELSE
            outfolder = 'output'
      ENDIF
C
      DO k = 1, DIMZ
        DO j = 1, DIMY
          DO i = 1, DIMX  
            XGSW(i) = Xbeg + (i-1)*dx
            YGSW(j) = Ybeg + (j-1)*dy
            ZGSW(k) = Zbeg + (k-1)*dz
          ENDDO
        ENDDO
      ENDDO
C
      OPEN (UNIT=iunit,FILE=inputfile,ACTION="read")
      READ(iunit,*)
C Skip first line with column names
      DO
        read(iunit,*,IOSTAT=status) ID,VGSEX,VGSEY,VGSEZ,
     *  PDYN,DST,B0y,B0z,XIND,TI,IOPT

      IF (status < 0) THEN
          ! In case end of file is reached
          EXIT
        END IF

        write(*, *) 'Generating output file ', ID

        CALL RECALC_08 (2004,128,0,0,0,VGSEX,VGSEY,VGSEZ)
C reset parameters
        BXGSW = 0.D0
        BYGSW = 0.D0
        BZGSW = 0.D0
C Reset dipole tilt to custom value
        PSI = TI
        PS  = TI
        SPS = SIN(PSI)
        CPS = COS(PSI)
C Set input parameters. Parmod has no meaning here. 
        PARMOD(1) = PDYN
        PARMOD(2) = DST
        PARMOD(3) = B0y
        PARMOD(4) = B0z
C        PARMOD(5:10) = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
C Specify name output file
        IF (ID < 10) THEN
          write (filename, "(A3,I1,I1,A4)") "OUT",0,ID,".DAT"
        ELSE
          write (filename, "(A3,I2,A4)") "OUT",ID,".DAT"
        ENDIF

C Compute fields
        OPEN (UNIT=ounit,FILE=outfolder//"/"//filename,ACTION="write",
     *        STATUS="replace")
C
        DO k = 1, DIMZ
          DO j = 1, DIMY
            DO i = 1, DIMX
              CALL T96_01 (IOPT,PARMOD,PSI,
     *                     REAL(XGSW(i)),REAL(YGSW(j)),REAL(ZGSW(k)),
     *                     BXGSW,BYGSW,BZGSW)
C -- Routines to include internal B field:
C                CALL DIP_08 (REAL(XGSW(i)),REAL(YGSW(j)),
C     *                 REAL(ZGSW(k)),DXGSW,DYGSW,DZGSW)
C                CALL IGRF_GSW_08 (REAL(XGSW(i)),REAL(YGSW(j)),
C     *                   REAL(ZGSW(k)),IXGSW,IYGSW,IZGSW)
C -- Save output to file
              WRITE(ounit,*) XGSW(i),YGSW(j),ZGSW(k),     
     *                   BXGSW+DBLE(DXGSW)+IXGSW,
     *                   BYGSW+DBLE(DYGSW)+IYGSW,
     *                   BZGSW+DBLE(DZGSW)+IZGSW
              ENDDO
            ENDDO
          ENDDO

        CLOSE(ounit)
      ENDDO
      END
      



