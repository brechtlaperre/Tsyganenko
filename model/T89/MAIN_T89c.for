C**************************************************
C
      PROGRAM T89_RUNNER
      IMPLICIT NONE
      REAL :: AA(10),SPS,CPS,BB(3),PSI,CC(18)
      COMMON /GEOPACK1/ AA,SPS,CPS,BB,PSI,CC
C
      EXTERNAL DIP_08
      EXTERNAL IGRF_GSW_08
      EXTERNAL T89D_DP

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
      REAL*8, DIMENSION(DIMX, DIMZ) :: XGSW
      REAL*8, DIMENSION(DIMX, DIMZ) :: YGSW
      REAL*8, DIMENSION(DIMX, DIMZ) :: ZGSW
C
      REAL*8 :: Xbeg = -40.D0
      REAL*8 :: Ybeg = 0.D0
      REAL*8 :: Zbeg = -30.D0
      REAL*8 :: dx = 0.1D0
      REAL*8 :: dy = 0.0D0
      REAL*8 :: dz = 0.1D0
C
C Define parameters
C      
      REAL*8, DIMENSION(10) :: PARMOD
      REAL*8 :: PS    = 0.D0
      REAL*8 :: BXGSW = 0.D0
      REAL*8 :: BYGSW = 0.D0
      REAL*8 :: BZGSW = 0.D0
      REAL :: DXGSW = 0.D0
      REAL :: DYGSW = 0.D0
      REAL :: DZGSW = 0.D0
      REAL :: IXGSW = 0.D0
      REAL :: IYGSW = 0.D0
      REAL :: IZGSW = 0.D0
C
C Define inputparameters
C
      INTEGER :: IOPT
      REAL :: PDYN
      REAL :: TI
      REAL :: B0y
      REAL :: B0z
      REAL :: XIND
      REAL :: VGSEX
      REAL :: VGSEY
      REAL :: VGSEZ
C
      INTEGER :: status 
      INTEGER :: ID
      CHARACTER(len=10) :: filename
      CHARACTER(len=10) :: inputfile

      print *, '  enter filename of input'
      read*, inputfile
C
      DO k = 1, DIMZ
C        DO j = 1, DIMY
          DO i = 1, DIMX  
            XGSW(i, k) = Xbeg + (i-1)*dx
C            YGSW(i, k) = Ybeg + (j-1)*dy
            ZGSW(i, k) = Zbeg + (k-1)*dz
          ENDDO
C        ENDDO
      ENDDO
C
      OPEN (UNIT=iunit,FILE=inputfile,ACTION="read")
      READ(iunit,*)
C Skip first line with column names
      DO
        read(iunit,*,IOSTAT=status) ID,VGSEX,VGSEY,VGSEZ,
     *  PDYN,B0y,B0z,XIND,TI,IOPT
        
        IF (status < 0) THEN
          ! In case end of file is reached
          EXIT
        END IF

        write(*, *) 'Generating output file ', ID

        CALL RECALC_08 (2004,129,9,0,0,VGSEX,VGSEY,VGSEZ)
C reset parameters
        BXGSW = 0.D0
        BYGSW = 0.D0
        BZGSW = 0.D0
C Reset dipole tilt to custom value
C        PSI = TI
C        PS  = TI
C        SPS = SIN(PSI)
C        CPS = COS(PSI)
C Set input parameters. Parmod has no meaning here. 
        PARMOD(1) = PDYN
        PARMOD(2) = B0y
        PARMOD(3) = B0z
        PARMOD(4) = XIND
        PARMOD(5:10) = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
C Specify name output file
        IF (ID < 10) THEN
          write (filename, "(A3,I1,I1,A4)") "OUT",0,ID,".DAT"
        ELSE
          write (filename, "(A3,I2,A4)") "OUT",ID,".DAT"
        ENDIF

C Compute fields
        OPEN (UNIT=ounit,FILE="output/"//filename,ACTION="write",
     *        STATUS="replace")
C
        DO k = 1, DIMZ
          DO i = 1, DIMX
                CALL T89D_DP (IOPT,PARMOD,PS,
     *                     XGSW(i,k),0.D0,ZGSW(i,k),
     *                     BXGSW,BYGSW,BZGSW)
C -- Routines to include internal B field:
                CALL DIP_08 (REAL(XGSW(i,k)),0.D0,
     *                 REAL(ZGSW(i,k)), DXGSW,DYGSW,DZGSW)
                CALL IGRF_GSW_08 (REAL(XGSW(i,k)),0.D0,
     *                   REAL(ZGSW(i,k)),IXGSW,IYGSW,IZGSW)
C -- Save output to file
                WRITE(ounit,*) XGSW(i,k),0.D0,ZGSW(i,k),
     *                   BXGSW+DBLE(DXGSW)+DBLE(IXGSW),
     *                   BYGSW+DBLE(DYGSW)+DBLE(IYGSW),
     *                   BZGSW+DBLE(DZGSW)+DBLE(IZGSW)
              ENDDO
          ENDDO

        CLOSE(ounit)
      ENDDO
      END
      



