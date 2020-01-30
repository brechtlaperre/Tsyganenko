C**************************************************
C
      PROGRAM T89_RUNNER
      COMMON /GEOPACK1/ AA(10),SPS,CPS,BB(3),PSI,CC(18)
C
      EXTERNAL T89D_DP, DIP_08, IGRF_GSW_08

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
      DIMENSION PARMOD(10)
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
        OPEN (UNIT=ounit,FILE=outfolder//"/"//filename,ACTION="write",
     *        STATUS="replace")
C
        DO k = 1, DIMZ
          DO j = 1, DIMY
            DO i = 1, DIMX
              CALL T89D_DP (IOPT,PARMOD,PS,
     *                     XGSW(i),YGSW(j),ZGSW(k),
     *                     BXGSW,BYGSW,BZGSW)
C -- Routines to include internal B field:
                CALL DIP_08 (REAL(XGSW(i)),REAL(YGSW(j)),
     *                 REAL(ZGSW(k)), DXGSW,DYGSW,DZGSW)
                CALL IGRF_GSW_08 (REAL(XGSW(i)),REAL(YGSW(j)),
     *                   REAL(ZGSW(k)),IXGSW,IYGSW,IZGSW)
C -- Save output to file
              WRITE(ounit,*) XGSW(i),YGSW(j),ZGSW(k),
C     *                   BXGSW, BYGSW, BZGSW
     *                   BXGSW+DBLE(DXGSW)+DBLE(IXGSW),
     *                   BYGSW+DBLE(DYGSW)+DBLE(IYGSW),
     *                   BZGSW+DBLE(DZGSW)+DBLE(IZGSW)
              ENDDO
            ENDDO
          ENDDO

        CLOSE(ounit)
      ENDDO
      END
      



