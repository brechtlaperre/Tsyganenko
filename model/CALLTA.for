       PROGRAM CALLTA
       IMPLICIT NONE

       REAL*8, DIMENSION(10) :: PARMOD
       REAL*8  :: PS
       INTEGER :: IOPT
       INTEGER :: i, j, k
       INTEGER, PARAMETER :: ounit=20

       REAL*8, DIMENSION(192,1,192) :: BX
       REAL*8, DIMENSION(192,1,192) :: BY
       REAL*8, DIMENSION(192,1,192) :: BZ
       REAL*8, DIMENSION(192,1,192) :: X
       REAL*8, DIMENSION(192,1,192) :: Y
       REAL*8, DIMENSION(192,1,192) :: Z
       REAL*8 :: Xbeg = -9.6D0
       REAL*8 :: Ybeg = 0.0D0
       REAL*8 :: Zbeg = -9.6D0
       REAL*8 :: dx = 0.1D0
       REAL*8 :: dy = 0.0D0
       REAL*8 :: dz = 0.1D0

       DO k = 1, 192
         DO j = 1, 192
           DO i = 1, 192
             X (i, j, k) = Xbeg + (i-1)*dx
             Y (i, j, k) = Ybeg + (j-1)*dy
             Z (i, j, k) = Zbeg + (k-1)*dz
             BX(i, j, k) = 0.0
             BY(i, j, k) = 0.0
             BZ(i, j, k) = 0.0
           ENDDO
         ENDDO
       ENDDO

       PARMOD = (/ 1.15, 0.0, 3.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
       PS = 0.D0

       OPEN (unit=ounit,FILE="Bxy.dat",ACTION="write",STATUS="replace")

       DO k = 1, 192
         PRINT*, " Doing k: ", k, "/", 192
         DO j = 1, 192
           PRINT*, "     k,j: ", k, j, "/", 192
           DO i = 1, 192
             CALL TA_2015_B (IOPT,PARMOD,PS,
     *                      X (i,j,k),Y (i,j,k),Z (i,j,k),
     *                      BX(i,j,k),BY(i,j,k),BZ(i,j,k))
             WRITE(ounit,*) X (i,j,k), Y(i,j,k), Z(i,j,k),
     *                      BX(i,j,k),BY(i,j,k),BZ(i,j,k)
           ENDDO
         ENDDO
       ENDDO

       CLOSE (ounit)

       END PROGRAM
