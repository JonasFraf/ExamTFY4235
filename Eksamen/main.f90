PROGRAM main
        USE WPmod
        USE fractalmod
        USE openmod
        IMPLICIT NONE
        CALL MakeGrid
        CALL openfile('test.dat', test)
        CALL openfile('test2.dat', test2)
 !       CALL CreatePoints
        CALL MainFracRoutine
        CALL MakeSquare
  !      DO i = 1, 9
!        DO i = 1,FracN
        PRINT *, MGXmin
        PRINT *, MGYmin
        DO i = 1,FracN*4
 !               WRITE(test,*) REAL(FractalArray(i)),IMAG(FractalArray(i))
 !               WRITE(test,*) (REAL(FractalSArray(i))-MGXmin)*Delta,(IMAG(FractalSArray(i)) - MGYmin)*Delta
!                WRITE(test,*) REAL(FractalSArray(i))*Delta,IMAG(FractalSArray(i))*Delta
        END DO
        CALL ExtendFA

        DO i = 1, FracN*8
 !               WRITE(test,*) REAL(FractalSArrayE(i))*Delta,IMAG(FractalSArrayE(i))*Delta
                WRITE(test,*) REAL(FractalSArrayE(i)),IMAG(FractalSArrayE(i))

        END DO


        IF (ANY(FractalSArray == CMPLX(0,0))) THEN
                PRINT *, 'HAHA, WORKED'
        END IF
        CALL CreateMatrix
        x = 0
        y = Delta
        DO i = 1, NTot*2
                x = x+Delta
                DO j = 1, NTot*2
                        WRITE (test2,*) x,y,GridMatrix(i,j)
                        y = y+Delta
                END DO
                WRITE (test2,*) 
                y = Delta
        END DO



END PROGRAM
