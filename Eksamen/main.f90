PROGRAM main
        USE WPmod       ! Contains all global parameters as well as WP
        USE fractalmod  ! Contains all functions in the creation of the fractals
        USE openmod     ! Opens and tests if files are succesfully opened
        USE plotmod     ! Contains routines for plotting all graphs
        USE eigenmod    ! Contains routines for eigenproblem
        IMPLICIT NONE
        CALL MakeGrid
  !      CALL openfile('test.dat', test)
        CALL openfile('test2.dat', test2)
        CALL openfile('Grid3D.dat', grid3d)
        CALL openfile('EFractal2D.dat', efractal)
!        CALL CreatePoints
        CALL MainFracRoutine
        CALL MakeSquare
        CALL ExtendFA
        CALL CreateMatrix
        CALL Indexise
        CALL CreateAMatrix
        CALL plotExtendedFractal2D
        ALLOCATE (WR(N))
        CALL SolveEP
        CALL BuildBack
        CALL plotGrid3D(kLambda,2)
        x = Delta
        y = Delta
        k = 0
        DO i = 1,GridN
                DO j = 1,GridN
 !                       WRITE(test2,*) AMatrix(i,:)
                 !       WRITE(test2,*) WR(i)
 !                       PRINT *, AMatrix(i,:)
                         WRITE (test2,*) x, y, GridMatrixR(i,j)
                        x = x+Delta
                END DO
                x = Delta
                y = y+Delta
                WRITE(test2,*)
        END DO
END PROGRAM
