PROGRAM main
        USE WPmod       ! Contains all global parameters as well as WP
        USE fractalmod  ! Contains all functions in the creation of the fractals
        USE openmod     ! Opens and tests if files are succesfully opened
        USE plotmod     ! Contains routines for plotting all graphs
        USE eigenmod    ! Contains routines for eigenproblem
        IMPLICIT NONE
        CALL MakeGrid
        CALL openfile('test.dat', test)
        CALL openfile('test2.dat', test2)
!        CALL CreatePoints
        CALL MainFracRoutine
        CALL MakeSquare
        CALL ExtendFA
        CALL CreateMatrix
        CALL Indexise
        CALL FindNOFL
    !    CALL CreateAMatrix
        CALL CreateAMatrix2
        CALL FindShortesDiag
 !       shortestDiag = 128
        CALL BuildLAPACKMatrix
        ALLOCATE (WR(N))
        PRINT *, 'REACHED SOLVER'
        CALL SolveEP
        DO kLambda = 1,10
                PRINT *, kLambda
                CALL openfile('Grid3D.dat', grid3d)
                CALL openfile('EFractal2D.dat', efractal)
                CALL BuildBack
                CALL plotExtendedFractal2D
                CALL plotGrid3D(kLambda,2)
                DEALLOCATE(GridMatrixR)
                CLOSE(grid3d)
                CLOSE(efractal)
        END DO
        x = Delta
        y = Delta
        k = 0
        DO i = 1,ShortestDiag
                DO j = 1,N
!                        WRITE(test2,*) AMatrix(i,j)
                        WRITE(test2,*) LAPACKMatrix(i,j)
                  !      WRITE(test2,*) Z(i,j)
                 !       WRITE(test2,*) WR(i)
 !                       PRINT *, AMatrix(i,:)
 !                        WRITE (test2,*) x, y, GridMatrixR(i,j)
                        x = x+Delta
                END DO
                x = Delta
                y = y+Delta
                WRITE(test2,*)
        END DO
        DO i = 1,10
                PRINT *, W(i)/(Delta**4)
        END DO
END PROGRAM
