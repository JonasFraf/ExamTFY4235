MODULE eigenmod
USE WPmod
USE F95_LAPACK, ONLY: LA_SYEVD
IMPLICIT NONE

CONTAINS


        SUBROUTINE Indexise()
        ! Creates IndexMatrix
                INTEGER         :: Ii, Ij
                COMPLEX         :: IC1
                INTEGER         :: ICounter = 1                

                ALLOCATE(IndexMatrix(GridN,GridN))
                IndexMatrix = 0
                DO Ii = 1,GridN
                        DO Ij = 1,GridN
                                IC1 = CMPLX(Ii, Ij)
                                IF (GridMatrix(Ii,Ij) == 1 .AND.&
                                    (.NOT. ANY(FractalSArrayE == IC1))) THEN
                                        IndexMatrix(Ii,Ij) = ICounter
                                        ICounter = 1 + ICounter
                                END IF
                        END DO
                END DO
                N = ICounter-1
        END SUBROUTINE



        SUBROUTINE CreateAMatrix()
                INTEGER         :: CAMi, CAMj
                ! Itterators
                INTEGER         :: CAMN, CAMS, CAMW, CAME, CAMC
                ! Keep track of the neighbours, and center
                ALLOCATE(Uvec(N))
                ALLOCATE(AMatrix(N,N))
                ALLOCATE(PVec(N))
                AMatrix = 0
                DO CAMi = 1, GridN
                        DO CAMj = 1, GridN
                                IF (IndexMatrix(CAMi,CAMj) == 0) THEN
                                        CYCLE
                                END IF
                                CAMC = IndexMatrix(CAMi, CAMj)
                                CAMN = IndexMatrix(CAMi,CAMj+1)
                                CAMS = IndexMatrix(CAMi,CAMj-1)
                                CAME = IndexMatrix(CAMi+1,CAMj)
                                CAMW = IndexMatrix(CAMi-1,CAMj)
                                AMatrix(CAMC, CAMC) = 4
                                PVec(CAMC) = CMPLX(CAMi,CAMj)
                                IF (CAMN /= 0) THEN
                                        AMatrix(CAMN,CAMC) = -1
                                END IF
                                IF (CAMS /= 0) THEN
                                        AMatrix(CAMS,CAMC) = -1
                                END IF
                                IF (CAME /= 0) THEN
                                        AMatrix(CAME,CAMC) = -1
                                END IF
                                IF (CAMW /= 0) THEN
                                        AMatrix(CAMW,CAMC) = -1
                                END IF
                        END DO
                END DO

        END SUBROUTINE
        SUBROUTINE SolveEP()
        ! Solves the eigenvalueproblem
                CALL LA_SYEVD(AMatrix,WR,'V','L')
        END SUBROUTINE
        
        SUBROUTINE isSYM()
                DO i = 2, N-1
                        DO j = 2,N-1
                                IF (AMatrix(i-1,j)/=AMatrix(i,j+1).AND. i == j) THEN
                                        PRINT *, 'DAMMIT'
                                END IF
                        END DO
                END DO
        END SUBROUTINE
        SUBROUTINE BuildBack()
                INTEGER         :: BBi

!                PVec
                ALLOCATE(GridMatrixR(GridN,GridN))
                GridMatrixR = 0

                DO BBi = 1, N
                        GridMatrixR(NINT(REAL(PVec(BBi))),NINT(IMAG(PVec(BBi))))&
                        = AMatrix(BBi,kLambda)
                END DO


        END SUBROUTINE
END MODULE
