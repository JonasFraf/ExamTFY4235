MODULE eigenmod
USE WPmod
!USE F95_LAPACK, ONLY: LA_SYEVD
USE F95_LAPACK, ONLY: LA_SBEVX
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
                                ELSE IF(GridMatrix(Ii,Ij) == 1) THEN
                                        IndexMatrix(Ii,Ij) = -1
                                        ! Makes the boundary -1
                                END IF
                        END DO
                END DO
                N = ICounter-1
        END SUBROUTINE
        SUBROUTINE FindNOFL()
        ! Finds number of flat lines next to each point inside the border
                INTEGER         :: FNi, FNj
                ALLOCATE(NOFLArray(N))
                NOFLArray = 0
                DO FNi = 1,GridN
                        DO FNj = 1,GridN
                                IF (IndexMatrix(FNi,FNj) > 0) THEN
                                ! Doesn't need to worry about +2 or -1
                                ! because if we are within the borders the
                                ! edge of grid is always less or equal to 2 away
                                        IF (IndexMatrix(FNi, FNj+2) == 0) THEN
                                                NOFLArray(IndexMatrix(FNi,FNj))=&
                                                NOFLArray(IndexMatrix(FNi,FNj))+1
                                        END IF
                                        IF (IndexMatrix(FNi, FNj-2) == 0) THEN
                                                NOFLArray(IndexMatrix(FNi,FNj))=&
                                                NOFLArray(IndexMatrix(FNi,FNj))+1

                                        END IF
                                        IF (IndexMatrix(FNi-2, FNj) == 0) THEN
                                                NOFLArray(IndexMatrix(FNi,FNj))=&
                                                NOFLArray(IndexMatrix(FNi,FNj))+1

                                        END IF
                                        IF (IndexMatrix(FNi+2, FNj) == 0) THEN
                                                NOFLArray(IndexMatrix(FNi,FNj))=&
                                                NOFLArray(IndexMatrix(FNi,FNj))+1
                                        END IF
                                END IF
                        END DO
                END DO
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
                DO CAMi = 2, GridN-1
                        DO CAMj = 2, GridN-1
                                IF (IndexMatrix(CAMi,CAMj) <= 0) THEN
                                        CYCLE
                                END IF
                                CAMC = IndexMatrix(CAMi, CAMj)
                                CAMN = IndexMatrix(CAMi,CAMj+1)
                                CAMS = IndexMatrix(CAMi,CAMj-1)
                                CAME = IndexMatrix(CAMi+1,CAMj)
                                CAMW = IndexMatrix(CAMi-1,CAMj)
                                AMatrix(CAMC, CAMC) = 4
                                PVec(CAMC) = CMPLX(CAMi,CAMj)
                                IF (CAMN > 0) THEN
                                        AMatrix(CAMN,CAMC) = -1
                                END IF
                                IF (CAMS > 0) THEN
                                        AMatrix(CAMS,CAMC) = -1
                                END IF
                                IF (CAME > 0) THEN
                                        AMatrix(CAME,CAMC) = -1
                                END IF
                                IF (CAMW > 0) THEN
                                        AMatrix(CAMW,CAMC) = -1
                                END IF
                        END DO
                END DO

        END SUBROUTINE
        SUBROUTINE CreateAMatrix2()
                INTEGER         :: CAMi, CAMj
                ! Itterators
                INTEGER         :: CAMN, CAMS, CAMW, CAME, CAMC
                ! Keep track of the neighbours, and center
                ! North, South, East, West
                INTEGER         :: CAMN2, CAMS2, CAMW2, CAME2
                INTEGER         :: CAMNE, CAMSE, CAMNW, CAMSW
                ALLOCATE(Uvec(N))
                ALLOCATE(AMatrix(N,N))
                ALLOCATE(PVec(N))
                AMatrix = 0
                DO CAMi = 2, GridN-1
                        DO CAMj = 2, GridN-1
                                IF (IndexMatrix(CAMi,CAMj) <= 0) THEN
                                        CYCLE
                                END IF
                                CAMC = IndexMatrix(CAMi, CAMj)
                                CAMN = IndexMatrix(CAMi,CAMj+1)
                                CAMS = IndexMatrix(CAMi,CAMj-1)
                                CAME = IndexMatrix(CAMi+1,CAMj)
                                CAMW = IndexMatrix(CAMi-1,CAMj)
                                CAMN2 = IndexMatrix(CAMi,CAMj+2)
                                CAMS2 = IndexMatrix(CAMi,CAMj-2)
                                CAME2 = IndexMatrix(CAMi+2,CAMj)
                                CAMW2 = IndexMatrix(CAMi-2,CAMj)
                                CAMNE = IndexMatrix(CAMi+1,CAMj+1)
                                CAMNW = IndexMatrix(CAMi-1,CAMj+1)
                                CAMSE = IndexMatrix(CAMi+1,CAMj-1)
                                CAMSW = IndexMatrix(CAMi-1,CAMj-1)
                                AMatrix(CAMC, CAMC) = 20+NOFLArray(CAMC)
                                PVec(CAMC) = CMPLX(CAMi,CAMj)
                                ! Testing if we are inside the fractal boundary
                                IF (CAMN > 0) THEN
                                        AMatrix(CAMN,CAMC) = -8
                                END IF
                                IF (CAMS > 0) THEN
                                        AMatrix(CAMS,CAMC) = -8
                                END IF
                                IF (CAME > 0) THEN
                                        AMatrix(CAME,CAMC) = -8
                                END IF
                                IF (CAMW > 0) THEN
                                        AMatrix(CAMW,CAMC) = -8
                                END IF
                                IF (CAMN2 > 0) THEN
                                        AMatrix(CAMN2,CAMC) = 1
                                END IF
                                IF (CAMS2 > 0) THEN
                                        AMatrix(CAMS2,CAMC) = 1
                                END IF
                                IF (CAME2 > 0) THEN
                                        AMatrix(CAME2,CAMC) = 1
                                END IF
                                IF (CAMW2 > 0) THEN
                                        AMatrix(CAMW2,CAMC) = 1
                                END IF
                                IF (CAMNE > 0) THEN
                                        AMatrix(CAMNE,CAMC) = 2
                                END IF
                                IF (CAMNW > 0) THEN
                                        AMatrix(CAMNW,CAMC) = 2
                                END IF
                                IF (CAMSE > 0) THEN
                                        AMatrix(CAMSE,CAMC) = 2
                                END IF
                                IF (CAMSW > 0) THEN
                                        AMatrix(CAMSW,CAMC) = 2
                                END IF
                      END DO
                END DO
              

        END SUBROUTINE

        SUBROUTINE FindShortesDiag()
        ! WIll find the Shortest diagonal in order to change the LAPACK routine
                INTEGER                 ::FLDi, FLDj
                ! Itterators
                INTEGER                 :: FLDDiag
                ! Temporary diagonal
                ShortestDiag = 0
                DO FLDi = 1, N
                        DO FLDj = 1, N
                                IF (AMatrix(FLDi,FLDj) /= 0) THEN
                                        IF (ABS(FLDi-FLDj)+1> ShortestDiag) THEN
                                                ShortestDiag = ABS(FLDi-FLDj)+1

                                        END IF
                                END IF
                        END DO
                END DO
                PRINT *, ShortestDiag, N
        END SUBROUTINE
        SUBROUTINE BuildLAPACKMatrix()
        ! This subroutine will build a Matrix which will be used in routine
        ! LA_SBEVX
                INTEGER         :: BLMi, BLMj, BLMC
                ! Itterators, and a counter
                ALLOCATE(LAPACKMatrix(ShortestDiag,N))
                DO BLMi = 1, ShortestDiag
                        BLMC = BLMi
                        DO BLMj = 1, N
                                IF(N-BLMj >= BLMC-1) THEN
                                        LAPACKMatrix(BLMi,BLMj) = Amatrix(BLMj+BLMC-1,BLMj)
                                END IF
                        END DO
                END DO
        END SUBROUTINE

        SUBROUTINE SolveEP()
        ! Solves the eigenvalueproblem
!                CALL LA_SYEVD(AMatrix,WR,'V','L')
                INTEGER :: M
                M = 10
                ALLOCATE(W(N))
                ALLOCATE(Z(N,N))
                CALL LA_SBEVX(LAPACKMatrix, W, 'L',Z=Z,IL = 1,IU=N, M=M)
                PRINT *, 'DONE'
        END SUBROUTINE
        
        SUBROUTINE BuildBack()
                INTEGER         :: BBi

!                PVec
                ALLOCATE(GridMatrixR(GridN,GridN))
                GridMatrixR = 0

                DO BBi = 1, N
                        GridMatrixR(NINT(REAL(PVec(BBi))),NINT(IMAG(PVec(BBi))))&
                        = Z(BBi,kLambda)
                END DO


        END SUBROUTINE
END MODULE
