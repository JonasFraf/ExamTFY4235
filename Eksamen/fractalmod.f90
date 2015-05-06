MODULE fractalmod
USE WPmod
IMPLICIT NONE

CONTAINS
        SUBROUTINE CreatePoints()
                ! This is just a tester for the program.
                ALLOCATE(FractalArray(9))
                FractalArray(1) = CMPLX(4,4)
                FractalArray(FracN) = CMPLX(4,0)
                CALL DoLine(FractalArray(1), FractalArray(9), FractalArray)
        END SUBROUTINE

        SUBROUTINE DoLine(DLposStart, DLposEnd, DLArray) ! Is a l1->l2 operation
                COMPLEX, INTENT(IN)                     :: DLposStart, DLposEnd
                ! Start and end position of linesegment
                COMPLEX, DIMENSION(9), INTENT (INOUT)   :: DLArray
                ! The complex points of the linesegment
                COMPLEX, DIMENSION(2)                   :: DLSubArray
                ! The complex points of the branches that are being created
                LOGICAL                                 :: DLisx
                ! Need to know if the change is in x og y direction
                LOGICAL                                 :: DLisposx
                ! Need to know if the change is in positive x first
                LOGICAL                                 :: DLisposy
                ! Need to know if the change is in positive y first
                COMPLEX                                 :: DLdiff
                ! The difference between starting position and end position
                INTEGER                                 :: DLi
                ! Itterator
                DLisx = .FALSE.
                DLisposx = .FALSE.
                DLisposy = .FALSE.
                DLArray(1) = DLposStart
                DLArray(9) = DLposEnd
                DLdiff = DLposEnd - DLposStart
                ! Following "IF" is done in order to know which direction the 
                ! linesegment has.
                IF (IMAG(DLdiff) > 0) THEN
                        DLisx = .TRUE.
                ELSE IF (IMAG(DLdiff) < 0) THEN
                        DLisx = .TRUE.
                        DLisposx = .TRUE.
                ELSE IF (REAL(DLdiff) > 0) THEN
                        DLisposy = .TRUE.
                END IF
                ! This "IF" is done to initiate the points that is needed
                ! to create the branches
                IF (DLisposx) THEN
                        DLArray(2) = DLposStart + CMPLX(0,-1)
                        DLArray(8) = DLposEnd + CMPLX(0,1)
                        DLArray(5) = DLposStart + CMPLX(0,-2)
                ELSE IF (DLisx) THEN
                        DLArray(2) = DLposStart + CMPLX(0,1)
                        DLArray(8) = DLposEnd + CMPLX(0,-1)
                        DLArray(5) = DLposStart + CMPLX(0,2)
                ELSE IF (DLisposy) THEN
                        DLArray(2) = DLposStart + CMPLX(1,0)
                        DLArray(8) = DLposEnd + CMPLX(-1,0)
                        DLArray(5) = DLposStart + CMPLX(2,0)
                ELSE
                        DLArray(2) = DLposStart + CMPLX(-1,0)
                        DLArray(8) = DLposEnd + CMPLX(1,0)
                        DLArray(5) = DLposStart + CMPLX(-2,0)

                END IF
                CALL AddBetween(DLarray(2), DLarray(5), DLisx, DLisposx,&
                                DLisposy, DLSubArray)
                DLArray(3) = DLSubArray(1)
                DLArray(4) = DLsubarray(2)
                ! Now we need to change the direction of the branch
                IF (DLisposx .OR. DLisposy) THEN
                        DLisposx = .FALSE.
                        DLisposy = .FALSE.
                ELSE
                        DLisposx = .TRUE.
                        DLisposy = .TRUE.
                END IF
                        
                CALL AddBetween(DLarray(5), DLarray(8), DLisx, DLisposx,&
                                DLisposy, DLSubArray)
                DLArray(6) = DLSubArray(1)
                DLArray(7) = DLsubarray(2)
        END SUBROUTINE

        SUBROUTINE AddBetween(ABposStart, ABposEnd, ABisx, ABisposx, ABisposy,&
                              ABArray)
        !Creates new points between start and stop
                COMPLEX, INTENT(IN)                     :: ABposStart, ABposEnd
                ! Start and stop of a branch
                LOGICAL, INTENT(IN)                     :: ABisx, ABisposx,&
                                                           ABisposy
                ! Logical values to determine the direction of branch
                COMPLEX, DIMENSION(2), INTENT(INOUT)    :: ABArray
                ! Complex branch points

                ! The branch is born
                IF (ABisx .AND. ABisposx) THEN
                        ABArray(1) = ABposStart + CMPLX(1,0)
                        ABArray(2) = ABposEnd + CMPLX(1,0)
                ELSE IF (ABisx) THEN
                        ABArray(1) = ABposStart + CMPLX(-1,0)
                        ABArray(2) = ABposEnd + CMPLX(-1,0)
                ELSE IF(ABisposy) THEN
                        ABArray(1) = ABposStart + CMPLX(0,1)
                        ABArray(2) = ABposEnd + CMPLX(0,1)
                ELSE
                        ABArray(1) = ABposStart + CMPLX(0,-1)
                        ABArray(2) = ABposEnd + CMPLX(0,-1)
                END IF
        END SUBROUTINE
        
        SUBROUTINE MainFracRoutine()
        ! Creates the fractal, but 1D start
                INTEGER         :: MFRi
                ! Itterator over every linesegment in current level
                INTEGER         :: MFRj
                ! Itterator over elements in previous FractalArray
                INTEGER         :: MFRk
                ! Itterator over elements in linesegment
                COMPLEX, DIMENSION(9)                   :: MFRLineSeg
                COMPLEX, DIMENSION(:), ALLOCATABLE      :: MFRTempArray
                ! Temp array which stores array temporarily

                ! Here the first line is going from l1 to l2
                ALLOCATE(FractalArray(9))
                FractalArray(1) = CMPLX(0,0)
                FractalArray(9) = CMPLX(4,0)
                CALL DoLine(FractalArray(1), FractalArray(9),FractalArray)

                ! IF lDim is 2, then we are done with this 
                IF (lDim < 3) THEN
                        RETURN
                END IF

                ! Itterating thorugh all dimensions we want
                DO lItt = 3,lDim
                        levelN = 8**(lItt-1)+1
                        ALLOCATE(MFRTempArray(levelN))
                        DO MFRj = 1, 8**(lItt-2)+1
                                MFRTempArray(1+(MFRj-1)*8) = FractalArray(MFRj)*4
                        END DO
                        DEALLOCATE(FractalArray)
                        ALLOCATE(FractalArray(levelN))
                        FractalArray = MFRtempArray
                        ! Now the FractalArray is ready to have linesegments
                        ! updated with branches
                        DO MFRi = 1,8**(lItt-2)
                                CALL DoLine(FractalArray(1+(MFRi-1)*8),&
                                            FractalArray(1+(MFRi)*8),&
                                            MFRLineSeg)
                                DO MFRk = 1,7
                                        FractalArray(1+(MFRi-1)*8+MFRk) = &
                                        MFRLineSeg(MFRk+1)
                                END DO
                        END DO
                        DEALLOCATE(MFRTempArray)
                END DO
        END SUBROUTINE
        SUBROUTINE MakeSquare()
        ! Builds the square fractal
                INTEGER         :: MSi
                ! Itterator
                INTEGER         :: MSxMax
                INTEGER         :: MSiMax
                ! This is the maximum value where the linesegments connect
                MSxMax = 4**(lDim-1)
                MSiMax = MSxMax
                ! Here the linesegments are put in order clockwise
                ! around, so that they make up a square
                DO MSi = 1, FracN
                        FractalSArray(MSi) = FractalArray(MSi)
                        FractalSarray(MSi+2*FracN) = &
                                FractalArray(FracN+1-MSi) + CMPLX(0,-MSiMax)
                END DO
                FractalArray = FractalArray*CMPLX(0,-1)+CMPLX(MSxMax,0)
                DO MSi = 1, FracN
                        FractalSArray(MSi+FracN) = FractalArray(MSi)
                        FractalSArray(MSi+3*FracN) = &
                                FractalArray(FracN+1-MSi) + CMPLX(-MSxMax,0)
                END DO
                ! Shift the graph so that it starts at (1,1)
                FractalSArray = FractalSArray - CMPLX(MGXmin-1, MGYmin-1)
        END SUBROUTINE

        SUBROUTINE MakeGrid()
        ! Creates the grid which contains the fractal
                INTEGER         :: MGi, MGj
                ! Itterator

                NTot = INT(1._wp/Delta)
                MGXmin = 0
                MGYmax = 0
                ! This is the case because we branch on every increasing level
                DO MGi = 2, lDim
                        NTot = NTot + INT(1/Delta)*2/4**(MGi-1)
                        MGYmax = MGYmax + INT(1/Delta)/4**(MGi-1)
                        MGXmin = MGXmin - INT(1/Delta)/4**(MGi-1)
                END DO
                MGYmin = MGYmax - NTot
                MGXmax = NTot + MGXmin
 !               PRINT *, MGXmin, MGXmax, MGYmin, MGYmax
        END SUBROUTINE

        SUBROUTINE ExtendFA()
        ! This subroutine will extend the fractalSquareArray in order to get a
        ! finer grid
                INTEGER         :: EFAi
                ! Itterator
                LOGICAL         :: EFAisx, EFAisy
                ! Controls if we move along x or y
                LOGICAL         :: EFAispos
                ! Controls if we move in positive x
                FractalSArray = FractalSArray*2
                EFAisx = .FALSE.
                EFAispos = .FALSE.
                EFAisy = .FALSE.
                DO EFAi = 1, FracMN
                        ! The following "IF" finds out if we move in x or y, and
                        ! if the movement is positive or negative
                        IF (REAL(FractalSArray(EFAi+1))&
                            -REAL(FractalSArray(EFAi))>0) THEN
                                EFAispos = .TRUE.
                                EFAisx = .TRUE.
                        ELSE IF (REAL(FractalSArray(EFAi+1))&
                            -REAL(FractalSArray(EFAi))<0) THEN
                                EFAisx = .TRUE.
                        ELSE IF (IMAG(FractalSArray(EFAi+1))&
                                 -IMAG(FractalSArray(EFAi))>0) THEN
                                EFAispos = .TRUE.
                                EFAisy = .TRUE.
                        ELSE IF (IMAG(FractalSArray(EFAi+1))&
                                 -IMAG(FractalSArray(EFAi))<0) THEN
                                EFAisy = .TRUE.
                        END IF
                        ! The following "IF" does the changes and moves in the
                        ! correct position
                        FractalSArrayE(EFAi*2-1) = FractalSArray(EFAi)
                        IF (EFAispos .AND. EFAisx) THEN
                                FractalSArrayE(EFAi*2) = FractalSArray(EFAi)&
                                                        + CMPLX(1,0)
                        ELSE IF (EFAisx) THEN
                                FractalSArrayE(EFAi*2) = FractalSArray(EFAi)&
                                                        - CMPLX(1,0)
                        ELSE IF (EFAispos) THEN
                                FractalSArrayE(EFAi*2) = FractalSArray(EFAi)&
                                                        + CMPLX(0,1)
                        ELSE IF (EFAisy) THEN
                                FractalSArrayE(EFAi*2) = FractalSArray(EFAi)&
                                                        - CMPLX(0,1)
                        ELSE
                                FractalSArrayE(EFAi*2) = FractalSArray(EFAi)
                        END IF
                        EFAisx = .FALSE.
                        EFAispos = .FALSE.
                        EFAisy = .FALSE.
                END DO
                Delta = Delta/2._wp
        END SUBROUTINE

        SUBROUTINE CreateMatrix()
        !Creates the "matrix" where we give a number to each inside the fractal
                INTEGER         :: CMi, CMj, CMx, CMy
                ! Iterators
                COMPLEX         :: CMC1
                ! Temporary complex variable
                LOGICAL         :: CMisAfter, CMlogic
                ! Is True if we hit the boundary
                INTEGER         :: CMCounter
                ! Counts how long we've crossed the boundary
                ! If the value is 1, we are on the boundary
                GridN = Ntot*2+2
                ALLOCATE(GridMatrix(GridN,GridN))
                GridMatrix = 0
                ! Since all complex numbers represents a point, the points
                ! on the border can be marked easily
                DO CMi = 1, FracMN*2
                        CMx = INT(REAL(FractalSArrayE(CMi)))
                        CMy = INT(IMAG(FractalSArrayE(CMi)))
                        GridMatrix(CMx,CMy) = 1
                END DO
                CMCounter = 0
                CMisAfter = .FALSE.
                x = 0
                DO CMi = 1, GridN
                        DO CMj = 1, GridN
                                CMC1 = CMPLX(CMi,CMj)
                                CALL ANYTEST(CMC1, CMlogic)
                                IF(CMlogic .AND. &
                                        (CMCounter == 0)) THEN
                                ! When crossing boundary the CMisAfter is
                                ! switched on and the value assigning can begin
                                        CMisAfter = .TRUE.
                                        CMCounter = 1
                                 ELSE IF(.NOT. CMlogic .AND.&
                                        CMCounter == 1) THEN
                                ! Checks if we are not on the boundary, and that
                                ! we just got off a vertical boundary, if this
                                ! is the case, we may, or may not be inside
                                        IF(GridMatrix(CMi-1,CMj) == 1) THEN
                                        ! If the left element is 1, then we are
                                        ! inside the boundary
                                                GridMatrix(CMi,CMj) = 1
                                                CMCounter = CMcounter + 1
                                        ELSE
                                        ! Else, we are out and the CMisAfter is
                                        ! turned off so that it does not assign
                                        ! any values
                                                CMisAfter = .FALSE.
                                                CMCounter = 0
                                        END IF
                                 ELSE IF(.NOT. CMlogic .AND.&
                                        CMisAfter) THEN
                                ! If we are not on the boundary and the CMisAfter
                                ! is turned on, then the gridpoint should 
                                ! be assigned a value
                                        GridMatrix(CMi,CMj) = 1
                                        CMCounter = CMCOUNTER + 1
                                ELSE IF(CMlogic .AND. CMcounter /= 1) THEN
                                ! If we cross the boundary whilst we have made
                                ! alot of switches, then we know we are about to
                                ! leave the boundary or get on a vertical line
                                ! in which would lead to a evaluation of the
                                ! latter case.
                                        CMisAfter = .FALSE.
                                        CMCounter = 0
                                END IF
                        END DO
                END DO
        END SUBROUTINE
        SUBROUTINE ANYTEST(ATCM1,ATLogic)
        ! Does exactly what ANY does, but is more controlled and open
                COMPLEX, INTENT(IN) :: ATCM1
                LOGICAL, INTENT(OUT) :: ATLogic
                INTEGER               :: ATi
                ATLogic = .FALSE.
                DO ATi = 1,FracMN*2
                        IF(FractalSArrayE(ATi) == ATCM1) THEN
                                ATLogic = .TRUE.
                                RETURN
                        END IF
                END DO


        END SUBROUTINE

END MODULE
