MODULE WPmod

        IMPLICIT NONE
        INTEGER, PARAMETER :: dp = kind(0.d0)
        INTEGER, PARAMETER :: sp = kind(0.0)
        INTEGER, PARAMETER :: wp = dp

        INTEGER                                 :: i, j
        ! General itterators
        REAL (wp), PARAMETER                    :: Length = 1._wp
        ! Length of each linesegment
        INTEGER, PARAMETER                      :: lDim = 3
        ! Level of the fractal
        INTEGER, PARAMETER                      :: FracN = 8**(ldim-1) + 1
        ! Dimension of FractalArray
        REAL(wp), PARAMETER                      :: Delta = 1._wp/4**(lDim-1)
        ! This is the number of elements along a line with size L
        COMPLEX, DIMENSION(:), ALLOCATABLE      :: FractalArray
        ! The array containing each corner point of the fractal
        COMPLEX, DIMENSION(9)                   :: LineSegment
        ! A general linesegment used to get the next level
        INTEGER                                 :: lItt
        ! Itterator to know which level I'm currently on
        INTEGER                                 :: levelN
        ! How many complex elements in the current level
        INTEGER, PARAMETER                      :: FracMN = FracN*4
        ! The dimension of the square
        COMPLEX, DIMENSION(FracMN)              :: FractalSArray
        ! The array containing the square, SquareArray
        COMPLEX, DIMENSION(FracMN*2)            :: FractalSArrayE
        ! The extended array containing the square, SquareArrayExtended
        INTEGER                                 :: NTot
        ! Total size of grid matrix enclosing the Fractal
        INTEGER, DIMENSION(:,:), ALLOCATABLE    :: GridMatrix
        ! The matrix enclosing the Fractal
        INTEGER                                 :: GridN
        ! The dimension of the GridMatrix
        INTEGER         :: MGXmin, MGXmax
        ! Defines the range of x
        INTEGER         :: MGYmin, MGYmax
        ! Defines the range of y
        REAL (wp)                               :: x,y

END MODULE
