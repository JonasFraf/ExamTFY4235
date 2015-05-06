MODULE plotmod
USE openmod
USE WPmod
IMPLICIT NONE
CONTAINS
        SUBROUTINE plotExtendedFractal2D()
        DO i = 1,FracN*8
                WRITE(efractal,*) REAL(FractalSArrayE(i))*Delta,&
                                  &IMAG(FractalSArrayE(i))*Delta, 0
        END DO
        END SUBROUTINE
        



        SUBROUTINE plotGrid3D(gnuiFrame,PGi)
                ! Makes a gnufile which will surface plot the grid
                ! Makes PNGs.
                INTEGER, INTENT(IN)             :: gnuiFrame, PGi
                ! PGi is just a count which determines what is being plotted
                CALL openfile('Gnuplotter.gnu', gnuplot)
                WRITE(gnuplot,*) 'set terminal png enhanced font&
                               & "ariel,10" fontscale 1.0 size&
                               &  600, 500'
                WRITE(gnuplot,'(A,i3.3,A)')'set output "plot',gnuiFrame,'.png"'
                WRITE(gnuplot,*) 'set xlabel "Position X/L"'
                WRITE(gnuplot,*) 'set ylabel "Position Y/L"'
                
!                WRITE(gnuplot,'(A,i5.5,A)') 'set title "milliseconds: ',gnuTime,'"'
                IF (PGi == 1) THEN
                        WRITE(gnuplot, *) 'set title "Fractal Drum"'
                ELSE IF (PGi == 2) THEN
                        WRITE(gnuplot,'(A,i2.2,A)') 'set title "Mode: ',&
                                                     GnuIFrame,'"'
                END IF
 !               WRITE(gnuplot, *) 'set pm3d'
 !               WRITE(gnuplot,*) 'set view 80,15'
                WRITE(gnuplot,*) 'set view 0,0'
                WRITE(gnuplot, *) 'unset key'
  !              WRITE(gnuplot, *) 'set palette grey'
                
                WRITE(gnuplot, *) 'set contour'
 !               WRITE(gnuplot, *) 'set contour base'
  !              WRITE(gnuplot, *) 'set nosurface'
   !             WRITE(gnuplot, *) 'set view 0,0'
                
                !WRITE(gnuplot,*) 'splot "Grid3D.dat" w pm3d, "EFractal2D.dat" w l fc&
                !                & rgb "black" notitle '
                !WRITE(gnuplot,*) 'splot "Grid3D.dat" w pm3d notitle '
                WRITE(gnuplot,*) 'splot "Grid3D.dat" w pm3d, "EFractal2D.dat"&
                                 w l fc rgb "black" notitle '
                x = 0
                y = Delta
                DO i = 1, GridN
                        x = x+Delta
                        DO j = 1, GridN
                                IF (PGi == 1) THEN
                                        WRITE (grid3d,*) x,y,GridMatrix(i,j)
                                ELSE IF (PGi == 2) THEN
                                        WRITE(grid3d,*) x,y,GridMatrixR(i,j)
                                END IF
                                y = y+Delta
                        END DO
                        WRITE (grid3d,*) 
                        y = Delta
                END DO
                CALL SYSTEM('gnuplot Gnuplotter.gnu')
                CALL SYSTEM('rm Gnuplotter.gnu')
        
        END SUBROUTINE        
END MODULE
