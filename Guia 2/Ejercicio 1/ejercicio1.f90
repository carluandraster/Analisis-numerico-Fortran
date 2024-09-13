program ejercicio1
    implicit none

    REAL(8), DIMENSION(0:1) :: vi
    REAL(8) h

    vi = (/1.0, 1.0/)
    h = 0.05
    
    call EulerSimple(vi,1,80,h)
    
    contains

    function v_prima(v)
        ! Definición de la Ecuacion Diferencial
        REAL(8), DIMENSION(0:1) :: v, v_prima
        v_prima(0) = 1.0
        v_prima(1) = v(0)*v(1)**(1/3.)
    END FUNCTION v_prima

    subroutine EulerSimple(vi,cant_ec,max_iter,h)
        INTEGER, INTENT(IN) :: cant_ec, max_iter
        REAL(8),INTENT(IN),DIMENSION(0:cant_ec) :: vi
        REAL(8),INTENT(IN) :: h
        INTEGER iter
        REAL(8), DIMENSION(0:cant_ec) :: v

        OPEN(unit=1,file="resultados.dat");
        WRITE (1, '(3F10.6)') vi,vi(1)

        v = vi
        DO iter = 1, max_iter
            v = v + h*v_prima(v)
            WRITE (1, '(3F10.6)') v,((v(0)**2+2)/3)**(3.0/2)
        END DO

        close(1)
        call system("gnuplot -persist script.p")
    end subroutine EulerSimple
end program ejercicio1