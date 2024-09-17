program ejercicio4a
    implicit none
    
    REAL(8), DIMENSION(0:1) :: vi
    REAL(8) :: h, max
    vi(0)=0.0
    vi(1)=-1.0
    h = 0.25
    max = 1.0

    call EulerModificado(vi,max,h)
    call system("gnuplot -persist script.p")

    contains

    function v_prima(v)
        real(8),dimension(0:1) :: v,v_prima
        v_prima(0) = 1.0
        v_prima(1) = -v(0)*v(1)+4*v(0)/v(1)
    end function v_prima

    SUBROUTINE EulerModificado(vi, max, h)
        !Metodo de Euler Modificado
        REAL(8), INTENT(IN), DIMENSION(0:1) :: vi
        REAL(8), INTENT(IN) :: h, max
        REAL(8), DIMENSION(0:1) :: v, vp
        open(unit=1,file="datos.dat")
        v = vi
        DO while (v(0)<=max)
            WRITE (1, '(2F10.6)') v
            vp = v_prima(v)
            v = v + h*(vp + v_prima(v + h*vp))/2.0
        END DO
        close(1)
    END SUBROUTINE EulerModificado
end program ejercicio4a