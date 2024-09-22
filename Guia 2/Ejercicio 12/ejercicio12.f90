program ejercicio12
    implicit none
    REAL(8) :: y1,yp1,max,h
    REAL(8),DIMENSION(0:2) :: vi
    vi = [0.,1.,0.]
    max = 1.
    h = 0.05

    call RungeKutta(vi,2,max,h,y1,yp1)
    print*,"y(1) = ",y1
    print*,"y'(1) = ",yp1
    
    contains

    function v_prima(v)
        REAL(8),DIMENSION(0:2) :: v,v_prima
        v_prima(0) = 1.0
        v_prima(1) = v(2)
        v_prima(2) = 0.05*v(2)-0.15*v(1)
    end function v_prima

    SUBROUTINE RungeKutta(vi, cant_ec, max, h,y1,yp1)
        !Metodo de Runge-Kutta (de 4to. orden)
        REAL(8), INTENT(IN), DIMENSION(0:cant_ec) :: vi
        REAL(8), INTENT(IN) :: h, max
        INTEGER, INTENT(IN) :: cant_ec
        REAL(8), DIMENSION(0:cant_ec) :: v, k1, k2, k3, k4
        REAL(8), INTENT(OUT) :: y1,yp1
        v = vi
        DO while (v(0)<max)
            k1 = h*v_prima(v)
            k2 = h*v_prima(v+k1/2.0)
            k3 = h*v_prima(v+k2/2.0)
            k4 = h*v_prima(v+k3)
            v = v + (k1 + 2.0*k2 + 2.0*k3 +k4)/6.0
        END DO
        y1 = v(1)
        yp1 = v(2)
    END SUBROUTINE RungeKutta
end program ejercicio12