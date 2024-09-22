program ejercicio14
    implicit none
    INTEGER,PARAMETER :: cant_ec = 3
    REAL(8),dimension(0:cant_ec) :: vi
    REAL(8) :: max,h
    max = 10.0
    h = 0.2
    vi = [0.,1.,2.,0.]
    call RungeKuttaFehlberg(vi,max,h)
    call system("gnuplot -persist script.p")
    
    contains

    function v_prima(v)
        real(8),dimension(0:cant_ec) :: v,v_prima
        v_prima(0) = 1.0
        v_prima(1) = v(2)
        v_prima(2) = v(3)
        v_prima(3) = exp(v(0))-2*v(3)+v(2)+2*v(1)
    end function v_prima

    SUBROUTINE RungeKuttaFehlberg(vi, max, h)
        !Metodo de Runge-Kutta-Fehlberg (de 6to. orden)
        REAL(8), INTENT(IN), DIMENSION(0:cant_ec) :: vi
        REAL(8) :: h,max
        REAL(8), DIMENSION(0:cant_ec) :: v, k1, k2, k3, k4, k5, k6, vc
        open(unit=1,file="datos.dat")
        v = vi
        vc = vi
        DO while (v(0)<=max)
            WRITE (1, '(4F10.6)') v
            k1 = h*v_prima(v)
            k2 = h*v_prima(v + k1/4.0)
            k3 = h*v_prima(v + (3.0*k1 + 9.0*k2)/32.0)
            k4 = h*v_prima(v + (1932.0*k1 - 7200.0*k2 + 7296.0*k3)/2197.0)
            k5 = h*v_prima(v + 439.0*k1/216.0 - 8.0*k2 + 3680.0*k3/513.0-845.0*k4/4104.0)
            k6 = h*v_prima(v - 8.0*k1/27.0 + 2.0*k2 - 3544.0*k3/2565.0 + 1859.0*k4/4104.0-11.0*k5/40.0)
            v = v + (25.0*k1/216.0 + 1408.0*k3/2565.0 + 2197.0*k4/4104.0 - k5/5.0)
        END DO
        close(1)
    END SUBROUTINE RungeKuttaFehlberg
end program ejercicio14