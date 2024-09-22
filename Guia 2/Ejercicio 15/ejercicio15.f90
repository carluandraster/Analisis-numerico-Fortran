program ejercicio15
    implicit none
    REAL,PARAMETER :: L = 200., C = 0.001, E = 1
    INTEGER,PARAMETER :: cant_ec = 2

    REAL(8), DIMENSION(0:cant_ec) :: vi
    REAL(8) :: h,max,R

    vi = [0.,0.,0.]
    max = 5.
    h = 0.001

    R = 0.
    call RungeKuttaFehlberg(vi,max,h,R,1,"datos1.dat")
    R = 50.
    call RungeKuttaFehlberg(vi,max,h,R,2,"datos2.dat")
    R = 100.
    call RungeKuttaFehlberg(vi,max,h,R,3,"datos3.dat")

    call system("gnuplot -persist script.p")

    contains

    function v_prima(v,R)
        real(8),dimension(0:cant_ec) :: v,v_prima
        real(8) :: R
        v_prima(0) = 1.
        v_prima(1) = v(2)
        v_prima(2) = (E-v(2)*R-v(1)/C)/L
    end function v_prima

    SUBROUTINE RungeKuttaFehlberg(vi, max, h,R,unidad,nombre)
        !Metodo de Runge-Kutta-Fehlberg (de 6to. orden)
        REAL(8), INTENT(IN), DIMENSION(0:cant_ec) :: vi
        REAL(8) :: h,max,R
        REAL(8), DIMENSION(0:cant_ec) :: v, k1, k2, k3, k4, k5, k6
        INTEGER :: unidad
        CHARACTER(LEN=10) :: nombre
        
        open(unit=unidad,file=nombre)
        v = vi
        
        DO while (v(0)<=max)
            WRITE (unidad, '(3F10.6)') v
            k1 = h*v_prima(v,R)
            k2 = h*v_prima(v + k1/4.0,R)
            k3 = h*v_prima(v + (3.0*k1 + 9.0*k2)/32.0,R)
            k4 = h*v_prima(v + (1932.0*k1 - 7200.0*k2 + 7296.0*k3)/2197.0,R)
            k5 = h*v_prima(v + 439.0*k1/216.0 - 8.0*k2 + 3680.0*k3/513.0-845.0*k4/4104.0,R)
            k6 = h*v_prima(v - 8.0*k1/27.0 + 2.0*k2 - 3544.0*k3/2565.0 + 1859.0*k4/4104.0-11.0*k5/40.0,R)
            v = v + (25.0*k1/216.0 + 1408.0*k3/2565.0 + 2197.0*k4/4104.0 - k5/5.0)
        END DO
        close(unidad)
    END SUBROUTINE RungeKuttaFehlberg
end program ejercicio15