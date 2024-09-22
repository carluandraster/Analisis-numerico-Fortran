program ejercicio11
    implicit none
    REAL(8), DIMENSION(0:2) :: vi
    REAL(8) :: h, max

    vi = [0,0,0]
    h = 0.2
    max = 2.0

    call RungeKutta(vi,2,max,h)
    call system ("gnuplot -persist script.p")
    
    contains

    FUNCTION v_prima(v)
        ! Definición del Sistema de Ecuaciones Diferenciales
        REAL(8), DIMENSION(0:2) :: v, v_prima
        v_prima(0) = 1.0
        v_prima(1) = v(2)
        v_prima(2) = 48*sin(10*v(0))-100*v(1)-12*v(2)
    END FUNCTION v_prima

    SUBROUTINE RungeKutta(vi, cant_ec, max, h)
        !Metodo de Runge-Kutta (de 4to. orden)
        REAL(8), INTENT(IN), DIMENSION(0:cant_ec) :: vi
        REAL(8), INTENT(IN) :: h, max
        INTEGER, INTENT(IN) :: cant_ec
        REAL(8), DIMENSION(0:cant_ec) :: v, k1, k2, k3, k4
        open(unit=1,file="datos.dat")
        v = vi
        DO while (v(0)<=max)
            WRITE (1, '(3F10.6)') v
            k1 = h*v_prima(v)
            k2 = h*v_prima(v+k1/2.0)
            k3 = h*v_prima(v+k2/2.0)
            k4 = h*v_prima(v+k3)
            v = v + (k1 + 2.0*k2 + 2.0*k3 +k4)/6.0
        END DO
        WRITE (1, '(3F10.6)') v
        close(1)
    END SUBROUTINE RungeKutta
end program ejercicio11