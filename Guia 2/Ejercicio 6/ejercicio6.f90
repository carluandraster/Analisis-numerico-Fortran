program ejercicio6
    implicit none
    REAL(8), DIMENSION(0:1) :: vi
    REAL(8) :: h,y1,y2

    ! Valores iniciales
    vi(0) = 0
    vi(1) = 0
    
    ! Encabezado del cuadro
    print*," h              y(1)                         y(2)"

    ! Inciso a
    h = 0.2
    call RungeKutta(vi,h,y1,y2)
    print*,"0,2   ",y1,"  ",y2

    ! Inciso b
    h = 0.1
    call RungeKutta(vi,h,y1,y2)
    print*,"0,1   ",y1,"  ",y2

    contains

    function v_prima(v)
        real(8),dimension(0:1) :: v,v_prima;
        v_prima(0) = 1.0;
        v_prima(1) = v(0)*v(1)+1;
    end function v_prima

    SUBROUTINE RungeKutta(vi,h,y1,y2)
        !Metodo de Runge-Kutta (de 4to. orden)
        REAL(8), INTENT(IN), DIMENSION(0:1) :: vi
        REAL(8), INTENT(IN) :: h
        REAL(8), INTENT(OUT) :: y1,y2
        REAL(8), DIMENSION(0:1) :: v, k1, k2, k3, k4
        v = vi
        DO while (v(0)<1.0)
            k1 = h*v_prima(v)
            k2 = h*v_prima(v+k1/2.0)
            k3 = h*v_prima(v+k2/2.0)
            k4 = h*v_prima(v+k3)
            v = v + (k1 + 2.0*k2 + 2.0*k3 +k4)/6.0
        END DO
        y1 = v(1)
        DO while (v(0)<2.0)
            k1 = h*v_prima(v)
            k2 = h*v_prima(v+k1/2.0)
            k3 = h*v_prima(v+k2/2.0)
            k4 = h*v_prima(v+k3)
            v = v + (k1 + 2.0*k2 + 2.0*k3 +k4)/6.0
        END DO
        y2 = v(1)
    END SUBROUTINE RungeKutta
end program ejercicio6