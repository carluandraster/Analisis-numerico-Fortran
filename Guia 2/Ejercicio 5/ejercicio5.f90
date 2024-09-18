program ejercicio5
    implicit none
    real(8),dimension(0:1) :: vi
    real(8) :: h
    vi(0) = 0
    vi(1) = 1
    
    ! Inciso a
    h = 0.5
    print*,"EULER MODIFICADO con h=0,5"
    call EulerModificado(vi,h)
    h = 0.1
    print*,"EULER MODIFICADO con h=0,1"
    call EulerModificado(vi,h)

    ! Inciso b
    h = 0.5
    print*,"RUNGE-KUTTA (4to orden) con h=0,5"
    call RungeKutta(vi,h)
    h = 0.1
    print*,"RUNGE-KUTTA (4to orden) con h=0,1"
    call RungeKutta(vi,h)

    contains

    function v_prima(v)
        real(8),dimension(0:1) :: v,v_prima
        v_prima(0) = 1.0
        v_prima(1) = -1/(1+v(1)**2)
    end function v_prima

    ! Inciso a
    subroutine EulerModificado(vi, h)
        real(8),dimension(0:1) :: vi,vp,v
        real(8) :: h
        v=vi
        do while (v(0)<1)
            vp = v_prima(v)
            v = v+h*(vp+v_prima(v+h*vp))/2.0
        end do
        print*,"y(1) = ",v(1)
        do while (v(0)<2)
            vp = v_prima(v)
            v = v+h*(vp+v_prima(v+h*vp))/2.0
        end do
        print*,"y(2) = ",v(1)
    end subroutine EulerModificado

    SUBROUTINE RungeKutta(vi,h)
        !Metodo de Runge-Kutta (de 4to. orden)
        REAL(8), INTENT(IN), DIMENSION(0:1) :: vi
        REAL(8), INTENT(IN) :: h
        REAL(8), DIMENSION(0:1) :: v, k1, k2, k3, k4
        v = vi
        DO while (v(0)<1)
            k1 = h*v_prima(v)
            k2 = h*v_prima(v+k1/2.0)
            k3 = h*v_prima(v+k2/2.0)
            k4 = h*v_prima(v+k3)
            v = v + (k1 + 2.0*k2 + 2.0*k3 +k4)/6.0
        END DO
        print*,"y(1) = ",v(1)
        DO while (v(0)<2)
            k1 = h*v_prima(v)
            k2 = h*v_prima(v+k1/2.0)
            k3 = h*v_prima(v+k2/2.0)
            k4 = h*v_prima(v+k3)
            v = v + (k1 + 2.0*k2 + 2.0*k3 +k4)/6.0
        END DO
        print*,"y(2) = ",v(1)
    END SUBROUTINE RungeKutta
end program ejercicio5