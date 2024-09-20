program ejercicio4a
    implicit none
    
    REAL(8), DIMENSION(0:1) :: vi
    REAL(8) :: h, max,tol
    vi(0)=0.0
    vi(1)=-1.0
    h = 0.01
    tol = 0.005
    max = 1.0

    call RungeKuttaFehlberg(vi, max, h,tol)
    call system("gnuplot -persist script.p")

    contains

    function v_prima(v)
        real(8),dimension(0:1) :: v,v_prima
        v_prima(0) = 1.0
        v_prima(1) = -v(0)*v(1)+4*v(0)/v(1)
    end function v_prima

    function solucionExacta(v)
        real(8),dimension(0:1) :: v
        real(8) :: solucionExacta
        solucionExacta = -sqrt(-3*exp(-v(0)**2)+4)
    end function solucionExacta

    SUBROUTINE RungeKuttaFehlberg(vi, max, h,tol)
        !Metodo de Runge-Kutta-Fehlberg (de 6to. orden)
        REAL(8), INTENT(IN), DIMENSION(0:1) :: vi
        REAL(8) :: h,max,tol,a,error
        REAL(8), DIMENSION(0:1) :: v, k1, k2, k3, k4, k5, k6, e, vc
        open(unit=1,file="datos.dat")
        v = vi
        vc = vi
        WRITE (1, '(3F10.6)') v,solucionExacta(v)
        DO while (v(0)<=max)
            k1 = h*v_prima(v)
            k2 = h*v_prima(v + k1/4.0)
            k3 = h*v_prima(v + (3.0*k1 + 9.0*k2)/32.0)
            k4 = h*v_prima(v + (1932.0*k1 - 7200.0*k2 + 7296.0*k3)/2197.0)
            k5 = h*v_prima(v + 439.0*k1/216.0 - 8.0*k2 + 3680.0*k3/513.0-845.0*k4/4104.0)
            k6 = h*v_prima(v - 8.0*k1/27.0 + 2.0*k2 - 3544.0*k3/2565.0 + 1859.0*k4/4104.0-11.0*k5/40.0)
            v = v + (25.0*k1/216.0 + 1408.0*k3/2565.0 + 2197.0*k4/4104.0 - k5/5.0)
            e = k1/360.0 - 128.0*k3/4275.0 - 2197.0*k4/75240.0 + k5/50.0 + 2.0*k6/55.0
            error = maxval(abs(e))
            if ( tol>= error) then ! No hay error
                a = 0.2
                vc = v
                WRITE (1, '(3F10.6)') v,solucionExacta(v)
            else ! Hay error
                a = 0.22
                v = vc
            end if
            h = h*abs(tol/error)**a
        END DO
        close(1)
    END SUBROUTINE RungeKuttaFehlberg
end program ejercicio4a