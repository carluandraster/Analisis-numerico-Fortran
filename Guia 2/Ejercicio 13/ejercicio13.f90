program ejercicio13
    implicit none
    ! Datos del problema
    REAL(8),PARAMETER :: K1 = 6.1, K12 = 8.8, K2 = 5.8
    INTEGER,PARAMETER :: M1 = 174, M2 = 132, c1 = 41, c2 = 21,cant_ec = 4
    
    ! Declaración de variables
    REAL(8),DIMENSION(0:cant_ec) :: vi
    REAL(8) :: h,max,tol

    ! Inicializar variables
    vi = [0.,-5.,7.,0.,0.]
    h = 0.001
    max = 30.
    tol = 10.**(-4)

    ! Runge Kutta Felberg
    call RungeKuttaFehlberg(vi,max,h,tol)
    call system("gnuplot -persist script.p")

    contains

    function v_prima(v)
        real(8),dimension(0:cant_ec) :: v,v_prima
        v_prima(0) = 1.0
        v_prima(1) = v(3)
        v_prima(2) = v(4)
        v_prima(3) = (-K1*v(1)+K12*(v(2)-v(1))-c1*v(3))/M1
        v_prima(4) = (-K2*v(2)-K12*(v(2)-v(1))-c2*v(4))/M2
    end function v_prima

    SUBROUTINE RungeKuttaFehlberg(vi, max, h,tol)
        !Metodo de Runge-Kutta-Fehlberg (de 6to. orden)
        REAL(8), INTENT(IN), DIMENSION(0:cant_ec) :: vi
        REAL(8) :: h,max,tol,a,error
        REAL(8), DIMENSION(0:cant_ec) :: v, k1, k2, k3, k4, k5, k6, e, vc
        open(unit=1,file="datos.dat")
        v = vi
        vc = vi
        WRITE (1, '(5F10.6)') vi
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
                WRITE (1, '(5F10.6)') v
            else ! Hay error
                a = 0.22
                v = vc
            end if
            h = h*abs(tol/error)**a
        END DO
        close(1)
    END SUBROUTINE RungeKuttaFehlberg
end program ejercicio13