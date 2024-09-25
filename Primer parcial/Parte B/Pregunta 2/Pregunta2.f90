program pregunta2
    implicit none

    ! Constantes
    INTEGER,PARAMETER :: cant_ec = 2
    REAL(8),PARAMETER :: ALFA = 1., BETA = 3.5

    ! Declaración de variables
    REAL(8),DIMENSION(0:cant_ec) :: vi
    REAL(8) :: max,h,tol

    ! Asignaciones
    vi = [0.,3.,0.]
    max = 6.
    h = 0.001
    tol = 0.01

    ! Graficar solución
    call RungeKuttaFehlberg(vi,max,h,tol)
    call system("gnuplot -persist script.p")
    
    contains

    function v_prima(v)
        REAL(8),DIMENSION(0:cant_ec) :: v,v_prima
        v_prima(0) = 1.
        v_prima(1) = v(2)
        v_prima(2) = -ALFA*v(2)-BETA**2*v(1)
    end function v_prima

    SUBROUTINE RungeKuttaFehlbergParaGraficar(vi, max, h)
        !Metodo de Runge-Kutta-Fehlberg (de 6to. orden)
        REAL(8), INTENT(IN), DIMENSION(0:cant_ec) :: vi
        REAL(8) :: h,max
        REAL(8), DIMENSION(0:cant_ec) :: v, k1, k2, k3, k4, k5, k6, vc
        open(unit=1,file="datos.dat")
        v = vi
        vc = vi
        DO while (v(0)<=max)
            WRITE (1, '(3F10.6)') v
            k1 = h*v_prima(v)
            k2 = h*v_prima(v + k1/4.0)
            k3 = h*v_prima(v + (3.0*k1 + 9.0*k2)/32.0)
            k4 = h*v_prima(v + (1932.0*k1 - 7200.0*k2 + 7296.0*k3)/2197.0)
            k5 = h*v_prima(v + 439.0*k1/216.0 - 8.0*k2 + 3680.0*k3/513.0-845.0*k4/4104.0)
            k6 = h*v_prima(v - 8.0*k1/27.0 + 2.0*k2 - 3544.0*k3/2565.0 + 1859.0*k4/4104.0-11.0*k5/40.0)
            v = v + (25.0*k1/216.0 + 1408.0*k3/2565.0 + 2197.0*k4/4104.0 - k5/5.0)
        END DO
        close(1)
    END SUBROUTINE RungeKuttaFehlbergParaGraficar

    SUBROUTINE RungeKuttaFehlbergParaObtenerMinimo(vi, h,tol)
        !Metodo de Runge-Kutta-Fehlberg (de 6to. orden)
        REAL(8), INTENT(IN), DIMENSION(0:cant_ec) :: vi
        REAL(8) :: h,tol,a,error
        REAL(8), DIMENSION(0:cant_ec) :: v, k1, k2, k3, k4, k5, k6, e, vc
        LOGICAL :: huboMinimo
        huboMinimo = .false.
        v = vi
        vc = vi
        DO while (.not. huboMinimo .and. (vc(2)<0))
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
            else ! Hay error
                a = 0.22
                v = vc
            end if
            h = h*abs(tol/error)**a
        END DO
        close(1)
    END SUBROUTINE RungeKuttaFehlbergParaObtenerMinimo
end program pregunta2