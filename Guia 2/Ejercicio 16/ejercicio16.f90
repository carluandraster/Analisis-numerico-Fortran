program ejercicio16
    implicit none
    INTEGER,PARAMETER :: cant_ec = 6
    REAL(8),PARAMETER :: K1 = 1., K2 = 1., K3 = 1., M1 = 1., M2 = 1., M3 = 1., B1 = 0.1, B2 = 0.1
    REAL(8) :: h,max
    REAL(8),DIMENSION(0:cant_ec) :: vi
    vi = [0.,0.,0.,0.,0.,0.,0.]
    h = 0.001
    max = 10.

    call RungeKuttaFehlberg(vi,max,h,"a")
    call system("gnuplot -persist scriptA.p")

    call RungeKuttaFehlberg(vi,max,h,"b")
    call system("gnuplot -persist scriptB.p")

    call RungeKuttaFehlberg(vi,max,h,"c")
    call system("gnuplot -persist scriptC.p")

    contains

    function v_prima(v,F1,F3)
        real(8),DIMENSION(0:cant_ec) :: v,v_prima
        REAL(8) :: F1,F3
        v_prima(0) = 1.
        v_prima(1) = v(4)
        v_prima(2) = v(5)
        v_prima(3) = v(6)
        v_prima(4) = (F1-B1*v(4)-K1*v(1)+B1*v(5)+K2*v(2))/M1
        v_prima(5) = (B1*v(4)+K1*v(1)-B1*v(5)-(K1+K2)*v(2)+K2*v(3))/M2
        v_prima(6) = (F3+K2*v(2)-B2*v(6)-(K2+K3)*v(3))/M3
    end function v_prima

    function F1(v,inciso)
        REAL(8) :: F1
        REAL(8),DIMENSION(0:cant_ec) :: v
        CHARACTER :: inciso
        select case(inciso)
            case("a")
                F1 = 0.2
            case("b")
                if ( v(0)<=1 ) then
                    F1 = 1.
                else
                    F1 = 0.
                end if
            case("c")
                if ( v(0)<=1 ) then
                    F1 = 1.
                else
                    F1 = 0.
                end if
        end select
    end function F1

    function F3(inciso)
        REAL(8) :: F3
        CHARACTER :: inciso
        select case(inciso)
            case("a")
                F3 = 0.
            case("b")
                F3 = 0.
            case("c")
                F3 = -0.1
        end select
    end function F3

    SUBROUTINE RungeKuttaFehlberg(vi, max, h,inciso)
        !Metodo de Runge-Kutta-Fehlberg (de 6to. orden)
        REAL(8), INTENT(IN), DIMENSION(0:cant_ec) :: vi
        REAL(8) :: h,max,fuerza1,fuerza3
        REAL(8), DIMENSION(0:cant_ec) :: v, k1, k2, k3, k4, k5, k6
        CHARACTER :: inciso
        
        open(unit=1,file="datos.dat")
        v = vi
        fuerza3 = F3(inciso)
        
        DO while (v(0)<=max)
            WRITE (1, '(7F10.6)') v
            fuerza1 = F1(v,inciso)
            k1 = h*v_prima(v,fuerza1,fuerza3)
            k2 = h*v_prima(v + k1/4.0,fuerza1,fuerza3)
            k3 = h*v_prima(v + (3.0*k1 + 9.0*k2)/32.0,fuerza1,fuerza3)
            k4 = h*v_prima(v + (1932.0*k1 - 7200.0*k2 + 7296.0*k3)/2197.0,fuerza1,fuerza3)
            k5 = h*v_prima(v + 439.0*k1/216.0 - 8.0*k2 + 3680.0*k3/513.0-845.0*k4/4104.0,fuerza1,fuerza3)
            k6 = h*v_prima(v - 8.0*k1/27.0 + 2.0*k2 - 3544.0*k3/2565.0 + 1859.0*k4/4104.0-11.0*k5/40.0,fuerza1,fuerza3)
            v = v + (25.0*k1/216.0 + 1408.0*k3/2565.0 + 2197.0*k4/4104.0 - k5/5.0)
        END DO
        close(1)
    END SUBROUTINE RungeKuttaFehlberg
end program ejercicio16