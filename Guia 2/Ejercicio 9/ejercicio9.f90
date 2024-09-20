program ejercicio9
    implicit none
    REAL(8),DIMENSION(0:1) :: vi
    REAL(8) :: h,res
    ! y(0) = 40
    vi(0) = 0.0
    vi(1) = 40.0
    h=0.5
    res=4
    
    write(*,'(A,F10.1,A)') "Tiempo en que tardara la concentracion en llegar a un decimo de su valor inicial: ", &
                            RungeKuttaFehlberg(vi, res, h), " minutos."

    contains

    function v_prima(v)
        real(8),dimension(0:1) :: v,v_prima
        v_prima(0) = 1.0
        v_prima(1) = -v(1)/25
    end function v_prima

    function RungeKuttaFehlberg(vi, res, h)
        !Metodo de Runge-Kutta-Fehlberg (de 6to. orden)
        REAL(8), DIMENSION(0:1) :: vi,v, k1, k2, k3, k4, k5, k6, vc
        REAL(8) :: h,res,RungeKuttaFehlberg
        v = vi
        DO while (v(1)>res)
            vc=v
            k1 = h*v_prima(v)
            k2 = h*v_prima(v + k1/4.0)
            k3 = h*v_prima(v + (3.0*k1 + 9.0*k2)/32.0)
            k4 = h*v_prima(v + (1932.0*k1 - 7200.0*k2 + 7296.0*k3)/2197.0)
            k5 = h*v_prima(v + 439.0*k1/216.0 - 8.0*k2 + 3680.0*k3/513.0-845.0*k4/4104.0)
            k6 = h*v_prima(v - 8.0*k1/27.0 + 2.0*k2 - 3544.0*k3/2565.0 + 1859.0*k4/4104.0-11.0*k5/40.0)
            v = v + (25.0*k1/216.0 + 1408.0*k3/2565.0 + 2197.0*k4/4104.0 - k5/5.0)
        END DO
        if ( abs(v(1)-res)<abs(vc(1)-res) ) then
            RungeKuttaFehlberg = v(0)
        else
            RungeKuttaFehlberg = vc(0)
        end if
    END function RungeKuttaFehlberg
end program ejercicio9