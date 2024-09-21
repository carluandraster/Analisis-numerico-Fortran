program ejercicio10
    implicit none
    REAL(8), DIMENSION(0:1) :: vi1,vi2
    REAL(8) :: max,h,tol

    vi1 = [0,4]
    vi2 = [0,6]
    max = 2.0
    h = 0.0001
    tol = 0.005
    
    call RungeKuttaFehlberg(vi1, max, h,tol,.TRUE.,vi2)
    call RungeKuttaFehlberg(vi2, max, h,tol,.FALSE.,vi1)
    call system("gnuplot -persist script.p")

    contains

    function v_prima1(v)
        real(8),dimension(0:1) :: v,v_prima1
        v_prima1(0) = 1.0
        v_prima1(1) = -0.5*v(1) 
    end function v_prima1

    function solucionExacta(v,funcion)
        real(8),dimension(0:1) :: v
        real(8) :: solucionExacta
        LOGICAL funcion
        if ( funcion ) then
            solucionExacta = 4*exp(-0.5*v(0))
        else
            solucionExacta = 40.0/3.0+2*exp(-0.5*v(0))-28.0/3.0*exp(-0.3*v(0))
        end if
    end function solucionExacta

    function v_prima2(v1,v2)
        real(8),dimension(0:1) :: v1,v2,v_prima2
        v_prima2(0) = 1.0
        v_prima2(1) = 4-0.1*v1(1)-0.3*v2(1)
    end function v_prima2

    SUBROUTINE RungeKuttaFehlberg(vi, max, h,tol,funcion,v1)
        !Metodo de Runge-Kutta-Fehlberg (de 6to. orden)
        REAL(8), INTENT(IN), DIMENSION(0:1) :: vi
        REAL(8) :: h,max,tol,a,error
        REAL(8), DIMENSION(0:1) :: v,v1, k1, k2, k3, k4, k5, k6, e, vc
        LOGICAL :: funcion
        INTEGER :: unidad
        open(unit=1,file="datos1.dat")
        unidad = 1
        if (.not. funcion) then
            open(unit=2,file="datos2.dat")
            unidad = 2
        end if
        v = vi
        vc = vi
        WRITE (unidad, '(3F20.6)') v,solucionExacta(v,funcion)
        DO while (v(0)<=max)
            if ( funcion ) then
                k1 = h*v_prima1(v)
                k2 = h*v_prima1(v + k1/4.0)
                k3 = h*v_prima1(v + (3.0*k1 + 9.0*k2)/32.0)
                k4 = h*v_prima1(v + (1932.0*k1 - 7200.0*k2 + 7296.0*k3)/2197.0)
                k5 = h*v_prima1(v + 439.0*k1/216.0 - 8.0*k2 + 3680.0*k3/513.0-845.0*k4/4104.0)
                k6 = h*v_prima1(v - 8.0*k1/27.0 + 2.0*k2 - 3544.0*k3/2565.0 + 1859.0*k4/4104.0-11.0*k5/40.0)
            else
                do while (v1(0)<v(0))
                    READ(1,'(3F20.6)') v1,a
                end do
                v1 = v1 -(v1(0)-v(0))*v_prima1(v1)
                k1 = h*v_prima2(v1,v)
                k2 = h*v_prima2(v1,v + k1/4.0)
                k3 = h*v_prima2(v1,v + (3.0*k1 + 9.0*k2)/32.0)
                k4 = h*v_prima2(v1,v + (1932.0*k1 - 7200.0*k2 + 7296.0*k3)/2197.0)
                k5 = h*v_prima2(v1,v + 439.0*k1/216.0 - 8.0*k2 + 3680.0*k3/513.0-845.0*k4/4104.0)
                k6 = h*v_prima2(v1,v - 8.0*k1/27.0 + 2.0*k2 - 3544.0*k3/2565.0 + 1859.0*k4/4104.0-11.0*k5/40.0)
            end if
            
            v = v + (25.0*k1/216.0 + 1408.0*k3/2565.0 + 2197.0*k4/4104.0 - k5/5.0)
            e = k1/360.0 - 128.0*k3/4275.0 - 2197.0*k4/75240.0 + k5/50.0 + 2.0*k6/55.0
            error = maxval(abs(e))
            if ( tol>= error) then ! No hay error
                a = 0.2
                vc = v
                WRITE (unidad, '(3F20.6)') v,solucionExacta(v,funcion)
            else ! Hay error
                a = 0.22
                v = vc
            end if
            h = h*abs(tol/error)**a
        END DO
        close(1)
        if ( .not. funcion ) then
            close(2)
        end if
    END SUBROUTINE RungeKuttaFehlberg
end program ejercicio10