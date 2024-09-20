program ejercicio8a
    implicit none
    real(8),dimension(0:1) :: vi
    real(8) :: h,max,tol
    
    vi(0) = 0
    vi(1) = 1
    h=0.01
    max=10.0
    tol = 0.005
    call RungeKutta(vi,h,max,tol)
    call system("gnuplot -persist script.p")

    contains

    function v_prima(v)
        real(8),dimension(0:1) :: v_prima,v
        v_prima(0) = 1.0
        v_prima(1) = -v(1)/(1+v(0)**2)
    end function v_prima

    function solucionExacta(v)
        real(8),dimension(0:1) :: v
        real(8) :: solucionExacta
        solucionExacta = exp(-atan(v(0)))
    end function solucionExacta

    subroutine RungeKutta(vi,h,max,tol)
        real(8),dimension(0:1) :: vi,v,vc,v_estrella,k1,k2,k3,k4
        real(8) :: h,max,tol,error
        integer :: i
        
        open(unit=1,file="datos.dat")

        v = vi
        v_estrella = vi
        vc = vi
        write(1,'(3F10.6)') v,solucionExacta(v)

        do while (v(0)<=max)
            ! Calcular Y sub n+1 con un paso h
            k1 = h*v_prima(v)
            k2 = h*v_prima(v+k1/2.0)
            k3 = h*v_prima(v+k2/2.0)
            k4 = h*v_prima(v+k3)
            v = v + (k1 + 2.0*k2 + 2.0*k3 +k4)/6.0

            ! Calcular Y* sub N+1 con 2 pasos h
            do i = 1, 2
                k1 = h*v_prima(v_estrella)
                k2 = h*v_prima(v_estrella+k1/2.0)
                k3 = h*v_prima(v_estrella+k2/2.0)
                k4 = h*v_prima(v_estrella+k3)
                v_estrella = v_estrella + (k1 + 2.0*k2 + 2.0*k3 +k4)/6.0
            end do

            error = abs(v(1)-v_estrella(1))
            if ( error>tol ) then ! Hay un error considerable
                h = h/2
                v = vc
                v_estrella = vc
            else ! Error despreciable
                write(1,'(3F10.6)') v,solucionExacta(v)
                vc = v
                v_estrella = v
                if ( error<tol/5 ) then
                    h = 2*h
                end if
            end if
        end do
        close(1)
    end subroutine RungeKutta
end program ejercicio8a