program ejercicio3a
    implicit none
    real(8),dimension(0:1) :: vi
    real(8) :: h,max
    vi(0) = 0
    vi(1) = 1
    h = 0.001
    max = 0.5
    
    call EulerSimple(vi,h,max)
    call system("gnuplot -persist script.p")

    contains
    function v_prima(v)
        real(8), dimension(0:1) :: v,v_prima
        v_prima(0) = 1.0
        v_prima(1) = exp(-v(0))-3*v(1)
    end function v_prima

    function solucionExacta(v)
        real(8),dimension(0:1) :: v    
        real(8) :: solucionExacta
        solucionExacta = (exp(-v(0))+exp(-3*v(0)))/2
    end function solucionExacta

    subroutine EulerSimple(vi,h,max)
        real(8),dimension(0:1) :: v,vi
        real(8) :: h,max
        open(unit=1,file="datos.dat")
        v = vi
        
        do while (v(0)<=max)
            write(1,'(3F10.6)') v,solucionExacta(v)
            v = v+h*v_prima(v)
        end do
        close(1)
    end subroutine EulerSimple
end program ejercicio3a