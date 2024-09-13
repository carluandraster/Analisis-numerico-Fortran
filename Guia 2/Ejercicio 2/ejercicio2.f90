program ejercicio2
    implicit none
    real(8),dimension(0:1) :: vi
    real(8) :: h,maxNum
    vi(0) = 0
    vi(1) = 5

    open(unit=1,file="./dat/incisoA.dat")
    open(unit=2,file="./dat/incisoB1.dat")
    open(unit=3,file="./dat/incisoB2.dat")

    h = 0.1
    maxNum = 0.2
    call EulerSimple(vi,maxNum,h,1)

    maxNum = 0.09
    h = 0.01
    call EulerSimple(vi,maxNum,h,2)
    h = 0.001
    call EulerSimple(vi,maxNum,h,3)

    close(1)
    close(2)
    close(3)

    !Invocacion a GNU PLOT
    call system("gnuplot -persist ./scripts/incisoA.p")
    call system("gnuplot -persist ./scripts/incisoB.p")
    
    contains

    function v_prima(v)
        real(8),dimension(0:1) :: v,v_prima
        v_prima(0) = 1.0
        v_prima(1) = -20*v(1)+7*exp(-0.5*v(0))
    end function v_prima

    subroutine EulerSimple(vi,maxNum,h,unidad)
        real(8),dimension(0:1), intent(in) :: vi
        real(8),dimension(0:1) :: v
        integer, intent(in) :: unidad
        real(8), intent(in) :: h,maxNum
        
        v = vi
        do while (v(0)<=maxNum)
            write(unidad,'(3F10.6)') v,5*exp(-20*v(0))+7*(exp(-0.5*v(0))-exp(-20*v(0)))/19.5
            v = v + h*v_prima(v)
        end do
        
    end subroutine EulerSimple
end program ejercicio2