program ejercicio4
    implicit none

    ! Declaración e inicialización de variables
    integer :: i,exp,signo,num
    real :: numero
    exp = 0
    numero = 1

    ! Leer archivo
    open(unit=1,action='READ',file='binario.txt')
    do i = 1, 32
        read(1,'(I1)',ADVANCE = 'NO') num
        select case(i)
            case(1)
                signo = num
            case(2:9)
                exp = exp +num * 2**(9-i)
            case (10:32)
                numero = numero + num*2**(9-i)
        end select
    end do
    close(1)
    ! Aplicar formula
    print*, (-1)**signo*numero*2**(exp-127)
end program ejercicio4