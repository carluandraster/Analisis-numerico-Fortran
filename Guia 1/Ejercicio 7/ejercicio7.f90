program ejercicio7
    implicit none

    ! Declaracion de variables
    real vector (100),max
    integer N, ioError,i
    N=0

    ! Leer archivo
    open(unit=1,action="read",file="vector.txt")
    read(1,*, iostat = ioError) vector(N+1)
    do while(ioError==0)
        N=N+1
        read(1,*, iostat = ioError) vector(N+1)
    end do
    close(1)

    ! Calcular norma
    max = 0
    do i = 1, N
        if ( abs(vector(i))>max ) then
            max = abs(vector(i))
        end if
    end do
    print*, "Norma del vector: ",max
end program ejercicio7