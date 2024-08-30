program ejercicio12
    implicit none
    real(8), allocatable :: MATRIZ(:,:)
    integer N,M

    call cargarMatriz(MATRIZ,N,M)
    print*,esEDD(MATRIZ,N,M)

    deallocate(MATRIZ)

    contains
    subroutine cargarMatriz(MATRIZ,N,M)
        real(8), allocatable :: MATRIZ(:,:)
        integer N,M,i,j
        open(unit=1,file="matriz.txt")
        read(1,'(2I2)',advance='NO') N,M
        allocate(MATRIZ(N,M))
        do i = 1, N
            read(1,*)
            do j = 1, M
                read(1,'(F5.2)',advance='NO') MATRIZ(i,j)
            end do
        end do
        close(1)
    end subroutine
    function esEDD (MATRIZ,N,M)
        logical esEDD
        real(8) :: MATRIZ(:,:),suma
        integer N,M,i,j
        i=1

        ! Sumar fila (excepto la diagonal)
        suma = 0
        do j = 1, M
            if ( j/=i ) then
                suma = suma + abs(MATRIZ(i,j))
            end if
        end do
        ! Recorrer matriz
        do while (i<=N .and. suma<abs(MATRIZ(i,i)))
            i = i+1
            suma = 0
            do j = 1, M
                if ( j/=i ) then
                    suma = suma + abs(MATRIZ(i,j))
                end if
            end do
        end do

        esEDD = i>N
    end function
end program ejercicio12