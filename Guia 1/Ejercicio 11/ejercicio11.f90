program ejercicio11
    implicit none
    integer n,i,j
    real(8), allocatable :: HILBERT(:,:)

    ! Ingresar n
    print*, "Ingrese n: "
    read*, n
    do while (n<1)
        print*,"Ingrese un numero positivo: "
        read*, n
    end do

    ! Imprimir la matriz de Hilbert de n x n
    allocate(HILBERT(n,n))
    call calcularHilbert (HILBERT, n)
    do i = 1, n
        do j = 1, n
            write(*,'(F5.2)',advance='NO') HILBERT(i,j)
        end do
        write(*,*)
    end do
    deallocate(HILBERT)

    contains
    subroutine calcularHilbert(HILBERT, n)
        integer i,j,n
        real(8) HILBERT(n,n)

        do i = 1, n
            do j = 1, n
                HILBERT(i,j) = 1.0/(i+j-1)
            end do
        end do
    end subroutine
end program ejercicio11