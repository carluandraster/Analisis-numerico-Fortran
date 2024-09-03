module ejercicio14
    implicit none
    
contains
    ! Ingresar una matriz de nxn
    subroutine cargarMatriz (MATRIZ,N,ruta)
        real(8), allocatable :: MATRIZ(:,:)
        integer N,i,j
        character(len=20) ruta
        open(unit=1,file=ruta)
        read(1,'(I2)') N
        allocate(MATRIZ(N,N))
        do i = 1, N
            do j = 1, N
                read(1,'(F5.2)',advance='NO') MATRIZ(i,j)
            end do
            read(1,*)
        end do
        close(1)
        deallocate(MATRIZ)
    end subroutine

    ! Imprimir por pantalla una matriz de nxn
    subroutine imprimirMatriz (MATRIZ,N)
        integer N, i,j
        real(8) :: MATRIZ(N,N)
        do i = 1, N
            do j = 1, N
                write(*,'(F5.2,x)',advance='NO') MATRIZ(i,j)
            end do
            write(*,*)
        end do
    end subroutine

    ! Grabar en un archivo una matriz de nxn
end module ejercicio14