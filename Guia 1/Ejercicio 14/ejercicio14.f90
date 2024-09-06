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
    subroutine grabarMatriz (MATRIZ, N, ruta)
        integer N,i,j
        real(8) :: MATRIZ(N,N)
        character(len=20) ruta
        open(unit = 2,file = ruta);
        write(2,'(I2)') N
        do i = 1, N
            do j = 1, N
                write(2,'(F5.2)', advance='NO') MATRIZ(i,j)
            end do
            write(2,*)
        end do
        close(2)
    end subroutine

    ! Ingresar un vector de n componentes
    subroutine ingresarVector (VECTOR, N, ruta)
        integer N, ioError
        real(8) :: VECTOR(N)
        character(len=20) ruta
        open(unit=3,file = ruta)
        N = 0
        read(3,'(F5.2)',ioStat = ioError) VECTOR(N+1)
        do while (ioError == 0)
            N = N+1
            read(3,'(F5.2)',ioStat = ioError) VECTOR(N+1)
        end do
        close(3)
    end subroutine

    ! Imprimir por pantalla un vector de n componentes
    subroutine imprimirVector(VECTOR,  N)
        integer :: N,i
        real(8) ::  VECTOR(N)
        do i = 1, N
            write(*,'(F5.2)',advance='NO') VECTOR(i)
        end do
    end subroutine imprimirVector

    ! Grabar en un archivo un vector de n componentes
    subroutine grabarVector(VECTOR,  N, ruta)
        integer :: N,i
        real(8) ::  VECTOR(N)
        character(len=20) :: ruta
        open(unit = 1, file = ruta)
        do i = 1, N
            write(1,'(F5.2)') VECTOR(N)
        end do
        close(1)
    end subroutine grabarVector
end module ejercicio14