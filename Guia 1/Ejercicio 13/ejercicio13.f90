program ejercicio13
    implicit none
    integer N,M,K
    real(8) :: VECTOR (100)
    real (8), allocatable :: MATRIZ(:,:)
    call ingresarVector(VECTOR,K)
    call ingresarMatriz(MATRIZ,N,M)
    write(*,'(A,F5.2)') "Norma del vector: ",maxval(VECTOR)
    write(*,'(A,F5.2)') "Norma de la matriz: ",sum(MATRIZ)
    deallocate(MATRIZ)
    contains

    subroutine ingresarVector (VECTOR,N)
        real(8) :: VECTOR (100)
        integer N, ioError
        N=0
        open(unit=1,file="./txt/vector.txt");
        read(1,'(F5.2)',ioStat = ioError) VECTOR(N+1)
        do while (ioError == 0)
            N = N +1
            read(1,'(F5.2)',ioStat = ioError) VECTOR(N+1)
        end do
        close(1);
    end subroutine

    subroutine ingresarMatriz (MATRIZ,N,M)
        real (8), allocatable :: MATRIZ(:,:)
        integer i,j,N,M
        open(unit=2,file="./txt/matriz.txt")
        read(2,'(2I2)',advance = 'NO') N, M
        allocate(MATRIZ(N,M));
        do i = 1, N
            read(2,*)
            do j = 1, M
                read(2,'(F5.2)',advance='NO') MATRIZ(i,j)
            end do
        end do
        close(2)
    end subroutine
end program ejercicio13