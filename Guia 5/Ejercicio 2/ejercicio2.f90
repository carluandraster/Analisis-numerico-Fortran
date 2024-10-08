program ejercicio2
    implicit none
    INTEGER,PARAMETER :: N=2,M=5
    REAL(8),DIMENSION(0:M+1,0:N+1) :: A
    INTEGER i,j

    A = reshape([100d0,0d0,0d0,0d0,0d0,0d0,0d0,&
    100d0,75d0,50d0,25d0,0d0,0d0,0d0,&
    100d0,75d0,50d0,25d0,0d0,0d0,0d0,&
    100d0,0d0,0d0,0d0,0d0,0d0,0d0],shape(A))

    call calcularNodos(A,0.01d0)
    do j = 0, N+1
        do i = 0, M+1
            write(*,'(F7.1)',advance='NO') A(i,j)
        end do
        write(*,*)
    end do

    contains
    subroutine calcularNodos(A,tol)
        REAL(8),DIMENSION(0:M+1,0:N+1) :: A,B
        REAL(8) :: tol
        INTEGER :: i,j
        B = tol*A
        do while (maxval(abs(A-B)) > tol)
            B = A
            do i = 1, M
                do j = 1, N
                    A(i,j) = (A(i+1,j)+A(i-1,j)+A(i,j+1)+A(i,j-1))/4.0
                end do
            end do
        end do
    end subroutine calcularNodos
end program ejercicio2