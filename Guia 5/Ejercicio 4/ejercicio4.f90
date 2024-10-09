program ejercicio4
    implicit none
    INTEGER,PARAMETER :: N = 3, M = 4
    REAL(8),DIMENSION(0:M+1,0:N+1) :: A

    A = reshape([0d0,60d0,120d0,180d0,240d0,300d0,&
                0d0,112d0,164d0,207d0,233d0,225d0,&
                0d0,147d0,182d0,200d0,192d0,150d0,&
                0d0,40d0,40d0,40d0,40d0,75d0,&
                0d0,0d0,0d0,0d0,0d0,0d0],shape(A))

    call calcNodos(A,0.01d0,1.112766298d0)
    call imprimirMatriz(A)
    call graficar(A)
    call system("gnuplot -persist script.p")

    contains
    subroutine calcNodos(A,tol,omega)
        REAL(8), DIMENSION(0:M+1,0:N+1), INTENT(INOUT) :: A
        REAL(8) :: tol, omega,error
        REAL(8), DIMENSION(0:M+1,0:N+1) :: B
        INTEGER :: i, j
    
        ! Inicializamos B con los valores de A
        B = A
    
        ! Iteramos hasta que el error máximo sea menor que la tolerancia
        do while (.true.)
            error = 0.0
    
            do i = 1, M
                do j = 1, N
                    ! Guardamos el valor anterior
                    B(i,j) = A(i,j)
    
                    ! Sobrerelajación: cálculo del nuevo valor ajustado con omega
                    A(i,j) = (1.0 - omega) * A(i,j) + omega * 0.25 * (A(i+1,j) + A(i-1,j) + A(i,j+1) + A(i,j-1))
    
                    ! Calculamos el error absoluto para verificar la convergencia
                    error = max(error, abs(A(i,j) - B(i,j)))
                end do
            end do
    
            ! Verificamos si el error es menor que la tolerancia
            if (error < tol) exit
        end do
    end subroutine calcNodos

    subroutine graficar(A)
        REAL(8),DIMENSION(0:M+1,0:N+1) :: A
        INTEGER i,j
        open(unit=1,file="datos.dat")
        do i = 0, M+1
            do j = 0, N+1
                if ( abs(A(i,j)-50)<=6 ) then
                    write(1,'(2I2)') i,j
                end if
            end do
        end do
        close(1)
    end subroutine graficar

    subroutine imprimirMatriz(A)
        REAL(8),DIMENSION(0:M+1,0:N+1),INTENT(IN) :: A
        INTEGER :: i,j
        do  j= 0, N+1
            do i = 0, M+1
                write(*,'(F6.1)',advance='NO') A(i,j)
            end do
            write(*,*)
        end do
    end subroutine imprimirMatriz
end program ejercicio4