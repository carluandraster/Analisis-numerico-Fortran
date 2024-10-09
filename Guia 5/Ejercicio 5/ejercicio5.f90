program ejercicio5
    implicit none
    INTEGER,PARAMETER :: N = 4, M = 5
    REAL(8),DIMENSION(0:M,0:N) :: A

    A = reshape([0d0,0d0,100d0,100d0,0d0,0d0,&
    0d0,50d0,75d0,100d0,100d0,100d0,&
    0d0,20d0,40d0,60d0,80d0,100d0,&
    0d0,25d0,50d0,75d0,100d0,0d0,&
    0d0,0d0,0d0,0d0,0d0,0d0],shape(A))

    call calcularNodos(A,0.01d0,1.112766298d0)
    call imprimirMatriz(A)

    contains
    subroutine calcularNodos(A,tol,omega)
        REAL(8),DIMENSION(0:M,0:N) :: A,B
        REAL(8) :: tol,omega,error
        INTEGER :: i,j,limInf,limSup

        ! Inicializamos B con los valores de A
        B = A
    
        ! Iteramos hasta que el error máximo sea menor que la tolerancia
        do while (.true.)
            error = 0.0
    
            do i = 1, M-1
                select case (i)
                    case(1)
                        limInf = 2
                        limSup = 3
                    case(2)
                        limInf = 1
                        limSup = 3
                    case(3)
                        limInf = 2
                        limSup = 3
                    case(4)
                        limInf = 2
                        limSup = 2
                end select
                do j = limInf, limSup
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
    end subroutine calcularNodos

    subroutine imprimirMatriz(A)
        REAL(8),DIMENSION(0:M,0:N),INTENT(IN) :: A
        INTEGER :: i,j
        do  j= 0, N
            do i = 0, M
                write(*,'(F6.1)',advance='NO') A(i,j)
            end do
            write(*,*)
        end do
    end subroutine imprimirMatriz
end program ejercicio5