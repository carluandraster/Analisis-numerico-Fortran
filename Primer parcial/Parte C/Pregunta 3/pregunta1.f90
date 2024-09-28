program resolucionSistema
    implicit none
    INTEGER, PARAMETER :: MAXELEM = 20
    REAL(8), DIMENSION(MAXELEM, MAXELEM) :: A, L, U
    REAL(8), DIMENSION(MAXELEM, 1) :: B, C, X
    INTEGER :: N

    print*,"Solucion"
    call leerArchivo(A, B, N)
    call crout(A, L, U, N)
    call primeraSolucion(B, L, C, N)
    call solucionFinal(U, X, C, N)
    call mostrarSolucion(X, N)



contains

    subroutine leerArchivo(matriz, term_indep, N)
        INTEGER :: N, i, j
        REAL(8), DIMENSION(MAXELEM, MAXELEM) :: matriz
        REAL(8), DIMENSION(MAXELEM, 1) :: term_indep
        open(unit=1, file="sistema.txt")
        read(1, '(I1)') N
        do i = 1, N
            do j = 1, N
                read(1, '(F5.1)', advance='NO') matriz(i,j)
            end do
            read(1, '(F5.1)') term_indep(i, 1)
        end do
        close(1)
    end subroutine leerArchivo

    subroutine crout(A, L, U, N)
        REAL(8), DIMENSION(MAXELEM, MAXELEM) :: A, L, U
        INTEGER :: i, j, k, N

        ! Inicialización de L y U
        L = 0.0
        U = 0.0

        ! Factorización de Crout
        do j = 1, N
            ! Calculo de la matriz L
            do i = j, N
                L(i,j) = A(i,j)
                do k = 1, j-1
                    L(i,j) = L(i,j) - L(i,k) * U(k,j)
                end do
            end do

            ! Calculo de la matriz U
            U(j,j) = 1.0  ! Diagonal de U debe ser 1
            do i = j+1, N
                U(j,i) = A(j,i)
                do k = 1, j-1
                    U(j,i) = U(j,i) - L(j,k) * U(k,i)
                end do
                U(j,i) = U(j,i) / L(j,j)
            end do
        end do
    end subroutine crout

    subroutine primeraSolucion(B, L, C, N)
        REAL(8) :: sumatoria
        REAL(8), DIMENSION(MAXELEM, 1) :: B, C
        REAL(8), DIMENSION(MAXELEM, MAXELEM) :: L
        INTEGER :: i, j, N

        ! Solución de L * C = B
        do i = 1, N
            sumatoria = 0.0
            do j = 1, i-1
                sumatoria = sumatoria + L(i,j) * C(j,1)
            end do
            C(i,1) = (B(i,1) - sumatoria) / L(i,i)
        end do
    end subroutine primeraSolucion

    subroutine solucionFinal(U, X, C, N)
        REAL(8) :: sumatoria
        REAL(8), DIMENSION(MAXELEM, 1) :: X, C
        REAL(8), DIMENSION(MAXELEM, MAXELEM) :: U
        INTEGER :: i, j, N

        ! Solución de U * X = C
        do i = N, 1, -1
            sumatoria = 0.0
            do j = i+1, N
                sumatoria = sumatoria + U(i,j) * X(j,1)
            end do
            X(i,1) = (C(i,1) - sumatoria) / U(i,i)
        end do
    end subroutine solucionFinal

    subroutine mostrarSolucion(X, N)
        INTEGER :: N, i
        REAL(8), DIMENSION(MAXELEM, 1) :: X
        do i = 1, N
            write(*,'(A,I1,A,F10.4)') "x", i, " = ", X(i,1)
        end do
    end subroutine mostrarSolucion
end program resolucionSistema