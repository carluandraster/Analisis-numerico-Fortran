program ejercicio2
    implicit none
    INTEGER,PARAMETER :: MAXELEM = 20
    REAL(8),DIMENSION(MAXELEM,MAXELEM) :: A,L,U
    REAL(8),DIMENSION(MAXELEM,1) ::  B,C,X
    INTEGER :: N

    call leerArchivo(1,"incisoA.txt",A,B,N)
    call crout(A,L,U,N)
    call primeraSolucion(B,L,C,N)
    call solucionFinal(U,X,C,N)
    call mostrarSolucion(X,N)

    contains

    subroutine leerArchivo(unidad,  archivo, matriz, term_indep,N)
        INTEGER :: unidad,N,i,j
        CHARACTER(LEN=11) :: archivo
        REAL(8),DIMENSION(MAXELEM,MAXELEM) :: matriz
        REAL(8),DIMENSION(MAXELEM,1) :: term_indep
        open(unit=unidad,file=archivo)
        read(unidad,'(I1)') N
        do i = 1, N
            do j = 1, N
                read(unidad,'(F8.4)',advance='NO') matriz(i,j)
            end do
            read(unidad,'(F8.4)') term_indep(i,1)
        end do
        close(unidad)
    end subroutine leerArchivo

    subroutine crout(A,L,U,N)
        REAL(8),DIMENSION(MAXELEM,MAXELEM) ::  A,L,U,VC
        INTEGER fila, col, i, k,N
        VC = A
        do i = 1, N
            do k = i+1, n
                L(i,k) = 0
                U(k,i) = 0
            end do
        end do
        do i = 1, n
            A(1,i) = A(1,i)/A(1,1)
            U(i,i) = 1
            L(i,1) = A(i,1)
        end do
        DO i=2, N
            ! Calcula la columna i
            DO fila=i, N
                DO k=1,i-1
                    A(fila,i)=A(fila,i) - A(fila,k)*A(k,i)
                END DO
            END DO
            ! Calcula fila i
            DO col=i+1, N
                DO k=1,i-1
                    A(i, col)=A(i,col) - A(i,k)*A(k,col)
                END DO
                A(i,col) = A(i,col) / A(i,i)
            END DO
        END DO
        do i = 1, N
            do k = 1, N
                if ( k<=i ) then
                    L(i,k) = A(i,k)
                else
                    U(i,k) = A(i,k)
                end if
            end do
        end do
        A = VC
    end subroutine crout

    subroutine primeraSolucion(B,L,C,N)
        REAL(8) :: sumatoria
        REAL(8),DIMENSION(MAXELEM,1) :: B,C
        REAL(8),DIMENSION(MAXELEM,MAXELEM) :: L
        INTEGER :: i,j,N
        do i = 1, N
            sumatoria = 0
            do j = 1, i-1
                sumatoria = sumatoria + L(i,j)*c(j,1)
            end do
            C(i,1) = (B(i,1)-sumatoria)/L(i,i)
        end do
    end subroutine primeraSolucion

    subroutine solucionFinal(U,X,C,N)
        REAL(8) :: sumatoria
        REAL(8),DIMENSION(MAXELEM,1) :: X,C
        REAL(8),DIMENSION(MAXELEM,MAXELEM) :: U
        INTEGER :: i,j,N
        do i = N, 1,-1
            sumatoria = 0
            do j = i+1, N
                sumatoria = sumatoria+U(i,j)*X(j,1)
            end do
            X(i,1) = C(i,1)-sumatoria
        end do
    end subroutine solucionFinal

    subroutine mostrarSolucion(X,N)
        INTEGER :: N,i
        REAL(8),DIMENSION(MAXELEM,1) :: X
        do i = 1, N
            write(*,'(A,I1,A,F10.6)') "x",i," = ",X(i,1)
        end do
    end subroutine mostrarSolucion
end program ejercicio2