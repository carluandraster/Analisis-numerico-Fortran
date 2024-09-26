program ejercicio2
    implicit none
    INTEGER,PARAMETER :: MAXELEM = 3
    REAL(8),DIMENSION(MAXELEM,MAXELEM) :: A,L,U
    REAL(8),DIMENSION(MAXELEM,1) ::  B
    INTEGER :: N,i,j

    call leerArchivo(1,"prueba1.txt",A,B,N)
    call crout(A,L,U,N)
    A = L*U
    do i = 1, N
        do j = 1, N
            write(*,'(F8.4)',advance='NO') L(i,j)
        end do
        write(*,*)
    end do

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

    subroutine crout(A,L,  U,N)
        REAL(8) A(MAXELEM,MAXELEM),L(MAXELEM,MAXELEM),U(MAXELEM,MAXELEM)
        INTEGER fila, col, i, k,N
        do i = 1, N
            do k = i+1, n
                L(i,k) = 0
                U(k,i) = 0
            end do
        end do
        do i = 1, n
            U(1,i) = A(1,i)/A(1,1)
            U(i,i) = 1
            L(i,1) = A(i,1)
        end do
        DO i=2, N
            ! Calcula la columna i
            DO fila=i, N
                DO k=1,i-1
                    L(fila,i)=A(fila,i) - A(fila,k)*A(k,i)
                END DO
            END DO
            ! Calcula fila i
            DO col=i+1, N
                DO k=1,i-1
                    U(i, col)=A(i,col) - A(i,k)*A(k,col)
                END DO
                U(i,col) = A(i,col) / A(i,i)
            END DO
        END DO 
    end subroutine crout
end program ejercicio2