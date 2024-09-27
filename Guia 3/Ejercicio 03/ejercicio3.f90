program ejercicio3
    implicit none
    REAL(8), DIMENSION(:),ALLOCATABLE :: u, d, l
    REAL(8), DIMENSION(:,:), ALLOCATABLE :: term_indep
    INTEGER,PARAMETER :: N = 10
    ALLOCATE(d(N),u(N-1),l(N-1),term_indep(N,1))

    call leerArchivo(u,d,l,term_indep)
    call Thomas(u,d,l,term_indep)

    DEALLOCATE(term_indep)

    contains

    subroutine leerArchivo(u,d,l,term_indep)
        REAL(8), DIMENSION(:) :: u, d, l
        REAL(8), DIMENSION(:,:) :: term_indep
        INTEGER :: i
        open(unit=1,file="sistema.txt")
        do i = 1, N-1
            write(1,'(F8.4)') u(i)
        end do
        do i = 1, N
            write(1,'(F8.4)') d(i)
        end do
        do i = 1, N-1
            write(1,'(F8.4)') l(i)
        end do
        do i = 1, N
            write(1,'(F8.4)') term_indep(i,1)
        end do
        close(1)
    end subroutine leerArchivo

    subroutine imprimeMatriz(b)
        REAL(8), DIMENSION(:,:) :: b
        INTEGER :: orden,cant_vec,i,j
        orden = SIZE(b,DIM=1)
        cant_vec = SIZE(b,DIM=2)
        do i = 1, orden
            do j = 1, cant_vec
                write(*,'(F10.6)',advance='NO') b(i,j)
            end do
            write(*,*)
        end do
    end subroutine imprimeMatriz

    SUBROUTINE Thomas(u_orig, d_orig, l_orig, term_indep)
        ! Metodo de Thomas para matrices Tri-Diagonales
        REAL(8), DIMENSION(:,:), INTENT(IN) :: term_indep
        REAL(8), DIMENSION(:), INTENT(IN) :: u_orig, d_orig, l_orig
        REAL(8), DIMENSION(:,:), ALLOCATABLE :: b
        REAL(8), DIMENSION(:), ALLOCATABLE :: u, d, l
        INTEGER orden, cant_vec, i
        
        orden = SIZE(term_indep, DIM=1)
        cant_vec = SIZE(term_indep, DIM=2)
        ! Realiza copias del original
        ALLOCATE(u(orden), d(orden), l(orden))
        u = u_orig
        d = d_orig
        l = l_orig
        ALLOCATE(b(orden, cant_vec))
        b = term_indep
        ! Aqui comienza el algoritmo
        DO i=1, orden-1
            u(i) = u(i) / d(i)
            b(i,:) = b(i,:) / d(i)
            d(i) = 1.0
            d(i+1) = d(i+1) - l(i+1)*u(i)
            b(i+1,:) = b(i+1,:) - l(i+1)*b(i,:)
            l(i+1) = 0.0
        ENDDO
        ! Obtencion de la solucion por Sustitucion Inversa
        b(orden,:) = b(orden,:)/d(orden)
        DO i=orden-1, 1, -1
            b(i,:) = b(i,:) - u(i)*b(i+1,:) / d(i)
        END DO
        CALL imprimeMatriz(b)
   
        DEALLOCATE(u, d, l, b)
    END SUBROUTINE
end program ejercicio3