program ejercicio1
    implicit none
    INTEGER,PARAMETER :: MAXELEM = 20
    REAL(8),DIMENSION(MAXELEM) :: X
    INTEGER :: N
    REAL(8),DIMENSION(MAXELEM,MAXELEM) :: matriz,matriz1
    REAL(8),DIMENSION(MAXELEM,MAXELEM+1) :: mat_amp
    REAL(8),DIMENSION(MAXELEM,1) :: term_indep,term_indep1


    ! Inciso a
    print*,"Inciso a"
    call leerArchivo(1,"incisoA.txt",matriz,term_indep,N)
    matriz1 = matriz
    term_indep1 = term_indep

    print*,"Resolucion por Metodo de Gauss"
    call Gauss(matriz, term_indep,N,mat_amp)
    X = solucionPorGauss(mat_amp,N)
    call mostrarSolucion(X,N)

    print*,"Resolucion por Metodo de Gauss-Jordan"
    call GaussJordan(matriz1, term_indep1,N,mat_amp)
    X = solucionPorJordan(mat_amp,N)
    call mostrarSolucion(X,N)


    ! Inciso b
    print*,"Inciso b"
    call leerArchivo(2,"incisoB.txt",matriz,term_indep,N)
    
    matriz1 = matriz
    term_indep1 = term_indep

    print*,"Resolucion por Metodo de Gauss"
    call Gauss(matriz, term_indep,N,mat_amp)
    X = solucionPorGauss(mat_amp,N)
    call mostrarSolucion(X,N)

    print*,"Resolucion por Metodo de Gauss-Jordan"
    call GaussJordan(matriz1, term_indep1,N,mat_amp)
    X = solucionPorJordan(mat_amp,N)
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

    subroutine creaMatrizAmpliada(matriz, term_indep, mat_amp,N)
        INTEGER :: N,i,j
        REAL(8),DIMENSION(MAXELEM,MAXELEM) :: matriz
        REAL(8),DIMENSION(MAXELEM,1) :: term_indep
        REAL(8),DIMENSION(MAXELEM,MAXELEM+1) :: mat_amp
        do i = 1, N
            do j = 1, N
                mat_amp(i,j) = matriz(i,j)
            end do
            mat_amp(i,N+1) = term_indep(i,1)
        end do
    end subroutine creaMatrizAmpliada

    SUBROUTINE Gauss(matriz, term_indep,N,mat_amp)
        ! Metodo de Gauss
        REAL(8), DIMENSION(MAXELEM,MAXELEM), INTENT(IN) :: matriz
        REAL(8), DIMENSION(MAXELEM,1), INTENT(IN) :: term_indep
        REAL(8), DIMENSION(MAXELEM,MAXELEM+1) :: mat_amp
        INTEGER t, fila,N,i
        CALL creaMatrizAmpliada(matriz, term_indep, mat_amp,N)
        DO t=1, N
            DO fila=t+1,N
                do i = t+1, N+1
                    mat_amp(fila,i) = mat_amp(fila,i) - mat_amp(t,i)*mat_amp(fila,t) / mat_amp(t,t)
                end do
                mat_amp(fila,t) = 0.0
            ENDDO
        ENDDO
    END SUBROUTINE Gauss

    SUBROUTINE GaussJordan(matriz, term_indep,N,mat_amp)
        ! Metodo de Gauss-Jordan
        INTEGER :: N
        REAL(8), DIMENSION(MAXELEM,MAXELEM), INTENT(IN) :: matriz
        REAL(8), DIMENSION(MAXELEM,1), INTENT(IN) :: term_indep
        REAL(8), DIMENSION(MAXELEM,MAXELEM+1) :: mat_amp
        INTEGER t, fila,i
        CALL creaMatrizAmpliada(matriz, term_indep, mat_amp,N)
        DO t=1, N
            DO fila=1, t -1
                do i = t+1, N+1
                    mat_amp(fila,i) = mat_amp(fila,i) - mat_amp(t,i)*mat_amp(fila,t) / mat_amp(t,t)
                end do
                mat_amp(fila,t) = 0.0
            ENDDO
            DO fila=t+1, N
                do i = t+1, N+1
                    mat_amp(fila,i) = mat_amp(fila,i) - mat_amp(t,i)*mat_amp(fila,t) / mat_amp(t,t)
                end do
                mat_amp(fila,t) = 0.0
            ENDDO
        ENDDO
    END SUBROUTINE

    function solucionPorGauss(mat_amp,N)
        INTEGER :: N,i,j
        REAL(8),DIMENSION(MAXELEM,MAXELEM+1) :: mat_amp
        REAL(8),DIMENSION(N) :: solucionPorGauss,X
        REAL(8) :: sumatoria
        
        do i = N, 1,-1
            sumatoria = 0
            do j = i+1, N
                sumatoria = sumatoria + mat_amp(i,j)*X(j)
            end do
            X(i) = (mat_amp(i,N+1)-sumatoria)/mat_amp(i,i)
        end do
        solucionPorGauss = X
    end function solucionPorGauss

    function solucionPorJordan(mat_amp,N)
        INTEGER :: N,i
        REAL(8),DIMENSION(MAXELEM,MAXELEM+1) :: mat_amp
        REAL(8),DIMENSION(N) :: solucionPorJordan
        do i = 1, N
            solucionPorJordan(i) = mat_amp(i,N+1)/mat_amp(i,i)
        end do
    end function solucionPorJordan

    subroutine mostrarSolucion(X,N)
        INTEGER :: N,i
        REAL(8),DIMENSION(MAXELEM) :: X
        do i = 1, N
            write(*,'(A,I1,A,F10.6)') "x",i," = ",X(i)
        end do
    end subroutine mostrarSolucion
end program ejercicio1