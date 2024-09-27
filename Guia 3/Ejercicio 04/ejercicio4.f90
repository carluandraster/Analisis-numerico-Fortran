program ejercicio4
    implicit none
    
    contains

    subroutine creaMatrizIdentidad(orden,matrizIdentidad)
        REAL(8), DIMENSION(:,:) :: matrizIdentidad
        INTEGER orden,i
        matrizIdentidad = 0.
        do i = 1, orden
            matrizIdentidad(i,i) = 1.
        end do
    end subroutine creaMatrizIdentidad

    subroutine GaussJordan(matriz, matrizIdentidad)
        INTEGER :: N,t, fila,i
        REAL(8), DIMENSION(:,:) :: matriz,matrizIdentidad
        N = SIZE(matriz,DIM=1)
        DO t=1, N
            do i = 1, N
                matriz(t,i) = matriz(t,i)/matriz(t,t)
                matrizIdentidad(t,i) = matrizIdentidad(t,i)/matriz(t,t)
            end do
            DO fila=1, t -1
                do i = t+1, N
                    matriz(fila,i) = matriz(fila,i) - matriz(t,i)*matriz(fila,t) / matriz(t,t)
                    matrizIdentidad(fila,i) = matrizIdentidad(fila,i) - matrizIdentidad(t,i)*matrizIdentidad(fila,t) / matrizIdentidad(t,t)
                end do
                matriz(fila,t) = 0.0
            ENDDO
            DO fila=t+1, N
                do i = t+1, N+1
                    matriz(fila,i) = matriz(fila,i) - matriz(t,i)*matriz(fila,t) / matriz(t,t)
                    matrizIdentidad(fila,i) = matrizIdentidad(fila,i) - matrizIdentidad(t,i)*matrizIdentidad(fila,t) / matrizIdentidad(t,t)
                end do
                matriz(fila,t) = 0.0
            ENDDO
        ENDDO
    end subroutine GaussJordan

    SUBROUTINE Inversa(matriz)
        ! Calcula la inversa de una matriz, por Gauss-Jordan
        REAL(8), DIMENSION(:,:), INTENT(IN) :: matriz
        REAL(8), ALLOCATABLE, DIMENSION(:,:) :: matrizIdentidad
        INTEGER orden
        orden = SIZE(matriz, DIM=1)
        CALL creaMatrizIdentidad(orden, matrizIdentidad)
        CALL GaussJordan(matriz, matrizIdentidad)
        DEALLOCATE(matrizIdentidad)
        
    END SUBROUTINE
end program ejercicio4