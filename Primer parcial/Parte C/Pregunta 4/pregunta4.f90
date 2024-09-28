program jacobi
    implicit none
    integer, parameter :: n = 4  ! Tamaño de la matriz
    real(8) :: A(n, n), b(n), x_anterior(n), x_nuevo(n)
    integer :: i, j, iter, max_iter

    ! Definir la matriz A y el vector b del sistema A*x = b
    A = reshape([9.0d0, 4.0d0, -1.0d0, 0.0d0, &
                 2.0d0, 10.0d0, -1.0d0, -4.0d0, &
                  0.0d0, -2.0d0, 8.0d0, 2.0d0, &
                  1.0d0, 3.0d0, 2.0d0, 8.0d0], shape(A))

    b = (/22.0d0, 31.0d0, 20.0d0, 35.0d0/)

    max_iter = 5  ! Máximo número de iteraciones

    ! Inicialización del vector x
    x_anterior = (/1.0d0,1.0d0,1.0d0,1.0d0/)

    ! Método iterativo de Jacobi
    do iter = 1, max_iter
        x_nuevo = x_anterior  ! Almacenar los valores anteriores

        ! Iteración para cada variable
        do i = 1, n
            x_nuevo(i) = b(i)
            do j = 1, n
                if (i /= j) then
                    x_nuevo(i) = x_nuevo(i) - A(i,j) * x_anterior(j)
                end if
            end do
            x_nuevo(i) = x_nuevo(i) / A(i,i)  ! División por el elemento diagonal
        end do

        x_anterior = x_nuevo  ! Actualizar el vector de aproximaciones
    end do

    ! Mostrar el resultado
    print *, 'Solucion:'
    do i = 1, n
        write(*,'(A,I1,A,F4.1)') 'x', i, ' = ', x_nuevo(i)
    end do

end program jacobi