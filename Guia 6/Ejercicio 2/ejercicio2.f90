program ejercicio2
    implicit none
    REAL(8),DIMENSION(3) :: x,y,coef
    INTEGER :: i

    x = [94d0,205d0,371d0]
    y = [929d0,902d0,860d0]
    call polinomio_lagrange(x,y,coef,3)
    do i = 1, 3
        print*,coef(i)
    end do

    contains

    subroutine polinomio_lagrange(x, y, coef,n)
        implicit none
        integer, intent(in) :: n
        real(8), dimension(n), intent(in) :: x, y
        real(8), dimension(n), intent(out) :: coef
        real(8), dimension(n) :: l
        integer :: i, j, k
    
        coef = 0.0
    
        do i = 1, n
            ! Inicializa el polinomio base l_i como [1, 0, ..., 0]
            l = 0.0
            l(1) = 1.0
    
            ! Construye el polinomio l_i para el valor actual de x(i)
            do j = 1, n
                if (j /= i) then
                    do k = n, 2, -1
                        l(k) = l(k) * (-x(j)) + l(k-1)
                    end do
                    l(1) = l(1) * (-x(j))
                    l = l / (x(i) - x(j))
                end if
            end do
    
            ! Añade l_i * y(i) a los coeficientes del polinomio
            coef = coef + y(i) * l
        end do
    end subroutine polinomio_lagrange
    
end program ejercicio2