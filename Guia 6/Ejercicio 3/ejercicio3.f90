program ejercicio3
    implicit none
    REAL(8),DIMENSION(0:2) :: coefA
    REAL(8),DIMENSION(0:4) :: x,y,coefC
    
    ! Inciso a
    x = [0.25d0,0.5d0,0.75d0,0d0,0d0]
    y = [0.8109d0,0.6931d0,0.5596d0,0d0,0d0]
    call polinomio_lagrange(x,y,coefA,2)
    print*,"Coeficientes del Polinomio de Lagrange que pasa por i=1,2,3: "
    call imprimirVector(coefA,2)

    ! Inciso b
    write(*,'(A,F10.6)') "Error de interpolacion estimado en x=0.6: ",error(0.6d0,x,2,-0.26d0)

    ! Inciso c
    x = [0d0,0.25d0,0.5d0,0.75d0,1d0]
    y = [0.9162d0,0.8109d0,0.6931d0,0.5596d0,0.4055d0]
    call polinomio_lagrange(x,y,coefC,4)
    print*,"Coeficientes del Polinomio de Lagrange que pasa por todos los puntos: "
    call imprimirVector(coefC,4)

    ! Inciso d
    write(*,'(A,F10.6)') "p_c(0,6) = ",p(0.6d0,coefC,4)
    write(*,'(A,F10.6)') "p_a(0,6) = ",p(0.6d0,coefA,2)

    contains
    subroutine polinomio_lagrange(x, y, coef, n)
        implicit none
        integer, intent(in) :: n
        real(8), dimension(0:n), intent(in) :: x, y
        real(8), dimension(0:n), intent(out) :: coef
        real(8), dimension(0:n) :: l
        integer :: i, j, k
    
        coef = 0.0
    
        do i = 0, n
            ! Inicializa el polinomio base l_i como [1, 0, ..., 0]
            l = 0.0
            l(0) = 1.0
    
            ! Construye el polinomio l_i para el valor actual de x(i)
            do j = 0, n
                if (j /= i) then
                    do k = n, 1, -1
                        l(k) = l(k) * (-x(j)) + l(k-1)
                    end do
                    l(0) = l(0) * (-x(j))
                    l = l / (x(i) - x(j))
                end if
            end do
    
            ! Añade l_i * y(i) a los coeficientes del polinomio
            coef = coef + y(i) * l
        end do
    end subroutine polinomio_lagrange

    subroutine imprimirVector(v,  n)
        REAL(8),DIMENSION(0:n), intent(in) :: v
        INTEGER, intent(in) ::  n
        INTEGER :: i
        do i = 0, n
            print*,v(i)
        end do
    end subroutine imprimirVector

    function factorial(n)
        integer, intent(in) :: n
        REAL(8) :: factorial,aux
        INTEGER :: i
        aux = 1
        do i = 1, n
            aux = aux*i
        end do
        factorial = aux
    end function factorial

    function error(x,x_vector,n,derivada)
        REAL(8) :: error
        REAL(8), intent(in) :: x,derivada
        REAL(8), dimension(0:n),intent(in) :: x_vector
        INTEGER, intent(in) :: n
        INTEGER :: i
        REAL(8) :: aux
        aux = 1
        do i = 0, n
            aux = aux*(x-x_vector(i))
        end do
        error = aux * derivada / factorial(n+1)
    end function error
    
    function p(x,coef,n)
        REAL(8) :: p,x,aux
        INTEGER :: n,i
        REAL(8),DIMENSION(0:n) :: coef
        aux = 0
        do i = 0, n
            aux = aux + coef(i)*x**i
        end do
        p = aux
    end function p
end program ejercicio3