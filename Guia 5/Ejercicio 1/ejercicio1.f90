program ejercicio1
    implicit none
    INTEGER,PARAMETER :: dimension=2
    REAL(8),DIMENSION(0:dimension+1,0:dimension+1) :: A
    INTEGER i,j

    A = reshape([1.2d0,2.3d0,4.4d0,7.5d0,&
    1.2d0, 1.d0, 1.d0, 6.5d0,&
    1.2d0,1.d0,1.d0,5.5d0,&
    1.2d0,2.3d0,3.4d0,4.5d0],shape(A))

    call calcularNodos(A,0.01d0)
    print*,"Sin sobrerrelajacion: "
    do j = 0, dimension+1
        do i = 0, dimension+1
            write(*,'(F4.1)',advance='NO') A(i,j)
        end do
        write(*,*)
    end do

    call sobreRelajacion(A,0.01d0,1.3d0)
    print*,"Con sobrerrelajacion: "
    do j = 0, dimension+1
        do i = 0, dimension+1
            write(*,'(F4.1)',advance='NO') A(i,j)
        end do
        write(*,*)
    end do

    contains
    subroutine calcularNodos(A,tol)
        REAL(8),DIMENSION(0:dimension+1,0:dimension+1) :: A,B
        REAL(8) :: tol
        INTEGER :: i,j
        B = tol*A
        do while (maxval(abs(A-B)) > tol)
            B = A
            do i = 1, dimension
                do j = 1, dimension
                    A(i,j) = (A(i+1,j)+A(i-1,j)+A(i,j+1)+A(i,j-1))/4.0
                end do
            end do
        end do
    end subroutine calcularNodos

    subroutine sobreRelajacion(A, tol, omega)
        REAL(8), DIMENSION(0:dimension+1,0:dimension+1), INTENT(INOUT) :: A
        REAL(8) :: tol, omega
        REAL(8), DIMENSION(0:dimension+1,0:dimension+1) :: B
        INTEGER :: i, j
        REAL(8) :: error
    
        ! Inicializamos B con los valores de A
        B = A
    
        ! Iteramos hasta que el error máximo sea menor que la tolerancia
        do while (.true.)
            error = 0.0
    
            do i = 1, dimension
                do j = 1, dimension
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
    end subroutine sobreRelajacion
    
end program ejercicio1