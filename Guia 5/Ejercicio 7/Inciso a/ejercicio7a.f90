program ejercicio7a
    implicit none
    REAL(8),PARAMETER :: pi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679
    REAL(8),PARAMETER :: xInicial = 0,xFinal = 1, dx = 0.001, dt = 0.000001
    REAL(8),DIMENSION(:,:),ALLOCATABLE :: u
    INTEGER :: N

    N = INT((xFinal-xInicial)/dx)
    ALLOCATE(u(N,1))

    ! t = 0
    call Crank_Nicolson(0d0,0d0,u)
    call cargarArchivo(u,N)
    call system("gnuplot -persist scripts/script0.p")

    ! t = 0.25
    call Crank_Nicolson(0d0,0.25d0,u)
    call cargarArchivo(u,N)
    call system("gnuplot -persist scripts/script1.p")

    ! t = 0.5
    call Crank_Nicolson(0.25d0,0.5d0,u)
    call cargarArchivo(u,N)
    call system("gnuplot -persist scripts/script2.p")
    
    DEALLOCATE(u)

    contains

    SUBROUTINE Thomas(u_orig, d_orig, l_orig, b)
        ! Metodo de Thomas para matrices Tri-Diagonales
        REAL(8), INTENT(IN) :: u_orig, d_orig, l_orig
        REAL(8), DIMENSION(:,:) :: b
        REAL(8), DIMENSION(:), ALLOCATABLE :: u, d, l
        INTEGER orden, cant_vec, i
        
        orden = SIZE(b, DIM=1)
        cant_vec = SIZE(b, DIM=2)
        ! Realiza copias del original
        ALLOCATE(u(orden), d(orden), l(orden))

        u(:) = u_orig
        u(orden) = 0
        d(:) = d_orig
        l(:) = l_orig
        l(1) = 0

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
   
        DEALLOCATE(u, d, l)
    END SUBROUTINE

    subroutine Crank_Nicolson(tInicial,tFinal,u)
        REAL(8),INTENT(IN) :: tInicial,tFinal
        REAL(8),DIMENSION(:,:) :: u
        REAL(8) :: x,t
        INTEGER :: i
        
        if ( tFinal == 0 ) then
            x = xInicial
            i = 0
            do while (x<xFinal)
                u(i,1) = sin(PI*x)
                x = x + dx
                i = i + 1
            end do
        else
            t = tInicial
            do while (t<tFinal)
                call Thomas(-1d0,4d0,-1d0,u)
                t = t + dt
                
            end do
        end if
        
    end subroutine Crank_Nicolson

    subroutine cargarArchivo(v,N)
        INTEGER :: i,N
        REAL(8),DIMENSION(0:N) :: v
        REAL(8) :: x
    
        open(unit = 1, file = "datos.dat")
        x = xInicial
        i = 0
        do while (x < xFinal)
            write(1,'(2F10.6)') x,v(i)
            x = x + dx
            i = i+1
        end do
        close(1)
    end subroutine cargarArchivo
end program ejercicio7a