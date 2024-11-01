program ejercicio9
    implicit none
    REAL(8),PARAMETER :: D = 0.5, dx = 0.1, dt = 0.1, r = D*dt/dx**2,xInicial = 0, xFinal = 20, tInicial = 0, tFinal = 1000
    REAL(8),DIMENSION(:),ALLOCATABLE :: u
    INTEGER,PARAMETER :: N = INT((xFinal-xInicial)/dx)


    REAL(8) :: t

    ALLOCATE(u(0:N))
    t = 0 
    ! Condiciones iniciales
    u(:) = 0.02
    u(0) = 2
    u(N) = 0
    do while (t<=tFinal+dt)

        if ( mod(t,10d0) < dt ) then
            call cargarArchivo(u,N)
            call escribirScript(t)
            call system("gnuplot -persist script.p")
        end if
        t = t+dt
        !write(*,'(f7.2)') u
        !read(*,*)
        !write(*,*) '**********************************************************'

        call Crank_Nicolson(t-dt,t,u)
    end do
    DEALLOCATE(u)

    contains
    
    subroutine escribirScript(tiempo)
        REAL(8),INTENT(IN) :: tiempo
        open(unit = 1, file = "script.p")

        write(1,'(A)') "set autoscale"
        write(1,'(A)') "unset log"
        write(1,'(A)') "unset label"
        write(1,'(A)') "set xtic auto"
        write(1,'(A)') "set ytic auto"

        write(1,'(A,F5.1,A)') "set title 'Concentracion de urea en t=",tiempo," minutos'"
        write(1,'(A)') "set xlabel 'x'"
        write(1,'(A)') "set ylabel 'Concentracion'"
        write(1,'(A)') "plot 'datos.dat' using 1:2 title 'u(x,t)' with lines"
        close(1)
    end subroutine escribirScript

    SUBROUTINE Thomas(u_orig, d_orig, l_orig, b)
        ! Metodo de Thomas para matrices Tri-Diagonales
        REAL(8), INTENT(IN) :: u_orig, d_orig, l_orig
        REAL(8), DIMENSION(:) :: b
        REAL(8), DIMENSION(:), ALLOCATABLE :: u, d, l
        INTEGER orden, i
        
        orden = SIZE(b, DIM=1)
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
            b(i) = b(i) / d(i)
            d(i) = 1.0
            d(i+1) = d(i+1) - l(i+1)*u(i)
            b(i+1) = b(i+1) - l(i+1)*b(i)
            l(i+1) = 0.0
        ENDDO
        ! Obtencion de la solucion por Sustitucion Inversa
        b(orden) = b(orden)/d(orden)
        DO i=orden-1, 1, -1
            b(i) = b(i) - u(i)*b(i+1) / d(i)
        END DO
   
        DEALLOCATE(u, d, l)
    END SUBROUTINE

    subroutine Crank_Nicolson(t0,tF,u)
        REAL(8),INTENT(IN) :: t0,tF
        REAL(8),DIMENSION(0:N) :: u
        REAL(8),DIMENSION(1:N-1) :: termIndep
        REAL(8) :: t
        INTEGER :: i
        t = t0
        do while (t<tF)
   
            do i = 1, N-1
                termIndep(i) = r*u(i-1)+(2-2*r)*u(i)+r*u(i+1)
            end do
            call Thomas(-r,2+2*r,-r,termIndep)
            t = t + dt
            !write(*,*)termIndep
            !read(*,*)
            u(1:N-1) = termIndep
        end do   
    end subroutine Crank_Nicolson

    subroutine cargarArchivo(v,N)
        INTEGER :: i,N
        REAL(8),DIMENSION(0:N) :: v
        REAL(8) :: x
    
        open(unit = 1, file = "datos.dat")
        x = xInicial
        i = 0
        do while (x <= xFinal)
            write(1,'(2F15.6)') x,v(i)
            x = x + dx
            i = i+1
        end do
        close(1)
    end subroutine cargarArchivo
end program ejercicio9