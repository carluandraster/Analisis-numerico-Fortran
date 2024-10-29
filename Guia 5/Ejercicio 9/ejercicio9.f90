program ejercicio9
    implicit none
    REAL(8),PARAMETER :: D = 0.5, dx = 0.02, dt = 0.001, r = D*dt/dx**2,xInicial = 0, xFinal = 20, tInicial = 0, tFinal = 100
    REAL(8),DIMENSION(:),ALLOCATABLE :: u
    INTEGER :: i
    INTEGER,PARAMETER :: N = INT((xFinal-xInicial)/dx)

    ALLOCATE(u(N))
    do i = 0, INT((tFinal-tInicial)/dt)
        if ( i == 0 ) then
            call Crank_Nicolson(tInicial,dt*i,u)
        else
            call Crank_Nicolson((i-1)*dt,i*dt,u)
        end if
        if ( mod(i,INT(10/dt)) == 0 ) then
            call cargarArchivo(u,N)
            call escribirScript(i*dt)
            call system("gnuplot -persist script.p")
        end if
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

    subroutine Crank_Nicolson(tInicial,tFinal,u)
        REAL(8),INTENT(IN) :: tInicial,tFinal
        REAL(8),DIMENSION(0:N) :: u
        REAL(8),DIMENSION(1:N-1) :: termIndep
        REAL(8) :: x,t
        INTEGER :: i
        
        if ( tFinal == 0 ) then
            x = xInicial
            i = 0
            do while (x<xFinal)
                if ( i == 0 ) then
                    u(i) = 10 ! Condicion de borde
                else
                    if ( i == N ) then
                        u(i) = 0 !Condicion de borde
                    else
                        u(i) = 0.02 ! Condiciones iniciales
                    end if
                end if
                
                x = x + dx
                i = i + 1
            end do
        else
            t = tInicial
            do while (t<tFinal)
                do i = 1, N-1
                    termIndep(i) = r*u(i-1)+(2-2*r)*u(i)+r*u(i+1)
                end do
                call Thomas(-r,2+2*r,-r,termIndep)
                t = t + dt
                u(1:N-1) = termIndep
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
        do while (x <= xFinal)
            write(1,'(2F15.6)') x,v(i)
            x = x + dx
            i = i+1
        end do
        close(1)
    end subroutine cargarArchivo
end program ejercicio9