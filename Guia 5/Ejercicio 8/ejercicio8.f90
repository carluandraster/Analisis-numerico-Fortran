program ejercicio8
    implicit none
    REAL(8),PARAMETER :: k = 385, c = 385, p = 8960, dx = 1,xInicial = 0,xFinal = 10, dt = 5,r = k*dt/(c*p*(dx*0.0254)**2)
    REAL(8),DIMENSION(:),ALLOCATABLE :: u
    INTEGER,PARAMETER :: N = INT((xFinal-xInicial)/dx)
    INTEGER :: i

    ALLOCATE(u(N))
    do i = 0, 10
        if ( i == 0 ) then
            call Crank_Nicolson(0d0,0d0,u)
        else
            call Crank_Nicolson((i-1)*5d0,i*5d0,u)
        end if
        call cargarArchivo(u,N)
        call escribirScript(i*5d0)
        call system("gnuplot -persist script.p")
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

        write(1,'(A,F5.1,A)') "set title 'Distribucion del calor en t=",tiempo,"'"
        write(1,'(A)') "set xlabel 'x'"
        write(1,'(A)') "set ylabel 'T'"
        write(1,'(A)') "plot 'datos.dat' using 1:2 title 'Solucion aproximada' with lines"
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
                u(i) = 10*x
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
end program ejercicio8