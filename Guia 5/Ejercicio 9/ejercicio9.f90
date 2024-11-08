program ejercicio9
    implicit none
    REAL(8),PARAMETER :: D = 0.017, dx = 0.1, dt = 0.1, r = D*dt/dx**2,xInicial = 0, xFinal = 20, tInicial = 0, tFinal = 10
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
        if ( mod(t,1d0) < dt ) then
            call cargarArchivo(u)
            call escribirScript(t)
            call system("gnuplot -persist script.p")
        end if
        t = t+dt
        call Crank_Nicolson(u)
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

    subroutine solve_tridiag(a,b,c,d,x,n)
        implicit none
        !	a - sub-diagonal (la que esta por debajo de la diagonal principal)
        !	b - la diagonal principal
        !	c - sup-diagonal (la que esta por encima de la diagonal principal)
        !	d - termino independiente
        !	x - la solucion
        !	n - cantidad de ecuaciones
  
          integer,intent(in) :: n
          real(8),dimension(n),intent(in) :: a,b,c,d
          real(8),dimension(n),intent(out) :: x
          real(8),dimension(n) :: cp,dp
          real(8) :: m
          integer i
  
        ! Inicializar c' and d'
          cp(1) = c(1)/b(1)
          dp(1) = d(1)/b(1)
        ! solve for vectors c-prime and d-prime
           do i = 2,n
            m = b(i)-cp(i-1)*a(i)
             cp(i) = c(i)/m
             dp(i) = (d(i)-dp(i-1)*a(i))/m
           enddo
        ! initialize x
           x(n) = dp(n)
        ! solve for x from the vectors c-prime and d-prime
          do i = n-1, 1, -1
            x(i) = dp(i)-cp(i)*x(i+1)
          end do
  
      end subroutine solve_tridiag

    subroutine Crank_Nicolson(u)
        REAL(8),DIMENSION(0:N) :: u
        REAL(8),DIMENSION(1:N-1) :: a,b,c,termIndep,x
        INTEGER :: i
        termIndep(1) = 2*r*u(0)+(2-2*r)*u(1)+r*u(2)
        do i = 2, N-2
            termIndep(i) = r*u(i-1)+(2-2*r)*u(i)+r*u(i+1)
        end do
        termIndep(N-1) = r*u(N-2)+(2-2*r)*u(N-1)+2*r*u(N)
        a(1) = 0
        a(2:N-1) = -r
        b(:) = 2+2*r
        c(1:n-2) = -r
        c(n-1) = 0
        call solve_tridiag(a,b,c,termIndep,x,n-1)
        do i = 1, N-1
            u(i) = x(i)
        end do
    end subroutine Crank_Nicolson

    subroutine cargarArchivo(v)
        INTEGER :: i
        REAL(8),DIMENSION(0:N) :: v
        REAL(8) :: x
    
        open(unit = 1, file = "datos.dat")
        x = xInicial
        i = 0
        do while (x <= xFinal+dx)
            write(1,'(2F15.6)') x,v(i)
            x = x + dx
            i = i+1
        end do
        close(1)
    end subroutine cargarArchivo
end program ejercicio9