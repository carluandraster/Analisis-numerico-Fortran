program ejercicio6b
    implicit none
    REAL(8),PARAMETER :: pi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679
    REAL(8),PARAMETER :: deltaT = 5*10d0**(-7),xInicial = 0d0,xFinal = 1d0,deltaX = 0.001d0
    REAL(8),ALLOCATABLE,DIMENSION(:) :: v
    INTEGER :: N

    N = INT((xFinal-xInicial)/deltaX)
    ALLOCATE(v(N))

    ! t = 0
    call MetodoExplicito(0d0,0d0,v,N)
    call cargarArchivo(v,N)
    call system("gnuplot -persist scripts/script0.p")

    ! t = 1
    call MetodoExplicito(0d0,1d0,v,N)
    call cargarArchivo(v,N)
    call system("gnuplot -persist scripts/script1.p")

    ! t = 2
    call MetodoExplicito(1d0,2d0,v,N)
    call cargarArchivo(v,N)
    call system("gnuplot -persist scripts/script2.p")

    DEALLOCATE(v)

    contains

    subroutine promedio(vi,vf,N)
        INTEGER,intent(IN) :: N
        REAL(8),DIMENSION(0:N), intent(in) :: vi
        REAL(8),DIMENSION(0:N), intent(out) ::  vf
        INTEGER :: i
        
        vf(0) = vi(0)
        do i = 1, N-1
            vf(i)=(vi(i-1)+vi(i+1))/2.0
        end do
        vf(N) = vi(N)
    end subroutine promedio

    subroutine MetodoExplicito(tInicial,tFinal,v,N)
        REAL(8) :: tFinal,tInicial,t
        INTEGER :: i,N
        REAL(8),DIMENSION(0:N) :: v,vc
        
        if ( tFinal == 0 ) then
            do i = 0, N
                v(i) = cos(pi*(REAL(i)/N-0.5)) ! Condiciones iniciales
            end do
        else
            t = tInicial
            do while (t<= tFinal)
                vc = v
                call promedio(vc,v,N)
                t =  t+deltaT
            end do
        end if
    end subroutine MetodoExplicito

    subroutine cargarArchivo(v,N)
        INTEGER :: i,N
        REAL(8),DIMENSION(0:N) :: v
        REAL(8) :: x
    
        open(unit = 1, file = "dat/datos.dat")
        x = xInicial
        i = 0
        do while (x < xFinal)
            write(1,'(2F10.6)') x,v(i)
            x = x + deltaX
            i = i+1
        end do
        close(1)
    end subroutine cargarArchivo
end program ejercicio6b