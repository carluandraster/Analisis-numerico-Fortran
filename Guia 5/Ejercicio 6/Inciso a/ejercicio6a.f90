program ejercicio6a
    implicit none
    REAL(8),PARAMETER :: pi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679
    
    contains

    subroutine resolverEDPP(deltaX,deltaT,v)
        REAL(8) :: deltaX,deltaT
        REAL(8),dimension(0:2) :: v
        REAL(8),dimension(0:2,1:1000) :: MATRIZ,ANTERIOR
        INTEGER :: i

        open(unit=1,file="dat/datos.dat")
        do while (v(1)<10)
            i = 1
            do while (v(0)<=1)
                if ( v(0) == 0 .or. v(0) == 1 ) then
                    v(2) = 0
                else if ( v(1)==0 ) then
                    v(2) = sin(pi*v(0))
                else
                    v(2) = (ANTERIOR(2,i+1)+ANTERIOR(2,i-1))/2.0
                end if
                MATRIZ(:,i) = v
                i = i+1
                v(0) = v(0)+deltaX
                write(1,'(3F10.6)') v
            end do
            write(1,*)
            ANTERIOR = MATRIZ
            v(1) = v(1)+deltaT
        end do
        close(1)
    end subroutine resolverEDPP

    subroutine solucionExacta(deltaX,deltaT,v)
        REAL(8) :: deltaT,deltaX
        REAL(8),DIMENSION(0:2) :: v

        open(unit=2,file="dat/exacto.dat")
        do while (v(1) < 10)
            do while (v(0) <= 1)
                write(2,'(3F10.6)') v
                v(2) = exp(-pi**2*v(1))*sin(pi*v(0))
                v(0) = v(0) + deltaX
            end do
            v(1) = v(1) + deltaT
            write(2,*)
        end do
        close(2)
    end subroutine solucionExacta
end program ejercicio6a