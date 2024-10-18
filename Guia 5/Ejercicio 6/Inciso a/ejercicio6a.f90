program ejercicio6a
    implicit none
    REAL(8),PARAMETER :: pi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679
    REAL(8),dimension(0:1000) :: v
    REAL(8) :: deltaX,deltaT,xFinal,xInicial
    INTEGER :: particiones

    ! Definiciones
    deltaT = 5*10d0**(-7)
    particiones = 1000
    xInicial = 0d0
    xFinal = 1d0
    deltaX = (xFinal-xInicial)/particiones

    ! Tiempo inicial
    call condicionInicial(v,particiones,xInicial,deltaX)
    call system("gnuplot -persist scripts/tiempo0.p")

    ! Tiempo = 1s
    call resolverEDPP(particiones,deltaX,deltaT,v,1.0d0,xFinal)
    call solucionExacta(deltaX,1d0,xInicial,xFinal)
    call system("gnuplot -persist scripts/tiempo1.p")

    ! Tiempo = 2s
    call resolverEDPP(particiones,deltaX,deltaT,v,2.0d0,xFinal)
    call solucionExacta(deltaX,2d0,xInicial,xFinal)
    call system("gnuplot -persist scripts/tiempo2.p")

    ! Tiempo = 3s
    call resolverEDPP(particiones,deltaX,deltaT,v,3.0d0,xFinal)
    call solucionExacta(deltaX,3d0,xInicial,xFinal)
    call system("gnuplot -persist scripts/tiempo3.p")
    
    contains

    subroutine condicionInicial(v,particiones,xInicial,deltaX)
        INTEGER :: particiones
        REAL(8),dimension(0:particiones) :: v
        REAL(8) :: xInicial,deltaX,x
        INTEGER :: i
        x = xInicial
        open(unit=1,file="dat/datos.dat")
        do i = 0, particiones
            v(i) = sin(pi*i/particiones)
            write(1,'(2F10.6)') x,v(i)
            x = x+deltaX
        end do
        close(1)
    end subroutine condicionInicial

    subroutine resolverEDPP(particiones,deltaX,deltaT,v,tFinal,xFinal)
        REAL(8) :: deltaX,deltaT,t,vc1,vc2,tFinal,xFinal,x
        REAL(8),dimension(0:particiones) :: v
        INTEGER :: i,particiones

        open(unit=1,file="dat/datos.dat")
        
        t = tFinal-1
        do while (t <= tFinal)
            vc1 = v(0)
            vc2 = v(1)
            x = 0
            i = 1
            do while (x<=xFinal)
                v(i) = (v(i+1)+vc1)/2.0
                vc1 = vc2
                vc2 = v(i+1)
                x = x +deltaX
                i = i +1
            end do
            t = t+deltaT
        end do
        x = 0
        do i = 0, particiones
            write(1,'(2F10.6)') x,v(i)
            x = x+deltaX
        end do
        
        close(1)
    end subroutine resolverEDPP

    subroutine solucionExacta(deltaX,t,xInicial,xFinal)
        REAL(8) :: t,xInicial,xFinal,deltaX,x
        x = xInicial

        open(unit=2,file="dat/exacto.dat")
        do while (x<=xFinal)
            write(2,'(2F10.6)') x,exp(-pi**2*t)*sin(pi*x)
            x = x + deltaX
        end do
        close(2)
    end subroutine solucionExacta
end program ejercicio6a