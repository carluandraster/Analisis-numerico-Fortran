program ejercicio9
    implicit none

    integer nroOrden
    real x

    open(unit=1,file="valores.dat",status="REPLACE")

    ! Numero de orden
    print*, "Ingrese un numero de orden: "
    read*,nroOrden
    do while (nroOrden<1 .OR. nroOrden>999)
        print*, "Ingrese un numero positivo de a lo sumo 3 digitos: "
        read*,nroOrden
    end do
    write(1,'(I3)') nroOrden

    ! Numero x
    print*, "Ingrese un numero: "
    read*,x
    write(1,'(F8.3)') x

    ! Escribir seno, coseno y tangente
    write(1,'(F10.5)') sin(x)
    write(1,'(F10.5)') cos(x)
    write(1,'(F10.5)') tan(x)

    close(1)
end program ejercicio9