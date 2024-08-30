program ejercicio10
    implicit none
    integer n,i,j

    ! Ingresar y validar n
    print*,"Ingrese dimension de la matriz de Hilbert: "
    read*, n
    do while (n<1)
        print*,"Ingrese dimension de la matriz de Hilbert: "
        read*, n
    end do

    ! Escribir matriz de Hilbert
    open(unit=1,file="hilbert.dat", status="REPLACE")
    do i = 1, n
        do j = 1, n
            write(1,'(F5.2)',advance='NO') 1.0/(i+j-1)
        end do
        write(1,*)
    end do
    close(1)
end program ejercicio10