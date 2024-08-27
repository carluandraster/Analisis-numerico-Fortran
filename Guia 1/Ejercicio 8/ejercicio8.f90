program ejercicio8
    implicit none
    
    ! Declaración de variables
    real MATRIZ(20,20), norma,suma
    integer i,j,N,M
    norma=0

    ! Lectura de archivo
    open(unit=2,file="matriz.txt")
    read(2,'(2I2)', ADVANCE='NO') N,M
    do i=1,N
        read(2,*)
        do j = 1, M
            read(2,'(F4.1)', ADVANCE='NO') MATRIZ(i,j)
        end do
    end do
    close(2)

    ! Calcular norma
    do i = 1, N
        suma = 0
        do j = 1, M
            suma = suma + abs(MATRIZ(i,j))
        end do
        if ( suma>norma ) then
            norma = suma
        end if
    end do
    print*, "Norma de la matriz: ",norma
end program ejercicio8