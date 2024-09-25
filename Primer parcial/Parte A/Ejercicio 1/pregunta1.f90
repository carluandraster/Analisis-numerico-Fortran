program pregunta1
    implicit none
    REAL,DIMENSION(4,4) :: A,B

    call leerArchivo(A)
    B = transpose(A)
    call imprimirMatriz(B)
    
    contains

    subroutine leerArchivo(A)
        REAL,DIMENSION(4,4) :: A
        INTEGER :: i,j
        open(unit=1,file="datos.txt")
        do i = 1, 4
            read(1,'(4F7.2)') (A(i,j),j=1,4)
        end do
        close(1)
    end subroutine leerArchivo

    subroutine imprimirMatriz(B)
        REAL,DIMENSION(4,4) :: B
        INTEGER :: i,j
        open(unit=2,file="datostraspuestos.txt")
        do i = 1, 4
            do j = 1, 4
                write(2,'(F7.2)',advance='NO') B(i,j)
            end do
            write(2,*)
        end do
        close(2)
    end subroutine imprimirMatriz
end program pregunta1