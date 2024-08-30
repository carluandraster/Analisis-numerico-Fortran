program ejercicio13
    implicit none
    
    contains
    subroutine ingresarVector (VECTOR,N)
        real(8) :: VECTOR
        integer N
        N=0
        open(unit=1,file="./txt/vector.txt")
    end subroutine
end program ejercicio13