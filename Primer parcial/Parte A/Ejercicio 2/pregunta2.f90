program pregunta2
    implicit none
    INTEGER,PARAMETER :: MAXELEM = 20
    INTEGER,DIMENSION(MAXELEM,MAXELEM) :: A
    INTEGER :: n
    INTEGER,DIMENSION(MAXELEM) :: v1,v2,vd

    print*,"Ingrese n"
    read(*,'(I2)') n

    call cargarMatriz(A,n)
    call cargarV1(A,n,v1)
    call cargarV2(A,n,v2)
    call cargarVD(A,n,vd)

    write(*,'(A)',advance='NO') "v1 = "
    call imprimirVector(v1,n)
    write(*,*)
    write(*,'(A)',advance='NO') "v2 = "
    call imprimirVector(v2,n)
    write(*,*)
    write(*,'(A)',advance='NO') "vd = "
    call imprimirVector(vd,n)
    write(*,*)

    contains

    subroutine cargarMatriz(A,  n)
        INTEGER,DIMENSION(MAXELEM,MAXELEM) :: A
        INTEGER :: n,i,j
        do i = 1, n
            do j = 1, n
                A(i,j) = (-1)**(i+j)*(i*j)
            end do
        end do
    end subroutine cargarMatriz

    subroutine cargarV1(A,  n,v1)
        INTEGER,DIMENSION(MAXELEM,MAXELEM) :: A
        INTEGER :: n,i,j
        INTEGER,DIMENSION(MAXELEM) :: v1
        do i = 1, n
            v1(i) = A(i,1)
            do j = 2, n
                if ( A(i,j)>v1(i) ) then
                    v1(i) = A(i,j)
                end if
            end do
        end do
    end subroutine cargarV1

    subroutine cargarV2(A,n,v2)
        INTEGER,DIMENSION(MAXELEM,MAXELEM) :: A
        INTEGER :: n,i,j
        INTEGER,DIMENSION(MAXELEM) :: v2
        do j = 1, n
            v2(j) = A(1,j)
            do i = 2, n
                if ( A(i,j)<v2(j) ) then
                    v2(j) = A(i,j)
                end if
            end do
        end do
    end subroutine cargarV2

    subroutine cargarVD(A,n,vd)
        INTEGER,DIMENSION(MAXELEM,MAXELEM) :: A
        INTEGER :: n,i
        INTEGER,DIMENSION(MAXELEM) :: vd
        do i = 1, n
            vd(i) = A(i,i)
        end do
    end subroutine cargarVD

    subroutine imprimirVector(v,  n)
        INTEGER,DIMENSION(MAXELEM) :: v
        INTEGER :: n,i
        do i = 1, n
            write(*,'(I2,A)',advance='no') v(i)," "
        end do
    end subroutine imprimirVector
end program pregunta2