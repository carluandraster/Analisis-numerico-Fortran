program ejercicio6
    implicit none
    integer factorial,n,i
    factorial = 1
    print*, "Ingrese un numero: "
    read*, n
    do while (n<0 .OR. n>12)
        print*, "Ingrese un numero entre el 0 y el 12: "
        read*, n
    end do
    do i = 2, n
        factorial=factorial*i
    end do
    print*, n,"! = ",factorial
end program ejercicio6