program ejercicio5
    implicit none
    integer n,fibo1,fibo2,i
    fibo1=0
    fibo2=1
    print*, "Ingrese cantidad de terminos: "
    read*, n
    do i = 1, n
        if ( MOD(i,2) == 0 ) then
            print*, fibo2
            fibo2 = fibo2+fibo1
        else
            print*, fibo1
            fibo1=fibo1+fibo2
        end if
    end do
end program ejercicio5