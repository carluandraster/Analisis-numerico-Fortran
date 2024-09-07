program ejercicio16
    implicit none
    integer n,fibo1,fibo2,i
    open(unit=1,file="fibonacci.dat")
    fibo1=0
    fibo2=1
    print*, "Ingrese cantidad de terminos: "
    read*, n
    do i = 1, n
        if ( MOD(i,2) == 0 ) then
            write(1, '(2I5)') i,fibo2
            fibo2 = fibo2+fibo1
        else
            write(1, '(2I5)') i,fibo1
            fibo1=fibo1+fibo2
        end if
    end do
    close(1)
    call system("gnuplot -persist script.p")
    read(*,*)
end program ejercicio16