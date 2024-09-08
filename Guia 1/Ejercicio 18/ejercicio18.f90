program ejercicio18
    implicit none
    REAL, PARAMETER :: PI = 3.14159265358979323846264338327950288419716939937510
    real h
    open(unit = 1, file = "archivo.dat")
    h = PI/4
    do while (h<=0.75*PI)
        write(1,'(4F5.2)') h,sin(h),cos(h),tan(h)
        h = h+PI/64
    end do
    close(1)
    call system("gnuplot -persist script.p")
    read(*,*)
end program ejercicio18