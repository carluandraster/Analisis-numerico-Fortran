set autoscale
unset log
unset label
set xtic auto
set ytic auto
set title "Sucesion de Fibonacci"
set xlabel "x"
set ylabel "y"
plot "fibonacci.dat" using 1:2 title 'Fibonacci' with lines