set autoscale
unset log
unset label
set xtic auto
set ytic auto
set title 'Distribucion del calor en t= 50.0'
set xlabel 'x'
set ylabel 'T'
plot 'datos.dat' using 1:2 title 'Solucion aproximada' with lines
