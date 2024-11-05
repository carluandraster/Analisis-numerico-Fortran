set autoscale
unset log
unset label
set xtic auto
set ytic auto
set title 'Concentracion de urea en t=  0.0 minutos'
set xlabel 'x'
set ylabel 'Concentracion'
plot 'datos.dat' using 1:2 title 'u(x,t)' with lines
