set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Solucion ED 2do orden"
set xlabel "x"
set ylabel "y"

plot "datos.dat" using 1:2 title 'Q(t)' with lines