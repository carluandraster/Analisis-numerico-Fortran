set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Poblacion de especies"
set xlabel "Tiempo"
set ylabel "Problacion"

plot "datos.dat" using 1:2 title 'Presas' with lines,\
    "datos.dat" using 1:3 title 'Predadores' with lines