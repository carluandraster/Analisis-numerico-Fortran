set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Solucion inciso a"
set xlabel "x"
set ylabel "y"

plot "datos.dat" using 1:2 title 'Solucion aproximada' with lines,\
    "datos.dat" using 1:3 title 'Solucion exacta' with lines