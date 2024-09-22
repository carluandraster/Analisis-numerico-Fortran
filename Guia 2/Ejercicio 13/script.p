set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Solucion ED 2do orden"
set xlabel "x"
set ylabel "y"

plot "datos.dat" using 1:2 title 'x1(t)' with lines,\
    "datos.dat" using 1:3 title 'x2(t)' with lines,\
    "datos.dat" using 1:4 title 'v1(t)' with lines,\
    "datos.dat" using 1:5 title 'v2(t)' with lines