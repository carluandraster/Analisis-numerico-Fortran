set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Inciso c"
set xlabel "x"
set ylabel "y"

plot "datos.dat" using 1:2 title 'y1(t)' with lines,\
    "datos.dat" using 1:3 title 'y2(t)' with lines,\
    "datos.dat" using 1:4 title 'y3(t)' with lines,\
    "datos.dat" using 1:5 title 'v1(t)' with lines,\
    "datos.dat" using 1:6 title 'v2(t)' with lines,\
    "datos.dat" using 1:7 title 'v3(t)' with lines