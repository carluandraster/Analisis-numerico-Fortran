set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Soluciones"
set xlabel "x"
set ylabel "y"

plot "datos1.dat" using 1:2 title 'y1 aproximado' with lines,\
    "datos1.dat" using 1:3 title 'y1 exacto' with lines,\
    "datos2.dat" using 1:2 title 'y2 aproximado' with lines,\
    "datos2.dat" using 1:3 title 'y2 exacto' with lines