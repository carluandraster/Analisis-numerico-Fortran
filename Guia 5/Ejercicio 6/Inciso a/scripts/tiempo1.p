set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Distribucion del calor en t=1"
set xlabel "x"
set ylabel "u"

plot "dat/datos.dat" using 1:2 title 'Solucion aproximada' with lines,\
    "dat/exacto.dat" using 1:2 title 'Solucion exacta' with lines