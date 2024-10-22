set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Distribucion del calor en t=0,25"
set xlabel "x"
set ylabel "u"

plot "datos.dat" using 1:2 title 'Solucion aproximada' with lines