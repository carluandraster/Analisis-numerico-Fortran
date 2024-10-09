set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Curva isotermica"
set xlabel "x"
set ylabel "y"

plot "datos.dat" using 1:2 title '50°C' with lines