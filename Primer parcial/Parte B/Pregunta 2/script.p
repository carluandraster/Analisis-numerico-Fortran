set terminal png
set output 'mi_Imagen.png'

set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Solucion"
set xlabel "Tiempo"
set ylabel "y"

plot "datos.dat" using 1:2 title 's(t)' with lines,\
    "datos.dat" using 1:3 title 's'(t)' with lines