set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Solucion EDO/PVI"
set xlabel "x"
set ylabel "y"

plot "resultados.dat" using 1:2 title 'Solucion aproximada' with lines,\
    "resultados.dat" using 1:3 title 'Solucion exacta' with lines