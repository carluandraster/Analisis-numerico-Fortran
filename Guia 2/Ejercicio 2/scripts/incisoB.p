set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Solucion inciso b"
set xlabel "x"
set ylabel "y"

plot "./dat/incisoB1.dat" using 1:2 title 'Solucion aproximada con h=0.01' with lines,\
    "./dat/incisoB1.dat" using 1:3 title 'Solucion exacta' with lines,\
    "./dat/incisoB2.dat" using 1:2 title 'Solucion aproximada con h=0.001' with lines