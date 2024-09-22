set autoscale
unset log
unset label
set xtic auto
set ytic auto

set title "Corriente en funcion del tiempo"
set xlabel "x"
set ylabel "y"

plot "datos1.dat" using 1:3 title 'R=0' with lines,\
    "datos2.dat" using 1:3 title 'R=50 ohms' with lines,\
    "datos3.dat" using 1:3 title 'R=100 ohms' with lines