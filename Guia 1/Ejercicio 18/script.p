set xtic auto
set ytic auto
set title "Ejercicio 18"
set xlabel "x"
set ylabel "y"
plot "archivo.dat" using 1:2 title 'Seno' with points,\
    "archivo.dat" using 1:3 title 'Coseno' with lines,\
    "archivo.dat" using 1:4 title 'Tangente' with lines,\
    "archivo.dat" using 1:4 title 'Tangente' with points