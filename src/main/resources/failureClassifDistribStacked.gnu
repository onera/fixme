set terminal png
set output 'output/testplot.png'
set title "Distribution of misclassification according to input label"
set key invert reverse Left outside
set key autotitle columnheader
set yrange [0:100]
set auto x
set xlabel "Class label of input"
set ylabel "% of total misclassifications"
unset xtics
set xtics nomirror rotate by -45 scale 0
set style data histogram
set style histogram rowstacked
set style fill solid border -1
set boxwidth 0.75
#
plot 'output/globalFailClassDistrib.dat' using 2:xtic(1), for [i=3:11] '' using i
#
