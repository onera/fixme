set terminal png;
set linetype 1 lc rgb "blue" lw 2 pt 1;
set linetype 2 lc rgb "dark-orange" lw 0 pt 1;
unset autoscale;
set autoscale x;
set autoscale ymax; set yrange [0:100];
set output 'all_FRbMeanyBitIdx.png';
set style data histograms;
set style fill solid 0.5;
set key invert reverse Left outside;
set key autotitle columnheader;
set title 'Failure rate against gold according to faulty bit';
set xlabel 'Bit index';
set ylabel 'Fail rate(%)';
plot '/mnt/c/Users/nxf67054/Documents/anitiNXP/code/postprocessing/output/all_FRbMeanyBitIdx.dat' using 2:xtic(1), for [i=3:4] '' using i
