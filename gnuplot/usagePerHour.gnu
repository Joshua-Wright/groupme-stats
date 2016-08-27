set terminal png size 500,500 font "Input,10"
set output 'usagePerHour.png'


set title "Usage per hour" font ",20"
set key off
set xtics 2

plot [0:24] "../data/usagePerHour.dat" with lines
