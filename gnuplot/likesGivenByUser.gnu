set terminal png size 1000,1000 font "Input,20"
set output 'likesGivenByUser.png'

set title "Likes Given Per User" font ",20"
set boxwidth 0.5
set key off
set style fill solid
set xtics rotate by -90
plot "../data/likesGivenByUser.dat" using 1:3:xtic(2) with boxes