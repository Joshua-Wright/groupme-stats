set terminal png size 1000,1000 font "Input,20"
set output 'likesReceivedByUser.png'

set title "Likes Received Per User" font ",20"
set boxwidth 0.5
set key off
set style fill solid
set xtics rotate by -90
plot "../data/likesReceivedByUser.dat" using 1:3:xtic(2) with boxes
