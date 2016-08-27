set terminal png size 1000,1000 font "Input,10"
set output 'allLikesGivenByUserToUser.png'


set title "Who Likes Who" font ",20"
set key off
set xtics 2
set xtics rotate by -90

set palette defined (0 "blue",17 "#00ffff",33 "white",50 "yellow",\
    66 "red",100 "#990000",101 "grey")

plot "../data/allLikesGivenByUserToUser.dat" matrix rowheaders columnheaders using 1:2:(log($3+10)) with image, \
     "../data/allLikesGivenByUserToUser.dat" matrix rowheaders columnheaders using 1:2:($3 == 0 ? "" : sprintf("%g",$3) ) with labels
