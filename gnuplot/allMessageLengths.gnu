set terminal png size 1000,800 font "Input,10"
set output 'allMessageLengths.png'


set title "Message Length (Lower 90%)" font ",20"
set key left
set xtics rotate by 0

# binwidth=30
# bin(x,width)=width*floor(x/width)
# plot '../data/allMessageLengths.dat' using (bin($2,binwidth)):(1.0) smooth freq with boxes



n=10 #number of intervals
set xtics n
max=100. #max value
min=0. #min value
width=(max-min)/n #interval width
#function used to map a value to the intervals
hist(x,width)=width*floor(x/width)+width/2.0
set boxwidth width*0.9

#count and plot
plot "../data/allMessageLengths.dat" u (hist($1,width)):(1.0) smooth freq w boxes lc rgb"green" notitle