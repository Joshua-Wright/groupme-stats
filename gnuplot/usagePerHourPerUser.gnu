set terminal png size 1000,800 font "Input,10"
set output 'usagePerTimePerUser.png'


set title "Usage Per Hour Per User" font ",20"
set key left
set xtics 2
set xtics rotate by 0

# more colors because we have more than the default palate
set linetype 1 lc rgb '#1B9E77' # dark teal
set linetype 2 lc rgb '#D95F02' # dark orange
set linetype 3 lc rgb '#7570B3' # dark lilac
set linetype 4 lc rgb '#E7298A' # dark magenta
set linetype 5 lc rgb '#66A61E' # dark lime green
set linetype 6 lc rgb '#E6AB02' # dark banana
set linetype 7 lc rgb '#A6761D' # dark tan
set linetype 8 lc rgb '#666666' # dark gray
set linetype 9 lc rgb '#7FC97F' # pale green
set linetype 10 lc rgb '#BEAED4' # pale purple
set linetype 11 lc rgb '#FDC086' # pale orange
set linetype 12 lc rgb '#FFFF99' # pale yellow
set linetype 13 lc rgb '#386CB0' # blue
set linetype 14 lc rgb '#F0027F' # magenta
set linetype 14 lc rgb '#BF5B17' # brown
set linetype 15 lc rgb '#666666' # grey

# plot [0:24] "../data/usagePerTimePerUser.dat" with lines
plot for [i=1:10] [0:23] "../data/usagePerTimePerUser.dat" u 0:i with lines lw 2 title columnheader
