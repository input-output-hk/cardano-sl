set term png size 1000, 1000
set output "rewards.png"

min(x,y) = (x+y-abs(x-y))/2
max(x,y) = (x+y+abs(x-y))/2
m(x) = min(x,0.1)
r(a0,s,sigma) = (m(sigma) + a0 * m(s) * (m(sigma) - m(s) * (0.1 - m(sigma)) / 0.1) / 0.1) / (1 + a0)

set xlabel 'stake'
set ylabel 'reward'

set multiplot layout 3,1 rowsfirst

set title 'a0 = 0'
plot [0:0.15] r(0,0,x) title 's=0.00', [0.05:0.15] r(0,0.05,x) title 's=0.05', [0.08:0.15] r(0,0.08,x) title 's=0.08'

set title 'a0 = 0.1'
plot [0:0.15] r(0.1,0,x) title 's=0.00', [0.05:0.15] r(0.1,0.05,x) title 's=0.05', [0.08:0.15] r(0.1,0.08,x) title 's=0.08'

set title 'a0 = 0.5'
plot [0:0.15] r(0.5,0,x) title 's=0.00', [0.05:0.15] r(0.5,0.05,x) title 's=0.05', [0.08:0.15] r(0.5,0.08,x) title 's=0.08'
