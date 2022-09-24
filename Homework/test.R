plot.new()

plot.window(xlim=c(0,2), ylim=c(0,10))

axis(1)

axis(2)

x = c(0.0, 0.5,0.5, 1.5, 2.0)

y =  c(0.0, 0, 6, 8.2, 10. )

lines(x,y, lwd=2, col="red")
x = c(1, 0.5,0.5, 1.5, 2.0)

y =  c(0.0, 0, 6, 8.2, 10. )
lines(x,y, lwd=2, col="blue")

title(xlab = "X-axis value")

title(ylab = "Y-axis value")

box()
