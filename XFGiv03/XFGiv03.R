# clear cache and close graphs
rm(list=ls(all=TRUE))
graphics.off()

# setwd("C:/...")

x  = read.table("XFGIVTermStructure.dat") # load data

x1 = cbind(1:8,x[,1])                    # prepare for plotting
x2 = cbind(1:8,x[,2])
x3 = cbind(1:8,x[,3])
x4 = cbind(1:8,x[,4])

plot(x1,type="l",col="blue3",ylim=c(0.20,0.5),lwd=2,xlab="Subindex",ylab="Percentage [%]")
lines(x2,col="darkgreen",lwd=2)
lines(x3,col="skyblue",lwd=2)
lines(x4,col="red3",lwd=2)
title("Term structure")
points(x1,pch=1,col="blue3",lwd=2)         # setting points
points(x2,pch=1,col="darkgreen",lwd=2)
points(x3,pch=1,col="skyblue",lwd=2)
points(x4,pch=1,col="red3",lwd=2)