# clear cache and close windows
rm(list=ls(all=TRUE))
graphics.off()
 
x          = read.table("implvola.dat") # load data
x          = x*100                      # scale
n          = nrow(x)                    # number of rows
z          = x[2:n,] - x[1:(n-1),]      # first difference
s          = cov(z)*100000              # covariance of returns
tmp        = eigen(s)                   # spectral decomposition
l          = tmp$values                 # eigenvalues
g          = tmp$vectors                # eigenvectors
g[,2]      = g[,2]*(-1)                 # correction of sign for publication purpose
  
gr1        = cbind(1:8,g[,1])           # first principal component
gr2        = cbind(1:8,g[,2])           # second principal component

plot(gr1,type="l",col="blue3",xlab="Subindex",ylab="Percentage [%]",lwd=2,ylim=c(min(g[,1:2]),max(g[,1:2])))
points(gr1,col="blue3",lwd=2,pch=1)
title("Factor Loadings")
lines(gr2,col="darkgreen",lwd=2)
points(gr2,col="darkgreen",lwd=2)

