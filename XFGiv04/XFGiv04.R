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
  
VarExpl    = l/(matrix(1,1,8)%*%l)*100  # percent of explained variance
print("Variance explained in each component:")
paste(round(VarExpl,3))
  
CumVarExpl = cumsum(l/(matrix(1,1,8)%*%l)*100) # cumulated variance explained
print("Cumulated Variance:")
paste(round(CumVarExpl,3))
