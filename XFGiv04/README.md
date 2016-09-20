
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGiv04** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : XFGiv04

Published in : Applied Quantitative Finance

Description : 'XFGiv04 explains variance components of PCA for the ATM implied volatilities of the
ATM implied volatility data set (implvola.dat). The two dominant PCs together explain around 83
percent of the total variance in implied ATM volatilities for DAX options.'

Keywords : 'covariance, dimension-reduction, eigenvalues, eigenvectors, implied-volatility, option,
pca, principal-components, spectral-decomposition'

See also : XFGLSK, XFGiv00, XFGiv01, XFGiv02, XFGiv03, XFGiv05, XFGiv06

Author : Awdesch Melzer

Submitted : Wed, May 28 2014 by Awdesch Melzer

Datafiles : implvola.dat

```


### R Code:
```r
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

```
