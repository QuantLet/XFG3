rm(list=ls(all=TRUE))


install.packages("CfEstimateQuantiles")
library(CfEstimateQuantiles)


z   = 0.99     # standard normal quantile
N   = 8        # order of expansion; uses N+2 cumulants
cum = 1:(N+2)  # (N x 1) vector of cumulants; cum[1]=mean, cum[2]=variance, etc.
i   = 2
r   = matrix(1,N-1,2)
  
while (i <= N){
    r[i-1,1] = i
    r[i-1,2] = cf_estimate_quantiles(i,z,cum)
    i        = i+1
}
  
r
