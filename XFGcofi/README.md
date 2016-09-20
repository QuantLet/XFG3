
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGcofi** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : XFGcofi

Published in : Applied Quantitative Finance

Description : Computes Cornish-Fisher approximations for increasing orders for a specific example

Keywords : 'Cornish-Fisher expansion, normal approximation, asymptotic, quantile, distribution,
cumulant'

Author : Awdesch Melzer

Submitted : Wed, June 05 2013 by Awdesch Melzer

Example : 'z = 0.99 N = 8 cum = 1:(N+2) i = 2 r = matrix(N-1,2) while (i <= N){ r[i-1,1] = i
r[i-1,2] = cf_estimate_quantiles(i,z,cum) i = i+1 } r. Result: [,1] [,2] [1,] 2 4.489522 [2,] 3
5.392926 [3,] 4 5.124802 [4,] 5 5.283749 [5,] 6 5.185927 [6,] 7 5.207992 [7,] 8 5.315067.'

```


### R Code:
```r
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

```
