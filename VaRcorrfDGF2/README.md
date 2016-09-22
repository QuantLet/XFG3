
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **VaRcorrDGF2** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : VaRcorrDGF2

Published in : Applied Quantitative Finance

Description : 'Computes the cumulative distribution function (CDF) of an approximated normal
distribution for the class of quadratic forms of Gaussian vectors.'

Keywords : Delta-Gamma-models, normal approximation, approximation, gaussian, cdf, normal

See also : VaRcdfDG, VaRcharfDGF2, VaRqDG, XFGqDGtest

Author : Awdesch Melzer

Submitted : Tue, June 04 2013 by Awdesch Melzer

Usage : r = VaRcorrfDGF2(x,l)

Input: 
- x: the argument to the CDF
- l: a list defining the distribution, contains at least theta, delta, lambda
- theta: the constant
- delta: the linear term
- lambda: the diagonal of the quadratic term

Output: 
- r: the value of the approximated normal CDF at x

```


### R Code:
```r
NA
```
