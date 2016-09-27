
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **VaRDGdecompG** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : VaRDGdecompG

Published in : Applied Quantitative Finance

Description : Computes the first and second derivatives with respect to the new risk factors.

Keywords : diagonalization, eigenvalue-decomposition, eigenvalues, risk, derivative

See also : DGdecompS, VaRDGdecomp

Author : Awdesch Melzer

Submitted : Sun, June 02 2013 by Awdesch Melzer

Usage : r = VaRDGdecompG(l)

Input: 
- l: a list with (at least) the components Delta, Gamma, B
- Delta: (m x 1) ''old'' first derivatives
- Gamma: (m x m) ''old'' second derivatives
- B: (m x m) square root of the covariance matrix, BB'' = Sigma

Output: 
- r: a list containing the additional components delta and lambda
- delta: (m x 1) new first derivatives
- lambda: (m x m) diagonal of the new second derivatives

Example : 'Delta = c(1,2,3) Gamma = matrix(1,3,3) + diag(rep(9,3)) B = diag(rep(1,3)) l = list()
l$Delta = Delta l$Gamma = Gamma l$B = B VaRDGdecompG(l)'

```


### R Code:
```r
VaRDGdecompG = function(l) {
    e = eigen(t(l$B) %*% l$Gamma %*% l$B)
    r = l
    lambda = e$values
    if (length(r$lambda) == 0) {
        r$lambda = lambda
    } else {
        r$lambda = lambda
    }
    delta = t((t(l$Delta) %*% l$B) %*% e$vectors)
    if (length(r$delta) == 0) {
        r$delta = delta
    } else {
        r$delta
    }
    return(r)
} 

```
