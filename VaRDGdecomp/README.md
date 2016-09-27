
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **VaRDGdecomp** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : VaRDGdecomp

Published in : Applied Quantitative Finance

Description : 'VaRDGdecomp uses a generalized eigenvalue decomposition to do a suitable coordinate
change. The new risk factors are independently standard normal distributed and the new Hessian
matrix (Gamma) is diagonal.'

Keywords : diagonalization, eigenvalue-decomposition, eigenvalues, risk, standard-normal

See also : DGdecompS, VaR, VaR, VaRDGdecompG

Author : Awdesch Melzer

Submitted : Sun, June 02 2013 by Awdesch Melzer

Usage : r = VaRDGdecomp(l)

Input: 
- l: a list with components Delta, Gamma, Sigma
- Delta: (m x 1) vector of first derivatives
- Gamma: (m x m) Hessian matrix
- Sigma: (m x m) covariance matrix

Output: 
- r: a list with the additional components B, delta, lambda
- B: (m x m) transpose(BB) = Sigma
- delta: (m x 1) first derivatives w.r.t. new coordinates
- lambda: (m x 1) diagonal of the Hessian matrix w.r.t. new coordinates

Example : 'Delta = c(1,2,3) Gamma = matrix(1,3,3) + diag(rep(9,3)) B = diag(rep(1,3)) l = list()
l$Delta = Delta l$Gamma = Gamma l$B = B VaRDGdecompG(l)

Result: $Delta [1] 1 2 3 $Gamma [,1] [,2] [,3] [1,] 10 1 1 [2,] 1 10 1 [3,] 1 1 10 $B [,1] [,2]
[,3] [1,] 1 0 0 [2,] 0 1 0 [3,] 0 0 1 $lambda [1] 12 9 9 $delta [,1] [1,] -3.4641016 [2,] 1.1716499
[3,] -0.7919827 '

```


### R Code:
```r
DGdecompS = function(Sigma) {
    # DGdecompS computes the square root of a positive semi-definite matrix, using an eigen value decomposition
    e = eigen(Sigma)
    e$values = 0.5 * (abs(e$values) + e$values)  # cancel negative eigenvalues
    B = e$vectors %*% diag(sqrt(e$values))
    if (any(B[, 1] < 0)) {
        B[, 1] = B[, 1] * sign(B[, 1])
    }
    n = nrow(B)
    BB = B[1:n, n:1]
    return(BB)
    
}

VaRDGdecompG = function(l) {
    # VaRDGdecompG computes the first and second derivatives with respect to the new risk factors.
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

######################### MAIN PROGRAM ############################


VaRDGdecomp = function(l) {
    B = DGdecompS(l$Sigma)
    l$B = B
    r = VaRDGdecompG(l)
    return(r)
}
 

```
