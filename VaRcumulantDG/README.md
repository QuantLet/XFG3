
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **VaRcumulantDG** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : VaRcumulantDG

Published in : Applied Quantitative Finance

Description : 'Computes the n-th cumulant for the class of quadratic forms of Gaussian vectors.
Notes: This function requires the eigenvalue decomposition that diagonalizes the quadratic term.
VaRcumulantsDG computes the first n cumulants without need for the initial diagonalization.'

Keywords : Delta-Gamma-models, cumulant, gaussian, eigenvalues, decomposition

See also : VaRcumulantsDG

Author : Awdesch Melzer

Submitted : Sun, June 02 2013 by Awdesch Melzer

Usage : c = VaRcumulantDG(n,l)

Input: 
- n: scalar, order of the required cumulant# n=1 is the mean
- l: a list defining the distribution contains at least the components theta, delta, lamda
- theta: the constant term
- delta: the linear term
- lambda: the diagonal of the quadratic term

Output: 
- c: scalar, the n-th cumulant

Example : 'theta = 0 delta = c(1) lambda = c(1) par = list() par$delta = delta par$lambda = lambda
par$theta = theta VaRcumulantDG(3,par) Result: Contents of c [1,] 4'

```


### R Code:
```r
VaRcumulantDG = function(n, l) {
    if (n == 1) {
        c = l$theta + 0.5 * sum(l$lambda)
        
    } else if (n == 2) {
        c = 0.5 * sum(l$lambda^2 + 2 * l$delta^2)
    } else {
        c = 0.5 * factorial(n - 1) * sum(l$lambda^n + n * l$delta^2 * l$lambda^(n - 2))
    }
    return(c)
} 

```
