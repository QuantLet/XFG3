
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **VaRcumulantsDG** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : VaRcumulantsDG

Published in : Applied Quantitative Finance

Description : 'Computes the first n cumulants for the class of quadratic forms of Gaussian vectors.
Notes: This function does not need the initial diagonalization. If the diagonalization has been
done already, use VaRcumulantDG, which is faster.'

Keywords : Delta-Gamma-models, cumulant, gaussian, diagonalization, function

See also : VaRcumulantDG

Author : Awdesch Melzer

Submitted : Sun, June 02 2013 by Awdesch Melzer

Usage : r = VaRcumulantsDG(n,l)

Input : '-n: scalar, highest order of cumulants to be computed -l: a list defining the distribution
contains at least the following components: - theta: the constant term - Delta: the linear term
(the first derivative) - Gamma: the quadratic term (the Hessian matrix) - Sigma: the covariance
matrix'

Output : '- rn : (rn x 1) vector of the first n cumulants'

Example : 'theta = 0 Delta = c(1) Gamma = diag(rep(1,1)) Sigma = diag(rep(1,1)) par = list()
par$theta = theta par$Delta = Delta par$Gamma = Gamma par$Sigma = Sigma VaRcumulantsDG(4,par)

Result: Contents of r [1,] 0.5 [2,] 1.5 [3,] 4 [4,] 15 '

```


### R Code:
```r
VaRcumulantsDG = function(n, l) {
    # Uses just matrix multiplication, a more sophisticated implementation would use a Hessenberg decomposition for higher n.
    r = matrix(1, n, 1)
    GS = l$Gamma %*% l$Sigma
    r[1] = l$theta + 0.5 * sum(diag(GS))
    if (n >= 2) {
        GSk = GS %*% GS
        SD = l$Sigma %*% l$Delta
        r[2] = 0.5 * (sum(diag(GSk)) + 2 * sum(SD * l$Delta))
    }
    if (n >= 3) {
        GSkm2D = l$Delta
        k = 3
        while (k <= n) {
            GSk = GSk %*% GS
            GSkm2D = GS %*% GSkm2D  # (GS)^(k-2) Delta
            r[k] = 0.5 * factorial(k - 1) * (sum(diag(GSk)) + k * sum(SD * GSkm2D))
            k = k + 1
        }
    }
    return(r)
} 

```
