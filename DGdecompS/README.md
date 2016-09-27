
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **DGdecompS** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : DGdecompS

Published in : Applied Quantitative Finance

Description : 'Computes the square root of a positive semi-definite matrix, using an eigenvalue
decomposition. Notes: Square roots of matrices can also be computed by the Cholesky decomposition,
which only works for positive definite matrices, but which is faster.'

Keywords : 'Cholesky decomposition, eigenvalue-decomposition, square root matrix, decomposition,
eigenvalues, eigenvectors'

See also : VaRDGdecomp, VaRDGdecompG

Author : Awdesch Melzer

Usage : B = DGdecompS(Sigma)

Input: 
- Sigma: positive semi-definite matrix (p x p) containing user-defined data

Output: 
- B: 'matrix (p x p) containing the square root of Sigma. Note that the solution is non-unique. B
solves the equation: transpose(BB) = Sigma.'

Example : 'Sigma = matrix(1,3,3) + diag(rep(10,3)) B = DGdecompS(Sigma) B B%*%t(B) Contents of B
[,1] [,2] [,3] [1,] 2.24135730 -1.281789 2.081666 [2,] -2.23074085 -1.300178 2.081666 [3,]
-0.01061645 2.581967 2.081666 Contents of B%*%t(B) [,1] [,2] [,3] [1,] 11 1 1 [2,] 1 11 1 [3,] 1 1
11'

```


### R Code:
```r
DGdecompS = function(Sigma) {
    
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

```
