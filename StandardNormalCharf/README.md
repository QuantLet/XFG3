
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **StandardNormalCharf** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : StandardNormalCharf

Published in : Applied Quantitative Finance

Description : 'Computes the characteristic function of a one-dimensional normally distributed
random variable.'

Keywords : characteristic function, normal, normal-distribution, characteristic, standard-normal

See also : XFGqDGtest, gFourierInversion

Author : Awdesch Melzer

Submitted : Tue, June 04 2013 by Awdesch Melzer

Input: 
- t: 'scalar, complex number indicating the point at which the characteristic function should be
calculated. l: list, containing l$mu (the expectation) and l$sigma (the standard deviation) of the
Standard Normal Distribution.'

Output: 
- r: scalar, complex number representing the value of the characteristic function at t''

Example : 'Please mind the necessary subroutines. mu = 0 sigma = 1 l = list(mu=mu,sigma=sigma) t =
compl(1,1) r= StandardNormalCharf(t,l). Result: Contents of r$re [1,] 0.5403. Contents of r$im [1,]
-0.84147.'

```


### R Code:
```r
compl = function(re, im) {
    # Complex array generation
    if (missing(re)) {
        stop("compl: no composed object for real part")
    }
    if (missing(im)) {
        im = 0 * (re <= Inf)
    }
    if (nrow(matrix(re)) != nrow(matrix(im))) {
        stop("compl: dim(re)<>dim(im)")
    }
    z = list()
    z$re = re
    z$im = im
    return(z)
}

cmul = function(x, y) {
    # Complex multiplication
    re = x$re * y$re - x$im * y$im
    im = x$re * y$im + x$im * y$re
    z = list()
    z$re = re
    z$im = im
    return(z)
}

cexp = function(x) {
    # Complex exponential
    re = exp(x$re) * cos(x$im)
    im = exp(x$re) * sin(x$im)
    z = list()
    z$re = re
    z$im = im
    return(z)
}

#################### MAIN FUNCTION ####################

StandardNormalCharf = function(t, l) {
    s2 = l$sigma^2
    tmp = compl(-0.5 * s2 * t$re, -0.5 * s2 * t$im + l$mu)
    r = cexp(cmul(tmp, t))
    return(r)
}

 

```
