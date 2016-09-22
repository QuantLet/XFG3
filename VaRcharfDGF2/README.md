
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **VaRcharfDGF2** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : VaRcharfDGF2

Published in : Applied Quantitative Finance

Description : 'Computes the Fourier transform of an approximating Gaussian cumulative distribution
function (CDF) for the class of quadratic forms of Gaussian vectors. Notes: This is an auxiliary
function to VaRcdfDG.'

Keywords : Delta-Gamma-models, Fourier transform, cdf, gaussian, function

See also : VaRcdfDG, VaRcharfDG, VaRcorrfDGF2, VaRqDG, XFGVaRcharfDGtest, XFGqDGtest

Author : Awdesch Melzer

Submitted : Sun, June 02 2013 by Awdesch Melzer

Usage : r = VaRcharfDGF2(t,l)

Input: 
- t: the complex argument of the transformed function
- l: a list defining the distribution contains at least the components theta, delta, labda
- theta: the constant term
- delta: the linear term
- lambda: the diagonal of the quadratic term

-r : the (complex) value of the Fourier inversion at t

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
cexp = function(x) {
    # Complex exponential
    re = exp(x$re) * cos(x$im)
    im = exp(x$re) * sin(x$im)
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
csub = function(x, y) {
    # Complex subtraction two arrays of complex numbers
    re = x$re - y$re
    im = x$im - y$im
    z = list()
    z$re = re
    z$im = im
    return(z)
}
cdiv = function(x, y) {
    # Complex division
    w = y$re^2 + y$im^2
    re = (x$re * y$re + x$im * y$im)/w
    im = (x$im * y$re - x$re * y$im)/w
    z = list()
    z$re = re
    z$im = im
    return(z)
}
cln = function(x) {
    # Complex natural logarithm
    re = log(x$re^2 + x$im^2)/2
    im = atan2(x$im, x$re)
    z = list()
    z$re = re
    z$im = im
    return(z)
}

VaRcgfDG = function(t, par) {
    # cumulant generating function (cgf) for the class of quadratic forms of Gaussian vectors.
    
    s = compl(par$theta * t$re, par$theta * t$im)
    i = 1
    m = length(par$lambda)
    while (i <= m) {
        # 1-lambda*t:
        omlt = compl(1 - par$lambda[i] * t$re, -par$lambda[i] * t$im)
        tmp = cmul(t, t)
        tmp = cdiv(tmp, omlt)
        tmp = compl(par$delta[i]^2 * tmp$re, par$delta[i]^2 * tmp$im)
        tmp = csub(tmp, cln(omlt))
        s = compl(s$re + 0.5 * tmp$re, s$im + 0.5 * tmp$im)
        i = i + 1
    }
    return(s)
}


VaRcharfDG = function(t, par) {
    # computes the characteristic function for the class of quadratic forms of Gaussian vectors.
    t = compl(-t$im, t$re)  # 1i*t
    r = cexp(VaRcgfDG(t, par))
}

############################ Main Program ############################
VaRcharfDGF2 = function(t, l) {
    mu = l$theta + 0.5 * sum(l$lambda)
    s2 = sum(l$delta^2 + 0.5 * l$lambda^2)
    tmp = compl(-0.5 * s2 * t$re, -0.5 * s2 * t$im + mu)
    tmp = cexp(cmul(tmp, t))
    tmp = csub(VaRcharfDG(t, l), tmp)
    tmp = cdiv(tmp, t)
    r = compl(-tmp$im, tmp$re)
    r$re = replace(r$re, r$re == "NaN", 0)
    r$im = replace(r$im, r$im == "NaN", 0)
    return(r)
}
 

```
