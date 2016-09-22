
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGVaRcharfDGtest** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : XFGVaRcharfDGtest

Published in : Applied Quantitative Finance

Description : 'Plots the real (blue line) and the imaginary part (red line) of the characteristic
function for a distribution, which is close to a chi^2 distribution with one degree of freedom.'

Keywords : Delta-Gamma-models, characteristic function, chi-square, plot, visualization

See also : VaRcharfDG, VaRcharfDGF2

Author : Awdesch Melzer

Submitted : Mon, June 03 2013 by Awdesch Melzer

Example : 'Plot of the real (blue line) and the imaginary part (red line) of the characteristic
function for a distribution, which is close to a chi^2 distribution with one degree of freedom.'

```

![Picture1](Characteristic function.png)


### R Code:
```r
rm(list = ls(all = TRUE))
graphics.off()


############################ SUBROUTINES ################################ Complex array generation
compl = function(re, im) {
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

VaRcgfDG = function(t, par) {
    # cumulant generating function (cgf) for the class of quadratic forms of Gaussian vectors.  Complex array generation
    compl = function(re, im) {
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
    s = compl(par$theta * t$re, par$theta * t$im)
    cmul = function(x, y) {
        # Complex multiplication
        re = x$re * y$re - x$im * y$im
        im = x$re * y$im + x$im * y$re
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
    csub = function(x, y) {
        # Complex subtraction two arrays of complex numbers
        re = x$re - y$re
        im = x$im - y$im
        z = list()
        z$re = re
        z$im = im
        return(z)
    }
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

cexp = function(x) {
    # Complex exponential
    re = exp(x$re) * cos(x$im)
    im = exp(x$re) * sin(x$im)
    z = list()
    z$re = re
    z$im = im
    return(z)
}

VaRcharfDG = function(t, par) {
    # computes the characteristic function for the class of quadratic forms of Gaussian vectors.
    t = compl(-t$im, t$re)  # 1i*t
    r = cexp(VaRcgfDG(t, par))
}

############################ Main Program ############################

XFGVaRcharfDGtest = function(par, n, xlim) {
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
    dt = (xlim[2] - xlim[1])/(n - 1)
    t = xlim[1] + (0:(n - 1)) * dt
    r = VaRcharfDG(compl(t, t * 0), par)
    z1 = cbind(t, r$re)
    z2 = cbind(t, r$im)
    plot(z1, type = "l", col = "blue3", lwd = 2, ylab = "Y", xlab = "X", ylim = c(min(r$re, r$im), max(r$re, r$im)))
    lines(z2, col = "red3", lwd = 2)
    title("Characteristic function")
}

theta = 0
delta = c(0)
lambda = c(1.4142)
par = list()
par$theta = theta
par$delta = delta
par$lambda = lambda

XFGVaRcharfDGtest(par, 300, c(-40, 40)) 

```
