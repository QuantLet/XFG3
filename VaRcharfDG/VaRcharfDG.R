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

cexp = function(x) {
    # Complex exponential
    re = exp(x$re) * cos(x$im)
    im = exp(x$re) * sin(x$im)
    z = list()
    z$re = re
    z$im = im
    return(z)
}

############################ Main Program ############################
VaRcharfDG = function(t, par) {
    t = compl(-t$im, t$re)  # 1i*t
    r = cexp(VaRcgfDG(t, par))
}
 
