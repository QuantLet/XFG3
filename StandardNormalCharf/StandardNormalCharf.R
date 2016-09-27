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

 
