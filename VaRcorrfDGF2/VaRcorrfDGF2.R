VaRcorrfDGF2 = function(x, l) {
    # cdf of normal approximation
    mu = l$theta + 0.5 * sum(l$lambda)
    s2 = sum(l$delta^2 + 0.5 * l$lambda^2)
    r = pnorm((x - mu)/sqrt(s2))
    return(r)
}

#################### TEST ######################

theta = 0
delta = c(1)
lambda = c(1)
par = list(theta = theta, delta = delta, lambda = lambda)
VaRcorrfDGF2(c(-1, 0, 1), par)

 
