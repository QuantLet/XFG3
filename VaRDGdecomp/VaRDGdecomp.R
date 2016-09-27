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
 
