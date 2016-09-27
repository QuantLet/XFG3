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
