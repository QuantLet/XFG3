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
