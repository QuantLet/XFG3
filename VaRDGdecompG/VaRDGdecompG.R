VaRDGdecompG = function(l) {
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
