VaRcumulantDG = function(n, l) {
    if (n == 1) {
        c = l$theta + 0.5 * sum(l$lambda)
        
    } else if (n == 2) {
        c = 0.5 * sum(l$lambda^2 + 2 * l$delta^2)
    } else {
        c = 0.5 * factorial(n - 1) * sum(l$lambda^n + n * l$delta^2 * l$lambda^(n - 2))
    }
    return(c)
} 
