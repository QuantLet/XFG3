VaRestMC = function(VaRdelta, VaRgamma, VaRcovmatrix, smethod, alpha, days, nsimu) {
    if (missing(VaRdelta)) {
        stop("Please enter a delta vector")
    }
    if (missing(VaRgamma)) {
        stop("Please enter a gamma vector")
    }
    if (missing(VaRcovmatrix)) {
        stop("Please enter a covariance matrix")
    }
    if (missing(alpha)) {
        alpha = 0.01
    }
    if (missing(days)) {
        days = 1
    }
    if (missing(nsimu)) {
        nsimu = 1000
    }
    if (missing(smethod)) {
        smethod = "IS"
    }
    
    alpha = alpha
    days = days
    nsimu = nsimu
    delta = as.matrix(VaRdelta)
    gamma = as.matrix(VaRgamma)
    covmatrix = as.matrix(VaRcovmatrix)
    n = nrow(delta)
    
    z = matrix(0, n, nsimu)
    for (i in 1:n) {
        z[i, ] = rnorm(nsimu, 0, 1)
    }
    
    chold = function(x) {
        tmp = abs(qr(x)$qr)
        tmp[col(tmp) > row(tmp)] = 0
        tmp = t(tmp)
        return(tmp)
    }
    
    # cholesky decomposition
    covmatrix = days/365 * covmatrix
    kd = chold(covmatrix)
    d = diag(diag(kd))
    k = kd - d + diag(rep(1, n))
    A = t(k) %*% sqrt(d)
    
    if (smethod == "PS") {
        # 'PS': Plain vanilla sampling method
        z = A %*% z
        i = 1
        L = matrix(0, nsimu, 1)
        while (i <= nsimu) {
            L[i, 1] = -t(delta) %*% z[, i] - 0.5 * t(z[, i]) %*% gamma %*% z[, i]
            i = i + 1
        }
        VaRMC = quantile(L, (1 - alpha))
        
        
    } else if (smethod == "MS") {
        # 'MS': Moment matching sampling method
        z = (z - matrix(apply(z, 1, mean), n, nsimu))/matrix(sqrt(apply(z, 1, var)), n, nsimu)  # moment matching sampling method
        z = A %*% z
        i = 1
        L = matrix(0, nsimu, 1)
        while (i <= nsimu) {
            L[i, 1] = -t(delta) %*% z[, i] - 0.5 * t(z[, i]) %*% gamma %*% z[, i]
            i = i + 1
        }
        VaRMC = quantile(L, (1 - alpha))
        
        
    } else if (smethod == "SS") {
        # 'SS': Stratified Latin Hypercube sampling method
        i = 1
        z = matrix(0, n, nsimu)
        while (i <= n) {
            ii = 1
            z1 = runif(nsimu, 0, 1)
            z2 = 1:nsimu
            z3 = cbind(z1, z2)
            z3 = z3[order(z3[, 1]), ]
            while (ii <= nsimu) {
                z[i, ii] = qnorm((z3[ii, 2] + runif(1) - 1)/nsimu)  #stratified latin hypercube sampling method
                ii = ii + 1
            }
            i = i + 1
        }
        z = A %*% z
        i = 1
        L = matrix(0, nsimu, 1)
        while (i <= nsimu) {
            L[i, 1] = -t(delta) %*% z[, i] - 0.5 * t(z[, i]) %*% gamma %*% z[, i]
            i = i + 1
        }
        VaRMC = quantile(L, (1 - alpha))
        
        
        
    } else if (smethod == "IS") {
        # 'IS': Importance sampling method 1. Decompostion Process
        tem = -0.5 * t(A) %*% gamma %*% A
        vx = eigen(tem)
        V = vx$vectors
        lambda = vx$values
        C = A %*% V
        b = t(-t(delta) %*% C)
        
        # 2. Use Newton-Raphson method to find solution for theta
        theta = 0
        ac = 1
        i = 1
        di = 1
        
        # use Delta normal method to set initial guest of x
        x = -qnorm(alpha) * sqrt(sum(b^2))
        while (i <= 1000) {
            # (ac<=-0.0001 || ac>=0.0001) &&
            ac = sum((theta * b^2 * (1 - theta * lambda))/((1 - 2 * theta * lambda)^2) + lambda/(1 - 2 * theta * lambda)) - 
                x
            di = sum(b^2/(1 - 2 * theta * lambda) + 2 * lambda^2/(1 - 2 * theta * lambda)^2 + 4 * theta * lambda * b^2 * (1 - 
                theta * lambda)/(1 - 2 * theta * lambda)^3)
            theta = theta - ac/di
            i = i + 1
        }
        
        # 3. set sig and mu
        sig = solve(diag(rep(1, n)) - 2 * theta * diag(lambda))
        mu = theta * sig %*% b
        
        # 4. Simulation
        z = matrix(mu, n, nsimu) + sqrt(sig) %*% z
        cs = C %*% z
        L = matrix(0, nsimu, 1)
        W = matrix(0, nsimu, 1)
        
        # To calculate the value for moment generating function
        psi = 0.5 * sum((theta * b)^2/(1 - 2 * theta * lambda) - log(1 - 2 * theta * lambda))
        i = 1
        while (i <= nsimu) {
            L[i, 1] = t(-delta) %*% cs[, i] + t(cs[, i]) %*% -(gamma) %*% cs[, i] * 0.5
            W[i, 1] = exp(-theta * L[i, 1] + psi)
            i = i + 1
        }
        WL = cbind(W, L)
        Wtem = cumsum(sort(W))/nsimu
        WL = WL[order(WL[, 1]), ]
        nr = length(which(Wtem <= alpha))
        VaRMC = WL[nr, 2]
        
        
    }
    
    return(VaRMC)
} 
