# Clear all variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Set working directory
# setwd("")

# Install and load packages
libraries = c("MASS", "scales", "foreach", "doParallel", "ald")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)} )
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

source("quantilelasso.r")

# Function computing quantile Lasso algorithm by Li & Zhu (2008) 
Qlasso = function(x, y, win) {

  k1             = ifelse(group.nr == 1, 1, sum(n.simcores[1:(group.nr - 1)],1))
  k2             = sum(n.simcores[1:group.nr])
  m              = n.param
  n              = n.obs
  xbeta.bic      = numeric(0)
  res.bic        = numeric(0)
  res.norm.bic   = numeric(0)
  coeff.norm.bic = numeric(0) 
  lambda.bic     = numeric(0)
  act.set.bic    = numeric(0)
  cond.num.bic   = numeric(0)
  max.eigen.bic  = numeric(0)
  
  xbeta.gcv      = numeric(0)
  res.gcv        = numeric(0)
  res.norm.gcv   = numeric(0)
  coeff.norm.gcv = numeric(0) 
  lambda.gcv     = numeric(0)
  act.set.gcv    = numeric(0)
  cond.num.gcv   = numeric(0)
  max.eigen.gcv  = numeric(0)
  
  for (j in k1:k2) {
    
    xbeta1.bic      = numeric(0)
    res1.bic        = numeric(0)
    res.norm1.bic   = numeric(0)
    coeff.norm1.bic = numeric(0)
    lambda.bic1     = numeric(0)
    act.set1.bic    = numeric(0)
    cond.num1.bic   = numeric(0)
    max.eigen1.bic  = numeric(0)
    
    xbeta1.gcv      = numeric(0)
    res1.gcv        = numeric(0)
    res.norm1.gcv   = numeric(0)
    coeff.norm1.gcv = numeric(0)
    lambda.gcv1     = numeric(0)
    act.set1.gcv    = numeric(0)
    cond.num1.gcv   = numeric(0)
    max.eigen1.gcv  = numeric(0)
    
    for (i in 1:(n - win + 1)) {
      
      # Normalization of columns of x
      ywin            = y[[j]][i:(i + win - 1)]
      xwin            = x[[j]][i:(i + win - 1), ]
      nwin            = nrow(xwin)
      xs              = scale(xwin, TRUE, TRUE)
      one             = rep(1, nwin)
      normx           = sqrt(drop(one %*% (xwin^2))/(n - 1))
      
      # Quantile Lasso 
      object          = qrL1(xs, ywin, tau, 50, trace = T)
      
      # Get minimal BIC
      step.bic        = which.min(object$Csic)    # Step with minimal BIC
      lambdatmp.bic   = -object$lambda[step.bic]  # Lambda minimizing BIC
      beta0.bic       = object$beta0[step.bic]    # Intercept
      coefftmp.bic    = object$beta[step.bic,]    # Coefficients
      
      coeff.bic       = coefftmp.bic/normx        # Get unscaled coefficients
      st.bic          = sum(coeff.bic != 0)       # Number of nonzero coefficients
      
      # Collect results for the fit with minimal BIC
      xbtmp.bic       = xwin %*% coeff.bic
      restmp.bic      = ywin - xbtmp.bic
      xbeta1.bic      = c(xbeta1.bic, xbtmp.bic)                  
      res1.bic        = c(res1.bic, restmp.bic)                   
      res.norm1.bic   = c(res.norm1.bic, sqrt(sum(restmp.bic^2)))
      coeff.norm1.bic = c(coeff.norm1.bic, sum(abs(coeff.bic)))
      lambda.bic1     = c(lambda.bic1, lambdatmp.bic)
      act.set1.bic    = c(act.set1.bic, st.bic)
      cond.num1.bic   = c(cond.num1.bic, kappa(t(xwin) %*% xwin))
      max.eigen1.bic  = c(max.eigen1.bic, max(eigen(t(xwin) %*% xwin)$values))  
      
      # Get minimal GACV
      step.gcv        = which.min(object$Cgacv)   # Step with minimal GACV
      lambdatmp.gcv   = -object$lambda[step.gcv]  # Lambda minimizing GACV
      beta0.gcv       = object$beta0[step.gcv]    # Intercept
      coefftmp.gcv    = object$beta[step.gcv,]    # Coefficients
      
      coeff.gcv       = coefftmp.gcv/normx        # Get unscaled coefficients
      st.gcv          = sum(coeff.gcv != 0)       # Number of nonzero coefficients
      
      # Collect results for the fit with minimal GACV
      xbtmp.gcv       = xwin %*% coeff.gcv
      restmp.gcv      = ywin - xbtmp.gcv
      xbeta1.gcv      = c(xbeta1.gcv, xbtmp.gcv)
      res1.gcv        = c(res1.gcv, restmp.gcv)
      res.norm1.gcv   = c(res.norm1.gcv, sqrt(sum(restmp.gcv^2)))
      coeff.norm1.gcv = c(coeff.norm1.gcv, sum(abs(coeff.gcv)))
      lambda.gcv1     = c(lambda.gcv1, lambdatmp.gcv)
      act.set1.gcv    = c(act.set1.gcv, st.gcv)
      cond.num1.gcv   = c(cond.num1.gcv, kappa(t(xwin) %*% xwin))
      max.eigen1.gcv  = c(max.eigen1.gcv, max(eigen(t(xwin) %*% xwin)$values))        
    }
    
    # Collect results for all simulations
    xbeta.bic         = cbind(xbeta.bic, xbeta1.bic)
    res.bic           = cbind(res.bic, res1.bic)
    res.norm.bic      = cbind(res.norm.bic, res.norm1.bic)
    coeff.norm.bic    = cbind(coeff.norm.bic, coeff.norm1.bic)
    lambda.bic        = cbind(lambda.bic, lambda.bic1)
    act.set.bic       = cbind(act.set.bic, act.set1.bic)
    cond.num.bic      = cbind(cond.num.bic, cond.num1.bic)
    max.eigen.bic     = cbind(max.eigen.bic, max.eigen1.bic)
    
    xbeta.gcv         = cbind(xbeta.gcv, xbeta1.gcv)
    res.gcv           = cbind(res.gcv, res1.gcv)
    res.norm.gcv      = cbind(res.norm.gcv, res.norm1.gcv)
    coeff.norm.gcv    = cbind(coeff.norm.gcv, coeff.norm1.gcv)
    lambda.gcv        = cbind(lambda.gcv, lambda.gcv1)
    act.set.gcv       = cbind(act.set.gcv, act.set1.gcv)
    cond.num.gcv      = cbind(cond.num.gcv, cond.num1.gcv)
    max.eigen.gcv     = cbind(max.eigen.gcv, max.eigen1.gcv)
    
    print(j)
  } 
  
  values            = list(lambda.bic, act.set.bic, res.norm.bic, coeff.norm.bic, 
                           cond.num.bic, max.eigen.bic, lambda.gcv, act.set.gcv, 
                           res.norm.gcv, coeff.norm.gcv, cond.num.gcv, max.eigen.gcv)
  names(values)     = c("lambda.bic", "act.set.bic", "res.norm.bic", "coeff.norm.bic", 
                        "cond.num.bic", "max.eigen.bic", "lambda.gcv", "act.set.gcv", 
                        "res.norm.gcv", "coeff.norm.gcv", "cond.num.gcv", "max.eigen.gcv")
  return(values)
}

# Function collecting results from all the cores
res.Qlasso = function(n.cores, input){
  
  res.norm.bic   = numeric(0)
  coeff.norm.bic = numeric(0) 
  lambda.bic     = numeric(0)
  act.set.bic    = numeric(0)
  cond.num.bic   = numeric(0)
  max.eigen.bic  = numeric(0)
  
  res.norm.gcv   = numeric(0)
  coeff.norm.gcv = numeric(0) 
  lambda.gcv     = numeric(0)
  act.set.gcv    = numeric(0)
  cond.num.gcv   = numeric(0)
  max.eigen.gcv  = numeric(0)  
  
  # Collect results from all the cores
  for (i in 1:n.cores){
    lambda.bic     = cbind(lambda.bic, input[[i]]$lambda.bic)
    act.set.bic    = cbind(act.set.bic, input[[i]]$act.set.bic)
    res.norm.bic   = cbind(res.norm.bic, input[[i]]$res.norm.bic)
    coeff.norm.bic = cbind(coeff.norm.bic, input[[i]]$coeff.norm.bic)
    cond.num.bic   = cbind(cond.num.bic, input[[i]]$cond.num.bic)
    max.eigen.bic  = cbind(max.eigen.bic, input[[i]]$max.eigen.bic)
    
    lambda.gcv     = cbind(lambda.gcv, input[[i]]$lambda.gcv)
    act.set.gcv    = cbind(act.set.gcv, input[[i]]$act.set.gcv)
    res.norm.gcv   = cbind(res.norm.gcv, input[[i]]$res.norm.gcv)
    coeff.norm.gcv = cbind(coeff.norm.gcv, input[[i]]$coeff.norm.gcv)
    cond.num.gcv   = cbind(cond.num.gcv, input[[i]]$cond.num.gcv)
    max.eigen.gcv  = cbind(max.eigen.gcv, input[[i]]$max.eigen.gcv)
  }
  
  # Compute means and medians over all simulations
  mean.rn.bic = apply(res.norm.bic, 1, mean)
  mean.cn.bic = apply(coeff.norm.bic, 1, mean)
  mean.lb.bic = apply(lambda.bic, 1, mean)  
  mean.as.bic = apply(act.set.bic, 1, mean)
  mean.k.bic  = apply(cond.num.bic, 1, mean)
  mean.me.bic = apply(max.eigen.bic, 1, mean)
  
  med.rn.bic  = apply(res.norm.bic, 1, median)
  med.cn.bic  = apply(coeff.norm.bic, 1, median)
  med.lb.bic  = apply(lambda.bic, 1, median)
  med.as.bic  = apply(act.set.bic, 1, median)
  med.k.bic   = apply(cond.num.bic, 1, median)
  med.me.bic  = apply(max.eigen.bic, 1, median)
  
  mean.rn.gcv = apply(res.norm.gcv, 1, mean)
  mean.cn.gcv = apply(coeff.norm.gcv, 1, mean)
  mean.lb.gcv = apply(lambda.gcv, 1, mean)  
  mean.as.gcv = apply(act.set.gcv, 1, mean)
  mean.k.gcv  = apply(cond.num.gcv, 1, mean)
  mean.me.gcv = apply(max.eigen.gcv, 1, mean)
  
  med.rn.gcv  = apply(res.norm.gcv, 1, median)
  med.cn.gcv  = apply(coeff.norm.gcv, 1, median)
  med.lb.gcv  = apply(lambda.gcv, 1, median)
  med.as.gcv  = apply(act.set.gcv, 1, median)
  med.k.gcv   = apply(cond.num.gcv, 1, median)
  med.me.gcv  = apply(max.eigen.gcv, 1, median)
  
  
  values        = list(lambda.bic, act.set.bic, res.norm.bic, coeff.norm.bic, 
                       cond.num.bic, max.eigen.bic, mean.lb.bic, mean.as.bic, 
                       mean.rn.bic, mean.cn.bic, mean.k.bic, mean.me.bic, med.lb.bic, 
                       med.as.bic, med.rn.bic, med.cn.bic, med.k.bic, med.me.bic,
                       lambda.gcv, act.set.gcv, res.norm.gcv, coeff.norm.gcv, 
                       cond.num.gcv, max.eigen.gcv, mean.lb.gcv, mean.as.gcv, 
                       mean.rn.gcv, mean.cn.gcv, mean.k.gcv, mean.me.gcv, med.lb.gcv, 
                       med.as.gcv, med.rn.gcv, med.cn.gcv, med.k.gcv, med.me.gcv)
  
  names(values) = c("lambda.bic", "act.set.bic", "res.norm.bic", "coeff.norm.bic", 
                    "cond.num.bic", "max.eigen.bic", "mean.lb.bic", 
                    "mean.as.bic", "mean.rn.bic", "mean.cn.bic", "mean.k.bic", 
                    "mean.me.bic", "med.lb.bic", "med.as.bic", "med.rn.bic", 
                    "med.cn.bic", "med.k.bic", "med.me.bic", "lambda.gcv", 
                    "act.set.gcv", "res.norm.gcv", "coeff.norm.gcv", "cond.num.gcv", 
                    "max.eigen.gcv", "mean.lb.gcv", "mean.as.gcv", "mean.rn.gcv", 
                    "mean.cn.gcv", "mean.k.gcv", "mean.me.gcv", "med.lb.gcv", 
                    "med.as.gcv", "med.rn.gcv", "med.cn.gcv", "med.k.gcv", 
                    "med.me.gcv")
  
  return(values)  
}

# Function to use Qlasso parallelly
par.Qlasso = function(gr.nr){ 
  
  group.nr  <<- gr.nr   # Define group of simulations to be computed on the core
  out_cores = Qlasso(X, Y, w) 
  return(out_cores)
}

# Function to simulate true coefficients (beta) with a change of q after i = n.cp
beta_sim = function(q.start, q.end){
  
  b      = list()
  
  tmp1   = rep(1, q.start) 
  tmp2   = rep(0, n.param - length(tmp1))
  b[[1]] = c(tmp1, tmp2)   # Coefficients for i <= n.cp
  
  tmp3   = rep(1, q.end)
  tmp4   = rep(0, n.param - length(tmp3))
  b[[2]] = c(tmp3, tmp4)   # Coefficients for i > n.cp
  
  return(b)
}

# Function to simulate design matrix (X) with a change of rho after i = n.cp
design_sim = function(r.start, r.end){
  
  mu  = rep(0, n.param) # Mean is set to be 0
  Sigma1 = matrix(0, nrow = n.param, ncol = n.param)   # Covariance matrix for i <= n.cp
  Sigma2 = matrix(0, nrow = n.param, ncol = n.param)   # Covariance matrix for i > n.cp
  
  for (i in 1:n.param) { 
    for (j in 1:n.param) {
      if (i == j){
        Sigma1[i, j] = 1
      } else {
        Sigma1[i, j] = r.start^abs(i - j)
      }
    }
  }
  
  for (i in 1:n.param) { 
    for (j in 1:n.param) {
      if (i == j){
        Sigma2[i, j] = 1
      } else {
        Sigma2[i, j] = r.end^abs(i - j)
      }
    }
  }
  
  X = list()
  set.seed(seed1)
  for (i in 1:n.sim){
    X1     = mvrnorm(n = n.cp, mu, Sigma1)             # Design matrix for i <= n.cp
    X2     = mvrnorm(n = (n.obs - n.cp), mu, Sigma2)   # Design matrix for i > n.cp
    X[[i]] = rbind(X1, X2)
  } 
  
  return(X)  
}

# Function to simulate error term with a change of variance after i = n.cp
eps_sim = function(sd.start, sd.end) {  
  
  eps1  = list()   # Error term for i <= n.cp
  set.seed(seed2)
  for (i in 1:n.sim){
    eps1[[i]] = rALD(n.cp, mu = 0, sigma = sd.start, p = tau)
  } 
  
  eps2  = list()   # Error term for i > n.cp
  set.seed(seed2)
  for (i in 1:n.sim){
    eps2[[i]] = rALD((n.obs - n.cp), mu = 0, sigma = sd.end, p = tau)
  }
  
  eps   = list()
  for (i in 1:n.sim){
    eps[[i]] = c(eps1[[i]],eps2[[i]])
  }
  
  return(eps)  
}

# Function to compute all n.obs observations of Y
Y_sim = function(sd.start, sd.end, q.start, q.end, r.start, r.end){
  
  Y     = list()
  eps   = eps_sim(sd.start, sd.end)       # Simulate n.sim vectors of error term
  b     = beta_sim(q.start, q.end)        # Simulate n.sim vectors of beta
  X     <<- design_sim(r.start, r.end)    # Simulate n.sim designs (define X globally)
  
  Y1    = list()                          # Y for i <= n.cp
  for (i in 1:n.sim){
    Y.tmp1 = numeric(0)
    for (j in 1:n.cp){
      Y.tmp1 = c(Y.tmp1, b[[1]] %*% X[[i]][j, ] + eps[[i]][j])
    }
    Y1[[i]] = Y.tmp1
  }
  
  Y2    = list()                          # Y for i > n.cp
  for (i in 1:n.sim){
    Y.tmp2 = numeric(0)
    for (j in (n.cp + 1):n.obs){
      Y.tmp2 = c(Y.tmp2, b[[2]] %*% X[[i]][j, ] + eps[[i]][j])
    }
    Y2[[i]] = Y.tmp2
  }
  
  Y.tmp3 = list()
  for (i in 1:n.sim){
    Y.tmp3[[i]] = c(Y1[[i]],Y2[[i]])
  }
  
  Y = Y.tmp3                              # n.sim vectors of Y of length n.obs
  return(Y)
}

# Simulation setup
n.obs    = 600        # Number of observations
n.param  = 100        # Number of parameters
n.sim    = 50         # Number of simulations
w        = 80         # Length of moving windows
seed1    = 20150206   # Seed to simulate design matrix X
seed2    = 20150602   # Seed to simulate error terms
tau      = 0.05       # Set quantile level
sd.start = 1          # Standard deviation of error term for i <= n.cp
sd.end   = 2          # Standard deviation of error term for i > n.cp
q.start  = 5          # Number of nonzero parameters for i <= n.cp
q.end    = 5          # Number of nonzero parameters for i > n.cp
r.start  = 0.5        # Correlation coefficient for design for i <= n.cp
r.end    = 0.5        # Correlation coefficient for design for i > n.cp
  
# Define observation with a change point
if(n.obs %% 2 == 1) n.obs = n.obs + 1 ;
n.cp  = n.obs/2

# Initiate cluster for parallel computing
n.cores = detectCores()   # Number of cores to be used
cl      = makeCluster(n.cores)
registerDoParallel(cl)
getDoParWorkers()

# Define number of simulation to use in every core
h.sub = function(htmp){
  repeat {
    htmp = htmp - n.cores
    if (htmp <= n.cores) break  
  }
  return(htmp)
}
n.simcores = rep(0, n.cores)    # Number of simulations for every core
h          = 1
while(h <= n.sim){
  i = ifelse(h <= n.cores, h, h.sub(h))
  n.simcores[i] = n.simcores[i] + 1
  h = h + 1
}

# Quantile Lasso estimation with moving windows of length w 
Y = Y_sim(sd.start, sd.end, q.start, q.end, r.start, r.end)
Sys.time()
out_tmp   = foreach(i = 1:n.cores) %dopar% par.Qlasso(i)    # Parallel computing
Sys.time()
out_final = res.Qlasso(n.cores, out_tmp)                    # Collect results from cores

# Close cluster
stopCluster(cl)

# Plot settings
par(mfrow = c(2, 1))
par(mar = c(3, 6, 1, 1))

# Lambda chosen by BIC criterion
# All of the simulations 
plot(out_final$lambda.bic[, 1], type = "l",  col = alpha("darkblue", 0.1), axes = FALSE, 
     xlab = "", frame = TRUE, cex.main = 1.5, ylab = expression(paste(lambda)),
     xlim = c(-(w + 10), (n.obs - w + 10)), cex.lab = 1.8)
for (i in 2:n.sim) {
  tmp = out_final$lambda.bic[, i]
  lines(tmp, col = alpha("darkblue", 0.1))
}
lines(out_final$mean.lb.bic, col = "red3")
axis(1, at = c(-w, n.cp - w, n.obs - w), labels = c("0", paste(expression("i ="), n.cp), 
                                                    n.obs), cex.axis = 1)
axis(2, cex.axis = 1)
abline(v = (n.cp - w), lty = 3)

# Average over all of the simulations 
plot(out_final$mean.lb.bic, type = "l",  col = "red3", axes = FALSE, 
     xlab = "", frame = TRUE, cex.main = 1.5, ylab = expression(paste(bar(lambda))),
     xlim = c(-(w + 10), (n.obs - w + 10)), cex.lab = 1.8)
axis(1, at = c(-w, n.cp - w, n.obs - w), labels = c("0", paste(expression("i ="), n.cp), 
                                                    n.obs), cex.axis = 1)
axis(2, cex.axis = 1)
abline(v = (n.cp - w), lty = 3)

# Cardinality of active set q
# All of the simulations 
plot(out_final$act.set.bic[, 1], type = "l", col = alpha("darkblue", 0.1), 
     axes = FALSE, xlab = "", frame = TRUE, cex.main = 1.5, ylab = "q", 
     xlim = c(-(w + 10), (n.obs - w + 10)), 
     ylim = c(min(out_final$act.set.bic), max(out_final$act.set.bic)), cex.lab = 1.8)
for (i in 2:n.sim) {
  tmp = out_final$act.set.bic[, i]
  lines(tmp, col = alpha("darkblue", 0.1))
}
lines(out_final$mean.as.bic, col = "red3")
axis(1, at = c(-w, n.cp - w, n.obs - w), labels = c("0", paste(expression("i ="), n.cp), 
                                                    n.obs), cex.axis = 1)
axis(2, cex.axis = 1)
abline(v = (n.cp - w), lty = 3)

# Average over all of the simulations 
plot(out_final$mean.as.bic, type = "l",  col = "red3", axes = FALSE, 
     xlab = "", frame = TRUE, cex.main = 1.5, ylab = expression(paste(bar(q))),
     xlim = c(-(w + 10), (n.obs - w + 10)), 
     ylim = c(min(out_final$mean.as.bic), max(out_final$mean.as.bic)), cex.lab = 1.8)
axis(1, at = c(-w, n.cp - w, n.obs - w), labels = c("0", paste(expression("i ="), n.cp), 
                                                    n.obs), cex.axis = 1)
axis(2, cex.axis = 1)
abline(v = (n.cp - w), lty = 3)

# L1-norm of Beta
# All of the simulations 
plot(out_final$coeff.norm.bic[, 1], type = "l", col = alpha("darkblue", 0.1), axes = FALSE, 
     xlab = "", frame = TRUE, cex.main = 1.5, 
     ylab = expression(paste("||", hat(beta), "|| " [1])), 
     xlim = c(-(w + 10), (n.obs - w + 10)), 
     ylim = c(min(out_final$coeff.norm.bic), max(out_final$coeff.norm.bic)), cex.lab = 1.8)
for (i in 2:n.sim) {
  tmp = out_final$coeff.norm.bic[, i]
  lines(tmp, col = alpha("darkblue", 0.1))
}
lines(out_final$mean.cn.bic, col = "red3")
axis(1, at = c(-w, n.cp - w, n.obs - w), labels = c("0", paste(expression("i ="), n.cp), 
                                                    n.obs), cex.axis = 1)
axis(2, cex.axis = 1)
abline(v = (n.cp - w), lty = 3)

# Average over all of the simulations 
plot(out_final$mean.cn.bic, type = "l",  col = "red3", axes = FALSE, 
     xlab = "", frame = TRUE, cex.main = 1.5, 
     ylab = expression(bar(paste("||", hat(beta), "|| " [1]))),
     xlim = c(-(w + 10), (n.obs - w + 10)), 
     ylim = c(min(out_final$mean.cn.bic), max(out_final$mean.cn.bic)), cex.lab = 1.8)
axis(1, at = c(-w, n.cp - w, n.obs - w), labels = c("0", paste(expression("i ="), n.cp), 
                                                    n.obs), cex.axis = 1)
axis(2, cex.axis = 1)
abline(v = (n.cp - w), lty = 3)

# L2-norm of residuals 
# All of the simulations
plot(out_final$res.norm.bic[, 1], type = "l", col = alpha("darkblue", 0.1), axes = FALSE, 
     xlab = "", frame = TRUE, cex.main = 1.5, ylab = expression(paste("RSS" ^ {1/2})), 
     xlim = c(-(w + 10), (n.obs - w + 10)), 
     ylim = c(min(out_final$res.norm.bic), max(out_final$res.norm.bic)), cex.lab = 1.8)
for (i in 2:n.sim) {
  tmp = out_final$res.norm.bic[, i]
  lines(tmp, col = alpha("darkblue", 0.1))
}
lines(out_final$mean.rn.bic, col = "red3")
axis(1, at = c(-w, n.cp - w, n.obs - w), labels = c("0", paste(expression("i ="), n.cp), 
                                                    n.obs), cex.axis = 1)
axis(2, cex.axis = 1)
abline(v = (n.cp - w), lty = 3)

# Average over all of the simulations 
plot(out_final$mean.rn.bic, type = "l",  col = "red3", axes = FALSE, 
     xlab = "", frame = TRUE, cex.main = 1.5, ylab = expression(bar(paste("RSS" ^ {1/2}))),
     xlim = c(-(w + 10), (n.obs - w + 10)), cex.lab = 1.8)
axis(1, at = c(-w, n.cp - w, n.obs - w), labels = c("0", paste(expression("i ="), n.cp), 
                                                    n.obs), cex.axis = 1)
axis(2, cex.axis = 1)
abline(v = (n.cp - w), lty = 3)
