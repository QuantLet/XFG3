
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGCHD_VaR_Gumbel** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of Quantlet : XFGCHD_VaR_Gumbel

Published in : Applied Quantitative Finance (3rd Edition)

Description : 'Shows VaRs of Gumbel copula model introduced in the paper. The alphas are chosen
eqaul to .001.'

Keywords : visualization, copula, plot, VaR, graphical representation

Author : Yafei Xu

Submitted : 26/05/2016

Datafile : eps.126, sp126.csv

Input : eps.126, sp126.csv

Output : XFGCHD_VaR_Gumbel.png

```

![Picture1](XFGCHD_VaR_Gumbel.png)


### R Code:
```r
############################# computation of VaR using Gumbel copula
rm(list = ls(all = TRUE))
graphics.off()

# please set working directory setwd('C:/...') setwd('~/...') #
# linux/mac os setwd('/Users/...') # windows

libraries = c("fGarch", "foreach", "doParallel", "copula")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

############################# set up
eps1                = read.csv("eps1.csv")
sp.126              = read.csv("sp126.csv", header = T)[, -c(1, 2)]
p1                  = sp.126[-1, ]
p2                  = sp.126[-length(sp.126[, 1]), ]
l.r                 = 100 * log(p1/p2)
r                   = l.r
r1                  = r[, -c(1)]
r2                  = r1
dims                = 5
attach(r2)
r2                  = data.frame(cbind(AVB, EQR, TXN, ADI, LLY))
M                   = 1000
backtestNr          = 1000
VaR                 = matrix(NA, backtestNr, 4)
slidingWindowLength = 300
eps                 = matrix(NA, backtestNr, dim(r2)[2])
colnames(eps)       = colnames(r2)
eps1                = data.frame()
dat                 = eps1
paraMat             = list()
para                = matrix(NA, dims, 4)
sigma               = matrix(NA, backtestNr, dims)

############################# estimation GARCH parameter
cl     = makeCluster(37)
registerDoParallel(cl)
getDoParWorkers()
dat    = r2
datMat = list()

for (i in 1:backtestNr) {
    datMat[[i]] = dat[c(i:(i + (slidingWindowLength - 1))), 1:dims]
}
lengthPara  = 4
lengthEps1  = slidingWindowLength
lengthSigma = slidingWindowLength

objFun = function(dMat) {
    paraComb = list()
    library(fGarch)
    for (i in 1:dims) {
        fit           = garchFit(~garch(1, 1), data = dMat[[i]], trace = F)
        eps1.loop     = fit@residuals/fit@sigma.t
        para.loop     = fit@fit$coef
        sigma.loop    = fit@sigma.t
        lengthPara    = length(para.loop)
        paraComb[[i]] = c(para.loop, sigma.loop, eps1.loop)
    }
    return(paraComb)
}
resultD = foreach(dMat = datMat) %dopar% objFun(dMat)
stopCluster(cl)


############################# estimation of copula parameter
cl = makeCluster(37)
registerDoParallel(cl) 
getDoParWorkers()
lengthPara  = 4 
lengthEps1  = slidingWindowLength  
lengthSigma = slidingWindowLength
totalLength = lengthPara + lengthEps1 + lengthSigma

epsComb = list()
for (i in 1:backtestNr) {
    k            = pobs(as.data.frame(resultD[[i]]))
    epsComb[[i]] = k[-(1:(lengthPara + lengthEps1)), ] 
}

datMatEps = epsComb  
objFun = function(dMat) {
    library(copula)
    gumbel.cop  = gumbelCopula(2, dim = dims)
    fit.mln     = fitCopula(gumbel.cop, dMat, method = "mpl")
    para.normal = summary(fit.mln)$coefficient[1] 
    return(para.normal)
}

resultDcopPara = foreach(dMat = datMatEps, .combine = "rbind") %dopar% 
    objFun(dMat)  
stopCluster(cl)

############################# VaR computation
spread.real.0 = sp.126
attach(spread.real.0)
spread.real   = data.frame(cbind(AVB, EQR, TXN, ADI, LLY)) 
S             = rowSums(spread.real)
S1            = S[-1]
S2            = S[-length(S)]
S3            = S2 - S1 
L.real        = S3[301:(300 + backtestNr)]
cl            = makeCluster(37)
registerDoParallel(cl)  
getDoParWorkers()
lengthPara    = 4  
lengthEps1    = slidingWindowLength 
lengthSigma   = slidingWindowLength
totalLength   = lengthPara + lengthEps1 + lengthSigma
para.vec      = resultDcopPara  
VaR           = matrix(NA, backtestNr, 4)  
datMatIndex   = c(1:backtestNr)
objFunVaR = function(dMat) {
    i = dMat
    library(copula)
    para.normal      = para.vec[i]
    gumbel.cop.2     = gumbelCopula(para.normal, dim = dims)   
    u                = rCopula(M, gumbel.cop.2)  
    u                = qnorm(u)
    k                = as.data.frame(resultD[[i]])  
    k1               = k[c(1:lengthPara, lengthPara + lengthEps1, totalLength), ] 
    h                = sqrt(k1[2, ] + k1[3, ] * k1[lengthPara + 1, ]^2 * k1[lengthPara + 
                       2, ]^2 + k1[4, ] * k1[lengthPara + 1, ]^2)  
    k2               = matrix(NA, M, dims)
    k3               = matrix(NA, M, dims)
    k2[1, ]          = as.matrix(k1[1, ])  
    mu.t             = k2[rep(1, M), ] 
    k3[1, ]          = as.matrix(k1[lengthPara + 1, ])  
    sig.t            = k3[rep(1, M), ]  
    R                = mu.t + sig.t * u  
    spread.real.loop = read.csv("sp126.csv")[, -c(1:2)]
    attach(spread.real.loop)
    spread           = data.frame(cbind(AVB, EQR, TXN, ADI, LLY))
    s1               = spread[i + (slidingWindowLength), ]  
    st               = data.frame(matrix(NA, M, dims)) 
    st[1, ]          = s1
    st               = st[rep(1, M), ]
    L.sim            = matrix(NA, M, 1) 
    L.sim            = rowSums(st * (exp(0.01 * R) - 1))  
    VaRt1005         = quantile(L.sim, 0.05)
    VaRt1001         = quantile(L.sim, 0.01)
    VaRt10005        = quantile(L.sim, 0.005)
    VaRt10001        = quantile(L.sim, 0.001)
    VaRt             = c(VaRt1005, VaRt1001, VaRt10005, VaRt10001)   
    return(VaRt)  
}
resultVaR  = foreach(dMat = datMatIndex, .combine = "rbind") %dopar% objFunVaR(dMat)  
stopCluster(cl)
head(resultVaR) 
VaR        = resultVaR
resultComb = data.frame(L.real, VaR)

############################# exceeding ratio
Exceeding_Ratio = numeric(4)
for (alpha in 2:5) {
    nullVector = rep(0, backtestNr)
    for (i in 1:length(resultComb[, alpha])) {
        if (resultComb[, 1][i] < resultComb[, alpha][i]) {
            nullVector[i] = 1
        } else {
            nullVector[i] = 0
        }
    }
    Exceeding_Ratio[alpha] = sum(nullVector)/backtestNr
}
Exceeding_Ratio

############################# plot of VaR and quantile = 0.001
alpha           = 5
ptForPlot       = c(min(resultComb[, 1]), min(resultComb[, 2]), min(resultComb[, 
                  3]), min(resultComb[, 4]), min(resultComb[, 5]))
lowPt           = min(ptForPlot)
upPt            = max(resultComb[, 1])
Portfolio_Value = seq(lowPt, upPt, length.out = length(resultComb[, 1]))
Time_Index      = 1:length(seq(lowPt, upPt, length.out = length(resultComb[, 
                  1])))
plot(Time_Index, 
     Portfolio_Value, 
     col  = "white", 
     pch  = 19, 
     cex  = 0.5, 
     xlab = "Time Index", 
     ylab  = "Profit and Loss of Portfolio")
lines(resultComb[, alpha], col = "gray", lwd = 6)

for (i in 1:length(resultComb[, alpha])) {
    if (resultComb[, 1][i] < resultComb[, alpha][i]) {
        points(i, lowPt + 1, col = "black", pch = 17, cex = 1.5)  
        points(i, resultComb[, 1][i], 
               col = "black", 
               pch = 3, 
               cex = 2.5, 
               lwd = 1)  
        points(i, resultComb[, 1][i], 
               col = "black", 
               pch = 5, 
               cex = 1.5, 
               lwd = 1) 
    } else {
        points(i, resultComb[, 1][i], 
               col = "black", 
               pch = 19, 
               cex = 1) 
    }
}



```
