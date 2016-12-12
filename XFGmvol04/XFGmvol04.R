# load required libraries
library(ggplot2)
library(gridExtra)
library(ks)

# load stored datasets
fxdata = NULL
fxdiff = NULL
load("fxdata.dat")

# load stored coefficient matrices
C = NULL
A = NULL
G = NULL
load("estimates.dat")

# load stored estimated (co)varinace processes
sigmaest = NULL
load("sigmaest.dat")


# prepare data ------------------------------------------------------------

set.seed(0)

# settings; caution: settings might imply a high computational workload
# 
# reps : number of replications for each simulation (5000 in book)
# simul: number of simulations (3714 in book)
# days : number of days to forecast (5 in book)
reps  = 5000 # 5000
simul = 3714 # 3714
days  = 5

# reshape the (co)variance processes for convenience
sigmamat = cbind(sigmaest$sigma11,
                 sigmaest$sigma12,
                 sigmaest$sigma12,
                 sigmaest$sigma22) / 1e5


# ex-ante density forecast comparison -------------------------------------

# predefinitions for convenience and speed
tA  = t(A)
tG  = t(G)
CC  = t(C) %*% C
uA  = diag(diag(A))
uG  = diag(diag(G))
uCC =
    diag(diag(matrix(sigmamat[1,], 2))) %*% (diag(2) - (uA ^ 2 + uG ^ 2))

# predefine for storing the ex-ante likelihood comparisons
likhoodmem  = -99
likhoodstat = 0

# define matrix decomposition functions for convenience
sqrtMat = function(A) {
    eig = eigen(A, TRUE)
    return(eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors))
}

# compute density forecast and conduct comparison
for (k in 1:simul) {
    sigma0    = matrix(sigmamat[k,], 2)
    fx0       = fxdata[k, 1]
    fx2       = fxdata[k, 2]
    
    sigmaUni0 = diag(diag(sigma0))
    sigma0    = array(sigma0, dim = c(2, 2, reps))
    sigmaUni0 = array(sigmaUni0, dim = c(2, 2, reps))
    
    # re-arrange starting values:
    # fx0 contains DEM/USD, fx2 GBP/USD;
    # 1. col BiGarch-values, 2. col UniGarch-values
    fx0 = array(log(cbind(fx0, fx0)), dim = c(1, 2, reps))
    fx2 = array(log(cbind(fx2, fx2)), dim = c(1, 2, reps))
    
    # forecast 'reps'-times the exchanges under two scenarios 'days' in advance.
    for (i in 1:days) {
        xi = matrix(rnorm(reps * 2), 2)
        for (j in 1:reps) {
            sigmahalf        = sqrtMat(sigma0[, , j])
            sigmahalfUni     = sqrt(sigmaUni0[, , j])
            r                = sigmahalf %*% xi[, j]
            ur               = sigmahalfUni %*% xi[, j]
            fx0[, , j]       = fx0[, , j] + cbind(r[1, 1], ur[1, 1])
            fx2[, , j]       = fx2[, , j] + cbind(r[2, 1], ur[2, 1])
            
            E                = r %*% t(r)
            uE               = diag(diag(ur %*% t(ur)))
            
            sigma0[, , j]    =
                CC + tA %*% E %*% A + tG %*% sigma0[, , j] %*% G
            sigmaUni0[, , j] =
                uCC + uA %*% uE %*% uA + uG %*% sigmaUni0[, , j] %*% uG
        }
    }
    
    # transform exchange rate forecasts and adjust variances
    fxT         = t(matrix(fx0, 2))
    fxT2        = t(matrix(fx2, 2))
    fxT[, 2]    =
        mean(fxT[, 2]) + (fxT[, 2] - mean(fxT[, 2])) * sd(fxT[, 1]) / sd(fxT[, 2])
    fxT2[, 2]   =
        mean(fxT2[, 2]) + (fxT2[, 2] - mean(fxT2[, 2])) * sd(fxT2[, 1]) / sd(fxT2[, 2])
    
    # reshape simulated samples
    fxTBiv = cbind(fxT[, 1], fxT2[, 1])
    fxTUni = cbind(fxT[, 2], fxT2[, 2])
    
    # Scott's Rule for bandwidth choice
    hBiv = sqrt(apply(fxTBiv, 2, var)) * (reps ^ (-1 / 6))
    
    # compute kernel density for "bivariate sample"
    BivDens = kde(x = fxTBiv, h = hBiv)
    bivd    = predict(BivDens, x = log(fxdata[k + days, ]))
    
    # compute kernel density for "univariate sample"
    UniDens = kde(x = fxTUni, h = hBiv)
    unid    = predict(UniDens, x = log(fxdata[k + days, ]))
    
    # compute density comparison and display progress
    likhoodstat = likhoodstat + (bivd >= unid)
    likhoodmem  = c(likhoodmem, bivd >= unid)
    cat(sprintf(
        "Status: %5.2f%%  Level: %4.3f\n",
        k / simul * 100,
        likhoodstat / k
    ))
}

# save results
save(likhoodmem,
     file = paste0("likhoodmem", "_", reps, "_", simul, ".dat"))


# prepare data ------------------------------------------------------------

# load likhoodmem again - if forecasting is done separately
likhoodmem = NULL
load(paste0("likhoodmem", "_", reps, "_", simul, ".dat"))

success = likhoodmem[-1]
dates   = fxdiff$date[-(c(1:4, 3719))]

# compute overall success ratio
mean(success)


# compute table 1 ---------------------------------------------------------

sryears = c(
    till82 = mean(success[dates <  1982]),
    till84 = mean(success[dates >= 1982 & dates < 1984]),
    till86 = mean(success[dates >= 1984 & dates < 1986]),
    till88 = mean(success[dates >= 1986 & dates < 1988]),
    till90 = mean(success[dates >= 1988 & dates < 1990]),
    till92 = mean(success[dates >= 1990 & dates < 1992]),
    till94 = mean(success[dates >= 1992])
)

nyears = c(
    till82 = sum(dates <  1982),
    till84 = sum(dates >= 1982 & dates < 1984),
    till86 = sum(dates >= 1984 & dates < 1986),
    till88 = sum(dates >= 1986 & dates < 1988),
    till90 = sum(dates >= 1988 & dates < 1990),
    till92 = sum(dates >= 1990 & dates < 1992),
    till94 = sum(dates >= 1992)
)

srtable = as.matrix(data.frame(sryears = sryears, nyears = nyears))
srtable


# plot figure 4 -----------------------------------------------------------

# compute success ratio rolling window with 80 days
n    = length(success)
sr80 = numeric(n)
for (i in 1:n) {
    sr80[i] = mean(success[i:min(i + 79, n)])
}

successratio = data.frame(date    = dates,
                          sr      = sr80,
                          sigma12 = sigmaest$sigma12[-(c(1:4, 3719))] / 10)

ggplot(successratio[-((n - 79):n),]) +
    geom_line(
        mapping = aes(x = date, y = sigma12),
        color   = "blue",
        size    = 0.2
    ) +
    geom_line(mapping = aes(x = date, y = sr),
              color   = "red",
              size    = 0.4) +
    scale_x_continuous(breaks = seq(1981, 1993, by = 4), expand = c(0, 0))  +
    ggtitle("Covariance and success ratio") +
    ylab(expression(paste(sigma[12], "          ", "SR"[J]))) +
    xlab("Time") +
    # theme_bw() +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text  = element_text(size = 10),
        axis.title = element_text(size = 12)
    )

# optionally, export figure to pdf
ggsave("successfig.pdf", width = 16, height = 12, units = "cm")
