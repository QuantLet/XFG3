
[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGbetatsacf** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml
Name of QuantLet : XFGbetatsacf

Published in : XFG (3rd Edition)

Description : 'Plots time evolvement and sample autocorrelation function for 
IBM. The upper panel presents the time-evolvement of the monthly realized beta 
over the period raging from 2001 to 2006 with 95%-confidence interval. The 
sample autocorrelation function of realized betas in the lower panel indicates 
the significant serial correlation.'

Keywords : 'acf, plot, graphical representation, frequency, time-series,
confidence-interval, autocorrelation'

See also : 

Author : Nicolas Hautsch, Uta Pigorsch

Submitted :

Datafile : ibmBeta.dat

Example : Time evolvement and sample autocorrelation function of realized volatility for IBM.

```

![Picture1](XFGbetatsacf.png)


```r
# clear history
rm(list = ls(all = TRUE))
graphics.off()


# load data
ibm = read.table("ibmBeta.dat", header = F, col.names = c("date", "beta", "conf"))

# if save output as pdf
# pdf(file = "beta_ts_acf.pdf", paper = "special", width = 6, height = 6)
# plots the upper panel
op        = par(no.readonly = TRUE)
par(mgp   = c(1.75, 0.75, 0))
par(mar   = c(3, 3, 0.7, 0.7))
par(mfrow = c(2, 1))
plot(as.Date(as.character(ibm$date), "%Y%m%d"), ibm$beta, type = "l", xlab = "time", 
     ylab = "beta", lwd = 2, col = "blue", ylim = c(0, 2))

# add confidence internal of upper panel
par(new   = T)
plot(as.Date(as.character(ibm$date), "%Y%m%d"), ibm$beta + ibm$conf, type = "l", 
     xlab = "time", ylab = "beta", lwd = 1, col = "red", lty = 2, ylim = c(0, 2))
par(new   = T)
plot(as.Date(as.character(ibm$date), "%Y%m%d"), ibm$beta - ibm$conf, type = "l", 
     xlab = "time", ylab = "beta", lwd = 1, col = "red", lty = 2, ylim = c(0, 2))

# plots the lower panel
acfData   = acf(ibm$beta, lag.max = 40, type = "correlation", plot = FALSE)
conf      = 2/sqrt(length(ibm$beta))
plot(acfData$lag, acfData$acf, type = "h", xlab = "lag", ylab = "ACF", lwd = 2, col = "blue", 
     ylim = c(-0.3, 1))
par(new   = T)
plot(acfData$lag, array(data = conf, dim = length(acfData$lag)), type = "l", xlab = "lag", 
     ylab = "ACF", lwd = 1, col = "red", lty = 2, ylim = c(-0.3, 1))
par(new   = T)
plot(acfData$lag, array(data = -conf, dim = length(acfData$lag)), type = "l", xlab = "lag", 
     ylab = "ACF", lwd = 1, col = "red", lty = 2, ylim = c(-0.3, 1))

```
