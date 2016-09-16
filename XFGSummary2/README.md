
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGSummary2** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : XFGSummary2

Published in : Applied Quantitative Finance

Description : 'Provides an overview of the data used in the estimation of the SV, SVt and SVJ
model. The data consists of daily continuously compounded returns of the DAX index, the Dow Jones
index and the GBP/USD FX rate. All time series cover the period from 1 January, 1991 to 21 March,
2007, yielding 4,231 observations.'

Keywords : Markov, kurtosis, mean, quantile, skewness, summary, time-series, volatility

Author : Marius Lux

Submitted : Mon, November 17 2014 by Felix Jung

Usage : 'Set working directory to source file location. The function is called by sumStat(X), with
X being the data.'

Datafile : XFGSummary2Returns

```


### R Code:
```r
# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()


# set working directory to source file location and read the data
# setwd("C:/...")
x = read.table("Returns.txt", header = TRUE)


# define the function
sum.stat = function(X) {

    # Computes the mean, standard deviation, median, quantiles (10% and 90 %),
    # skewness and kurtosis of the input data. If the the input data is a
    # matrix, the calculations are done on the columns. 
    #
    # Args:
    #   x: Input Data

    m    = apply(x,2,mean)  # mean
    sd   = apply(x, 2, sd)  # standard deviation
    med  = apply(x, 2, median)  # median
    q    = apply(x, 2, quantile, probs = c(0.1, 0.9))  # quantiles

    z    = scale(x)  # normalize columns of x
    skew = colSums(z^3)/(dim(x)[1])  # skewness
    kurt = colSums(z^4)/(dim(x)[1])  # kurtosis

    tab            = data.frame(cbind(m, sd, med, t(q), skew, kurt))  # create table with results
    names(tab)     = c("Mean", "SD", "Median", "0.1-q", "0.9-q", "Skewness","Kurtosis")  # name table columns
    row.names(tab) = names(x)  # name table rows

    return(tab)
}

# call function
sum.stat(x)
```
