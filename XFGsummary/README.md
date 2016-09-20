
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGsummary** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : XFGsummary

Published in : Applied Quantitative Finance

Description : 'Calculates different summary statistics, such as minimum, maximum, mean, median and
standard errors for the INAAA data'

Keywords : summary, mean, median, variance, descriptive-statistics, time-series, financial

Author : Germar Knoechlein, Awdesch Melzer

Submitted : Sat, June 16 2012 by Dedy Dwi Prastyo

Datafiles : XFGINAAA.dat

```


### R Code:
```r
# ----------------------------------------------------------------------
#  Book          XFG2        
# ----------------------------------------------------------------------
#  See_also      summarize
# ----------------------------------------------------------------------
#  Macro         XFGsummary
# ----------------------------------------------------------------------
#  Description   Descriptive Statistics
# ----------------------------------------------------------------------
#  Usage         -
# ----------------------------------------------------------------------
#  Input         none
# ----------------------------------------------------------------------
#  Output        
# ----------------------------------------------------------------------
#  Author        Xplore: Germar Knoechlein, 20010106;
#                R: Awdesch Melzer, 20120614
# ----------------------------------------------------------------------

# clear variables and close windows
rm(list=ls(all=TRUE))
graphics.off()

# define summarize function
summarize = function(x){
    Minimum   = apply(x,2,min)
    Maximum   = apply(x,2,max)
    Mean      = apply(x,2,mean)
    Median    = apply(x,2,median)
    Std.Error = apply(x,2,sd)

    table = cbind(Minimum, Maximum, Mean, Median, Std.Error)
    print(table)
}

# load data
INAAA = read.table("XFGINAAA.dat")

# specify columns
INAAA = INAAA[,4:12]

# rename columns
colnames(INAAA) = c("3M", "6M", "1Y", "2Y", "3Y", "4Y", "5Y", "7Y", "10Y")

summarize(INAAA)

```
