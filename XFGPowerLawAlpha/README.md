[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGPowerLawAlpha** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)


```yaml
<Name of QuantLet : XFGPowerLawAlpha

Published in : Applied Quantitative Finance

Description : 'Estimates the Power Law parameter Alpha for wealth distribution of Bitcoin and Auroracoin.'

Keywords : ' power law, bitcoin, crypto, currency, index, wealth distribution'

See also : 

Author : Guo Li

Submitted :

Datafile : i.csv, i = 1, 2, ..., 60; Bitcoin_Data1.csv; Aj.csv, j=2, 3, ..., 10 Auroracoin.csv

Example : >
```


![Picture1](XFGPowerLawAlpha1.PNG)
![Picture1](XFGPowerLawAlpha2.PNG)
![Picture1](XFGPowerLawAlpha3.PNG)

```R
<setwd("~/Desktop/Data")
source("XFGPowerLawEst1.R")
# sample period
startDate                                  = ymd(20081231)
Date                                       = startDate %m+% months(c(1:(length(alpha))))
# Draw Figure 5: Bitcoin Alpha (whole sample)
par(mfrow                                    = c(1, 1))
plot(Date[2:60], alpha[2:60], 
     type                                     = "l", 
     ylab                                     = c("Alpha"), 
     xlab                                     = c("Date"), 
     main                                     = "Bitcoin Powerlaw Estimation (whole sample)")


source("PowerLawEst2.R")
# sample period
startDate                                  = ymd(20140228)
Date                                       = startDate %m+% months(c(1:(length(alpha.2))))
# Draw Figure 6: Auroracoin Alpha right tail
par(mfrow                                    = c(1, 1))
plot(Date, alpha.2, 
     type                                     = "l", 
     ylab                                     = c("Alpha"), 
     xlab                                     = c("Date"), 
     main                                     = "Aurora Powerlaw Estimation (Right Tail)")

source("PowerLawEst3.R")
# sample period
startDate                                  = ymd(20081231)
Date                                       = startDate %m+% months(c(1:(length(alpha))))
# Draw Figure 9: Bitcoin Alpha (Right Tail)
par(mfrow                                    = c(1, 1))
plot(Date[1:60], alpha[1:60], 
     type                                     = "l", 
     ylab                                     = c("Alpha"), 
     xlab                                     = c("Date"), 
     main                                     = "Bitcoin Powerlaw Estimation (Right Tail)")
)










>
```
