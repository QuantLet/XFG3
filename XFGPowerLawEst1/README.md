[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGPowerLawEst1** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)


```yaml
<Name of QuantLet : XFGPowerLawEst1

Published in : Applied Quantitative Finance

Description : 'Estimates the Power Law parameter Alpha as well as the goodness 
of fit of wealth distribution of Bitcoin with Xmin = 1 by default.'

Keywords : 'power law, bitcoin, crypto, currency, index'

See also : 

Author : Guo Li

Submitted :

Datafile : i.csv, i = 1, 2, ..., 60; Bitcoin_Data1.csv

Example : >
```


```R
<# please download the following package
libraries = c("lubridate", "poweRlaw","igraph","tables","texreg")
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Part1: for figure 2,5,7 & Table 1 specify the path to the desired folder,
# please set your own directory, e.g (C:/Users/Desktop/...)
#setwd("~/Desktop/Data/Table 1")
# pre-allocating for caculation of Power Law parameters
Xmin                          = rep(NA, 60)
P                             = rep(NA, 60)
KS                            = rep(NA, 60)
alpha                         = rep(NA, 60)
# Wealth distribution calculation loop (Fitted by Power Law model)
for (i in 1:length(Xmin)) {
  print(i)
  Data.PL                     = read.csv(paste(i, ".csv", sep = ""), 
                                         header                 = T)[, 1]
  fit                         = power.law.fit(Data.PL, xmin     = 1, 
                                              start             = 2, 
                                              force.continuous  = FALSE, 
                                              implementation    = c("plfit"))
  P[i]                        = fit$KS.p
  Xmin[i]                     = fit$xmin
  alpha[i]                    = fit$alpha
  KS[i]                       = fit$KS.stat
}





>
```
