[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGPowerLawEst3** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)


```yaml
<Name of QuantLet : XFGPowerLawEst3

Published in : Applied Quantitative Finance

Description : 'Estimates the Power Law parameter Alpha as well as the goodness
of fit of right tail of wealth distribution of Bitcoin.'

Keywords : 'power law, bitcoin, crypto, currency, index, wealth distribution'

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


# Part 3: Table 3
#setwd("~/Desktop/Data/Table 1")
# pre-allocating for caculation of Power Law parameters
alpha               = rep(NA, 60)
Xmin                = rep(NA, 60)
KS                  = rep(NA, 60)
P                   = rep(NA, 60)
# calculation loop for wealth distribution (using power law)
for (i in 1:60) {
  print(i)
  Data.PL           = read.csv(paste(i, ".csv", sep = ""), header = T)[, 1]
  m                 = conpl$new(Data.PL)
  est               = estimate_xmin(m)  # Estimate xmin and pars
  m$setXmin(est)
  Xmin[i]           = est$xmin
  alpha[i]          = est$pars
  KS[i]             = est$KS
  fit               = power.law.fit(Data.PL, 
                                    xmin               = Xmin[i], 
                                    start              = 2, 
                                    force.continuous   = FALSE, 
                                    implementation     = c("plfit"))
  P[i]              = fit$KS.p
}



>
```
