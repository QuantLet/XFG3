[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGPowerLawEst2** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)


```yaml
<Name of QuantLet : XFGPowerLawEst2

Published in : Applied Quantitative Finance

Description : 'Estimates the Power Law parameter Alpha as well as the goodness
of fit of right tail of wealth distribution of Auroracoin.'

Keywords : 'power law, bitcoin, crypto, currency, index'

See also : 

Author : Guo Li

Submitted :

Datafile : Aj.csv, j=2, 3, ..., 10; Auroracoin.csv

Example : >
```


```R
<# please download the following package
libraries = c("lubridate", "poweRlaw","igraph","tables","texreg")
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Part 2: for figure 3,6,8 $ Table 2

setwd("~/Desktop/Data")
# pre-allocating for caculation of Power Law parameters
alpha.2           = rep(NA, 9)
Xmin.2            = rep(NA, 9)
KS.2              = rep(NA, 9)
P.2               = rep(NA, 9)
# Calculation loop for wealth distribution (using Power Law)
for (i in 1:length(Xmin.2)) {
  print(i + 1)
  Data.PL         = read.csv(paste("A",(i + 1), ".csv", sep = ""), header = T)[, 1]
  m               = conpl$new(Data.PL)
  est             = estimate_xmin(m)  # Estimate xmin and pars
  m$setXmin(est)
  Xmin.2[i]       = est$xmin
  alpha.2[i]      = est$pars
  KS.2[i]         = est$KS
  fit             = power.law.fit(Data.PL, 
                                  xmin                = Xmin.2[i], 
                                  start               = 2, 
                                  force.continuous    = FALSE, 
                                  implementation      = c("plfit"))
  P.2[i]          = fit$KS.p
}



>
```
