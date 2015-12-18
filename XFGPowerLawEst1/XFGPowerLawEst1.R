# please download the following package
libraries = c("lubridate", "poweRlaw","igraph","tables","texreg")
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Part1: for figure 2,5,7 & Table 1 specify the path to the desired folder,
# please set your own directory, e.g (C:/Users/Desktop/...)
#setwd("~/Desktop/Data")
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




