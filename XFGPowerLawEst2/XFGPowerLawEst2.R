# please download the following package
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


