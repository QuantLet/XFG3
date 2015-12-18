# please download the following package
libraries = c("lubridate", "poweRlaw","igraph","tables","texreg")
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


# Part 3: Table 3
setwd("~/Desktop/Data")
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


