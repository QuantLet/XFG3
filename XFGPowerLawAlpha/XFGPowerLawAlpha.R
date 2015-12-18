setwd("~/Desktop/Data")
source("PowerLawEst1.R")
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






