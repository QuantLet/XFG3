setwd("~/Desktop/Data/Table 1")
source("PowerLawEst1.R")
# sample period
startDate                                  = ymd(20081231)
Date                                       = startDate %m+% months(c(1:(length(alpha))))
# Draw Figure 7: Bitcoin goodness of fit (whole sample)
plot(Date[1:length(Date)], P, 
     type                                     = "l", 
     ylab                                     = c("Goodness of Fit"), 
     xlab                                     = c("Date"), 
     main                                     = "Goodness of Fit of Bitcoin using Whole Sample")



#Goodness of Fit of right tail wealth distribution of Auroracoin
#setwd("~/Desktop/Data/Table 2")
setwd("C:/Users/liguo.2014/Dropbox/Data/Table 2")
source("PowerLawEst2.R")
# sample period
startDate                                  = ymd(20140228)
Date                                       = startDate %m+% months(c(1:(length(alpha.2))))
# Draw Figure 8: Auroracoin goodness of fit (right tail)
plot(Date[1:length(Date)], P.2, 
     type                                     = "l", 
     ylab                                     = c("Goodness of Fit"), 
     xlab                                     = c("Date"), 
     main                                     = "Goodness of Fit of Bitcoin using Whole Sample")


#Goodness of Fit of right tail wealth distribution of Bitcoin
setwd("C:/Users/liguo.2014/Dropbox/Data/Table 1")
#setwd("~/Desktop/Data/Table 1")
source("PowerLawEst3.R")
# sample period
startDate                                  = ymd(20081231)
Date                                       = startDate %m+% months(c(1:(length(alpha))))
# Draw Figure 10: Bitcoin goodness of fit (Right Tail)
plot(Date[1:60], P, 
     type                                     = "l", 
     ylab                                     = c("Goodness of Fit"), 
     xlab                                     = c("Date"), 
     main                                     = "Goodness of Fit of Bitcoin (Right Tail)")
