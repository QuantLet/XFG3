source(PowerLawEst1)
# sample period
startDate                                  = ymd(20081231)
Date                                       = startDate %m+% months(c(1:(length(alpha))))
# store the Power Law estimation results in 'PL'
PL                                         = cbind(as.Date(Date), alpha, Xmin, KS, P)
# load fundamental variables
TVs                                        = as.matrix(read.csv("Bitcoin_Data1.csv", header = T))
TVs                                        = TVs[match("3/1/2009", TVs[, 1]):match("31/12/2013", TVs[, 1]), ]
startDate                                  = ymd(20090103)
endDate                                    = ymd(20131231)
dmy                                        = seq.Date(as.Date(startDate), as.Date(endDate), by = "1 day")
Year                                       = year(dmy)
Mon                                        = month(dmy)
ym                                         = as.numeric(paste(Mon, Year, sep = "."))
TVs[, 1]                                   = ym
# convert daily data to monthly data
Duni                                       = unique(TVs[, 1])
M.TVs                                      = numeric()
for (i in 1:length(Duni)) {
  m                                        = apply(matrix(as.numeric(as.matrix(TVs[(TVs[, 1] == Duni[i]), ])[, -c(1:2)]), ncol = (ncol(TVs) - 2)), 2, sum)
  m                                        = matrix(m, nrow = 1)
  M.TVs                                    = rbind(M.TVs, m[1, ])
}

# store Power Law parameters with Transaction Variables in PL.C
PL.C                                       = cbind(PL, M.TVs)
PL.s                                       = PL.C
# remove the date column
PL.s                                       = PL.s[, -1]
colnames(PL.s)[5:length(colnames(PL.s))]   = colnames(TVs)[-c(1:2)]
PL.s                                       = as.matrix(PL.s)
D.Alpha                                    = diff(as.numeric(PL.s[, 1]))
# run linear regression days.destroyed, 8
Days.destroyed.alpha.fit                   = lm(diff(as.numeric(PL.s[, 8])) ~ D.Alpha)
Days.destroyed.alpha.table                 = summary(Days.destroyed.alpha.fit)
# MB.1
MB..alpha.fit                              = lm(diff(as.numeric(PL.s[, 9])) ~ D.Alpha)
MB..alpha.table                            = summary(MB..alpha.fit)
# difficulty, 11
Difficulty.alpha.fit                       = lm(diff(as.numeric(PL.s[, 12])) ~ D.Alpha)
Difficulty.alpha.table                     = summary(Difficulty.alpha.fit)
# Hashrate 14
Hashrate.alpha.fit                         = lm(diff(as.numeric(PL.s[, 15])) ~ D.Alpha)
Hashrate.alpha.table                       = summary(Hashrate.alpha.fit)
# Market.cap.usd 15
Market.cap.alpha.fit                       = lm(diff(as.numeric(PL.s[, 16])) ~ D.Alpha)
Market.cap.alpha.table                     = summary(Market.cap.alpha.fit)
# price.usd 16
Market.price.alpha.fit                     = lm(diff(as.numeric(PL.s[, 17])) ~ D.Alpha)
Market.price.alpha.table                   = summary(Market.price.alpha.fit)
# miners.Revenue 17
Miners.Revenue.alpha.fit                   = lm(diff(as.numeric(PL.s[, 18])) ~ D.Alpha)
Miners.Revenue.alpha.table                 = summary(Miners.Revenue.alpha.fit)
# network.deficit.usd 18
Network.Deficit.alpha.fit                  = lm(diff(as.numeric(PL.s[, 19])) ~ D.Alpha)
Network.Deficit.alpha.table                = summary(Network.Deficit.alpha.fit)
# No. of Transactions 26
No.Transaction.alpha.fit                   = lm(diff(as.numeric(PL.s[, 26])) ~ D.Alpha)
No.Transaction.alpha.table                 = summary(No.Transaction.alpha.fit)
# Ratio 27
Ratio.alpha.fit                            = lm(as.numeric(PL.s[-1, 29]) ~ D.Alpha)
Ratio.alpha.table                          = summary(Ratio.alpha.fit)
# Draw Table 1
texreg(list(Days.destroyed.alpha.fit, 
            MB..alpha.fit, 
            Difficulty.alpha.fit, 
            Hashrate.alpha.fit, 
            Market.cap.alpha.fit, 
            Market.price.alpha.fit, 
            Miners.Revenue.alpha.fit, 
            Network.Deficit.alpha.fit, 
            No.Transaction.alpha.fit, 
            Ratio.alpha.fit), 
       stars                                    = c(0.01, 0.05, 0.1), 
       caption.above                            = TRUE, 
       caption                                  = "Estimation Results for Bitcoin and Auroracoin", 
       dcolumn                                  = FALSE, 
       custom.model.names                       = c("Days Destroyed", 
                                                    "MB.1", 
                                                    "Difficulty", 
                                                    "Hashrate", 
                                                    "Market Cap", 
                                                    "Market Price", 
                                                    "Miners Revenue", 
                                                    "Network Deficit", 
                                                    "No. of Transactions", 
                                                    "Ratio"), 
       override.se                              = list(Days.destroyed.alpha.table$coef[, 2], 
                                                       MB..alpha.table$coef[, 2], 
                                                       Difficulty.alpha.table$coef[, 2], 
                                                       Hashrate.alpha.table$coef[, 2], 
                                                       Market.cap.alpha.table$coef[, 2], 
                                                       Market.price.alpha.table$coef[, 2], 
                                                       Miners.Revenue.alpha.table$coef[, 2], 
                                                       Network.Deficit.alpha.table$coef[, 2], 
                                                       No.Transaction.alpha.table$coef[, 2], 
                                                       Ratio.alpha.table$coef[, 2]), 
       override.pval                            = list(Days.destroyed.alpha.table$coef[, 4], 
                                                       MB..alpha.table$coef[, 4], 
                                                       Difficulty.alpha.table$coef[, 4], 
                                                       Hashrate.alpha.table$coef[, 4], 
                                                       Market.cap.alpha.table$coef[, 4], 
                                                       Market.price.alpha.table$coef[, 4], 
                                                       Miners.Revenue.alpha.table$coef[, 4], 
                                                       Network.Deficit.alpha.table$coef[, 4], 
                                                       No.Transaction.alpha.table$coef[, 4], 
                                                       Ratio.alpha.table$coef[, 4]))

source(PowerLawEst2)
# sample period
startDate                                  = ymd(20140228)
Date                                       = startDate %m+% months(c(1:(length(alpha.2))))
# using difference data
D.Alpha                                    = diff(alpha.2)
D.P                                        = diff(P.2)
# read transaction data
Data                                       = read.csv("Auroracoin.csv",header=T)
# Regression days.destroyed,
days.destroyed.alpha.fita                  = lm(diff(as.numeric(Data[, 8])) ~ D.Alpha)
days.destroyed.alpha.tablea                = summary(days.destroyed.alpha.fita)
# cost.transaction,
cost.transaction.alpha.fita                = lm(diff(as.numeric(Data[, 6])) ~ D.Alpha)
cost.transaction.alpha.tablea              = summary(cost.transaction.alpha.fita)
# difficulty,
difficulty.alpha.fita                      = lm(diff(as.numeric(Data[, 3])) ~ D.Alpha)
difficulty.alpha.tablea                    = summary(difficulty.alpha.fita)
# Transaction.volume,usd 12
TransactionVolumeu.alpha.fita              = lm(diff(as.numeric(Data[, 5])) ~ D.Alpha)
TransactionVolumeu.alpha.tablea            = summary(TransactionVolumeu.alpha.fita)
# Price
Price.alpha.fita                           = lm(diff(as.numeric(Data[, 2])) ~ D.Alpha)
Price.alpha.tablea                         = summary(Price.alpha.fita)
# No.of.Transactions
No.of.Transactions.alpha.fita              = lm(diff(as.numeric(Data[, 4])) ~ D.Alpha)
No.of.Transactions.alpha.tablea            = summary(No.of.Transactions.alpha.fita)
# Transaction.fees
Transaction.fees.alpha.fita                = lm(diff(as.numeric(Data[, 7])) ~ D.Alpha)
Transaction.fees.alpha.tablea              = summary(Transaction.fees.alpha.fita)


# Draw Table 2
texreg(list(days.destroyed.alpha.fita, 
            cost.transaction.alpha.fita, 
            difficulty.alpha.fita, 
            TransactionVolumeu.alpha.fita, 
            Price.alpha.fita, 
            No.of.Transactions.alpha.fita, 
            Transaction.fees.alpha.fita), 
       stars                                    = c(0.01, 0.05, 0.1), 
       caption.above                            = TRUE, 
       caption                                  = "Estimation Results for Bitcoin and Auroracoin", 
       dcolumn                                  = FALSE, 
       custom.model.names                       = c("Days Destroyed", 
                                                    "Cost.transaction", 
                                                    "Difficulty", 
                                                    "TransactionVolumeu", 
                                                    "Market Price", 
                                                    "No. of Transactions", 
                                                    "Transaction.fees"), 
       override.se                              = list(days.destroyed.alpha.tablea$coef[, 2], 
                                                       cost.transaction.alpha.tablea$coef[, 2], 
                                                       difficulty.alpha.tablea$coef[, 2], 
                                                       TransactionVolumeu.alpha.tablea$coef[, 2], 
                                                       Price.alpha.tablea$coef[, 2], 
                                                       No.of.Transactions.alpha.tablea$coef[, 2], 
                                                       Transaction.fees.alpha.tablea$coef[, 2]), 
       override.pval                            = list(days.destroyed.alpha.tablea$coef[, 4], 
                                                       cost.transaction.alpha.tablea$coef[, 4], 
                                                       difficulty.alpha.tablea$coef[, 4], 
                                                       TransactionVolumeu.alpha.tablea$coef[, 4], 
                                                       Price.alpha.tablea$coef[, 4], 
                                                       No.of.Transactions.alpha.tablea$coef[, 4], 
                                                       Transaction.fees.alpha.tablea$coef[, 4]))

source(PowerLawEst3)
# sample period
startDate                                  = ymd(20081231)
Date                                       = startDate %m+% months(c(1:(length(alpha))))
### store the results in 'PL'
PL                                         = cbind(as.Date(Date), alpha, Xmin, KS, P)
# combine the transaction data with the power law estimation results
PL.C                                       = cbind(PL, M.TVs)
PL.s                                       = PL.C
# remove the data column
PL.s                                       = PL.s[, -1]
colnames(PL.s)[5:length(colnames(PL.s))]   = colnames(TVs)[-c(1:2)]
PL.s                                       = as.matrix(PL.s)
# store PL.s in PL.S
PL.S                                       = PL.s
# Using periods when powerlaw is well fitted
PL.s                                       = PL.S[(P > 0.1), ]
D.Alpha                                    = diff(as.numeric(PL.s[, 1]))
# run linear regression for table 3 days.destroyed, 8
Days.destroyed.alpha.fit                   = lm(diff(as.numeric(PL.s[, 8])) ~ D.Alpha)
Days.destroyed.alpha.table                 = summary(Days.destroyed.alpha.fit)
# MB.1, 9
MB..alpha.fit                              = lm(diff(as.numeric(PL.s[, 9])) ~ D.Alpha)
MB..alpha.table                            = summary(MB..alpha.fit)
# difficulty, 11
Difficulty.alpha.fit                       = lm(diff(as.numeric(PL.s[, 12])) ~ D.Alpha)
Difficulty.alpha.table                     = summary(Difficulty.alpha.fit)
# Hashrate 14
Hashrate.alpha.fit                         = lm(diff(as.numeric(PL.s[, 15])) ~ D.Alpha)
Hashrate.alpha.table                       = summary(Hashrate.alpha.fit)
# Market.cap.usd 15
Market.cap.alpha.fit                       = lm(diff(as.numeric(PL.s[, 16])) ~ D.Alpha)
Market.cap.alpha.table                     = summary(Market.cap.alpha.fit)
# price.usd 16
Market.price.alpha.fit                     = lm(diff(as.numeric(PL.s[, 17])) ~ D.Alpha)
Market.price.alpha.table                   = summary(Market.price.alpha.fit)
# miners.Revenue 17
Miners.Revenue.alpha.fit                   = lm(diff(as.numeric(PL.s[, 18])) ~ D.Alpha)
Miners.Revenue.alpha.table                 = summary(Miners.Revenue.alpha.fit)
# network.deficit.usd 18
Network.Deficit.alpha.fit                  = lm(diff(as.numeric(PL.s[, 19])) ~ D.Alpha)
Network.Deficit.alpha.table                = summary(Network.Deficit.alpha.fit)
# No. of Transactions 26
No.Transaction.alpha.fit                   = lm(diff(as.numeric(PL.s[, 26])) ~ D.Alpha)
No.Transaction.alpha.table                 = summary(No.Transaction.alpha.fit)
# Ratio 27
Ratio.alpha.fit                            = lm(as.numeric(PL.s[-1, 29]) ~ D.Alpha)
Ratio.alpha.table                          = summary(Ratio.alpha.fit)



# Draw Table 3
texreg(list(Days.destroyed.alpha.fit, 
            MB..alpha.fit, 
            Difficulty.alpha.fit, 
            Hashrate.alpha.fit, 
            Market.cap.alpha.fit, 
            Market.price.alpha.fit, 
            Miners.Revenue.alpha.fit, 
            Network.Deficit.alpha.fit, 
            No.Transaction.alpha.fit, 
            Ratio.alpha.fit), 
       stars                                   = c(0.01, 0.05, 0.1), 
       caption.above                           = TRUE, 
       caption                                 = "Estimation Results for Bitcoin and Auroracoin", 
       dcolumn                                 = FALSE, 
       custom.model.names                      = c("Days Destroyed", 
                                                   "MB.1", 
                                                   "Difficulty", 
                                                   "Hashrate", 
                                                   "Market Cap", 
                                                   "Market Price", 
                                                   "Miners Revenue", 
                                                   "Network Deficit", 
                                                   "No. of Transactions", 
                                                   "Ratio"), 
       override.se                             = list(Days.destroyed.alpha.table$coef[, 2], 
                                                      MB..alpha.table$coef[, 2], 
                                                      Difficulty.alpha.table$coef[, 2], 
                                                      Hashrate.alpha.table$coef[, 2], 
                                                      Market.cap.alpha.table$coef[, 2], 
                                                      Market.price.alpha.table$coef[, 2], 
                                                      Miners.Revenue.alpha.table$coef[, 2], 
                                                      Network.Deficit.alpha.table$coef[, 2], 
                                                      No.Transaction.alpha.table$coef[, 2], 
                                                      Ratio.alpha.table$coef[, 2]), 
       override.pval                           = list(Days.destroyed.alpha.table$coef[, 4], 
                                                      MB..alpha.table$coef[, 4], 
                                                      Difficulty.alpha.table$coef[, 4], 
                                                      Hashrate.alpha.table$coef[, 4], 
                                                      Market.cap.alpha.table$coef[, 4], 
                                                      Market.price.alpha.table$coef[, 4], 
                                                      Miners.Revenue.alpha.table$coef[, 4], 
                                                      Network.Deficit.alpha.table$coef[, 4], 
                                                      No.Transaction.alpha.table$coef[, 4], 
                                                      Ratio.alpha.table$coef[, 4]))

# Part 4: Table 4 Using periods when powerlaw is well fitted
PL.s                                       = PL.S[(P > 0.1), ]  # choosing power law periods
P.value                                    = as.numeric(PL.s[-1, 4])
# days.destroyed, 8
Days.destroyed.P.value.fit                 = lm(diff(as.numeric(PL.s[, 8])) ~ P.value)
Days.destroyed.P.value.table               = summary(Days.destroyed.P.value.fit)
# MB.1
MB..P.value.fit                            = lm(diff(as.numeric(PL.s[, 9])) ~ P.value)
MB..P.value.table                          = summary(MB..P.value.fit)
# difficulty, 11
Difficulty.P.value.fit                     = lm(diff(as.numeric(PL.s[, 12])) ~ P.value)
Difficulty.P.value.table                   = summary(Difficulty.P.value.fit)
# Hashrate 14
Hashrate.P.value.fit                       = lm(diff(as.numeric(PL.s[, 15])) ~ P.value)
Hashrate.P.value.table                     = summary(Hashrate.P.value.fit)
# Market.cap.usd 15
Market.cap.P.value.fit                     = lm(diff(as.numeric(PL.s[, 16])) ~ P.value)
Market.cap.P.value.table                   = summary(Market.cap.P.value.fit)
# price.usd 16
Market.price.P.value.fit                   = lm(diff(as.numeric(PL.s[, 17])) ~ P.value)
Market.price.P.value.table                 = summary(Market.price.P.value.fit)
# miners.Revenue 17
Miners.Revenue.P.value.fit                 = lm(diff(as.numeric(PL.s[, 18])) ~ P.value)
Miners.Revenue.P.value.table               = summary(Miners.Revenue.P.value.fit)
# network.deficit.usd 18
Network.Deficit.P.value.fit                = lm(diff(as.numeric(PL.s[, 19])) ~ P.value)
Network.Deficit.P.value.table              = summary(Network.Deficit.P.value.fit)
# No. of Transactions 26
No.Transaction.P.value.fit                 = lm(diff(as.numeric(PL.s[, 26])) ~ P.value)
No.Transaction.P.value.table               = summary(No.Transaction.P.value.fit)
# Ratio 27
Ratio.P.value.fit                          = lm(as.numeric(PL.s[-1, 29]) ~ P.value)
Ratio.P.value.table                        = summary(Ratio.P.value.fit)

# Draw Table 4
texreg(list(Days.destroyed.P.value.fit, 
            MB..P.value.fit, 
            Difficulty.P.value.fit, 
            Hashrate.P.value.fit, 
            Market.cap.P.value.fit, 
            Market.price.P.value.fit, 
            Miners.Revenue.P.value.fit, 
            Network.Deficit.P.value.fit, 
            No.Transaction.P.value.fit, 
            Ratio.P.value.fit), 
       stars                                    = c(0.01, 0.05, 0.1), 
       caption.above                            = TRUE, 
       caption                                  = "Estimation Results for Bitcoin and Auroracoin", 
       dcolumn                                  = FALSE, 
       custom.model.names                       = c("Days Destroyed", 
                                                    "MB.1", 
                                                    "Difficulty", 
                                                    "Hashrate", 
                                                    "Market Cap", 
                                                    "Market Price", 
                                                    "Miners Revenue", 
                                                    "Network Deficit", 
                                                    "No. of Transactions", 
                                                    "Ratio"), 
       override.se                              = list(Days.destroyed.P.value.table$coef[, 2], 
                                                       MB..P.value.table$coef[, 2], 
                                                       Difficulty.P.value.table$coef[, 2], 
                                                       Hashrate.P.value.table$coef[, 2], 
                                                       Market.cap.P.value.table$coef[, 2], 
                                                       Market.price.P.value.table$coef[, 2], 
                                                       Miners.Revenue.P.value.table$coef[, 2], 
                                                       Network.Deficit.P.value.table$coef[, 2], 
                                                       No.Transaction.P.value.table$coef[, 2], 
                                                       Ratio.P.value.table$coef[, 2]), 
       override.pval                            = list(Days.destroyed.P.value.table$coef[, 4], 
                                                       MB..P.value.table$coef[, 4], 
                                                       Difficulty.P.value.table$coef[, 4], 
                                                       Hashrate.P.value.table$coef[, 4], 
                                                       Market.cap.P.value.table$coef[, 4], 
                                                       Market.price.P.value.table$coef[, 4], 
                                                       Miners.Revenue.P.value.table$coef[, 4], 
                                                       Network.Deficit.P.value.table$coef[, 4], 
                                                       No.Transaction.P.value.table$coef[, 4], 
                                                       Ratio.P.value.table$coef[, 4]))


# Part 5: Table 5 & 6 Using whole periods
PL.s                                       = PL.S
Ps                                         = as.numeric((as.numeric(PL.s[-1, 4]) > 0.1))  
P.value                                    = as.numeric(PL.s[-1, 4])
# run linear regression for table 5 & 6 days.destroyed, 8
Days.destroyed.Ps.fit                      = lm(diff(as.numeric(PL.s[, 8])) ~ Ps)
Days.destroyed.Ps.table                    = summary(Days.destroyed.Ps.fit)
Days.destroyed.P.value.fit                 = lm(diff(as.numeric(PL.s[, 8])) ~ P.value)
Days.destroyed.P.value.table               = summary(Days.destroyed.P.value.fit)
# MB.1
MB..Ps.fit                                 = lm(diff(as.numeric(PL.s[, 9])) ~ Ps)
MB..Ps.table                               = summary(MB..Ps.fit)
MB..P.value.fit                            = lm(diff(as.numeric(PL.s[, 9])) ~ P.value)
MB..P.value.table                          = summary(MB..P.value.fit)
# difficulty, 11
Difficulty.Ps.fit                          = lm(diff(as.numeric(PL.s[, 12])) ~ Ps)
Difficulty.Ps.table                        = summary(Difficulty.Ps.fit)
Difficulty.P.value.fit                     = lm(diff(as.numeric(PL.s[, 12])) ~ P.value)
Difficulty.P.value.table                   = summary(Difficulty.P.value.fit)
# Hashrate 14
Hashrate.Ps.fit                            = lm(diff(as.numeric(PL.s[, 15])) ~ Ps)
Hashrate.Ps.table                          = summary(Hashrate.Ps.fit)
Hashrate.P.value.fit                       = lm(diff(as.numeric(PL.s[, 15])) ~ P.value)
Hashrate.P.value.table                     = summary(Hashrate.P.value.fit)
# Market.cap.usd 15
Market.cap.Ps.fit                          = lm(diff(as.numeric(PL.s[, 16])) ~ Ps)
Market.cap.Ps.table                        = summary(Market.cap.Ps.fit)
Market.cap.P.value.fit                     = lm(diff(as.numeric(PL.s[, 16])) ~ P.value)
Market.cap.P.value.table                   = summary(Market.cap.P.value.fit)
# price.usd 16
Market.price.Ps.fit                        = lm(diff(as.numeric(PL.s[, 17])) ~ Ps)
Market.price.Ps.table                      = summary(Market.price.Ps.fit)
Market.price.P.value.fit                   = lm(diff(as.numeric(PL.s[, 17])) ~ P.value)
Market.price.P.value.table                 = summary(Market.price.P.value.fit)
# miners.Revenue 17
Miners.Revenue.Ps.fit                      = lm(diff(as.numeric(PL.s[, 18])) ~ Ps)
Miners.Revenue.Ps.table                    = summary(Miners.Revenue.Ps.fit)
Miners.Revenue.P.value.fit                 = lm(diff(as.numeric(PL.s[, 18])) ~ P.value)
Miners.Revenue.P.value.table               = summary(Miners.Revenue.P.value.fit)
# network.deficit.usd 18
Network.Deficit.Ps.fit                     = lm(diff(as.numeric(PL.s[, 19])) ~ Ps)
Network.Deficit.Ps.table                   = summary(Network.Deficit.Ps.fit)
Network.Deficit.P.value.fit                = lm(diff(as.numeric(PL.s[, 19])) ~ P.value)
Network.Deficit.P.value.table              = summary(Network.Deficit.P.value.fit)
# No. of Transactions 26
No.Transaction.Ps.fit                      = lm(diff(as.numeric(PL.s[, 26])) ~ Ps)
No.Transaction.Ps.table                    = summary(No.Transaction.Ps.fit)
No.Transaction.P.value.fit                 = lm(diff(as.numeric(PL.s[, 26])) ~ P.value)
No.Transaction.P.value.table               = summary(No.Transaction.P.value.fit)
# Ratio 27
Ratio.Ps.fit                               = lm(as.numeric(PL.s[-1, 29]) ~ Ps)
Ratio.Ps.table                             = summary(Ratio.Ps.fit)
Ratio.P.value.fit                          = lm(as.numeric(PL.s[-1, 29]) ~ P.value)
Ratio.P.value.table                        = summary(Ratio.P.value.fit)


# Draw Table 5
texreg(list(Days.destroyed.Ps.fit, 
            MB..Ps.fit, 
            Difficulty.Ps.fit, 
            Hashrate.Ps.fit, 
            Market.cap.Ps.fit, 
            Market.price.Ps.fit, 
            Miners.Revenue.Ps.fit, 
            Network.Deficit.Ps.fit, 
            No.Transaction.Ps.fit, 
            Ratio.Ps.fit), 
       stars                                    = c(0.01, 0.05, 0.1), 
       caption.above                            = TRUE, 
       caption                                  = "Estimation Results for Bitcoin and Auroracoin", 
       dcolumn                                  = FALSE, 
       custom.model.names                       = c("Days Destroyed", 
                                                    "MB.1", 
                                                    "Difficulty", 
                                                    "Hashrate", 
                                                    "Market Cap", 
                                                    "Market Price", 
                                                    "Miners Revenue", 
                                                    "Network Deficit", 
                                                    "No. of Transactions", 
                                                    "Ratio"), 
       override.se                              = list(Days.destroyed.Ps.table$coef[, 2], 
                                                       MB..Ps.table$coef[, 2], 
                                                       Difficulty.Ps.table$coef[, 2], 
                                                       Hashrate.Ps.table$coef[, 2], 
                                                       Market.cap.Ps.table$coef[, 2], 
                                                       Market.price.Ps.table$coef[, 2], 
                                                       Miners.Revenue.Ps.table$coef[, 2], 
                                                       Network.Deficit.Ps.table$coef[, 2], 
                                                       No.Transaction.Ps.table$coef[, 2], 
                                                       Ratio.Ps.table$coef[, 2]), 
       override.pval                            = list(Days.destroyed.Ps.table$coef[, 4], 
                                                       MB..Ps.table$coef[, 4], 
                                                       Difficulty.Ps.table$coef[, 4], 
                                                       Hashrate.Ps.table$coef[, 4], 
                                                       Market.cap.Ps.table$coef[, 4], 
                                                       Market.price.Ps.table$coef[, 4], 
                                                       Miners.Revenue.Ps.table$coef[, 4], 
                                                       Network.Deficit.Ps.table$coef[, 4], 
                                                       No.Transaction.Ps.table$coef[, 4], 
                                                       Ratio.Ps.table$coef[, 4]))

# Draw Table 6
texreg(list(Days.destroyed.P.value.fit, 
            MB..P.value.fit, 
            Difficulty.P.value.fit, 
            Hashrate.P.value.fit, 
            Market.cap.P.value.fit, 
            Market.price.P.value.fit, 
            Miners.Revenue.P.value.fit, 
            Network.Deficit.P.value.fit, 
            No.Transaction.P.value.fit, 
            Ratio.P.value.fit), 
       stars                                    = c(0.01, 0.05, 0.1), 
       caption.above                            = TRUE, 
       caption                                  = "Estimation Results for Bitcoin and Auroracoin", 
       dcolumn                                  = FALSE, 
       custom.model.names                       = c("Days Destroyed", 
                                                    "MB.1", 
                                                    "Difficulty", 
                                                    "Hashrate", 
                                                    "Market Cap", 
                                                    "Market Price", 
                                                    "Miners Revenue", 
                                                    "Network Deficit", 
                                                    "No. of Transactions", 
                                                    "Ratio"), 
       override.se                              = list(Days.destroyed.P.value.table$coef[, 2], 
                                                       MB..P.value.table$coef[, 2], 
                                                       Difficulty.P.value.table$coef[, 2], 
                                                       Hashrate.P.value.table$coef[, 2], 
                                                       Market.cap.P.value.table$coef[, 2], 
                                                       Market.price.P.value.table$coef[, 2], 
                                                       Miners.Revenue.P.value.table$coef[, 2], 
                                                       Network.Deficit.P.value.table$coef[, 2], 
                                                       No.Transaction.P.value.table$coef[, 2], 
                                                       Ratio.P.value.table$coef[, 2]), 
       override.pval                            = list(Days.destroyed.P.value.table$coef[, 4], 
                                                       MB..P.value.table$coef[, 4], 
                                                       Difficulty.P.value.table$coef[, 4], 
                                                       Hashrate.P.value.table$coef[, 4], 
                                                       Market.cap.P.value.table$coef[, 4], 
                                                       Market.price.P.value.table$coef[, 4], 
                                                       Miners.Revenue.P.value.table$coef[, 4], 
                                                       Network.Deficit.P.value.table$coef[, 4], 
                                                       No.Transaction.P.value.table$coef[, 4], 
                                                       Ratio.P.value.table$coef[, 4]))






