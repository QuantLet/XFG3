rm(list=ls(all=TRUE))
graphics.off()

install.packages("locpol")
library(locpol)

# Specify your working directory
# setwd("Users...")

############################ SUBROUTINE ################################
 
spdbl = function(m, sigma, sigma1, sigma2, s, r, tau){ # (fstar,delta, gamma)
    # spdbl uses the Breeden and Litzenberger (1978) method and a
    #    semiparametric specification of the Black-Scholes
    #    option pricing function to calculate the empirical State 
    #    Price Density. The analytic formula uses an estimate of 
    #    the volatility smile and its first and second derivative to
    #    calculate the State-price density, as well as Delta and 
    #    Gamma of the option. This method can only be applied to
    #    European options (due to the assumptions).   

    rm   = length(m)
    ones = matrix(1,rm,1)
    st   = sqrt(tau)
    ert  = exp(r*tau)
    rt   = r*tau
  
    # Modified Black-Scholes scaled by S-div instead of F
    d1   = (log(m)+tau*(r+0.5*(sigma^2)))/(sigma*st)
    d2   = d1-sigma*st
    f    = pnorm(d1)-pnorm(d2)/(ert*m)

    # first derivative of d1 term
    d11  = (1/(m*sigma*st))-(1/(st*(sigma^2)))*((log(m)+tau*r)*sigma1)+0.5*st*sigma1

    # first derivative of d2 term
    d21  = d11-st*sigma1

    # second derivative of d1 term
    d12  = -(1/(st*(m^2)*sigma))-sigma1/(st*m*(sigma^2))+sigma2*(0.5*st-(log(m)+rt)/(st*(sigma^2)))+sigma1*(2*sigma1*(log(m)+rt)/(st*sigma^3)-1/(st*m*sigma^2))

    # second derivative of d2 term
    d22  = d12-st*sigma2

    # Please refer to either Rookley (1997) or the XploRe Finance Guide for derivations
    f1   = dnorm(d1)*d11+(1/ert)*((-dnorm(d2)*d21)/m+pnorm(d2)/(m^2))
    f2   = dnorm(d1)*d12-d1*dnorm(d1)*(d11^2)-(1/(ert*m)*dnorm(d2)*d22)+ ((dnorm(d2)*d21)/(ert*m^2))+(1/(ert*m)*d2*dnorm(d2)*(d21^2))-(2*pnorm(d2)/(ert*(m^3)))+(1/(ert*(m^2))*dnorm(d2)*d21) 

    #recover strike price
    x    = s/m  
    c1   = -(m^2)*f1
    c2   = s*((1/x^2)*((m^2)*f2+2*m*f1))

    #calculate the quantities of interest
    cdf   = ert*c1+1
    fstar = ert*c2
    delta = f + s* f1/x
    gamma = 2*f1/x+s*f2/(x^2)

    return(list(fstar=fstar,delta=delta, gamma=gamma))
}

# BlackScholes price for a European Call or Put Option 
BlackScholes = function(S, K, r, sigma, tau, task){
    S     = c(S)        # spot price
    K     = c(K)        # exercise price
    tau   = c(tau)      # time to maturity
    sigma = c(sigma)    # volatility, std error
    r     = c(r)        # interest rate  
    if(task == "call" || task == "Call" || task == "CALL"){
        task = 1     # call option
    }else{
        if(task == "put" || task == "Put" || task == "PUT"){
            task = 0     # put option
        }
    }  
    if((min(S)<=0)){
        stop("BlackScholes: price needs to be larger than 0")
    }
    if((min(K)<=0)){
        stop("BlackScholes: strike price needs to be larger than 0")
    }
    if(((task!=0)&(task!=1))){
        stop("BlackScholes: task needs to be either 1 or 0")
    }
    if(((r<=0)|(r>=1))){
        stop("BlackScholes: interest rate needs to be between 0 and 1")
    }
    if((min(r)<0)){
        stop("BlackScholes: interest rate can not be negative")
    }
    if(min(sigma)<=0){
        stop("BlackScholes: volatility needs to be larger than 0")	
    }  
    if(min(tau)<0){
        stop("BlackScholes: time to expiration can not be negative")	
    }

# Black-Scholes formula #
    t    = (tau==0) # check if it is the expire day   							
    y    = (log(S/K)+(r-sigma^2/2)*tau)/(sigma*sqrt(tau)+t)
    if (task==1){
        opv   = S*(pnorm(y+sigma*sqrt(tau))*(!t)+t)-K*exp(-r*tau)*(pnorm(y)*(!t)+t)
    } 
    if (task==0){
        opv   = K*exp(-r*tau)*(pnorm(-y)*(!t)+t)-S*(pnorm(-y-sigma*sqrt(tau))*(!t)+t)
    }
    opv  = (opv>0)*opv
    return(opv)	
}
 
# Function to find BS Implied Vol using Bisection Method
ImplVola = function(S, K, Time, r, market, type){
    sig      = 0.20     # start value
    sig.up   = 1        # upper bound
    sig.down = 0.001    # lower bound
    count    = 0        # iteration
    err      = BlackScholes(S, K, r, sig, Time, type) - market 
    # repeat until error is sufficiently small or counter hits 1000
    while(abs(err) > 0.00001 && count<1000){
        if(err < 0){
            sig.down  =  sig
            sig       =  (sig.up + sig)/2
        }else{
            sig.up  =  sig
            sig     =  (sig.down + sig)/2
        }
        err    =  BlackScholes(S, K, r, sig,Time, type) - market
        count  =  count + 1
    }
    # return NA if counter hit 1000
    if(count==1000){
        return(NA)
    }else{
        return(sig)
    }
}

############################ Main Computation ############################  

firstXF     = 0.8		
lastXF      = 1.21      # firstXF and lastXF define the range of the estimation for the moneyness dimension.
stepwidth   = 0.01      # 0.02 is the step between each estimated point in the moneyness dimension
mat         = 0.25
date9701    = c(6,7,8,9,10,13,14,15,16,17,20,21,22,23,24) #,27,28,29,30,31)
dataloaded  = read.table("XFGData9701.dat")
stepwidth   = c(stepwidth,mat)
k           = 0
i           = 1
while(i<=length(date9701)){
    data      = subset(dataloaded,(dataloaded[,1]==date9701[i]))
    date      = data[,1:3]
    
    # First column contains the day, 
    # second column contains the month
    # third column contains the year.
    
    type      = data[,4]  # 1 for calls 0 for puts
    maturity  = data[,5]  # Maturity in calendar days.
    K         = data[,6]  # K strike price
    OP        = data[,7]  # OP option price
    S         = data[,8]  # S is the underlying price corrected for future dividends
                          # Thus, S depends on the date and the maturity of the option.
    IR        = data[,9]  # Interest rate for the given maturity
    IVola     = data[,10] # Implied volatility of the function
    SpotPrice = data[,11] # It is the real spot price not corrected for future dividends!

    ###########################################################################################################
    #Here we compute the implied volatilities of the options by the Newton Raphson method
    #and estimate the surface with local polynomials using the quartic Kernel. 
    ###########################################################################################################

    data      = cbind(S,K,IR,maturity/365,OP,type)
    metric    = 0           # metric=0 specifies the spot price metric (if =1 then strike metric) 

    bandwidth = c(0.15,0.3) # 0.15 is the bandwidth used for the moneyness dimension.
                            # 0.3 is the bandwidth used for the maturity dimension. 
    firstMat  = 0
    lastMat   = 1           # firstMat and lastMat define the range of the estimation for the maturity dimension.
    Price     = data[,1]                                    # Spot price
    Strike    = data[,2]                                    # Strike price
    Rate      = data[,3]                                    # Risk-free interest rate
    Time      = data[,4]                                    # Time to maturity
    Value     = data[,5]                                    # Market value
    Class     = data[,6]                                    # Call==1 | Put==0
    mon       = data[,2]/(data[,1]*exp(data[,3]*data[,4]))  # Moneyness
    data      = cbind(data,mon)
    x         = data

    n         = length(x[,1])                   # number of observations

    # calculate implied volatility
    iv        = rep(0,n)
    for(j in 1:n){
        iv[j] = ImplVola(S=Price[j], K=Strike[j],Time=Time[j], r=Rate[j], market=Value[j],type=Class[j]);
    }

    imax      = ceiling((lastXF-firstXF)/stepwidth[1])
    jmax      = ceiling((lastMat-firstMat)/stepwidth[2])
    result    = matrix(1,imax*jmax,1)
    grid1     = seq(firstMat,lastMat,by=stepwidth[2]) # grid
    grid2     = seq(firstXF,lastXF,by=stepwidth[1]) # grid

    x12       = expand.grid(grid1,grid2)   # expand the grid
    MAT       = x12[[1]]
    MON       = x12[[2]]

    dataf     = cbind(iv, mon, Time)
    dataf     = subset(dataf,iv!="NA")

    lpfit     = locLinSmootherC(x=cbind(dataf[,2],dataf[,3]),y=cbind(dataf[,1],dataf[,1]), bw=bandwidth, xeval=cbind(MON,MAT), kernel=EpaK)

    IV        = lpfit[1:(nrow(lpfit)/2),3]
    IVSurface = cbind(c(MON),c(MAT),c(IV))

    ###########################################################################################################
    ###########################################################################################################

    ################################################################################################
    #Here, we compute (S-Div) and r for tau=0.25 by linear interpolation between the two series of
    #options around tau=0.25. In this example, we use the options with maturity tau=0.21096 and
    #0.46027.
    ################################################################################################
    
    temp1        = subset(cbind(data[,1],data[,3],data[,4]),data[,4]<=mat)
    temp2        = subset(cbind(data[,1],data[,3],data[,4]),data[,4]>mat)

    SandRMatinf  = subset(cbind(temp1[,1],temp1[,2],temp1[,3]),round(temp1[,3],8)==round(mat-min(abs(mat-temp1[,3])),8))
    SandRMatsup  = subset(cbind(temp2[,1],temp2[,2],temp2[,3]),round(temp2[,3],8)==round(mat+min(abs(mat-temp2[,3])),8))

    a            = (mean(SandRMatsup[,1])-mean(SandRMatinf[,1]))/(mean(SandRMatsup[,3])-mean(SandRMatinf[,3]))
    b            = mean(SandRMatinf[,1])
    x            = mat-mean(SandRMatinf[,3])
    sMat         = a*x+b

    a            = (mean(SandRMatsup[,2])-mean(SandRMatinf[,2]))/(mean(SandRMatsup[,3])-mean(SandRMatinf[,3]))
    b            = mean(SandRMatinf[,2])
    rMat         = a*x+b

    ##################################################################################################
    ##################################################################################################
    
    dataMatbis   = subset(IVSurface,IVSurface[,2]==mat)
    dataMat      = cbind(1/(dataMatbis[,1]*exp(rMat*mat)),dataMatbis[,2:3])

    hh           = 0.1            # bandwidth

    # estimation of the smile
    smileMat     = cbind(dataMat[,1],dataMat[,3])
    smileMat     = smileMat[order(smileMat[,1]),]
    sM           = data.frame(S=smileMat[,1],M=smileMat[,2])

    # estimation of the first derivative of the smile w.r. to the strike dimension.  
    dersmileMat  = locpol(M~S,data=sM,bw=hh,kernel=EpaK,deg=2,xevalLen=nrow(smileMat))$lpFit
  
    
    lpspd        = spdbl(m=smileMat[,1], sigma=smileMat[,2], sigma1=dersmileMat[,3], sigma2=dersmileMat[,4], s=mean(sMat), r=mean(rMat), tau=mat)

    moneyness    = sort(dataMatbis[,1],decreasing=T)
    spd1         = cbind(moneyness,(lpspd$fstar*sMat*exp(rMat*mat)))

    if(k==1){
        spdest   = rbind(spdest,cbind(matrix(i,nrow(spd1),1),spd1))
    }
    if (k==0){	
        spdest   = cbind(matrix(i,nrow(spd1),1),spd1)
        k        = k+1	
    }
    print(i)
    i            = i+1
}

spdest           = spdest[order(spdest[,2],spdest[,1]),]

##################################################################################################

#plot local polynomial SPD
#graph data for Local Polynomial

time             = matrix(spdest[,1],15,42)
money            = matrix(spdest[,2],15,42)
est              = matrix(spdest[,3],15,42)

require(lattice)
wireframe(est~time+money,ticktype="detailed",drape=T,colorkey=F,sub=paste("Local-Polynomial SPD: 01-1997, tau = ", mat), aspect = c(1,1),scales=list(arrows=FALSE, y=list(labels=round(seq(firstMat,lastMat,length=5),2)),x=list(labels=round(seq(firstXF,lastXF,length=8),2)),z=list(labels=round(seq(0,max(est),length=7),2))), ylab=list("days",rot=-8), xlab=list("ST/F",rot=25), zlab=list("SPD",rot=94) ,screen = list(z = 240, x = -70))

  

