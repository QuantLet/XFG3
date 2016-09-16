# clear cache and close windows
rm(list=ls(all=TRUE))
graphics.off()

# install packages and load libraries
# install.packages("locpol")
# install.packages("KernSmooth")
# install.packages("regpro")
library(locpol)
library(KernSmooth)
library(regpro)


############################ SUBROUTINE ################################

ibtspd = function(data, days){
    IVmethod  = "bisect"              # golden bisection method
    stepwidth = c(0.01,(1/100))       # stepwidth
    bandwidth = c(0.1, 0.2)           # bandwidth
    F         = data[,8]*exp(data[,9]*(data[,5]/365)) # forward pricing function
    firstXF   = min(data[,6]/F)-0.05  # lower bound moneyness
    lastXF    = max(data[,6]/F)+ 0.05 # upper bound moneyness    
    firstMat  = 0                     # lower bound time to maturity
    lastMat   = 0.5                   # upper bound time to maturity
    imax      = ceiling((lastXF-firstXF)/stepwidth[1])   # length grid moneyness
    jmax      = ceiling((lastMat-firstMat)/stepwidth[2]) # length grid time to maturity
    tmp       = expand.grid(seq(firstXF,by=stepwidth[1],length=imax),seq(firstMat,by=stepwidth,length=jmax))
    X1        = tmp[[1]]              # grid moneyness
    X2        = tmp[[2]]              # grid maturity
    IVsurf    = cbind(X1,X2)
    IVpoints  = cbind(data[,6]/F,(data[,5]/365),data[,10])
    IVSurface = pcf.kernesti(x=IVpoints[,1:2],y=IVpoints[,3], N=c(imax,jmax),h=bandwidth,kernel="gauss",support(IVsurf)) # NW
    IVS       = cbind(IVsurf,IVSurface$value)
    IVSurface = subset(IVS, is.nan(IVS[,3])==0) # set NAs zero
    firstXF   = min(IVSurface[,1])
    lastXF    = max(IVSurface[,1])
    IVSurface <<- IVSurface            # set global variable
    firstXF   <<- firstXF
    lastXF    <<- lastXF
    S         = subset(data,data[,5]==days)[1,8]
    r         = mean(subset(data,data[,5]==days)[,9])
    r         <<- r
    lev       = 20
    tau       = days/365
    ibtree    = IBTbc(S, r, tau, lev,'para')
    bcibdat   = cbind(ibtree$S[,lev+1],ibtree$AD[,lev+1]*exp(r*tau))
    return(bcibdat)
}

IBTimpliedvola = function(S,K,T,func) {
    if (func == 1) {    # Interpolated for the real data, see XFGIBT05: EurexVolatilities_rawData19
        	            # not working
		            # add interpolation package, 2-dimensional interpolation needed
    }else{    # Parabola   
        X = S/K          
        iv = (-0.2/(log(X)^2+1))+0.3             # M.Fengler (3.78) page 82
    }
    return(iv)
}

IBTblackscholes <- function(S, X, rf, sigma,T,flag) {
    values <- c(2)
    d1 <- (log(S/X)+(rf+sigma^2/2)*T)/sigma*sqrt(T)
    d2 <- d1 - sigma * sqrt(T)
    if(flag==1){
        values[1] <- S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2)
    }else if(flag==2){
        values[2] <- X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1)
    }
    return(values)
}

IBTbc = function(s0, r, t, n, func) {
    if (s0<=0) {
        print('IBTbc: Price of Underlying Asset should be positive! Please input again')
        print('s0=')
        s0=scan() 
    }
    if ((r<0) || (r>1)) {
        print('IBTbc: Interest rate need to be between 0 and 1! Please input again')
        print('r=')
        r=scan()
    }
    if (t<=0) {
        print('IBTbc: Time to expiration should be positive! Please input again')
        print('t=')
        t=scan()
    }
    if (n<1) {
        print('IBTbc: Number of steps should be at least equal to 1! Please input again')
        print('n=')
        n=scan()
    }
    if (n>150)	{   # Constraint of n, otherwise it will take too much time
        print('IBTbc: Could you please choose a smaller n? Please input again')
        print('n=')
        n=scan()
    }
    dt         = t/n                     
    Smat       = matrix(0,n+1,n+1)    # Stock price at nodes
    Smat[1,1]  = s0                   # First node equals the underlying price
    ADmat      = matrix(0,n+1,n+1)    # Arrow-Debreu prices
    ADmat[1,1] = 1                    # First Arrow-Debreu price equals 1
    pmat       = matrix(0,n,n)        # Transition probabilites
    infl       = exp(r*dt)

    for (i in seq(1,n)) {
        
        #******   Step 1 : Find central nodes of stock tree *************************
        if ((i%%2)==0) {    #(i+1) is odd
            mi=(i/2+1)
            Smat[mi,i+1]=s0*exp(r*dt*i)    #center point price set to spot
            if ((Smat[mi, i+1]< infl*Smat[mi-1,i]) || (Smat[mi, i+1]>infl*Smat[mi,i])) {
                Smat[mi, i+1] = infl*(Smat[mi-1,i]+Smat[mi,i])/2
            }
            lnode=mi
            llnode=mi
        }else {    # (i+1) is even 
                   ##### find the upper and the lower node 
            mi            = round((i+1)/2)
            Call_Put_Flag = 1                                 # 1 for call/0 for put
            S             = Smat[mi,i]                        # S_n^i
            F             = infl*S  
            sigma         = IBTimpliedvola(s0,F,i*dt,func)    # func=1 for interpolation, else a parabola
		
            C             = IBTblackscholes(s0,F,r,sigma,i*dt,Call_Put_Flag)    # Call price from BS model
            rho_u         = 0
            if ((mi+1)<=i) {
                rho_u = sum(ADmat[(mi+1):i,i]*(infl*Smat[(mi+1):i,i]-F))
            }
            Sl = F*(ADmat[mi,i]*F-infl*C+rho_u)/(ADmat[mi,i]*F+infl*C-rho_u) # Lower node
            Su = F^2/Sl 
        	                                                            
            ####### Compensation ########
            if  ((Su < F) || (Sl>F)) {
                Su = sqrt(F^3/Smat[mi-1,i])
                Sl = F^2/Su 
            }    
            if ((mi<i) && (mi>1)) {
                if ((Su>infl*Smat[mi+1,i])||(Sl<infl*Smat[mi-1,i])) {
                    Su = sqrt(F^3/Smat[mi-1,i])
                    Sl = F^2/Su
                }
                if ((Su>infl*Smat[mi+1,i])||(Su<infl*S)) {
                    Su = infl*(S+Smat[mi+1,i])/2
                }
                if ((Sl > infl*S) || (Sl<infl*Smat[mi-1,i])) {
                    Sl = infl*(S+Smat[mi-1,i])/2
                }
            }
            Smat[mi+1,i+1] = Su
            Smat[mi,i+1]   = Sl
            lnode          = mi+1
            llnode         = mi
        }

        #******   Step 2 : Find upper nodes of stock tree *************************
        for (j in seq(lnode+1,i+1)) {
            if (Smat[j-1,i]!=0) {
                Call_Put_Flag = 1                                 # Call price
                S             = Smat[j-1,i]                       # S_n^i, i=j-1, i^1 = j
                F             = infl*S
                sigma         = IBTimpliedvola(s0,F,i*dt,func)    # func=1 for interpolation, else a parabola
        	    
                C             = IBTblackscholes(s0,F,r,sigma,i*dt,Call_Put_Flag)    # Call price from BS model
                rho_u         = 0
                if (j<=i) {
                    rho_u = sum(ADmat[j:i,i]*(infl*Smat[j:i,i]-F))
                }
                dc = C*infl-rho_u
                Su = (Smat[j-1,i+1]*dc-ADmat[j-1,i]*F*(F-Smat[j-1,i+1]))/(C*infl-rho_u-ADmat[j-1,i]*(F-Smat[j-1,i+1]))  # Upper node
       		
                #### Compensation #######
                if (j<=i) {
                    if ((Su > infl*Smat[j,i]) || (Su< infl*S)) {
                        Su = infl*(Smat[j,i]+S)/2
                    }
                }else{
                    if ((Su > S*exp(2*sigma*sqrt(dt)))|| (Su< F))  {
                        Su = S*Smat[j-1,i+1]/Smat[j-2,i]
                    }
                }   
                Smat[j,i+1] = Su 
            }
        }
	
        #******   Step 3 : Find lower nodes of stock tree *************************
        if ((llnode-1)!=0) {
            for (j in seq(llnode-1,1)) {
                Call_Put_Flag = 0                                 # Put price
                S             = Smat[j,i]                         # S_n^i
                F             = infl*S
                sigma         = IBTimpliedvola(s0,F,i*dt,func)    # func=1 for interpolation, else a parabola
                
                P             = IBTblackscholes(s0,F,r,sigma,i*dt,Call_Put_Flag)    # Put price from BS model
                rho_l         = 0
                if (j>1) {
                    rho_l = sum(ADmat[1:(j-1),i]*(F-infl*Smat[1:j-1,i]))    
                }
                dc = infl*P-rho_l 
                Sl = (-Smat[j+1,i+1]*(dc)+ADmat[j,i]*F*(Smat[j+1,i+1]-F))/(ADmat[j,i]*(Smat[j+1,i+1]-F)-dc)   #Lower node
       		
                ###### Compensation #########
                if (j>1)  {
                    if ((Sl < infl* Smat[j-1,i]) || (Sl > infl * S)) {
                        Sl = infl* (Smat[j-1,i]+S)/2
                    }
                }else{
                    if ((Sl>infl*S) || (Sl< S*exp(-2*sigma*sqrt(dt)))) {
                        Sl = S* Smat[j+1,i+1]/Smat[j+1,i]
                    }
                }
                Smat[j,i+1] = Sl
            }
        }

        #*****   Step 4 : Find nodes of probability and Arrow Debreu tree *************************
        ####### Transition probabilities #####
        pmat[1,1] = (infl*Smat[1,1]-Smat[1,2])/(Smat[2,2]-Smat[1,2])       
        if (i > 1) {
            pmat[1:i,i] = (infl*Smat[1:i,i]-Smat[1:i,i+1])/(Smat[2:(i+1),i+1]-Smat[1:i,i+1])
        }
        ####### Arrow-Debreu Prices #####
        ADmat[1,i+1] = ADmat[1,i]*(1-pmat[1,i])/infl     
        if (i>1) {
            ADmat[2:i,i+1] = (ADmat[2:i,i]*(1-pmat[2:i,i])+ADmat[1:(i-1),i]*pmat[1:(i-1),i])/infl
        }
        ADmat[i+1,i+1]=ADmat[i,i]*pmat[i,i]/infl   
    }

    #******   Step 5 : Find nodes of implied local volatility tree *************************
    LVmat = matrix(0,n,n)
    for (i in seq(1,n,1)) {
        LVmat[1:i,i] = log(Smat[2:(i+1),i+1]/Smat[1:i,i+1])*(pmat[1:i,i]*(1-pmat[1:i,i]))^0.5 #
    }

    print(Smat)
    print(ADmat)
    print(pmat)
    print(LVmat)

    IBTbc = list(S=Smat,p=pmat,AD=ADmat,LV=LVmat)
    return(IBTbc)
}

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
  
    #Modified Black-Scholes scaled by S-div instead of F
    d1   = (log(m)+tau*(r+0.5*(sigma^2)))/(sigma*st)
    d2   = d1-sigma*st
    f    = pnorm(d1)-pnorm(d2)/(ert*m)
 
    #first derivative of d1 term
    d11  = (1/(m*sigma*st))-(1/(st*(sigma^2)))*((log(m)+tau*r)*sigma1)+0.5*st*sigma1

    #first derivative of d2 term
    d21  = d11-st*sigma1

    #second derivative of d1 term
    d12  = -(1/(st*(m^2)*sigma))-sigma1/(st*m*(sigma^2))+sigma2*(0.5*st-(log(m)+rt)/(st*(sigma^2)))+sigma1*(2*sigma1*(log(m)+rt)/(st*sigma^3)-1/(st*m*sigma^2))

    #second derivative of d2 term
    d22  = d12-st*sigma2

    #Please refer to either Rookley (1997) or the XploRe Finance Guide for derivations
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

locspd = function(data, days){
    data         = subset(data, data[,5] == days) # time matrurity in days
    ftau         = data[1,5]/365                  # scale
    vola         = data[,10]                      # implied volatility
    tau          = data[,5]/365                   # scaled time to maturity
    r            = data[,9]                       # risk free interest rate
    s            = data[,8]                       # underlying price corrected for future dividends
    F            = s*exp(r*tau)                   # futures pricing formula
    K            = data[,6]                       # strike price
    m1           = K/F                            # moneyness
    firstXF      = min(m1)                        # lower bound of moneyness
    lastXF       = max(m1)                        # upper bound of moneyness
    IVpoints     = cbind(m1,tau,vola)             # implied volatility coordinates
    IVpoints     = IVpoints[order(IVpoints[,1],IVpoints[,2]),] # sort in order: moneyness, time to maturity
    IVpoints     = data.frame(Mon=IVpoints[,1],Tau=IVpoints[,2],ivs=IVpoints[,3])
    bandwidth    = c(0.1,0.1)                     # bandwidth
    stepwidth    = 0.02                           # stepwidth
    imax         = ceiling((lastXF-firstXF)/stepwidth) # length of grid
    IVsurf       = cbind(seq(firstXF,by=stepwidth,length=imax),matrix(ftau,imax,1)) # grid
    IVsurff      = loess(ivs~Mon+Tau,data=IVpoints,degree=0,normalize=FALSE)$y # NW estimator
    IVsurf2      = spline(IVsurff,n=nrow(IVsurf))$y
    j1           = ceiling((0.8 - min(c(firstXF,0.8)))/stepwidth)
    j2           = ceiling((1.2 - min(c(lastXF,1.2)))/stepwidth)
    IVsurf1      = cbind(c(seq(0.8,by=stepwidth,length=j1),seq(lastXF, by=stepwidth, length=j2)),matrix(ftau,j1+j2,1))
    IVsurf1      = IVsurf1[order(IVsurf1[,1]),]
    IVsurf1      = data.frame(IVm=IVsurf1[,1],IVt=IVsurf1[,2],IVs=IVsurff)
    bandwidth    = c(0.2,0.1)
    IVsurff1     = loess(IVs~IVm+IVt,data=IVsurf1,degree=0,normalize=FALSE)$y # NW estimator
    IV1          = cbind(IVsurf,IVsurf2)
    colnames(IV1)= c("V1","V2","V3")
    IV2          = cbind(IVsurf1[,1:2],IVsurff1)
    colnames(IV2)= c("V1","V2","V3")
    IV           = rbind(IV1,IV2)
    IVsurf       = IV[order(IV[,1],IV[,2]),]
    ovola        = cbind(IVsurf[,1]*s[1]*exp(r[1]*ftau),IVsurf[,3]) # observed volatility
    m2           = s[1]/ovola[,1]                   
    smdata       = cbind(m2,ovola[,2])             
    smdata       = smdata[order(smdata[,1]),]
    bw           = 0.01                            # binwidth
    hh           = 0.2                             # bandwidth
    m3           = seq(min(m2),by=bw,length=(max(m2)-min(m2))/bw+1)
    mh           = locpoly(x=smdata[,1],y=smdata[,2],bandwidth=hh,degree=2,x.range(min(m3),max(m3)),gridsize=length(m3),drv=0)
    mh1          = locpoly(x=smdata[,1],y=smdata[,2],bandwidth=hh,degree=2,x.range(min(m3),max(m3)),gridsize=length(m3),drv=1)	
    mh2          = locpoly(x=smdata[,1],y=smdata[,2],bandwidth=hh,degree=2,x.range(min(m3),max(m3)),gridsize=length(m3),drv=2)	
    lpspd        = spdbl(mh$x, mh$y, mh1$y, mh2$y, s[1], r[1], ftau)
    r1           = cbind((s[1]/m3),(lpspd$fstar))
    f            = r1[order(r1[,1]),]
    return(f)
}

############################ Main Computation ############################ 

data     = read.table("XFGData9701.dat")          # load data
data     = subset(data, data[,10]>0)              # data, given implied vola >0
data     = subset(data,(data[,1]==3&data[,4]==1)) # & day==3 & call options only
date     = data[1,1]+data[1,2]*100+data[1,3]*10000# calculate date
days     = 77                                     # tau, time to maturity

# estimate SPD using Rookley's method
f        = locspd(data, days)

#estimate SPD using IBT method
bcibdat  = ibtspd(data, days)
bcibdat  = subset(bcibdat, bcibdat[,2]>0.00005)
  
mmh      = bkde(bcibdat,bandwidth=180,gridsize=48,range.x=c(2200,3250))
mh1      = mmh$x
mh2      = mmh$y
m        = length(mh1)
s        = sum(mh2[1:(m-1)]*(mh1[2:m]-mh1[1:(m-1)]));
mf       = cbind(mh1, mh2/s);

#plot(mf[,1],mf[,2],type="l",lwd=2,xlab="Stock Price", ylab="Probability");
#title("Estimated Implied Distribution");

meanmf   = max(f[,2],mf[,2])
measure  = 10^(floor(log10(meanmf))+1)
 
plot(f,type="l",col="blue3",lwd=2,xlab="stock price",ylab="density",)
lines(mf[,1],mf[,2],col="black",lwd=1.5)
title(paste("SPD estimations:",date, "tau = ",days ,"days" ))




