
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGiv00** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : XFGiv00

Published in : Applied Quantitative Finance

Description : 'XFGiv00 calculates the Black and Scholes price for a European call or put option for
a given spot price S, strike price K, risk-free interest rate r, volatility sigma, time to maturity
tau and a task (0 = put option, 1 = call option).'

Keywords : black-scholes, call, european-option, option, option-price, put

See also : XFGLSK, XFGiv01, XFGiv02, XFGiv03, XFGiv04, XFGiv05, XFGiv06

Author : Awdesch Melzer

Submitted : Wed, May 28 2014 by Awdesch Melzer

Usage : BlackScholes(S=100, K=120, r=0.05, sigma=0.2494, tau=0.5, task=1)

```


### R Code:
```r
rm(list=ls(all=TRUE))
graphics.off()


############################ SUBROUTINE ################################

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

############################ MAIN COMPUTATION ##############################

optionprice = BlackScholes(S=100, K=120, r=0.05, sigma=0.2494, tau=0.5, task=1) # Call option
paste("The option price is:", round(optionprice,4))



```
