
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGqDGtest** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : XFGqDGtest

Published in : Applied Quantitative Finance

Description : 'XFGqDGtest plots the 1%-quantile from the two one-parametric sub-families of the
class of quadratic forms of Gaussian vectors, as defined by the functions XFGdg11par and
XFGdg12par'

Keywords : 'FFT, Fourier inversion, Fourier transform, cdf, characteristic function, gaussian,
normal approximation, quantile'

See also : 'StandardNormalCharf, VaRcdfDG, VaRcgfDG, VaRcharfDG, VaRcharfDGF2, VaRcorrfDGF2,
VaRqDG, cdf2quant, gFourierInversion'

Author : Awdesch Melzer

Submitted : Wed, June 05 2013 by Awdesch Melzer

Usage : XFGqDGtest(n,N,dt)

Input : 'n- number of samples to use in the plot N- modulus of the FFT to use in the approximation,
should be a power of 2 dt- the size of the grid in the approximation of the Fourier inversion
integral'

Output : '- plots the 1%-quantile from the two one-parametric sub-families of the class of
quadratic forms of Gaussian vectors, as defined by the functions XFGdg11par and XFGdg12par'

Example : 'plots the 1%-quantile from the two one-parametric sub-families of the class of quadratic
forms of Gaussian vectors, as defined by the functions XFGdg11par'

```

![Picture1](plot of XFGdg11par.png)

![Picture2](plot of XFGdg12par.png)


### R Code:
```r
##################### SUBROUTINES ####################

compl = function (re, im){ # Complex array generation
    if(missing(re)){
        stop("compl: no composed object for real part")
    }
    if(missing(im)){
        im = 0*(re<=Inf)
    }
    if(nrow(matrix(re))!=nrow(matrix(im))){
        stop("compl: dim(re)<>dim(im)")
    }
    z    = list()
    z$re = re
    z$im = im
    return(z)
}

cmul = function(x, y){ # Complex multiplication
    re   = x$re*y$re - x$im*y$im  
    im   = x$re*y$im + x$im*y$re
    z    = list()
    z$re = re
    z$im = im  
    return(z)
}
  
cexp = function(x){ # Complex exponential
    re   = exp(x$re) * cos(x$im) 
    im   = exp(x$re) * sin(x$im) 
    z    = list()
    z$re = re
    z$im = im  
    return(z)
}
  
csub = function(x, y){# Complex subtraction two arrays of complex numbers
       re   = x$re - y$re  
       im   = x$im - y$im
       z    = list()
       z$re = re
       z$im = im  
       return(z)
}
  
cdiv = function(x, y) { # Complex division
    w    = y$re^2 + y$im^2
    re   = (x$re*y$re + x$im*y$im) / w  
    im   = (x$im*y$re - x$re*y$im) / w  
    z    = list()
    z$re = re
    z$im = im  
    return(z)
}
  
cln = function(x){ # Complex natural logarithm
    re   = log(x$re^2+x$im^2) / 2
    im   = atan2(x$im, x$re)
    z    = list()
    z$re = re
    z$im = im  
    return(z)
}
  
creal = function(z){ # Returning real part
    re = z$re
    return(re)
}
  
StandardNormalCharf = function(t,l){ # Standard Normal Characteristic Function
    s2  = l$sigma^2
    tmp = compl(-0.5*s2*t$re,-0.5*s2*t$im + l$mu)
    r   = cexp(cmul(tmp,t))
    return(r)
}

gFourierInversion = function(N,K,dt,t0,x0,charf,l){ # generic function for density approximation
    # 1. form the grids:
    dx = (2*pi)/(N*dt)
    t  = (0:(K-1))*dt
    if (t0 != 0) {
        t = t + dt/2
    }
    t  = compl(t,t*0) 
    x  = x0 + (0:(N-1))*dx
    # 2. do the FFT:
    tmp        = charf(t,l)
    phi        = matrix(1,N,2)                 
    phi[1:K,1] = tmp$re
    phi[1:K,2] = tmp$im
    tmp        = x0 * dt *(0:(N-1)) 
    phi        = cmul(compl(phi[,1],phi[,2]),cexp(compl(tmp*0,-tmp)))
    phi        = cbind(phi$re,phi$im)
    phitmp     = complex(real=phi[,1],imaginary=phi[,2])
    ninvfft    = length(phitmp)
    y          = fft(phitmp,inverse=T)#ninvfft
    y          = compl(Re(y),Im(y))
    if (t0 != 0){
        tmp = x*dt/2
        y   = cmul(y,cexp(compl(tmp*0,-tmp)))
    }
    # 3. rescale:
    if (t0 == 0){ 
        r = dt * ( creal(y) - 0.5*creal(charf(list(re=0,im=0),l)) )/ pi
    }else{ 
        r = dt * creal(y)/pi
    }
    return(r)
}

VaRcgfDG = function(t,par){ # cumulant generating function (cgf) for the 
    # class of quadratic forms of Gaussian vectors.
  
    s = compl(par$theta*t$re, par$theta*t$im)
    i = 1
    m = length(par$lambda)
    while (i <= m){
        # 1-lambda*t:
        omlt = compl(1 - par$lambda[i] * t$re,  -par$lambda[i] * t$im)
        tmp  = cmul(t,t)
        tmp  = cdiv(tmp,omlt)
        tmp  = compl(par$delta[i]^2 * tmp$re, par$delta[i]^2 * tmp$im)
        tmp  = csub(tmp,cln(omlt))
        s    = compl(s$re + 0.5*tmp$re, s$im + 0.5*tmp$im)   
        i    = i+1
    }
    return(s)
}

VaRcharfDG = function(t,par){# computes the characteristic function for the
    # class of quadratic forms of Gaussian vectors.
  
    t = compl(-t$im,t$re)                 # 1i*t
    r = cexp(VaRcgfDG(t,par))
}
  
VaRcharfDGF2 = function(t, l){ # Fourier transform of an approximating Gaussian cdf
    mu   = l$theta + 0.5*sum(l$lambda)
    s2   = sum(l$delta^2 + 0.5*l$lambda^2)
    tmp  = compl(-0.5*s2*t$re,-0.5*s2*t$im + mu)
    tmp  = cexp(cmul(tmp,t))
    tmp  = csub(VaRcharfDG(t,l),tmp)
    tmp  = cdiv(tmp,t)
    r    = compl(-tmp$im,tmp$re)
    r$re = replace(r$re,r$re=="NaN",0)
    r$im = replace(r$im,r$im=="NaN",0)
    return(r)
}

VaRcorrfDGF2 = function(x,l){ # cdf of normal approximation
    mu = l$theta + 0.5*sum(l$lambda)
    s2 = sum(l$delta^2 + 0.5*l$lambda^2)
    r  = pnorm((x-mu)/sqrt(s2))
    return(r)
}

VaRcdfDG = function(l,N,K,dt){ # approximates the cdf for the class of quadratic forms
    # Gaussian vectors 

    dx      = 2*pi/(N*dt)
    x0      = -pi/dt
    y       = gFourierInversion(N,K,dt,dt/2,x0,VaRcharfDGF2,l)
    x       = x0 + (0:(N-1))*dx
    y       = y + VaRcorrfDGF2(x,l)
    y[x<=0] = 0 # correct for misspecification
    r       = list(x=x,y=y)
    return(r)
}

cdf2quant = function(a,l){ # compute the quantile from a CDF on a given grid
    # robust version:

    x     = l$x
    y     = l$y
    left  = 1
    right = length(y)
    # deal with extreme cases:
    if( a <= y[left]  ){ 
        q = x[left]
    }else if( a >= y[right] ){
        q = x[right] 
    }else{
        idx   = 1:length(y)
        left  = max(idx[which(y <= a)])
        right = min(idx[which(y >= a)])
        if ( left < right ){
            w = (y[right]-a)/(y[right]-y[left])
            q = w*x[left] + (1-w)*x[right]
        }else{
            q = 0.5*(x[left]+x[right])
        }
    }
return(q)
}

VaRqDG = function(a,par,N,K,dt){
    r = VaRcdfDG(par,N,K,dt)
    q = cdf2quant(a,r)
    return(q)
}

#################### MAIN FUNCTIONS ####################

XFGqDGtest = function(n,N,dt){	
    # di      = createdisplay(1,2)
    lambdav = -sqrt(2) + (0:(n-1))*2*sqrt(2)/(n-1)
    q       = lambdav
    i       = 1
    while ( i <= n ){
        q[i] = VaRqDG(0.01,XFGdg11par(lambdav[i]),N,N,dt)
        i    = i+1
    }
    z = cbind(lambdav,q)
    plot(z,type="l",col="blue3",lwd=2,xlab="X",ylab="Y")
    phi = -pi/2 + (0:(n-1))*pi/(n-1)
    q   = phi
    i   = 1
    while (i <= n){
        q[i] = VaRqDG(0.01,XFGdg12par(phi[i]),N,N,dt)
        i    = i+1
    }
    z = cbind(phi,q)
    dev.new()
    plot(z,type="l",col="blue3",lwd=2,xlab="X",ylab="Y")
}

XFGdg11par = function(la,de,th){
    lambda = la
    if (missing(de)){
        delta = c(sqrt(max(c(0,1-lambda^2/2))))
    }else{
        delta = de
    }
    if (missing(th)){  
        theta = -0.5*lambda
    }else{
        theta = th
    }
    Sigma  = diag(rep(1,1))
    Gamma  = lambda*diag(c(1))
    Delta  = delta
    return(list(Sigma=Sigma,Gamma=Gamma,Delta=Delta,theta=theta,lambda=lambda,delta=delta))
}

XFGdg12par = function(phi){	#XFG
  sr2    = sqrt(2)
  phi    = phi + pi/4
  lambda = sr2* c(sin(phi),-cos(phi))
  delta  = c(0,0)
  theta  = -0.5 * sum(lambda)
  Sigma  = diag(rep(1,2))
  Gamma  = lambda*diag(c(1))
  Delta  = delta
  return(list(Sigma=Sigma,Gamma=Gamma,Delta=Delta,theta=theta,lambda=lambda,delta=delta))
}

XFGqDGtest(n=33,N=1024,dt=0.2)


```
