
[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGsignature** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : XFGsignature

Published in : XFG (3rd Edition)

Description : 'Plots realized volatility signature for IBM, 2001-2006. Average time between trades: 6.78 seconds.'

Keywords : 'plot, graphical representation, frequency, volatility, sampling'

See also : 

Author : Nicolas Hautsch, Uta Pigorsch

Submitted :

Datafile : ibmsignature.dat

Example : Volatility signature plot for IBM.

```

![Picture1](signature.png)


```r
# clear history
rm(list = ls(all = TRUE))
graphics.off()

# load data
ibm = read.table("ibmSignature.dat", header = F, col.names = c("trades", "rv"))

# if save output as pdf
# pdf(file = "signature.pdf", paper = "special", width = 6, height = 3)


par(mgp = c(1.75, 0.75, 0))
par(mar = c(3, 3, 0.7, 0.7))

#Plot the volatility signature
plot(ibm$trades, ibm$rv, type = "l", xlab = "sampling frequency in ticks", ylab = "RV", 
     lwd = 1, col = "blue", log = "x")
dev.off() 


```
