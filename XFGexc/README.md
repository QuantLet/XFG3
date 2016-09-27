
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGexc** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : XFGexc

Published in : Applied Quantitative Finance

Description : 'Counts 99% Value-at-Risk exceedences over a profit and loss function for basic
simulation data.'

Keywords : simulation, Loss-Profit function, VaR, exceedance, counts

Author : Germar Knöchlein, Awdesch Melzer

Submitted : Fri, June 29 2012 by Dedy Dwi Prastyo

Datafile : XFGMMPL.dat, XFGPL.dat

```

![Picture1](XFGELESC.png)


### R Code:
```r
rm(list = ls(all = TRUE))
graphics.off()

# load data
PL = read.table("XFGPL.dat")
MMPL = read.table("XFGMMPL.dat")

# main computation
m = nrow(PL)
t = 1
Exc = 0
while (t <= m) {
    q = quantile(t(PL[t, ]), p = 0.99, type = 1)  # 99% quantile
    if (q < MMPL[t, 1]) {
        # counts exceeding values of q in MMPL
        Exc = Exc + 1
    }
    t = t + 1
}
Exc = Exc/m
Exc 

```

### MATLAB Code:
```matlab
close all
clc

% load data
  PL = load('XFGPL.dat');
  MMPL = load('XFGMMPL.dat');

% main computation
  [m n] = size(PL);              % m = number of rows
  t = 1;
  Exc = 0;
  while (t<=m)
    q = quantile(PL(t,:)',0.99); % 99% quantile of PL
    if (q<MMPL(t,1))             % counts exceeding values of q in MMPL
      Exc = Exc+1;
    end
    t = t+1;
  end
  Exc = Exc/m




```
