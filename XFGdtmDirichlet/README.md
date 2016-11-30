
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGdtmDirichlet** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet : XFGdtmDirichlet

Published in : Applied Quantitative Finance (3rd Edition)

Description : Simulates and plots the Dirichlet distribution with various values for alpha.

Keywords : simulation, distribution, plot, graphical representation, visualization

Author : Ernie Teo

```

![Picture1](XFGdtmDirichlet_1.png)

![Picture2](XFGdtmDirichlet_2.png)

![Picture3](XFGdtmDirichlet_3.png)

![Picture4](XFGdtmDirichlet_4.png)


### MATLAB Code:
```matlab
n = 2000;

% Plot 1
a = [1 1 1];
r = drchrnd(a,n);

HD        = scatter3(r(:,1),r(:,2),r(:,3),'MarkerFaceColor',[1 0 0]);
direction = [0 0 1];
view(-40,-18)

% Plot 2
a = [.1 .1 .1];
r = drchrnd(a,n);
HD        = scatter3(r(:,1),r(:,2),r(:,3),'MarkerFaceColor',[1 0 0]);
direction = [0 0 1];
view(-40,-18)

% Plot 3
a = [10 10 10];
r = drchrnd(a,n);
HD        = scatter3(r(:,1),r(:,2),r(:,3),'MarkerFaceColor',[1 0 0]);
direction = [0 0 1];
view(-40,-18)

% Plot 4
a = [2 5 15];
r = drchrnd(a,n);
HD        = scatter3(r(:,1),r(:,2),r(:,3),'MarkerFaceColor',[1 0 0]);
direction = [0 0 1];
view(-40,-18)
```
