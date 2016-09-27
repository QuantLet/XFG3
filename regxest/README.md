
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **regxest** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of Quantlet : regxest

Published in : Applied Quantitative Finance

Description : 'Computes the Nadaraya-Watson estimator for univariate regression. Required by
XFGIBT02.m and XFGIBT03.m.'

Keywords : Nadaraya Watson, nonparametric, estimation, kernel, univariate

See also : SPMtruenadloc, XFGIBT02

Author : Dedy Dwi Prastyo

Submitted : Fri, July 27 2012

Usage : mh = regxest(x {,h {,K} {,v} })

Output: 
- mh: '(n x 2) or (m x 2) matrix, the first column represents the sorted first column of x or the
sorted v and the second column contains the regression estimate on the values of the first column.'

```


### MATLAB Code:
```matlab

function[mh] = regxest(x,h,K)
    [n,m] = size(x);
    if m ~= 2
        disp('regxest: data matrix must contain 2 columns');
        disp('Please input the data matrix again');
        dat = input('x=');
    end
    eh = exist('h');
    if (eh == 0)
        %h=(max(x(:,1))-min(x(:,1)))/5;
        h = 2.42*std(x(:,1))*n^(-0.2);
    end
    eK = exist('K');
    if (eK == 0)
        K=1;        %quartic kernel
    else
        if K == 0 || K >4
            disp('Type of the kernel must be "1", "2", "3" or "4"');
            disp('The quartic kernel will be used');
            K = 1;
        end  
    end
    tmp = sortrows(x);
    x   = tmp(:,1);
    y   = tmp(:,2);
    %if (exist(v)==0)
        %v = x;
    %else
        %v = sort(v);
    %endif
    %require nw (Nadaraya-Watson) function / toolbox
    for i = 1:n  
        s(i,1)= nw(x(i),x,y,h,K)./nw(x(i),x,ones(n,1),h,K);
    end
    %for i = 1:n  % Gaussian kernel
        %s(i,1)= nw(x(i),x,y,5*h,K)./nw(x(i),x,ones(n,1),5*h,K);
    %end
    mh=[x s];
```
