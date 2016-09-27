
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **kernel** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : kernel

Published in : Applied Quantitative Finance

Description : 'Calculates kernel density estimate for given data set. Kernel can have one of the
folllowing forms: Quadratic, Epanechnikow, Triangular or Uniform'

Keywords : kernel, kde, Epanechnikov, uniform, quadratic kernel, estimation

Author : Awdesch Melzer

See also : XFGIBT03

Submitted : Thu, January 17 2013 by Awdesch Melzer

Input: 
- type 1: Quadratic
- type 2: Epanechnikov
- type 3: Triangular
- type 4: Uniform
- u: data set

Output: 
- y: kernel density estimate

```


### MATLAB Code:
```matlab
function y = kernel(u, type)
% Kernel
% Usage
    % y = kernel(x, type)
% Input
    % x, type
% Output
    % y
% y = 15/16/bandwith.*((1-(u./bandwith).^2).^2).*(abs(u)<=1) 

if type == 1        % Quartic
    y= 15/16.*((1-(u).^2).^2).*(abs(u)<=1); 
end
if type == 2        % Epanechnikov
    y= 3/4.*(1-u.^2).*(abs(u)<=1) ;
end
if type == 3        % Triangular
    y= (1-abs(u)).*(abs(u)<=1); 
end
if type == 4        % Uniform
    y= (1/2).*(abs(u)<=1); 
end
```
