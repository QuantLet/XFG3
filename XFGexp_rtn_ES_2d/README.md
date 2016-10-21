
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="880" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGexp_rtn_ES_2d** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet : XFGexp_rtn_ES_2d

Published in : Applied Quantitative Finance

Description : 'computes the values of the expected returns and the expected shortfall (ES) of the
optimal portfolios with different holding weights % for the 1st underlying asset under an
EGARCH(1,1) model, where the number of assets is 2'

Keywords : Linear programming, expected shortfall, garch, portfolio, risk aversion, risk measure

See also : XFGTWSE100_strategy_fixedESlevel, XFGexp_rtn_SRM, XFGexp_rtn_SRM_2d

Author : Shih-Feng Huang, Hsiao-Chi Lin, Tze-Yun Lin, Alla Petukhina

Submitted : Thu, August 06 2015 by Sergey Nasekin

Input : None

Output : 'plots of the expected returns and the ES of the optimal portfolios with different holding
weights of the 1st underlying asset under an EGARCH(1,1) model, where the number of assets is 2'

Example : 'a plot of the expected returns and the ES of the optimal portfolios with different
holding weights of the 1st underlying asset'

```

![Picture1](expected returns and the ES of the optimal portfolios.png)


### MATLAB Code:
```matlab
clear all;tic;
p      = 2;                 % number of assets 
T      = 250;
alpha  = 0.05;
% Model parameters
Offset = [0.01, 0.0105];    % constant in the AR model
theta  = [0.02, 0.0199];    % AR(1) coefficient

% EGARCH
k      = [-0.3, -0.3];      % The (1-alpha)alpha_0 in Tsay (2010) (3.26) 
garch  = [0.95, 0.95];      % The alpha in Tsay (2010) (3.26)  
arch   = [0.1776, 0.1776];  % The gamma in Tsay (2010) (3.24)  
L      = [-0.05, -0.05];    % The theta in Tsay (2010) (3.24)  

% Generate random data
mu = zeros(1, p);
for m = 1:p
    sigma2(1, m) = exp(k(m)/(1-garch(m)));  
end
SIGMA = eye(p);
rho   = 0.5;             
for i = 1:length(mu)-1
    SIGMA(i,i+1) = rho*sqrt(SIGMA(i,i)*SIGMA(i+1,i+1));
    SIGMA(i+1,i) = SIGMA(i,i+1);
end
Mean1    = Offset(1)/(1-theta(1));
Mean2    = Offset(2)/(1-theta(2));
STD(1:2) = [sqrt(sigma2(1, 1)), sqrt(sigma2(1, 2))];
SR1      = Mean1/STD(1);
SR2      = Mean2/STD(2);

% t-distribution
degree   = 10;
E        = 2*sqrt((degree-2)/pi)*gamma((degree+1)/2)/gamma(degree/2)/(degree-1);

ite      = 1;
sum_flag = 0;
while sum_flag < ite
    epsilon = mvtrnd(SIGMA, degree, T); % multivariate t 
    for m = 1:p
        r(1, m) = Offset(1, m) + sqrt(sigma2(1, m))*epsilon(1, m); 
        for i = 2:T
            sigma2(i, m) =...
            exp( garch(m) * log(sigma2(i-1, m)) +...
            k(m) + L(m) * epsilon(i-1, m) +...
            arch(m) * (abs(epsilon(i-1, m))- E) );        
            r(i, m) = Offset(m) + theta(m) * r(i-1) +...
            sqrt(sigma2(i, m))*epsilon(i, m); % return
        end
    end
    SORT_R = sort(r);
    
    % Model estimation
    spec = garchset('VarianceModel','EGARCH','R',1,'P',1,'Q',1,'Dist','t');
    spec = garchset(spec, 'Display', 'off');
    for m = 1:p
        [coeff(m), errors(m), LLF, Innovations, Sigmas] =...
        garchfit(spec, r(:, m));
        hat_phi0(m)       = coeff(1, m).C;        % phi0
        hat_phi1(m)       = coeff(1, m).AR;       % phi1
        hat_epsilon(:, m) = Innovations./Sigmas;  
        S(m, m)           = Sigmas(end);
    end
    B = zeros(p, p);
    for ii = 1 : p
        B(ii, ii) = r(T, ii);
    end
    R             = hat_phi0' + (hat_phi1 * B)'; % expected return

    % Compute the upper bound of the risk
    Weight1       = [0.5 0.5];
    hat_epsilon_p = hat_epsilon * S * Weight1';    % kappa_{t+1}
    var1          = - prctile(hat_epsilon_p, alpha * 100); % xi*, VaR of -kappa
  
    % ES > 0, ES*, ES of -kappa 
    ES_alpha1     = -sum(hat_epsilon_p(-hat_epsilon_p > var1)) /(T * alpha);  
    ES_alphaf1    = -R' * Weight1' + ES_alpha1;    % for portfolio
    
    % Linear programming
    f                     = [0, -R' zeros(1, T)];        
    lb                    = [-10^6; zeros(T+p,1)];          
    b                     = [1; ES_alphaf1; zeros(T,1)];      
    
    A                     = zeros(T+2,1+p+T);
    A(1, 2:p+1)           = 1;
    A(2, 1)               = 1;
    A(2, 2 : p+1)         = -R';
    A(2, p+2 : T+p+1)     = 1/(T * alpha);
    A(3:T+2, 1)           = -1;
    A(3:T+2, 2 : p+1)     = -hat_epsilon(1:T, :) * S;
    A(3:T+2, p+2 : T+p+1) = -eye(T);

    [ww, minf, exitflag]  = linprog(f, A, b ,[], [], lb);   

    weight_SUM  = 0; 
    if exitflag == 1
        weight(:,1) = ww(2:p+1);  % The optimal weights obtained from the LP
        weight_SUM  = sum(weight); % The sum of the weights of the optimal 
                                  % portfolio obtained from the LP        
    end
    
    % Note that in this demonstration, we focus on the case of c_1 + c_2 =1 
    % for comparison. In general, this constraint is not necessary. 
    if weight_SUM > 0.9999
        sum_flag  = sum_flag+1;
    end   
end
% ww(1:3)'

% Different combination of the investment weights
c1 = 0:0.01: 1;
c2 = 1-c1;

for mm = 1:length(c1)
    % Compute the expected returns under different weights
    rrr(mm)        = [c1(mm) c2(mm)] * R;
    Weight         = [c1(mm) c2(mm)];
    hat_epsilon_p1 = hat_epsilon * S * Weight';  
    var2(mm)       = -prctile(hat_epsilon_p1, alpha * 100); 
    ES_alpha2(mm)  = sum(hat_epsilon_p1(-hat_epsilon_p1 > var2(mm))) *...
                     (-1/(T * alpha));  
    ES_alphaf2(mm) = -R' * Weight' + ES_alpha2(mm); % for portfolio
end

figure(1)
y4_index(:,1)      = 1:101;
y4_index(:,2)      = (ES_alphaf2 > ES_alphaf1);
y4_i1              = y4_index(y4_index(:,2)==1,1);

subplot(2, 1, 1);
x                  = 0:0.01:1;
y1                 = rrr;   % Expected returns under different weights
x3                 = ww(2); % Optimal weights of the 1st asset obtained from the LP
y3                 = -minf; % Expected return of the optimal portfolio obtained from the LP

plot(x, y1, 'g.', x3, y3, 'rO', x(y4_i1), rrr(y4_i1), 'b.');
title('ES with alpha = 0.05 and p = 2');
xlabel('c_1');
ylabel('Expected return');

subplot(2, 1, 2);
y1                = ES_alphaf2;                     % Values of ES under different weights
y2                = ES_alphaf1*ones(1, length(c1)); % Upper bound of the risk constraint

plot(x, y1, 'g.', x, y2, 'r--', x(y4_i1), y1(y4_i1), 'b.');
xlabel('c_1');
ylabel('ES of the portfolio');

toc;
```
