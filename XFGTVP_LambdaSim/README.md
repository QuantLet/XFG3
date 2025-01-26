<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of Quantlet: XFGTVP_LambdaSim

Published in: Applied Quantitative Finance (3rd Edition)

Description: Performs quantile LASSO regression in a moving window by using BIC

Keywords: quantile, regression, lasso, L1-norm penalty, lasso shrinkage, L1-constraint,

See also: XFGTVP_LambdaVIX, XFGTVP_BetaChange, XFGTVP_FRM, XFGTVP_LambdaSysRisk,

Author: Lenka Zboňáková

Submitted: Fri, September 02 2016 by Lenka Zboňáková

Input: 
- n.obs    : Number of observations to simulate
- n.param  : Number of parameters to simulate
- n.sim    : Number of simulations
- w        : Length of moving windows
- seed1    : Seed to simulate design matrix X
- seed2    : Seed to simulate error terms
- tau      : Quantile level
- sd.start : Standard deviation of error term before change point
- sd.end   : Standard deviation of error term after change point
- q.start  : Number of nonzero parameters before change point
- q.end    : Number of nonzero parameters after change point
- r.start  : Correlation coefficient for X before change point
- r.end    : Correlation coefficient for X after change point

Example: 
- Lambda
- Cardinality of q
- L1-norm of the beta
- L2-norm of the residuals

```
<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/XFG3/master/XFGTVP_LambdaSim/XFGTVP_LambdaSim-1.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/XFG3/master/XFGTVP_LambdaSim/XFGTVP_LambdaSim-2.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/XFG3/master/XFGTVP_LambdaSim/XFGTVP_LambdaSim-3.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/XFG3/master/XFGTVP_LambdaSim/XFGTVP_LambdaSim-4.png" alt="Image" />
</div>

