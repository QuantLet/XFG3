<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of Quantlet: XFGTVP_FRM

Published in: Applied Quantitative Finance (3rd Edition)

Description: Performs quantile LASSO regression in a moving window by using BIC and GACV criteria to choose penalty parameter (lambda). The input data are daily logarithmic stock returns of 200 largest U.S. financial companies and 6 macro variables. Each of the selected companies is taken as a dependent variable and is regressed by means of linear quantile regression with L1-penalty term on the remaining ones together with the macro variables. Plots time series of lambda in quantile LASSO regression. Furthermore, the cardinality of the active set q, the L2-norm of the residuals and the L1-norm of the parameter beta are plotted. All of the plots contain results from a number of regressions (equal to number of selected companies) and the average over all of them. Writes resulting time series of average lambda chosen by BIC and GACV into a .csv file

Keywords: quantile, regression, lasso, L1-norm penalty, lasso shrinkage, L1-constraint, cross-validation, time varying, bic, plot, visualization, historical moving window, nonstationary, financial, log-returns

See also: XFGTVP_LambdaVIX, XFGTVP_LambdaSim, XFGTVP_BetaChange, XFGTVP_LambdaSysRisk, quantilelasso, FRM_download_data, FRM_lambda_series, http://frm.wiwi.hu-berlin.de, TVPactiveset, TVPbetanorm, TVPdesign, TVPvariance

Author: Lenka Zboňáková

Submitted: Wed, September 07 2016 by Lenka Zboňáková

Datafile: 200_firms_returns_and_scaled_macro_2016-08-18.csv

Input: 
- n.firm   : Number of companies (maximum is 200)
- w        : Length of moving windows
- tau      : Quantile level

Example: 
- Lambda
- Cardinality of q
- L1-norm of the beta
- L2-norm of the residuals

```
<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/XFG3/master/XFGTVP_FRM/XFGTVP_FRM-1.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/XFG3/master/XFGTVP_FRM/XFGTVP_FRM-2.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/XFG3/master/XFGTVP_FRM/XFGTVP_FRM-3.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/XFG3/master/XFGTVP_FRM/XFGTVP_FRM-4.png" alt="Image" />
</div>

