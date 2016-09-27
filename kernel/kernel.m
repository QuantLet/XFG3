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