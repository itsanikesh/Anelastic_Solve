%@t
% \textbf{fitgaussian1d.m}
%@h
%   Description:
%     Contains a function which calculates a 1D Gaussian fit.
%@q
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [z] = fitgaussian1D(p,v,x1);

%cx = p(1);
%wx = p(2);
%amp = p(3);

% KYLE DO YOU HAVE A REFERENCE FOR THIS? WHAT ARE V, X1 AND Z?

zx = p(3)*exp(-0.5*(x1-p(1)).^2./(p(2)^2)) - v;

z = sum(zx.^2);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
