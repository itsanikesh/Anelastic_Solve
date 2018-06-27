%@t
% \textbf{Gaussian2d.m}
%@h
%   Description:
%     Contains a function to fit a thermal cloud 2-D.
%@q

% KYLE WHAT IS THIS AND WHY IS IT USED?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
function [cx,cy,sx,sy,PeakOD] = Gaussian2D(m,tol);
%% m = image
%% tol = fitting tolerance

options = optimset('Display','iter','TolFun',tol,'TolX',tol,'LargeScale','off','MaxIter',10,'MaxFunEvals',10,'FunValCheck','on');

[sizey sizex] = size(m);
[cx,cy,sx,sy] = centerofmass(m);
pOD = max(max(m));
PeakOD=pOD;
return
mx = m(round(cy),:);
x1D = 1:sizex;
ip1D = [cx,sx,pOD];
A=1
fp1D = fminunc(@fitGaussian1D,ip1D,options,mx,x1D)
A=2
cx = fp1D(1);
sx = fp1D(2);
PeakOD = fp1D(3);
my = m(:,round(cx))';
y1D = 1:sizey;
ip1D = [cy,sy,pOD];
A=3
fp1D = fminunc(@fitGaussian1D,ip1D,options,my,y1D)
A=4
cy = fp1D(1);
sy = fp1D(2);
PeakOD = fp1D(3);
[X,Y] = meshgrid(1:sizex,1:sizey);
A=5
initpar = [cx,cy,sx,sy,PeakOD];
return
fp = fminunc(@fitGaussian2D,initpar,options,m,X,Y);
cx = fp(1);
cy = fp(2);
sx = fp(3);
sy = fp(4);
PeakOD = fp(5);
A=6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
