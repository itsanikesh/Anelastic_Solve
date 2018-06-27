%@t
% \textbf{centerofmass.m}
%@h
%   Description:
%     Contains a function to find the center of mass of a 2D distribution.
%@q
%   Current Code Owner:
%     Matt de Stadler (mdestadl@ucsd.edu)

%   Modification History
%     Version   Date     Comment 
%     -------   ----     ------- 
%     1.0       07/2008  Original code. [Kyle A. Brucker] 

%   Language:
%     Matlab
%@h
%   Comments:
%     DOES NOT SEEM TO BE WORKING PROPERLY, FAILS SIMPLE 1D CONSTANT 
%     VALUE TESTS, re-write this before usage.
%@q

function [cx,cy,sx,sy] = centerofmass(m);

[sizey sizex] = size(m);
vx = sum(m);
vy = sum(m');

vx = vx.*(vx>0);
vy = vy.*(vy>0);

x = [1:sizex];
y = [1:sizey];

%
cx = sum(vx.*x)/sum(vx);
cy = sum(vy.*y)/sum(vy);

% k
sx = sqrt(sum(vx.*(abs(x-cx).^2))/sum(vx));
sy = sqrt(sum(vy.*(abs(y-cy).^2))/sum(vy));
