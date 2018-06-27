%@t
% \textbf{Gauss2d.m}
%@h
%   Description:
%     Contains an example of a function which calculates a 2D Gaussian fit
%     to an image.
%@q

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% To fit a 2-D gaussian
%% m = Image
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
m=data;
[cx,cy,sx,sy,PeakOD] = Gaussian2D(m,.0005);

[sizey sizex] = size(m);
[x,y] = MeshGrid(1:sizex,1:sizey);
fit = abs(PeakOD)*(exp(-0.5*(x-cx).^2./(sx^2)-0.5*(y-cy).^2./(sy^2)));
imagesc(fit)
