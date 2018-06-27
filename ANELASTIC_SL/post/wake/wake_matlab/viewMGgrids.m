%@t
% \textbf{viewMGgrids.m}
%@h
%   Description:
%     Reads in and generates a plot of the grid points at each level for 
%     multigrid.
%@q

%%%%%%%%%% CLEAN THIS UP SO THAT YOU ENTER DATA DIRECTORY AND VARIABLE ONLY ONCE

close all, clear all, clc
dir = '/work/temp/';
var = 'x1_grid';
basename=[dir,var,'_'];

fname = [basename,'006.dat'];
fid = fopen(fname,'r');
fgetl(fid); fgetl(fid); fgetl(fid); fgetl(fid);
a = fscanf(fid, '%g %g %g %g %g %g %g', [7 inf]);
a = a';
fclose(fid);
xe6 = a(:,2); xc6 = a(:,3);

fname = [basename,'005.dat'];
fid = fopen(fname,'r');
fgetl(fid); fgetl(fid); fgetl(fid); fgetl(fid);
a = fscanf(fid, '%g %g %g %g %g %g %g', [7 inf]);
a = a';
fclose(fid);
xe5 = a(:,2); xc5 = a(:,3);

fname = [basename,'004.dat'];
fid = fopen(fname,'r');
fgetl(fid); fgetl(fid); fgetl(fid); fgetl(fid);
a = fscanf(fid, '%g %g %g %g %g %g %g', [7 inf]);
a = a';
fclose(fid);
xe4 = a(:,2); xc4 = a(:,3);

fname = [basename,'003.dat'];
fid = fopen(fname,'r');
fgetl(fid); fgetl(fid); fgetl(fid); fgetl(fid);
a = fscanf(fid, '%g %g %g %g %g %g %g', [7 inf]);
a = a';
fclose(fid);
xe3 = a(:,2); xc3 = a(:,3);

fname = [basename,'002.dat'];
fid = fopen(fname,'r');
fgetl(fid); fgetl(fid); fgetl(fid); fgetl(fid);
a = fscanf(fid, '%g %g %g %g %g %g %g', [7 inf]);
a = a';
fclose(fid);
xe2 = a(:,2); xc2 = a(:,3);

fname = [basename,'001.dat'];
fid = fopen(fname,'r');
fgetl(fid); fgetl(fid); fgetl(fid); fgetl(fid);
a = fscanf(fid, '%g %g %g %g %g %g %g', [7 inf]);
a = a';
fclose(fid);
xe1 = a(:,2); xc1 = a(:,3);

% xe5 = xe5 - xe5(1)+xe6(1); % MATT WHAT IS THIS???
figure
plot(xe6,0,'g.'), hold on,  plot(xe5,0.01,'m.'), plot(xe4,0.02,'r.'), plot(xe3,0.03,'b.'), plot(xe2,0.04,'k.')
plot(xe1,0.05,'c.')
ylim([-0.1,0.3])
title('Edge Grids');

figure
plot(xc6,0,'g.'), hold on,  plot(xc5,0.01,'m.'), plot(xc4,0.02,'r.'), plot(xc3,0.03,'b.'), plot(xc2,0.04,'k.')
plot(xc1,0.05,'c.')
ylim([-0.1,0.3])
title('Center Grids');

% figure, 
% plot(xc6,0,'g.'), hold on,  plot(xc5,0.0,'m.')
% % legend('predicted solution','computed solution');
