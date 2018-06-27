%@t
% \textbf{etaresolution.m}
%@h
%   Description:
%     Calculates the Kolmogorov viscous and thermal scales to determine
%     how well the small scales are being captured.
%@q

 clear all; close all; clc; format long;

Re=10000;
Pr=1;
virtual_origin=0;

% dir = '/work/temp/statrelax9.25/'; % location of .vtk files
% dir = '/work/temp/statrefine/'; % location of .vtk files
% dir = '/work/temp/statRe6000/'; % location of .vtk files
% dir = '/work/temp/relaxRe6000/'; % location of .vtk files
% dir = '/work/temp/relaxRe7500/'; % location of .vtk files
 dir = '/work/temp/prerelaxrefine9.25/'; % location of .vtk files
 dir = '/work/temp/relaxnewspectrum/'; % location of .vtk files
 dir = '/work/temp/relaxvrefine/'; % location of .vtk files
 outfname = 'etaoverdx.jpg';

%DATA SIZE
jmin=1;
kmin=1;
jmax=512;
kmax=256;
js=26;		% sponge size in j
ks=13;		% sponge size in k
dx=24/(jmax-1); % grid resolution

%TIME LOOP
imin=4;
imax=204;
iskip=4;

%%%%%%%%%%%%%%%%%%%%% END USER INPUT %%%%%%%%%%%%%%%%%%%%%
Tplot=0; %OUTPUT CONTOUR TO FIGURE WINDOW ON READ OF VTK FILE
eta(kmin:kmax,jmin:jmax) = 1; etat(kmin:kmax,jmin:jmax) = 1;
counter = 1;
rSqrtPr = 1/sqrt(Pr);

for iter=imin:iskip:imax

% LOAD VARIABLES
    var='DISS';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [time y z diss] = read_vtk(fname,1,Tplot);

  for k = kmin+ks:kmax-ks
    for j = jmin+js:jmax-js
      eta(k,j) = ( (1/Re)^3 / ( diss(k,j) ) )^(1/4); 	% eta = (nu^3 / epsilon)^(1/4)
      etat(k,j) = eta(k,j)*rSqrtPr;		  	%eta_theta = eta / sqrt(Pr)
    end
  end


% Get smallest scales at each time
dxovereta(counter) = dx/min(min(eta));
dxoveretat(counter) = dx/min(min(etat));
x(counter) = time + virtual_origin;

%  Verification Tool: see if the smallest scale is occurring inside the wake
%% find location of smallest scale
%  etaminval = 10; xloc = 1000; yloc = 1000;
%  for k = kmin+ks:kmax-ks
%    for j = jmin+js:jmax-js
%      if (eta(k,j) < etaminval)
%      xloc = j;
%      yloc = k;
%      etaminval = eta(k,j);
%      end
%    end
%  end
%  xloct(counter) = xloc;
%  yloct(counter) = yloc;
counter = counter+1; 
end

% Plot scales
plot(x,dxovereta,'r'); hold on; plot(x,dxoveretat,'b'); xlabel('x');
title('Simulation resolution','FontSize',14); 
h_l = legend('\Deltax/\eta','\Deltax/\eta_\theta'); set(h_l,'FontSize',14);
outname= [dir,outfname];
print('-djpeg',outname,'-r150') % Save a jpeg version of the current figure

