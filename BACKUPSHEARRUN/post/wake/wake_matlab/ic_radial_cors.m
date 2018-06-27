%@t
% \textbf{ic\_radial\_cors.m}
%@h
%   Description:
%     Calculates statistics in cylindrical coordinates and tracks their
%     evolution in time for determining when to start a simulation. 
%@q
%   Current Code Owner:
%     Matt de Stadler (mdestadl@ucsd.edu)

%   Modification History
%     Version   Date     Comment 
%     -------   ----     ------- 
%     1.0       07/2008  Original code. [Kyle A. Brucker] 
%     2.0       03/2009  Update for new file format. [Matt de Stadler] 
%     3.0       06/2009  Track integrated Prod and Dissipation. [Matt de Stadler] 
%@h
%   Comments:
%     This program is best run by entering matlab from the command line 
%     with the command: matlab -nodisplay. Next enter: ic_radial_cors 
%     This makes the output go much faster since it doesn't have to write
%     to screen first. Unfortunately this means that you don't get the
%     nice formatting that matlab automatically generates (ghostscript
%     print drivers are used instead and these need to be manually 
%     configured).
%     This program can be updated if the integrated turbulent kinetic
%     energy quantities are calculated in the code itself. 
%@q

clear all; clc; close all; format long;

Re=10000;          % Reynolds number of the simulation
js=26;             % Sponge extent in X2, used for integrated quantities
ks=13;             % Sponge extent in X3, used for integrated quantities

binF=200;          % Number of shells to use in r
rplotmax=3;        % outer limit for plots of r

Tplot=0;           % if 1 open a contour plot each time read_vtk is called
pl2d=0;            % if 1 open a contour plot for each variable at each timestep
intproddiss=1;     % if 1 make a plot of the integrated production and dissipation
savejpgs=1;        % if 1 save a jpeg of the evaluation variables at each timestep
tracktkecomps=0;   % if 1 track how tke components evolve over time, also u1',u2',u3'

% dir = '/work/temp/wakerho0/relax/'; % file directory
% dir = '/work/temp/prerelaxrefine9.25/'; % file directory
% dir = '/work/temp/waketest/results/relax/'; % file directory
 dir = '/work/temp/relax/'; % file directory
 dir = '/work/temp/relaxnewspectrum/'; % file directory
 dir = '/work/temp/relaxvrefine/'; % file directory

imin = 4;
iskip = 4;
imax = 204;

count = 1; 
for iter=imin:iskip:imax % start time loop
%%%%%%%%%%%%%%%% LOAD VARIABLES %%%%%%%%%%%%%%%% 
var='u1p';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z u1p] = read_vtk(fname,1,Tplot);

var='u2p';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z u2p] = read_vtk(fname,1,Tplot);

var='u3p';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z u3p] = read_vtk(fname,1,Tplot);

var='u1pu3p';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z u1u3] = read_vtk(fname,1,Tplot);

var='u1pu2p';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z u1u2] = read_vtk(fname,1,Tplot);

var='dU1dx2';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z du1dx2]=read_vtk(fname,1,Tplot);

var='dU1dx3';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z du1dx3]=read_vtk(fname,1,Tplot);

var='DISS';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z diss]=read_vtk(fname,1,Tplot);

var='PROD12';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z prod12]=read_vtk(fname,1,Tplot);

var='PROD13';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z prod13]=read_vtk(fname,1,Tplot);

KIN= .5*(u1p.^2+u2p.^2+u3p.^2);

%%%%%%%%%%%%%%%% CALCULATE RADIAL STATISTICS %%%%%%%%%%%%%%%% 

nz=length(z);
ny=length(y);

% initialize variables to zero, set up shells
for k=1:nz
    for j=1:ny
        Rmag   = ((y(j))^2+(z(k))^2)*binF;
        nshell = round(sqrt(Rmag))+1;
        kinetic(nshell) = 0;
        rdiss(nshell) = 0;
        rprod(nshell) = 0;
        r(nshell) = sqrt(Rmag);
        counter(nshell) = 0;
        Shear(nshell) = 0;
        u1r(nshell) = 0; 
        u2r(nshell) = 0;
        u3r(nshell) = 0;
        u1ur(nshell) = 0;
        ru1u3(nshell) = 0;
        eta(nshell) = 0;
        Shear(nshell)=0;
        du1dr(k,j)=0;
    end
end

% calculate variables as functions of r
for k=1:nz
    for j=1:ny
        Rmag   = ((y(j))^2+(z(k))^2)*binF;
        nshell = round(sqrt(Rmag))+1;
        kin    = 0.5*(u1p(k,j)^2+u2p(k,j)^2+u3p(k,j)^2);
        kinetic(nshell) = kinetic(nshell) + kin;
        rdiss(nshell) = rdiss(nshell) + diss(k,j);
        rprod(nshell) = rprod(nshell) + prod12(k,j) + prod13(k,j);
        u1r(nshell) = u1r(nshell) + u1p(k,j);
        u2r(nshell) = u2r(nshell) + u2p(k,j);
        u3r(nshell) = u3r(nshell) + u3p(k,j);  
        ru1u3(nshell) = ru1u3(nshell) + u1u3(k,j);  
        eta(nshell) = eta(nshell) + ( (1/Re)^3 / ( diss(k,j) ) )^(1/4);
      
        if (nshell > binF/8) 
            ru1u3(nshell) = 0;
            eta(nshell) = 0;
        end
        
        counter(nshell) = counter(nshell) + 1;
    end
end

Pi = 4.0*atan(1.0);

% special case for u1ur, shear and du1dr
for k=1:nz
    for j=1:ny        
        Rmag   = ((y(j))^2+(z(k))^2)*binF;
        nshell = round(sqrt(Rmag))+1;
        
        if ( y(j) > 0) & (z(k) > 0 )  %Q1
            theta = atan( abs(z(k)/y(j)) );
        elseif (y(j)==0 ) & ( z(k) > 0 ) %Pi/2
            theta = Pi/2.d0;
        elseif (y(j) < 0 ) & ( z(k) > 0 ) %Q2
             theta = Pi - atan( abs(z(k)/y(j)) );
        elseif (z(k)==0 ) & ( y(j) < 0 ) %Pi
            theta = Pi;
        elseif (y(j) < 0 ) & ( z(k) < 0 ) %Q3
            theta = Pi + atan( abs(z(k)/y(j)) );
        elseif ( y(j)==0 ) & ( z(k) < 0 ) %3Pi/2
             theta = 3.0*Pi/2.0;
        elseif (y(j) > 0 ) & ( z(k)<0 ) %Q4 
            theta = 2.0*Pi - atan( abs(z(k)/y(j)) );
        elseif ( z(k)==0 ) & ( y(j) > 0 ) %0,2Pi
             theta = 2.0*Pi;
        else
             theta = atan( (z(k)/y(j)) );
        end

%        theta = katan(y(j),z(k)); % MATT TEST THIS TO MAKE SURE YOU GET THE SAME ANSWER
        u1ur(nshell)= u1ur(nshell)+u1u3(k,j)*sin(theta)+u1u2(k,j)*cos(theta);
        Shear(nshell) = Shear(nshell) + du1dx3(k,j)*sin(theta)+du1dx2(k,j)*cos(theta);
        du1dr(k,j)=du1dx3(k,j)*sin(theta)+du1dx2(k,j)*cos(theta);
        if (nshell > binF/10)
            u1ur(nshell) = 0;
        end
        
    end
end

for k=1:nz
    for j=1:ny
        Rmag   = ((y(j))^2+(z(k))^2)*binF;
        nshell = round(sqrt(Rmag))+1;
        ske(nshell) = Shear(nshell)*kinetic(nshell)/rdiss(nshell);
        if (nshell > binF/7) 
            ske(nshell) = 0;
        end
    end
end


% Normalize values to get averages
r=r/sqrt(binF);      
kinetic=kinetic./counter;
rdiss=rdiss./counter;
rprod=rprod./counter;

u1ur=u1ur./counter;
u1ur=u1ur./kinetic;
ru1u3=ru1u3./counter;
ru1u3=ru1u3./kinetic;

u1r=u1r./counter;
u2r=u2r./counter;
u3r=u3r./counter;

for k=1:nz
    for j=1:ny
        Rmag   = ((y(j))^2+(z(k))^2)*binF;
        nshell = round(sqrt(Rmag))+1;
        
        if (nshell > binF/5) 
            u1ur(nshell) = 0;
        end
 
    end
end

eta=eta./counter;
Shear=Shear./counter;
ske=ske./counter;

if (intproddiss==1)
%%%%%%%%%%%%%%%% INTEGRATED PRODUCTION AND DISSIPATION %%%%%%%%%%%%%%%% 
    intProd  = 0.0;
    intDiss  = 0.0;
    for k=1+ks:length(z)-ks
        for j=1+js:length(y)-js
            dA = (z(k+1)-z(k) )*(y(j+1)-y(j));
            intProd = intProd + dA*( prod12(k,j)+prod13(k,j) );
            intDiss = intDiss + dA*diss(k,j);
        end
    end
    tsX(count) = x;
    tsProd(count)=intProd;
    tsDiss(count)=intDiss;
end % if intproddiss 

if (pl2d==1)
%%%%%%%%%%%%%%%% CONTOUR PLOTS OF VARIABLES %%%%%%%%%%%%%%%% 
figure; set(gcf,'color',[1 1 1])

subplot(4,2,1) ; 
imagesc(y,z,du1dx3); 
colorbar; 
title('du1dx3');
xlabel('x2');
ylabel('x3');

subplot(4,2,2)
imagesc(y,z,du1dx2); 
colorbar; 
title('du1dx2');
xlabel('x2');
ylabel('x3');

subplot(4,2,3)
imagesc(y,z,du1dr); 
colorbar; 
title('du1dr');
xlabel('x2');
ylabel('x3');

subplot(4,2,4)
imagesc(y,z,u1u3); 
colorbar; 
title('u1u3');
xlabel('x2');
ylabel('x3');

subplot(4,2,5)
imagesc(y,z,u1u2); 
colorbar; 
title('u1u2');
xlabel('x2');
ylabel('x3');

subplot(4,2,6)
imagesc(y,z,KIN); 
colorbar; 
title('Kinetic');
xlabel('x2');
ylabel('x3');

subplot(4,2,7)
imagesc(y,z,diss); 
colorbar; 
title('Dissipation');
xlabel('x2');
ylabel('x3');

subplot(4,2,8)
imagesc(y,z,u1p); 
colorbar; 
title('u1rms');
xlabel('x2');
ylabel('x3');
end % if pl2d

if (savejpgs == 1)
%%%%%%%%%%%%%%%% PLOT VARIABLES AS A FUNCTION OF R %%%%%%%%%%%%%%%% 
figure; set(gcf,'color',[1 1 1])

subplot(4,3,1)
plot(r,u1r)
title('u1 rms');

subplot(4,3,2)
plot(r,u2r)
title('u2 rms');

subplot(4,3,3)
plot(r,u3r)
title('u3 rms');

subplot(4,3,4)
guideline=r; guideline(:)=0.25;
plot(r,u1ur); hold on; plot(r,guideline,'r');
title('u1ur');

subplot(4,3,5)
plot(r,ru1u3)
title('u1u3 (r)');

subplot(4,3,6)
plot(r,kinetic)
title('Kinetic');

subplot(4,3,7)
plot(r,rdiss)
title('Dissipation');

subplot(4,3,8)
plot(r,rprod)
title('Production');

subplot(4,3,9)
plot(r,eta)
title('eta');

subplot(4,3,10)
plot(r,Shear)
title('Shear');

subplot(4,3,11)
plot(r,ske)
title('Sk/\epsilon');


subplot(4,3,12)
t=r;
t(:)=iter;
itTitle = ['Iteration at time= ',num2str(x)];
plot(r,t)
title(itTitle);

% label in an orderly manner
temp = int2str(iter);
if (iter < 10000)
    temp=['0',temp];
end
if (iter < 1000)
    temp=['0',temp];
end
if (iter < 100) 
    temp=['0',temp];
end
if (iter < 10) 
    temp=['0',temp];
end

% adjust size of image saved
set(gcf, 'PaperPositionMode', 'manual');
set(gcf, 'PaperUnits', 'inches');
set(gcf, 'PaperPosition', [0.5 0 15 11]); % [horiz. margin, bottom margin, image width, image height]
                                          % The dimensions don't seem to be preserved

% adjust figure properties for all the subplots
h = get(gcf,'Children');
for i = 1:length(h)
    set(h(i),'xlim',[0 rplotmax]);			
    set(h(i),'FontSize',12,'FontWeight','bold')		% size of ticks on axes 
    set(get(h(i),'xlabel'),'String','r','Fontsize',14,'FontWeight','bold');
    set(get(h(i),'title'),'Fontsize',14,'FontWeight','bold');
end
 outname =[dir,'jpgs/',temp,'.jpg'];
% -r150 is the default, this can not be changed when running with -nodisplay
 print('-djpeg',outname,'-r150') % Save a jpeg version of the current figure
%  close all
end %if savejpgs

if (tracktkecomps ==1) % 
%%%%%%%%%%%%%%%% CALCULATE TKE COMPONENTS %%%%%%%%%%%%%%%% 
    xoft(count)=x;
    tke(count)=0.;
    uuint(count)=0.;
    vvint(count)=0.;
    wwint(count)=0.;
    u1max(count)=0.0;
    u2max(count)=0.0;
    u3max(count)=0.0;
    tkemax(count)=0.0;
    for k=zmin+ks:zmax-ks
        for j=ymin+js:ymax-js
            dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
            ktemp = 0.5*dA*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j));
            tke(count) = tke(count) + ktemp;
            uuint(count) = uuint(count) + dA*(u1p(k,j)*u1p(k,j));
            vvint(count) = vvint(count) + dA*(u2p(k,j)*u2p(k,j));
            wwint(count) = wwint(count) + dA*(u3p(k,j)*u3p(k,j));
            if (ktemp > tkemax(count))
                tkemax(count)=ktemp;
            end
            if ( u1p(k,j) > u1max(count) )
               u1max(count)=u1p(k,j);
            end
            if ( u2p(k,j) > u2max(count) )
               u2max(count)=u2p(k,j);
            end
            if ( u3p(k,j) > u3max(count) )
               u3max(count)=u3p(k,j);
            end
        end
    end


end % track tke variables
 
 % variation of variables with count
 u1u2overk(count) = max(max(abs(u1u2)))/max(max(KIN));
 u1u3overk(count) = max(max(abs(u1u3)))/max(max(KIN));
 u1rmax(count) = max(abs(u1r));
 kmax(count) = max(max(KIN));
 u2rmax(count) = max(abs(u2r));
 u3rmax(count) = max(abs(u3r));
 skemax(count) = max(abs(ske));
 u1urmax(count) = max(abs(u1ur));
 count = count+1;
end % time loop

if (intproddiss==1)
    figure; plot(tsX, tsProd, 'b'); hold on; plot(tsX, tsDiss, 'g');
    xlabel('x'); legend('Integrated Production','Integrated Dissipation')
    outname =[dir,'jpgs/integratedproddiss.jpg'];
    print('-djpeg',outname) % Save a jpeg version of the current figure
end % if intproddiss

if (tracktkecomps == 1)
for i = 1:count-1
    uuoverk(i)=uuint(i)/tke(i);
    vvoverk(i)=vvint(i)/tke(i);
    wwoverk(i)=wwint(i)/tke(i);
end

figure; subplot(2,2,1); plot(xoft,uuoverk,'b');
xlabel('x/D'); ylabel('u1p^2/k');
subplot(2,2,2); plot(xoft,vvoverk,'b');
xlabel('x/D'); ylabel('u2p^2/k');
subplot(2,2,3); plot(xoft,wwoverk,'b');
xlabel('x/D'); ylabel('u3p^2/k');
subplot(2,2,4); plot(xoft,tke,'b');
xlabel('x/D'); ylabel('integrated tke');

figure; subplot(2,2,1); plot(xoft,u1max);
xlabel('x/D'); ylabel('max(u1)');
subplot(2,2,2); plot(xoft,u2max);
xlabel('x/D'); ylabel('max(u2)');
subplot(2,2,3); plot(xoft,u3max);
xlabel('x/D'); ylabel('max(u3)');
subplot(2,2,4); plot(xoft,tkemax);
xlabel('x/D'); ylabel('max(tke)');

end % tracktkecomps

% % plot rho' at the last timestep
% var='rhop';
% basename=[dir,var,'_'];
% fname=Cfilename(basename,iter);
% [x y z rhop] = read_vtk(fname,1,Tplot);
% figure; imagesc(y,z,rhop); 
