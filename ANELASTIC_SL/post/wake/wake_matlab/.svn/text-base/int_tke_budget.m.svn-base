%@t
% \textbf{int\_tke\_budget.m}
%@h
%   Description:
%     Program to calculate the Turbulent Kinetic Energy Budget <x2 x3> for
%     Stratified Wakes.

%   Comments:
%     This assumes that all data is equally spaced in time and that 
%     dTRANSdx2, dTRANSdx3, PROD12, PROD13, BFLUX, and DISS are correct.
%     See int_tke_budget_kyle_specialversion.m for an example where this 
%     is not the case.
%@q

%Kyle A. Brucker
%v.0 09/2008
%Matt de Stadler
%v.1 06/2009

%REQUIRES single precision .vtk files for
%dTRANSdx2 %dTRANSdx3 %PROD12 %PROD13 %BFLUX %DISS

clear all; close all; clc; format long;

%DIRECTORY CONTAINING DATA
%baseDIR='~/work2/T_Large';
%dir=[baseDIR,'/stat/results/'];
%dir='/work/temp/statrefine/';
%dir='/work/temp/statrelax9.25/';
dir='/work/temp/statRe6000/';

%OUTPUT FILE NAME
fname=[dir,'int_tke_budget.dat'];
TimeFile=fopen(fname,'wt');

%SPONGE GRID POINTS NEAR x2min,x2max and x3min,x2max TO BE EXCLUDED FROM
%INTEGRAL (especially important for Transport)
js=26;
ks=13;

%X2 bounds
jmin=1;
%jmax=258;
jmax=386;

%X3 bounds
kmin=1;
%kmax=130;
kmax=194;

%TIME
imin=10;
imax=3000;
%imax=2500;
iskip=10;

%Parameters
Fr=2; % this is not actually used...
Re=10000;
t0=6; % virtual origin

%Loop over Dumps and compute the budget
counter=1;
for iter=imin:iskip:imax
    var='dTRANSdx2';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z dtdx2] = read_vtk(fname,1,0);

    var='dTRANSdx3';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z dtdx3] = read_vtk(fname,1,0);

    var='PROD13';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z p13] = read_vtk(fname,1,0);

    var='PROD12';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z p12] = read_vtk(fname,1,0);   
    
    var='DISS';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z diss] = read_vtk(fname,1,0);
    
    var='BFLUX';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z bflux] = read_vtk(fname,1,0);

    intProd12  = 0.0; 
    intProd13  = 0.0; 
    intDiss  = 0.0;
    intBflux = 0.0;
    intTrans2 = 0.0;
    intTrans3 = 0.0;

    % Calculate integrated quantities
    for k=kmin+ks:kmax-ks
        for j=jmin+js:jmax-js
            dA = (z(k+1)-z(k) )*(y(j+1)-y(j));
            intProd12 = intProd12 + dA*(p12(k,j));
            intProd13 = intProd13 + dA*(p13(k,j));
            intDiss = intDiss + dA*diss(k,j);
            intBflux = intBflux + dA*bflux(k,j) ;     
            intTrans2 = intTrans2 + dA*(dtdx2(k,j));
            intTrans3 = intTrans3 + dA*(dtdx3(k,j));
        end
    end

% Adjust timings for calculating dk/dt
      if (iter==imin)
          iterm=iter;
          iterp=iter+iskip;
      elseif (iter==imax)
          iterp=iter;
          iterm=iter-iskip;
      else
          iterm=iter-iskip;
          iterp=iter+iskip;
      end

    var='u1p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterm);
    [x1 y z u1p] = read_vtk(fname,1,0); 

    var='u2p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterm);
    [x1 y z u2p] = read_vtk(fname,1,0); 

    var='u3p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterm);
    [x1 y z u3p] = read_vtk(fname,1,0); 
    
    % TKE before
    t1=x1(1);
    tke1=0.0;
   
    for k=kmin+ks:kmax-ks
        for j=jmin+js:jmax-js
            dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
            tke1 = tke1 + 0.5*dA*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j));
        end
    end

    var='u1p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterp);
    [x2 y z u1p] = read_vtk(fname,1,0); 

    var='u2p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterp);
    [x2 y z u2p] = read_vtk(fname,1,0); 

    var='u3p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterp);
    [x2 y z u3p] = read_vtk(fname,1,0);  

    % TKE after
    t2=x2(1);
    tke2=0;

    for k=kmin+ks:kmax-ks
        for j=jmin+js:jmax-js
            dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
            tke2 = tke2 + 0.5*dA*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j));
        end
    end
    dkdt=(tke2-tke1)/(t2-t1);

    time=x(1);
    tsP13(counter) = intProd13;
    tsP12(counter) = intProd12;
    tsD(counter) = intDiss;
    tsB(counter) = intBflux;
    tsT2(counter) = intTrans2;
    tsT3(counter) = intTrans3;
    tstime(counter) = time+t0;
    tsdkdt(counter) = dkdt;
    counter=counter+1;
    % note that dissipation and transport are usually plotted as negative quantities
    fprintf(TimeFile,'%13.8G %13.8G %13.8G %13.8G %13.8G %13.8G\n',time, dkdt, intProd12+intProd13, -(intDiss), intBflux, -(intTrans2+intTrans3));
end

xx=fclose(TimeFile);
