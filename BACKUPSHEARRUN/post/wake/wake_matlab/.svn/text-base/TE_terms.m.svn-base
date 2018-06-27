%@t
% \textbf{TE\_terms.m}
%@h
%   Description:
%     Calculates the terms in the kinetic energy budget.
%@q

%REQUIRES U1, U2, U3, u1p, u2p, u3p, rhop

clear all; clc; close all; format long;

%DIRECTORY CONTAINING DATA
%dir='/work/temp/statrefine/';
dir='/work/temp/statrelax9.25/';


%OUTPUT FILE NAME
%fname=[baseDIR,'./Te_terms_SP_test_2.dat'];

fname=[dir,'Energybreakdown.dat'];
TimeFile=fopen(fname,'wt');

%SPONGE GRID POINTS NEAR x2min,x2max and x3min,x2max TO BE EXCLUDED FROM INTEGRAL 
js=26;
ks=13;

% drhodz=0.006371;
%D=1.0; % Is this the diameter Kyle???
Fr=2; 

U=1.0; rho0 = 1.0; g=9.81; D=1.0; % U=Free stream velocity, D=sphere diameter
drhodz=U^2*rho0 / (g*Fr^2*D^2);
Fact=(Fr^2*(D*drhodz)^2); % What is fact Kyle??? Why is this used???

%X2
ymin=1;
%ymax=386;
ymax=258;
%X3
zmin=1;
%zmax=192;
%zmax=258;
zmax=130;

%TIME
imin=10;
imax=300;
%imax=2500;
iskip=10;

file_2=fopen(fname,'wt');

for iter=imin:iskip:imax
    var='u1p'; %<u1'u1'>^(1/2)
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u1p] = read_vtk(fname,1,0); 

    var='u2p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u2p] = read_vtk(fname,1,0); 

    var='u3p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u3p] = read_vtk(fname,1,0); 

    var='rhop';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z rp] = read_vtk(fname,1,0); 

    var='U1';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U1] = read_vtk(fname,1,0);

    var='U2';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U2] = read_vtk(fname,1,0);

    var='U3';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U3] = read_vtk(fname,1,0);

    tke = 0.0;
    mke = 0.0;
    tpe = 0.0;
    Umax= 0.0;
    u1max=0.0;
    u2max=0.0;
    u3max=0.0;
    kmax=0.0;
    Umax=max(max(U1));
    
    for k=zmin+ks:zmax-ks
        for j=ymin+js:ymax-js
            dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
            tke = tke + 0.5*dA*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j));
            mke = mke + 0.5*dA*(U1(k,j)*U1(k,j)+ U2(k,j)*U2(k,j)+ U3(k,j)*U3(k,j));
            tpe = tpe + 0.5*dA/Fact*(rp(k,j)*rp(k,j));
        end
    end
    total=tpe+tke+mke;
    tkep=tke/total;
    mkep=mke/total;
    tpep=tpe/total;
    time=x(1);
    fprintf(TimeFile,'%13.8G    %13.8G    %13.8G    %13.8G    %13.8G   %13.8G   %13.8G   %13.8G   %13.8G\n',time,tke,mke,tpe,tkep,mkep,tpep,total,Umax);
end
xx=fclose(TimeFile);
