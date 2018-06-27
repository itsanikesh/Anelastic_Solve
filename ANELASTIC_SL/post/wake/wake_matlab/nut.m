%@t
% \textbf{mke\_tke.m}
%@h
%   Description:
%     Calculates the turbulent diffusivity and Cmuk/E and generates plots.
%@q

% KYLE WHAT IS THIS AND WHY IS IT USED?

clear all; clc; close all; format long;
Tplot=0;
ymin=1;
ymax=1026;
zmin=1;
zmax=514;
imax=1625;
g=9.81;
rho_0=1.0;
Cmu=0.09;
nut_max=.5;
iterT = 125;
iterSP= 125;
ycl=floor((ymax+ymin)/2)+1;
zcl=floor((zmax+zmin)/2)+1;

dirSP='~/work2/SP_Large/stat/results/';
dirT='~/work2/T_Large/stat/results/';
iter=iterT;
%for iter=0:25:imax
%TOWED
var='u1pu2p';
basename=[dirT,var,'_'];
fname=Cfilename(basename,iter);
[x y z u1pu2pT] = read_vtk(fname,1,Tplot);

var='u1pu3p';
basename=[dirT,var,'_'];
fname=Cfilename(basename,iter);
[x y z u1pu3pT] = read_vtk(fname,1,Tplot);

var='dU1dx2';
basename=[dirT,var,'_'];
fname=Cfilename(basename,iter);
[x y z du1dx2T] = read_vtk(fname,1,Tplot);

var='dU1dx3';
basename=[dirT,var,'_'];
fname=Cfilename(basename,iter);
[x y z du1dx3T] = read_vtk(fname,1,Tplot);

var='DISS';
basename=[dirT,var,'_'];
fname=Cfilename(basename,iter);
[x y z dissT] = read_vtk(fname,1,Tplot);

var='u1p';
basename=[dirT,var,'_'];
fname=Cfilename(basename,iter);
[x y z u1pT] = read_vtk(fname,1,Tplot);

var='u2p';
basename=[dirT,var,'_'];
fname=Cfilename(basename,iter);
[x y z u2pT] = read_vtk(fname,1,Tplot);

var='u3p';
basename=[dirT,var,'_'];
fname=Cfilename(basename,iter);
[xT y z u3pT] = read_vtk(fname,1,Tplot);

kineticT= .5*(u1pT.^2+u2pT.^2+u3pT.^2);

%SELF-PROPELLED
iter=iterSP;
var='u1pu3p';
basename=[dirSP,var,'_'];
fname=Cfilename(basename,iter);
[x y z u1pu3pSP] = read_vtk(fname,1,Tplot);

var='dU1dx2';
basename=[dirSP,var,'_'];
fname=Cfilename(basename,iter);
[x y z du1dx2SP] = read_vtk(fname,1,Tplot);

var='dU1dx3';
basename=[dirSP,var,'_'];
fname=Cfilename(basename,iter);
[x y z du1dx3SP] = read_vtk(fname,1,Tplot);

var='u1pu2p';
basename=[dirSP,var,'_'];
fname=Cfilename(basename,iter);
[x y z u1pu2pSP] = read_vtk(fname,1,Tplot);

var='DISS';
basename=[dirSP,var,'_'];
fname=Cfilename(basename,iter);
[x y z dissSP] = read_vtk(fname,1,Tplot);

var='u1p';
basename=[dirSP,var,'_'];
fname=Cfilename(basename,iter);
[x y z u1pSP] = read_vtk(fname,1,Tplot);

var='u2p';
basename=[dirSP,var,'_'];
fname=Cfilename(basename,iter);
[x y z u2pSP] = read_vtk(fname,1,Tplot);

var='u3p';
basename=[dirSP,var,'_'];
fname=Cfilename(basename,iter);
[xSP y z u3pSP] = read_vtk(fname,1,Tplot);

kineticSP= .5*(u1pSP.^2+u2pSP.^2+u3pSP.^2);

%u1'u2'
u1u2_H_SP=u1pu2pSP(zcl,:);
u1u2_V_SP=u1pu2pSP(:,ycl);
u1u2_H_T=u1pu2pT(zcl,:);
u1u2_V_T=u1pu2pT(:,ycl);

%u1'u3'
u1u3_H_SP=u1pu3pSP(zcl,:);
u1u3_V_SP=u1pu3pSP(:,ycl);
u1u3_H_T=u1pu3pT(zcl,:);
u1u3_V_T=u1pu3pT(:,ycl);

%dU1dx2
du1dx2_H_SP=du1dx2SP(zcl,:);
du1dx2_V_SP=du1dx2SP(:,ycl);
du1dx2_H_T=du1dx2T(zcl,:);
du1dx2_V_T=du1dx2T(:,ycl);

%dU1dx3
du1dx3_H_SP=du1dx3SP(zcl,:);
du1dx3_V_SP=du1dx3SP(:,ycl);
du1dx3_H_T=du1dx3T(zcl,:);
du1dx3_V_T=du1dx3T(:,ycl);

%nu_t
nut_1_H_SP=u1u2_H_SP./du1dx2_H_SP;
nut_1_V_SP=u1u2_V_SP./du1dx2_V_SP;
nut_2_H_SP=u1u3_H_SP./du1dx3_H_SP;
nut_2_V_SP=u1u3_V_SP./du1dx3_V_SP;
nut_1_H_T=u1u2_H_T./du1dx2_H_T;
nut_1_V_T=u1u2_V_T./du1dx3_V_T;
nut_2_H_T=u1u3_H_T./du1dx3_H_T;
nut_2_V_T=u1u3_V_T./du1dx3_V_T;

for zz = zmin:zmax
    if abs(nut_1_V_SP(zz)) >= nut_max
        nut_1_V_SP(zz) =0.0;
    end
    if abs(nut_2_V_SP(zz)) >= nut_max
        nut_2_V_SP(zz) =0.0;
    end
    if abs(nut_1_V_T(zz)) >= nut_max
        nut_1_V_T(zz) =0.0;
    end
    if abs(nut_2_V_T(zz)) >= nut_max
        nut_2_V_T(zz) =0.0;
    end
    if abs(z(zz) ) >=2.0
        nut_1_V_SP(zz) = 0.0;
        nut_2_V_SP(zz) = 0.0;
        nut_1_V_T(zz) = 0.0;
        nut_2_V_T(zz) = 0.0;
    end
    
end

for yy = ymin:ymax
    if abs(nut_1_H_SP(yy)) >= nut_max
        nut_1_H_SP(yy) =0.0;
    end
    if abs(nut_2_H_SP(yy)) >= nut_max
        nut_2_H_SP(yy) =0.0;
    end
    if abs(nut_1_H_T(yy)) >= nut_max
        nut_1_H_T(yy) =0.0;
    end
    if abs(nut_2_H_T(yy)) >= nut_max
        nut_2_H_T(yy) =0.0;
    end
    if abs(y(yy) ) >=2.0
        nut_1_H_SP(yy) = 0.0;
        nut_2_H_SP(yy) = 0.0;
        nut_1_H_T(yy) = 0.0;
        nut_2_H_T(yy) = 0.0;
    end
    
end        

%CmuK/epsilon
cmuke_H_SP=Cmu*kineticSP(zcl,:).^2./dissSP(zcl,:);
cmuke_V_SP=Cmu*kineticSP(:,ycl).^2./dissSP(:,ycl);
cmuke_H_T=Cmu*kineticT(zcl,:).^2./dissT(zcl,:);
cmuke_V_T=Cmu*kineticT(:,ycl).^2./dissT(:,ycl);

figure; set(gcf,'color',[1 1 1])

subplot(5,2,1);
imagesc(y,z,u1pu2pT) 
%colorbar;
title('<u1`u2`> T');
xlabel('x2');
ylabel('x3');

subplot(5,2,2);
imagesc(y,z,u1pu2pSP)
%colorbar;
title('<u1`u2`> SP');
xlabel('x2');
ylabel('x3');
 
subplot(5,2,3);
imagesc(y,z,u1pu3pT) 
%colorbar;
title('<u1`u3`> T');
xlabel('x2');
ylabel('x3');


subplot(5,2,4);
imagesc(y,z,u1pu3pSP)
%colorbar;
title('<u1`u3`> SP');
xlabel('x2');
ylabel('x3');
 
subplot(5,2,5)
plot(y,u1u2_H_SP,'g');
title('<u1`u2`>(x3=CL)');
xlabel('x2');
hold on
plot(y,u1u2_H_T,'r');
string1=[num2str(xSP(1))];
string2=[num2str(xT(1))];
legend(string1,string2)
subplot(5,2,6)
plot(z,u1u2_V_SP,'g');
title('<u1`u2`>(x2=CL)');
xlabel('x3');
hold on
plot(z,u1u2_V_T,'r');

subplot(5,2,7)
plot(y,u1u3_H_SP,'g');
title('<u1`u3`>(x3=CL)');
xlabel('x2');
hold on
plot(y,u1u3_H_T,'r');

subplot(5,2,8)
plot(z,u1u3_V_SP,'g');
title('<u1`u3`>(x2=CL)');
xlabel('x3');
hold on
plot(z,u1u3_V_T,'r');

subplot(5,2,9)
plot(y,cmuke_H_SP,'g');
title('Cmu<k^2/eps>(x3=CL)');
xlabel('x2');
hold on
plot(y,cmuke_H_T,'r');

subplot(5,2,10)
plot(z,cmuke_V_SP,'g');
title('Cmu<k^2/eps>(x2=CL)');
xlabel('x3');
hold on
plot(z,cmuke_V_T,'r');
outname =['./cors',int2str(iter),'.jpg'];
print('-djpeg',outname)


figure
subplot(2,2,1)
plot(y,nut_1_H_SP,'g');
title('<u1`u2`>/<du1dx2>(x3=CL)');
xlabel('x2');
hold on
plot(y,nut_1_H_T,'r');
string1=[num2str(xSP(1))];
string2=[num2str(xT(1))];
legend(string1,string2)

subplot(2,2,2)
plot(z,nut_1_V_SP,'g');
title('<u1`u2`>/<du1dx2>(x2=CL)');
xlabel('x3');
hold on
plot(z,nut_1_V_T,'r');

subplot(2,2,3)
plot(y,nut_2_H_SP,'g');
title('<u1`u3`>/<du1dx3>(x3=CL)');
xlabel('x2');
hold on
plot(y,nut_2_H_T,'r');

subplot(2,2,4)
plot(z,nut_2_V_SP,'g');
title('<u1`u3`>/<du1dx3>(x2=CL)');
xlabel('x3');
hold on
plot(z,nut_2_V_T,'r');

outname =['./nut',int2str(iter),'.jpg'];
print('-djpeg',outname)
%close all
%end
