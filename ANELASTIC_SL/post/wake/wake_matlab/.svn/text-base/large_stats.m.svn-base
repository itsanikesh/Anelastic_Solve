%@t
% \textbf{large\_stats.m}
%@h
%   Description:
%     Calculates results from the small statistics package and computes 
%     integrated values and writes everything to a text file. 
%@q

% KYLE WHAT IS THIS AND WHY IS IT USED?

clear;
format long;
Tplot=0;
pl2d=0;

dir='~/work/T_Re50000_Fr0/stat2/results/';

ymin=1;
%ymax=1026;
%ymax=514;
ymax=258;
zmin=1;
%zmax=514;
zmax=258;

%UTowed
%part1
imin=1005;
imax=1800;
istep=5;
%UTowed
%part2

%Towed
%part1
%imin=0;
%imax=1675;
%istep=10;
%part2
%imin=1510;
%imax=3420;
%istep=5;

%SP
%part1
%imin=0;
%imax=1640;
%istep=5;
%part2
%imin=1505;
%imax=2865;
%istep=5;


file_2=fopen('./UT_2.dat','wt');

for iter=imin:istep:imax

var='u1p'; %<u1'u1'>^(1/2)
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

KIN= .5*(u1p.^2+u2p.^2+u3p.^2);

var='rhop';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z rhop] = read_vtk(fname,1,Tplot);

var='U1';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z U1] = read_vtk(fname,1,Tplot);

var='U2';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z U2] = read_vtk(fname,1,Tplot);

var='U3';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z U3] = read_vtk(fname,1,Tplot);

var='RHO';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z RHO] = read_vtk(fname,1,Tplot);

for k=zmin+1:zmax-1
    for j=ymin:ymax-1
        dRHOdz(k,j) = ( RHO(k+1,j)-RHO(k-1,j) )/(z(k+1)-z(k-1));
    end
end

for j=ymin:ymax-1
 dRHOdz(zmin,j) = dRHOdz(zmin+1,j);
 dRHOdz(zmax,j) = dRHOdz(zmax-1,j);
end
    

intM =0; 
intT =0;
Umax=0;
kmax=0;
u1max=0;
u2max=0;
u3max=0;
rhomax=0;
int1=0;
int2=0;
int3=0;
int4=0;


for k=zmin+26:zmax-26
    for j=ymin+26:ymax-26
      dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
      intT = intT + 0.5*dA*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j)); % integrated tke
      intM = intM + 0.5*dA*(U1(k,j)*U1(k,j)+ U2(k,j)*U2(k,j)+ U3(k,j)*U3(k,j)); % integrated mean ke
      int1 = int1 + 0.5*dA*u1p(k,j)*u1p(k,j);
      int2 = int2 + 0.5*dA*u2p(k,j)*u2p(k,j);
      int3 = int3 + 0.5*dA*u3p(k,j)*u3p(k,j);
      int4 = int4 + dA*rhop(k,j)*rhop(k,j);
      if (U1(k,j) > Umax) 
          Umax=U1(k,j);
      end
      ktemp=0.5*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j));
      if (ktemp > kmax) 
      kmax=ktemp;
      end
      if ( u1p(k,j) > u1max ) 
          u1max=u1p(k,j);
      end
      if ( u2p(k,j) > u2max ) 
       u2max=u2p(k,j);
      end
      if ( u3p(k,j) > u3max ) 
       u3max=u3p(k,j);
      end
      if ( rhop(k,j) > rhomax )
       rhomax=rhop(k,j);
      end
    end
end

[cx,cy,sx,sy] = centerofmass(U1);
PeakOD = max(max(U1));

fprintf(file_2,'%13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G\n',x(1),Umax, kmax, intM, intT,sx,sy,int1,int2,int3,int4,u1max,u2max,u3max,rhomax);

end
close all;

