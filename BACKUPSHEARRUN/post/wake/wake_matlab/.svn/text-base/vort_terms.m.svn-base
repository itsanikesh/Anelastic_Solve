%@t
% \textbf{vort\_terms.m}
%@h
%   Description:
%     Calculates vorticity terms??? KYLE why does this take in all the eps values?
%@q

clear;
format long;
Tplot=0;
ymin=1;
ymax=1026;
zmin=1;
zmax=514;
imax=1675;
dir='~/work2/T_Large/stat/results/';
file_2=fopen('./diss_terms.dat','wt');

for iter=0:25:imax
    
var='eps11';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z e11] = read_vtk(fname,1,Tplot);

var='eps12';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps12] = read_vtk(fname,1,Tplot);

var='eps13';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps13] = read_vtk(fname,1,Tplot);

var='eps21';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps21] = read_vtk(fname,1,Tplot);

var='eps22';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps22] = read_vtk(fname,1,Tplot);

var='eps23';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps23] = read_vtk(fname,1,Tplot);

var='eps31';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps31] = read_vtk(fname,1,Tplot);

var='eps32';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps32] = read_vtk(fname,1,Tplot);

var='eps33';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps33] = read_vtk(fname,1,Tplot);

var='seps1';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z s1] = read_vtk(fname,1,Tplot);

var='seps2';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z s2] = read_vtk(fname,1,Tplot);

var='seps3';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z s3] = read_vtk(fname,1,Tplot);

int1 =0; 
int2 =0;
int3 =0;
int4 =0;
int5 =0;
int6 =0;
int7 =0;
int8 =0;
int9 =0;
int10=0;
int11=0;
int12=0;
int13=0;

for k=zmin:zmax-1
    for j=ymin:ymax-1
      dA = (z(k+1)-z(k) )*(y(j+1)-y(j));
      int1 = int1 + dA*p12(k,j);
      int2 = int2 + dA*p13(k,j);
      int3 = int3 + dA*diss(k,j);
      int4 = int4 + dA*buoy(k,j) ;     
      int5 = int5 + dA*(dtdx2(k,j));
      int6 = int6 + dA*(dtdx3(k,j));
      int7 = int7 + 0.5*dA*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j));
      k2(k,j) = 0.5*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j));
      int10 = int10 + dA*DVD(k,j);
      int11 = int11 + dA*DVT(k,j);
      int12 = int12 + dA*DVP(k,j);
      int13 = int13 + dA*rhop(k,j)*rhop(k,j);
    end
end

for k=zmin+1:zmax-1
    for j=ymin+1:ymax-1
      dkdx2(k,j) = ( k2(j+1)-k2(j-1) ) / ( y(j+1)-y(j-1) );
      dkdx3(k,j) = ( k2(k+1)-k2(k-1) ) / ( z(k+1)-z(k-1) );
    end
end

for k=zmin:zmax-1
    for j=ymin:ymax-1
      dA= (z(k+1)-z(k) )*(y(j+1)-y(j));
       int8 =int8 + dA*(U2(k,j)*dkdx2(k,j));
       int9 =int9 + dA*(U3(k,j)*dkdx3(k,j));
    end
end

       
n=(iter/25)+1;
P(n)  = int1+int2;
D(n)  = int3;
B(n)  = int4;
T2(n) = int5;
T3(n) = int6;
time(n) = x(1);
K(n) =int7;
E(n) = int8+int9;
PE(n) = int13;
Ppe(n) = int12;
Dpe(n) = int10;
Tpe(n) = int11;

fprintf(file_2,'%13.6G %13.6G %13.6G %13.6G %13.6G %13.6G %13.6G %13.6G %13.6G\n',x(1), int1, int2, int3, int4, int5, int6, int7, int13);

end
for iter=0:25:imax-50
 n=(iter/25)+2;
 dk(n) = ( K(n+1)-K(n-1) ) / ( time(n+1)-time(n-1) );
 dpe(n) =( PE(n+1)-PE(n-1) ) / ( time(n+1)-time(n-1) );
end
 dk(n+1)=0;
 dpe(n+1)=0;

 int14=0;
 int15=0;
 int16=0;
 for n=1,size(E)-1
     int14 = int14 + P(n)*( time(n+1)-time(n) );
     int15 = int15 + D(n)*( time(n+1)-time(n) );
     int16 = int16 + B(n)*( time(n+1)-time(n) );
 end
 int14
 int15
 int16
 fprintf(file_2,'%13.6G %13.6G %13.6G\n',int14, int15, int16);

remdr = P+D+B+T2+T3;
plot(time,dk,time,remdr);
outname =['./remainder.jpg']
print('-djpeg',outname)
 
 close all
