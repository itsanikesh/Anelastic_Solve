%@t
% \textbf{diss\_terms.m}
%@h
%   Description:
%     Calculates the integrated dissipation and writes it to a file.
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
%     KYLE Can you add some comments on how to use this?
%@q

clear;
format long;
Tplot=0;
ymin=1;
%ymax=1026;
ymax=514
zmin=1;
%zmax=514;
zmax=258

imin=1525;
imax=3420;
iskip=25;

js=26;
ks=26;
nvars=12;

dir='~/work2/T_Large/stat2/results/';
TimeFile=fopen('./diss_termsTt_2.dat','wt');
%Initialize storage arrays in time
for jj=1:nvars
    counter=1;
    for iter=imin:iskip:imax
        ts1(jj,counter) = 0;
        counter=counter+1;
    end
end

counter=1;
for iter=imin:iskip:imax
    
var='eps_11';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps11] = read_vtk(fname,1,Tplot);

var='eps_12';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps12] = read_vtk(fname,1,Tplot);

var='eps_13';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps13] = read_vtk(fname,1,Tplot);

var='eps_21';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps21] = read_vtk(fname,1,Tplot);

var='eps_22';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps22] = read_vtk(fname,1,Tplot);

var='eps_23';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps23] = read_vtk(fname,1,Tplot);

var='eps_31';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps31] = read_vtk(fname,1,Tplot);

var='eps_32';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps32] = read_vtk(fname,1,Tplot);

var='eps_33';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z eps33] = read_vtk(fname,1,Tplot);

var='Seps1';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z s1] = read_vtk(fname,1,Tplot);

var='Seps2';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z s2] = read_vtk(fname,1,Tplot);

var='Seps3';
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
sum1=0;
sum2=0;

for k=zmin+ks:zmax-1-ks
    for j=ymin+js:ymax-1-js
      dA = (z(k+1)-z(k) )*(y(j+1)-y(j));
      int1 = int1 + dA*eps11(k,j);
      int2 = int2 + dA*eps12(k,j);
      int3 = int3 + dA*eps13(k,j);
      int4 = int4 + dA*eps21(k,j) ;     
      int5 = int5 + dA*eps22(k,j);
      int6 = int6 + dA*eps23(k,j);
      int7 = int7 + dA*eps31(k,j);
      int8 = int8 + dA*eps32(k,j);
      int9 = int9 + dA*eps33(k,j);
      int10 = int10 + dA*s1(k,j);
      int11 = int11 + dA*s2(k,j);
      int12 = int12 + dA*s3(k,j);

    end
end
sum1 = int1 + int2 + int3 + int4 + int5 + int6 + int7 + int8 + int9;
sum2 = int10 + int11 + int12;

ts(1,counter)= int1/sum1;
ts(2,counter) = int2/sum1;
ts(3,counter)= int3/sum1;
ts(4,counter)= int4/sum1;
ts(5,counter)= int5/sum1;
ts(6,counter)= int6/sum1;
ts(7,counter)= int7/sum1;
ts(8,counter)= int8/sum1;
ts(9,counter)= int9/sum1;
ts(10,counter)= int10/sum2;
ts(11,counter)= int11/sum2;
ts(12,counter)= int12/sum2;
tstime(counter)=x(1);
%fprintf(file_2,'%13.8G    %13.8G   %13.8G    %13.8G    %13.8G    %13.8G    %13.8G    %13.8G    %13.8G     %13.8G    %13.8G    %13.8G    %13.8G\n',x(1), int1, int2, int3, int4, int5, int6, int7, int8, int9, int10, int11, int12);
counter=counter+1;
end

counter=counter-1;
for jj=1:nvars
fprintf(TimeFile,'%1c\n','&');
    for ii=1:counter
        fprintf(TimeFile,'%13.8G  %13.8G\n',tstime(ii),ts(jj,ii));
    end
fprintf(TimeFile,'%1c\n','&');
end
close(file_2);
close all
