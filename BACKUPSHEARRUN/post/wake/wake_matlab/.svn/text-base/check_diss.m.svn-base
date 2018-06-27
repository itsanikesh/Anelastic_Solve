%@t
% \textbf{check\_diss.m}
%@h
%   Description:
%     Compares the dissipation between two files.
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
warning('off','MATLAB:dispatcher:InexactMatch')


dir1='~/work2/T_Large/stat/results/';
dir2='~/work2/SP_Large/stat/results/';


ymin=1;
ymax=1026;
%ymax=514;
%ymax=260;
zmin=1;
zmax=514;
%zmax=258;
%zmax=132;
Tplot=0;
js=13;
ks=13;

Titer=550;
SPiter=800;

y1=-2;
y2=2;
z1=-2;
z2=2;

vard1='omg1p';
vard2='omg2p';
vard3='omg3p';

var2='DISS';

var3='EpsSUM';


basename=[dir1,vard1,'_'];
fname=Cfilename(basename,Titer);
[x y z Tvard1] = read_vtk(fname,1,Tplot); 
basename=[dir1,vard2,'_'];
fname=Cfilename(basename,Titer);
[x y z Tvard2] = read_vtk(fname,1,Tplot); 
basename=[dir1,vard3,'_'];
fname=Cfilename(basename,Titer);
[x y z Tvard3] = read_vtk(fname,1,Tplot); 
basename=[dir1,var2,'_'];
fname=Cfilename(basename,Titer);
[x y z Tvar2] = read_vtk(fname,1,Tplot); 
Tvar1=Tvard1.^2+Tvard2.^2+Tvard3.^2;
basename=[dir1,var3,'_'];
fname=Cfilename(basename,Titer);
[x y z Tvar3] = read_vtk(fname,1,Tplot); 

basename=[dir2,vard1,'_'];
fname=Cfilename(basename,Titer);
[x y z SPvard1] = read_vtk(fname,1,Tplot); 
basename=[dir2,vard2,'_'];
fname=Cfilename(basename,Titer);
[x y z SPvard2] = read_vtk(fname,1,Tplot); 
basename=[dir2,vard3,'_'];
fname=Cfilename(basename,Titer);
[x y z SPvard3] = read_vtk(fname,1,Tplot); 
basename=[dir2,var2,'_'];
fname=Cfilename(basename,Titer);
[x y z SPvar2] = read_vtk(fname,1,Tplot); 
SPvar1=SPvard1.^2+SPvard2.^2+SPvard3.^2;
basename=[dir2,var3,'_'];
fname=Cfilename(basename,Titer);
[x y z SPvar3] = read_vtk(fname,1,Tplot);

int1=0;
int2=0;
int1=0;
int2=0;
int3=0;
int4=0;
int5=0;
int6=0;

for k=zmin+ks:zmax-ks
    for j=ymin+js:ymax-js
      dA = (z(k+1)-z(k) )*(y(j+1)-y(j));
      int1 = int1 + dA*Tvar1(k,j);
      int2 = int2 + dA*SPvar1(k,j);
      int3 = int3 + dA*Tvar2(k,j)/2;
      int4 = int4 + dA*SPvar2(k,j)/2;
      int5 = int5 + dA*Tvar3(k,j);
      int6 = int6 + dA*SPvar3(k,j);
    end
end

Tval1=int1/50000
Tval2=int3
Tval3=int5/50000
SPval1=int2/50000
SPval2=int4
Spval3=int6/50000
r1=Tval1/Tval2
r2=SPval1/SPval2
