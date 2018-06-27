%@t
% \textbf{contour\_var.m}
%@h
%   Description:
%     Produces a contour plots for a variable in two separate directories.
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


%dir1='~/work2/T_Large/stat2/results/';
%dir2='~/work2/SP_Large/stat2/results/';

dir1='~/work/TR10F02_stat/stat/results/';
dir2='~/work/SPR10F02_stat/stat/results/';
ymin=1;
%ymax=1026;
%ymax=514;
ymax=260;
zmin=1;
%zmax=514;
%zmax=258;
zmax=132;
Tplot=0;
js=13;
ks=13;

Titer=1900;
SPiter=900;

y1=-10;
y2=10;
z1=-6;
z2=6;

var1='U1';
var2='rhop';

basename=[dir1,var1,'_'];
fname=Cfilename(basename,Titer);
[x1 y z Tvar1] = read_vtk(fname,1,Tplot); 
basename=[dir1,var2,'_'];
fname=Cfilename(basename,Titer);
[x1 y z Tvar2] = read_vtk(fname,1,Tplot); 

basename=[dir2,var1,'_'];
fname=Cfilename(basename,SPiter);
[x2 y z SPvar1] = read_vtk(fname,1,Tplot); 

basename=[dir2,var2,'_'];
fname=Cfilename(basename,SPiter);
[x2 y z SPvar2] = read_vtk(fname,1,Tplot); 

%for k=zmin:zmax
%    for j=ymin:ymax
%        DIFF1(k,j) = Tvar1(k,j)/(1+Tvar2(k,j));
%        DIFF2(k,j) = SPvar1(k,j)/(1+SPvar2(k,j));
%    end
%end

Tvar1=(Tvar1); 
SPvar1=(SPvar1);

imagesc(y,z,Tvar1)
axis([y1 y2 z1 z2])
figure
imagesc(y,z,SPvar1)
axis([y1 y2 z1 z2])
figure
imagesc(y,z,Tvar2)
axis([y1 y2 z1 z2])
figure
imagesc(y,z,SPvar2)
axis([y1 y2 z1 z2])
%break

int1=0;
int2=0;
int3=0;
int4=0;

for k=zmin+ks:zmax-ks
    for j=ymin+js:ymax-js
      dA = (z(k+1)-z(k) )*(y(j+1)-y(j));
      int1 = int1 + dA*Tvar1(k,j)*Tvar1(k,j);
      int2 = int2 + dA*SPvar1(k,j)*SPvar1(k,j);
%      int1 = int1 + dA*Tvar1(k,j);
%      int2 = int2 + dA*SPvar1(k,j);
      int3 = int3 + dA*Tvar1(k,j);
      int4 = int4 + dA*SPvar2(k,j);
    end
end
%Tval=sqrt(int1)*2/50000
%SPval=sqrt(int2)*2/50000
%Tval=int1/50000
%SPval=int2/50000
x1
int1
x2
int2
%break
pause
close all;
break
h=gca;
set(h,'FontSize',18); 
set(h,'FontName','Times');
colormap gray;
[C,h]=contour(y,z,Tvar1,'LineWidth',2,'Color','black');
xlabel('@1')
ylabel('@2')
axis([y1 y2 z1 z2])
%clabel(C,h,[.1 .2 .3 .5 .7 .9],'FontSize',15,'Color','black','LabelSpacing',2312);

figure

h=gca;
set(h,'FontSize',18); 
set(h,'FontName','Times');
colormap gray;
[C,h]=contour(y,z,SPvar1,'LineWidth',2,'Color','black');
xlabel('@1')
ylabel('@2')
axis([y1 y2 z1 z2])
pause
close all
%clabel(C,h,[.1 .2 .3 .5 .7 .9],'FontSize',15,'Color','black','LabelSpacing',2312);
%outname = 'test.eps'
%print('-depsc',outname)
