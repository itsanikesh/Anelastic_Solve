%@t
% \textbf{check\_trans.m}
%@h
%   Description:
%     Calculates the transport for a given flow.
%@q
%   Current Code Owner:
%     Matt de Stadler (mdestadl@ucsd.edu)

%   Modification History
%     Version   Date     Comment 
%     -------   ----     ------- 
%     1.0       07/2008  Original code. [Kyle A. Brucker] 
%@h
%   Comments:
%     KYLE Can you add some comments on how to use this?
%@q

clear;
format long;
Tplot=0;
ymin=1;
ymax=1026;
%ymax=514;
zmin=1;
zmax=514;
%zmax=258;
iter=1675;
dir='~/work2/T_Large/stat/results/';

var='u3ppp';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z u3ppp] = read_vtk(fname,1,Tplot);

for k=zmin+1:zmax-1
    for j=ymin:ymax-1
        du3pppdx3(k,j) = ( u3ppp(k+1,j)-u3ppp(k-1,j) )/(z(k+1)-z(k-1));  %Note dividing by 2 deltaZ 
    end
end

int1=0;
for k=zmin+26:zmax-26
    for j=ymin+26:ymax-26
      dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
      int1 = int1 + dA*du3pppdx3(k,j);
    end
end
int1
imagesc(y,z,du3pppdx3)

int2=0;
k=zmax-55;
for j=ymin+26:ymax-26
    dL   =  (y(j+1)-y(j));
    int2 = int2 + dL*u3ppp(k,j);
end
int2

int3=0;
k=zmin+55;
for j=ymin+26:ymax-26
    dL   =  (y(j+1)-y(j));
    int3 = int3 - dL*u3ppp(k,j);
end

int3

int1
int2+int3
x(1)
