%@t
% \textbf{check\_trans.m}
%@h
%   Description:
%     Calculates something with u3'rho'??? KYLE?
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
ymax=1026;
%ymax=514;
zmin=1;
zmax=514;
%zmax=258;
iter=0;
dir='~/work2/T_Large/stat/results/';

var='u3prhop';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z u3prp] = read_vtk(fname,1,Tplot);


imagesc(y,z,-9.81.*u3prp)

int1=0;
for k=zmin+26:zmax-26
    for j=ymin+26:ymax-26
      dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
      int1 = int1  - 9.81*dA*u3prp(k,j);
   
    end
end
int1
