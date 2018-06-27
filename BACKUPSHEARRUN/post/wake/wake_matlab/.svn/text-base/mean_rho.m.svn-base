%@t
% \textbf{mean\_rho.m}
%@h
%   Description:
%     Calculates the TPE???
%@q

% KYLE WHAT IS THIS AND WHY IS IT USED?

clear;
format long;
Tplot=0;
ymin=1;
ymax=1026;
%ymax=514;
zmin=1;
zmax=514;
%zmax=258;
iter=1625;
dir='~/work2/SP_Large/stat/results/';

var='RHO';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z RHO] = read_vtk(fname,1,Tplot);
var='rhop';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z rhop] = read_vtk(fname,1,Tplot);

for k=zmin:zmax
    rho_bar=0;
    L=0;
    for j=ymin:ymax
        rhoMeanFIXED(k,j) = 1.0-0.006371*z(k);
        rho_bar = rho_bar+RHO(k,j);
        L=L+1;
    end
    rhoMean(k)=rho_bar/L;
end

for k=zmin:zmax
    for j=ymin:ymax
        rrr(k,j)=RHO(k,j);
       
      RHO(k,j) = RHO(k,j)-rhoMeanFIXED(k);
    end
end
for k=zmin:zmax
    for j=ymin:ymax
        KRHO(k,j) = 0.5/16*RHO(k,j)*RHO(k,j)/0.006371^2;
        krho(k,j )=  0.5/16*rhop(k,j)*rhop(k,j)/0.006371^2;
    end        
end


int3=0;
int1=0;

for k=zmin+13:zmax-13
    for j=ymin+13:ymax-13
      dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
      int1 = int1 + dA*KRHO(k,j);
      int3 = int3 + dA*krho(k,j);
    end
end
int1
int3

imagesc(y,z,KRHO); 
