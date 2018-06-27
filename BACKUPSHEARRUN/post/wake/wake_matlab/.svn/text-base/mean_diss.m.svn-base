%@t
% \textbf{mean\_diss.m}
%@h
%   Description:
%     Calculate the mean integrated dissipation and write it to a file. 
%@q

% KYLE WHAT IS THIS AND WHY IS IT USED?

clear;
format long;
Tplot=0;
ymin=1;
%ymax=1026;
ymax=514;
%ymax=258;
zmin=1;
zmax=514;
%zmax=258;
imin=000;
imax=1000;
istep=25;
is=26;
baseDIR='~/work/SP_Re50000_Fr0/'
dir=[baseDIR,'stat/results/'];
fname=[baseDIR,'USP_mean_diss_1a'];
file_2=fopen(fname,'wt');

for iter=imin:istep:imax
    var='dU1dx2';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z S12] = read_vtk(fname,1,Tplot);

    var='dU1dx3';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z S13] = read_vtk(fname,1,Tplot);

    int1 =0; 
    int2 =0;
    for k=zmin+is:zmax-is
        for j=ymin+is:ymax-is
          dA = (z(k+1)-z(k) )*(y(j+1)-y(j));
          int1 = int1 + dA*S12(k,j)*S12(k,j);
          int2 = int2 + dA*S13(k,j)*S13(k,j);
        end
    end

fprintf(file_2,'%13.8G %13.8G\n',x(1), int1+int2);

end

close all
