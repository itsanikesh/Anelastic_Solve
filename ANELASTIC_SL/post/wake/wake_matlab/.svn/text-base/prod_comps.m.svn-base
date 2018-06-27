%@t
% \textbf{prod\_comps.m}
%@h
%   Description:
%     Calculates the production components and writes them to a file.
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
is=13;
baseDIR='~/work/SP_Re50000_Fr0/'
dir=[baseDIR,'stat/results/'];
fname=[baseDIR,'junk.dat'];
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

    var='PROD13';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z p13] = read_vtk(fname,1,Tplot);

    var='PROD12';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z p12] = read_vtk(fname,1,Tplot);
    
    var='u1pu2p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u1u2] = read_vtk(fname,1,Tplot);

    var='u1pu3p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u1u3] = read_vtk(fname,1,Tplot);
    
    
    p12c = -u1u2.*S12;
    p13c = -u1u3.*S13;
    
    int1 =0; 
    int2 =0;
    int3 =0;
    int4 =0;
    
    for k=zmin+is:zmax-is
        for j=ymin+is:ymax-is
          dA = (z(k+1)-z(k) )*(y(j+1)-y(j));
          int1 = int1 + dA*p12c(k,j);
          int2 = int2 + dA*p13c(k,j);
          int3 = int3 + dA*p12(k,j);
          int4 = int4 + dA*p13(k,j);
        end
    end

fprintf(file_2,'%13.8G    %13.8G    %13.8G    %13.8G     %13.8G\n',x(1), int1, int2, int3, int4);

end

close all
