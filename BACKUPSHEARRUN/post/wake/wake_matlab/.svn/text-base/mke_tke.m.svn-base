%@t
% \textbf{mke\_tke.m}
%@h
%   Description:
%     Calculates tke and mke, writes both to a file.
%@q

% KYLE WHAT IS THIS AND WHY IS IT USED?

%REQUIRES U1, U2, U3, u1p, u2p, u3p

clear;
format long;

%DIRECTORY CONTAINING DATA
baseDIR='~/work2/SP_Large/';
%baseDIR='~/work/TR10F02_stat/';

dir=[baseDIR,'stat/results/'];

%OUTPUT FILE NAME
fname=[baseDIR,'SP_int_mke_tke_I25_1.dat'];
%fname=[baseDIR,'junk.dat'];
TimeFile=fopen(fname,'wt');

%SPONGE GRID POINTS NEAR x2min,x2max and x3min,x2max TO BE EXCLUDED FROM INTEGRAL 
js=13;
ks=13;

%X2
ymin=1;
ymax=1026;
%ymax=514;
%ymax=258;
%X3
zmin=1;
zmax=514;
%zmax=258;
%zmax=130;
kcl=zmax/2;
jcl=ymax/2;
%TIME
imin=0;
imax=1640;
iskip=25;

file_2=fopen(fname,'wt');

for iter=imin:iskip:imax
    
    var='u1p'; %<u1'u1'>^(1/2)
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u1p] = read_vtk(fname,1,0); 

    var='u2p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u2p] = read_vtk(fname,1,0); 

    var='u3p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u3p] = read_vtk(fname,1,0); 

    if (imin==0)
        a=(u1p(kcl,jcl)+u2p(kcl,jcl)+u3p(kcl,jcl))/3.0;
        a2=(max(max(u1p))+max(max(u2p))+max(max(u3p)))/3.0;
    end

    var='U1';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U1] = read_vtk(fname,1,0);

    var='U2';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U2] = read_vtk(fname,1,0);

    var='U3';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U3] = read_vtk(fname,1,0);

    tke = 0.0;
    mke = 0.0;
    Umax= 0.0;
    u1max=0.0;
    u2max=0.0;
    u3max=0.0;
    kmax=0.0;
    for k=zmin+ks:zmax-ks
        for j=ymin+js:ymax-js
            dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
            tke = tke + 0.5*dA*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j));
            mke = mke + 0.5*dA*(U1(k,j)*U1(k,j)+ U2(k,j)*U2(k,j)+ U3(k,j)*U3(k,j));
            ktemp=0.5*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j));
            if (ktemp > kmax) 
                kmax=ktemp;
            end
            if (U1(k,j) > Umax) 
                Umax=U1(k,j);
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
        end
    end
    time=x(1);
    fprintf(TimeFile,'%13.8G   %13.8G   %13.8G   %13.8G   %13.8G   %13.8G   %13.8G\n',time,tke,mke,Umax,u1max,u2max,u3max);
end
xx=fclose(TimeFile);
