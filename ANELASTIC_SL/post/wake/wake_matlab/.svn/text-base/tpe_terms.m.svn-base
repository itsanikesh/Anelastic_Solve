%@t
% \textbf{tpe\_terms.m}
%@h
%   Description:
%     Calculates terms for the turbulent potential energy budget.
%@q

%REQUIRES dTRANSRHO, dTRANSdx3, PROD12, PROD13, BFLUX, DISS 

clear;
format long;

%DIRECTORY CONTAINING DATA
baseDIR='~/work2/SP_Large/';
dir=[baseDIR,'/stat/results/'];

%OUTPUT FILE NAME
fname=[baseDIR,'/SP_int_tpe_terms_1.dat'];

TimeFile=fopen(fname,'wt');

flow=0; %TLarge Part 1 small skip was 10 instead of 5

%SPONGE GRID POINTS NEAR x2min,x2max and x3min,x2max TO BE EXCLUDED FROM INTEGRAL 
js=26;
ks=26;

%X2
jmin=1;
jmax=1026;
%jmax=514;
%jmax=258;
%X3
kmin=1;
kmax=514;
%kmax=258;

%TIME
%imin=1525;
%imax=3420;
imin=0;
imax=1640;
iskip=25;

drhodz=-0.006371;
D=1.0;
Fr=4;
Fact=Fr^2*(D*drhodz)^2

for iter=imin:iskip:imax
    var='DVdissipation';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z DVD] = read_vtk(fname,1,0);

    var='DVtransport';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z DVT] = read_vtk(fname,1,0);

    var='DVproduction';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z DVP] = read_vtk(fname,1,0);

    intProd  = 0.0; 
    intDiss  = 0.0;
    intTrans = 0.0;

    for k=kmin+ks:kmax-ks
        for j=jmin+js:jmax-js
            dA = (z(k+1)-z(k) )*(y(j+1)-y(j));
            intProd = intProd + dA*(DVP(k,j));
            intDiss = intDiss + dA*DVD(k,j);
            intTrans = intTrans + dA*(DVT(k,j));
        end
    end

    if (flow==1) %Tsmall iskip10
        if (iter==imin)
                iterm=imin;
                iterp=10;
                inext=1;
        elseif (iter==imax)
                iterp=iter;
                iterm=iter-5;
        elseif (inext==1)
                iterm=iter-5;
                iterp=iter+5;
                inext=0;
        else
                iterm=iter-10;
                iterp=iter+10;
                inext=1;
        end
    else
        if (iter==imin)
            iterm=iter;
            iterp=iter+5;
        elseif (iter==imax)
            iterp=iter;
            iterm=iter-5;
        else
            iterm=iter-5;
            iterp=iter+5;
        end
    end
    
    var='rhop';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterm);
    [x1 y z rhop] = read_vtk(fname,1,0); 
    
    t1=x1(1);
    tpe1=0.0;
    
    for k=kmin+ks:kmax-ks
        for j=jmin+js:jmax-js
            dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
            tpe1 = tpe1 + dA*(rhop(k,j)*rhop(k,j));
        end
    end

    var='rhop';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterp);
    [x2 y z rhop] = read_vtk(fname,1,0); 

    t2=x2(1);
    tpe2=0;
    for k=kmin+ks:kmax-ks
        for j=jmin+js:jmax-js
            dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
            tpe2 = tpe2 + dA*(rhop(k,j)*rhop(k,j));
        end
    end
    dPdt=(tpe2-tpe1)/(t2-t1);

    time=x(1); 
    fprintf(TimeFile,'%13.8G       %13.8G       %13.8G         %13.8G         %13.8G\n',time, dPdt/Fact, intProd/Fact, intDiss/Fact, intTrans/Fact);
 end

xx=fclose(TimeFile);
