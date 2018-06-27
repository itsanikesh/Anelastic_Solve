%@t
% \textbf{int\_tke\_budget.m}
%@h
%   Description:
%     Program to calculate the Turbulent Kinetic Energy Budget <x2 x3> for
%     Stratified Wakes.

%   Comments:
%     WARNING THIS FILE HAS SOME SPECIAL TWEAKS THAT NEED TO BE 
%     FIXED, SEE COMMENTS IN PROGRAM.
%@q

%Kyle A. Brucker
%v.0 09/2009

%REQUIRES single precision .vtk files for
%dTRANSdx2
%dTRANSdx3
%PROD12**1
%PROD13
%BFLUX
%DISS**2 
%**1 in the original RE=50,000 data P12 is incorrect and needs to be
%      calculated as <u1pu2p>dU1dx2
%**2 in the original RE=50,000 data the Dissipation is twice the actual
%      dissipation

clear;
format long;

%DIRECTORY CONTAINING DATA
baseDIR='~/work2/T_Large';
dir=[baseDIR,'/stat/results/'];

%OUTPUT FILE NAME
%fname=[baseDIR,'/SP_int_tke_terms_p1a.dat'];
%fname=[baseDIR,'/T_int_tke_terms_1_FINAL.dat'];
fname=[baseDIR,'/junk.dat'];

TimeFile=fopen(fname,'wt');

flow=1; %TLarge Part 1 small skip was 10 instead of 5

%SPONGE GRID POINTS NEAR x2min,x2max and x3min,x2max TO BE EXCLUDED FROM
%INTEGRAL (especially important for Trasport)
js=26;
ks=26;

%X2 bounds
jmin=1;
jmax=1026;
%jmax=514;
%jmax=258;

%X3 bounds
kmin=1;
kmax=514;
%kmax=258;
%kmax=130;

%TIME
imin=200;
imax=201;
iskip=25;

%Parameters
Fr=4;
Re=50000;
t0=4;

%Initialize storage arrays in time
counter=1;
for iter=imin:iskip:imax
    tsP13(counter) = 0;
    tsP12(counter) = 0;
    tsD(counter) = 0;
    tsB(counter) = 0;
    tsT2(counter) = 0;
    tsT3(counter) = 0;
    tstime(counter) = 0;
    tsdkdt(counter)=0;
    tsREM(counter)=0;
    counter=counter+1;
end

%Loop over Dumps and compute the budget
counter=1;
for iter=imin:iskip:imax
    var='dTRANSdx2';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z dtdx2] = read_vtk(fname,1,0);

    var='dTRANSdx3';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z dtdx3] = read_vtk(fname,1,0);

    var='PROD13';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z p13] = read_vtk(fname,1,0);
%%1    
%    var='PROD12';
%    basename=[dir,var,'_'];
%    fname=Cfilename(basename,iter);
%    [x y z p12] = read_vtk(fname,1,0);   
    
    var='u1pu2p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u1u2] = read_vtk(fname,1,0);
    
    var='u1pu3p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u1u3] = read_vtk(fname,1,0);

    var='dU1dx2';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z du1dx2]=read_vtk(fname,1,0);

    p12 = -du1dx2.*u1u2;

    var='DISS';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z diss] = read_vtk(fname,1,0);
%%2
    diss=diss/2;
    
    vard1='omg1p';
    vard2='omg2p';
    vard3='omg3p';

    basename=[dir,vard1,'_'];
    fname=Cfilename(basename,iter);
    [x y z Tvard1] = read_vtk(fname,1,0); 
    basename=[dir,vard2,'_'];
    fname=Cfilename(basename,iter);
    [x y z Tvard2] = read_vtk(fname,1,0); 
    basename=[dir,vard3,'_'];
    fname=Cfilename(basename,iter);
    [x y z Tvard3] = read_vtk(fname,1,0); 
    diss2=Tvard1.^2+Tvard2.^2+Tvard3.^2;
    
    
    var='BFLUX';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z bflux] = read_vtk(fname,1,0);

    var='U1';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U1] = read_vtk(fname,1,0);

    var='P';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z P] = read_vtk(fname,1,0);
    
    intProd12  = 0.0; 
    intProd13  = 0.0; 
    intDiss  = 0.0;
    intBflux = 0.0;
    intTrans2 = 0.0;
    intTrans3 = 0.0;
    intDiss2=0;    
    temp1=0.0;
    intT1=0.0;
    intT2=0.0;
    intT3=0.0;
    intT4=0.0;
    intT=0.0;
    
    for k=kmin:kmax
        for j=jmin:jmax
            tempf1(k,j) = 0;
            tempf2(k,j) = 0;
            tempf5(k,j) = 0;
            tempf6(k,j) = 0;
            tempf7(k,j) = 0;
            tempf8(k,j) = 0;
            tempf4(k,j) = 0;
            tempf3(k,j) = 0;
        end
    end

    %MEAN TRANSPORT AND DISSIPATION CORRECTION TERM
    for k=kmin:kmax-1
        for j=jmin:jmax-1
            tempf1(k,j) = ( u1u2(k+1,j) - u1u2(k,j) )/( z(k+1)-z(k) );
            tempf2(k,j) = ( u1u3(k,j+1) - u1u3(k,j) )/( y(j+1)-y(j) );
            tempf5(k,j) = (U1(k+1,j)*u1u3(k+1,j) - U1(k,j)*u1u3(k,j)  )/( z(k+1)-z(k) );
            tempf6(k,j) = (U1(k,j+1)*u1u2(k,j+1) - U1(k,j)*u1u2(k,j) )/( y(j+1)-y(j) );
            tempf7(k,j) = (U1(k,j+1)*P(k,j+1) - U1(k,j)*P(k,j) )/( y(j+1)-y(j) );
            tempf8(k,j) = (U1(k+1,j)*P(k+1,j) - U1(k,j)*P(k,j) )/( z(k+1)-z(k) );
        end
    end
    figure
    %contour(y,z,tempf5);
    imagesc(y,z,tempf7);
    figure
    %contour(y,z,tempf6);
    imagesc(y,z,tempf8);
    
    for k=kmin+1:kmax-2
        for j=jmin+1:jmax-2
            tempf4(k,j) = (tempf2(k+1,j) - tempf2(k,j) )/( z(k+1)-z(k) );
            tempf3(k,j) = (tempf1(k,j+1) - tempf1(k,j) )/( y(j+1)-y(j) );
        end
    end
    
    for k=kmin+ks:kmax-ks
        for j=jmin+js:jmax-js
            dA = (z(k+1)-z(k) )*(y(j+1)-y(j));
            intProd12 = intProd12 + dA*(p12(k,j));
            intProd13 = intProd13 + dA*(p13(k,j));
            intDiss = intDiss + dA*diss(k,j);
            intDiss2 = intDiss2 + dA*diss2(k,j);
            intBflux = intBflux + dA*bflux(k,j) ;     
            intTrans2 = intTrans2 + dA*(dtdx2(k,j));
            intTrans3 = intTrans3 + dA*(dtdx3(k,j));
            temp1 = temp1 + (1/50000)*dA*(tempf4(k,j)+tempf3(k,j)); %u1u2(k,j)+u1u3(k,j);
            intT1 = intT1 + dA*(tempf5(k,j));
            intT2 = intT2 + dA*(tempf6(k,j));
            intT3 = intT3 + dA*(tempf7(k,j));
            intT4 = intT4 + dA*(tempf8(k,j));
        end
    end
    intT4
    intT3
    intT = intT1+intT2+intT3+intT4
    temp1
break

%%3  Small tke planes only available at every 10 iterations in Towed
%%Re=50000 simulations should make sure mod(ibig,ismall)=0
   if (flow==1) %tke from small stats
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
    
    var='u1p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterm);
    [x1 y z u1p] = read_vtk(fname,1,0); 

    var='u2p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterm);
    [x1 y z u2p] = read_vtk(fname,1,0); 

    var='u3p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterm);
    [x1 y z u3p] = read_vtk(fname,1,0); 
    
    t1=x1(1);
    tke1=0.0;
    
    for k=kmin+ks:kmax-ks
        for j=jmin+js:jmax-js
            dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
            tke1 = tke1 + 0.5*dA*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j));
        end
    end

    var='u1p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterp);
    [x2 y z u1p] = read_vtk(fname,1,0); 

    var='u2p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterp);
    [x2 y z u2p] = read_vtk(fname,1,0); 

    var='u3p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iterp);
    [x2 y z u3p] = read_vtk(fname,1,0);  

    t2=x2(1);
    tke2=0;
    for k=kmin+ks:kmax-ks
        for j=jmin+js:jmax-js
            dA   =  ( z(k+1)-z(k) )*(y(j+1)-y(j));
            tke2 = tke2 + 0.5*dA*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j));
        end
    end
    dkdt=(tke2-tke1)/(t2-t1);

    time=x(1);
    tsP13(counter) = intProd13;
    tsP12(counter) = intProd12;
    tsD(counter) = intDiss-temp1;
    tsB(counter) = intBflux;
    tsT2(counter) = intTrans2;
    tsT3(counter) = intTrans3;
    tstime(counter) = time+t0;
    tsdkdt(counter) = dkdt;
    tsREM(counter)  = intT;%-(1/50000)*intDiss2;%dkdt-(intProd12+intProd13+intDiss+intBflux-intTrans2-intTrans3);
    counter=counter+1;
%    fprintf(TimeFile,'%13.8G      %13.8G      %13.8G      %13.8G
%    %13.8G     %13.8G\n',time, dkdt, intProd12+intProd13, intDiss, intBflux, intTrans2+intTrans3);
end
counter=counter-1;
fprintf(TimeFile,'%1c\n','&');
for ii=1:counter
    fprintf(TimeFile,'%13.8G  %13.8G\n',tstime(ii),tsdkdt(ii));
end
fprintf(TimeFile,'%1c\n','&');
for ii=1:counter
    fprintf(TimeFile,'%13.8G  %13.8G\n',tstime(ii),tsP12(ii)+tsP13(ii));
end
fprintf(TimeFile,'%1c\n','&');
for ii=1:counter
    fprintf(TimeFile,'%13.8G  %13.8G\n',tstime(ii),tsD(ii));
end
fprintf(TimeFile,'%1c\n','&');
for ii=1:counter
    fprintf(TimeFile,'%13.8G  %13.8G\n',tstime(ii),tsB(ii));
end
fprintf(TimeFile,'%1c\n','&');
for ii=1:counter
    fprintf(TimeFile,'%13.8G  %13.8G\n',tstime(ii),tsT2(ii)+tsT3(ii));
end
fprintf(TimeFile,'%1c\n','&');
for ii=1:counter
    fprintf(TimeFile,'%13.8G  %13.8G\n',tstime(ii),tsREM(ii));
end
fprintf(TimeFile,'%1c\n','&');

xx=fclose(TimeFile);
