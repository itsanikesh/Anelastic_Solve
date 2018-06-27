%@t
% \textbf{unstrat\_radial\_TKE.m}
%@h
%   Description:
%     Calculates turbulent kinetic energy terms for the radial unstratified 
%     wake.
%@q

clear;
format long;

%OUTPUT CONTOUR TO FIGURE WINDOW ON READ OF VTK FILE
Tplot=0;

fbase='U1_Tr';
base='~/work/T_Re50000_Fr0/';
plndir=[base,'stat/results/'];
profDIR=[base,'k2eps_profiles/'];
jpgDIR=[profDIR,'jpg/'];
dir=plndir;
mkdir(profDIR)
mkdir(jpgDIR)

%TIME STATS FILE
fname3=[base,fbase,'_radial_tke_stats_1.dat'];
TimeFile=fopen(fname3,'wt');

%DATA SIZE
jmin=1;
kmin=1;
jmax=514;
kmax=514;
%jmax=258;
%kmax=258;


%TIME LOOP
imin=0;
imax=0;
iskip=25;

%Number of Radial Bins for Statistics
binS=500;

for iter=imin:iskip:imax
    var='U1';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U1] = read_vtk(fname,1,Tplot);

    var='dTRANSdx2';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z dtdx2] = read_vtk(fname,1,Tplot);

    var='dTRANSdx3';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z dtdx3] = read_vtk(fname,1,Tplot);

    var='dU1dx2';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z du1dx2] = read_vtk(fname,1,Tplot);

    var='dU1dx3';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z du1dx3] = read_vtk(fname,1,Tplot);

    var='u1pu2p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u1u2] = read_vtk(fname,1,Tplot);

    var='u1pu3p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u1u3] = read_vtk(fname,1,Tplot);

%    p12=-du1dx2.*u1pu2p;
    var='PROD12';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z p12] = read_vtk(fname,1,Tplot);

    var='PROD13';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z p13] = read_vtk(fname,1,Tplot);

    var='DISS';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z diss] = read_vtk(fname,1,Tplot);
    var='u1p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u1p] = read_vtk(fname,1,Tplot);

    var='u2p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u2p] = read_vtk(fname,1,Tplot);

    var='u3p';
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u3p] = read_vtk(fname,1,Tplot);

    for k=kmin:kmax
        for j=jmin:jmax
            Rmag   = ((y(j))^2+(z(k))^2)*binS;
            nshell = round(sqrt(Rmag))+1;
            r(nshell) = 0;
            U1r(nshell) = 0;
            counter(nshell) = 0;
            u1R(nshell) = 0;
            u2R(nshell) = 0;
            u3R(nshell) = 0;
            kineticR(nshell) = 0;
            u1ur(nshell) = 0;
            Shear(nshell) = 0;
            dissR(nshell) = 0;
            k2eps(nshell) = 0;
            temp(nshell) = 0;
            du1ur_dr(nshell) = 0;
        end
    end

    for k=kmin:kmax
        for j=jmin:jmax
            Rmag   = ((y(j))^2+(z(k))^2)*binS;
            nshell = round(sqrt(Rmag))+1;
            U1r(nshell) = U1r(nshell) + U1(j,k);
            r(nshell) = sqrt((y(j))^2+(z(k))^2);
            kin    = 0.5*(u1p(j,k)^2+u2p(j,k)^2+u3p(j,k)^2);
            kineticR(nshell) = kineticR(nshell) + kin;
            dissR(nshell) = dissR(nshell) + diss(j,k);
            u1R(nshell) = u1R(nshell) + u1p(j,k);
            u2R(nshell) = u2R(nshell) + u2p(j,k);
            u3R(nshell) = u3R(nshell) + u3p(j,k);  
            counter(nshell) = counter(nshell) + 1;
        end
    end
    kineticR=kineticR./counter;
    dissR=dissR./counter;
Pi = 4.0*atan(1.0);
    for k=kmin:kmax
        for j=jmin:jmax
            Rmag   = ((y(j))^2+(z(k))^2)*binS;
            nshell = round(sqrt(Rmag))+1;
        if ( y(j) > 0) & (z(k) > 0 )  %Q1
            theta = atan( abs(z(k)/y(j)) );
        elseif (y(j)==0 ) & ( z(k) > 0 ) %Pi/2
            theta = Pi/2.d0;
        elseif (y(j) < 0 ) & ( z(k) > 0 ) %Q2
             theta = Pi - atan( abs(z(k)/y(j)) );
        elseif (z(k)==0 ) & ( y(j) < 0 ) %Pi
            theta = Pi;
        elseif (y(j) < 0 ) & ( z(k) < 0 ) %Q3
            theta = Pi + atan( abs(z(k)/y(j)) );
        elseif ( y(j)==0 ) & ( z(k) < 0 ) %3Pi/2
             theta = 3.0*Pi/2.0;
        elseif (y(j) > 0 ) & ( z(k)<0 ) %Q4 
            theta = 2.0*Pi - atan( abs(z(k)/y(j)) );
        elseif ( z(k)==0 ) & ( y(j) > 0 ) %0,2Pi
             theta = 2.0*Pi;
        else
             theta = atan( (z(k)/y(j)) );
        end

            u1ur(nshell) = u1ur(nshell) + u1u3(j,k)*cos(theta);
            u1ur(nshell) = u1ur(nshell) + u1u2(j,k)*sin(theta);
            Shear(nshell) = Shear(nshell) + du1dx3(j,k)*cos(theta)+du1dx2(j,k)*sin(theta);
            if (nshell > binS/10) 
              u1ur(nshell) = 0;
            end
        end
    end
    u1ur=u1ur./counter;

    for k=kmin:kmax
        for j=jmin:jmax
            Rmag   = ((y(j))^2+(z(k))^2)*binS;
            nshell = round(sqrt(Rmag))+1;
            k2eps(nshell) = kineticR(nshell)*kineticR(nshell)/(-dissR(nshell));
%            temp(nshell) = r(nshell)*u1ur(nshell);
            if (nshell > binS/7) 
                k2eps(nshell) = 0;
            end
        end
    end
 %   temp=temp./counter
 %   for ir=1:size(u1R,2)-1
 %       Rmag   = ((y(j))^2+(z(k))^2)*binS;
 %       nshell = round(sqrt(Rmag))+1;
 %       dr =  ( r(ir+1)-r(ir) );
 %       du1ur_dr(nshell) =  ( temp(ir+1)-temp(ir) ) / dr; 
 %   end
    
    
    
    max_k2eps = max(k2eps);
    
    max_u1ur = max(u1ur);
    
    Shear=Shear./counter;
    u1R=u1R./counter;
    u2R=u2R./counter;
    u3R=u3R./counter;
    U1r=U1r./counter;

    int_sum  = 0.0;
    int_sum2 = 0.0;

    for ir=1:size(u1R,2)-1
        dr =  ( r(ir+1)-r(ir) );
        int_sum = int_sum  + dr*r(ir)^3*kineticR(ir)*2;
        int_sum2= int_sum2 + dr*r(ir)*kineticR(nshell)*2;
    end
    Lr_k = sqrt(int_sum/int_sum2);
    max_tke=max(kineticR);

    
    int_sum  = 0.0;
    int_sum2 = 0.0;
    for ir=1:size(u1R,2)-1
        dr =  ( r(ir+1)-r(ir) );
        int_sum = int_sum  + dr*r(ir)^3*dissR(nshell)^2;
        int_sum2= int_sum2 + dr*r(ir)*dissR(nshell)^2;
    end
    Lr_eps = sqrt(int_sum/int_sum2);
    max_eps=min(dissR);
    
    time=x(1);
    
    %APPEND TO TIME STATS FILE
    fprintf(TimeFile,'%13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G\n',iter*1.0,time,Lr_k,Lr_eps,max_tke,max_eps,max_u1ur,max_k2eps);

    %OUTPUT PROFILE
    siter = int2str(iter);
    fname2=[profDIR,fbase,siter,'.dat'];
    file_2=fopen(fname2,'wt');
    fprintf(file_2,'#%13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %18.8G\n',time,Lr_k,Lr_eps,max_tke,max_eps,max_u1ur,max_k2eps);
    for ir=1:size(kineticR,2)
        fprintf(file_2,'%13.8G %13.8G %13.8G %13.8G %13.8G %13.8G\n',r(ir)-r(1),kineticR(ir),dissR(ir),k2eps(ir),(r(ir)-r(1))*u1ur(ir),Shear(ir) );
    end
    xx=fclose(file_2);
    %OUTPUT JPG
    siter = int2str(iter);
    outjpg=[jpgDIR,fbase,'_',siter,'.jpg'];

    plot(r,k2eps);
%    set(gca,'XLim',[0 3],'YLim',[.0 .01]);    
    xlabel('r/D','FontSize',14);
    ylabel('k*k/epsilon','FontSize',14);
    t1=floor(time);
    t2=round(100*mod(time,t1));
    stitle=[int2str(t1),'.',int2str(t2)];
    title(stitle,'FontSize',14);

  
   % print('-djpeg',outjpg) 
end
    xx=fclose(TimeFile);
