%@t
% \textbf{unstrat\_radial.m}
%@h
%   Description:
%     Calculates radial statistics for the unstratified wake.
%@q

clear;
format long;

%OUTPUT CONTOUR TO FIGURE WINDOW ON READ OF VTK FILE
Tplot=0;

fbase='U1_SPr';
base='~/work/SP_Re50000_Fr0/';
plndir=[base,'stat2/results/'];
profDIR=[base,fbase,'_sbin_1/']
jpgDIR=[profDIR,'jpg/']
mkdir(profDIR)
mkdir(jpgDIR)

%TIME STATS FILE
fname3=[base,fbase,'_radial_time_stats_2.dat'];
TimeFile=fopen(fname3,'wt');

%DATA SIZE
jmin=1;
kmin=1;
%jmax=514;
%kmax=514;
jmax=258;
kmax=258;


%TIME LOOP
imin=1000;
imax=1750;
iskip=5;

%Number of Radial Bins for Statistics
binS=100;

for iter=imin:iskip:imax

    var='U1';
    basename=[plndir,var,'_'];
    fname=Cfilename(basename,iter);
    [time y z U1] = read_vtk(fname,1,Tplot);

    
    for k=kmin:kmax
        for j=jmin:jmax
            Rmag   = ((y(j))^2+(z(k))^2)*binS;
            nshell = round(sqrt(Rmag))+1;
            r(nshell) = 0;
            U1r(nshell) = 0;
            counter(nshell) = 0;
        end
    end

    for k=kmin:kmax
        for j=jmin:jmax
            Rmag   = ((y(j))^2+(z(k))^2)*binS;
            nshell = round(sqrt(Rmag))+1;
            U1r(nshell) = U1r(nshell) + U1(j,k);
            r(nshell) = sqrt((y(j))^2+(z(k))^2);
            %Determin Correct sign for atan 
%            theta = katan(y(j),z(k));
%            Shear(nshell) = Shear(nshell) + du1dx3(j,k)*cos(theta)+du1dx2(j,k)*sin(theta);
            counter(nshell) = counter(nshell) + 1;
        end
    end

    U1r=U1r./counter;

    int_sum  = 0.0;
    int_sum2 = 0.0;

    for ir=1:size(U1r,2)-1
        dr =  ( r(ir+1)-r(ir) );
        int_sum = int_sum  + dr*r(ir)^3*U1r(ir)^2;
        int_sum2= int_sum2 + dr*r(ir)*U1r(ir)^2;
    end
    Lr_1 = sqrt(int_sum/int_sum2);
    maxU_1=(max(U1r));

    
    int_sum  = 0.0;
    int_sum2 = 0.0;
    for k=kmin+1:kmax
        for j=jmin+1:jmax
           dA =  ( z(k)-z(k-1) )*(y(j)-y(j-1));
           int_sum = int_sum  + dA*(z(k)^2+y(j)^2)*U1(k,j)^2;
           int_sum2= int_sum2 + dA*U1(k,j)^2;
        end
    end
    Lr_2 = sqrt(int_sum/int_sum2);
    maxU_2=max(max(U1));
    
    %APPEND TO TIME STATS FILE
    fprintf(TimeFile,'%13.8G %13.8G %13.8G %13.8G %13.8G %13.8G\n',iter,time,Lr_1,Lr_2,maxU_1,maxU_2);

    %OUTPUT PROFILE
    siter = int2str(iter);
    fname2=[profDIR,fbase,siter,'.dat'];
    file_2=fopen(fname2,'wt');
    fprintf(file_2,'# %13.8G %13.8G %13.8G %13.8G %13.8G\n',time,Lr_1,Lr_2,maxU_1,maxU_2);
    for ir=1:size(U1r,2)
        fprintf(file_2,'%13.8G %13.8G\n',r(ir)-r(1),U1r(ir));
    end
    xx=fclose(file_2);
    %OUTPUT JPG
    siter = int2str(iter);
    outjpg=[jpgDIR,fbase,'_',siter,'.jpg'];

    plot(r,U1r);
    set(gca,'XLim',[0 3],'YLim',[-.03 .12]);    
    xlabel('r/D','FontSize',14);
    ylabel('U1(r)/U','FontSize',14);
    t1=floor(time);
    t2=round(100*mod(time,t1));
    stitle=[int2str(t1),'.',int2str(t2)];
    title(stitle,'FontSize',14);

  
    print('-djpeg',outjpg) 
end
    xx=fclose(TimeFile);
