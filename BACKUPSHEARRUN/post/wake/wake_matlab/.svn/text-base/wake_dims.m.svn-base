%@t
% \textbf{wake\_dims.m}
%@h
%   Description:
%     Calculate wake dimensions for the stratified case based on U1 or 
%     the Kinetic Energy.
%@h
%   Comments:
%     This program can also calculate the late time scaling of U1, R2, and
%     R3 if desired. There is an option to calculate R1 and R2 using a 1D
%     pencil at the centerline or else a full 2D plane average.
%@q

close all, clear all, clc; format long;
warning('off','MATLAB:dispatcher:InexactMatch')
dir='/work/temp/statRe6000/';
%dir='/work/temp/statrefine/';
% dir='/work/temp/statrelax9.25/';
%dir='/work/temp/statrelax5/';
% dir='/work/temp/wakerelax10/results/stat/';
%dir='/work/temp/wake1psolve/results/stat/';
fname_1=[dir,'wakeDims_U1_2d.dat']
fname_2=[dir,'wakeDims_KE_2d.dat']

js=26;        % sponge in x2
ks=13;        % sponge in x3

ymax=386;     % domain dimensions ymin and zmin are set to 1 below
zmax=194;

imin=10;
imax=3000;
iskip=10;

Fr=2;         % Froude number (for calculating tpe)

latescaling  = 0;  % if 1 calculate the late time scaling of U1, R2, R3
plotcontours = 0;  % if 1 make a contour plot of a variable with R2 x R3 boxes
plotvariable = 1;  % 1=tpe, 2=tke, 3=mke, 4=KE, 5=U1
oneDdims     = 2;  % if 1 calculate R2, R3 along a line, if 2 use a plane average
                   % In general 2 should be used!

%fact=22/7;
% fact = 6.0/7.0*4
fact=2; % KYLE WHAT IS THIS FACT? HOW IS IT SET? sometimes it is 2 sometimes it is 1, it is 3 in the latest version, why? 

U=1.0; rho0 = 1.0; g=9.81; D=1.0; % U=Free stream velocity, D=sphere diameter
drhodz=U^2*rho0 / (g*Fr^2*D^2);
Fact=(Fr^2*(D*drhodz)^2); % What is fact Kyle??? Why is this used???
file_1=fopen(fname_1,'wt');
file_2=fopen(fname_2,'wt');
ymin=1;
zmin=1;
Tplot=0; % set to 1 to open a contour plot every time read_vtk is called.

for iter=imin:iskip:imax

% Load TKE Variables
    var='u1p';
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

% Load MKE Variables
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

% Load TPE Variable
    var='rhop'; %<u1'u1'>^(1/2)
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z rhop] = read_vtk(fname,1,0);

% Calculate tke, mke, tpe, and kinetic energy (tke+mke)
    for k=zmin+ks:zmax-ks
        for j=ymin+js:ymax-js
            tke(k,j) = 0.5*(u1p(k,j)*u1p(k,j)+u2p(k,j)*u2p(k,j)+u3p(k,j)*u3p(k,j));
            mke(k,j) = 0.5*(U1(k,j)*U1(k,j)+ U2(k,j)*U2(k,j)+ U3(k,j)*U3(k,j));
            tpe(k,j) = 0.5*(rhop(k,j)*rhop(k,j))/Fact;
            KE(k,j) = tke(k,j) + mke(k,j);
        end
    end

    tkemax=max(max(tke));
    mkemax=max(max(mke));
    tpemax=max(max(tpe));
    KEmax = max(max(KE));
    U1max = max(max(U1));

if (oneDdims == 1)
% Calculate Wake Dimensions 1D pencil
    int_sum(1:2)  = 0.0;
    int_sum2(1:2) = 0.0;
    j=ymax/2;
    for k=zmin+ks:zmax-ks
        dz   =  ( z(k+1)-z(k) );
        int_sum(1)  = int_sum(1)  + dz*z(k)^2*U1(k,j)^2;
        int_sum(2)  = int_sum(2)  + dz*z(k)^2*KE(k,j)^2;
        int_sum2(1) = int_sum2(1) + dz*U1(k,j)^2;   
        int_sum2(2) = int_sum2(2) + dz*KE(k,j)^2;   
    end
    R3_U1 = sqrt(fact*int_sum(1)/int_sum2(1));
    R3_KE = sqrt(fact*int_sum(2)/int_sum2(2));

    int_sum(1:2)  = 0.0;
    int_sum2(1:2) = 0.0;
    k=zmax/2;
    for j=ymin+js:ymax-js
        dy   = (y(j+1)-y(j));
        int_sum(1) = int_sum(1) + dy*y(j)^2*U1(k,j)^2;
        int_sum(2) = int_sum(2) + dy*y(j)^2*KE(k,j)^2;
        int_sum2(1)= int_sum2(1)+ dy*U1(k,j)^2;   
        int_sum2(2)= int_sum2(2)+ dy*KE(k,j)^2;   

    end
    R2_U1 = sqrt(fact*int_sum(1)/int_sum2(1));
    R2_KE = sqrt(fact*int_sum(2)/int_sum2(2));
else
% 2D averages using what is Kyle's paper
int1(1:2)=0.0;
int2(1:2)=0.0;
int3(1:2)=0.0;
int4(1:2)=0.0;
int5(1:2)=0.0;
int6=0.0;
int7=0.0;

for j=ymin+js:ymax-js
    for k=zmin+ks:zmax-ks
      dA   = ( z(k)-z(k-1) )*( y(j)-y(j-1) );
%      int6 = int6 + dA*Rho(k,j)*(U1(k,j)+U2(k,j)+U3(k,j)); % why do we want this KYLE?
%      int7 = int7 + dA*Rho(k,j);
      % U1 terms
      int3(1) = int3(1) + dA*y(j)*U1(k,j).^2; % integral of x2*u_1^2 dA
      int4(1) = int4(1) + dA*z(k)*U1(k,j).^2; % integral of x3*u_1^2 dA
      int5(1) = int5(1) + dA*U1(k,j).^2;      % integral of u_1^2 DA
      % KE terms
      int3(2) = int3(2) + dA*y(j)*KE(k,j).^2; % integral of x2*u_1^2 dA
      int4(2) = int4(2) + dA*z(k)*KE(k,j).^2; % integral of x3*u_1^2 dA
      int5(2) = int5(2) + dA*KE(k,j).^2;      % integral of u_1^2 DA
    end
end

y0(1)=int3(1)/int5(1); % horizontal center
z0(1)=int4(1)/int5(1); % vertical center
y0(2)=int3(2)/int5(2); % horizontal center
z0(2)=int4(2)/int5(2); % vertical center

%for j=ymin+2:ymax-2
%    for k=zmin+2:zmax-2
for j=ymin+js:ymax-js
    for k=zmin+ks:zmax-ks
        dA   = ( z(k)-z(k-1) )*( y(j)-y(j-1) );
      % U1 terms
        int1(1) = int1(1) + dA*(y(j)-y0(1))^2*U1(k,j).^2;
        int2(1) = int2(1) + dA*(z(k)-z0(1))^2*U1(k,j).^2;
      % KE terms
        int1(2) = int1(2) + dA*(y(j)-y0(2))^2*KE(k,j).^2;
        int2(2) = int2(2) + dA*(z(k)-z0(2))^2*KE(k,j).^2;
    end
end


R2_U1=sqrt(fact*int1(1)/int5(1));
R3_U1=sqrt(fact*int2(1)/int5(1));
R2_KE=sqrt(fact*int1(2)/int5(2));
R3_KE=sqrt(fact*int2(2)/int5(2));

end % end 2D averages

% Gaussian fit to U1
[cx,cy,r2,r3,PeakOD] =Gaussian2D(U1,0.000001);
dy = y(ymax/2)-y(ymax/2-1);
dz = z(zmax/2)-z(zmax/2-1);
r2=r2*dy;
r3=r3*dz;
x(1) = x(1) + 6.0; % wake at t=0 is at x/d of 6
 fprintf(file_1,'%13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G\n',x(1),U1max,R2_U1,R3_U1,PeakOD,r2,r3);
 fprintf(file_2,'%13.8G %13.8G %13.8G %13.8G\n',x(1),KEmax,R2_KE,R3_KE);

end
fclose(file_1); fclose(file_2);

if (plotcontours == 1);
% Plot a contour plot of a given variable with a box for R2,R3 and 2R2,2R3
  if(plotvariable == 1);
    plotvar=tpe/tpemax;
    strmax=num2str(tpemax)
  elseif(plotvariable == 2);
    plotvar=tke/tkemax;
    strmax=num2str(tkemax)
  elseif(plotvariable == 3);
    plotvar=mke/mkemax;
    strmax=num2str(mkemax)
  elseif(plotvariable == 4);
    plotvar=KE/KEmax;
    strmax=num2str(KEmax)
  else(plotvariable == 5);
    plotvar=U1/U1max;
    strmax=num2str(U1max)
  end

h=gca;
set(h,'FontSize',18);       % sets the font size of axis
set(h,'FontName','Times');

colormap gray;
[C,h]=contour(y,z,plotvar,'LineWidth',1,'Color','black');
xlabel('@1')
ylabel('@2')
axis([y1 y2 z1 z2])
clabel(C,h,[.1 .3 .5 .7 .9],'FontSize',15,'Color','black','LabelSpacing',1512)%,'BackgroundColor',[1 1 1],'rotation',0);

hold on;

l1p2=[-2*R2 2*R2];  l1p1=[-2*R3 -2*R3];
l2p2=[-2*R2 2*R2];  l2p1=[2*R3 2*R3];
l3p1=[-2*R2 -2*R2]; l3p2=[-2*R3 2*R3];
l4p1=[2*R2 2*R2];   l4p2=[-2*R3 2*R3];
plot(l1p2,l1p1,'k--'); plot(l2p2,l2p1,'k--');plot(l3p1,l3p2,'k--'); plot(l4p1,l4p2,'k--');


l1p2=[-R2 R2];  l1p1=[-R3 -R3];
l2p2=[-R2 R2];  l2p1=[R3 R3];
l3p1=[-R2 -R2]; l3p2=[-R3 R3];
l4p1=[R2 R2];   l4p2=[-R3 R3];
plot(l1p2,l1p1,'k--'); plot(l2p2,l2p1,'k--'); plot(l3p1,l3p2,'k--'); plot(l4p1,l4p2,'k--');

h=gcf; 
%a1p1=.15;
%a1p2=.515
%a1p3=.75;
%annotation(h,'doublearrow',[a1p1 a1p1],[a1p2 a1p3]);
annotation(h,'textbox','String',strmax,'FitBoxToText','on','LineStyle','none','Position',[0.13 0.425 0.13 0.5],'FontSize',15);
hold off;
%clabel(C,h,'manual','FontSize',15,'Color','black','LabelSpacing',800,'BackgroundColor',[1 1 1],'rotation',0);
outname = [dir,'test.eps'];
print('-depsc',outname);
end

if (latescaling == 1);
% load data back in to get scaling as a function of x
fid = fopen(fname_1,'r');
a = fscanf(fid, '%g %g %g %g %g %g',[6 inf]);
a = a';
fclose(fid);

% % plot variables as a function of x
% plot(a(:,1),a(:,2)); % U1 vs. x
% figure; plot(a(:,1),a(:,5)); % r vs. x
% figure; plot(a(:,1),a(:,4),'g'); hold on; plot(a(:,1),a(:,3),'r');% ls and ly vs. x

tstart=200; % self-similarity starts at late time
p = polyfit(log(a(tstart:length(a),1)),log(a(tstart:length(a),2)),1) % U ~ x^p
p2 = polyfit(log(a(tstart:length(a),1)),log(a(tstart:length(a),3)),1) % r ~ x^p2
% 
figure; plot(log(a(tstart:length(a),1)),log(a(tstart:length(a),2)));
figure; plot(log(a(tstart:length(a),1)),log(a(tstart:length(a),3)));
end
