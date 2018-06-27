%@t
% \textbf{sdiss\_check.m}
%@h
%   Description:
%     Calculates integrated dissipation as a function of time. KYLE???
%@q

clear;
format long;
Tplot=0;
ymin=1;
ymax=1026;
%ymax=514
zmin=1;
zmax=514;
%zmax=258

imin=0;
imax=1675;
iskip=25;

js=13;
ks=13;

drhodz=-0.006371;
D=1;
Fr=4;
Fact=Fr^2*(D*drhodz)^2;

dir='~/work2/T_Large/stat/results/';
file_2=fopen('./junk.dat','wt');

for iter=imin:iskip:imax

var='Seps1';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z s1] = read_vtk(fname,1,Tplot);

var='Seps2';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z s2] = read_vtk(fname,1,Tplot);

var='Seps3';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x y z s3] = read_vtk(fname,1,Tplot);

int1 =0; 

for k=zmin+ks:zmax-1-ks
    for j=ymin+js:ymax-1-js
      dA = (z(k+1)-z(k) )*(y(j+1)-y(j));
      int1 = int1 + dA/Fact*(s1(k,j)+s2(k,j)+s3(k,j));
    end
end

fprintf(file_2,'%13.8G    %13.8G\n',x(1),int1);
end

fclose(file_2);
