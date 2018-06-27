%@t
% \textbf{Ret\_Frt.m}
%@h
%   Description:
%     Compute a bunch of values as a function of time. KYLE ???
%@q
clear;
format long;
warning('off','MATLAB:dispatcher:InexactMatch')
dir='~/work2/SP_Large/stat2/results/';
fname='./Ret_Frt_SP_2.dat'

%Fr=U/(ND)=4 if U=1,D=1 then N=0.25
N=0.25;
nu=1/50000;

ymin=1;
%ymax=1026;
ymax=514;
zmin=1;
zmax=258;
%zmax=514;
Tplot=0;

%SP 
%part1
%imin=0;
%imax=1640;
%part2
imin=1505;
imax=2865;
iskip=5;

%Towed
%part1
%imin=0;
%imax=1675;
%iskip=10;
%part2
%imin=1510;
%imax=3420;
%iskip=5;

file_2=fopen(fname,'wt');
%write header
fprintf(file_2,'#Time qmax qvmax IntQ qvmax/qmax R2p R3p R3p/R2p Ret Frt Re_lamda\n');

%fact=22/7;
%fact=2;
fact=88/31;

for iter=imin:iskip:imax

var='u1p';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x2 y2 z2 u1p] = read_vtk(fname,1,Tplot);

var='u2p';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x2 y2 z2 u2p] = read_vtk(fname,1,Tplot);

var='u3p';
basename=[dir,var,'_'];
fname=Cfilename(basename,iter);
[x2 y2 z2 u3p] = read_vtk(fname,1,Tplot);

%RECALL u1' ~ sqrt(1/3)q

for k=zmin+2:zmax-2
   for j=ymin+2:ymax-2
       q(k,j) = ( u1p(k,j)*u1p(k,j) + u2p(k,j)*u2p(k,j) + u3p(k,j)*u3p(k,j) )^(1/2);
       q1(k,j) = ( u1p(k,j)*u1p(k,j) )^(1/2);
       q2(k,j) = ( u2p(k,j)*u2p(k,j) )^(1/2);
       q3(k,j) = ( u3p(k,j)*u3p(k,j) )^(1/2);
   end
end


int_sum = 0.0;
int_sum2 = 0.0;

j=ymax/2;
for k=zmin+2:zmax-2
        dz   =  ( z2(k+1)-z2(k) );
        int_sum  = int_sum  + dz*z2(k)^2*q(k,j)^2;
        int_sum2 = int_sum2 + dz*q(k,j)^2;   
end
R3p = sqrt(fact*int_sum/int_sum2);

int_sum = 0.0;
int_sum2 = 0.0;

k=zmax/2;
for j=ymin+2:ymax-2
        dy   = (y2(j+1)-y2(j));
        int_sum  = int_sum  + dy*y2(j)^2*q(k,j)^2;
        int_sum2 = int_sum2 + dy*q(k,j)^2;   

end
R2p = sqrt(fact*int_sum/int_sum2);

ztmin=zmin;
ztmax=zmax;

for k=zmin+2:zmax-2
    if ( z2(k) < -R3p ) 
        ztmin = k;
    end
end
for k=zmax-2:-1:zmin+2
    if ( z2(k) > R3p ) 
        ztmax = k;
    end
end
%z2(ztmin)
%z2(ztmax)
ytmin=ymin;
ytmax=ymax;

for j=ymin+2:ymax-2
    if ( y2(j) < -R2p ) 
        ytmin = j;
    end
end
for j=ymax-2:-1:ymin+2
    if ( y2(j) > R2p ) 
        ytmax = j;
    end
end
%y2(ytmin)
%y2(ytmax)


intq1 = 0;
intq2 = 0;
intq3 = 0;
intq = 0;
Area = 0;
for k=ztmin:ztmax
    for j=ytmin:ytmax
      dA   =  ( z2(k+1)-z2(k) )*(y2(j+1)-y2(j));
      intq = intq + dA*q(k,j);
      intq1 = intq1 + dA*q1(k,j)*sqrt(1/3);
      intq2 = intq2 + dA*q2(k,j)*sqrt(1/3);    
      intq3 = intq3 + dA*q3(k,j)*sqrt(1/3);
      Area = Area+dA;
    end
end
p1 = intq1/intq;
p2 = intq2/intq;
p3 = intq3/intq;
check=p1+p2+p3;
intq=intq/Area;
intq3=intq3/Area;
   
%u1pmax=max(max(u1p));
%u2pmax=max(max(u2p));
%u3pmax=max(max(u3p));

qmax = max(max(q));
qvmax= max(max(q3));
rqv_q = qvmax/qmax*sqrt(1/3);
Ret = qmax*R2p/nu;
Frt = qmax/(N*R3p);
ReLam = sqrt(20*Ret/3);
r32 = R3p/R2p;

fprintf(file_2,'%13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G %13.8G\n',x2(1),qmax,qvmax,intq,rqv_q,R2p,R3p,r32,Ret,Frt,ReLam);

end
fclose(file_2);
%imagesc(y2,z2,q); 
