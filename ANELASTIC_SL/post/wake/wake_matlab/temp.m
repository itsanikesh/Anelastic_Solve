%@t
% \textbf{temp.m}
%@h
%   Description:
%     Integrate a variable over a plane. KYLE?
%@q

clear;
format long;
warning('off','MATLAB:dispatcher:InexactMatch')


dir1='~/work2/SP_Large/relax/results/';

ymin=1;
ymax=1026;

zmin=1;
zmax=514;

js=26;
ks=26;

Titer=105;
var1='rhop';

basename=[dir1,var1,'_'];
fname=Cfilename(basename,Titer);
[x y z Tvar1] = read_vtk(fname,1,1); 

int1=0;

for k=zmin+ks:zmax-ks
    for j=ymin+js:ymax-js
      dA = (z(k+1)-z(k) )*(y(j+1)-y(j));
      int1 = int1 + dA*Tvar1(k,j)*Tvar1(k,j);
    end
end

int1
