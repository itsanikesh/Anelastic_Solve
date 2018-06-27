% \textbf{read\_vtkpln.m}
%@h
%   Description:
%     Reads in a vtk plane file and automatically determines whether it is an
%     xy, yz or xz plane. 
%   Comments:
%     Works for the names u1,u2,u3,uu,vv,ww, rho, p, and omg1,omg2,omg3.
%@q

function [x y z data2d] = read_vtkpln (filename,pln,Tplot);
% Matt de Stadler
% 10 Feb 2009
% filename='u2_i0384_n00000.vtk';

% determine whether we have a u,v,w,p,rho or omg plane
usl = find('_'==filename,2,'last'); % underscore location
if (filename(usl(1)-2) == 'u' | filename(usl(1)-2) == 'v' | filename(usl(1)-2) == 'w'),
  varnml = 2;                %variable name length
end
if (filename(usl(1)-1) == 'p'),
  varnml = 1;                %variable name length
end
if (filename(usl(1)-3:usl(1)-1) == 'rho'),
  varnml = 3;                %variable name length
end
if (filename(usl(1)-4:usl(1)-2) == 'omg'),
  varnml = 4;                %variable name length
end

vtkfile=fopen(filename,'r','b'); %the data file
A=fread(vtkfile,27,'*char'); %read arbitrary header
B=fread(vtkfile,6+varnml,'*char'); %read arbitrary header
C=fread(vtkfile,7,'*char'); %read arbitrary header
D=fread(vtkfile,25,'*char'); %read arbitrary header
E=fread(vtkfile,11,'*char'); %read arbitrary header
F=fread(vtkfile,5,'*char'); %read arbitrary header
G=fread(vtkfile,5,'*char'); %read arbitrary header
H=fread(vtkfile,5,'*char'); %read arbitrary header
nx=str2double(F);
ny=str2double(G);
nz=str2double(H);
I=fread(vtkfile,28,'*char'); %read arbitrary header\
x = fread(vtkfile,nx,'float');
J=fread(vtkfile,27,'*char'); %read arbitrary header\
y = fread(vtkfile,ny,'float');
K=fread(vtkfile,27,'*char'); %read arbitrary header\
z = fread(vtkfile,nz,'float');
L=fread(vtkfile,27,'*char'); %read arbitrary header\
M=fread(vtkfile,25,'*char'); %read arbitrary header\
N=fread(vtkfile,21,'*char'); %read arbitrary header\
count=nx*ny*nz;

% yz plane
if nx == 1
    data1d = fread(vtkfile,count,'float');
    data = reshape(data1d,[nx ny nz]);
    sizedata1d = size(data1d);
    data2d=zeros(size(data,2),size(data,3));
    dataT=data(pln,:,:);
    data2dT=reshape(dataT,[ny nz]);
    data2d=data2dT';
    if (Tplot==1)
        figure; imagesc(y,z,data2d); xlabel('y'); ylabel('z');
    end
end

% xz plane
if ny == 1
    data1d = fread(vtkfile,count,'float');
    data = reshape(data1d,[nx ny nz]);
    sizedata1d = size(data1d);
    data2d=zeros(size(data,1),size(data,3));
    dataT=data(:,pln,:);
    data2dT=reshape(dataT,[nx nz]);
    data2d=data2dT';
    if (Tplot==1)
        figure; imagesc(x,z,data2d); xlabel('x'); ylabel('z');
    end
end

% nz  plane
if nz == 1
    data1d = fread(vtkfile,count,'float');
    data = reshape(data1d,[nx ny nz]);
    sizedata1d = size(data1d);
    data2d=zeros(size(data,1),size(data,3));
    dataT=data(:,:,pln);
    data2dT=reshape(dataT,[nx ny]);
    data2d=data2dT';
    if (Tplot==1)
        figure; imagesc(x,z,data2d); xlabel('x'); ylabel('y');
    end
end

fclose(vtkfile);
