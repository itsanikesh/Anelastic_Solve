function [x y z data2d] = read_vtk(filename,pln,Tplot);
%@t
% \textbf{read\_vtk.m}
%@h
%   Description:
%     Reads a formatted vtk file.
%
%   Comments:
%     Check to see if it works for all Variables Matt!!!
%@q

%Kyle A. Brucker
%08/08/2008
%Reads in a vtk file
%filename='u1p_00100.vtk';

vtkfile=fopen(filename,'r','b'); %the data file
A=fread(vtkfile,27,'*char'); %read arbitrary header
B=fread(vtkfile,26,'*char'); %read arbitrary header
C=fread(vtkfile,7,'*char'); %read arbitrary header
D=fread(vtkfile,25,'*char'); %read arbitrary header
E=fread(vtkfile,11,'*char'); %read arbitrary header
F=fread(vtkfile,5,'*char'); %read arbitrary header
G=fread(vtkfile,5,'*char'); %read arbitrary header
H=fread(vtkfile,5,'*char'); %read arbitrary header
nx=str2double(F);
ny=str2double(G);
nz=str2double(H);
I=fread(vtkfile,27,'*char'); %read arbitrary header\
x = fread(vtkfile,nx,'float');
J=fread(vtkfile,27,'*char'); %rea arbitrary header\
y = fread(vtkfile,ny,'float');
K=fread(vtkfile,27,'*char'); %read arbitrary header\
z = fread(vtkfile,nz,'float');
L=fread(vtkfile,27,'*char'); %read arbitrary header\
M=fread(vtkfile,25,'*char'); %read arbitrary header\
N=fread(vtkfile,21,'*char'); %read arbitrary header\
count=nx*ny*nz;
data1d = fread(vtkfile,count,'float');
data = reshape(data1d,[nx ny nz]);
data2d=zeros(size(data,2),size(data,3));
dataT=data(pln,:,:);
data2dT=reshape(dataT,[ny nz]);
data2d=data2dT';
if (Tplot==1)
    imagesc(y,z,data2d)
end
fclose(vtkfile);





