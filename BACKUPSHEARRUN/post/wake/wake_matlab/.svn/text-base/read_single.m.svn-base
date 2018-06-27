%@t
% \textbf{read.m}
%@h
%   Description:
%     Reads a formatted binary file. KYLE IS THIS DIFFERENT THAN READ.M???
%@q

%Kyle A. Brucker
%6/18/2006
%Reads in the following fortran write statement
%with big_endian byte ordering
% open(310,file=filename,status='unknown',form='unformatted',iostat=s1)
%  write(310) n_time,time,visc_nd,prandtl
%  write(310) bound
%  write(310) Var
% close(310)
y1='t1/U_25.pln';
warning('off','MATLAB:dispatcher:InexactMatch');
x1=fopen(y1,'rb','b'); %the data file
A = fread(x1,1,'int'); %read arbitrary header
ntime = fread(x1,1,'int'); %ntime
time = fread(x1,1,'float64'); %time
visc_nd = fread(x1,1,'float64'); %non-dimensional viscosity
prandtl = fread(x1,1,'float64'); %Prandtl Number
A       = fread(x1,1,'int'); %read trailer
A       = fread(x1,1,'int'); %read header
b1      = fread(x1,1,'int'); %y_min
b2      = fread(x1,1,'int'); %y_max
b3      = fread(x1,1,'int'); %z_min
b4      = fread(x1,1,'int'); %z_max
A       = fread(x1,1,'int'); %read trailer
A       = fread(x1,1,'int'); %read header
s1      = b2 - b1 + 1;
s2      = b4 - b3 + 1;
data    = fread(x1,[s1 s2],'float64');
A       = fread(x1,1,'int'); %read trailer

%load('y_grid.out'); %y_grid
%load('z_grid.out'); %z_grid

%y = y_grid(:,2); %grid
%z = z_grid(:,2); %grid
%contour(data)
[sizey sizex] = size(data);
[x,y] = MeshGrid(1:sizex,1:sizey);
fit = abs(PeakOD)*(exp(-0.5*(x-cx).^2./(sx^2)-0.5*(y-cy).^2./(sy^2)));
imagesc(fit)
data3 = data-fit;
imagesc(data3)
data4 = reallog(sqrt(data.^2+1));
imagesc( data4 )

%data5 = data4 - reallog(abs(PeakOD))*((-0.5*(x-cx).^2./(sx^2)-0.5*(y-cy).^2./(sy^2)));
%imagesc( data5 )
%time
%sx
%sy
%cx
%cy
%PeakOD
fclose(x1);
Result=[ntime time cx cy sx sy PeakOD];
