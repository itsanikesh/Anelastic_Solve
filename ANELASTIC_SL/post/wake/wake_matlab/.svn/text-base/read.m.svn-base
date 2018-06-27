%@t
% \textbf{read.m}
%@h
%   Description:
%     Reads a formatted binary file.
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

warning('off','MATLAB:dispatcher:InexactMatch');

file_2=fopen('./Fr2/fit.dat','wt');
c = fprintf(file_2,'#ntime time cx cy sx sy PeakOD\n');
for i=0:25:3000,
    j = int2str(i);
    z= ['./Fr2/U_',j];
    y= [z,'.pln'];

x=fopen(y,'rb','b'); %the data file
A = fread(x,1,'int'); %read arbitrary header
ntime = fread(x,1,'int'); %ntime
time = fread(x,1,'float64'); %time
visc_nd = fread(x,1,'float64'); %non-dimensional viscosity
prandtl = fread(x,1,'float64'); %Prandtl Number
A       = fread(x,1,'int'); %read trailer
A       = fread(x,1,'int'); %read header
b1      = fread(x,1,'int'); %y_min
b2      = fread(x,1,'int'); %y_max
b3      = fread(x,1,'int'); %z_min
b4      = fread(x,1,'int'); %z_max
A       = fread(x,1,'int'); %read trailer
A       = fread(x,1,'int'); %read header
s1      = b2 - b1 + 1;
s2      = b4 - b3 + 1;
data    = fread(x,[s1 s2],'float64');
A       = fread(x,1,'int'); %read trailer

%load('y_grid.out'); %y_grid
%load('z_grid.out'); %z_grid

%y = y_grid(:,2); %grid
%z = z_grid(:,2); %grid
%contour(x1,y1,data)
%imagesc(y,z,data)
[cx,cy,sx,sy,PeakOD] = Gaussian2D(data,0.0001);
%time
%sx
%sy
%cx
%cy
%PeakOD
fclose(x);
Result=[ntime time cx cy sx sy PeakOD];


fprintf(file_2,'%5d %13.6G %13.6G %13.6G %13.6G %13.6G %13.6G\n',Result);
end
fclose(file_2);
