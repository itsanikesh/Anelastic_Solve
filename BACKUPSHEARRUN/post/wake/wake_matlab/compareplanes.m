% \textbf{compareplanes.m}
%@h
%   Description:
%     This program can be used to compare the results of two simulations.
%   Comments:
%     It reads in two data files for a given plane, calculates their difference
%     and produces a contour plot of their differences over time.
%@q

clear all; clc; close all
warning('off','MATLAB:dispatcher:InexactMatch'); format long;
% Matt de Stadler
% 10 Feb 2009

%data directory
% dir='./statpzero/';
dir='./plnpnotzero/';
% dir='./statmodrho/';
dir2='./kylepln/';

istart = 0; 
iend = 100;
istride = 50;
Tplot =0;

counter = 1; 
for iter=istart:istride:iend
%%%%%%%%%%%%%%%%%%%%%% Dataset #1 %%%%%%%%%%%%%%%%%%%%%
    loc='j0256_n';
    var='u1';                                   % Load u1' from set 1  
    basename=[dir,var,'_',loc];
    fname=Cfilename(basename,iter);
    [x y z u1] = read_vtkpln(fname,1,Tplot);
    
    var='u1';                                   % Load u1' from set 2  
    basename=[dir2,var,'_',loc];
    fname=Cfilename(basename,iter);
    [x y z u1k] = read_vtkpln(fname,1,Tplot);
    
    var='u2';                                   % Load u2' from set 1  
    basename=[dir,var,'_',loc];
    fname=Cfilename(basename,iter);
    [x y z u2] = read_vtkpln(fname,1,Tplot);
    
    var='u2';                                   % Load u2k' from set 2  
    basename=[dir2,var,'_',loc];
    fname=Cfilename(basename,iter);
    [x y z u2k] = read_vtkpln(fname,1,Tplot);    
    
    var='u3';                                   % Load u3' from set 1  
    basename=[dir,var,'_',loc];
    fname=Cfilename(basename,iter);
    [x y z u3] = read_vtkpln(fname,1,Tplot);
    
    var='u3';                                   % Load u3k' from set 2  
    basename=[dir2,var,'_',loc];
    fname=Cfilename(basename,iter);
    [x y z u3k] = read_vtkpln(fname,1,Tplot);    

    % plot of differences in data at given times
    figure(1); subplot(2,2,counter); imagesc(x,z,abs(u1-u1k)); xlabel('y'); ylabel('z'); colorbar; Title(['U1 difference at iteration ',num2str(iter)]); axis('square')
    figure(2); subplot(2,2,counter); imagesc(x,z,abs(u2-u2k)); xlabel('x'); ylabel('z'); colorbar; Title(['U2 difference at iteration ',num2str(iter)]); axis('square')
    figure(3); subplot(2,2,counter); imagesc(x,z,abs(u3-u3k)); xlabel('x'); ylabel('y'); colorbar; Title(['U3 difference at iteration ',num2str(iter)]); axis('square')    
    
%%%%%%%%%%%%%%%%%%%%%% Dataset #2 %%%%%%%%%%%%%%%%%%%%%
    loc='k0128_n';    
    var='u1';                     % Load u1' from set 1  
    basename=[dir,var,'_',loc];
    fname=Cfilename(basename,iter);
    [x y z u1] = read_vtkpln(fname,1,Tplot);
    
    var='u1';                    % Load u1k' from set 2  
    basename=[dir2,var,'_',loc];
    fname=Cfilename(basename,iter);
    [x y z u1k] = read_vtkpln(fname,1,Tplot);
    
    var='u2';                    % Load u2' from set 1  
    basename=[dir,var,'_',loc];
    fname=Cfilename(basename,iter);
    [x y z u2] = read_vtkpln(fname,1,Tplot);
    
    var='u2';                    % Load u2k' from set 2  
    basename=[dir2,var,'_',loc];
    fname=Cfilename(basename,iter);
    [x y z u2k] = read_vtkpln(fname,1,Tplot);    
    
    var='u3';                     % Load u3' from set 1  
    basename=[dir,var,'_',loc];
    fname=Cfilename(basename,iter);
    [x y z u3] = read_vtkpln(fname,1,Tplot);
    
    var='u3';                    % Load u3' from set 2  
    basename=[dir2,var,'_',loc];
    fname=Cfilename(basename,iter);
    [x y z u3k] = read_vtkpln(fname,1,Tplot);    

    % plot of differences in data at given times    
    figure(4); subplot(2,2,counter); imagesc(x,z,abs(u1-u1k)); xlabel('y'); ylabel('z'); colorbar; Title(['U1 difference at iteration ',num2str(iter)]); axis('square')
    figure(5); subplot(2,2,counter); imagesc(x,z,abs(u2-u2k)); xlabel('x'); ylabel('z'); colorbar; Title(['U2 difference at iteration ',num2str(iter)]); axis('square')
    figure(6); subplot(2,2,counter); imagesc(x,z,abs(u3-u3k)); xlabel('x'); ylabel('y'); colorbar; Title(['U3 difference at iteration ',num2str(iter)]); axis('square')        

    counter = counter +1;    
end
