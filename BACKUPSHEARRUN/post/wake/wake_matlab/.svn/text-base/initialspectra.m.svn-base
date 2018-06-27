%@t
% \textbf{initialspectra.m}
%@h
%   Description:
%     Reads in the target spectrum and the actual spectrum and makes a plot
%     showing both.
%@q

close all, clear all, clc

filename='TargetSpectrum.dat';          %the data file
fid=fopen(filename,'r'); 
A = fscanf(fid, '%f %f', [2 inf]);
Ek = A(2,:);
k = A(1,:);
fclose(fid);


% plot log(k) vs. log[E(k)]
loglog(k(1:100),Ek(1:100)); hold on; loglog(k(1:100),Ek(1:100),'b.');
% loglog(k(1:10),Ek(1:10)); hold on; loglog(k(1:10),Ek(1:10),'b.');

filename2='Target';
fid2=fopen(filename2,'r');
fgetl(fid); 
fgetl(fid);
A2 = fscanf(fid2, '%f %f %f %f %f %f %f %f %f', [9 inf]);
Ek2 = A(2,:);
k2 = A(1,:);
fclose(fid2);

loglog(k(1:100),Ek(1:100),'r'); hold on; loglog(k(1:100),Ek(1:100),'r.');
