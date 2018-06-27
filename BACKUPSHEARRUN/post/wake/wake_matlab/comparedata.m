%@t
% \textbf{comparedata.m}
%@h
%   Description:
%     This program can be used to compare the results of two simulations. It 
%     can be used to plot the time evolution of a given quantity or produce a 
%     2D contour plot of the difference between two sets of data files.
%@q

clear all; clc; close all
warning('off','MATLAB:dispatcher:InexactMatch'); format long;
% Matt de Stadler
% 10 Feb 2009

% data directories
% dir='./statpzero/';
% dir='./statneumannbcs/';
% dir='./statpnotzero/';
% dir='./statmodrho/';
% dirk='./kylestats/';
dir ='/work/temp/psolver/wake/results/relax/';
dirk ='/work/temp/psolver/wakeold/results/relax/';


istart = 1;
iend = 4;
istride = 1; 
Tplot = 0;                                       % if set to 1 this will produce a contour plot every time read_vtk is called

tplots = 1;                                     % 1 for showing time evolution plots
diffplots = 1;                                  % 1 for showing contour plot comparing u1(file1) -u1(file2) at a given time

counter = 1; 
for iter=istart:istride:iend
%%%%%%%%%%%%%%%%%%%%%% Load variables from both data files %%%%%%%%%%%%%%%%%%%%%
    var='u1p';                                      % Load u1'
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u1p] = read_vtk(fname,1,Tplot);

    var='u2p';                                      % Load u2'
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u2p] = read_vtk(fname,1,Tplot);

    var='u3p';                                      % Load u3'
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u3p] = read_vtk(fname,1,Tplot);

    var='pp';                                      % Load p'
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z pp] = read_vtk(fname,1,Tplot);

    var='rhop';                                      % Load rho'
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z rhop] = read_vtk(fname,1,Tplot);

    var='P';                                      % Load P
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z P] = read_vtk(fname,1,Tplot);
 
    var='U1';                                      % Load U1
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U1] = read_vtk(fname,1,Tplot);

    var='U2';                                       % Load U2
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U2] = read_vtk(fname,1,Tplot);

    var='U3';                                      % Load U3
    basename=[dir,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U3] = read_vtk(fname,1,Tplot);

%%%%%%%%%%%%%%%%%%%%%% End of Loading first set of files %%%%%%%%%%%%%%%%%%%%%

    var='u1p';
    basename=[dirk,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u1pk] = read_vtk(fname,1,Tplot);

    var='u2p';
    basename=[dirk,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u2pk] = read_vtk(fname,1,Tplot);

    var='u3p';
    basename=[dirk,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z u3pk] = read_vtk(fname,1,Tplot);

    var='pp';
    basename=[dirk,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z ppk] = read_vtk(fname,1,Tplot);

    var='rhop';
    basename=[dirk,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z rhopk] = read_vtk(fname,1,Tplot);

    var='P';
    basename=[dirk,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z Pk] = read_vtk(fname,1,Tplot);

    var='U1';
    basename=[dirk,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U1k] = read_vtk(fname,1,Tplot);

    var='U2';
    basename=[dirk,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U2k] = read_vtk(fname,1,Tplot);

    var='U3';
    basename=[dirk,var,'_'];
    fname=Cfilename(basename,iter);
    [x y z U3k] = read_vtk(fname,1,Tplot);

%%%%%%%%%%%%%%%%%%%%%% End of Loading second set of files %%%%%%%%%%%%%%%%% 

    kin = .5*(u1p.^2+u2p.^2+u3p.^2);                % calculate tke for the two files
    kink = .5*(u1pk.^2+u2pk.^2+u3pk.^2);

    tkeperc(counter) = abs(max(max(abs(kin)))-max(max(abs(kink))))/max(max(abs(kink)));

%%%%%%%%%%%%%%%%%%%%%% Get the maximum value of variables over time %%%%%%%%
    U1oft(counter,1) = max(max(abs(U1)));
    U1oft(counter,2) = max(max(abs(U1k)));

    U2oft(counter,1) = max(max(abs(U2)));
    U2oft(counter,2) = max(max(abs(U2k)));

    U3oft(counter,1) = max(max(abs(U3)));
    U3oft(counter,2) = max(max(abs(U3k)));

    u1poft(counter,1) = max(max(abs(u1p)));
    u1poft(counter,2) = max(max(abs(u1pk)));

    u2poft(counter,1) = max(max(abs(u2p)));
    u2poft(counter,2) = max(max(abs(u2pk)));

    u3poft(counter,1) = max(max(abs(u3p)));
    u3poft(counter,2) = max(max(abs(u3pk)));

    tkeoft(counter,1) = max(max(abs(kin)));
    tkeoft(counter,2) = max(max(abs(kink)));

    rhopoft(counter,1) = max(max(abs(rhop)));
    rhopoft(counter,2) = max(max(abs(rhopk)));

    xoft(counter,1) = x;

%%%%%%%%%%%%%%%%%%%%%% Get the maximum difference between variables over time %%%%%%%%
    u1pdiff(counter) = max(max(abs(u1p-u1pk)));
    u2pdiff(counter) = max(max(abs(u2p-u2pk)));
    u3pdiff(counter) = max(max(abs(u3p-u3pk)));
    ppdiff(counter) = max(max(abs(pp-ppk)));
    rhopdiff(counter) = max(max(abs(rhop-rhopk)));
    timing(counter) = iter;

    counter = counter + 1; 
end

if (tplots == 1)
%%%%%%%%%%%%%%%%%%%%%% Plot perturbation quantities over time %%%%%%%%
figure; subplot(2,2,1); plot(xoft(:,1),u1poft(:,1),'b'); hold on; plot(xoft(:,1),u1poft(:,2),'k'); plot(xoft(:,1),u1poft(:,1),'b.'); plot(xoft(:,1),u1poft(:,2),'k.');
xlabel('x/D'); ylabel('u1p'); legend('new run', 'original','Location','South')
subplot(2,2,2); plot(xoft(:,1),u2poft(:,1),'b'); hold on; plot(xoft(:,1),u2poft(:,2),'k'); plot(xoft(:,1),u2poft(:,1),'b.'); plot(xoft(:,1),u2poft(:,2),'k.'); 
xlabel('x/D'); ylabel('u2p'); legend('new run', 'original','Location','South')
subplot(2,2,3); plot(xoft(:,1),u3poft(:,1),'b'); hold on; plot(xoft(:,1),u3poft(:,2),'k'); plot(xoft(:,1),u3poft(:,1),'b.'); plot(xoft(:,1),u3poft(:,2),'k.'); 
xlabel('x/D'); ylabel('u3p'); legend('new run', 'original','Location','South')
subplot(2,2,4); plot(xoft(:,1),tkeoft(:,1),'b'); hold on; plot(xoft(:,1),tkeoft(:,2),'k'); plot(xoft(:,1),tkeoft(:,1),'b.'); plot(xoft(:,1),tkeoft(:,2),'k.'); 
xlabel('x/D'); ylabel('tke'); legend('new run', 'original','Location','South')

figure; plot(xoft(:,1),rhopoft(:,1),'b'); hold on; plot(xoft(:,1),rhopoft(:,2),'k'); plot(xoft(:,1),rhopoft(:,1),'b.'); plot(xoft(:,1),rhopoft(:,2),'k.');
xlabel('x/D'); ylabel('rhop'); legend('new run', 'original','Location','South')

%%%%%%%%%%%%%%%%%%%%%% Plot U1,U2,U3 over time %%%%%%%%
figure; subplot(2,2,1); plot(xoft(:,1),U1oft(:,1),'b'); hold on; plot(xoft(:,1),U1oft(:,2),'k');
xlabel('x/D'); ylabel('U1'); legend('new run', 'original','Location','South')
subplot(2,2,2); plot(xoft(:,1),U2oft(:,1),'b'); hold on; plot(xoft(:,1),U2oft(:,2),'k');
xlabel('x/D'); ylabel('U2'); legend('new run', 'original','Location','South')
subplot(2,2,3); plot(xoft(:,1),U3oft(:,1),'b'); hold on; plot(xoft(:,1),U3oft(:,2),'k');
xlabel('x/D'); ylabel('U3'); legend('new run', 'original','Location','South')

figure; plot(timing,u1pdiff, 'k'); hold on; plot(timing,u2pdiff, 'b'); plot(timing,u3pdiff, 'g'); plot(timing,ppdiff, 'r'); plot(timing,rhopdiff, 'y')
legend('u1p','u2p','u3p','pp','rhop');
plot(timing,u1pdiff, 'k.'); hold on; plot(timing,u2pdiff, 'b.'); plot(timing,u3pdiff, 'g.'); plot(timing,ppdiff, 'r.'); plot(timing,rhopdiff, 'y.')
xlabel('iteration'); ylabel('maximum difference')
 
 figure; plot(timing,tkeperc,'k.'); hold on; plot(timing,tkeperc,'k');
 xlabel('iteration'); ylabel('relative error in tke');
end

if (diffplots ==1)
%%%%%%% Plot difference in perturbation quantities between 2 runs at a given time %%
figure; subplot(2,3,1); imagesc(y,z,abs(u1p-u1pk)); xlabel('y'); ylabel('z'); colorbar; Title('u1p');
subplot(2,3,2); imagesc(y,z,abs(u2p-u2pk)); xlabel('y'); ylabel('z'); colorbar; Title('u2p');
subplot(2,3,3); imagesc(y,z,abs(u3p-u3pk)); xlabel('y'); ylabel('z'); colorbar; Title('u3p');
subplot(2,3,4); imagesc(y,z,abs(pp-ppk)); xlabel('y'); ylabel('z'); colorbar; Title('pp');
subplot(2,3,5); imagesc(y,z,abs(rhop-rhopk)); xlabel('y'); ylabel('z'); colorbar; Title('rhop');
subplot(2,3,6); imagesc(y,z,abs(kin-kink)); xlabel('y'); ylabel('z'); colorbar; Title('tke');

%%%%%%% Plot difference in U1, U2, U3, P between 2 runs at a given time %%%
figure; subplot(2,2,1); imagesc(y,z,abs(U1-U1k)); xlabel('y'); ylabel('z'); colorbar; Title('U1')
subplot(2,2,2); imagesc(y,z,abs(U2-U2k)); xlabel('y'); ylabel('z'); colorbar; Title('U2')
subplot(2,2,3); imagesc(y,z,abs(U3-U3k)); xlabel('y'); ylabel('z'); colorbar; Title('U3')
subplot(2,2,4); imagesc(y,z,abs(P-Pk)); xlabel('y'); ylabel('z'); colorbar; Title('P')
end
