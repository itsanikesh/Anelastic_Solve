function [filename] = Cfilename(basename,iter); 
%@t
% \textbf{Cfilename.m}
%@h
%   Description:
%     Generates a filename having 5 digits for the iteration number from 
%     a base file name and the current iteration. The filename ends in 
%     .vtk
%@q
%   Current Code Owner:
%     Matt de Stadler (mdestadl@ucsd.edu)

%   Modification History
%     Version   Date     Comment 
%     -------   ----     ------- 
%     1.0       07/2008  Original code. [Kyle A. Brucker] 

siter = int2str(iter);
stemp=basename;
if (iter < 10000)
    stemp=[stemp,'0'];
end
if (iter < 1000)
    stemp=[stemp,'0'];
end
if (iter < 100) 
    stemp=[stemp,'0'];
end
if (iter < 10) 
    stemp=[stemp,'0'];
end
stemp=[stemp,siter];
filename = [stemp,'.vtk'];
