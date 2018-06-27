%@t
% \textbf{katan.m}
%@h
%   Description:
%     Contains a function which calculates the angle for given y,z data.
%
%   Comments:
%     Y is the horizontal component and z is the vertical component. 
%@q

function theta=katan(y1,z1)

    Pi = 4.0*atan(1.0);

    if ( y1 > 0) & (z1 > 0 )       % Q1	     Quadrants:
        theta = atan( abs(z1/y1) );%	 	    	 2 | 1
    elseif (y1==0 ) & ( z1 > 0 )   % Pi/2			 -----		
        theta = Pi/2.d0;	   %	       		 3 | 4
    elseif (y1 < 0 ) & ( z1>0 )    % Q2 
        theta = Pi - atan( abs(z1/y1) );
    elseif (z1 == 0 ) & ( y1 < 0 ) % Pi
        theta = Pi;
    elseif (y1 < 0 ) & ( z1 < 0 )   % Q3
        theta = Pi + atan( abs(z1/y1) );
    elseif (y1 == 0) & ( z1 < 0 )   % 3Pi/2
        theta = 3.0*Pi/2.0;
    elseif (y1 > 0 ) & ( z1 < 0 )   % Q4
        theta = 2.0*Pi - atan( abs(z1/y1) );
    elseif ( z1 == 0 ) & ( y1 > 0 ) % 0,2Pi
        theta = 2.0*Pi;
    else
        theta = atan(z1/y1);
    end
