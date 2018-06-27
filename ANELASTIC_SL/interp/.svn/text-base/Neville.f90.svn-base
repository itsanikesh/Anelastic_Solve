function Neville(x,Q,n_int,xx)
!@t
! \textbf{function Neville(x,Q,n\_int,xx)}
!@h
!   Description:
!    Neville's Iterative Interpolation Algorithm
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
!@h
!   Comments:
!     Interpolates Q at point xx with order of accuracy n_int.
!@q

!RETURNS:
!Neville=VALUE OF DEPENDENT VARIABLE AT xx
!INPUTS:
!n = order of interpolation (n+1 = # of points)
!x(1),...,x(n+1)     INDEPENDENT VARIABLE 
!Q(1),...,Q(n+1)     DEPENDENT VARIABLE 
!xx                  INDEPENDENT VARIABLE EVALUATION POINT

 implicit none
 
!Returned Value
 real(8)              :: Neville
!Passed Variables
 integer               :: n_int
 real(8)              :: x(n_int+1),Q(n_int+1), xx
!Local Variables
 integer :: i,j

 do i = n_int,1,-1
  do j = 1,i
   Q(j) = (xx-x(j))*Q(j+1) - (xx-x(j+n_int+1-i))*Q(j)
   Q(j) = Q(j)/(x(j+n_int+1-i)-x(j))
  enddo
 enddo

 Neville = Q(1)

 return
end function Neville

