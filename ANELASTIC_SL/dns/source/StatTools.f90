












subroutine vorticity(varOUT,term1,cmp)
!@t
! \textbf{subroutine vorticity(varOUT,term1,cmp)}
!@h
!   Description:
!     Calculates the full vorticity field (with the mean included).
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

!NOTE UPON RETURN THE FOLLOWING ARE AVAILABLE: 
! term1=prl_u3_prl_x2, u2_tmp2=u2_cen, u3_tmp2=u3_cen (cmp=1)
! term1=prl_u1_prl_x3, u1_tmp2=u1_cen, u3_tmp2=u3_cen (cmp=2)
! term1=prl_u2_prl_x1, u1_tmp2=u1_cen, u2_tmp2=u2_cen (cmp=3)

 use ntypes, only: r8
 use Flow,   only: u,v,w, u1_tmp2, u2_tmp2, u3_tmp2
 use domain, only: sx,ex,sy,ey,sz,ez
 use IO,     only: IOUT
 implicit none

!Passed Variables
 real(r8),intent(out) :: varOUT(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out) :: term1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(in)   :: cmp

!Local Variables
 integer              :: i,j,k

 varOUT = 0.d0

 if (cmp.EQ.1) then 
  !omg_1 = prl_u3_prl_x2 - prl_u2_prl_x3
  u2_tmp2 = 0.d0
  u3_tmp2 = 0.d0
  call center_velocity(w,u3_tmp2,3)
  call center_velocity(v,u2_tmp2,2)
  !prl_u3_prl_x2 
  call deriv(u3_tmp2,term1,2)
  !prl_u2_prl_x3
  call deriv(u2_tmp2,varOUT,3)
 elseif (cmp.EQ.2) then 
  !omg_2 = prl_u1_prl_x3 - prl_u3_prl_x1
  u1_tmp2 = 0.d0
  u3_tmp2 = 0.d0
  call center_velocity(u,u1_tmp2,1)
  call center_velocity(w,u3_tmp2,3)
  !prl_u1_prl_x3 
  call deriv(u1_tmp2,term1,3)
  !prl_u3_prl_x1
  call deriv(u3_tmp2,varOUT,1)

 elseif (cmp.EQ.3) then 
  !omg_3 = prl_u2_prl_x1 - prl_u1_prl_x2
  u1_tmp2 = 0.d0
  u2_tmp2 = 0.d0
  call center_velocity(u,u1_tmp2,1) 
  call center_velocity(v,u2_tmp2,2) 
  !prl_u2_prl_x1 
  call deriv(u2_tmp2,term1,1)
  !prl_u1_prl_x2
  call deriv(u1_tmp2,varOUT,2)
 else
  write(IOUT,'(a60,i2)') "INVALID COMPONENT FOR VORTICITY: cmp must be 1,2,3.  cmp= ", cmp
  stop
 endif

 do k=sz,ez
  do j=sy,ey
   do i=sx,ex
    varOUT(i,j,k) = term1(i,j,k) - varOUT(i,j,k) 
   enddo
  enddo
 enddo

return
end subroutine vorticity

subroutine center_velocity(Uin,Ucen,dir)
!@t
! \textbf{subroutine center\_velocity(Uin,Ucen,dir)}
!@h
!   Description:
!     Interpolate edge quantity to cell center.
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
!     dir can be 1, 2, or 3.
!@q
 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez
 use IO,     only: IOUT
 use grid,    only: xc,xe,yc,ye,zc,ze,gridtype
 use ratios
 implicit none

!Passed Variables
 real(r8),intent(in)         :: Uin(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) 
 real(r8),intent(out)        :: Ucen(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) 
 integer,intent(in)          :: dir

!Local Variables
 integer              :: i,j,k,err
 real(r8)             :: dep(0:5),ind(0:5),loc,interpoly
!Zero Output Array
 Ucen=0.d0

!*************************************************
!********************X1***************************
!*************************************************
if(dir.EQ.1) then
 select case(gridType(1))
  case('uniform')
   !U
    do k=sz,ez
     do j=sy,ey
      do i=sx+1,ex-1
!       Ucen(i,j,k)=r_9_16*(uin(i,j,k)+uin(i-1,j,k)) - r_1_16*(uin(i+1,j,k)+uin(i-2,j,k))
      Ucen(i,j,k)=r_1_2*(uin(i,j,k)+uin(i-1,j,k))
      enddo
     enddo
    enddo
    do k=sz,ez
     do j=sy,ey
      Ucen(sx,j,k)=r_1_2*(uin(sx,j,k)+uin(sx-1,j,k))
      Ucen(ex,j,k)=r_1_2*(uin(ex,j,k)+uin(ex-1,j,k))
     enddo
    enddo
   case('stretched')
   write(IOUT,'(a)') "STRETCHED GRIDS NOT IMPLEMENTED"
   !Try using a 4th order interpolating polynomial
  do k=sz,ez
   do j=sy,ey
    do i=sx+1,ex-1
     dep=uin(i-2:i+2,j,k)
     ind=xe(i-2:i+2)
     loc=xc(i)
     Ucen(i,j,k)= interpoly(dep,ind,loc,4)
    enddo
   enddo
  enddo
  do k=sz,ez
   do j=sy,ey
     dep=uin(sx-1:sx+3,j,k)
     ind=xe(sx-1:sx+3)
     loc=xc(sx)
     Ucen(sx,j,k)= interpoly(dep,ind,loc,4)
     dep=uin(ex-3:ex+1,j,k)
     ind=xe(ex-3:ex+1)
     loc=xc(ex)
     Ucen(ex,j,k)= interpoly(dep,ind,loc,4)
   enddo
  enddo


  end select

  call ghost(Ucen,'u',err)

!*************************************************
!********************X2***************************
!*************************************************
elseif (dir.EQ.2) then 
 select case(gridType(2))
  case('uniform')
 !V
  do k=sz,ez
   do j=sy+1,ey-1
    do i=sx,ex
!     Ucen(i,j,k)=r_9_16*(uin(i,j,k)+uin(i,j-1,k)) - r_1_16*(uin(i,j+1,k)+uin(i,j-2,k))
    Ucen(i,j,k)=r_1_2*(uin(i,j,k)+uin(i,j-1,k))

    enddo
   enddo
  enddo
  do k=sz,ez
   do i=sx,ex
    Ucen(i,sy,k)=r_1_2*(uin(i,sy,k)+uin(i,sy-1,k))
    Ucen(i,ey,k)=r_1_2*(uin(i,ey,k)+uin(i,ey-1,k))
   enddo
  enddo

  case('stretched')
   write(IOUT,'(a)') "STRETCHED GRIDS NOT IMPLEMENTED"
  do k=sz,ez
   do j=sy+1,ey-1
    do i=sx,ex
     dep=uin(i,j-2:j+2,k)
     ind=ye(j-2:j+2)
     loc=yc(j)
     Ucen(i,j,k)= interpoly(dep,ind,loc,4)
    enddo
   enddo
  enddo
  do k=sz,ez
   do i=sx,ex
     dep=uin(i,sy-1:sy+3,k)
     ind=ye(sy-1:sy+3)
     loc=yc(sy)
     Ucen(i,sy,k)= interpoly(dep,ind,loc,4)
     dep=uin(i,ey-3:ey+1,k)
     ind=ye(ey-3:ey+1)
     loc=yc(ey)
     Ucen(i,ey,k)= interpoly(dep,ind,loc,4)
   enddo
  enddo

 end select

 call ghost(Ucen,'v',err)

!*************************************************
!********************X3***************************
!*************************************************
elseif (dir.EQ.3) then 
 select case(gridType(3))
  case('uniform')

 !W
  do k=sz+1,ez-1
   do j=sy,ey
    do i=sx,ex
!     Ucen(i,j,k)=r_9_16*(uin(i,j,k)+uin(i,j,k-1)) - r_1_16*(uin(i,j,k+1)+uin(i,j,k-2))
    Ucen(i,j,k)=r_1_2*(uin(i,j,k)+uin(i,j,k-1))
    enddo
   enddo
  enddo
  do j=sy,ey
   do i=sx,ex
    Ucen(i,j,sz)=r_1_2*(uin(i,j,sz)+uin(i,j,sz-1))
    Ucen(i,j,ez)=r_1_2*(uin(i,j,ez)+uin(i,j,ez-1))
   enddo
  enddo

   case('stretched')
   write(IOUT,'(a)') "STRETCHED GRIDS NOT IMPLEMENTED"
  do k=sz+1,ez-1
   do j=sy,ey
    do i=sx,ex
     dep=uin(i,j,k-2:k+2)
     ind=ze(k-2:k+2)
     loc=zc(k)
     Ucen(i,j,k)= interpoly(dep,ind,loc,4)
    enddo
   enddo
  enddo
  do j=sy,ey
   do i=sx,ex
     dep=uin(i,j,sz-1:sz+3)
     ind=ze(sz-1:sz+3)
     loc=zc(sz)
     Ucen(i,j,sz)= interpoly(dep,ind,loc,4)
     dep=uin(i,j,ez-3:ez+1)
     ind=ze(ez-3:ez+1)
     loc=zc(ez)
     Ucen(i,j,ez)= interpoly(dep,ind,loc,4)
   enddo
  enddo

  end select

  call ghost(Ucen,'w',err)

 else
 !Invalid direction
  write(IOUT,'(a60,i2)') "INVALID DIRECTION IN center_velocities dir must be 1,2,3.  dir= ", dir
 endif

return
end subroutine center_velocity


subroutine deriv(varIN,varOUT,dir)
!@t
! \textbf{subroutine deriv(varIN,varOUT,dir)}
!@h
!   Description:
!     Calculate the derivative of a variable in a given direction over the
!     whole field.
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
!     Valid For Cell Centered Variables, ONLY for post processing/statistics!
!@q
 use ntypes, only: r8 
 use ratios
 use grid,   only: dxc,dyc,dzc,xe,xc,ye,yc,ze,zc, gridtype,dze,dxe,dye
 use domain, only: sx,ex,sy,ey,sz,ez
 use IO,     only: IOUT
 implicit none

 !Passed Variables
 real(r8),intent(in)  :: VarIN(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) 
 real(r8),intent(out) :: VarOUT(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) 
 integer,intent(in)   :: dir
 !Local Variables
 integer              :: i,j,k
 real(r8)             :: t_2, t_4, diffpoly,ind(0:5),dep(0:5),loc
 
 !Zero Output Array
 varOUT=0.d0

!*************************************************
!********************X1***************************
!*************************************************
  if(dir.EQ.1) then
   select case(gridType(1))
   case('uniform')
  !d()/dx1
   do k=sz,ez
    do j=sy,ey
     do i=sx+1,ex-1
      t_2=r_1_2*( varIN(i+1,j,k)-varIN(i-1,j,k) )/dxc(i)
      t_4=r_1_4*( varIN(i+2,j,k)-varIN(i-2,j,k) )/dxc(i)
!      varOUT(i,j,k)=r_4_3*t_2-r_1_3*t_4
      varOUT(i,j,k)=t_2
     enddo
    enddo
   enddo
   do k=sz,ez
    do j=sy,ey
      varOUT(sx,j,k)=r_1_2*( varIN(sx+1,j,k)-varIN(sx-1,j,k) )/(dxc(sx) )
      varOUT(ex,j,k)=r_1_2*( varIN(ex+1,j,k)-varIN(ex-1,j,k) )/(dxc(ex) ) 
    enddo
   enddo


   case('stretched')
   write(IOUT,'(a)') "STRETCHED GRIDS NOT IMPLEMENTED"
   !Try using a 4th order interpolating polynomial
   do k=sz,ez
    do j=sy,ey
     do i=sx+1,ex-1
      dep=varIN(i-2:i+2,j,k)
      ind=xe(i-2:i+2)
      loc=xc(i)
      varOUT(i,j,k)= diffpoly(dep,ind,loc,4)
     enddo
    enddo
   enddo
   do k=sz,ez
    do j=sy,ey
      dep=varIN(sx-1:sx+3,j,k)
      ind=xe(sx-1:sx+3)
      loc=xc(sx)
      varOUT(sx,j,k)= diffpoly(dep,ind,loc,4)
      dep=varIN(ex-3:ex+1,j,k)
      ind=xe(ex-3:ex+1)
      loc=xc(ex)
      varOUT(ex,j,k)= diffpoly(dep,ind,loc,4)
    enddo
   enddo
   case DEFAULT
   write(IOUT,'(a)') "GRID TYPE: "//trim(gridtype(1))//" NOT AVAILABLE FOR X1 (deriv.f90)"



   end select


!*************************************************
!********************X2***************************
!*************************************************



  elseif (dir.EQ.2) then 
   select case(gridType(2))
   case('uniform')
   !d()/dx2
   do k=sz,ez
    do j=sy+1,ey-1
     do i=sx,ex
      t_2=r_1_2*( varIN(i,j+1,k)-varIN(i,j-1,k) )/dyc(j)
      t_4=r_1_4*( varIN(i,j+2,k)-varIN(i,j-2,k) )/dyc(j)
!      varOUT(i,j,k)=r_4_3*t_2-r_1_3*t_4
      varOUT(i,j,k)=t_2
     enddo
    enddo
   enddo
   do k=sz,ez
    do i=sx,ex
      varOUT(i,sy,k)=r_1_2*( varIN(i,sy+1,k)-varIN(i,sy-1,k) )/(dyc(sy) )
      varOUT(i,ey,k)=r_1_2*( varIN(i,ey+1,k)-varIN(i,ey-1,k) )/(dyc(ey) ) 
    enddo
   enddo
   case('stretched')
   write(IOUT,'(a)') "STRETCHED GRIDS NOT IMPLEMENTED"
    do k=sz,ez
    do j=sy+1,ey-1
     do i=sx,ex
      dep=varIN(i,j-2:j+2,k)
      ind=ye(j-2:j+2)
      loc=yc(j)
      VarOUT(i,j,k)= diffpoly(dep,ind,loc,4)
     enddo
    enddo
   enddo
   do k=sz,ez
    do i=sx,ex
      dep=varIN(i,sy-1:sy+3,k)
      ind=ye(sy-1:sy+3)
      loc=yc(sy)
      varOUT(i,sy,k)= diffpoly(dep,ind,loc,4)
      dep=varIN(i,ey-3:ey+1,k)
      ind=ye(ey-3:ey+1)
      loc=yc(ey)
      varOUT(i,ey,k)= diffpoly(dep,ind,loc,4)
    enddo
   enddo
   case DEFAULT
   write(IOUT,'(a)') "GRID TYPE: "//trim(gridtype(2))//" NOT AVAILABLE FOR X2 (deriv.f90)"

  end select

!*************************************************
!********************X3***************************
!*************************************************

  elseif (dir.EQ.3) then 
   select case(gridType(3))
   case('uniform','custom')
  !d()/dx3
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      t_4=dze(k-1)/dze(k)*varIN(i,j,k+1)+(1.d0-dze(k-1)/dze(k))*varIN(i,j,k) 
      t_2=( t_4-varIN(i,j,k-1) )/(2.d0*dze(k-1))
      varOUT(i,j,k)= t_2
     enddo
    enddo
   enddo


   case('stretched')
   write(IOUT,'(a)') "STRETCHED GRIDS NOT IMPLEMENTED"
   do k=sz+1,ez-1
    do j=sy,ey
     do i=sx,ex
      dep=varIN(i,j,k-2:k+2)
      ind=ze(k-2:k+2)
      loc=zc(k)
      varOUT(i,j,k)= diffpoly(dep,ind,loc,4)
     enddo
     enddo
   enddo
   do j=sy,ey
    do i=sx,ex
      dep=varIN(i,j,sz-1:sz+3)
      ind=ze(sz-1:sz+3)
      loc=zc(sz)
      varOUT(i,j,sz)= diffpoly(dep,ind,loc,4)
      dep=varIN(i,j,ez-3:ez+1)
      ind=ze(ez-3:ez+1)
      loc=zc(ez)
      varOUT(i,j,ez)= diffpoly(dep,ind,loc,4)
    enddo
   enddo
   case DEFAULT
   write(IOUT,'(a)') "GRID TYPE: "//trim(gridtype(3))//" NOT AVAILABLE FOR X3 (deriv.f90)"
 
   end select

  else
   !Invalid direction
   write(IOUT,'(a60,i2)') "INVALID DIRECTION IN center_velocities dir must be 1,2,3.  dir= ", dir
  endif

return
end subroutine deriv
function diffpoly(dep_var,ind_var,eval_loc,n)
!@t
! \textbf{function diffpoly(dep\_var,ind\_var,eval\_loc,n)}
!@h
!   Description:
!     Differentiation algorithm based on a Lagrangian interpolating
!     polynomial.
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
!     the numerical derivatives have order of O(h^n) in this algorithm, so
!     long as eval_loc is located near the middle. Only relevant for 
!     highly stretched grids. Should be TESTED FURTHER BEFORE USAGE.
!@q
 use ntypes, only: r8

!Function Variable
 real(r8) :: diffpoly

!Passed Variables
 integer,intent(in)            :: n
 real(r8),intent(in)           :: dep_var(0:n)
 real(r8),intent(in)           :: ind_var(0:n)
 real(r8),intent(in)           :: eval_loc

!Local Variables
 real(r8) :: Df,Prod
 real(r8) :: a(0:n)
 integer  :: j

 do k=0,n
  a(k)=ind_var(k)
 enddo

 do j=1,n
  do k=n,j,-1
   a(k)=(A(k)-A(k-1))/(dep_var(k)-dep_var(k-j))
  enddo
 enddo

 Df=a(1)
 Prod=1
 do k=2,n
  Prod=Prod*(eval_loc-dep_var(k-1))
  Df=Df+Prod*a(k)
 enddo

 diffpoly=Df
 return
end function

subroutine divdiffs(x,y,a,n)
!@t
! \textbf{subroutine divdiffs(x,y,a,n)}
!@h
!   Description:
!     Something to do with divided differences??? KYLE?
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: r8
 implicit none

!Passed Variables
 integer,intent(in)            :: n
 real(r8),intent(in)           :: x(0:n)
 real(r8),intent(in)           :: y(0:n)
 real(r8),intent(out)           :: a(0:n)

!Local Variables
 real(r8) :: D(0:n,0:n)
 integer  :: j,k

 do k=0,n
  D(k,0)=y(k)
 enddo

 do j=1,n
  do k=j,n
   D(k,j)=(D(k,j-1)-D(k-1,j-1))/(x(k)-x(k-j))
  enddo
 enddo

 do k=0,n
  a(k)=D(k,k)
 enddo

return
end subroutine divdiffs

function interpoly(dep_var,ind_var,eval_loc,n)
!@t
! \textbf{function interpoly(dep\_var,ind\_var,eval\_loc,n)}
!@h
!   Description:
!     Integrating polynomial or interpolating polynomial? KYLE?
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: r8
 implicit none

!Function Variable
 real(r8)                      :: interpoly

!Passed Variables
 real(r8),intent(in)           :: dep_var(0:n)
 real(r8),intent(in)           :: ind_var(0:n)
 real(r8),intent(in)           :: eval_loc
 integer,intent(in)            :: n

!Local Variables
 integer  :: k
 real(r8) :: sum1
 real(r8) :: a(0:n)

 call divdiffs(dep_var,ind_var,a,n)

 sum1=a(n)
 do k=(n-1),0,-1
  sum1=sum1*(eval_loc-dep_var(k))+a(k)
 enddo

 interpoly=sum1
 return
end function

!program test_poly2diff
! implicit none
! integer, parameter :: r8  = selected_real_kind(8)
! integer,parameter  :: n = 4
! real(r8) :: x(0:n), y(0:n), approx, ap2
! integer :: i
!
! real(r8) :: diffpoly, interpoly
! do i=0,n
!  x(i) = .1d0*dble(i)
!  y(i) = dsin(x(i))
! enddo

! approx = diffpoly(x,y,0.25d0,n)
! ap2    = interpoly(x,y,0.25d0,n)
! write(6,*) approx, dcos(0.25d0)
! write(6,*) ap2, dsin(0.25d0)
!stop
!end

!NOTE: TO use the test program the use ntypes statement in function poly2diff
!should be replaced with
!integer, parameter :: r8  = selected_real_kind(8)





subroutine statistics_transient(ok)
!@t
! \textbf{subroutine statistics\_vshear(ok)}
!@h
!   Description:
!     Calculate all statistics for the vertical shear layer.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!     1.0       08/2009  updated. [Hieu T. Pham] 

 use ntypes, only: r8 
 use IO,     only: IOUT, statDIR, statbin, tkstat
 use Flow,   only: u,v,w,p,rho,u1_tmp2,u2_tmp2,u3_tmp2,r_tmp1,r_tmp2
 use transient
 use domain, only: sx,ex,sy,ey,sz,ez,nzp2,nyp2
 use Spng,   only: x3spng
 use grid,   only: dzc, dze, rdzc, rdze, zc, ze, yc
 use parameters, only: time, rRe, nstep, rho_0, g, rPr
 use dd,     only: myid,coords,rankx3,comm3d
 use ratios
 implicit none

!Passed Variables
 integer,intent(out) :: ok
   
!Local Variables
 integer,parameter :: nstats=13
 integer,parameter :: ngroups=3

 real(r8)          :: STATS(1:nzp2,1:nstats)
 character(len=25) :: Sname(1:nstats), Gname(1:ngroups)
 integer           :: group(1:nstats), stat

 integer  :: err1, i, j, k, s1 , sloc, n, plane, szint, ezint, inttmp
 integer  :: rholoc(sz-1:ez+1),rank(sz-1:ez+1)
 real(r8) :: mean(sz-1:ez+1), rms(sz-1:ez+1), temp(sz-1:ez+1),input(sz-1:ez+1)
 real(r8) :: tempF(1:nzp2)
 character(len=150) :: filen1
 real(r8) :: scalatmp,maxerr
 character(len=1024) :: tkheader

 real(r8),allocatable,dimension(:,:,:) :: Ftemp


 Gname(1)="Ddt"
 Gname(2)="intDdt"
 Gname(2)="sintDdt"
!**********************************************************************
!**************GROUP 1 -- TRANSIENT TERMS d/dt*************************
!**********************************************************************
 ok=s1
return
end subroutine statistics_transient

subroutine lambda2(ok)
!@t
! \textbf{subroutine statistics\_vshear(ok)}
!@h
!   Description:
!     Calculate all statistics for the vertical shear layer.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!     1.0       08/2009  updated. [Hieu T. Pham] 

 use ntypes, only: r8 
 use IO,     only: IOUT, flowDIR
 use Flow,   only: u,v,w,u1_tmp2,u2_tmp2,u3_tmp2,rho
 use domain, only: sx,ex,sy,ey,sz,ez, nxp2,nyp2,nzp2
 use grid,   only: rdxe, rdye, rdze 
 use dd,     only: myid,coords,comm3d,MPI_STATUS_SIZE
 use Parameters, only: Re, Pr, g, rho_0, time, nstep, delt
 implicit none

!Passed Variables
 integer,intent(out) :: ok
   
!Local Variables
 integer  :: err, err1, s1, stat, i, j, k, l, is,ie,js,je,ks,ke
 real(r8) :: S11, S22, S33, S12, S13, S23, S21, S31, S32
 real(r8) :: W11, W22, W33, W12, W13, W23, W21, W31, W32
 real(r8) :: A11, A22, A33, A12, A13, A23, A21, A31, A32
 real(r8) :: a, b, c, d, p, q
 real(r8) :: phi, temp1, pi, DET
 real(r8) :: y1, y2 ,y3, x(1:3)
 real(r8),allocatable,dimension(:,:,:) :: lambda2var, uF, tmp1
 character(len=250):: ICfile, basename
 logical,parameter :: debug=.false.

 pi = 4.d0*atan(1.d0)
 is = 1
 ie = nxp2
 js = 1
 je = nyp2
 ks = 49
 ke = nzp2

 write(IOUT,*)"Begin computing lambda"

 allocate( lambda2var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,'(a,i4)') "ERROR: Allocating lamda2 field, stat:",s1
  stat=2
 goto 1000
 endif

 call center_velocity(u,u1_tmp2,1)
 call center_velocity(v,u2_tmp2,2)
 call center_velocity(w,u3_tmp2,3)
 call ghost(u1_tmp2,'u',err1)
 call ghost(u2_tmp2,'v',err1)
 call ghost(u3_tmp2,'w',err1)

 do k=sz,ez
  do j=sy,ey
   do i=sx,ex
    !Symmetric Sij 
    S11 =  ( u1_tmp2(i+1,j,k)-u1_tmp2(i-1,j,k) )*(rdxe(i)+rdxe(i-1)) 
    S22 =  ( u2_tmp2(i,j+1,k)-u2_tmp2(i,j-1,k) )*(rdye(j)+rdye(j-1)) 
    S33 =  ( u3_tmp2(i,j,k+1)-u3_tmp2(i,j,k-1) )*(rdze(k)+rdze(k-1)) 
    S12 =  0.5d0 *( ( u1_tmp2(i,j+1,k)-u1_tmp2(i,j-1,k) )*(rdye(j)+rdye(j-1)) + &
                    ( u2_tmp2(i+1,j,k)-u2_tmp2(i-1,j,k) )*(rdxe(i)+rdxe(i-1)) )
    S13 =  0.5d0 *( ( u1_tmp2(i,j,k+1)-u1_tmp2(i,j,k-1) )*(rdze(k)+rdze(k-1)) + &
                    ( u3_tmp2(i+1,j,k)-u3_tmp2(i-1,j,k) )*(rdxe(i)+rdxe(i-1)) )
    S23 =  0.5d0 *( ( u2_tmp2(i,j,k+1)-u2_tmp2(i,j,k-1) )*(rdze(k)+rdze(k-1)) + &
                    ( u3_tmp2(i,j+1,k)-u3_tmp2(i,j-1,k) )*(rdye(j)+rdye(j-1)) )
    S21 = S12
    S31 = S13
    S32 = S23
    !Antisymmetric Wij
    W11 =  0.d0 
    W22 =  0.d0 
    W33 =  0.d0 
    W12 =  0.5d0 *( ( u1_tmp2(i,j+1,k)-u1_tmp2(i,j-1,k) )*(rdye(j)+rdye(j-1)) - &
                    ( u2_tmp2(i+1,j,k)-u2_tmp2(i-1,j,k) )*(rdxe(i)+rdxe(i-1)) )
    W13 =  0.5d0 *( ( u1_tmp2(i,j,k+1)-u1_tmp2(i,j,k-1) )*(rdze(k)+rdze(k-1)) - &
                    ( u3_tmp2(i+1,j,k)-u3_tmp2(i-1,j,k) )*(rdxe(i)+rdxe(i-1)) )
    W23 =  0.5d0 *( ( u2_tmp2(i,j,k+1)-u2_tmp2(i,j,k-1) )*(rdze(k)+rdze(k-1)) - &
                    ( u3_tmp2(i,j+1,k)-u3_tmp2(i,j-1,k) )*(rdye(j)+rdye(j-1)) )
    W21 = -W12
    W31 = -W13
    W32 = -W23
    !Symmetric Aij = Sij^2 + Wij^2
    A11 = S11*S11 + S12*S21 + S13*S31 + W11*W11 + W12*W21 + W13*W31
    A22 = S21*S12 + S22*S22 + S23*S32 + W21*W12 + W22*W22 + W23*W32
    A33 = S31*S13 + S32*S23 + S33*S33 + W31*W13 + W32*W23 + W33*W33
    A12 = S11*S12 + S12*S22 + S13*S32 + W11*W12 + W12*W22 + W13*W32 
    A13 = S11*S13 + S12*S23 + S13*S33 + W11*W13 + W12*W23 + W13*W33
    A23 = S21*S13 + S22*S23 + S23*S33 + W21*W13 + W22*W23 + W23*W33
    A21 = A12
    A31 = A13
    A32 = A23 
   !Find eigenvalues x of Aij by solving  a*x**3 + b*x**2 + c*x + d = 0
    a = -1.d0
    b = A11+A22+A33
    c = 0.5d0* ( A11*A11 + A12*A21 + A13*A31 + &
                 A21*A12 + A22*A22 + A23*A32 + &
                 A31*A13 + A32*A23 + A33*A33 - b**2.d0 )
    d =  A11*A22*A33 + A12*A23*A31 + A13*A21*A32 &
        -A13*A31*A22 - A11*A23*A32 - A12*A21*A33

!   p  = c/a - b*b/a/a/3.d0
!   q  = (2.d0*b*b*b/a/a/a - 9.d0*b*c/a/a + 27.d0*d/a) / 27.d0
!   DET = p*p*p/27.d0 + q*q/4.d0

!Eric implementation 
     b=-b
     c=-c
     d=-d
     p=(3.d0*c - b*b)/9.d0
     q=(9.d0*c*b -27.d0*d -2.d0*b*b*b)/54.d0
     
     if (p.LT.0.d0) then
      phi = dacos(q/dsqrt(-p*p*p))
      x(1) = 2.d0*dsqrt(-p)*dcos(phi/3.d0)-b/3.d0
      x(2) = 2.d0*dsqrt(-p)*dcos((phi+2.d0*pi)/3.d0)-b/3.d0
      x(3) = 2.d0*dsqrt(-p)*dcos((phi+4.d0*pi)/3.d0)-b/3.d0
     else
      write(IOUT,*)"FAIL !!! Positive p for cubic equation at:", i,j,k
      go to 1000
     endif

!   if (DET.lt.0.d0) then
     !3 real unequal roots -- use the trigonometric formulation
!     phi = acos(-q/2./sqrt(abs(p*p*p)/27.))
!     temp1=2.*sqrt(abs(p)/3.)
!     y1 =  temp1*cos(phi/3.)
!     y2 = -temp1*cos((phi+pi)/3.)
!     y3 = -temp1*cos((phi-pi)/3.)
!   else
!    write(IOUT,*)"Negative Discriminant for cubic equation at:", i,j,k
!    go to 1000
!   endif

!   temp1 = b/a/3.
!   x(1) = y1-temp1
!   x(2) = y2-temp1
!   x(3) = y3-temp1
    
    do l=1,3
     if (x(l).GT.minval(x).and.x(l).LT.maxval(x)) lambda2var(i,j,k) = x(l)
    enddo
   enddo
  enddo
 enddo

!
 lambda2var=rho


 call ghost(lambda2var,'cfluc',err1)

 write(IOUT,*)"Done computing lambda. Begin writing to file"

 !write field
 if (debug) call check_point('write_field_small#mpi_1',.false.)

 if (myid.eq.0) then
  call deallocate_temps(err)
  if (err.NE.0) then
   write(IOUT,'(a)') "ERROR: DEALLOCATION OF TEMPS IN `lambda2` FAILED"
   stat=1
   goto 1000
  endif
  allocate( uF(1:nxp2,1:nyp2,1:nzp2), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR: Allocating full field, stat:",s1
   stat=2
   goto 1000
  endif

  allocate( tmp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR: Allocating temp field, stat:",s1
   stat=2
   goto 1000
  endif

  basename="lambda2"
  ICfile=flowDIR
  call concat(ICfile,basename)
  call concati(ICfile,nstep)
  open(310,file=ICfile,form='unformatted',status='unknown',iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR OPENING FILE: "//trim(ICfile)//" IOSTAT: ",s1
   stat=3
   goto 1000
  endif
  write(310) nstep,time,delt,g,rho_0,Re,Pr
  write(310) is,ie,js,je,ks,ke
 endif
   if (debug) call check_point('write_flow_small#mpi_2',.false.)
 
 !Gather field
  if (myid.eq.0) then
   call Gather3dM(lambda2var,uF,tmp1,0,err)
  else
   call Gather3dS(lambda2var,0,err)
  endif
   call MPI_BARRIER(comm3d,err)
   if (debug) call check_point('write_flow_small#mpi_3',.false.)

 !Write field
  if(myid.eq.0) then
   write(310) uF(is:ie,js:je,ks:ke)
  endif
   call MPI_BARRIER(comm3d,err)
   if (debug) call check_point('write_flow_small#mpi_4',.false.)

  if(myid.eq.0) then
   deallocate( uF, stat=s1 )
   if (s1.NE.0) then
    write(IOUT,'(a,i4)') "ERROR: Deallocating lambda field, stat:",s1
    goto 1000
   endif
   deallocate( tmp1, stat=s1 )
   if (s1.NE.0) then
    write(IOUT,'(a,i4)') "ERROR: Deallocating lambda tmp1, stat:",s1
    goto 1000
   endif
   call allocate_temps(err)
   if (err.NE.0) then
    write(IOUT,'(a)') "ERROR ALLOCATING TEMPS IN `lambda2` FATAL!"
    stat=4
    goto 2000
   endif
   close(310)
   write(IOUT,'(a)') "WRITE OF SMALL RESTART :"//trim(ICfile)//" COMPLETED"
  endif

   call MPI_BARRIER(comm3d,err)

 ok=max(err,s1)
 1000 continue
 
 write(IOUT,*)"Successful lambda"
 return
 2000 continue
 write(IOUT,'(a,i4)') "WRITE OF SMALL RESTART :"//trim(ICfile)//" FAILED STAT: ",s1
 return
end subroutine lambda2

