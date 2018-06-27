subroutine filter(var,filvar,fil_type,x_dir,y_dir,z_dir,vtype,stat)
 use ntypes, only : r8
 use IO,     only: IOUT 
 use Domain, only : sx,ex,sy,ey,sz,ez

 implicit none

 real(r8),intent(in),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) :: var
 real(r8),intent(out),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) :: filvar
 character(len=*)   :: vtype
 integer,intent(in) :: fil_type, x_dir, y_dir, z_dir
 integer,intent(out) :: stat

!Local variables
 real(r8) :: wa(1:5),wab(1:5)
 real(r8) :: Xpen(sx-1:ex+1),Ypen(sy-1:ey+1),Zpen(sz-1:ez+1)
 integer  :: ierr, i,j,k
 logical,parameter                :: debug=.false.


if (debug) call check_point('filter#0',.false.)
 
 if (fil_type == 1) then
  !second order: test filter from Zhang et al 1993
  !interior
  wa(1) = 0.d0
  wa(2) = 1.d0/8.d0
  wa(3) = 6.d0/8.d0
  wa(4) = 1.d0/8.d0
  wa(5) = 0.d0
  !bound
  wab(1) = 1.d0/8.d0
  wab(2) = 6.d0/8.d0
  wab(3) = 1.d0/8.d0
  wab(4) = 0.d0
  wab(5) = 0.d0
 else if (fil_type == 2) then
  !second order: trapezoidal / top hat grid filter from Zhang et al 1993
  !interior
  wa(1) = 0.d0
  wa(2) = 1.d0/4.d0
  wa(3) = 1.d0/2.d0
  wa(4) = 1.d0/4.d0
  wa(5) = 0.d0
  !bound
  wab(1) = 1.d0/4.d0
  wab(2) = 1.d0/2.d0
  wab(3) = 1.d0/4.d0
  wab(4) = 0.d0
  wab(5) = 0.d0
 else if (fil_type == 3) then
  ! fourth order explicit filter: curve b in Lele JCP 1992 
  !interior
  wa(1) = -1.d0/16.d0
  wa(2) =  4.d0/16.d0
  wa(3) = 10.d0/16.d0
  wa(4) =  4.d0/16.d0
  wa(5) = -1.d0/16.d0
  !bound
  wab(1) =  1.d0/16.d0
  wab(2) = 12.d0/16.d0
  wab(3) =  6.d0/16.d0
  wab(4) = -4.d0/16.d0
  wab(5) =  1.d0/16.d0
 else if (fil_type == 4) then
  !Second order: Simpson rule 
  !interior
  wa(1) =  0.d0
  wa(2) =  1.d0/6.d0
  wa(3) =  4.d0/6.d0
  wa(4) =  1.d0/6.d0
  wa(5) =  0.d0
  !bound
  wab(1) =  1.d0/6.d0
  wab(2) =  4.d0/6.d0
  wab(3) =  1.d0/6.d0
  wab(4) =  0.d0
  wab(5) =  0.d0
 else
   write(IOUT,'(a)') "ABORTING FILTERING: FILTYPE", fil_type," IS NOT IMPLEMENTED"
   stat = 1;
   stop
 endif

 filvar = var  

if (debug) call check_point('filter#1',.false.)
 
!X-filter
 if (x_dir == 1) then
  do k=sz,ez
   do j=sy,ey
    do i=sx-1,ex+1
     Xpen(i) = filvar(i,j,k)
    enddo
    do i=sx+1,ex-1
     filvar(i,j,k) = wa(1)*Xpen(i-2) + wa(2)*Xpen(i-1) + wa(3)*Xpen(i) + wa(4)*Xpen(i+1) + wa(5)*Xpen(i+2)
    enddo
     filvar(sx,j,k) = wab(1)*Xpen(sx-1) + wab(2)*Xpen(sx) + wab(3)*Xpen(sx+1) + wab(4)*Xpen(sx+2) + wab(5)*Xpen(sx+3)
     filvar(ex,j,k) = wab(1)*Xpen(ex+1) + wab(2)*Xpen(ex) + wab(3)*Xpen(ex-1) + wab(4)*Xpen(ex-2) + wab(5)*Xpen(ex-3)
   enddo
  enddo
 endif
 call ghost(filvar,vtype,ierr)


 if (debug) call check_point('filter#2',.false.)
 
!Y-filter
 if (y_dir == 1) then
  do k=sz,ez
   do i=sx,ex
    do j=sy-1,ey+1
     Ypen(j) = filvar(i,j,k)
    enddo
    do j=sy+1,ey-1 
     filvar(i,j,k) = wa(1)*Ypen(j-2) + wa(2)*Ypen(j-1) + wa(3)*Ypen(j) + wa(4)*Ypen(j+1) + wa(5)*Ypen(j+2) 
    enddo
     filvar(i,sy,k) = wab(1)*Ypen(sy-1) + wab(2)*Ypen(sy) + wab(3)*Ypen(sy+1) + wab(4)*Ypen(sy+2) + wab(5)*Ypen(sy+3) 
     filvar(i,ey,k) = wab(1)*Ypen(ey+1) + wab(2)*Ypen(ey) + wab(3)*Ypen(ey-1) + wab(4)*Ypen(ey-2) + wab(5)*Ypen(ey-3) 
   enddo
  enddo
 endif
 call ghost(filvar,vtype,ierr)

if (debug) call check_point('filter#3',.false.)

!Z-filter
 if (z_dir == 1) then
  do j=sy,ey
   do i=sx,ex
    do k=sz-1,ez+1
     Zpen(k) = filvar(i,j,k)
    enddo
    do k=sz+1,ez-1
     filvar(i,j,k) = wa(1)*Zpen(k-2) + wa(2)*Zpen(k-1) + wa(3)*Zpen(k) + wa(4)*Zpen(k+1) + wa(5)*Zpen(k+2)
    enddo
    filvar(i,j,sz) = wab(1)*Zpen(sz-1) + wab(2)*Zpen(sz) + wab(3)*Zpen(sz+1) + wab(4)*Zpen(sz+2) + wab(5)*Zpen(sz+3)
    filvar(i,j,ez) = wab(1)*Zpen(ez+1) + wab(2)*Zpen(ez) + wab(3)*Zpen(ez-1) + wab(4)*Zpen(ez-2) + wab(5)*Zpen(ez-3)
   enddo
  enddo
 endif
 call ghost(filvar,vtype,ierr)

if (debug) call check_point('filter#4',.false.)

 stat=ierr
 return 
end subroutine filter


subroutine filterHF(var,filvar,fil_type,x_dir,y_dir,z_dir,vtype,EDIFF,tol,stat)
 use ntypes, only : r8
 use IO,     only: IOUT 
 use Domain, only : sx,ex,sy,ey,sz,ez

 implicit none

 real(r8),intent(in),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) :: var,EDIFF
 real(r8),intent(out),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) :: filvar
 real(r8),intent(in) :: tol
 character(len=*)   :: vtype
 integer,intent(in) :: fil_type, x_dir, y_dir, z_dir
 integer,intent(out) :: stat

!Local variables
 real(r8) :: wa(1:3),wab(1:3)
 real(r8) :: Xpen(sx-1:ex+1),Ypen(sy-1:ey+1),Zpen(sz-1:ez+1)
 real(r8) :: Xtemp,Ytemp,Ztemp,ddx,ddy,ddz
 integer  :: ierr, i,j,k
 logical,parameter                :: debug=.false.


if (debug) call check_point('filter#0',.false.)
 
 if (fil_type == 1) then
  !second order: test filter from Zhang et al 1993
  wa(1) = 1.d0/8.d0
  wa(2) = 6.d0/8.d0
  wa(3) = 1.d0/8.d0
 else if (fil_type == 2) then
  !second order: trapezoidal / top hat grid filter from Zhang et al 1993
  wa(1) = 1.d0/4.d0
  wa(2) = 1.d0/2.d0
  wa(3) = 1.d0/4.d0
 else if (fil_type == 4) then
  !Second order: Simpson rule 
  wa(1) =  1.d0/6.d0
  wa(2) =  4.d0/6.d0
  wa(3) =  1.d0/6.d0
 else
   write(IOUT,'(a)') "ABORTING FILTERING: FILTYPE", fil_type," IS NOT IMPLEMENTED"
   stat = 1;
   stop
 endif

 filvar = var  

if (debug) call check_point('filter#1',.false.)
 
!X-filter
 if (x_dir == 1) then
  do k=sz,ez
   do j=sy,ey
    do i=sx-1,ex+1
     Xpen(i) = filvar(i,j,k)
    enddo
    do i=sx,ex
!     if (EDIFF(i,j,k).GT.tol) then
      if (Xpen(i).LT.Xpen(i+1).and.Xpen(i).LT.Xpen(i-1)) then
       Xtemp = wa(1)*Xpen(i-1) + wa(2)*Xpen(i) + wa(3)*Xpen(i+1) 
       if (Xtemp.LT.Xpen(i+1).and.Xtemp.LT.Xpen(i-1)) then
        filvar(i,j,k) = Xtemp
       endif
      elseif (Xpen(i).GT.Xpen(i+1).and.Xpen(i).GT.Xpen(i-1)) then
       Xtemp = wa(1)*Xpen(i-1) + wa(2)*Xpen(i) + wa(3)*Xpen(i+1) 
       if (Xtemp.GT.Xpen(i+1).and.Xtemp.GT.Xpen(i-1)) then
        filvar(i,j,k) = Xtemp
       endif
      endif
!     endif
    enddo
   enddo
  enddo
 endif
 call ghost(filvar,vtype,ierr)

 if (debug) call check_point('filter#2',.false.)
!Y-filter
 if (y_dir == 1) then
  do k=sz,ez
   do i=sx,ex
    do j=sy-1,ey+1
     Ypen(j) = filvar(i,j,k)
    enddo
    do j=sy,ey 
!     if (EDIFF(i,j,k).GT.tol) then
      if (Ypen(j).LT.Ypen(j+1).and.Ypen(j).LT.Ypen(j-1)) then
       Ytemp = wa(1)*Ypen(j-1) + wa(2)*Ypen(j) + wa(3)*Ypen(j+1) 
       if (Ytemp.LT.Ypen(j+1).and.Ytemp.LT.Ypen(j-1)) then
        filvar(i,j,k) = Ytemp
       endif
      elseif (Ypen(j).GT.Ypen(j+1).and.Ypen(j).GT.Ypen(j-1)) then
       Ytemp = wa(1)*Ypen(j-1) + wa(2)*Ypen(j) + wa(3)*Ypen(j+1) 
       if (Ytemp.GT.Ypen(j+1).and.Ytemp.GT.Ypen(j-1)) then
        filvar(i,j,k) = Ytemp
       endif
      endif
!     endif
    enddo
   enddo
  enddo
 endif
 call ghost(filvar,vtype,ierr)

if (debug) call check_point('filter#3',.false.)

!Z-filter
 if (z_dir == 1) then
  do j=sy,ey
   do i=sx,ex
    do k=sz-1,ez+1
     Zpen(k) = filvar(i,j,k)
    enddo
    do k=sz,ez
!     if (EDIFF(i,j,k).GT.tol) then
      if (Zpen(k).LT.Zpen(k+1).and.Zpen(k).LT.Zpen(k-1)) then
       Ztemp = wa(1)*Zpen(k-1) + wa(2)*Zpen(k) + wa(3)*Zpen(k+1) 
       if (Ztemp.LT.Zpen(k+1).and.Ztemp.LT.Zpen(k-1)) then
        filvar(i,j,k) = Ztemp
       endif
      elseif (Zpen(k).GT.Zpen(k+1).and.Zpen(k).GT.Zpen(k-1)) then
       Ztemp = wa(1)*Zpen(k-1) + wa(2)*Zpen(k) + wa(3)*Zpen(k+1) 
       if (Ztemp.GT.Zpen(k+1).and.Ztemp.GT.Zpen(k-1)) then
        filvar(i,j,k) = Ztemp
       endif
      endif
!     endif
    enddo
   enddo
  enddo
 endif
 call ghost(filvar,vtype,ierr)
if (debug) call check_point('filter#4',.false.)

 stat=ierr
 return 
end subroutine filterHF


subroutine filterSURF(var,filvar,fil_type,x_dir,y_dir,z_dir,vtype,stat)
 use ntypes, only : r8
 use IO,     only: IOUT 
 use Domain, only : sx,ex,sy,ey,sz,ez,nzp2

 implicit none

 real(r8),intent(in),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) :: var
 real(r8),intent(out),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) :: filvar
 character(len=*)   :: vtype
 integer,intent(in) :: fil_type, x_dir, y_dir, z_dir
 integer,intent(out) :: stat

!Local variables
 real(r8) :: wa(1:5),wab(1:5)
 real(r8) :: Xpen(sx-1:ex+1),Ypen(sy-1:ey+1),Zpen(sz-1:ez+1)
 integer  :: ierr, i,j,k
 logical,parameter                :: debug=.false.


if (debug) call check_point('filter#0',.false.)
 
 if (fil_type == 1) then
  !second order: test filter from Zhang et al 1993
  !interior
  wa(1) = 0.d0
  wa(2) = 1.d0/8.d0
  wa(3) = 6.d0/8.d0
  wa(4) = 1.d0/8.d0
  wa(5) = 0.d0
  !bound
  wab(1) = 1.d0/8.d0
  wab(2) = 6.d0/8.d0
  wab(3) = 1.d0/8.d0
  wab(4) = 0.d0
  wab(5) = 0.d0
 else if (fil_type == 2) then
  !second order: trapezoidal / top hat grid filter from Zhang et al 1993
  !interior
  wa(1) = 0.d0
  wa(2) = 1.d0/4.d0
  wa(3) = 1.d0/2.d0
  wa(4) = 1.d0/4.d0
  wa(5) = 0.d0
  !bound
  wab(1) = 1.d0/4.d0
  wab(2) = 1.d0/2.d0
  wab(3) = 1.d0/4.d0
  wab(4) = 0.d0
  wab(5) = 0.d0
 else if (fil_type == 3) then
  ! fourth order explicit filter: curve b in Lele JCP 1992 
  !interior
  wa(1) = -1.d0/16.d0
  wa(2) =  4.d0/16.d0
  wa(3) = 10.d0/16.d0
  wa(4) =  4.d0/16.d0
  wa(5) = -1.d0/16.d0
  !bound
  wab(1) =  1.d0/16.d0
  wab(2) = 12.d0/16.d0
  wab(3) =  6.d0/16.d0
  wab(4) = -4.d0/16.d0
  wab(5) =  1.d0/16.d0
 else if (fil_type == 4) then
  !Second order: Simpson rule 
  !interior
  wa(1) =  0.d0
  wa(2) =  1.d0/6.d0
  wa(3) =  4.d0/6.d0
  wa(4) =  1.d0/6.d0
  wa(5) =  0.d0
  !bound
  wab(1) =  1.d0/6.d0
  wab(2) =  4.d0/6.d0
  wab(3) =  1.d0/6.d0
  wab(4) =  0.d0
  wab(5) =  0.d0
 else
   write(IOUT,'(a)') "ABORTING FILTERING: FILTYPE", fil_type," IS NOT IMPLEMENTED"
   stat = 1;
   stop
 endif

 filvar = var  

if (debug) call check_point('filter#1',.false.)
 
!X-filter
 if (x_dir == 1) then
  do k=sz,ez
   if (k.GE.nzp2-5) then
   do j=sy,ey
    do i=sx-1,ex+1
     Xpen(i) = filvar(i,j,k)
    enddo
    do i=sx+1,ex-1
     filvar(i,j,k) = wa(1)*Xpen(i-2) + wa(2)*Xpen(i-1) + wa(3)*Xpen(i) + wa(4)*Xpen(i+1) + wa(5)*Xpen(i+2)
    enddo
     filvar(sx,j,k) = wab(1)*Xpen(sx-1) + wab(2)*Xpen(sx) + wab(3)*Xpen(sx+1) + wab(4)*Xpen(sx+2) + wab(5)*Xpen(sx+3)
     filvar(ex,j,k) = wab(1)*Xpen(ex+1) + wab(2)*Xpen(ex) + wab(3)*Xpen(ex-1) + wab(4)*Xpen(ex-2) + wab(5)*Xpen(ex-3)
   enddo
    endif
  enddo
 endif
 call ghost(filvar,vtype,ierr)


 if (debug) call check_point('filter#2',.false.)
 
!Y-filter
 if (y_dir == 1) then
  do k=sz,ez
   if (k.GE.nzp2-5) then
   do i=sx,ex
    do j=sy-1,ey+1
     Ypen(j) = filvar(i,j,k)
    enddo
    do j=sy+1,ey-1 
     filvar(i,j,k) = wa(1)*Ypen(j-2) + wa(2)*Ypen(j-1) + wa(3)*Ypen(j) + wa(4)*Ypen(j+1) + wa(5)*Ypen(j+2) 
    enddo
     filvar(i,sy,k) = wab(1)*Ypen(sy-1) + wab(2)*Ypen(sy) + wab(3)*Ypen(sy+1) + wab(4)*Ypen(sy+2) + wab(5)*Ypen(sy+3) 
     filvar(i,ey,k) = wab(1)*Ypen(ey+1) + wab(2)*Ypen(ey) + wab(3)*Ypen(ey-1) + wab(4)*Ypen(ey-2) + wab(5)*Ypen(ey-3) 
   enddo
   endif 
  enddo
 endif
 call ghost(filvar,vtype,ierr)

if (debug) call check_point('filter#3',.false.)

!Z-filter
 if (z_dir == 1) then
  do j=sy,ey
   do i=sx,ex
    do k=sz-1,ez+1
     Zpen(k) = filvar(i,j,k)
    enddo
    do k=sz+1,ez-1
     if (k.GE.nzp2-5) then
     filvar(i,j,k) = wa(1)*Zpen(k-2) + wa(2)*Zpen(k-1) + wa(3)*Zpen(k) + wa(4)*Zpen(k+1) + wa(5)*Zpen(k+2)
     endif
    enddo
    if (ez.GE.nzp2-5) then 
     filvar(i,j,ez) = wab(1)*Zpen(ez+1) + wab(2)*Zpen(ez) + wab(3)*Zpen(ez-1) + wab(4)*Zpen(ez-2) + wab(5)*Zpen(ez-3)
    endif
   enddo
  enddo
 endif
 call ghost(filvar,vtype,ierr)

if (debug) call check_point('filter#4',.false.)

 stat=ierr
 return 
end subroutine filterSURF

