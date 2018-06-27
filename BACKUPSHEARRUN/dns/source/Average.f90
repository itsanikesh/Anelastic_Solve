












subroutine avgX1X2X3(Var,outm,outp,vtype)
!@t
! \textbf{subroutine avgX1X2X3(Var,outm,outp,vtype)}
!@h
!   Description:
!     Averages over all dimensions.
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
 use domain, only: sx,ex,sy,ey,sz,ez
 use grid,   only: dxc, dyc, dzc, dxe,dye,dze
 use IO,     only: IOUT
 implicit none

!Passed Variables
 character(len=*),intent(in) :: vtype
 real(r8),intent(in),dimension( sx-1:ex+1,sy-1:ey+1,sz-1:ez+1 ) :: Var
 real(r8),intent(out)                                           :: outm, outp

!Local Variables
 real(r8) :: rsum, Volume 
 integer  :: i,j,k
 real(r8) :: temp
 integer  :: size3dpoint
 integer  :: ierr
 real(r8),dimension(:),pointer :: dxt,dyt,dzt 

 select case(vtype)
  case('u')
   dxt=>dxe
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('v')
   dxt=>dxc
   dyt=>dye
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('w')
   dxt=>dxc
   dyt=>dyc
   dzt=>dze
   call ghost(Var,'w',ierr)
  case('uc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('vc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('wc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'w',ierr)
  case('c','p')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'p',ierr)
  case('rf')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'rho',ierr)
  case('cfluc','rp')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'cfluc',ierr)
  case DEFAULT
   write(IOUT,'(a)') "AVERAGING TYPE NOT AVAILABLE: "//trim(vtype)
    outm = 1.d16
    outp = -1.d16
   return
 end select

 Volume = 0.d0
 rsum=0.0d0
 do k=sz-1,ez+1
  do j=sy-1,ey+1
   do i=sx-1,ex+1
    rsum = rsum + Var(i,j,k)*dxt(i)*dyt(j)*dzt(k)
    Volume = Volume + dxt(i)*dyt(j)*dzt(k)
   enddo
  enddo
 enddo            

 outm = rsum/Volume


 rsum=0.0d0
 do k=sz-1,ez+1
  do j=sy-1,ey+1
   do i=sx-1,ex+1
    rsum = rsum + ( (Var(i,j,k) - outm)**2)*dxt(i)*dyt(j)*dzt(k)
   enddo
  enddo
 enddo

 outp  = dsqrt(rsum/Volume)

 return      
end subroutine avgX1X2X3

subroutine avgrmsX1X2(Var,outm,outp,vtype)
!@t
! \textbf{subroutine avgX1X2(Var,outm,outp,vtype)}
!@h
!   Description:
!     Averages over 1st and 2nd dimensions.
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
 use domain, only: sx,ex,sy,ey,sz,ez
 use grid,   only: dxc, dyc,dxe,dye, dzc, dze
 use IO,     only: IOUT
 implicit none

!Passed Variables
 character(len=*),intent(in) :: vtype
 real(r8),intent(in),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)  :: Var
 real(r8),intent(out),dimension( sz-1:ez+1 )                   :: outm, outp

!Local Variables
 real(r8) :: rsum, Area
 integer :: i,j,k
 real(r8) :: temp1d( sz-1:ez+1 )
 integer :: size1dpencil
 integer :: ierr
 real(r8),dimension(:),pointer :: dxt,dyt,dzt 

 select case(vtype)
  case('u')
   dxt=>dxe
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('v')
   dxt=>dxc
   dyt=>dye
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('w')
   dxt=>dxc
   dyt=>dyc
   dzt=>dze
   call ghost(Var,'w',ierr)
  case('uc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('vc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('wc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'w',ierr)

  case('c','p')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'p',ierr)
  case('rf')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'rho',ierr)
  case('sf')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'scal1',ierr)
  case('cfluc','rp')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'cfluc',ierr)
  case DEFAULT
   write(IOUT,'(a)') "AVERAGING TYPE NOT AVAILABLE: "//trim(vtype)
    outm = 1.d16
    outp = -1.d16
   return

 end select

 Area = 0.d0
 do j=sy,ey
  do i=sx,ex
   Area = Area + dxt(i)*dyt(j)
  enddo
 enddo

 do k=sz-1,ez+1
  rsum=0.0d0
  do j=sy,ey
   do i=sx,ex
    rsum = rsum + Var(i,j,k)*dxt(i)*dyt(j)
   enddo
  enddo
   outm(k) = rsum/Area
 enddo


 do k=sz-1,ez+1
  rsum=0.0d0
  do j=sy,ey
   do i=sx,ex
   rsum = rsum + ( (Var(i,j,k) - outm(k))**2)*dxt(i)*dyt(j)
   enddo
  enddo
  outp(k) = dsqrt(rsum/Area)
 enddo            


 return
end subroutine avgrmsx1x2

subroutine avgX1X2(Var,outm,vtype)
!@t
! \textbf{subroutine avgX1X2(Var,outm,outp,vtype)}
!@h
!   Description:
!     Averages over 1st and 2nd dimensions.
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
 use domain, only: sx,ex,sy,ey,sz,ez
 use grid,   only: dxc, dyc,dxe,dye, dzc, dze
 use IO,     only: IOUT
 implicit none

!Passed Variables
 character(len=*),intent(in) :: vtype
 real(r8),intent(in),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)  :: Var
 real(r8),intent(out),dimension( sz-1:ez+1 )                   :: outm 

!Local Variables
 real(r8) :: rsum, Area
 integer :: i,j,k
 real(r8) :: temp1d( sz-1:ez+1 )
 integer :: size1dpencil
 integer :: ierr
 real(r8),dimension(:),pointer :: dxt,dyt,dzt 

 select case(vtype)
  case('u')
   dxt=>dxe
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('v')
   dxt=>dxc
   dyt=>dye
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('w')
   dxt=>dxc
   dyt=>dyc
   dzt=>dze
   call ghost(Var,'w',ierr)
  case('uc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('vc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('wc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'w',ierr)
  case('c','p')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'p',ierr)
  case('rf')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'rho',ierr)
  case('sf')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'scal1',ierr)
  case('cfluc','rp')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'cfluc',ierr)
  case DEFAULT
   write(IOUT,'(a)') "AVERAGING TYPE NOT AVAILABLE: "//trim(vtype)
    outm = 1.d16
   return
 end select

 Area = 0.d0
 do j=sy,ey
  do i=sx,ex
   Area = Area + dxt(i)*dyt(j)
  enddo
 enddo

 do k=sz-1,ez+1
  rsum=0.0d0
  do j=sy,ey
   do i=sx,ex
    rsum = rsum + Var(i,j,k)*dxt(i)*dyt(j)
   enddo
  enddo
   outm(k) = rsum/Area
 enddo


 return
end subroutine avgx1x2

subroutine avgX1X3(Var,outm,outp,vtype)
!@t
! \textbf{subroutine avgX1X3(Var,outm,outp,vtype)}
!@h
!   Description:
!     Averages over 1st and 3rd dimensions.
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
 use domain, only: sx,ex,sy,ey,sz,ez
 use grid,   only: dxc, dyc, dzc, dxe,dye,dze
 use IO,     only: IOUT
 implicit none

!Passed Variables
 character(len=*),intent(in) :: vtype 
 real(r8),intent(in),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)  :: Var
 real(r8),intent(out),dimension( sy-1:ey+1 )                   :: outm, outp

!Local Variables
 real(r8) :: rsum, Area
 integer :: i,j,k
 real(r8) :: temp1d( sy-1:ey+1 )
 integer :: size1dpencil
 integer :: ierr
 real(r8),dimension(:),pointer :: dxt,dyt,dzt 

 select case(vtype)
  case('u')
   dxt=>dxe
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('v')
   dxt=>dxc
   dyt=>dye
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('w')
   dxt=>dxc
   dyt=>dyc
   dzt=>dze
   call ghost(Var,'w',ierr)
  case('uc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('vc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('wc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'w',ierr)
  case('c','p')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'p',ierr)
  case('rf')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'rho',ierr)
  case('cfluc','rp')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'cfluc',ierr)
  case DEFAULT
   write(IOUT,'(a)') "AVERAGING TYPE NOT AVAILABLE: "//trim(vtype)
    outm = 1.d16
    outp = -1.d16
   return
                                                                                                                             
 end select

 Area = 0.d0
 do k=sz-1,ez+1
  do i=sx-1,ex+1
  Area = Area + dxt(i)*dzt(k)
  enddo
 enddo

 do j=sy-1,ey+1
  rsum=0.0d0
  do k=sz-1,ez+1
   do i=sx-1,ex+1
    rsum = rsum + Var(i,j,k)*dxt(i)*dzt(k)
   enddo
  enddo
   outm(j) = rsum/Area
 enddo


 do j=sy-1,ey+1
  rsum=0.0d0
  do k=sz-1,ez+1
   do i=sx-1,ex+1
    rsum = rsum + ( (Var(i,j,k) - outm(j))**2)*dxt(i)*dzt(k)
   enddo
  enddo
  outp(j) = dsqrt(rsum/Area)
 enddo            


 return
end subroutine avgx1x3

subroutine avgX2X3(Var,outm,outp,vtype)
!@t
! \textbf{subroutine avgX2X3(Var,outm,outp,vtype)}
!@h
!   Description:
!     Averages over 2nd and 3rd dimensions.
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
 use domain, only: sx,ex,sy,ey,sz,ez
 use grid,   only: dxc, dyc, dzc, dxe,dye,dze
 use IO,     only: IOUT
 implicit none

!Passed Variables
 character(len=*),intent(in) :: vtype
 real(r8),intent(in),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)  :: Var
 real(r8),intent(out),dimension( sx-1:ex+1 )                   :: outm, outp

!Local Variables
 real(r8) :: rsum, Area
 integer :: i,j,k
 real(r8) :: temp1d( sx-1:ex+1 )
 integer :: size1dpencil
 integer :: ierr
 real(r8),dimension(:),pointer :: dxt,dyt,dzt 

 select case(vtype)
  case('u')
   dxt=>dxe
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('v')
   dxt=>dxc
   dyt=>dye
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('w')
   dxt=>dxc
   dyt=>dyc
   dzt=>dze
   call ghost(Var,'w',ierr)
  case('uc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('vc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('wc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'w',ierr)

  case('c','p')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'p',ierr)
  case('rf')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'rho',ierr)
  case('cfluc','rp')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'cfluc',ierr)
  case DEFAULT
   write(IOUT,'(a)') "AVERAGING TYPE NOT AVAILABLE: "//trim(vtype)
    outm = 1.d16
    outp = -1.d16
   return
                                                                                                                             
 end select

 Area = 0.d0
 do k=sz-1,ez+1
  do j=sy-1,ey+1
   Area = Area + dyt(j)*dzt(k)
  enddo
 enddo

 do i=sx-1,ex+1
  rsum=0.0d0
  do k=sz-1,ez+1
   do j=sy-1,ey+1
    rsum = rsum + Var(i,j,k)*dyt(j)*dzt(k)
   enddo
  enddo
   outm(i) = rsum/Area
 enddo            


 do i=sx-1,ex+1
  rsum=0.0d0
  do k=sz-1,ez+1
   do j=sy-1,ey+1
    rsum = rsum + ( (Var(i,j,k) - outm(k))**2.d0)*dyt(j)*dzt(k)
   enddo
  enddo
  outp(i) = dsqrt(rsum/Area)
 enddo            


 return
end subroutine avgx2x3

subroutine avgX1(Var,outm,outp,vtype)
!@t
! \textbf{subroutine avgX1(Var,outm,outp,vtype)}
!@h
!   Description:
!     Averages var over 1st dimension
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
!     Running from sy-1:ey+1 automatically ghosts the output
!     Running from sz-1:ez+1 automatically ghosts the output.
!@q
 use ntypes, only: r8
 use domain, only: sx,ex,sy,ey,sz,ez
 use grid,   only: dxc, dyc, dzc, dxe,dye,dze
 use IO,     only: IOUT
 implicit none

!Passed Variables
 character(len=*),intent(in) :: vtype
 real(r8),intent(in),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)  :: Var
 real(r8),intent(out),dimension( sy-1:ey+1,sz-1:ez+1 )         :: outm, outp

!Local Variables
 real(r8) :: rsum, Length
 integer :: i,j,k, ierr 

 real(r8) :: temp2d( sy-1:ey+1,sz-1:ez+1 )
 integer :: size2dplane
 real(r8),dimension(:),pointer :: dxt,dyt,dzt 


 select case(vtype)
  case('u')
   dxt=>dxe
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('v')
   dxt=>dxc
   dyt=>dye
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('w')
   dxt=>dxc
   dyt=>dyc
   dzt=>dze
   call ghost(Var,'w',ierr)
  case('uc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('vc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('wc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'w',ierr)
  case('c','p')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'p',ierr)
  case('rf')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'rho',ierr)
  case('cfluc','rp')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'cfluc',ierr)
  case DEFAULT
   write(IOUT,'(a)') "AVERAGING TYPE NOT AVAILABLE: "//trim(vtype)
    outm = 1.d16
    outp = -1.d16
   return
                                                                                                                             
 end select


 Length = 0.d0
 do i=sx,ex
  Length = Length + dxt(i)
 enddo

 do k=sz-1,ez+1
  do j=sy-1,ey+1
   rsum=0.0d0
   do i=sx,ex
    rsum = rsum + Var(i,j,k)*dxt(i)
   enddo
   outm(j,k) = rsum/Length
  enddo
 enddo            


 do k=sz-1,ez+1
  do j=sy-1,ey+1
   rsum=0.0d0
   do i=sx,ex
    rsum = rsum + ( (Var(i,j,k) - outm(j,k))**2)*dxt(i) 
   enddo
   outp(j,k) = dsqrt(rsum/Length)
  enddo
 enddo

 return
end subroutine avgX1

subroutine avgX2(Var,outm,outp,vtype)
!@t
! \textbf{subroutine avgX2(Var,outm,outp,vtype)}
!@h
!   Description:
!     Averages var over the 2nd dimension.
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
!     Running from sx-1:ex+1 automatically ghosts the output
!     Running from sz-1:ez+1 automatically ghosts the output.
!@q
 use ntypes, only: r8
 use domain, only: sx,ex,sy,ey,sz,ez
 use grid,   only: dxc, dyc, dzc, dxe,dye,dze
 use IO,     only: IOUT
 implicit none

!Passed Variables
 character(len=*),intent(in) :: vtype
 real(r8),intent(in),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)  :: Var
 real(r8),intent(out),dimension( sx-1:ex+1,sz-1:ez+1 )         :: outm, outp

!Local Variables
 real(r8) :: rsum, Length
 integer :: i,j,k, ierr 

 real(r8) :: temp2d( sx-1:ex+1,sz-1:ez+1 )
 integer :: size2dplane
 real(r8),dimension(:),pointer :: dxt,dyt,dzt 

 select case(vtype)
  case('u')
   dxt=>dxe
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('v')
   dxt=>dxc
   dyt=>dye
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('w')
   dxt=>dxc
   dyt=>dyc
   dzt=>dze
   call ghost(Var,'w',ierr)
  case('uc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('vc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('wc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'w',ierr)

  case('c','p')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'p',ierr)
  case('rf')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'rho',ierr)
  case('sf')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'scal1',ierr)
  case('cfluc','rp')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'cfluc',ierr)
  case DEFAULT
   write(IOUT,'(a)') "AVERAGING TYPE NOT AVAILABLE: "//trim(vtype)
    outm = 1.d16
    outp = -1.d16
   return
                                                                                                                             
 end select


 Length = 0.d0
 do j=sy,ey
  Length = Length + dyt(j)
 enddo

 do k=sz-1,ez+1
  do i=sx-1,ex+1
   rsum=0.0d0
   do j=sy,ey
    rsum = rsum + Var(i,j,k)*dyt(j)
   enddo
   outm(i,k) = rsum/Length
  enddo
 enddo            


 do k=sz-1,ez+1
  do i=sx-1,ex+1
   rsum=0.0d0
   do j=sy,ey
    rsum = rsum + ( (Var(i,j,k) - outm(i,k))**2)*dyt(j) 
   enddo
   outp(i,k) = dsqrt(rsum/Length)
  enddo
 enddo


 return
end subroutine avgX2

subroutine avgX3(Var,outm,outp,vtype)
!@t
! \textbf{subroutine avgX3(Var,outm,outp,vtype)}
!@h
!   Description:
!     Averages var over the 3rd dimension.
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
!     Running from sx-1:ex+1 automatically ghosts the output
!     Running from sy-1:ey+1 automatically ghosts the output.
!@q
 use ntypes, only: r8
 use domain, only: sx,ex,sy,ey,sz,ez
 use grid,   only: dxc, dyc, dzc, dxe,dye,dze
 use IO,     only: IOUT
 implicit none

!Passed Variables
 character(len=*),intent(in) :: vtype
 real(r8),intent(in),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)  :: Var
 real(r8),intent(out),dimension( sx-1:ex+1,sy-1:ey+1 )         :: outm, outp

!Local Variables
 real(r8) :: rsum, Length
 integer :: i,j,k, ierr 

 real(r8) :: temp2d( sx-1:ex+1,sy-1:ey+1 )
 integer :: size2dplane
 real(r8),dimension(:),pointer :: dxt,dyt,dzt 

 select case(vtype)
  case('u')
   dxt=>dxe
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('v')
   dxt=>dxc
   dyt=>dye
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('w')
   dxt=>dxc
   dyt=>dyc
   dzt=>dze
   call ghost(Var,'w',ierr)
  case('uc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'u',ierr)
  case('vc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'v',ierr)
  case('wc')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'w',ierr)
  case('c','p')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'p',ierr)
  case('rf')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'rho',ierr)
  case('cfluc','rp')
   dxt=>dxc
   dyt=>dyc
   dzt=>dzc
   call ghost(Var,'cfluc',ierr)
  case DEFAULT
   write(IOUT,'(a)') "AVERAGING TYPE NOT AVAILABLE: "//trim(vtype)
    outm = 1.d16
    outp = -1.d16
   return
                                                                                                                             
 end select

 Length = 0.d0
 do k=sz,ez
  Length = Length + dzc(k)
 enddo

 do j=sy-1,ey+1
  do i=sx-1,ex+1
   rsum=0.0d0
   do k=sz,ez
    rsum = rsum + Var(i,j,k)*dzc(k)
   enddo
   outm(i,j) = rsum/Length
  enddo
 enddo            


 do j=sy-1,ey+1
  do i=sx-1,ex+1
   rsum=0.0d0
   do k=sz,ez
    rsum = rsum + ( (Var(i,j,k) - outm(i,j))**2)*dzc(k) 
   enddo
   outp(i,j) = dsqrt(rsum/Length)
  enddo
 enddo

 return
end subroutine avgX3
