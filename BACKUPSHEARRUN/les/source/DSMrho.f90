












subroutine DSMrho
 use Flow
 use LESmod
 use ntypes
 use Grid
 use Domain,   only: sx,ex,sy,ey,sz,ez
 use dd
 use IO
 implicit none

!Local variables
 real(r8),dimension(sz-1:ez+1) :: tmpZd1, tmpZd2, tmpZd3
 real(r8),dimension(sx-1:ex+1,sz-1:ez+1) :: tmpXZd1, tmpXZd2, tmpXZd3
 real(r8):: tmp1, tmp2, tmp3, del_g, kappa_sgs
 integer :: ierr,err1, ij,i,j,k
 integer, parameter      :: C_avg_dir=12 
 logical,parameter             :: debug=.false.

 if (debug) call check_point('DSMrho#0',.false.)

 call straincal(u,v,w,err1)
 
!*****************************************
!Scalar/Density equation
!Begin computing the numerator LRi*MRi 
!Zeros temporary variables.
!*****************************************
 lestmp1 = 0.d0
 lestmp2 = 0.d0
 lestmp3 = 0.d0
 lestmp4 = 0.d0
 CTsgs = 0.d0
 kappaT = 0.d0

 do ij=1,3
  if (ij == 1) then
   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      tmp1 = 0.5d0 * (rho(i,j,k)+rho(i-1,j,k))
      tmp2 = 0.5d0 * (rho(i,j,k)+rho(i+1,j,k))
      lestmp1(i,j,k) = delg(i,j,k)**2.d0*modS(i,j,k) * (tmp2-tmp1)*rdxc(i)
     enddo
    enddo
   enddo
  else if (ij == 2) then
   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      tmp1 = 0.5d0 * (rho(i,j,k)+rho(i,j-1,k))
      tmp2 = 0.5d0 * (rho(i,j,k)+rho(i,j+1,k))
      lestmp1(i,j,k) = delg(i,j,k)**2.d0*modS(i,j,k) * (tmp2-tmp1)*rdyc(j)
     enddo
    enddo
   enddo
  elseif (ij == 3) then
   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      tmp1 = 0.5d0 * (rho(i,j,k)+rho(i,j,k-1))
      tmp2 = 0.5d0 * (rho(i,j,k)+rho(i,j,k+1))
      lestmp1(i,j,k) = delg(i,j,k)**2.d0*modS(i,j,k) * (tmp2-tmp1)*rdzc(k)
     enddo
    enddo
   enddo
  endif
  call ghost(lestmp1,'cfluc',err1)
  call filter(lestmp1,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
!lestmp2 now stores the filter(product) portion of MRi


!add on; working
  if (ij == 1) then
   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      tmp1 = 0.5d0 * (rho(i,j,k)+rho(i-1,j,k))
      tmp2 = 0.5d0 * (rho(i,j,k)+rho(i+1,j,k))
      lestmp1(i,j,k) = (tmp2-tmp1)*rdxc(i)
     enddo
    enddo
   enddo
  else if (ij == 2) then
   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      tmp1 = 0.5d0 * (rho(i,j,k)+rho(i,j-1,k))
      tmp2 = 0.5d0 * (rho(i,j,k)+rho(i,j+1,k))
      lestmp1(i,j,k) = (tmp2-tmp1)*rdyc(j)
     enddo
    enddo
   enddo
  else if (ij == 3) then
   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      tmp1 = 0.5d0 * (rho(i,j,k)+rho(i,j,k-1))
      tmp2 = 0.5d0 * (rho(i,j,k)+rho(i,j,k+1))
      lestmp1(i,j,k) = (tmp2-tmp1)*rdzc(k)
     enddo
    enddo
   enddo
  endif
  call ghost(lestmp1,'cfluc',err1)
  call filter(lestmp1,lestmp3,2,xfil,yfil,zfil,'cfluc',err1)
  !filter(drho/dxi) in lestmp3

  !call filter(modS,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
  lestmp1=fmodS

   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      lestmp2(i,j,k) = lestmp2(i,j,k) - (r_dgt_dg*delg(i,j,k))**2.d0 * lestmp1(i,j,k) * lestmp3(i,j,k)
     enddo
    enddo
   enddo
  call ghost(lestmp2,'cfluc',err1)
!lestmp2 now stores full MRi
 if (debug) call check_point('DSMrho#1',.false.)
 
!Now begin computing LRi
  if (ij == 1) then
   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      lestmp1(i,j,k) = rho(i,j,k) * 0.5d0 * ( u(i-1,j,k)+u(i,j,k) )
     enddo
    enddo
   enddo
  else if (ij == 2) then
   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      lestmp1(i,j,k) = rho(i,j,k) * 0.5d0 * ( v(i,j-1,k)+v(i,j,k) )
     enddo
    enddo
   enddo
  else if (ij == 3) then
   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      lestmp1(i,j,k) = rho(i,j,k) * 0.5d0 * ( w(i,j,k-1)+w(i,j,k) )
     enddo
    enddo
   enddo
  endif
  call ghost(lestmp1,'cfluc',err1)
  call filter(lestmp1,lestmp3,2,xfil,yfil,zfil,'cfluc',err1)
!lestmp3 now stores filter(product) of LRi
   
  if (ij == 1) then
   call center_velocity(u,lestmp,1)
   call filter(lestmp,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   call filter(rho,lestmp,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp3=lestmp3-lestmp*lestmp1
  else if (ij == 2) then
   call center_velocity(v,lestmp,2)
   call filter(lestmp,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   call filter(rho,lestmp,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp3=lestmp3-lestmp*lestmp1
  else if (ij ==3) then
   call center_velocity(w,lestmp,3)
   call filter(lestmp,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   call filter(rho,lestmp,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp3=lestmp3-lestmp*lestmp1
  endif
  call ghost(lestmp3,'cfluc',err1)
!lestmp3 now stores full LRi

!compute denominator MRi*MRi,stored in lestmp4 
  lestmp4=lestmp4 + lestmp2*lestmp2

!Compute numerator LRi*MRi, stored in CTsgs
  CTsgs=CTsgs+lestmp2*lestmp3
 enddo !ij loops
 call ghost(CTsgs,'cfluc',err1)
 call ghost(lestmp4,'cfluc',err1)

 if (debug) call check_point('DSMrho#2',.false.)

!Average in homogenous direction: x,y for VSL
  if (C_avg_dir.EQ.12) then
   call avgX1X2(CTsgs,tmpZd1,'cfluc')
   call avgX1X2(lestmp4,tmpZd2,'cfluc')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      CTsgs(i,j,k) = tmpZd1(k)
      lestmp4(i,j,k) = tmpZd2(k)
      if (CTsgs(i,j,k).LT.0.d0) CTsgs(i,j,k) = 0.d0
     enddo
    enddo
   enddo
  else if (C_avg_dir.EQ.2) then
   call avgX2(CTsgs,tmpXZd1,tmpXZd3,'cfluc')
   call avgX2(lestmp4,tmpXZd2,tmpXZd3,'cfluc')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      CTsgs(i,j,k) = tmpXZd1(i,k)
      lestmp4(i,j,k) = tmpXZd2(i,k)
      if (CTsgs(i,j,k).LT.0.d0) CTsgs(i,j,k) = 0.d0
     enddo
    enddo
   enddo
  else if (C_avg_dir.EQ.0) then
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      if (CTsgs(i,j,k).LT.0.d0) CTsgs(i,j,k) = 0.d0
     enddo
    enddo
   enddo
  else
   write(IOUT,'(a)')"ABORTING DSMrho: C_avg_dir is not valid!", C_avg_dir
   stop
  end if
 call ghost(CTsgs,'cfluc',err1)
 call ghost(lestmp4,'cfluc',err1)
 

!Compute CTsgs
 do k = sz,ez
  do j = sy,ey
   do i = sx,ex
    if (lestmp4(i,j,k)==0.d0) then
     CTsgs(i,j,k) = 1.d-20 !prevent divide by zero 
    else
     CTsgs(i,j,k) = 0.5d0 * CTsgs(i,j,k) / lestmp4(i,j,k)
    endif
   enddo
  enddo
 enddo
 call ghost(CTsgs,'cfluc',err1)
 call filterHF(CTsgs,lestmp4,2,1,1,1,'cfluc',kappaT,1.d0,err1)
 CTsgs=lestmp4
 call ghost(CTsgs,'cfluc',err1)

 if (debug) call check_point('DSMrho#4',.false.)

 call MaxMin(CTsgs,tmp1,tmp2,err1)

 write(IOUT,*)"CTsgs: max =", tmp1, " min = ", tmp2

return
end subroutine DSMrho



