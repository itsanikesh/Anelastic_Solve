subroutine DMM
 use Flow
 use LESmod
 use ntypes
 use Grid
 use Domain,  only: sx,ex,sy,ey,sz,ez
 use IO,      only: IOUT
 use ratios
#ifdef PARALLEL
 use dd,     only: myid,comm3d,commx3,realtype,logictype,MPI_MAX,MPI_MIN,MPI_SUM,chartype
#endif

 implicit none

!Local variables
 real(r8),dimension(sz-1:ez+1) :: tmpZd1, tmpZd2, tmpZd3
 real(r8),dimension(sx-1:ex+1,sz-1:ez+1) :: tmpXZd1, tmpXZd2, tmpXZd3
 real(r8):: tmp1, tmp2, tmp3, tmp4, del_g, nu_sgs
 integer :: err1, ij,i,j,k, ierr
 logical,parameter             :: debug=.false.
 integer,parameter    :: C_avg_dir=12

 if (debug) call check_point('DMM#0',.false.)
 
 call straincal(u,v,w,err1)

 if (debug) call check_point('DMM#1',.false.)
!***********************************
!Momentum Equation
!Begin computing denominator Mij*Mij
!Zeros temporary variable
!***********************************
 lestmp  = 0.d0
 lestmp1 = 0.d0
 lestmp2 = 0.d0
 lestmp3 = 0.d0
 lestmp4 = 0.d0
 Csgs = 0.d0
 un_gt = 0.d0
 vn_gt = 0.d0
 wn_gt = 0.d0

!get un_gt, vn_gt, and wn_gt
   call center_velocity(u,lestmp1,1)
   call filter(lestmp1,un_gt,1,xfil,yfil,zfil,'cfluc',err1)
   call center_velocity(v,lestmp1,2)
   call filter(lestmp1,vn_gt,1,xfil,yfil,zfil,'cfluc',err1)
   call center_velocity(w,lestmp1,3)
   call filter(lestmp1,wn_gt,1,xfil,yfil,zfil,'cfluc',err1)

 do ij = 1,6
!interp. shear Sijs to P-point, stored in lestmp1.
  if (ij==1) then
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp1(i,j,k) = (u(i+1,j,k)-u(i,j,k))*rdxc(i)
   enddo; enddo; enddo;
  elseif (ij==2) then
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp1(i,j,k) = (v(i,j+1,k)-v(i,j,k))*rdyc(j)
   enddo; enddo; enddo;
  elseif (ij==3) then
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp1(i,j,k) = (w(i,j,k+1)-w(i,j,k))*rdzc(k)
   enddo; enddo; enddo;
  elseif (ij==4) then
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    tmp1 = 0.25d0*(u(i,j,k)+u(i+1,j,k)+u(i,j+1,k)+u(i+1,j+1,k))
    tmp2 = 0.25d0*(u(i,j,k)+u(i+1,j,k)+u(i,j-1,k)+u(i+1,j-1,k))
    tmp3 = 0.25d0*(v(i,j,k)+v(i,j+1,k)+v(i+1,j,k)+v(i+1,j+1,k))
    tmp4 = 0.25d0*(v(i,j,k)+v(i,j+1,k)+v(i-1,j,k)+v(i-1,j+1,k))
    lestmp1(i,j,k) = 0.5d0*((tmp1-tmp2)*rdyc(j)-(tmp3-tmp4)*rdxc(i))
   enddo; enddo; enddo;
  else if (ij==5) then
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    tmp1 = 0.25d0*(u(i,j,k)+u(i+1,j,k)+u(i,j,k+1)+u(i+1,j,k+1))
    tmp2 = 0.25d0*(u(i,j,k)+u(i+1,j,k)+u(i,j,k-1)+u(i+1,j,k-1))
    tmp3 = 0.25d0*(w(i,j,k)+w(i,j,k+1)+w(i+1,j,k)+w(i+1,j,k+1))
    tmp4 = 0.25d0*(w(i,j,k)+w(i,j,k+1)+w(i-1,j,k)+w(i-1,j,k+1))
    lestmp1(i,j,k) = 0.5d0*((tmp1-tmp2)*rdzc(k)-(tmp3-tmp4)*rdxc(i))
   enddo; enddo; enddo;
  else if (ij==6) then
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    tmp1 = 0.25d0*(v(i,j,k)+v(i,j+1,k)+v(i,j,k+1)+v(i,j+1,k+1))
    tmp2 = 0.25d0*(v(i,j,k)+v(i,j+1,k)+v(i,j,k-1)+v(i,j+1,k-1))
    tmp3 = 0.25d0*(w(i,j,k)+w(i,j,k+1)+w(i,j+1,k)+w(i,j+1,k+1))
    tmp4 = 0.25d0*(w(i,j,k)+w(i,j,k+1)+w(i,j-1,k)+w(i,j-1,k+1))
    lestmp1(i,j,k) = 0.5d0*((tmp1-tmp2)*rdzc(k)-(tmp3-tmp4)*rdyc(j))
   enddo; enddo; enddo;
  endif
  call ghost(lestmp1,'cfluc',err1)

  do k = sz,ez
   do j = sy,ey
    do i = sx,ex
     lestmp2(i,j,k) =  delg(i,j,k)**2.d0 * modS(i,j,k) * lestmp1(i,j,k) 
    enddo
   enddo
  enddo
  call ghost(lestmp2,'cfluc',err1)
  call filter(lestmp2,lestmp3,2,xfil,yfil,zfil,'cfluc',err1)
!now lestmp3 stores only the filter(product) of Mij

!Filter cell center strain, stored  in lestmp2
  call filter(lestmp1,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
!Filter modS, stored in lestmp1
  !call filter(modS,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
  lestmp1 = fmodS

  if (debug) call check_point('DMM#3',.false.)

  do k = sz,ez
   do j = sy,ey
    do i = sx,ex
     lestmp2(i,j,k) = (r_dgt_dg * delg(i,j,k))**2.d0 * lestmp1(i,j,k) * lestmp2(i,j,k) 
    enddo
   enddo
  enddo
  call ghost(lestmp2,'cfluc',err1)
!lestmp2 now stores the product(filter) portion of Mij

  if (debug) call check_point('DMM#4',.false.)
!total Mij at P-point, stored in lestmp3
  lestmp3 = lestmp3-lestmp2
  call ghost(lestmp3,'cfluc',err1)

  if (debug) call check_point('DMM#5',.false.)

!First part of Lij: (ui_g*uj_g)_t
  if (ij==1) then
   call center_velocity(u,lestmp1,1)
   lestmp1 = lestmp1*lestmp1
   call filter(lestmp1,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + lestmp3*lestmp2
  else if (ij==2) then
   call center_velocity(v,lestmp1,2)
   lestmp1 = lestmp1*lestmp1
   call filter(lestmp1,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + lestmp3*lestmp2
  else if (ij==3) then
   call center_velocity(w,lestmp1,3)
   lestmp1 = lestmp1*lestmp1
   call filter(lestmp1,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + lestmp3*lestmp2
  else if (ij==4) then
   call center_velocity(u,lestmp1,1)
   call center_velocity(v,lestmp2,2)
   lestmp1 = lestmp1*lestmp2
   call filter(lestmp1,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + 2.d0*lestmp3*lestmp2
  else if (ij==5) then
   call center_velocity(u,lestmp1,1)
   call center_velocity(w,lestmp2,3)
   lestmp1 = lestmp1*lestmp2
   call filter(lestmp1,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + 2.d0*lestmp3*lestmp2
  else if (ij==6) then
   call center_velocity(v,lestmp1,2)
   call center_velocity(w,lestmp2,3)
   lestmp1 = lestmp1*lestmp2
   call filter(lestmp1,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + 2.d0*lestmp3*lestmp2
  endif
 if (debug) call check_point('DMM#6',.false.)

!Second part of Lij: ui_gt*uj_gt
  if (ij==1) then
   call center_velocity(u,lestmp1,1)
   call filter(lestmp1,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - lestmp3*lestmp2*lestmp2
  else if (ij==2) then
   call center_velocity(v,lestmp1,2)
   call filter(lestmp1,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - lestmp3*lestmp2*lestmp2
  else if (ij==3) then
   call center_velocity(w,lestmp1,3)
   call filter(lestmp1,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - lestmp3*lestmp2*lestmp2
  else if (ij==4) then
   call center_velocity(u,lestmp,1)
   call filter(lestmp,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   call center_velocity(v,lestmp,2)
   call filter(lestmp,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - 2.d0*lestmp3*lestmp1*lestmp2
  else if (ij==5) then
   call center_velocity(u,lestmp,1)
   call filter(lestmp,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   call center_velocity(w,lestmp,3)
   call filter(lestmp,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - 2.d0*lestmp3*lestmp1*lestmp2
  else if (ij==6) then
   call center_velocity(v,lestmp,2)
   call filter(lestmp,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   call center_velocity(w,lestmp,3)
   call filter(lestmp,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - 2.d0*lestmp3*lestmp1*lestmp2
  endif
 
!Compute Nij
!First part of Nij: +(ui_gt*uj_gt)_gt
  if (ij==1) then
   lestmp1=un_gt*un_gt
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - lestmp3*lestmp1
  elseif (ij==2) then
   lestmp1=vn_gt*vn_gt
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - lestmp3*lestmp1
  elseif (ij==3) then
   lestmp1=wn_gt*wn_gt
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - lestmp3*lestmp1
  elseif (ij==4) then
   lestmp1=un_gt*vn_gt
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - 2.d0*lestmp3*lestmp1
  elseif (ij==5) then
   lestmp1=un_gt*wn_gt
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - 2.d0*lestmp3*lestmp1
  elseif (ij==6) then
   lestmp1=vn_gt*wn_gt
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - 2.d0*lestmp3*lestmp1
  endif

!Second part of Nij:  ui_gtgt*uj_gtgt
  if (ij==1) then
   call filter(un_gt,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + lestmp3*lestmp1*lestmp1
  else if (ij==2) then
   call filter(vn_gt,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + lestmp3*lestmp1*lestmp1
  else if (ij==3) then
   call filter(wn_gt,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + lestmp3*lestmp1*lestmp1
  else if (ij==4) then
   call filter(un_gt,lestmp,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   call filter(vn_gt,lestmp,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + 2.d0*lestmp3*lestmp1*lestmp2
  else if (ij==5) then
   call filter(un_gt,lestmp,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   call filter(wn_gt,lestmp,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + 2.d0*lestmp3*lestmp1*lestmp2
  else if (ij==6) then
   call filter(vn_gt,lestmp,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   call filter(wn_gt,lestmp,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp,lestmp2,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + 2.d0*lestmp3*lestmp1*lestmp2
  endif

!now third part of Nij: ((ui_g*uj_g)_g)_t 
  if (ij==1) then
   call center_velocity(u,lestmp1,1)
   lestmp1 = lestmp1**2.d0
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + lestmp3*lestmp1
  else if (ij==2) then
   call center_velocity(v,lestmp1,2)
   lestmp1 = lestmp1**2.d0
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + lestmp3*lestmp1
  else if (ij==3) then
   call center_velocity(w,lestmp1,3)
   lestmp1 = lestmp1**2.d0
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + lestmp3*lestmp1
  else if (ij==4) then
   call center_velocity(u,lestmp1,1)
   call center_velocity(v,lestmp2,2)
   lestmp1 = lestmp1*lestmp2
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + 2.d0*lestmp3*lestmp1
  else if (ij==5) then
   call center_velocity(u,lestmp1,1)
   call center_velocity(w,lestmp2,3)
   lestmp1 = lestmp1*lestmp2
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + 2.d0*lestmp3*lestmp1
  else if (ij==6) then
   call center_velocity(v,lestmp1,2)
   call center_velocity(w,lestmp2,3)
   lestmp1 = lestmp1*lestmp2
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 + 2.d0*lestmp3*lestmp1
  endif
 
!now fourth part of Nij: (ui_gg*uj_gg)_t 
  if (ij==1) then
   call center_velocity(u,lestmp1,1)
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   lestmp2=lestmp2**2.d0
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - lestmp3*lestmp1
  else if (ij==2) then
   call center_velocity(v,lestmp1,2)
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   lestmp2 = lestmp2**2.d0
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - lestmp3*lestmp1
  else if (ij==3) then
   call center_velocity(w,lestmp1,3)
   call filter(lestmp1,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   lestmp2 = lestmp2**2.d0
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - lestmp3*lestmp1
  else if (ij==4) then
   call center_velocity(u,lestmp,1)
   call filter(lestmp,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call center_velocity(v,lestmp,2)
   call filter(lestmp,lestmp1,1,xfil,yfil,zfil,'cfluc',err1)
   lestmp2 = lestmp1*lestmp2
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - 2.d0*lestmp3*lestmp1
  else if (ij==5) then
   call center_velocity(u,lestmp,1)
   call filter(lestmp,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call center_velocity(w,lestmp,3)
   call filter(lestmp,lestmp1,1,xfil,yfil,zfil,'cfluc',err1)
   lestmp2 = lestmp1*lestmp2
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - 2.d0*lestmp3*lestmp1
  else if (ij==6) then
   call center_velocity(v,lestmp,2)
   call filter(lestmp,lestmp2,1,xfil,yfil,zfil,'cfluc',err1)
   call center_velocity(w,lestmp,3)
   call filter(lestmp,lestmp1,1,xfil,yfil,zfil,'cfluc',err1)
   lestmp2 = lestmp1*lestmp2
   call filter(lestmp2,lestmp1,2,xfil,yfil,zfil,'cfluc',err1)
   lestmp4 = lestmp4 - 2.d0*lestmp3*lestmp1
  endif
 
!Compute denominator = Mij*Mij, stored in Csgs
  if (ij==1.OR.ij==2.OR.ij==3) then
   Csgs =  Csgs + lestmp3*lestmp3
  else
   Csgs =  Csgs + 2.d0*lestmp3*lestmp3
  endif
 enddo !end ij loop

 call ghost(Csgs,'cfluc',err1)
 call ghost(lestmp4,'cfluc',err1)
 if (debug) call check_point('DMM#7',.false.)

 write(IOUT,*)"DMM"

  if (C_avg_dir.EQ.12) then
   call avgX1X2(lestmp4,tmpZd1,'cfluc')
   call avgX1X2(Csgs,tmpZd2,'cfluc')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      lestmp4(i,j,k) = tmpZd1(k)
      Csgs(i,j,k) = tmpZd2(k)
      if (lestmp4(i,j,k).LT.0.d0) lestmp4(i,j,k) = 0.d0
     enddo
    enddo
   enddo
  else if (C_avg_dir.EQ.2) then
   call avgX2(lestmp4,tmpXZd1,tmpXZd3,'cfluc')
   call avgX2(Csgs,tmpXZd2,tmpXZd3,'cfluc')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      lestmp4(i,j,k) = tmpXZd1(i,k)
      Csgs(i,j,k) = tmpXZd2(i,k)
      if (lestmp4(i,j,k).LT.0.d0) lestmp4(i,j,k) = 0.d0
     enddo 
    enddo 
   enddo
  else if (C_avg_dir.EQ.0) then 
   !use Lagrangian Averaging as Meneveau et al JFM 1996
   !input lestmp1-LijMij and lestmp2-MijMij
   !output lestmp4-JLM and Csgs-JMM
   lestmp1 = lestmp4
   lestmp2 = Csgs
   call LagAvg(lestmp1,lestmp2,lestmp4,Csgs,err1)
  else
   write(IOUT,'(a)')'ABORTING DMM: C_avg_dir is invalid', C_avg_dir
   stop
  end if
  call ghost(lestmp4,'cfluc',err1)
  call ghost(Csgs,'cfluc',err1)

!Compute Csgs
 do k = sz,ez
  do j = sy,ey
   do i = sx,ex
    if (Csgs(i,j,k).EQ.0.d0) then
     Csgs(i,j,k) = 1.d-20 !prevent divide by zero
    else
     Csgs(i,j,k) = 0.5d0 * lestmp4(i,j,k) / Csgs(i,j,k)
    endif
   enddo
  enddo
 enddo
 call ghost(Csgs,'cfluc',err1)
 call MaxMin(Csgs,tmp1,tmp2,err1)
 write(IOUT,*)"Cdyn: max = ", tmp1, " min = ", tmp2

return
end subroutine DMM



