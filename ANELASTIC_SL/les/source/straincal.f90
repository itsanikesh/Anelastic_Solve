












subroutine straincal(un,vn,wn,stat) 
 use LESmod
 use Flow,       only: Vmodel
 use ntypes   
 use Domain,     only: sx,ex,sy,ey,sz,ez,nzp2
 use dd,     only: coords, sizeX3, comm3d
 use Grid,       only: rdxe,rdxc,rdye,rdyc,rdze,rdzc,dxc,dyc,dzc,dxe,dye,dze
 use ratios
 use IO, only: IOUT
 implicit none
 
!Passed Variables
 real(r8),intent(in)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(inout)        :: stat
 
!Local Variables
 integer                   :: i,j,k, err
 real(r8),dimension(sz-1:ez+1) :: u1d, v1d, w1d, ptmp
 real(r8)                  :: tmp1,tmp2,tmp3,tmp4,tmp5,tmp12, tmp23, tmp13
 real(r8)                  :: TS11, TS22, TS33, TS12, TS13, TS23
 logical,parameter         :: debug=.false.

 if (debug) call check_point('strain_cal#0',.false.)

 lestmp1 = 0.d0
 lestmp2 = 0.d0
 lestmp3 = 0.d0
 modS = 0.d0
 modSp = 0.d0
 fmodS = 0.d0
 fmodSp = 0.d0
 delg = 0.d0
 u1d = 0.d0
 v1d  = 0.d0
 w1d = 0.d0

 if (debug) call check_point('strain_cal#1',.false.)
!Kyle said to compute subgrid using fluctuation only
!Bishak used the full velocity as in Salon et al 2007
!Sij is computed using only the fluctuating velocities.

  lestmp1 = un 
  lestmp2 = vn 
  lestmp3 = wn 
!Normal Sij are at P-point. Shear Sij are at cell edges.
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    TS11 = (lestmp1(i+1,j,k)-lestmp1(i,j,k))*rdxc(i)
    TS22 = (lestmp2(i,j+1,k)-lestmp2(i,j,k))*rdyc(j)
    TS33 = (lestmp3(i,j,k+1)-lestmp3(i,j,k))*rdzc(k)
   
    tmp1 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j+1,k)+lestmp1(i+1,j+1,k))
    tmp2 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j-1,k)+lestmp1(i+1,j-1,k))
    tmp3 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i+1,j,k)+lestmp2(i+1,j+1,k))
    tmp4 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i-1,j,k)+lestmp2(i-1,j+1,k))
    TS12 = 0.5d0*((tmp1-tmp2)*rdyc(j)+(tmp3-tmp4)*rdxc(i))
   
    tmp1 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j,k+1)+lestmp1(i+1,j,k+1))
    tmp2 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j,k-1)+lestmp1(i+1,j,k-1))
    tmp3 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i+1,j,k)+lestmp3(i+1,j,k+1))
    tmp4 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i-1,j,k)+lestmp3(i-1,j,k+1))
    TS13 = 0.5d0*((tmp1-tmp2)*rdzc(k)+(tmp3-tmp4)*rdxc(i))
    
    tmp1 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i,j,k+1)+lestmp2(i,j+1,k+1))
    tmp2 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i,j,k-1)+lestmp2(i,j+1,k-1))
    tmp3 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i,j+1,k)+lestmp3(i,j+1,k+1))
    tmp4 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i,j-1,k)+lestmp3(i,j-1,k+1))
    TS23 = 0.5d0*((tmp1-tmp2)*rdzc(k)+(tmp3-tmp4)*rdyc(j))

    modS(i,j,k) = dsqrt(2.d0)*dsqrt(TS11**2.d0+TS22**2.d0+TS33**2.d0+2.d0*TS12**2.d0+&
                  2.d0*TS13**2.d0+2.d0*TS23**2.d0 )
   enddo; enddo; enddo;
 call ghost(modS,'cfluc',err)

 if (debug) call check_point('strain_cal#2',.false.)

!Calculating fluctuating modS
 call avgX1X2(un,u1d,'u')
 call avgX1X2(vn,v1d,'v')
 call avgX1X2(wn,w1d,'w')
 do k=sz,ez; do j=sy,ey; do i=sx,ex
  lestmp1(i,j,k) = un(i,j,k) - u1d(k)
  lestmp2(i,j,k) = vn(i,j,k) - v1d(k)
  lestmp3(i,j,k) = wn(i,j,k) - w1d(k) 
 enddo; enddo; enddo;
 call ghost(lestmp1,'cfluc',err)
 call ghost(lestmp2,'cfluc',err)
 call ghost(lestmp3,'cfluc',err)

   do k=sz,ez; do j=sy,ey; do i=sx,ex
    TS11 = (lestmp1(i+1,j,k)-lestmp1(i,j,k))*rdxc(i)
    TS22 = (lestmp2(i,j+1,k)-lestmp2(i,j,k))*rdyc(j)
    TS33 = (lestmp3(i,j,k+1)-lestmp3(i,j,k))*rdzc(k)
   
    tmp1 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j+1,k)+lestmp1(i+1,j+1,k))
    tmp2 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j-1,k)+lestmp1(i+1,j-1,k))
    tmp3 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i+1,j,k)+lestmp2(i+1,j+1,k))
    tmp4 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i-1,j,k)+lestmp2(i-1,j+1,k))
    TS12 = 0.5d0*((tmp1-tmp2)*rdyc(j)+(tmp3-tmp4)*rdxc(i))
   
    tmp1 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j,k+1)+lestmp1(i+1,j,k+1))
    tmp2 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j,k-1)+lestmp1(i+1,j,k-1))
    tmp3 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i+1,j,k)+lestmp3(i+1,j,k+1))
    tmp4 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i-1,j,k)+lestmp3(i-1,j,k+1))
    TS13 = 0.5d0*((tmp1-tmp2)*rdzc(k)+(tmp3-tmp4)*rdxc(i))
    
    tmp1 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i,j,k+1)+lestmp2(i,j+1,k+1))
    tmp2 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i,j,k-1)+lestmp2(i,j+1,k-1))
    tmp3 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i,j+1,k)+lestmp3(i,j+1,k+1))
    tmp4 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i,j-1,k)+lestmp3(i,j-1,k+1))
    TS23 = 0.5d0*((tmp1-tmp2)*rdzc(k)+(tmp3-tmp4)*rdyc(j))

    modSp(i,j,k) = dsqrt(2.d0)*dsqrt(TS11**2.d0+TS22**2.d0+TS33**2.d0+2.d0*TS12**2.d0+&
                  2.d0*TS13**2.d0+2.d0*TS23**2.d0 )
   enddo; enddo; enddo;
 call ghost(modSp,'cfluc',err)

 if (debug) call check_point('strain_cal#3',.false.)

!Compute filtered modS and its fluctuation
 call get_fmodS(un,vn,wn,fmodS,fmodSp,err)

 if (debug) call check_point('strain_cal#4',.false.)
!Compute delg defined at cell center
 select case(les_grid_size)
  case('3root')
   do k = sz,ez; do j = sy,ey; do i = sx,ex
    delg(i,j,k) = ( dxc(i) * dyc(j) * dzc(k) )**(1.d0/3.d0)
   enddo; enddo; enddo
  case('2root')
   do k = sz,ez; do j = sy,ey; do i = sx,ex
    delg(i,j,k) = ( dxc(i)**2.d0  + dyc(j)**2.d0 + dzc(k)**2.d0 )**(1.d0/2.d0)
   enddo; enddo; enddo
  case DEFAULT
   write(IOUT,'(a)') "ABORTING STRAIN_CAL, LES_GRID_SIZE = : "//trim(les_grid_size)//" NOT IMPLEMENTED"
   err=1
 end select

!Correction for anisotropic grid: need to re-evaluate
! do k = sz,ez; do j = sy,ey; do i = sx,ex
!  tmp4 = min(dxc(i),dyc(j),dzc(k))/max(dxc(i),dyc(j),dzc(k))
!  tmp1 = dxc(i)/max(dxc(i),dyc(j),dzc(k))
!  tmp2 = dyc(j)/max(dxc(i),dyc(j),dzc(k))
!  tmp3 = dzc(k)/max(dxc(i),dyc(j),dzc(k))
!  if (tmp1.LT.1.d0.and.tmp1.GT.tmp4) then 
!   tmp5 = tmp1
!  elseif (tmp2.LT.1.d0.and.tmp1.GT.tmp4) then 
!   tmp5 = tmp2
! elseif (tmp3.LT.1.d0.and.tmp1.GT.tmp4) then 
!   tmp5 = tmp3
!  else
!   tmp5 = tmp4
!  endif
!  delg(i,j,k) = delg(i,j,k) &
!               *cosh( sqrt(4.d0/27.d0*( (log10(tmp4))**2.d0 -log10(tmp4)*log10(tmp5) + (log10(tmp5))**2.d0 ) ) )   
!  enddo; enddo; enddo
 call ghost(delg,'cfluc',err)

 stat=err
 if (debug) call check_point('strain_cal#5',.false.)
 return
end subroutine straincal


subroutine get_fmodS(un,vn,wn,fmodSn,fmodSpn,stat) 
 use LESmod
 use Flow,       only: Vmodel
 use ntypes   
 use dd,     only: coords, sizeX3, comm3d
 use Domain,     only: sx,ex,sy,ey,sz,ez,nzp2
 use Grid,       only: rdxe,rdxc,rdye,rdyc,rdze,rdzc,dxc,dyc,dzc,dxe,dye,dze
 use ratios
 use IO, only: IOUT
 implicit none
 
!Passed Variables
 real(r8),intent(in)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)       :: fmodSn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)       :: fmodSpn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(inout)      :: stat
 
!Local Variables
 integer                   :: i,j,k, err
 real(r8),dimension(sz-1:ez+1) :: u1d, v1d, w1d, ptmp
 real(r8)                  :: tmp1,tmp2,tmp3,tmp4,tmp5,tmp12, tmp23, tmp13
 real(r8)                  :: TS11, TS22, TS33, TS12, TS13, TS23
 logical,parameter         :: debug=.false.

 if (debug) call check_point('get_fmodS#0',.false.)

 lestmp1 = 0.d0
 lestmp2 = 0.d0
 lestmp3 = 0.d0

  call filter(un,lestmp1,2,xfil,yfil,zfil,'u',err) 
  call filter(vn,lestmp2,2,xfil,yfil,zfil,'v',err) 
  call filter(wn,lestmp3,2,xfil,yfil,zfil,'w',err) 

!Normal Sij are at P-point. Shear Sij are at cell edges.
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    TS11 = (lestmp1(i+1,j,k)-lestmp1(i,j,k))*rdxc(i)
    TS22 = (lestmp2(i,j+1,k)-lestmp2(i,j,k))*rdyc(j)
    TS33 = (lestmp3(i,j,k+1)-lestmp3(i,j,k))*rdzc(k)
   
    tmp1 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j+1,k)+lestmp1(i+1,j+1,k))
    tmp2 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j-1,k)+lestmp1(i+1,j-1,k))
    tmp3 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i+1,j,k)+lestmp2(i+1,j+1,k))
    tmp4 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i-1,j,k)+lestmp2(i-1,j+1,k))
    TS12 = 0.5d0*((tmp1-tmp2)*rdyc(j)+(tmp3-tmp4)*rdxc(i))
   
    tmp1 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j,k+1)+lestmp1(i+1,j,k+1))
    tmp2 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j,k-1)+lestmp1(i+1,j,k-1))
    tmp3 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i+1,j,k)+lestmp3(i+1,j,k+1))
    tmp4 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i-1,j,k)+lestmp3(i-1,j,k+1))
    TS13 = 0.5d0*((tmp1-tmp2)*rdzc(k)+(tmp3-tmp4)*rdxc(i))
    
    tmp1 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i,j,k+1)+lestmp2(i,j+1,k+1))
    tmp2 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i,j,k-1)+lestmp2(i,j+1,k-1))
    tmp3 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i,j+1,k)+lestmp3(i,j+1,k+1))
    tmp4 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i,j-1,k)+lestmp3(i,j-1,k+1))
    TS23 = 0.5d0*((tmp1-tmp2)*rdzc(k)+(tmp3-tmp4)*rdyc(j))

    fmodSn(i,j,k) = dsqrt(2.d0)*dsqrt(TS11**2.d0+TS22**2.d0+TS33**2.d0+2.d0*TS12**2.d0+&
                  2.d0*TS13**2.d0+2.d0*TS23**2.d0 )
   enddo; enddo; enddo;
  call ghost(fmodSn,'cfluc',err)


 if (debug) call check_point('get_fmodS#1',.false.)

!Calculating fluctuating fmodS
 call avgX1X2(lestmp1,u1d,'cfluc')
 call avgX1X2(lestmp2,v1d,'cfluc')
 call avgX1X2(lestmp3,w1d,'cfluc')

 if (debug) call check_point('get_fmodS#1a',.false.)
 do k=sz,ez; do j=sy,ey; do i=sx,ex
  lestmp1(i,j,k) = lestmp1(i,j,k) - u1d(k)
  lestmp2(i,j,k) = lestmp2(i,j,k) - v1d(k)
  lestmp3(i,j,k) = lestmp3(i,j,k) - w1d(k) 
 enddo; enddo; enddo;
 call ghost(lestmp1,'cfluc',err)
 call ghost(lestmp2,'cfluc',err)
 call ghost(lestmp3,'cfluc',err)

 if (debug) call check_point('get_fmodS#1b',.false.)

!Normal Sij are at P-point. Shear Sij are at cell edges.
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    TS11 = (lestmp1(i+1,j,k)-lestmp1(i,j,k))*rdxc(i)
    TS22 = (lestmp2(i,j+1,k)-lestmp2(i,j,k))*rdyc(j)
    TS33 = (lestmp3(i,j,k+1)-lestmp3(i,j,k))*rdzc(k)
   
    tmp1 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j+1,k)+lestmp1(i+1,j+1,k))
    tmp2 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j-1,k)+lestmp1(i+1,j-1,k))
    tmp3 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i+1,j,k)+lestmp2(i+1,j+1,k))
    tmp4 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i-1,j,k)+lestmp2(i-1,j+1,k))
    TS12 = 0.5d0*((tmp1-tmp2)*rdyc(j)+(tmp3-tmp4)*rdxc(i))
   
    tmp1 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j,k+1)+lestmp1(i+1,j,k+1))
    tmp2 = 0.25d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)+lestmp1(i,j,k-1)+lestmp1(i+1,j,k-1))
    tmp3 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i+1,j,k)+lestmp3(i+1,j,k+1))
    tmp4 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i-1,j,k)+lestmp3(i-1,j,k+1))
    TS13 = 0.5d0*((tmp1-tmp2)*rdzc(k)+(tmp3-tmp4)*rdxc(i))
    
    tmp1 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i,j,k+1)+lestmp2(i,j+1,k+1))
    tmp2 = 0.25d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)+lestmp2(i,j,k-1)+lestmp2(i,j+1,k-1))
    tmp3 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i,j+1,k)+lestmp3(i,j+1,k+1))
    tmp4 = 0.25d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)+lestmp3(i,j-1,k)+lestmp3(i,j-1,k+1))
    TS23 = 0.5d0*((tmp1-tmp2)*rdzc(k)+(tmp3-tmp4)*rdyc(j))
    
    fmodSpn(i,j,k) = dsqrt(2.d0)*dsqrt(TS11**2.d0+TS22**2.d0+TS33**2.d0+2.d0*TS12**2.d0+&
                  2.d0*TS13**2.d0+2.d0*TS23**2.d0 )
   enddo; enddo; enddo;
  call ghost(fmodSpn,'cfluc',err)


 stat=err
 if (debug) call check_point('get_fmodS #2',.false.)
 return
end subroutine get_fmodS

subroutine center_strain(Svarin,Scen,dir)
 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez
 use IO,     only: IOUT
 use grid,    only: xc,xe,yc,ye,zc,ze,gridtype
 use ratios
 implicit none

!Passed Variables
 real(r8),intent(in)         :: Svarin(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) 
 real(r8),intent(out)        :: Scen(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(in)          :: dir

!Local Variables
 integer              :: i,j,k,err
 real(r8)             :: dep(0:5),ind(0:5),loc,interpoly
 logical,parameter         :: debug=.false.

 if (dir.EQ.1) then
  do k=sz,ez
   do j=sy,ey
    do i=sx,ex
     Scen(i,j,k)=r_1_4*( Svarin(i,j,k)+Svarin(i-1,j,k)+&
                         Svarin(i,j-1,k)+Svarin(i-1,j-1,k) )
    enddo
   enddo
  enddo
 elseif (dir.EQ.2) then
  do k=sz,ez
   do j=sy,ey
    do i=sx,ex
     Scen(i,j,k)=r_1_4*( Svarin(i,j,k)+Svarin(i-1,j,k)+&
                         Svarin(i,j,k-1)+Svarin(i-1,j,k-1) )
    enddo
   enddo
  enddo
 elseif (dir.EQ.3) then
  do k=sz,ez
   do j=sy,ey
    do i=sx,ex
     Scen(i,j,k)=r_1_4*( Svarin(i,j,k)+Svarin(i,j-1,k)+&
                         Svarin(i,j,k-1)+Svarin(i,j-1,k-1) )
    enddo
   enddo
  enddo
 else
 !Invalid direction
  write(IOUT,'(a60,i2)') "INVALID DIRECTION IN center_strain dir must be 1,2,3.  dir= ", dir
 endif
 call ghost(Scen,'cfluc',err)
 return
end subroutine center_strain

subroutine SGSdiff(stat)
 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez,nzp2
 use grid,   only: ze,zc,rdzc
 use dd,     only: commX1X2X3, sizeX1X2X3, realtype, MPI_MAX
 use dd,     only: coords, sizeX3, comm3d
 use IO,     only: IOUT
 use Flow,   only: u,v,w,Vmodel
 use Parameters,   only: rRe,rPr,rSc
 use LESmod,    only: Csgs,CTsgs,CSsgs,delg,modS,modSp, &
                      lestmp1, lestmp2, lestmp3,lestmp4, &
                      S11,S22,S33,S12,S13,S23, &    
                      nuT,kappaT,nappaT, &
                   nu_sgs_max, nu_sgs_min, &  
                   nusm, kappasm, nappasm,  &  
                   kappa_sgs_max, kappa_sgs_min, &  
                   nappa_sgs_max, nappa_sgs_min  
 implicit none

!Passed Variables
 integer,intent(in)          :: stat

!Local Variables
 integer              :: i,j,k,err,ierr,knw,knwC,knwCT,knwCS
 real(r8)             :: Zo,ustar,tmp,tmp1,tmp2,powerC,powerCT,powerCS,modSo,modSpo,So
 real(r8),dimension(sz-1:ez+1) :: nu_nw,kappa_nw,nappa_nw,Csgs_nw,CTsgs_nw,CSsgs_nw,Ztmp1,Ztmp2,Ztmp3,Ztmp4,phim 
 logical,parameter         :: debug=.false.
   
  if (debug) call check_point('SGSdiff #1',.false.)

   call straincal(u,v,w,err)
 
   lestmp1=0.d0;lestmp2=0.d0;lestmp3=0.d0;lestmp4=0.d0
   nuT=0.d0;kappaT=0.d0;nappaT=0.d0
   S11=0.d0;S22=0.d0;S33=0.d0;S12=0.d0;S13=0.d0;S23=0.d0
   Ztmp1=0.d0;Ztmp2=0.d0;Ztmp3=0.d0;Ztmp4=0.d0;phim=0.d0
   nu_nw=0.d0;kappa_nw=0.d0;nappa_nw=0.d0;
   nusm=0.d0;kappasm=0.d0;nappasm=0.d0;
   Csgs_nw=0.d0;CTsgs_nw=0.d0;CSsgs_nw=0.d0;
    
   !Calculate nuT
   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      lestmp4(i,j,k) = -(Csgs(i,j,k)+Csgs_nw(k))*delg(i,j,k)**2.d0*modS(i,j,k) + nu_nw(k)
     enddo
    enddo
   enddo
   nuT = lestmp4
   call ghost(nuT,'cfluc',err)
!   call MaxMin(nuT,nu_sgs_max,nu_sgs_min,err)
!   write(IOUT,*)"Nu_sgs: max = ", nu_sgs_max, " min = ", nu_sgs_min

   !Compute KappaT
   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      lestmp4(i,j,k) = -(CTsgs(i,j,k)+CTsgs_nw(k))*delg(i,j,k)**2.d0*modS(i,j,k) + kappa_nw(k)
     enddo
    enddo
   enddo
   kappaT = lestmp4
   call ghost(kappaT,'cfluc',err)
!   call MaxMin(kappaT,kappa_sgs_max,kappa_sgs_min,err)   
!   write(IOUT,*)"Kappa_sgs: max =", kappa_sgs_max, " min = ", kappa_sgs_min

   !Compute NappaT
   do k = sz,ez
    do j = sy,ey
     do i = sx,ex
      lestmp4(i,j,k) = -(CSsgs(i,j,k)+CSsgs_nw(k))*delg(i,j,k)**2.d0*modS(i,j,k) + nappa_nw(k)
     enddo
    enddo
   enddo
   nappaT = lestmp4
   call ghost(nappaT,'cfluc',err)
!   call MaxMin(nappaT,nappa_sgs_max,nappa_sgs_min,err)
!   write(IOUT,*)"Nappa_sgs: max =", nappa_sgs_max, " min = ", nappa_sgs_min

  if (debug) call check_point('SGSdiff #3',.false.)

  !Compute the scale-similarity sgs stresses stored in Sij
  !Normal stresses at center 
  !Shear stresses at corner
  if (Vmodel.EQ.'DMM') then
   call filter(u,lestmp1,1,1,1,1,'u',err)
   call filter(v,lestmp2,1,1,1,1,'v',err)
   call filter(w,lestmp3,1,1,1,1,'w',err)
   
 !tauSS11
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp4(i,j,k)=(0.5d0*(u(i,j,k)+u(i+1,j,k)))**2.d0
   enddo; enddo; enddo;
   call ghost(lestmp4,'cfluc',err)
   call filter(lestmp4,S11,1,1,1,1,'cfluc',err)
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp4(i,j,k)=(0.5d0*(lestmp1(i,j,k)+lestmp1(i+1,j,k)))**2.d0
   enddo; enddo; enddo;
   call ghost(lestmp4,'cfluc',err)
   S11 = S11 - lestmp4
 
   !tauSS22
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp4(i,j,k)=(0.5d0*(v(i,j,k)+v(i,j+1,k)))**2.d0
   enddo; enddo; enddo;
   call ghost(lestmp4,'cfluc',err)
   call filter(lestmp4,S22,1,1,1,1,'cfluc',err)
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp4(i,j,k)=(0.5d0*(lestmp2(i,j,k)+lestmp2(i,j+1,k)))**2.d0
   enddo; enddo; enddo;
   call ghost(lestmp4,'cfluc',err)
   S22 = S22 - lestmp4

   !tauSS33
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp4(i,j,k)=(0.5d0*(w(i,j,k)+w(i,j,k+1)))**2.d0
   enddo; enddo; enddo;
   call ghost(lestmp4,'cfluc',err)
   call filter(lestmp4,S33,1,1,1,1,'cfluc',err)
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp4(i,j,k)=(0.5d0*(lestmp3(i,j,k)+lestmp3(i,j,k+1)))**2.d0
   enddo; enddo; enddo;
   call ghost(lestmp4,'cfluc',err)
   S33 = S33 - lestmp4
 
   !tauSS12
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp4(i,j,k)=(0.5d0*(u(i,j,k)+u(i,j+1,k))) &
                  *(0.5d0*(v(i,j,k)+v(i+1,j,k))) 
   enddo; enddo; enddo;
   call ghost(lestmp4,'cfluc',err)
   call filter(lestmp4,S12,1,1,1,1,'cfluc',err)
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp4(i,j,k)=(0.5d0*(lestmp1(i,j,k)+lestmp1(i,j+1,k))) &
                  *(0.5d0*(lestmp2(i,j,k)+lestmp2(i+1,j,k)))
   enddo; enddo; enddo;
   call ghost(lestmp4,'cfluc',err)
   S12 = S12 - lestmp4

   !tauSS13
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp4(i,j,k)=(0.5d0*(u(i,j,k)+u(i,j,k+1))) &
                  *(0.5d0*(w(i,j,k)+w(i+1,j,k))) 
   enddo; enddo; enddo;
   call ghost(lestmp4,'cfluc',err)
   call filter(lestmp4,S13,1,1,1,1,'cfluc',err)
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp4(i,j,k)=(0.5d0*(lestmp1(i,j,k)+lestmp1(i,j,k+1))) &
                  *(0.5d0*(lestmp3(i,j,k)+lestmp3(i+1,j,k)))
   enddo; enddo; enddo;
   call ghost(lestmp4,'cfluc',err)
   S13 = S13 - lestmp4

   !tauSS23
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp4(i,j,k)=(0.5d0*(v(i,j,k)+v(i,j,k+1))) &
                  *(0.5d0*(w(i,j,k)+w(i,j+1,k))) 
   enddo; enddo; enddo;
   call ghost(lestmp4,'cfluc',err)
   call filter(lestmp4,S23,1,1,1,1,'cfluc',err)
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp4(i,j,k)=(0.5d0*(lestmp2(i,j,k)+lestmp2(i,j,k+1))) &
                  *(0.5d0*(lestmp3(i,j,k)+lestmp3(i,j+1,k)))
   enddo; enddo; enddo;
   call ghost(lestmp4,'cfluc',err)
   S23 = S23 - lestmp4
  
   call ghost(S11,'cfluc',err)
   call ghost(S22,'cfluc',err)
   call ghost(S33,'cfluc',err)
   call ghost(S12,'cfluc',err)
   call ghost(S13,'cfluc',err)
   call ghost(S23,'cfluc',err)

  endif
 return
end subroutine SGSdiff

subroutine MaxMin(varin,varMax,varMin,stat)
 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez
 use dd
 implicit none

!Passed Variables
 real(r8),intent(in)         :: varin(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) 
 real(r8),intent(out)        :: varMax, varMin
 integer,intent(out)         :: stat

!Local Variables
 integer              :: i,j,k,err
 real(r8)             :: Ltmp, Gtmp

!CALCULATE maximum 
  Ltmp = 0.d0
  Gtmp = 0.d0
  do k = sz,ez
   do j = sy,ey
    do i = sx,ex
     Ltmp = max(Ltmp,varin(i,j,k))
    enddo
   enddo
  enddo

 varMax=Gtmp

!CALCULATE minimum nappa_sgs
  Ltmp = 0.d0
  Gtmp = 0.d0 
  do k = sz,ez
   do j = sy,ey
    do i = sx,ex 
     Ltmp = min(Ltmp,varin(i,j,k))
    enddo
   enddo
  enddo

 varMin=Gtmp  
 stat = err
 return
end subroutine MaxMin


