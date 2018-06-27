subroutine output(ver,stat)
!@t
! \textbf{subroutine output(ver,stat)}
!@h
!   Description:
!     Output planes, restart files, and statistics.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     Kyle why aren't pencils output here???
!@q

 use ntypes, only: r8
 use Flow,   only: u,v,w,p,rho,scal1,u1_tmp2,u2_tmp2,u3_tmp2,&
                   r_tmp1,r_tmp2,scal1_tmp1,scal1_tmp2,&
                   Vmodel,Rmodel,Smodel
 use LESmod
 use domain, only: sx,ex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use Grid,   only: xc,xe,yc,ye,zc,ze
 use Grid,   only: rdxc,rdxe,rdyc,rdye,rdzc,rdze
 use IO     
 use Parameters
 implicit none

 !Passed Variables
 integer,intent(out)   :: stat
 logical,intent(in)    :: ver

 !Local Variables
 integer               :: err1, i, j, k, s1
 logical,parameter     :: debug=.false.
 logical,parameter     :: XYavg=.true.
 logical,parameter     :: Yavg=.false.
 real(r8) :: mean(sz-1:ez+1), rms(sz-1:ez+1), del_g
 real(r8) :: mean2d(sx-1:ex+1,sz-1:ez+1), rms2d(sx-1:ex+1,sz-1:ez+1)
 real(r8),allocatable,dimension(:,:,:) :: FFtemp

 allocate( FFtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating FFtemp in stat1d"
  goto 1000  
 endif

 if (XYavg.and.Yavg) then
  write(IOUT,*) "ERROR IN OUTPUT:  XYAVG AND YAVG AT THE SAME"
  goto 1000  
 endif
 
 err1=0
  s1=0
 
  !YOUR OUTPUT GOES HERE


  !PENCILS
  if (mod(nstep,wpencils).EQ.0)  then 
   call write_pencil(u,1,nyp2/2,nzp2/2,0,'u1',ver,err1)
  endif

  !CALCULATE AND OUTPUT TOTAL KINETIC, POTENTIAL ENERGY, MOMENTUM, AND MASS
  call energy(err1)

  !STATISTICS
  if (mod(nstep,wstats_small).EQ.0) call statistics_small(err1)
  if (mod(nstep,wstats).EQ.0)       call statistics(err1)

  !FLOW
  if (mod(nstep,wflow).EQ.0) call write_flow(rest_file_write,err1)

  !ALL PLANES


 if (mod(nstep,wplanes).EQ.0)  then 


 if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then

   u1_tmp2 = 0.d0
   u2_tmp2 = 0.d0
   u3_tmp2 = 0.d0
   r_tmp1 = 0.d0
   r_tmp2 = 0.d0
   scal1_tmp1 = 0.d0
   scal1_tmp2 = 0.d0
   call center_velocity(u,u1_tmp2,1 )
   call center_velocity(v,u2_tmp2,2 )
   call center_velocity(w,u3_tmp2,3 )
  
   do i=1,niplanes
   !u1
    call write_plane(u1_tmp2,1,iplanes(i),0,0,'u1',ver,err1)
   !u2
    call write_plane(u2_tmp2,1,iplanes(i),0,0,'u2',ver,err1)
   !u3
    call write_plane(u3_tmp2,1,iplanes(i),0,0,'u3',ver,err1)
   !p
    call write_plane(p,1,iplanes(i),0,0,'p',ver,err1)
   !rho
    call write_plane(rho,1,iplanes(i),0,0,'rho',ver,err1)
   !scal1
    call write_plane(scal1,1,iplanes(i),0,0,'scal1',ver,err1)
   enddo

   do j=1,njplanes
   !u1
    call write_plane(u1_tmp2,2,jplanes(j),0,0,'u1',ver,err1)
   !u2
    call write_plane(u2_tmp2,2,jplanes(j),0,0,'u2',ver,err1)
    !u3
    call write_plane(u3_tmp2,2,jplanes(j),0,0,'u3',ver,err1)
   !p
    call write_plane(p,2,jplanes(j),0,0,'p',ver,err1)
   !rho
    call write_plane(rho,2,jplanes(j),0,0,'rho',ver,err1)
   !scal1
    call write_plane(scal1,2,jplanes(j),0,0,'scal1',ver,err1)
   enddo

   do k=1,nkplanes
   !u1
    call write_plane(u1_tmp2,3,kplanes(k),0,0,'u1',ver,err1)
   !u2
    call write_plane(u2_tmp2,3,kplanes(k),0,0,'u2',ver,err1)
   !u3
    call write_plane(u3_tmp2,3,kplanes(k),0,0,'u3',ver,err1)
   !p
    call write_plane(p,3,kplanes(k),0,0,'p',ver,err1)
   !rho
    call write_plane(rho,3,kplanes(k),0,0,'rho',ver,err1)
   !scal1
    call write_plane(scal1,3,kplanes(k),0,0,'scal1',ver,err1)
   enddo

   if (XYavg) then
   !u'
   call avgX1X2(u1_tmp2,mean,'uc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u1_tmp2(i,j,k)=u1_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u1_tmp2,'cfluc',err1)
  !v'
   call avgX1X2(u2_tmp2,mean,'vc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u2_tmp2(i,j,k)=u2_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u2_tmp2,'cfluc',err1)
   !w'  
   call avgX1X2(u3_tmp2,mean,'wc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u3_tmp2(i,j,k)=u3_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u3_tmp2,'cfluc',err1)
  !p'  
   call avgX1X2(p,mean,'p')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    r_tmp1(i,j,k)=p(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(r_tmp1,'cfluc',err1)
   !rho'  
   call avgX1X2(rho,mean,'rf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    r_tmp2(i,j,k)=rho(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(r_tmp2,'cfluc',err1)
   !scal1'  
   call avgX1X2(scal1,mean,'sf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    scal1_tmp2(i,j,k)=scal1(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(scal1_tmp2,'cfluc',err1)

   elseif (Yavg) then
   !u'
   call avgX2(u1_tmp2,mean2d,rms2d,'uc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u1_tmp2(i,j,k)=u1_tmp2(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(u1_tmp2,'cfluc',err1)
  !v'
   call avgX2(u2_tmp2,mean2d,rms2d,'vc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u2_tmp2(i,j,k)=u2_tmp2(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(u2_tmp2,'cfluc',err1)
   !w'  
   call avgX2(u3_tmp2,mean2d,rms2d,'wc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u3_tmp2(i,j,k)=u3_tmp2(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(u3_tmp2,'cfluc',err1)
  !p'  
   call avgX2(p,mean2d,rms2d,'wc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    r_tmp1(i,j,k)=p(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(r_tmp1,'cfluc',err1)
   !rho'  
   call avgX2(rho,mean2d,rms2d,'rf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
      r_tmp2(i,j,k)=rho(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(r_tmp2,'cfluc',err1)
   !scal1'  
   call avgX2(scal1,mean2d,rms2d,'sf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    scal1_tmp2(i,j,k)=scal1(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(scal1_tmp2,'cfluc',err1)
   else 
    write(IOUT,*)"ERROR IN OUTPUT: PLN AVG iS UKNOWN"
    go to 1000
   endif  

   do i=1,niplanes
   !u1'
    call write_plane(u1_tmp2,1,iplanes(i),0,0,'u1p',ver,err1)
   !u2'
    call write_plane(u2_tmp2,1,iplanes(i),0,0,'u2p',ver,err1)
   !u3'
    call write_plane(u3_tmp2,1,iplanes(i),0,0,'u3p',ver,err1)
   !p'
    call write_plane(r_tmp1,1,iplanes(i),0,0,'pp',ver,err1)
   !rho'
    call write_plane(r_tmp2,1,iplanes(i),0,0,'rhop',ver,err1)
   !scal1'
    call write_plane(scal1_tmp2,1,iplanes(i),0,0,'scal1p',ver,err1)
   enddo
   do j=1,njplanes
   !u1'
    call write_plane(u1_tmp2,2,jplanes(j),0,0,'u1p',ver,err1)
   !u2'
    call write_plane(u2_tmp2,2,jplanes(j),0,0,'u2p',ver,err1)
   !u3'
    call write_plane(u3_tmp2,2,jplanes(j),0,0,'u3p',ver,err1)
   !p'
    call write_plane(r_tmp1,2,jplanes(j),0,0,'pp',ver,err1)
   !rho'
    call write_plane(r_tmp2,2,jplanes(j),0,0,'rhop',ver,err1)
   !scal1'
    call write_plane(scal1_tmp2,2,jplanes(j),0,0,'scal1p',ver,err1)
   enddo
   do k=1,nkplanes
   !u1'
    call write_plane(u1_tmp2,3,kplanes(k),0,0,'u1p',ver,err1)
   !u2'
    call write_plane(u2_tmp2,3,kplanes(k),0,0,'u2p',ver,err1)
   !u3'
    call write_plane(u3_tmp2,3,kplanes(k),0,0,'u3p',ver,err1)
   !p'
    call write_plane(r_tmp1,3,kplanes(k),0,0,'pp',ver,err1)
   !rho'
    call write_plane(r_tmp2,3,kplanes(k),0,0,'rhop',ver,err1)
   !scal1'
    call write_plane(scal1_tmp2,3,kplanes(k),0,0,'scal1p',ver,err1)
   enddo

  !Dissipation
   r_tmp1=0.d0
   r_tmp2=0.d0
   FFtemp=0.d0
  !epsilon11=-2*rRe*(prl_u1'_prl_x1)**2
   call deriv(u1_tmp2,r_tmp1,1)
   FFtemp=-2.d0*rRe*r_tmp1*r_tmp1
  !epsilon12=-rRe*( (prl_u1'_prl_x2)**2 + prl_u1'_prl_x2*prl_u2'_prl_x1 )
   call deriv(u1_tmp2,r_tmp1,2)
   call deriv(u2_tmp2,r_tmp2,1)
   r_tmp2=r_tmp1*r_tmp2
   FFtemp=FFtemp-rRe*(r_tmp1*r_tmp1+r_tmp2)
  !epsilon13=-rRe*( (prl_u1'_prl_x3)**2 + prl_u1'_prl_x3*prl_u3'_prl_x1 )
   call deriv(u1_tmp2,r_tmp1,3)
   call deriv(u3_tmp2,r_tmp2,1)
   r_tmp2=r_tmp2*r_tmp1
   FFtemp=FFtemp-rRe*(r_tmp1*r_tmp1+r_tmp2)
  !epsilon21=-rRe*( (prl_u2'_prl_x1)**2 + prl_u2'_prl_x1*prl_u1'_prl_x2 )
   call deriv(u2_tmp2,r_tmp1,1)
   call deriv(u1_tmp2,r_tmp2,2)
   r_tmp2=r_tmp2*r_tmp1
   FFtemp=FFtemp-rRe*(r_tmp1*r_tmp1+r_tmp2)
  !epsilon22=-2*rRe*(prl_u2'_prl_x2)**2
   call deriv(u2_tmp2,r_tmp1,2)
   FFtemp=FFtemp-2.d0*rRe*r_tmp1*r_tmp1
  !epsilon23=-rRe*( (prl_u2'_prl_x3)**2 + prl_u2'_prl_x3*prl_u3'_prl_x2 )
   call deriv(u2_tmp2,r_tmp1,3)
   call deriv(u3_tmp2,r_tmp2,2)
   r_tmp2=r_tmp2*r_tmp1
   FFtemp=FFtemp-rRe*(r_tmp1*r_tmp1+r_tmp2)
  !epsilon31=-rRe*( (prl_u3'_prl_x1)**2 + prl_u3'_prl_x1*prl_u1'_prl_x3)
   call deriv(u3_tmp2,r_tmp1,1)
   call deriv(u1_tmp2,r_tmp2,3)
   r_tmp2=r_tmp2*r_tmp1
   FFtemp=FFtemp-rRe*(r_tmp1*r_tmp1+r_tmp2)
  !epsilon32=-rRe*( (prl_u3'_prl_x2)**2 + prl_u3'_prl_x3*prl_u2'_prl_x3)
   call deriv(u3_tmp2,r_tmp1,2)
   call deriv(u2_tmp2,r_tmp2,3)
   r_tmp2=r_tmp2*r_tmp1
   FFtemp=FFtemp-rRe*(r_tmp1*r_tmp1+r_tmp2)
  !epsilon33=-2*rRe*(prl_u3'_prl_x3)**2
   call deriv(u3_tmp2,r_tmp1,3)
   FFtemp=FFtemp-2.d0*rRe*r_tmp1*r_tmp1
   call ghost(FFtemp,'cfluc',err1)

   do i=1,niplanes
    call write_plane(FFtemp,1,iplanes(i),0,0,'eps',ver,err1)
   enddo
   do j=1,njplanes
    call write_plane(FFtemp,2,jplanes(j),0,0,'eps',ver,err1)
   enddo
   do k=1,nkplanes
    call write_plane(FFtemp,3,kplanes(k),0,0,'eps',ver,err1)
   enddo

 else
   write(IOUT,'(a)') "ABORTING outputplane, Vmodel = " //trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif

if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
!Subgrid dissipation
  u1_tmp2 = 0.d0
  u2_tmp2 = 0.d0
  u3_tmp2 = 0.d0
  lestmp = 0.d0
  lestmp1 = 0.d0
  lestmp2 = 0.d0
  lestmp3 = 0.d0
  lestmp4 = 0.d0
  un_gt = 0.d0
  vn_gt = 0.d0
  wn_gt = 0.d0
  call center_velocity(u,u1_tmp2,1 )
  call center_velocity(v,u2_tmp2,2 )
  call center_velocity(w,u3_tmp2,3 )

  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp1(i,j,k) = -2.d0*nuT(i,j,k)*(u(i,j,k)-u(i-1,j,k))*rdxc(i)
   lestmp2(i,j,k) = -2.d0*nuT(i,j,k)*(v(i,j,k)-v(i,j-1,k))*rdyc(j)
   lestmp3(i,j,k) = -2.d0*nuT(i,j,k)*(w(i,j,k)-w(i,j,k-1))*rdzc(k)
   un_gt(i,j,k) = -2.d0*nuT(i,j,k)*0.5d0*( (u(i,j+1,k) - u(i,j,k))*rdye(j) &
                                          +(v(i+1,j,k) - v(i,j,k))*rdxe(i) )
   vn_gt(i,j,k) = -2.d0*nuT(i,j,k)*0.5d0*( (u(i,j,k+1) - u(i,j,k))*rdze(k) &
                                          +(w(i+1,j,k) - w(i,j,k))*rdxe(i) )
   wn_gt(i,j,k) = -2.d0*nuT(i,j,k)*0.5d0*( (v(i,j,k+1) - v(i,j,k))*rdze(k) &
                                          +(w(i,j+1,k) - w(i,j,k))*rdye(j) )
  enddo; enddo; enddo
  call ghost(lestmp1,'cfluc',err1)
  call ghost(lestmp2,'cfluc',err1)
  call ghost(lestmp3,'cfluc',err1)
  call ghost(un_gt,'cfluc',err1)
  call ghost(vn_gt,'cfluc',err1)
  call ghost(wn_gt,'cfluc',err1)
 
   if (XYavg) then
   !u'
   call avgX1X2(u1_tmp2,mean,'uc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
      u1_tmp2(i,j,k)=u1_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u1_tmp2,'cfluc',err1)
  !v'
   call avgX1X2(u2_tmp2,mean,'vc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
      u2_tmp2(i,j,k)=u2_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u2_tmp2,'cfluc',err1)
   !w'  
   call avgX1X2(u3_tmp2,mean,'wc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u3_tmp2(i,j,k)=u3_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u3_tmp2,'cfluc',err1)
   !rho'  
   call avgX1X2(rho,mean,'rf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    r_tmp2(i,j,k)=rho(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(r_tmp2,'cfluc',err1)
   !scal1'  
   call avgX1X2(scal1,mean,'sf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    scal1_tmp2(i,j,k)=scal1(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(scal1_tmp2,'cfluc',err1)
   !tau11'
   call avgX1X2(lestmp1,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp1(i,j,k)=lestmp1(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(lestmp1,'cfluc',err1)
  !tau22'
   call avgX1X2(lestmp2,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp2(i,j,k)=lestmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(lestmp2,'cfluc',err1)
   !tau33'
   call avgX1X2(lestmp3,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp3(i,j,k)=lestmp3(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u3_tmp2,'cfluc',err1)
   !tau12'  
   call avgX1X2(un_gt,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    un_gt(i,j,k)=un_gt(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(un_gt,'cfluc',err1)
   !tau13' 
   call avgX1X2(vn_gt,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    vn_gt(i,j,k)=vn_gt(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(vn_gt,'cfluc',err1)
   !tau23' 
   call avgX1X2(wn_gt,mean,'wc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    wn_gt(i,j,k)=wn_gt(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(wn_gt,'cfluc',err1)

   elseif (Yavg) then
   !u'
   call avgX2(u1_tmp2,mean2d,rms2d,'uc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u1_tmp2(i,j,k)=u1_tmp2(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(u1_tmp2,'cfluc',err1)
  !v'
   call avgX2(u2_tmp2,mean2d,rms2d,'vc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u2_tmp2(i,j,k)=u2_tmp2(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(u2_tmp2,'cfluc',err1)
   !w'  
   call avgX2(u3_tmp2,mean2d,rms2d,'wc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u3_tmp2(i,j,k)=u3_tmp2(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(u3_tmp2,'cfluc',err1)
   !rho'  
   call avgX2(rho,mean2d,rms2d,'rf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
      r_tmp2(i,j,k)=rho(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(r_tmp2,'cfluc',err1)
   !scal1'  
   call avgX2(scal1,mean2d,rms2d,'sf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    scal1_tmp2(i,j,k)=scal1(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   !tau11'
   call avgX2(lestmp1,mean2d,rms2d,'cfluc')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      lestmp1(i,j,k)=lestmp1(i,j,k)-mean2d(i,k)
     enddo
    enddo
   enddo
   call ghost(lestmp1,'cfluc',err1)
  !tau22'
   call avgX2(lestmp2,mean2d,rms2d,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp2(i,j,k)=lestmp2(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(lestmp2,'cfluc',err1)
   !tau33'
   call avgX2(lestmp3,mean2d,rms2d,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp3(i,j,k)=lestmp3(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(u3_tmp2,'cfluc',err1)
   !tau12'  
   call avgX2(un_gt,mean2d,rms2d,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    un_gt(i,j,k)=un_gt(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(un_gt,'cfluc',err1)
   !tau13' 
   call avgX2(vn_gt,mean2d,rms2d,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    vn_gt(i,j,k)=vn_gt(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(vn_gt,'cfluc',err1)
   !tau23' 
   call avgX2(wn_gt,mean2d,rms2d,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    wn_gt(i,j,k)=wn_gt(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(wn_gt,'cfluc',err1)
  endif
 
  !Esgs11=-tau11'*du_1'/dx_1
   call deriv(u1_tmp2,r_tmp1,1)
   r_tmp1=-r_tmp1*lestmp1
   FFtemp = FFtemp-r_tmp1
  !Esgs12=-tau12'*du_1'/dx_2
   call deriv(u1_tmp2,r_tmp1,2)
   r_tmp1=-r_tmp1*un_gt
   FFtemp = FFtemp-r_tmp1
  !Esgs13=-tau13'*du_1'/dx_3
   call deriv(u1_tmp2,r_tmp1,3)
   r_tmp1=-r_tmp1*vn_gt
   FFtemp = FFtemp-r_tmp1
  !Esgs21=-tau21'*du_2'/dx_1
   call deriv(u2_tmp2,r_tmp1,1)
   r_tmp1=-r_tmp1*un_gt
   FFtemp = FFtemp-r_tmp1
  !Esgs22=-tau22'*du_2'/dx_2
   call deriv(u2_tmp2,r_tmp1,2)
   r_tmp1=-r_tmp1*lestmp2
   FFtemp = FFtemp-r_tmp1
  !Esgs23=-tau23'*du_2'/dx_3
   call deriv(u2_tmp2,r_tmp1,3)
   r_tmp1=-r_tmp1*wn_gt
   FFtemp = FFtemp-r_tmp1
  !Esgs31=-tau31'*du_3'/dx_1
   call deriv(u3_tmp2,r_tmp1,1)
   r_tmp1=-r_tmp1*vn_gt
   FFtemp = FFtemp-r_tmp1
  !Esgs32=-tau32'*du_3'/dx_2
   call deriv(u3_tmp2,r_tmp1,2)
   r_tmp1=-r_tmp1*wn_gt
   FFtemp = FFtemp-r_tmp1
  !Esgs33=-tau33'*du_3'/dx_3
   call deriv(u3_tmp2,r_tmp1,3)
   r_tmp1=-r_tmp1*lestmp3
   FFtemp = FFtemp-r_tmp1
   call ghost(FFtemp,'cfluc',err1)

   do i=1,niplanes
    call write_plane(FFtemp,1,iplanes(i),0,0,'epssgs',ver,err1)
   enddo
   do j=1,njplanes
    call write_plane(FFtemp,2,jplanes(j),0,0,'epssgs',ver,err1)
   enddo
   do k=1,nkplanes
    call write_plane(FFtemp,3,kplanes(k),0,0,'epssgs',ver,err1)
   enddo
!  elseif (Vmodel.NE.'DNS') then
!   write(IOUT,'(a)') "ABORTING outputplane, Vmodel = " //trim(Vmodel)//" NOT IMPLEMENTED"
!   stat=1
!   return
  endif

 if (Rmodel.EQ.'DNSrho'.or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then
  !Chi-rho: density variance dissipation
  !rho'
   FFtemp=0.d0
   r_tmp2=0.d0
   if (XYavg) then
    call avgX1X2(rho,mean,'rf')
    do k=sz,ez; do j=sy,ey; do i=sx,ex
     r_tmp2(i,j,k)=rho(i,j,k)-mean(k)
    enddo; enddo; enddo
   elseif (Yavg) then
    call avgX2(rho,mean2d,rms2d,'rf')
    do k=sz,ez; do j=sy,ey; do i=sx,ex
      r_tmp2(i,j,k)=rho(i,j,k)-mean2d(i,k)
     enddo; enddo; enddo
   else
    write(IOUT,*)"ERROR IN OUTPUT PLN :CHI-RHO"
    go to 1000 
   endif

   call ghost(r_tmp2,'cfluc',err1)
  !Repsilon_1=-2/Re/Pr*(prl_rho'_prl_x1)**2
   call deriv(r_tmp2,r_tmp1,1)
   FFtemp=-2.d0*rRe*rPr*r_tmp1*r_tmp1
  !Repsilon_1=-2/Re/Pr*(prl_rho'_prl_x2)**2
   call deriv(r_tmp2,r_tmp1,2)
   FFtemp=FFtemp-2.d0*rRe*rPr*r_tmp1*r_tmp1
  !Repsilon_2=-2/Re/Pr(prl_rho'_prl_x3)**2
   call deriv(r_tmp2,r_tmp1,3)
   FFtemp=FFtemp-2.d0*rRe*rPr*r_tmp1*r_tmp1
   call ghost(FFtemp,'cfluc',err1)

   do i=1,niplanes
    call write_plane(FFtemp,1,iplanes(i),0,0,'chiR',ver,err1)
   enddo
   do j=1,njplanes
    call write_plane(FFtemp,2,jplanes(j),0,0,'chiR',ver,err1)
   enddo
   do k=1,nkplanes
    call write_plane(FFtemp,3,kplanes(k),0,0,'chiR',ver,err1)
   enddo
!  else 
!   write(IOUT,'(a)') "ABORTING outputplane, Rmodel = " //trim(Vmodel)//" NOT IMPLEMENTED"
!   stat=1
!   return
  endif

 if (Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then
!subgrid heat flux
   lestmp1=0.d0
   lestmp2=0.d0
   lestmp3=0.d0
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
     !del_g = ( dxc(i) * dyc(j) * dzc(k) )**(1.d0/3.d0)
      del_g = delg(i,j,k)
      lestmp1(i,j,k) = kappaT(i,j,k)*(rho(i+1,j,k)-rho(i-1,j,k))/(xc(i+1)-xc(i-1))
      lestmp2(i,j,k) = kappaT(i,j,k)*(rho(i,j+1,k)-rho(i,j-1,k))/(yc(j+1)-yc(j-1))
      lestmp3(i,j,k) = kappaT(i,j,k)*(rho(i,j,k+1)-rho(i,j,k-1))/(zc(k+1)-zc(k-1))
     enddo
    enddo
   enddo
   call ghost(lestmp1,'cfluc',err1)
   call ghost(lestmp2,'cfluc',err1)
   call ghost(lestmp3,'cfluc',err1)

 if (XYavg) then
 !Q1'
   call avgX1X2(lestmp1,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp1(i,j,k)=lestmp1(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(lestmp1,'cfluc',err1)
 !Q2'
   call avgX1X2(lestmp2,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp2(i,j,k)=lestmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(lestmp2,'cfluc',err1)
 !Q3'
   call avgX1X2(lestmp3,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp3(i,j,k)=lestmp3(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(lestmp3,'cfluc',err1)
 elseif (Yavg) then
 !Q1'
   call avgX2(lestmp1,mean2d,rms2d,'rf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp1(i,j,k)=lestmp1(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(lestmp1,'cfluc',err1)
 !Q2'
   call avgX2(lestmp2,mean2d,rms2d,'rf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp2(i,j,k)=lestmp2(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(lestmp2,'cfluc',err1)
 !Q3'
   call avgX2(lestmp3,mean2d,rms2d,'rf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp3(i,j,k)=lestmp3(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(lestmp3,'cfluc',err1)
  endif

 !ER1sgs=-2*<Q1'*drho'dx1>
   call deriv(r_tmp2,r_tmp1,1)
   FFtemp=FFtemp-2.d0*lestmp1*r_tmp1
 !ER2sgs=-2*<Q2'*drho'dx2>
   call deriv(r_tmp2,r_tmp1,2)
   FFtemp=FFtemp-2.d0*lestmp2*r_tmp1
 !ER3sgs=-2*<Q3'*drho'dx3>
   call deriv(r_tmp2,r_tmp1,3)
   FFtemp=FFtemp-2.d0*lestmp3*r_tmp1

   do i=1,niplanes
    call write_plane(FFtemp,1,iplanes(i),0,0,'chiRsgs',ver,err1)
   enddo
   do j=1,njplanes
    call write_plane(FFtemp,2,jplanes(j),0,0,'chiRsgs',ver,err1)
   enddo
   do k=1,nkplanes
    call write_plane(FFtemp,3,kplanes(k),0,0,'chiRsgs',ver,err1)
   enddo
!  else 
!   write(IOUT,'(a)') "ABORTING outputplane, Rmodel = " //trim(Rmodel)//" NOT IMPLEMENTED"
!   stat=1
!   return
  endif



 if (Smodel.EQ.'DNSscal1'.or.Smodel.EQ.'SSMscal1'.or.Rmodel.EQ.'DSMsca1') then
  !Chi-scal1: scalar1 variance dissipation
  !scal1'
   FFtemp=0.d0
   scal1_tmp2=0.d0
   if (XYavg) then
    call avgX1X2(scal1,mean,'sf')
    do k=sz,ez; do j=sy,ey; do i=sx,ex
     scal1_tmp2(i,j,k)=scal1(i,j,k)-mean(k)
    enddo; enddo; enddo
   elseif (Yavg) then
    call avgX2(scal1,mean2d,rms2d,'sf')
    do k=sz,ez; do j=sy,ey; do i=sx,ex
     scal1_tmp2(i,j,k)=scal1(i,j,k)-mean2d(i,k)
    enddo; enddo; enddo
   else
    write(IOUT,*)"ERROR IN OUTPUT PLN :CHI-SCALAR1"
    go to 1000 
   endif

   call ghost(scal1_tmp2,'cfluc',err1)
  !Repsilon_1=-2/Re/Sc*(prl_scal1'_prl_x1)**2
   call deriv(scal1_tmp2,r_tmp1,1)
   FFtemp=-2.d0*rRe*rSc*r_tmp1*r_tmp1
  !Repsilon_1=-2/Re/Sc*(prl_scal'_prl_x2)**2
   call deriv(scal1_tmp2,r_tmp1,2)
   FFtemp=FFtemp-2.d0*rRe*rSc*r_tmp1*r_tmp1
  !Repsilon_2=-2/Re/Sc(prl_scalar1'_prl_x3)**2
   call deriv(scal1_tmp2,r_tmp1,3)
   FFtemp=FFtemp-2.d0*rRe*rSc*r_tmp1*r_tmp1
   call ghost(FFtemp,'cfluc',err1)

   do i=1,niplanes
    call write_plane(FFtemp,1,iplanes(i),0,0,'chiS1',ver,err1)
   enddo
   do j=1,njplanes
    call write_plane(FFtemp,2,jplanes(j),0,0,'chiS1',ver,err1)
   enddo
   do k=1,nkplanes
    call write_plane(FFtemp,3,kplanes(k),0,0,'chiS1',ver,err1)
   enddo
!  else 
!   write(IOUT,'(a)') "ABORTING outputplane, Smodel = " //trim(Smodel)//" NOT IMPLEMENTED"
!   stat=1
!   return
  endif

 if (Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then
!subgrid salt flux
   lestmp1=0.d0
   lestmp2=0.d0
   lestmp3=0.d0
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp1(i,j,k) = nappaT(i,j,k)*(scal1(i+1,j,k)-scal1(i-1,j,k))/(xc(i+1)-xc(i-1))
    lestmp2(i,j,k) = nappaT(i,j,k)*(scal1(i,j+1,k)-scal1(i,j-1,k))/(yc(j+1)-yc(j-1))
    lestmp3(i,j,k) = nappaT(i,j,k)*(scal1(i,j,k+1)-scal1(i,j,k-1))/(zc(k+1)-zc(k-1))
   enddo; enddo; enddo
   call ghost(lestmp1,'cfluc',err1)
   call ghost(lestmp2,'cfluc',err1)
   call ghost(lestmp3,'cfluc',err1)

 if (XYavg) then
 !QS1'
   call avgX1X2(lestmp1,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp1(i,j,k)=lestmp1(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(lestmp1,'cfluc',err1)
 !QS2'
   call avgX1X2(lestmp2,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp2(i,j,k)=lestmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(lestmp2,'cfluc',err1)
 !QS3'
   call avgX1X2(lestmp3,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp3(i,j,k)=lestmp3(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(lestmp3,'cfluc',err1)
 elseif (Yavg) then
 !QS1'
   call avgX2(lestmp1,mean2d,rms2d,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp1(i,j,k)=lestmp1(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(lestmp1,'cfluc',err1)
 !QS2'
   call avgX2(lestmp2,mean2d,rms2d,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp2(i,j,k)=lestmp2(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(lestmp2,'cfluc',err1)
 !QS3'
   call avgX2(lestmp3,mean2d,rms2d,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp3(i,j,k)=lestmp3(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   call ghost(lestmp3,'cfluc',err1)
  endif

 !ES1sgs=-2*<QS1'*dscal1'dx1>
   call deriv(scal1_tmp2,r_tmp1,1)
   FFtemp=FFtemp-2.d0*lestmp1*r_tmp1
 !ES2sgs=-2*<QS2'*dscal1'dx2>
   call deriv(scal1_tmp2,r_tmp1,2)
   FFtemp=FFtemp-2.d0*lestmp2*r_tmp1
 !ES3sgs=-2*<QS3'*dscal1'dx3>
   call deriv(scal1_tmp2,r_tmp1,3)
   FFtemp=FFtemp-2.d0*lestmp3*r_tmp1

   do i=1,niplanes
    call write_plane(FFtemp,1,iplanes(i),0,0,'chiSsgs',ver,err1)
   enddo
   do j=1,njplanes
    call write_plane(FFtemp,2,jplanes(j),0,0,'chiSsgs',ver,err1)
   enddo
   do k=1,nkplanes
    call write_plane(FFtemp,3,kplanes(k),0,0,'chiSsgs',ver,err1)
   enddo
!  else 
!   write(IOUT,'(a)') "ABORTING outputplane, Smodel = " //trim(Smodel)//" NOT IMPLEMENTED"
!   stat=1
!   return
  endif

 if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
  !omg1
   u1_tmp2 = 0.d0
   r_tmp1 = 0.d0
   call vorticity(u1_tmp2,r_tmp1,1)
   call ghost(u1_tmp2,'cfluc',err1)
   do i=1,niplanes
    call write_plane(u1_tmp2,1,iplanes(i),0,0,'omg1',ver,err1)
   enddo
   do j=1,njplanes
    call write_plane(u1_tmp2,2,jplanes(j),0,0,'omg1',ver,err1)
   enddo
   do k=1,nkplanes
    call write_plane(u1_tmp2,3,kplanes(k),0,0,'omg1',ver,err1)
   enddo

  !omg1'
  if (XYavg) then
   call avgX1X2(u1_tmp2,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u1_tmp2(i,j,k)=u1_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   elseif (Yavg) then
   call avgX2(u1_tmp2,mean2d,rms2d,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u1_tmp2(i,j,k)=u1_tmp2(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
   else
    write(IOUT,*)"ERROR IN OUTPUT PLN: OMG1"
    go to 1000
   endif
    call ghost(u1_tmp2,'cfluc',err1)
   do i=1,niplanes
    call write_plane(u1_tmp2,1,iplanes(i),0,0,'omg1p',ver,err1)
   enddo
   do j=1,njplanes
    call write_plane(u1_tmp2,2,jplanes(j),0,0,'omg1p',ver,err1)
   enddo
   do k=1,nkplanes
    call write_plane(u1_tmp2,3,kplanes(k),0,0,'omg1p',ver,err1)
   enddo
   
  !omg2
   u2_tmp2 = 0.d0
   r_tmp1 = 0.d0
   call vorticity(u2_tmp2,r_tmp1,2)
   call ghost(u2_tmp2,'cfluc',err1)
   do i=1,niplanes
    call write_plane(u2_tmp2,1,iplanes(i),0,0,'omg2',ver,err1)
   enddo
   do j=1,njplanes
    call write_plane(u2_tmp2,2,jplanes(j),0,0,'omg2',ver,err1)
   enddo
   do k=1,nkplanes
    call write_plane(u2_tmp2,3,kplanes(k),0,0,'omg2',ver,err1)
   enddo

  !omg2'
  if (XYavg) then
   call avgX1X2(u2_tmp2,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u2_tmp2(i,j,k)=u2_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
  elseif (Yavg) then
   call avgX2(u2_tmp2,mean2d,rms2d,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u2_tmp2(i,j,k)=u2_tmp2(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
  else
   write(IOUT,*)"ERROR IN OUTPUT PLN: OMG2"
   go to 1000
  endif
   call ghost(u2_tmp2,'cfluc',err1)
   do i=1,niplanes
    call write_plane(u2_tmp2,1,iplanes(i),0,0,'omg2p',ver,err1)
   enddo
   do j=1,njplanes
    call write_plane(u2_tmp2,2,jplanes(j),0,0,'omg2p',ver,err1)
   enddo
   do k=1,nkplanes
    call write_plane(u2_tmp2,3,kplanes(k),0,0,'omg2p',ver,err1)
   enddo
 
  !omg3
   u3_tmp2 = 0.d0
   r_tmp1 = 0.d0
   call vorticity(u3_tmp2,r_tmp1,3)
   call ghost(u3_tmp2,'cfluc',err1)
   do i=1,niplanes
    call write_plane(u3_tmp2,1,iplanes(i),0,0,'omg3',ver,err1)
   enddo
   do j=1,njplanes
    call write_plane(u3_tmp2,2,jplanes(j),0,0,'omg3',ver,err1)
   enddo
   do k=1,nkplanes
    call write_plane(u3_tmp2,3,kplanes(k),0,0,'omg3',ver,err1)
   enddo

  !omg3'
  if (XYavg) then  
   call avgX1X2(u3_tmp2,mean,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u3_tmp2(i,j,k)=u3_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
  elseif (Yavg) then
   call avgX2(u3_tmp2,mean2d,rms2d,'cfluc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u3_tmp2(i,j,k)=u3_tmp2(i,j,k)-mean2d(i,k)
   enddo; enddo; enddo
  else
   write(IOUT,*)"ERROR IN OUTPUT PLN: OMG3"
   go to 1000
  endif

   call ghost(u3_tmp2,'cfluc',err1)
   do i=1,niplanes
    call write_plane(u3_tmp2,1,iplanes(i),0,0,'omg3p',ver,err1)
   enddo
   do j=1,njplanes
    call write_plane(u3_tmp2,2,jplanes(j),0,0,'omg3p',ver,err1)
   enddo
   do k=1,nkplanes
    call write_plane(u3_tmp2,3,kplanes(k),0,0,'omg3p',ver,err1)
   enddo

!  else
!   write(IOUT,'(a)') "ABORTING outputplane-omg, Vmodel = " //trim(Vmodel)//" NOT IMPLEMENTED"
!   stat=1
!   return
  endif


 endif

   if ( allocated(FFtemp) ) deallocate(FFtemp,stat=s1)
   if (s1.NE.0) then
    write(IOUT,*) "Error De-allocating FFtemp in stat1d"
    goto 1000
   endif

 1000 continue
   stat=max(err1,s1)
return
end subroutine output 
