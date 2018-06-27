












subroutine sponge(varNew,VarOld,var,dir,face,stat)
!@t
! \textbf{subroutine sponge(varNew,VarOld,var,dir,face,stat)}
!@h
!   Description:
!     Applies a sponge to the given variable.
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
!     KYLE CAN YOU SAY A FEW LINES ABOUT THIS??? 
!@q

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez
 use Grid,   only: xc,yc,zc
 use Spng
 use boundC
 use IO,     only: IOUT
 implicit none

!Passed Variables
 real(r8),intent(inout)    :: varNew(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)       :: varOld(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(in)        :: var, dir, face
 integer,intent(out)       :: stat

!Local Variables
 integer                   :: i,j,k
 integer                   :: Btype

 Btype = TB(dir,face,var)
 if (Btype.EQ.3) return   !return !Periodic BC no sponge can be applied


 if (dir.EQ.1) then !0
  if (Btype.EQ.1) then !1
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      varNew(i,j,k) = varNew(i,j,k) - SAmpX1(face,var)*PhiX1(i,face)*(varOld(i,j,k)-X1inf(j,k,face,var) )
     enddo
    enddo
   enddo
  elseif (Btype.EQ.2) then !1
   if (face.EQ.1) then !0.5
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
       varNew(i,j,k) = varNew(i,j,k) - SAmpX1(face,var)*PhiX1(i,face)*( varOld(i,j,k)- &
                                           ( X1inf(j,k,face,var) + VB(dir,face,var)*xc(i) ) ) 
      enddo
     enddo
    enddo
   elseif (face.EQ.2) then !0.5
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
       varNew(i,j,k) = varNew(i,j,k) - SAmpX1(face,var)*PhiX1(i,face)*( varOld(i,j,k)- &
                                           ( X1inf(j,k,face,var) - VB(dir,face,var)*xc(i) ) ) 
      enddo
     enddo
    enddo 
   else !0.5
    write(IOUT,'(a,i1,a)') "ERROR IN FACE TYPE:",face,"is not 1,2"
    goto 1000
   endif !0.5
  elseif (Btype.EQ.3) then !1
   return !Periodic BC no sponge can be applied
  else !1
   write(IOUT,'(a,i1,a)') "ERROR IN SPONGE TYPE:",Btype,"is not 1,2,3"
   goto 1000
  endif !1
 elseif (dir.EQ.2) then !0
  if (Btype.EQ.1) then  !2
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      varNew(i,j,k) = varNew(i,j,k) - SAmpX2(face,var)*PhiX2(j,face)*(varOld(i,j,k)-X2inf(i,k,face,var) )
     enddo
    enddo
   enddo
  elseif (Btype.EQ.2) then !2
   if (face.EQ.1) then !2.5
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
       varNew(i,j,k) = varNew(i,j,k) - SAmpX2(face,var)*PhiX2(j,face)*( varOld(i,j,k)- &
                                           ( X2inf(i,k,face,var) - VB(dir,face,var)*(yc(sy-1)-yc(j)) ) ) 
      enddo
     enddo
    enddo
   elseif (face.EQ.2) then !2.5
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
       varNew(i,j,k) = varNew(i,j,k) - SAmpX2(face,var)*PhiX2(j,face)*( varOld(i,j,k)- &
                                           ( X2inf(i,k,face,var) - VB(dir,face,var)*(yc(ey+1)-yc(j)) ) ) 
      enddo
     enddo
    enddo 
   else !2.5
    write(IOUT,'(a,i1,a)') "ERROR IN FACE TYPE:",face,"is not 1,2"
    goto 1000
   endif !2.5
  elseif (Btype.EQ.3)  then !2
   return !Periodic BC no sponge can be applied
  else !2
   write(IOUT,'(a,i1,a)') "ERROR IN SPONGE TYPE:",Btype,"is not 1,2,3"
   goto 1000
  endif !2

 elseif (dir.EQ.3) then !0

  if (Btype.EQ.1) then !3
  do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      varNew(i,j,k) =  varNew(i,j,k) - SAmpX3(face,var)*PhiX3(k,face)*(varOld(i,j,k)- &
                                           X3inf(i,j,face,var) )
     enddo
    enddo
   enddo
 elseif (Btype.EQ.2) then !3
   if (face.EQ.1) then !3.5
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
       varNew(i,j,k) = varNew(i,j,k) - SAmpX3(face,var)*PhiX3(k,face)*( varOld(i,j,k)- &
                                           ( X3inf(i,j,face,var) - VB(dir,face,var)*(zc(sz-1)-zc(k)) ) ) 
      enddo
     enddo
    enddo
   elseif (face.EQ.2) then !3.5
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
       varNew(i,j,k) = varNew(i,j,k) - SAmpX3(face,var)*PhiX3(k,face)*( varOld(i,j,k)- &
                                           ( X3inf(i,j,face,var) - VB(dir,face,var)*(zc(ez+1)-zc(k)) ) ) 
      enddo
     enddo
    enddo 
   else !3.5
    write(IOUT,'(a,i1,a)') "ERROR IN FACE TYPE:",face,"is not 1,2"
    goto 1000
   endif !3.5
  elseif (Btype.EQ.3.or.Btype.EQ.6.or.Btype.EQ.7) then !3
   return !Periodic BC no sponge can be applied
  else !3
   write(IOUT,'(a,i1,a)') "ERROR IN SPONGE TYPE:",Btype,"is not 1,2,3"
   goto 1000
  endif !3
 else !0
  write(IOUT,'(a,i1,a)') "ERROR IN DIRECTION TYPE:",dir,"is not 1,2,3"
  goto 1000
 endif !0

 stat=0 !clean exit
 return

 1000 continue
 stat=-1 !unclean exit
 return
end subroutine


subroutine sponge_setup(inifile,stat)
!@t
! \textbf{subroutine sponge\_setup(inifile,stat)}
!@h
!   Description:
!     Sets up a sponge region and writes the sponge to an output file.
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
!@t A sponge region can be employed near boundaries to control spurious reflections from internal waves 
 !and other disturbances propagating out of the domain.  The sponge layer takes the form of a Rayleigh 
 !damping function which is designed in such a way that it gradually relaxes the velocities and density 
 !to their respective values at the boundaries. This is accomplished by adding the explicit damping terms:
 !\begin{equation}
 !\label{sponge2}
 !- \phi \left( x_i \right) \left[ U_i \left( x_i,t \right)-U_{i,\infty} \right],\quad 
 !- \phi\left(x_i\right) \left[\rho\left(x_i,t\right)-\rho_{\infty} \right]
 !\end{equation}
 !to the $r.h.s$ of momentum, Eq.(\ref{eq:mom}), and density evolution, Eq.~(\ref{eq:rho}), equations respectively.
 !The free stream velocity and density are taken to be $u_{i,\infty}\left(x_i\right) = \left<u_{i}\right>\left(x_i,t=0\right)$
 !and $\rho_{\infty}\left(x_i\right)=\left<\rho\right>\left(t=0\right)$ respectively. 
 !The function $\phi \left(x_i\right)$ increases quadratically from $\phi=0$ to $\phi = A$ in the number 
 !of grid points specified. 
 !The sponge region should always be sufficiently far from the area of interest during the entire 
 !simulation and so it does not affect flow evolution.
!@q

 use ntypes,     only: r8
 use Flow,       only: u,v,w,p,rho,scal1
 use Domain,     only: sx,ex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use Grid,       only: xc,yc,zc,xe,ye,ze
 use Spng
 use boundC
 use IO,         only: IOUT,write_sponge,griddir,ext

 use Parameters, only: Rsponge
 implicit none

!Passed Variables
 integer,intent(out)       :: stat
 character(len=*)          :: inifile

!Local Variables
 integer                   :: i,j,k, s1
 character(len=500)        :: spongefile
 character(len=50)         :: block
 character(len=5),parameter:: defval="ERROR"
 real(r8)                  :: amp

 integer                     :: IOUTL

 logical :: debug = .false.

 stat=0

 if (debug) then
  IOUTL=IOUT !output to screen or output file
 else
  IOUTL=0 !redirect to stderr
 endif

 allocate( X1inf(sy-1:ey+1,sz-1:ez+1,1:2,1:8), stat=s1 )
 allocate( X2inf(sx-1:ex+1,sz-1:ez+1,1:2,1:8), stat=s1 )
 allocate( X3inf(sx-1:ex+1,sy-1:ey+1,1:2,1:8), stat=s1 )
 allocate( phiX1(1:nxp2,1:2), stat=s1 )
 allocate( phiX2(1:nyp2,1:2), stat=s1 )
 allocate( phiX3(1:nzp2,1:2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,'(a,i3)') "ERROR: Allocating Sponge Failed, stat:",s1
  stat=s1
  goto 1000
 endif

!SPONGE EXTENT
!SCAN THE INPUT FILE FOR VALUES
![Sponge]
 block='Sponge'
 call scaniniint(inifile,block,'Ilower',x1spng(1),defval,IOUTL)
 call scaniniint(inifile,block,'Iupper',x1spng(2),defval,IOUTL)
 call scaniniint(inifile,block,'Jlower',x2spng(1),defval,IOUTL)
 call scaniniint(inifile,block,'Jupper',x2spng(2),defval,IOUTL)
 call scaniniint(inifile,block,'Klower',x3spng(1),defval,IOUTL)
 call scaniniint(inifile,block,'Kupper',x3spng(2),defval,IOUTL)
 call scaninireal(inifile,block,'AmpX1',amp,defval,IOUTL)

 SAmpX1(:,:) = amp
 call scaninireal(inifile,block,'AmpX2',amp,defval,IOUTL)
 SAmpX2(:,:) = amp
 call scaninireal(inifile,block,'AmpX3',amp,defval,IOUTL)
 SAmpx3(:,:) = amp
 call scaninilogical(inifile,block,'UseSponge',Rsponge,defval,IOUTL)

if (Rsponge) then
!FREE STREAM CONDITIONS
!X1-Direction
 if ( BND(1,1) ) then
   X1inf(:,:,1,1) = u(sx-1,:,:) 
   X1inf(:,:,1,2) = v(sx-1,:,:) 
   X1inf(:,:,1,3) = w(sx-1,:,:) 
   X1inf(:,:,1,4) = p(sx-1,:,:) 
   X1inf(:,:,1,5) = rho(sx-1,:,:) 
   X1inf(:,:,1,8) = scal1(sx-1,:,:) 
 else
   X1inf(:,:,1,:) = 0.d0
 endif
 if ( BND(1,2) ) then
   X1inf(:,:,2,1) = u(ex+1,:,:) 
   X1inf(:,:,2,2) = v(ex+1,:,:) 
   X1inf(:,:,2,3) = w(ex+1,:,:) 
   X1inf(:,:,2,4) = p(ex+1,:,:) 
   X1inf(:,:,2,5) = rho(ex+1,:,:) 
   X1inf(:,:,2,8) = scal1(ex+1,:,:) 
 else
   X1inf(:,:,2,:) = 0.d0
 endif

!X2-Direction
 if ( BND(2,1) ) then
   X2inf(:,:,1,1) = u(:,sy-1,:) 
   X2inf(:,:,1,2) = v(:,sy-1,:) 
   X2inf(:,:,1,3) = w(:,sy-1,:) 
   X2inf(:,:,1,4) = p(:,sy-1,:) 
   X2inf(:,:,1,5) = rho(:,sy-1,:) 
   X2inf(:,:,1,8) = scal1(:,sy-1,:) 
 else
   X2inf(:,:,1,:) = 0.d0
 endif
 if ( BND(2,2) ) then
   X2inf(:,:,2,1) = u(:,ey+1,:) 
   X2inf(:,:,2,2) = v(:,ey+1,:) 
   X2inf(:,:,2,3) = w(:,ey+1,:) 
   X2inf(:,:,2,4) = p(:,ey+1,:) 
   X2inf(:,:,2,5) = rho(:,ey+1,:) 
   X2inf(:,:,2,8) = scal1(:,ey+1,:) 
 else
   X2inf(:,:,2,:) = 0.d0
 endif
!X3-Direction
 if ( BND(3,1) ) then
   X3inf(:,:,1,1) = u(:,:,sz-1) 
   X3inf(:,:,1,2) = v(:,:,sz-1) 
   X3inf(:,:,1,3) = w(:,:,sz-1) 
   X3inf(:,:,1,4) = p(:,:,sz-1) 
   X3inf(:,:,1,5) = rho(:,:,sz-1) 
   X3inf(:,:,1,8) = scal1(:,:,sz-1) 
 else
   X3inf(:,:,1,:) = 0.d0
 endif
 if ( BND(3,2) ) then
   X3inf(:,:,2,1) = u(:,:,ez+1) 
   X3inf(:,:,2,2) = v(:,:,ez+1) 
   X3inf(:,:,2,3) = w(:,:,ez+1) 
   X3inf(:,:,2,4) = p(:,:,ez+1) 
   X3inf(:,:,2,5) = rho(:,:,ez+1) 
   X3inf(:,:,2,8) = scal1(:,:,ez+1) 
 else
   X3inf(:,:,2,:) = 0.d0
 endif


!*************************************************************
!***************Sponge Damping Function Setup*****************
!*************************************************************

 do i=1,nxp2
  phiX1(i,1) = 0.d0
  phiX1(i,2) = 0.d0
  if (i.LE.x1spng(1).AND.x1spng(1).GT.0) then
   phiX1(i,1) = ( (xc(i)-xc(x1spng(1))) / (xc(2)-xc(x1spng(1))) )**2.d0
  endif
  if (i.GE.nxp2-x1spng(2).AND.x1spng(2).GT.0) then
   phiX1(i,2) = ( (xc(i)-xc(nxp2-x1spng(2))) / (xc(nxp2-1)-xc(nxp2-x1spng(2))) )**2.d0
  endif
 enddo

 do j=1,nyp2
  phiX2(j,1) = 0.d0
  phiX2(j,2) = 0.d0
  if (j.LE.x2spng(1).AND.x2spng(1).GT.0) then
   phiX2(j,1) = ( (yc(j)-yc(x2spng(1))) / (yc(2)-yc(x2spng(1))) )**2.d0
  endif
  if (j.GE.nyp2-x2spng(2).AND.x2spng(2).GT.0) then
   phiX2(j,2) = ( (yc(j)-yc(nyp2-x2spng(2))) / (yc(nyp2-1)-yc(nyp2-x2spng(2))) )**2.d0
  endif
 enddo

 do k=1,nzp2
  phiX3(k,1) = 0.d0
  phiX3(k,2) = 0.d0
  if (k.LE.x3spng(1).AND.x3spng(1).GT.0) then
   phiX3(k,1) = ( (zc(k)-zc(x3spng(1))) / (zc(2)-zc(x3spng(1))) )**2.d0
  endif
  if (k.GE.nzp2-x3spng(2).AND.x3spng(2).GT.0) then
   phiX3(k,2) = ( (zc(k)-zc(nzp2-x3spng(2))) / (zc(nzp2-1)-zc(nzp2-x3spng(2))) )**2.d0
  endif
 enddo

 if (write_sponge.EQ..false.) goto 8000
 spongefile=trim(gridDIR)//'x1_sponge.'//trim(ext)
 open(unit=201,file=spongefile,status='unknown',form='formatted',iostat=s1)
  if (s1.NE.0) then
   close(201)
   write(IOUT,'(a,a,a,i4)') "ERROR Opening File: ",spongefile,". iostat: ",s1
   stat=1
   goto 1000
  endif
 write(201,'(a)') "#X1-SPONGE"
 write(201,'(a)') "#min max"
 write(201,'(a,i4,i4,e12.4)') "#",x1spng(1),x1spng(2)
 write(201,'(a)') "#i xe(i) xc(i) phiX1(i,1) phiX1(i,2)"
 do i=1,nxp2
  write(201,130) i,' ',xe(i),' ',xc(i),' ',phiX1(i,1),' ',phiX1(i,2)
 enddo
 close(201)

 spongefile=trim(gridDIR)//'x2_sponge.'//trim(ext)
 open(unit=202,file=spongefile,status='unknown',form='formatted',iostat=s1)
  if (s1.NE.0) then
   close(202)
   write(IOUT,'(a,a,a,i4)') "ERROR Opening File: ",spongefile,". iostat: ",s1
   stat=2
   goto 1000
  endif
 write(202,'(a)') "#X2-SPONGE"
 write(202,'(a)') "#min max"
 write(202,'(a,i4,i4,e12.4)') "#",x2spng(1),x2spng(2)
 write(202,'(a)') "#j ye(j) yc(j) phiX2(j,1) phiX2(j,2)"
 do j=1,nyp2
  write(202,130) j,' ',ye(j),' ',yc(j),' ',phiX2(j,1),' ',phiX2(j,2)
 enddo
 close(202)

 spongefile=trim(gridDIR)//'x3_sponge.'//trim(ext)
 open(unit=203,file=spongefile,status='unknown',form='formatted',iostat=s1)
  if (s1.NE.0) then
   close(203)
   write(IOUT,'(a,a,a,i4)') "ERROR Opening File: ",spongefile,". iostat: ",s1
   stat=3
   goto 1000
  endif
 write(203,'(a)') "#X3-SPONGE"
 write(203,'(a)') "#min max"
 write(203,'(a,i4,i4,e12.4)') "#",x3spng(1),x3spng(2)
 write(203,'(a)') "#k ze(k) zc(k) phiX3(k,1) phiX3(k,2)"
 do k=1,nzp2
  write(203,130) k,' ',ze(k),' ',zc(k),' ',phiX3(k,1),' ',phiX3(k,2)
 enddo
 close(203)

 8000 continue
 stat=0
 write(IOUT,'(a)') "SPONGE SETUP COMPLETED"
 return

else
 return
endif

 1000 continue
 write(IOUT,'(a)') "SPONGE SETUP FAILED"
 return

 130  FORMAT( 1(i3,a1), 10(1x,f26.22,a1) )

end subroutine sponge_setup
