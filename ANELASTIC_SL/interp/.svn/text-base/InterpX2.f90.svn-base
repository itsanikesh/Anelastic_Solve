program InterpX2
!@t
! \textbf{program InterpX2}
!@h
!   Description:
!     Interpolate from a fine field to a coarser field.
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
!     The domain decomposition can be changed using this tool. The grid
!     length in X2 can be changed as well if necessary. This should be
!     called after InterpX1 but before InterpX3.
!     KYLE CAN YOU ADD A FEW MORE DETAILS ON WHAT THIS DOES / HOW TO USE IT???
!@q

 implicit none
 
 character(len=50) :: fname,baseName,outbaseName 
 integer           :: coords(1:3),ntime,xprocsIN,xprocsOUT,yprocsIN,yprocsOUT,zprocsIN,zprocsOUT
 integer           :: sx,ex,sy,ey,sz,ez,i,j,k,n,iin,jin,kin,s1,xprocs,yprocs,zprocs,n1,n2,n3
 integer           :: nx,ny,nz,nxb,nyb,nzb
 integer           :: nxn,nyn,nzn,nxbn,nybn,nzbn
 real(8)           :: xL,yL,zL,x0,y0,z0,y0n
 real(8)           :: xLn,yLn,zLn
 real(8)           :: time_IN,delt_IN,g_IN,rho_0_IN,Re_IN,Pr_IN
 integer           :: nstep_IN
 real(8)           :: sm(-1:1),ssum
 real(8),allocatable,dimension(:,:,:) :: u,v,w,p,rho,u1_tmp1,u2_tmp1,u3_tmp1,r_tmp1,r_tmp2
 real(8),allocatable,dimension(:,:,:) :: uT,vT,wT,pT,rhoT

 real(8),allocatable,dimension(:,:,:) :: uIN,vIN,wIN,pIN,rhoIN
 real(8),allocatable,dimension(:,:,:) :: uOUT,vOUT,wOUT,pOUT,rhoOUT

 real(8),allocatable,dimension(:) :: xc,xe,xcn,xen,yc,ye,ycn,yen,zc,ze,zcn,zen,zc_rho,ztemp

 real(8) :: Neville, xx,mindist,rho_0,param1,z0r,zLr,ydamp
 integer :: i2,il,j2,jmin,jmax,syn,eyn,k2,nzT
 integer :: nI, nr, n1c,n3c
 logical :: smooth,padding
 real(8),allocatable,dimension(:) :: ind,dep
 !SMOOTHING AND INTERPOLATION AND PADDING IN X2 DIRECTION UNIFORM GRID-->UNIFORM GRID

 !SMOOTHING 
 smooth=.false.
 sm(-1)=1.d0
 sm(0)=2.d0
 sm(-1)=1.d0
 ssum=1.d0/( sm(-1)+sm(0)+sm(1) )

 !INTERPOLATION 
 nI=9 !number of interpolation points must be odd
 nr = (nI-1)/2
 allocate( ind(-nr:nr) )
 allocate( dep(-nr:nr) )
 
 !USER PARAMETERS
 n1c=2
 n3c=3
 baseName='Nflow'
 outbaseName='N2flow'
 ntime=1500
 xprocs=4
 zprocs=8
 yprocsIN=16
 yprocsOUT=8
 ny=1024
 nyn=512
 padding=.true. !set to .true. if yLn>yL
 yL=15.36d0
 yLn=24.576d0
 rho_0=1.d0
 param1=-.006371d0
 y0=1000.d0  + yL/2.d0+yL/dble(ny+1)
 y0n=1000.d0 + yLn/2.d0+yLn/dble(nyn+1)
 zLr=7.68d0
 z0r=zLr/2.d0+zLr/dble(nzT+1)

 !X-blocks
 xprocsIN=1
 xprocsOUT=1
 nx=320
 nxn=320
 xL=1.d0
 xLn=1.d0
 x0=0.d0

 !Z-blocks
 zprocsIN=1
 zprocsOUT=1
 nz=64
 nzn=64
 zL=1.d0
 zLn=1.d0
 z0=0.d0
 nzT=512

 nzb=nz/zprocsIN
 nzbn=nzn/zprocsOUT
 nxb=nx/xprocsIN
 nxbn=nxn/xprocsOUT
 nyb=ny/yprocsIN
 nybn=nyn/yprocsOUT

  allocate( xc(1:nx+2), xe(1:nx+2), xcn(1:nxn+2), xen(1:nxn+2) )
  !DEFINE NEW GRIDS (course)
  do i=1,nxn+2
   xen(i) = -x0 + xLn/dble(nxn+2-1)*dble(i)
  enddo
  do i=2,nxn+2
   xcn(i)  = ( xen(i)+xen(i-1) )/2.d0
  enddo
   xcn(1) = xen(1)-0.5d0*( xen(2)-xen(1) )
 
  !DEFINE OLD GRIDS (fine)
  do i=1,nx+2
   xe(i) = -x0 + xL/dble(nx+2-1)*dble(i)
  enddo
  do i=2,nx+2
   xc(i)  = ( xe(i)+xe(i-1) )/2.d0
  enddo
   xc(1) = xe(1)-0.5d0*( xe(2)-xe(1) )

  allocate( yc(1:ny+2), ye(1:ny+2), ycn(1:nyn+2), yen(1:nyn+2) )

  !DEFINE NEW GRIDS (course)
  do j=1,nyn+2
   yen(j) = -y0n + yLn/dble(nyn+2-1)*dble(j)
  enddo
  do j=2,nyn+2
   ycn(j)  = ( yen(j)+yen(j-1) )/2.d0
  enddo
   ycn(1) = yen(1)-0.5d0*( yen(2)-yen(1) )
 
  !DEFINE OLD GRIDS (fine)
  do j=1,ny+2
   ye(j) = -y0 + yL/dble(ny+2-1)*dble(j)
  enddo
  do j=2,ny+2
   yc(j)  = ( ye(j)+ye(j-1) )/2.d0
  enddo
   yc(1) = ye(1)-0.5d0*( ye(2)-ye(1) ) 


!  write(6,*) ye(1),yc(1),yen(1),ycn(1)
  allocate( zc(1:nz+2), ze(1:nz+2), zcn(1:nzn+2), zen(1:nzn+2), ztemp(1:nzT+2), zc_rho(1:nzT+2) )
 

  !DEFINE NEW GRIDS (course)
  do k=1,nzn+2
   zen(k) = -z0 + zLn/dble(nzn+2-1)*dble(k)
  enddo
  do k=2,nzn+2
   zcn(k)  = ( zen(k)+zen(k-1) )/2.d0
  enddo
   zcn(1) = zen(1)-0.5d0*( zen(2)-zen(1) )
  !DEFINE OLD GRIDS (fine)
  do k=1,nz+2
   ze(k) = -z0 + zL/dble(nz+2-1)*dble(k)
  enddo
  do k=2,nz+2
   zc(k)  = ( ze(k)+ze(k-1) )/2.d0
  enddo
   zc(1) = ze(1)-0.5d0*( ze(2)-ze(1) )


!CREATE A FULL cell centered Z grid to calculate density gradient in padding region
  do k=1,nzT+2
   ztemp(k) = -z0r + zLr/dble(nzT+2-1)*dble(k)
  enddo
  do k=2,nzT+2
   zc_rho(k)  = ( ztemp(k)+ztemp(k-1) )/2.d0
  enddo
   zc_rho(1) = ztemp(1)-0.5d0*( ztemp(2)-ztemp(1) )


 !LOOP OVER Y AND Z PROCS
!n1=1
!n3=3
 do n3 = 0,zprocs-1
  coords(3)=n3
  do n1 = 0,xprocs-1
   coords(1)=n1

    do n=0,yprocsIN-1 !3

     fname=trim(baseName)
     call concati(fname,coords(1))
     call concat(fname,'_')
     call concati(fname,n)
     call concat(fname,'_')
     call concati(fname,coords(3))
     call concat(fname,'.')
     call concati(fname,ntime)

     !Direct Access read
     open(310,file=fname,form='unformatted',status='old',iostat=s1,action='read')
      read(310) nstep_IN,time_IN,delt_IN,g_IN,rho_0_IN,Re_IN,Pr_IN
      read(310) sx,ex,sy,ey,sz,ez 

      if (n.EQ.0) then
       allocate( u(sx-1:ex+1,1:ny+2,sz-1:ez+1) )
       allocate( v(sx-1:ex+1,1:ny+2,sz-1:ez+1) )
       allocate( w(sx-1:ex+1,1:ny+2,sz-1:ez+1) )
       allocate( p(sx-1:ex+1,1:ny+2,sz-1:ez+1) )
       allocate( rho(sx-1:ex+1,1:ny+2,sz-1:ez+1) )
      endif
       allocate( uIN(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) )
       allocate( vIN(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) )
       allocate( wIN(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) )
       allocate( pIN(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) )
       allocate( rhoIN(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) )

      read(310) uIN
      read(310) vIN
      read(310) wIN
      read(310) pIN
      read(310) rhoIN
     close(310)
 
     write(6,*) n,sy,ey,trim(fname)

     do k=sz,ez
      do j=sy,ey
       do i=sx,ex
        u(i,j,k)=uIN(i,j,k)
        v(i,j,k)=vIN(i,j,k)
        w(i,j,k)=wIN(i,j,k)
        p(i,j,k)=pIN(i,j,k)
        rho(i,j,k)=rhoIN(i,j,k)
       enddo
      enddo
     enddo

     deallocate(uIN,vIN,wIN,pIN,rhoIN)

    enddo !3


    if (smooth) then
     allocate( u1_tmp1(1:nx+2,1:ny+2,1:nz+2) )

    !1-2-1 smoothing in y
     u1_tmp1=0.d0
     do k=sz,ez
      do j=2,ny+1
       do i=sx,ex
        u1_tmp1(i,j,k)=ssum*( sm(-1)*u(i,j-1,k)+sm(0)*u(i,j,k)+sm(1)*u(i,j+1,k) )
       enddo
      enddo
     enddo
     u=u1_tmp1

     u1_tmp1=0.d0
     do k=sz,ez
      do j=2,ny+1
       do i=sx,ex
        u1_tmp1(i,j,k)=ssum*( sm(-1)*v(i,j-1,k)+sm(0)*v(i,j,k)+sm(1)*v(i,j+1,k) )
       enddo
      enddo
     enddo
     v=u1_tmp1

     u1_tmp1=0.d0
     do k=sz,ez
      do j=2,ny+1
       do i=sx,ex
        u1_tmp1(i,j,k)=ssum*( sm(-1)*w(i,j-1,k)+sm(0)*w(i,j,k)+sm(1)*w(i,j+1,k) )
       enddo
      enddo
     enddo
     w=u1_tmp1

     u1_tmp1=0.d0
     do k=sz,ez
      do j=2,ny+1
       do i=sx,ex
        u1_tmp1(i,j,k)=ssum*( sm(-1)*p(i,j-1,k)+sm(0)*p(i,j,k)+sm(1)*p(i,j+1,k) )
       enddo
      enddo
     enddo
     p=u1_tmp1

     u1_tmp1=0.d0
     do k=sz,ez
      do j=2,ny+1
       do i=sx,ex
        u1_tmp1(i,j,k)=ssum*( sm(-1)*rho(i,j-1,k)+sm(0)*rho(i,j,k)+sm(1)*rho(i,j+1,k) )
       enddo
      enddo
     enddo
     rho=u1_tmp1

     deallocate( u1_tmp1)

    endif !smoothing

    allocate( uT(sx-1:ex+1,1:nyn+2,sz-1:ez+1) )
    allocate( vT(sx-1:ex+1,1:nyn+2,sz-1:ez+1) )
    allocate( wT(sx-1:ex+1,1:nyn+2,sz-1:ez+1) )
    allocate( pT(sx-1:ex+1,1:nyn+2,sz-1:ez+1) )
    allocate( rhoT(sx-1:ex+1,1:nyn+2,sz-1:ez+1) )

    !OUTPUT centerline uT
    if (n1.eq.n1c.and.n3.eq.n3c) then
     open(unit=100,file='yu.txt',form='formatted')
     do j=1,ny
      write(100,*) yc(j), u(sx+(ex-sx)/2,j,sz+(ez-sz)/2)
     enddo
     close(100)
    endif
!    call write_vtk_field('test.vtk','u',nx+2,ny+2,nz+2,xe,yc,zc,u)


    if (padding) then
     !CHECK IF POINT IS OUTSIDE OLD DOMAIN
     !EDGE
     do j2=1,nyn
      if (yen(j2).LE.ye(1) ) jmin=j2
     enddo
     jmin=jmin+1
     do j2=nyn,1,-1
      if ( yen(j2).GE.ye(ny+1) ) jmax=j2
     enddo
     jmax=jmax-1

     vT(:,:,:) = 0.d0

     !PAD vT near ymin with v(:,2,:)*exp(-y^2)
     do j=2,jmin-1
      ydamp = yen(j)-ye(2)
      vT(:,j,:)  = v(:,2,:)*dexp(-ydamp**2)
     enddo
 
     !PAD vT near ymax with v(:,ny+1,:)*exp(-y^2)
     do j=jmax+1,nyn+1
      ydamp = yen(j)-ye(ny+1)
      vT(:,j,:)  = v(:,ny+1,:)*dexp(-ydamp**2)
     enddo

    else
     jmin=2
     jmax=nyn+1
     vT(:,:,:) = 0.d0
    endif

    do j2=jmin+nr,jmax-nr
     !INDEPENDENT VARIABLE INTERPOLATION POINT
     xx = yen(j2)
     mindist=1e15
     !FIND n surrounding points
     do j = 1,ny
      if ( (ye(j)-xx)**2.LE.mindist ) then
       mindist=(ye(j)-xx)**2
       il = j-1
      endif
     enddo

     !CALCULATE dependent variable at interpolation point
     do k=sz,ez
      do i=sx,ex
       ind = ye(il-nr:il+nr)
       !V
       dep = v(i,il-nr:il+nr,k)
       vT(i,j2,k) = Neville(ind,dep,nI-1,xx)
      enddo
     enddo

    enddo

   !UPDATE NEAR Ymin
   do j2=jmin,jmin+nr-1
    xx = yen(j2)
    do k=sz,ez
     do i=sx,ex
      ind = ye(2:nI+1)
      dep = v(i,2:nI+1,k)
      vT(i,j2,k) = Neville(ind,dep,nI-1,xx)
     enddo
    enddo
   enddo

   !UPDATE NEAR Ymax
   do j2=jmax-nr+1,jmax 
    xx = yen(j2)
    do k=sz,ez
     do i=sx,ex
      ind = ye(ny-nI+1:ny+1)
      dep = v(i,ny-nI+1:ny+1,k)
      vT(i,j2,k) = Neville(ind,dep,nI-1,xx)
     enddo
    enddo
   enddo

   !CENTER
   if (padding) then
    do j2=1,nyn
     if (ycn(j2).LE.yc(1) ) jmin=j2
    enddo
    jmin=jmin+1
    do j2=nyn,1,-1
     if ( ycn(j2).GE.yc(ny+1) ) jmax=j2
    enddo
    jmax=jmax-1

    uT  = 0.d0 
    wT  = 0.d0
    pT  = 0.d0
    rhoT=0.d0

    !PAD uT,wT,pT near ymin with uT,wT,pT(:,2,:)*exp(-y^2)
    do j=2,jmin-1
     ydamp = ycn(j)-yc(2)
     uT(:,j,:)  = u(:,2,:)*dexp(-ydamp**2)
     wT(:,j,:)  = w(:,2,:)*dexp(-ydamp**2)
     pT(:,j,:)  = p(:,2,:)*dexp(-ydamp**2)
    enddo

    !PAD uT,wT,pT near ymax with uT,wT,pT(:,2,:)*exp(-y^2)
    do j=jmax+1,nyn+1
     ydamp = ycn(j)-yc(ny+1)
     uT(:,j,:)  = u(:,ny+1,:)*dexp(-ydamp**2)
     wT(:,j,:)  = w(:,ny+1,:)*dexp(-ydamp**2)
     pT(:,j,:)  = p(:,ny+1,:)*dexp(-ydamp**2)
    enddo

    !Set RHO to background density gradient
    do k=sz,ez
     rhoT(:,:,k)  = rho_0+param1*zc_rho(k)
    enddo

   else
    jmin=2
    jmax=nyn+1
    rhoT=0.d0
    uT  = 0.d0 
    wT  = 0.d0
    pT  = 0.d0
   endif

   do j2=jmin+nr,jmax-nr
    !INDEPENDENT VARIABLE INTERPOLATION POINT
    xx = ycn(j2)
     mindist=1e15
     !FIND n surrounding points
     do j = 1,ny
      if ( (yc(j)-xx)**2.LE.mindist ) then
       mindist=(yc(j)-xx)**2
       il = j-1
      endif
     enddo

     !CALCULATE dependent variable at interpolation point
     do k=sz,ez
      do i=sx,ex
       ind = yc(il-nr:il+nr)
      !U
       dep = u(i,il-nr:il+nr,k)
       uT(i,j2,k) = Neville(ind,dep,nI-1,xx)
       !W
       dep = w(i,il-nr:il+nr,k)
       wT(i,j2,k) = Neville(ind,dep,nI-1,xx)
       !P
       dep = p(i,il-nr:il+nr,k)
       pT(i,j2,k) = Neville(ind,dep,nI-1,xx)
      !RHO
       dep = rho(i,il-nr:il+nr,k)
       rhoT(i,j2,k) = Neville(ind,dep,nI-1,xx)
      enddo
     enddo

   enddo

   !UPDATE NEAR Ymin
   do j2=jmin,jmin+nr-1
    xx = ycn(j2)
    do k=sz,ez
     do i=sx,ex
      ind = yc(2:nI+1)
      dep = u(i,2:nI+1,k)
      uT(i,j2,k) = Neville(ind,dep,nI-1,xx)
      dep = w(i,2:nI+1,k)
      wT(i,j2,k) = Neville(ind,dep,nI-1,xx)
      dep = p(i,2:nI+1,k)
      pT(i,j2,k) = Neville(ind,dep,nI-1,xx)
      dep = rho(i,2:nI+1,k)
      rhoT(i,j2,k) = Neville(ind,dep,nI-1,xx)
     enddo
    enddo

   enddo

   !UPDATE NEAR Ymax
   do j2=jmax-nr+1,jmax 
    xx = ycn(j2)
    do k=sz,ez
     do i=sx,ex
      ind = yc(ny+1-nI:ny+1)
      dep = u(i,ny+1-nI:ny+1,k)
      uT(i,j2,k) = Neville(ind,dep,nI-1,xx)
      dep = w(i,ny+1-nI:ny+1,k)
      wT(i,j2,k) = Neville(ind,dep,nI-1,xx)
      dep = p(i,ny+1-nI:ny+1,k)
      pT(i,j2,k) = Neville(ind,dep,nI-1,xx)
      dep = rho(i,ny+1-nI:ny+1,k)
      rhoT(i,j2,k) = Neville(ind,dep,nI-1,xx)
     enddo
    enddo
   enddo

   deallocate(u,v,w,p,rho)

   !OUTPUT centerline uT
   if (n1.eq.n1c.and.n3.eq.n3c) then
    open(unit=100,file='yuT.txt',form='formatted')
    do j=1,nyn
     write(100,*) ycn(j), uT(sx+(ex-sx)/2,j,sz+(ez-sz)/2)
    enddo
    close(100)
   endif
!   call write_vtk_field('testS.vtk','uOUT',nxn+2,nyn+2,nzn+2,xen,ycn,zcn,rhoT)

   do n=0,yprocsOUT-1
    fname=trim(outbaseName)
    call concati(fname,coords(1))
    call concat(fname,'_')
    call concati(fname,n)
    call concat(fname,'_')
    call concati(fname,coords(3))
    call concat(fname,'.')
    call concati(fname,ntime)

    call MPE_DECOMP1D(nyn,yprocsOUT,n,syn,eyn)
    syn=syn+1
    eyn=eyn+1
    write(6,*) n,syn,eyn,trim(fname)

    allocate( uOUT(sx-1:ex+1,syn-1:eyn+1,sz-1:ez+1) )
    allocate( vOUT(sx-1:ex+1,syn-1:eyn+1,sz-1:ez+1) )
    allocate( wOUT(sx-1:ex+1,syn-1:eyn+1,sz-1:ez+1) )
    allocate( pOUT(sx-1:ex+1,syn-1:eyn+1,sz-1:ez+1) )
    allocate( rhoOUT(sx-1:ex+1,syn-1:eyn+1,sz-1:ez+1) )
    uOUT=0.d0
    vOUT=0.d0
    wOUT=0.d0
    pOUT=0.d0
    rhoOUT=0.d0

    !Direct Access write 
    open(310,file=fname,form='unformatted',status='new',iostat=s1,action='write')
     write(310) nstep_IN,time_IN,delt_IN,g_IN,rho_0_IN,Re_IN,Pr_IN
     write(310) sx,ex,syn,eyn,sz,ez 
 
     do k=sz,ez
      do j=syn,eyn
       do i=sx,ex
        uOUT(i,j,k)=uT(i,j,k)
        vOUT(i,j,k)=vT(i,j,k)
        wOUT(i,j,k)=wT(i,j,k)
        pOUT(i,j,k)=pT(i,j,k)
        rhoOUT(i,j,k)=rhoT(i,j,k)
       enddo
      enddo
     enddo

     write(310) uOUT
     write(310) vOUT
     write(310) wOUT
     write(310) pOUT
     write(310) rhoOUT
    close(310)
    deallocate(uOUT,vOUT,wOUT,pOUT,rhoOUT)
   enddo

   deallocate(uT,vT,wT,pT,rhoT)

  enddo
 enddo

stop
end program InterpX2



subroutine MPE_DECOMP1D(n,numprocs,myid,s,e)
 implicit none

!Passed Variables
 integer :: nlocal
 integer :: deficit
 integer :: n, numprocs, myid, s, e

!------------------------------------------------------------------------
!  From the MPE library
!  This file contains a routine for producing a decomposition of a 1-d 
!  array when given a number of processors.  It may be used in "direct" 
!  product decomposition.  The values returned assume a "global" domain 
!  in [1:n]
!
! Code      : tmgd3
! Called in : mpi_setup
! Calls     : --
!------------------------------------------------------------------------

 nlocal  = n / numprocs
 s       = myid * nlocal + 1
 deficit = mod(n,numprocs)
 s       = s + min(myid,deficit)

 if (myid .lt. deficit) then
  nlocal = nlocal + 1
 endif

 e       = s + nlocal - 1

 if (e .gt. n .or. myid .eq. numprocs-1) e = n

 return
end


