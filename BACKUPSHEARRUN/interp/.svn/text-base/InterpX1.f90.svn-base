program InterpX1
!@t
! \textbf{program InterpX1}
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
!     Assumes same length in X1 direction (temporal simulation, periodic
!     length cannot change). The domain decomposition can be changed using
!     this tool. This should be called before InterpX2 or InterpX3.
!     KYLE CAN YOU ADD A FEW MORE DETAILS ON WHAT THIS DOES / HOW TO USE IT???
!@q
 implicit none
 
 character(len=50) :: fname,basename,outbasename 
 integer           :: coords(1:3),ntime,xprocsIN,xprocsOUT,yprocsIN,yprocsOUT,zprocsIN,zprocsOUT
 integer           :: sx,ex,sy,ey,sz,ez,i,j,k,n,iin,jin,kin,s1,n2,n3
 integer           :: nx,ny,nz,nxb,nyb,nzb,yprocs,zprocs
 integer           :: nxn,nyn,nzn,nxbn,nybn,nzbn
 real(8)           :: xL,yL,zL,x0,y0,z0,x0n,y0n,z0n
 real(8)           :: xLn,yLn,zLn
 real(8)           :: time_IN,delt_IN,g_IN,rho_0_IN,Re_IN,Pr_IN
 integer           :: nstep_IN
 real(8)           :: sm(-1:1),ssum
 real(8),allocatable,dimension(:,:,:) :: u,v,w,p,rho,u1_tmp1,u2_tmp1,u3_tmp1,r_tmp1,r_tmp2
 real(8),allocatable,dimension(:,:,:) :: uT,vT,wT,pT,rhoT,uper,vper,wper,pper,rhoper

 real(8),allocatable,dimension(:,:,:) :: uIN,vIN,wIN,pIN,rhoIN
 real(8),allocatable,dimension(:,:,:) :: uOUT,vOUT,wOUT,pOUT,rhoOUT

 logical           :: xsweep, ysweep, zsweep
 real(8),allocatable,dimension(:) :: xc,xe,xcn,xen,yc,ye,ycn,yen,zc,ze,zcn,zen,xeper,xcper

 real(8) :: Neville,xx,mindist
 integer :: i2,il,nr,nI,ip,sxn,exn,n2c,n3c
 logical :: smooth
 real(8),allocatable,dimension(:) :: ind,dep
 
 !SMOOTHING AND INTERPOLATION IN X1 DIRECTION UNIFORM GRID --> UNIFORM GRID
 !Assumes same length in X1 direction (temporal simulation, periodic length cannot change)

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
! write(6,*) nI,nr,shape(ind)

!*****************USER PARAMETERS*******************

 !OUTPUT FULL AND COARSENED FIELDS
 n3c=3
 n2c=7

 basename='flow'
 outbasename='Nflow'
 ntime=1500
 yprocs=16
 zprocs=8
 xprocsIN=8
 xprocsOUT=4
 nx=4096
 nxn=1280
 xL=61.44d0
 xLn=61.44d0
 x0=1000.d0  !Add a large number to be sure not to interpolate through a zero... Necessary because of the implementation of Neville's algorithm for interpolation 
 x0n=1000.d0

 !Y-block sizes 
 yprocsIN=1
 yprocsOUT=1
 ny=64
 nyn=64
 yL=1.d0
 yLn=1.d0

 !Z-block sizes
 zprocsIN=1
 zprocsOUT=1
 nz=64
 nzn=64
 zL=1.d0
 zLn=1.d0

 nzb=nz/zprocsIN
 nzbn=nzn/zprocsOUT
 nxb=nx/xprocsIN
 nxbn=nxn/xprocsOUT
 nyb=ny/yprocsIN
 nybn=nyn/yprocsOUT

  allocate( xeper(1-nI:nx+2+nI), xcper(1-nI:nx+2+nI), xc(1:nx+2), xe(1:nx+2), xcn(1:nxn+2), xen(1:nxn+2) )
  !DEFINE NEW GRIDS (coarse)
  do i=1,nxn+2
   xen(i) = -x0n + xLn/dble(nxn+2-1)*dble(i)
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
 
  !DEFINE PERIODICALLY EXTENDED GRIDS 
  do i=-nI+1,nx+nI+2
   xeper(i) = -x0 + xL/dble(nx+2-1)*dble(i)
  enddo
  do i=-nI+2,nx+nI+2
   xcper(i)  = ( xeper(i)+xeper(i-1) )/2.d0
  enddo
   xcper(1) = xeper(1)-0.5d0*( xeper(2)-xeper(1) )


  !DEFINE NEW GRIDS (coarse)
  do j=1,nyn+2
   yen(j) = -y0 + yLn/dble(nyn+2-1)*dble(j)
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

  allocate( zc(1:nz+2), ze(1:nz+2), zcn(1:nzn+2), zen(1:nzn+2) )

  !DEFINE NEW GRIDS (coarse)
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


 !LOOP OVER Y AND Z PROCS
 do n3 = 0,zprocs-1
  coords(3)=n3
  do n2 = 0,yprocs-1
   coords(2)=n2

   do n=0,xprocsIN-1 !LOOP OVER XPROCS
    fname=basename
    call concati(fname,n)
    call concat(fname,'_')
    call concati(fname,coords(2))
    call concat(fname,'_')
    call concati(fname,coords(3))
    call concat(fname,'.')
    call concati(fname,ntime)


    !Direct Access read
    open(310,file=fname,form='unformatted',status='old',iostat=s1,action='read')
     read(310) nstep_IN,time_IN,delt_IN,g_IN,rho_0_IN,Re_IN,Pr_IN
     read(310) sx,ex,sy,ey,sz,ez 
     if (n.EQ.0) then
      allocate( u(1:nx+2,sy-1:ey+1,sz-1:ez+1) )
      allocate( v(1:nx+2,sy-1:ey+1,sz-1:ez+1) )
      allocate( w(1:nx+2,sy-1:ey+1,sz-1:ez+1) )
      allocate( p(1:nx+2,sy-1:ey+1,sz-1:ez+1) )
      allocate( rho(1:nx+2,sy-1:ey+1,sz-1:ez+1) )
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
 
    write(6,*) n,sx,ex,trim(fname)
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

   enddo !LOOP OVER XPROCS

   if (smooth) then

    allocate( u1_tmp1(1:nx+2,1:ny+2,1:nz+2) )
    !1-2-1 smoothing in x
    do k=sz,ez
     do j=sy,ey
      do i=2,nx+1
       u1_tmp1(i,j,k)=ssum*( sm(-1)*u(i-1,j,k)+sm(0)*u(i,j,k)+sm(1)*u(i+1,j,k) )
      enddo
     enddo
    enddo
    u=u1_tmp1

    do k=sz,ez
     do j=sy,ey
      do i=2,nx+1
       u1_tmp1(i,j,k)=ssum*( sm(-1)*v(i-1,j,k)+sm(0)*v(i,j,k)+sm(1)*v(i+1,j,k) )
      enddo
     enddo
    enddo
    v=u1_tmp1

    do k=sz,ez
     do j=sy,ey
      do i=2,nx+1
       u1_tmp1(i,j,k)=ssum*( sm(-1)*w(i-1,j,k)+sm(0)*w(i,j,k)+sm(1)*w(i+1,j,k) )
      enddo
     enddo
    enddo
    w=u1_tmp1

    do k=sz,ez
     do j=sy,ey
      do i=2,nx+1
        u1_tmp1(i,j,k) =ssum*( sm(-1)*p(i-1,j,k)+sm(0)*p(i,j,k)+sm(1)*p(i+1,j,k) )
      enddo
     enddo
    enddo
    p=u1_tmp1

    do k=sz,ez
     do j=sy,ey
      do i=2,nx+1
       u1_tmp1(i,j,k) =ssum*( sm(-1)*rho(i-1,j,k)+sm(0)*rho(i,j,k)+sm(1)*rho(i+1,j,k) )
      enddo
     enddo
    enddo
    rho=u1_tmp1 

    deallocate(u1_tmp1)
   endif !smoothing

   allocate( uT(1:nxn+2,sy-1:ey+1,sz-1:ez+1) )
   allocate( vT(1:nxn+2,sy-1:ey+1,sz-1:ez+1) )
   allocate( wT(1:nxn+2,sy-1:ey+1,sz-1:ez+1) )
   allocate( pT(1:nxn+2,sy-1:ey+1,sz-1:ez+1) )
   allocate( rhoT(1:nxn+2,sy-1:ey+1,sz-1:ez+1) )

   !OUTPUT centerline pencil of u
   if (n2.eq.n2c.and.n3.eq.n3c) then
    open(unit=100,file='1.txt',form='formatted')
    do i=1,nx
     write(100,*) xe(i), u(i,sy+(ey-sy)/2,sz+(ez-sz)/2)
     enddo
    close(100)
   endif
  ! call write_vtk_field('test.vtk','u',nx+2,ny+2,nz+2,xe,yc,zc,u)

!EXTEND PERIODIC 
  !U 
  allocate( uper(1-nI:nx+nI+2,sy-1:ey+1,sz-1:ez+1) )
  do i=2,nx+1
   uper(i,:,:) = u(i,:,:)
  enddo

  !Add Periodic Points to xmin
  do i=1,-nI+1,-1
   ip=(nx)-(-i)    
   uper(i,:,:) = u(ip,:,:)
  enddo
  !Add Periodic Points to xmax
  do i=nx+2,nx+nI+2
   ip=i-nx
   uper(i,:,:) = u(ip,:,:)
  enddo
  deallocate(u)

  allocate( vper(1-nI:nx+nI+2,sy-1:ey+1,sz-1:ez+1) )
  do i=2,nx+1
   vper(i,:,:) = v(i,:,:)
  enddo

  !Add Periodic Points to xmin
  do i=1,-nI+1,-1
   ip=nx-(-i)    
   vper(i,:,:) = v(ip,:,:)
  enddo
  !Add Periodic Points to xmax
  do i=nx+2,nx+nI+2
   ip=i-nx
   vper(i,:,:) = v(ip,:,:)
  enddo
  deallocate(v)

  allocate( wper(1-nI:nx+nI+2,sy-1:ey+1,sz-1:ez+1) )
  do i=2,nx+1
   wper(i,:,:) = w(i,:,:)
  enddo

  !Add Periodic Points to xmin
  do i=1,-nI+1,-1
   ip=nx-(-i)    
   wper(i,:,:) = w(ip,:,:)
  enddo
  !Add Periodic Points to xmax
  do i=nx+2,nx+nI+2
   ip=i-nx
   wper(i,:,:) = w(ip,:,:)
  enddo
  deallocate(w)

  allocate( pper(1-nI:nx+nI+2,sy-1:ey+1,sz-1:ez+1) )
  do i=2,nx+1
   pper(i,:,:) = p(i,:,:)
  enddo

  !Add Periodic Points to xmin
  do i=1,-nI+1,-1
   ip=nx-(-i)    
   pper(i,:,:) = p(ip,:,:)
  enddo
  !Add Periodic Points to xmax
  do i=nx+2,nx+nI+2
   ip=i-nx
   pper(i,:,:) = p(ip,:,:)
  enddo
  deallocate(p)

  allocate( rhoper(1-nI:nx+nI+2,sy-1:ey+1,sz-1:ez+1) )
  do i=2,nx+1
   rhoper(i,:,:) = rho(i,:,:)
  enddo

  !Add Periodic Points to xmin
  do i=1,-nI+1,-1
   ip=nx-(-i)    
   rhoper(i,:,:) = rho(ip,:,:)
  enddo
  !Add Periodic Points to xmax
  do i=nx+2,nx+nI+2
   ip=i-nx
   rhoper(i,:,:) = rho(ip,:,:)
  enddo
  deallocate(rho)

   do i2=2,nxn+1
    !INDEPENDENT VARIABLE INTERPOLATION POINT
    xx = xen(i2)
    mindist=1e15
    !FIND n surrounding points
    do i = 2,nx+1
     if ( (xe(i)-xx)**2.LE.mindist ) then
      mindist=(xe(i)-xx)**2
      il = i-1
     endif
    enddo

    !CALCULATE dependent variable at interpolation point
    do k=sz,ez
     do j=sy,ey
      ind = xeper(il-nr:il+nr)
      !U
      dep = uper(il-nr:il+nr,j,k)
      uT(i2,j,k) = Neville(ind,dep,nI,xx)
     enddo
    enddo

   enddo

   do i2=2,nxn+1
    !INDEPENDENT VARIABLE INTERPOLATION POINT
    xx = xcn(i2)
    mindist=1e15
    !FIND n surrounding points
    do i = 2,nx+1
     if ( (xc(i)-xx)**2.LE.mindist ) then
      mindist=(xc(i)-xx)**2
      il = i-1
     endif
    enddo

    !CALCULATE dependent variable at interpolation point
    do k=sz,ez
     do j=sy,ey

      ind = xcper(il-nr:il+nr)
      !V
      dep = vper(il-nr:il+nr,j,k)
      vT(i2,j,k) = Neville(ind,dep,nI,xx)
      !W
      dep = wper(il-nr:il+nr,j,k)
      wT(i2,j,k) = Neville(ind,dep,nI,xx)
      !P
      dep = pper(il-nr:il+nr,j,k)
      pT(i2,j,k) = Neville(ind,dep,nI,xx)
      !RHO
      dep = rhoper(il-nr:il+nr,j,k)
      rhoT(i2,j,k) = Neville(ind,dep,nI,xx)
     enddo
    enddo

   enddo

   !OUTPUT centerline pencil of uT
   if (n2.EQ.n2c.and.n3.eq.n3c) then
    open(unit=100,file='2.txt',form='formatted')
    do i=1,nxn
     write(100,*) xen(i), uT(i,sy+(ey-sy)/2,sz+(ez-sz)/2)
    enddo
    close(100)
   endif
  !   call write_vtk_field('testS.vtk','uOUT',nxn+2,nyn+2,nzn+2,xen,ycn,zcn,uT)

   deallocate(uper,vper,wper,pper,rhoper)

   do n=0,xprocsOUT-1
    fname=outbasename
    call concati(fname,n)
    call concat(fname,'_')
    call concati(fname,coords(2))
    call concat(fname,'_')
    call concati(fname,coords(3))
    call concat(fname,'.')
    call concati(fname,ntime)

    call MPE_DECOMP1D(nxn,xprocsOUT,n,sxn,exn)
    sxn=sxn+1
    exn=exn+1

    write(6,*) n,sxn,exn,trim(fname)
    allocate( uOUT(sxn-1:exn+1,sy-1:ey+1,sz-1:ez+1) )
    allocate( vOUT(sxn-1:exn+1,sy-1:ey+1,sz-1:ez+1) )
    allocate( wOUT(sxn-1:exn+1,sy-1:ey+1,sz-1:ez+1) )
    allocate( pOUT(sxn-1:exn+1,sy-1:ey+1,sz-1:ez+1) )
    allocate( rhoOUT(sxn-1:exn+1,sy-1:ey+1,sz-1:ez+1) )
    uOUT=0.d0
    vOUT=0.d0
    wOUT=0.d0
    pOUT=0.d0
    rhoOUT=0.d0
 
    !Direct Access write 
    open(310,file=fname,form='unformatted',status='new',iostat=s1,action='write')
     write(310) nstep_IN,time_IN,delt_IN,g_IN,rho_0_IN,Re_IN,Pr_IN
     write(310) sxn,exn,sy,ey,sz,ez 
     uOUT=0.d0
     vOUT=0.d0
     wOUT=0.d0
     pOUT=0.d0
     rhoOUT=0.d0

     do k=sz,ez
      do j=sy,ey
       do i=sxn,exn
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

   enddo !Loop over coarsened x1

   deallocate(uT,vT,wT,pT,rhoT)


  enddo !Loop over yprocs
 enddo !Loop over zprocs

stop
end program InterpX1

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

