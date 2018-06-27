program InterpX3
!@t
! \textbf{program InterpX3}
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
!     length in X3 can be changed as well if necessary. This should be 
!     called after InterpX1 and InterpX2.
!     KYLE CAN YOU ADD A FEW MORE DETAILS ON WHAT THIS DOES / HOW TO USE IT???
!@q

 implicit none
 
 character(len=50) :: fname,baseName,outbaseName
 integer           :: coords(1:3),ntime,xprocsIN,xprocsOUT,yprocsIN,yprocsOUT,zprocsIN,zprocsOUT
 integer           :: sx,ex,sy,ey,sz,ez,i,j,k,n,iin,jin,kin,s1,xprocs,yprocs,zprocs,n1,n2,n3
 integer           :: nx,ny,nz,nxb,nyb,nzb
 integer           :: nxn,nyn,nzn,nxbn,nybn,nzbn
 real(8)           :: xL,yL,zL,x0,y0,z0,z0n
 real(8)           :: xLn,yLn,zLn
 real(8)           :: time_IN,delt_IN,g_IN,rho_0_IN,Re_IN,Pr_IN
 integer           :: nstep_IN
 real(8)           :: sm(-1:1),ssum
 real(8),allocatable,dimension(:,:,:) :: u,v,w,p,rho,u1_tmp1,u2_tmp1,u3_tmp1,r_tmp1,r_tmp2
 real(8),allocatable,dimension(:,:,:) :: uT,vT,wT,pT,rhoT

 real(8),allocatable,dimension(:,:,:) :: uIN,vIN,wIN,pIN,rhoIN
 real(8),allocatable,dimension(:,:,:) :: uOUT,vOUT,wOUT,pOUT,rhoOUT

 real(8),allocatable,dimension(:) :: xc,xe,xcn,xen,yc,ye,ycn,yen,zc,ze,zcn,zen,ztemp,zc_rho

 real(8) :: Neville, xx,mindist,rho_0,param1,z0r,zdamp
 integer :: i2,il,k2,kmin,kmax,szn,ezn,n1c,n2c
 integer :: nI, nr
 logical :: smooth,padding
 real(8),allocatable,dimension(:) :: ind,dep

 !SMOOTHING AND INTERPOLATION AND PADDING IN X3 DIRECTION UNIFORM GRID-->UNIFORM GRID

 !SMOOTHING STENCIL
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
 n1c=1
 n2c=3
 baseName='N2flow'
 outbaseName='N3flow'
 ntime=1500
 xprocs=4
 yprocs=8
 zprocsIN=8
 zprocsOUT=4
 nz=512
 nzn=256
 padding=.true.
 zL=7.68d0
 zLn=12.288d0
 rho_0=1.d0
 param1=-.006371d0
 z0=1000.d0  + zL/2.d0+zL/dble(nz+1)
 z0n=1000.d0 + zLn/2.d0+zLn/dble(nzn+1)
 z0r=zL/2.d0+zL/dble(nz+1)

 !X-blocks
 xprocsIN=1
 xprocsOUT=1
 nx=320
 nxn=320
 xL=1.d0
 xLn=1.d0

 !Y-blocks
 yprocsIN=1
 yprocsOUT=1
 ny=64
 nyn=64
 yL=1.d0
 yLn=1.d0

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

  allocate( zc(1:nz+2), ze(1:nz+2), zcn(1:nzn+2), zen(1:nzn+2), zc_rho(1:nzn+2) )

  !DEFINE NEW GRIDS (course)
  do k=1,nzn+2
   zen(k) = -z0n + zLn/dble(nzn+2-1)*dble(k)
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

  do k=1,nzn+2
   zc_rho(k)  =  zcn(k) +1000.d0 
  enddo

!   do k=1,10
!   write(6,*) zc_rho(k)
!  enddo
!  do k=nzn-10,nzn+2
!   write(6,*) zc_rho(k)
!  enddo
! stop

! write(6,*) ze(1),zc(1),zen(1),zcn(1)
! write(6,*) ze(nz+2),zc(nz+2),zen(nzn+2),zcn(nzn+2)

 !LOOP OVER Y AND Z PROCS
!n2=2
!n1=0
 do n2 = 0,yprocs-1
  coords(2)=n2
  do n1 = 0,xprocs-1
   coords(1)=n1

   do n=0,zprocsIN-1

    fname=trim(baseName)
    call concati(fname,coords(1))
    call concat(fname,'_')
    call concati(fname,coords(2))
    call concat(fname,'_')
    call concati(fname,n)
    call concat(fname,'.')
    call concati(fname,ntime)

    !Direct Access read
    open(310,file=fname,form='unformatted',status='old',iostat=s1,action='read')
     read(310) nstep_IN,time_IN,delt_IN,g_IN,rho_0_IN,Re_IN,Pr_IN
     read(310) sx,ex,sy,ey,sz,ez 
     if (n.EQ.0) then
      allocate( u(sx-1:ex+1,sy-1:ey+1,1:nz+2) )
      allocate( v(sx-1:ex+1,sy-1:ey+1,1:nz+2) )
      allocate( w(sx-1:ex+1,sy-1:ey+1,1:nz+2) )
      allocate( p(sx-1:ex+1,sy-1:ey+1,1:nz+2) )
      allocate( rho(sx-1:ex+1,sy-1:ey+1,1:nz+2) )
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
 
    write(6,*) n,sz,ez,trim(fname)

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

   enddo


   if (smooth) then
    allocate( u1_tmp1(sx-1:ex+1,sy-1:ey+1,1:nz+2) )
  
    !1-2-1 smoothing in y
    u1_tmp1=0.d0
    do k=2,nz+1
     do j=sy,ey
      do i=sx,ex
       u1_tmp1(i,j,k)=ssum*( sm(-1)*u(i,j,k-1)+sm(0)*u(i,j,k)+sm(1)*u(i,j,k+1) )
      enddo
     enddo
    enddo
    u=u1_tmp1

    u1_tmp1=0.d0
    do k=2,nz+1
     do j=sy,ey
      do i=sx,ex
       u1_tmp1(i,j,k)=ssum*( sm(-1)*v(i,j,k-1)+sm(0)*v(i,j,k)+sm(1)*v(i,j,k+1) )
      enddo
     enddo
    enddo
    v=u1_tmp1

    u1_tmp1=0.d0
    do k=2,nz+1
     do j=sy,ey
      do i=sx,ex
       u1_tmp1(i,j,k)=ssum*( sm(-1)*w(i,j,k-1)+sm(0)*w(i,j,k)+sm(1)*w(i,j,k+1) )
      enddo
     enddo
    enddo
    w=u1_tmp1

    u1_tmp1=0.d0
    do k=2,nz+1
     do j=sy,ey
      do i=sx,ex
       u1_tmp1(i,j,k)=ssum*( sm(-1)*p(i,j,k-1)+sm(0)*p(i,j,k)+sm(1)*p(i,j,k+1) )
      enddo
     enddo
    enddo
    p=u1_tmp1

    u1_tmp1=0.d0
    do k=2,nz+1
     do j=sy,ey
      do i=sx,ex
       u1_tmp1(i,j,k)=ssum*( sm(-1)*rho(i,j,k-1)+sm(0)*rho(i,j,k)+sm(1)*rho(i,j,k+1) )
      enddo
     enddo
    enddo
    rho=u1_tmp1

    deallocate( u1_tmp1)
   endif !smooth

   allocate( uT(sx-1:ex+1,sy-1:ey+1,1:nzn+2) )
   allocate( vT(sx-1:ex+1,sy-1:ey+1,1:nzn+2) )
   allocate( wT(sx-1:ex+1,sy-1:ey+1,1:nzn+2) )
   allocate( pT(sx-1:ex+1,sy-1:ey+1,1:nzn+2) )
   allocate( rhoT(sx-1:ex+1,sy-1:ey+1,1:nzn+2) )
   
   if (n1.eq.n1c.and.n2.eq.n2c) then
    open(unit=100,file='zu.txt',form='formatted')
    do k=1,nz
     write(100,*) zc(k), u(sx+(ex-sx)/2,sy+(ey-sy)/2,k)
    enddo
    close(100)
   endif 
!   call write_vtk_field('test.vtk','u',nx+2,ny+2,nz+2,xe,yc,zc,u)

   if (padding) then
    !CHECK IF POINT IS OUTSIDE OLD DOMAIN
    !EDGE
    do k2=1,nzn
     if (zen(k2).LT.ze(1) ) kmin=k2
    enddo
    kmin=kmin+1
     do k2=nzn,1,-1
     if ( zen(k2).GT.ze(nz+1) ) kmax=k2
    enddo
    kmax=kmax-1
    wT  = 0.d0
    !PAD wT near zmin with w(:,:,2)*exp(-z^2)
     do k=2,kmin-1
      zdamp = zen(k)-ze(2)
      wT(:,:,k)  = w(:,:,2)*dexp(-zdamp**2)
     enddo
    
     !PAD wT near zmax with w(:,:,nz+1)*exp(-z^2)
     do k=kmax+1,nzn+1
      zdamp = zen(k)-ze(nz+1)
      wT(:,:,k)  = w(:,:,nz+1)*dexp(-zdamp**2)
     enddo
   else
    kmin=2
    kmax=nzn+2
    wT  = 0.d0
   endif 



   do k2=kmin+nr,kmax-nr
    !INDEPENDENT VARIABLE INTERPOLATION POINT
    xx = zen(k2)
    mindist=1e15
    !FIND n surrounding points
    do k = 1,nz
     if ( (ze(k)-xx)**2.LE.mindist ) then
      mindist=(ze(k)-xx)**2
      il = k-1
     endif
    enddo

    !CALCULATE dependent variable at interpolation point
    do j=sy,ey
     do i=sx,ex
      ind = ze(il-nr:il+nr)
      !W
      dep = w(i,j,il-nr:il+nr)
      wT(i,j,k2) = Neville(ind,dep,nI-1,xx)
     enddo
    enddo

   enddo
 

   !UPDATE NEAR Zmin
   do k2=kmin,kmin+nr-1
    xx = zen(k2)
    do j=sy,ey
     do i=sx,ex
      ind = ze(2:nI+1)
      dep = w(i,j,2:nI+1)
      wT(i,j,k2) = Neville(ind,dep,nI-1,xx)
     enddo
    enddo
   enddo
 
   !UPDATE NEAR Zmax
   do k2=kmax-nr+1,kmax
    xx = zen(k2)
    do j=sy,ey
     do i=sx,ex
      ind = ze(nz-nI+1:nz+1)
      dep = w(i,j,nz-nI+1:nz+1)
      wT(i,j,k2) = Neville(ind,dep,nI-1,xx)
     enddo
    enddo
   enddo

   if (padding) then
   !CENTER
    do k2=1,nzn
      if (zcn(k2).LT.zc(1) ) kmin=k2
    enddo
    kmin=kmin+1
    do k2=nzn,1,-1
     if ( zcn(k2).GT.zc(nz+1) ) kmax=k2
    enddo
    kmax=kmax-1
    uT  = 0.d0
    vT  = 0.d0
    pT  = 0.d0

    !PAD uT,vT,pT near zmin with uT,vT,pT(:,:,2)*exp(-z^2)
    do k=2,kmin-1
     zdamp = zcn(k)-zc(2)
     uT(:,:,k)  = u(:,:,2)*dexp(-zdamp**2)
     vT(:,:,k)  = v(:,:,2)*dexp(-zdamp**2)
     pT(:,:,k)  = p(:,:,2)*dexp(-zdamp**2)
    enddo

    !PAD uT,vT,pT near zmax with uT,vT,pT(:,:nz+1)*exp(-z^2)
    do k=kmax+1,nzn+1
     zdamp = zcn(k)-zc(nz+1)
     uT(:,:,k)  = u(:,:,nz+1)*dexp(-zdamp**2)
     vT(:,:,k)  = v(:,:,nz+1)*dexp(-zdamp**2)
     pT(:,:,k)  = p(:,:,nz+1)*dexp(-zdamp**2)
    enddo

    do k=1,nzn+2
     rhoT(:,:,k)  = rho_0+param1*( zc_rho(k) )  
    enddo

   else
    kmin=2
    kmax=nzn+2
    rhoT(:,:,:)  = 0.d0
    uT(:,:,:)    = 0.d0
    vT(:,:,:)    = 0.d0
    pT(:,:,:)    = 0.d0
   endif


   do k2=kmin+nr,kmax-nr
    !INDEPENDENT VARIABLE INTERPOLATION POINT
    xx = zcn(k2)
    mindist=1e15
    !FIND n surrounding points
    do k = 1,nz
     if ( (zc(k)-xx)**2.LE.mindist ) then
      mindist=(zc(k)-xx)**2
      il = k-1
     endif
    enddo

    !CALCULATE dependent variable at interpolation point
    do j=sy,ey
     do i=sx,ex
      ind = zc(il-nr:il+nr)
      !U
      dep = u(i,j,il-nr:il+nr)
      uT(i,j,k2) = Neville(ind,dep,nI-1,xx)
      !V
      dep = v(i,j,il-nr:il+nr)
      vT(i,j,k2) = Neville(ind,dep,nI-1,xx)
      !P
      dep = p(i,j,il-nr:il+nr)
      pT(i,j,k2) = Neville(ind,dep,nI-1,xx)
      !RHO
      dep = rho(i,j,il-nr:il+nr)
      rhoT(i,j,k2) = Neville(ind,dep,nI-1,xx)
      enddo
    enddo

   enddo
   !UPDATE NEAR Zmin
   do k2=kmin,kmin+nr-1
    xx = zcn(k2)
    do j=sy,ey
     do i=sx,ex
      ind = zc(2:nI+1)
      dep = u(i,j,2:nI+1)
      uT(i,j,k2) = Neville(ind,dep,nI-1,xx)
      dep = v(i,j,2:nI+1)
      vT(i,j,k2) = Neville(ind,dep,nI-1,xx)
      dep = p(i,j,2:nI+1)
      pT(i,j,k2) = Neville(ind,dep,nI-1,xx)
      dep = rho(i,j,2:nI+1)
      rhoT(i,j,k2) = Neville(ind,dep,nI-1,xx)
     enddo
    enddo
 
   enddo

   !UPDATE NEAR Zmax
   do k2=kmax-nr+1,kmax
    xx = zcn(k2)
    do j=sy,ey
     do i=sx,ex
      ind = zc(nz+1-nI:nz+1)
      dep = u(i,j,nz+1-nI:nz+1)
      uT(i,j,k2) = Neville(ind,dep,nI-1,xx)
      dep = v(i,j,nz+1-nI:nz+1)
      vT(i,j,k2) = Neville(ind,dep,nI-1,xx)
      dep = p(i,j,nz+1-nI:nz+1)
      pT(i,j,k2) = Neville(ind,dep,nI-1,xx)
      dep = rho(i,j,nz+1-nI:nz+1)
      rhoT(i,j,k2) = Neville(ind,dep,nI-1,xx)
     enddo
    enddo
   enddo

   if (n1.eq.n1c.and.n2.eq.n2c) then
    open(unit=100,file='z2.txt',form='formatted')
    do k=1,nzn
     write(100,*) zcn(k), uT(sx+(ex-sx)/2,sy+(ey-sy)/2,k)
    enddo
    close(100)
   endif
!   call write_vtk_field('testS.vtk','uOUT',nxn+2,nyn+2,nzn+2,xen,ycn,zcn,uT)

   deallocate(u,v,w,p,rho)


   do n=0,zprocsOUT-1
    fname=trim(outbaseName)
    call concati(fname,coords(1))
    call concat(fname,'_')
    call concati(fname,coords(2))
    call concat(fname,'_')
    call concati(fname,n)
    call concat(fname,'.')
    call concati(fname,ntime)

    call MPE_DECOMP1D(nzn,zprocsOUT,n,szn,ezn)
    szn=szn+1
    ezn=ezn+1
    write(6,*) n,szn,ezn,trim(fname)

    allocate( uOUT(sx-1:ex+1,sy-1:ey+1,szn-1:ezn+1) )
    allocate( vOUT(sx-1:ex+1,sy-1:ey+1,szn-1:ezn+1) )
    allocate( wOUT(sx-1:ex+1,sy-1:ey+1,szn-1:ezn+1) )
    allocate( pOUT(sx-1:ex+1,sy-1:ey+1,szn-1:ezn+1) )
    allocate( rhoOUT(sx-1:ex+1,sy-1:ey+1,szn-1:ezn+1) )
    uOUT=0.d0
    vOUT=0.d0
    wOUT=0.d0
    pOUT=0.d0
    rhoOUT=0.d0

    write(6,*) n,sz,ez,trim(fname)
   !Direct Access write 
    open(310,file=fname,form='unformatted',status='new',iostat=s1,action='write')
     write(310) nstep_IN,time_IN,delt_IN,g_IN,rho_0_IN,Re_IN,Pr_IN
     write(310) sx,ex,sy,ey,szn,ezn 
 
     do k=szn,ezn
      do j=sy,ey
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
end program InterpX3 


subroutine MPE_DECOMP1D(n,numprocs,myid,s,e)
!@t
! \textbf{subroutine MPE\_DECOMP1D(n,numprocs,myid,s,e)}
!@h
!   Description:
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

!   Language:
!     Fortran 90
!@q

 implicit none

!Passed Variables
 integer :: nlocal
 integer :: deficit
 integer :: n, numprocs, myid, s, e

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


