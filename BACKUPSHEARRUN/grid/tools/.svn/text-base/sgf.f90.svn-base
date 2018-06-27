program sf
 use BTEC
 use BVTK
 implicit none

 integer                         :: kp,km,km2, ip,im,im2, jp,jm,jm2,idir
 real                            :: dxc, dyc, dzc
 real                            :: LxC, Lxcm, Lxcm2 
 real                            :: LyC, Lycm, Lycm2 
 real                            :: LzC, Lzcm, Lzcm2 
 real                            :: dzNorth, dzSouth, dyFront, dyBack, dxEast, dxWest
 real,allocatable,dimension(:)   :: p1,p2,p3,p4,p5,p6,xE,yE,zE
 real,allocatable,dimension(:,:) :: tg1, tg2, tfac 
 real                            :: a1,a2,a3,a4,da,amin
 character(len=100)              :: title,vars,tecfilename,tectempdir,fname(1:3),vtkfilename
 integer                         :: ts1, ts2, ts3, s1, idum
 integer                         :: nx,ny,nz, i,j,k, iin,jin,kin, iLocAmax(1:3)
 real(r8)                        :: amax, a 

 logical,parameter               :: debug=.false.
 
 if (debug) write(6,*) ' nx,ny,nz '
 read(5,*) nx,ny,nz 
 if (debug) write(6,*) nx,ny,nz

 if (debug) write(6,*) ' Location of Planes ix,iy,iz '
 read(5,*) iin,jin,kin
 if (debug) write(6,*) nx,ny,nz


 allocate(xE(-1:nx+3) )
 allocate(yE(-1:ny+3) ) 
 allocate(zE(-1:nz+3) )

 do idir = 1,3 ! x,y and z
  if (debug) write(6,*) ' Names of x1grid x2grid x3grid'
  READ (5,*) fname(idir)
  if (debug) write(6,*) fname(idir)
 enddo
 
 open(unit=10,file=fname(1),status='old',iostat=s1)
 open(unit=11,file=fname(2),status='old',iostat=s1)
 open(unit=12,file=fname(3),status='old',iostat=s1)
  if (s1.NE.0) then
   write(6,'(a)') "ERROR CANNOT OPEN GRID FILES BP#1" 
   stop
  endif


 !READ EDGE GRIDS
 do i = -1, nx+3
  read(10,*) idum,xE(i)
 enddo
 do j = -1, ny+3
  read(11,*)  idum,yE(j)
  enddo
 do k = -1, nz+3
  read(12,*)  idum,zE(k)
 enddo


 allocate( p1(1:nx+1), stat=s1 )
 allocate( p2(1:nx+1), stat=s1 )
 allocate( p3(1:ny+1), stat=s1 )
 allocate( p4(1:ny+1), stat=s1 )
 allocate( p5(1:nz+1), stat=s1 )
 allocate( p6(1:nz+1), stat=s1 )
  if (s1.NE.0) then
   write(6,'(a)') "ERROR CANNOT ALLOCATE" 
   stop
  endif

 !X1-direction
 do i=1,nx+1 
  ip=i+1
  im=i-1
  im2=i-2

  dxEast =  ( xE(ip) - xE(im) )/2.d0 !dxe(i)
  dxWest =  ( xE(i) - xE(im2) )/2.d0 !dxe(i-1)
  dxc    = xE(i)-xE(im) 

  p1(i)=1.0d0/(dxc*dxWest)   
  p2(i)=1.0d0/(dxc*dxEast)
 enddo

!X2-direction
 do j=1,ny+1
  jp=j+1
  jm=j-1
  jm2=j-2

  dyFront= ( yE(jp) - yE(jm) )/2.d0 !dye(j)
  dyBack=  ( yE(j) - yE(jm2) )/2.d0 !dye(j-1)
  dyc=     yE(j)-yE(jm)

  p3(j)=1.0d0/(dyc*dyBack)
  p4(j)=1.0d0/(dyc*dyFront)
 enddo

!X3-direction
 do k=1,nz+1
  kp=k+1
  km=k-1
  km2=k-2

  dzNorth= ( zE(kp) - zE(km) )/2.d0 !dze(k)
  dzSouth= ( zE(k) - zE(km2) )/2.d0 !dze(k-1)

  dzc= zE(k)-zE(km)

  p5(k)=1.0d0/(dzc*dzSouth)
  p6(k)=1.0d0/(dzc*dzNorth)
 enddo

 amax=0.d0
 iLocAmax(1:3)=-99

 do k = 1,nz+1 
  do j= 1,ny+1
   do i = 1,nx+1
    a1        = dsqrt( (p1(i)**2 + p2(i)**2)/2.d0 )
    a2        = dsqrt( (p3(j)**2 + p4(j)**2)/2.d0 )
    a3        = dsqrt( (p5(k)**2 + p6(k)**2)/2.d0 )
    da        = a1 + a2 + a3
    a1        = a1/da
    a2        = a2/da
    a3        = a3/da
    amin      = min(a1,a2,a3)
    a         =  (1.d0-amin)**2
    if (a.GT.amax) then
     amax=a
     iLocAmax(1) = i
     iLocAmax(2) = j
     iLocAmax(3) = k
    endif
   enddo
  enddo
 enddo
 write(6,*) "Maximum: ",amax
 write(6,'(a22,3(1x,i4),3(1x,f15.6))') "At [i,j,k]/[x1,x2,x3]: ",iLocAmax,xE(iLocAmax(1)),yE(iLocAmax(2)),zE(iLocAmax(3)) 

!X1-X3 PLANE @ x2=yE(jin)
 allocate( tg1(1:nx+1,1:nz+1), stat=s1 )
 allocate( tg2(1:nx+1,1:nz+1), stat=s1 )
 allocate( tfac(1:nx+1,1:nz+1), stat=s1 )

 write(title,'(a,f15.8)') 'X1-X3 PLANE at x2=',yE(jin)
 vars='x1 x3 SF'
 tecfilename='./SFx1x3.plt'
 vtkfilename='./SFx1x3.vtk'
 tectempdir='.'
 ts1=nx+1
 ts2=nz+1
 ts3=1

j=jin 
do k = 1,nz+1 
 do i = 1,nx+1
   tg1(i,k)   = xE(i)
   tg2(i,k)   = zE(k)
   a1        = dsqrt( (p1(i)**2 + p2(i)**2)/2.d0 )
   a2        = dsqrt( (p3(j)**2 + p4(j)**2)/2.d0 )
   a3        = dsqrt( (p5(k)**2 + p6(k)**2)/2.d0 )
   da        = a1 + a2 + a3
   a1        = a1/da
   a2        = a2/da
   a3        = a3/da
   amin      = min(a1,a2,a3)
   tfac(i,k) =  (1.d0-amin)**2
  enddo
 enddo

 
 call open_tec_binary(1.d0,ts1,ts2,ts3,title,vars,tecfilename,tectempdir,tec_debug,VIsDouble)
 call write_tec_binary(ts1,ts2,ts3,tg1)
 call write_tec_binary(ts1,ts2,ts3,tg2)
 call write_tec_binary(ts1,ts2,ts3,tfac)
 call close_tec_binary

 call write_vtk(vtkfilename,title,nx+1,1,nz+1,xE(1:nx+1),yE(jin),zE(1:nz+1),tfac(:,:))

 deallocate( tg1, stat=s1 )
 deallocate( tg2, stat=s1 )
 deallocate( tfac, stat=s1 )
 if (s1.NE.0) then
  write(6,'(a)') "ERROR CANNOT DEALLOCATE" 
  stop
 endif

!X2-X3 PLANE @ x=xE(iin)
 allocate( tg1(1:ny+1,1:nz+1), stat=s1 )
 allocate( tg2(1:ny+1,1:nz+1), stat=s1 )
 allocate( tfac(1:ny+1,1:nz+1), stat=s1 )

 write(title,'(a,f15.8)') 'X2-X3 PLANE at x1=',xE(iin)
 vars='x2 x3 SF'
 tecfilename='./SFx2x3.plt'
 vtkfilename='./SFx2x3.vtk'
 tectempdir='.'
 ts1=ny+1
 ts2=nz+1
 ts3=1

 i=iin 
 do k = 1,nz+1
  do j = 1,ny+1
   tg1(j,k)   = yE(j)
   tg2(j,k)   = zE(k)
   a1        = dsqrt( (p1(i)**2 + p2(i)**2)/2.d0 )
   a2        = dsqrt( (p3(j)**2 + p4(j)**2)/2.d0 )
   a3        = dsqrt( (p5(k)**2 + p6(k)**2)/2.d0 )
   da        = a1 + a2 + a3
   a1        = a1/da
   a2        = a2/da
   a3        = a3/da
   amin      = min(a1,a2,a3)
   tfac(j,k) =  (1.d0-amin)**2
  enddo
 enddo

 call open_tec_binary(1.d0,ts1,ts2,ts3,title,vars,tecfilename,tectempdir,tec_debug,VIsDouble)
 call write_tec_binary(ts1,ts2,ts3,tg1)
 call write_tec_binary(ts1,ts2,ts3,tg2)
 call write_tec_binary(ts1,ts2,ts3,tfac)
 call close_tec_binary
 call write_vtk(vtkfilename,title,1,ny+1,nz+1,xE(iin),yE(1:ny+1),zE(1:nz+1),tfac(:,:))

 deallocate( tg1, stat=s1 )
 deallocate( tg2, stat=s1 )
 deallocate( tfac, stat=s1 )
 if (s1.NE.0) then
  write(6,'(a)') "ERROR CANNOT DEALLOCATE" 
  stop
 endif

!X1-X2 PLANE @ x=xE(iin)
 allocate( tg1(1:nx+1,1:ny+1), stat=s1 )
 allocate( tg2(1:nx+1,1:ny+1), stat=s1 )
 allocate( tfac(1:nx+1,1:ny+1), stat=s1 )

 write(title,'(a,f15.8)') 'X1-X2 PLANE at x3=',zE(kin)
 vars='x1 x2 SF'
 tecfilename='./SFx1x2.plt'
 vtkfilename='./SFx1x2.vtk'
 tectempdir='.'
 ts1=nx+1
 ts2=ny+1
 ts3=1
 
 k=kin 
 do j = 1,ny+1 
  do i = 1,nx+1
   tg1(i,j)   = xE(i)
   tg2(i,j)   = yE(j)
   a1        = dsqrt( (p1(i)**2 + p2(i)**2)/2.d0 )
   a2        = dsqrt( (p3(j)**2 + p4(j)**2)/2.d0 )
   a3        = dsqrt( (p5(k)**2 + p6(k)**2)/2.d0 )
   da        = a1 + a2 + a3
   a1        = a1/da
   a2        = a2/da
   a3        = a3/da
   amin      = min(a1,a2,a3)
   tfac(i,j) =  (1.d0-amin)**2
  enddo
 enddo

 call open_tec_binary(1.d0,ts1,ts2,ts3,title,vars,tecfilename,tectempdir,tec_debug,VIsDouble)
 call write_tec_binary(ts1,ts2,ts3,tg1)
 call write_tec_binary(ts1,ts2,ts3,tg2)
 call write_tec_binary(ts1,ts2,ts3,tfac)
 call close_tec_binary
 
 call write_vtk(vtkfilename,title,nx+1,ny+1,1,xE(1:nx+1),yE(1:ny+1),zE(kin),tfac(:,:))

 deallocate( tg1, stat=s1 )
 deallocate( tg2, stat=s1 )
 deallocate( tfac, stat=s1 )
 deallocate( p1,p2,p3,p4,p5,p6, stat=s1 )
 if (s1.NE.0) then
  write(6,'(a)') "ERROR CANNOT DEALLOCATE" 
  stop
 endif
 stop
end program sf
