Program Grid_generation
!@t
! \textbf{Program Grid\_generation}
!@h
!   Description:
!     Used to generate the x1_grid.in x2_grid.in and x3_grid.in file for
!     use with Sturb.x.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       04/2009  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
!@h
!   Comments:
!     Grid settings are read from gridgen.ini. This is a serial program. 
!     Support is provided for uniform grids and hyperbolic sine stretching
!     but not percent stretching at this time. The start and end points are
!     defined by the center grid, not the edge one.
!@q

 use ntypes
 implicit none

 !Variables
 integer,parameter           :: IOUT=6
 character(len=25),parameter :: inFILE='gridgen.ini'
 integer                     :: i,j,k
 integer                     :: nx,ny,nz,nxp2,nyp2,nzp2
 integer                     :: CenterLine, kts1, kts2, kls1, kls2
 real(r8)                    :: xL, yL, zL
 real(r8)                    :: x0, y0, z0
 character(len=25)           :: Gridtype(1:3),locX0,locY0,locZ0

 real(r8),allocatable,dimension(:),target :: xe,xc,dxe,dxc
 real(r8),allocatable,dimension(:),target :: ye,yc,dye,dyc
 real(r8),allocatable,dimension(:),target :: ze,zc,dze,dzc
 real(r8),allocatable,dimension(:),target :: rdxe,rdxc
 real(r8),allocatable,dimension(:),target :: rdye,rdyc
 real(r8),allocatable,dimension(:),target :: rdze,rdzc
 character(len=5),parameter  :: defval='ERROR'
 character(len=100)                       :: gridfile, block

 ! Percent stretching variables
 real(r8)                    :: rx, ry, rz
 real(r8)                    :: mindx, mindy, mindz
 real(r8)                    :: xstretchfactor, ystretchfactor, zstretchfactor
 integer                     :: boxflag

 ! Local Variables
 integer :: s1,stat

![Domain]
 block='Domain'
 call scaniniint(infile,block,'NX',nx,defval,IOUT)
 call scaniniint(infile,block,'NY',ny,defval,IOUT)
 call scaniniint(infile,block,'NZ',nz,defval,IOUT)
 nxp2=nx+2
 nyp2=ny+2
 nzp2=nz+2


!GRID ALLOCATION
 !X1-Direction
 allocate( xe(1:nxp2), stat=s1 )
 allocate( xc(1:nxp2), stat=s1 )
 allocate( dxe(1:nxp2), stat=s1 )
 allocate( dxc(1:nxp2), stat=s1 )
 allocate( rdxe(1:nxp2), stat=s1 )
 allocate( rdxc(1:nxp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating X1 Grid Variables"
  goto 1000
 endif

 !X2-Direction
 allocate( ye(1:nyp2), stat=s1 )
 allocate( yc(1:nyp2), stat=s1 )
 allocate( dye(1:nyp2), stat=s1 )
 allocate( dyc(1:nyp2), stat=s1 )
 allocate( rdye(1:nyp2), stat=s1 )
 allocate( rdyc(1:nyp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating X2 Grid Variables"
  goto 1000
 endif

 !X3-Direction
 allocate( ze(1:nzp2), stat=s1 )
 allocate( zc(1:nzp2), stat=s1 )
 allocate( dze(1:nzp2), stat=s1 )
 allocate( dzc(1:nzp2), stat=s1 )
 allocate( rdze(1:nzp2), stat=s1 )
 allocate( rdzc(1:nzp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating X3 Grid Variables"
  goto 1000
 endif

![Grid]
 block='Grid' 
 call scaninireal(infile,block,'Lx',xL,defval,IOUT)
 call scaninireal(infile,block,'Ly',yL,defval,IOUT)
 call scaninireal(infile,block,'Lz',zL,defval,IOUT)
 call scaninichar(infile,block,'x0',locx0,defval,IOUT)
 call scaninichar(infile,block,'y0',locy0,defval,IOUT)
 call scaninichar(infile,block,'z0',locz0,defval,IOUT)
 call scaninichar(infile,block,'XGridtype',Gridtype(1),defval,IOUT)
 call scaninichar(infile,block,'YGridtype',Gridtype(2),defval,IOUT)
 call scaninichar(infile,block,'ZGridtype',Gridtype(3),defval,IOUT)

![PercentStretching]
 block='PercentStretching' 
 call scaninireal(infile,block,'rx',rx,defval,IOUT)
 call scaninireal(infile,block,'ry',ry,defval,IOUT)
 call scaninireal(infile,block,'rz',rz,defval,IOUT)
 call scaninireal(infile,block,'mindx',mindx,defval,IOUT)
 call scaninireal(infile,block,'mindy',mindy,defval,IOUT)
 call scaninireal(infile,block,'mindz',mindz,defval,IOUT)
 call scaninireal(infile,block,'percentstretchinginx',xstretchfactor,defval,IOUT)
 call scaninireal(infile,block,'percentstretchinginy',ystretchfactor,defval,IOUT)
 call scaninireal(infile,block,'percentstretchinginz',zstretchfactor,defval,IOUT)

!**********************************************************************
!****************************GRID ORIGIN*******************************
!**********************************************************************
 !X0
 select case(locX0)
  case('start')
   x0 = 0.d0 !+xL/dble(nxp2-1)
  case('end')
   x0 = -( xL)!+xL/dble(nxp2-1) )
  case('center')
   x0 = -xL/2.d0 !+xL/dble(nxp2-1)
  case DEFAULT
   goto 1000
 end select
 !Y0
 select case(locY0)
  case('start')
   y0 = 0.d0!+yL/dble(nyp2-1)
  case('end')
   y0 = -( yL) !+yL/dble(nyp2-1) )
  case('center')
   y0 = -yL/2.d0 !+yL/dble(nyp2-1)
  case DEFAULT
   goto 1000
 end select
 !Z0
 select case(locZ0)
  case('start')
   z0 = 0.d0!+zL/dble(nzp2-1)
  case('end')
   z0 = -( zL) !+zL/dble(nzp2-1) )
  case('center')
   z0 = -zL/2.d0 !+zL/dble(nzp2-1)
  case('custom')
   z0 = 0.d0
   case DEFAULT
   goto 1000
 end select

!**********************************************************************
!*******************************X1 GRID********************************
!**********************************************************************
  select case(GridType(1))
   case('uniform')
    do i=1,nxp2
 !    xe(i) = x0 + xL/dble(nxp2-1)*dble(i) - 1.d0*xL/dble(nxp2-1)
     xe(i) = x0 + xL/dble(nxp2-1)*dble(i) - 0.5d0*xL/dble(nxp2-1)
    enddo
   case('stretch1')
    do i=1,nxp2
     xe(i) = x0 + xL/dble(nxp2-1)*dsinh( 1.75d0*( dble(i-1)/(dble(nxp2-1)/2.d0)-1.d0) )
    enddo
     x0 = ( xe(1)+xe(nxp2) )/2.d0
     xe(:) = xe(:)-x0
   case('percentstretch')
     boxflag = 0;
     xe(nxp2/2+1) = mindx/2.0d0
     xe(nxp2/2) = -mindx/2.0d0
     do i = 1,nxp2/2-1
       if (boxflag==1) then 
       ! STRETCHED TOWARDS THE BOUNDARIES
         xe(nxp2/2+i+1)  = xe(nxp2/2+i)+( xe(nxp2/2+i)-xe(nxp2/2+i-1) )*xstretchfactor
         xe(nxp2/2-i)    = xe(nxp2/2-i+1)-( xe(nxp2/2-i+2)-xe(nxp2/2-i+1) )*xstretchfactor
       endif
       if (boxflag==0) then
       ! UNSTRETCHED CENTER REGION
         xe(nxp2/2+i+1) = xe(nxp2/2+i) + mindx
         xe(nxp2/2-i)   = xe(nxp2/2-i+1) - mindx
         if (xe(nxp2/2+i+1) > rx) then
           boxflag = 1;
         endif
       endif
     enddo
   case('custom')
    write(6,*)' I am in custom x'
    CenterLine = 3072 
    dxe(CenterLine) = 0.25d0

    kts1=3073
    kts2=nxp2
    do i = kts1,kts2
     dxe(i) = dxe(i-1) * 1.0d0
    enddo


    kts1=3071
    kts2=1536
    do i = kts1,kts2,-1
     dxe(i) = dxe(i+1)*1.d0
    enddo
    
    kts1=1535
    kts2=1435
    do i = kts1,kts2,-1
     dxe(i) = dxe(i+1) * (1 + ( dtanh( 4.d0* (i-(kts1+kts2)/2.d0) / ( (kts2-kts1)/2.d0 ) ) *.001d0+ .001d0 ))
    enddo
    
    kts1=1434
    kts2=101
        do i = kts1,kts2,-1
     dxe(i) = dxe(i+1)*1.002d0
    enddo
    
    kts1=100
    kts2=5
    do i = kts1,kts2,-1
     dxe(i) = dxe(i+1) * (1 + ( dtanh( 4.d0* (i-(kts1+kts2)/2.d0) / ( (kts1-kts2)/2.d0 ) ) *.001d0+ .001d0 ))
    enddo

    kts1=4
    kts2=1
    do i = kts1,kts2,-1
     dxe(i) = dxe(i+1)*1.0d0
    enddo


    xe(CenterLine) = 0.0d0 
   do i = CenterLine+1,nxp2
    xe(i) = xe(i-1) + dxe(i)
   enddo
   do i= CenterLine-1,1,-1
    xe(i) = xe(i+1) - dxe(i+1)
   enddo
    xL = xe(nxp2)-xe(1)
   case DEFAULT

   goto 1000
  end select

 !COMPUTE xc, dxe and dxc, rdxe, rdxc
 !xc(i)= ( xe(i)+xe(i-1) )/2.d0 !BY DEFINITION
 !dxe=xc(i+1)-xc(i)             !BY DEFINITION
 !dxc=xe(i)-xe(i-1)             !BY DEFINITION

 do i=2,nxp2
  xc(i)  = ( xe(i)+xe(i-1) )/2.d0
  dxc(i) = xe(i)-xe(i-1)
  rdxc(i) = 1.d0/dxc(i)
 enddo
! assign values for the first grid cell
 select case(GridType(1))
   case('uniform','stretchi1','custom')
     dxc(1)=dxc(2)
     rdxc(1)=rdxc(2)
     xc(1) = xe(1)-0.5d0*dxc(1) 
   case('percentstretch')
     dxc(1)=dxc(2)*xstretchfactor
     rdxc(1)= 1.d0/dxc(1)
     xc(1) = xe(1)-0.5d0*dxc(1) 
   case DEFAULT
     goto 1000
 end select

 do i=1,nxp2-1
  dxe(i) = xc(i+1)-xc(i)
  rdxe(i) = 1.d0/dxe(i)
 enddo
! assign values for the last grid cell
 select case(GridType(1))
   case('uniform','stretch1','custom')
     dxe(nxp2) = dxe(nxp2-1)
     rdxe(nxp2) = rdxe(nxp2-1)
   case('percentstretch')
     dxe(nxp2) = dxe(nxp2-1)*xstretchfactor
     rdxe(nxp2) = 1.d0/dxc(nxp2)
   case DEFAULT
     goto 1000
 end select
 write(6,*)'I am done with x'


!**********************************************************************
!*******************************X2 GRID********************************
!**********************************************************************
  select case(GridType(2))
   case('uniform')
    do j=1,nyp2
 !    ye(j) = y0 + yL/dble(nyp2-1)*dble(j) - 1.d0*yL/dble(nyp2-1)
     ye(j) = y0 + yL/dble(nyp2-1)*dble(j) - 0.5d0*yL/dble(nyp2-1)
    enddo
   case('stretch1')
    do j=1,nyp2
     ye(j) = y0 + yL/(2.d0)*dsinh( 1.75d0*( dble(j-1)/(dble(nyp2-1)/2.d0)-1.d0) )
    enddo
     y0 = ( ye(1)+ye(nyp2) )/2.d0
     ye(:) = ye(:)-y0
   case('percentstretch')
     boxflag = 0;
     ye(nyp2/2+1) = mindy/2.0d0
     ye(nyp2/2) = -mindy/2.0d0
     do i = 1,nyp2/2-1
       if (boxflag==1) then
       ! STRETCHED TOWARDS THE BOUNDARIES
         ye(nyp2/2+i+1)  = ye(nyp2/2+i)+( ye(nyp2/2+i)-ye(nyp2/2+i-1) )*ystretchfactor
         ye(nyp2/2-i)    = ye(nyp2/2-i+1)-( ye(nyp2/2-i+2)-ye(nyp2/2-i+1) )*ystretchfactor
       endif
       if (boxflag==0) then
       ! UNSTRETCHED CENTER REGION
         ye(nyp2/2+i+1) = ye(nyp2/2+i) + mindy
         ye(nyp2/2-i)   = ye(nyp2/2-i+1) - mindy
         if (ye(nyp2/2+i+1) > ry) then
           boxflag = 1;
         endif
       endif
     enddo
    case DEFAULT
    goto 1000
   end select


 !COMPUTE yc, dye and dyc, rdye, rdyc
 !yc(i)= ( ye(i)+ye(i-1) )/2.d0 !BY DEFINITION
 !dye=yc(i+1)-yc(i)             !BY DEFINITION
 !dyc=ye(i)-ye(i-1)             !BY DEFINITION

 do j=2,nyp2
  yc(j) = ( ye(j)+ye(j-1) )/2.d0
  dyc(j) = ye(j)-ye(j-1)
  rdyc(j) = 1.d0/dyc(j)
 enddo


! assign values for the first grid cell
 select case(GridType(2))
   case('uniform','stretch1')
     dyc(1)=dyc(2)
     rdyc(1)=rdyc(2)
     yc(1) = ye(1)-0.5d0*dyc(1)
   case('percentstretch')
     dyc(1)=dyc(2)*ystretchfactor
     rdyc(1)= 1.d0/dyc(1)
     yc(1) = ye(1)-0.5d0*dyc(1)
   case DEFAULT
     goto 1000
 end select


 do j=1,nyp2-1
  dye(j) = yc(j+1)-yc(j)
  rdye(j) = 1.d0/dye(j)
 enddo
! assign values for the last grid cell
 select case(GridType(2))
   case('uniform','stretch1')
     dye(nyp2) = dye(nyp2-1)
     rdye(nyp2) = rdye(nyp2-1)
   case('percentstretch')
     dye(nyp2) = dye(nyp2-1)*ystretchfactor
     rdye(nyp2) = 1.d0/dyc(nyp2)
   case DEFAULT
     goto 1000
 end select


 !Ensure No Stretching at First and Last Cells in X2
  dyc(nyp2) = dyc(nyp2-1)
  rdyc(nyp2) = rdyc(nyp2-1)
  dye(1)=dye(2)
  rdye(1)=rdye(2)
  ye(nyp2)=ye(nyp2-1)+dyc(nyp2)
  ye(1)=ye(2)-dyc(2)
  yc(nyp2)=yc(nyp2-1)+dye(nyp2-1)
  yc(1)=yc(2)-dye(1)

 write(6,*)'I am done with y'
!**********************************************************************
!*******************************X3 GRID********************************
!**********************************************************************
  select case(GridType(3))
   case('uniform')
    do k=1,nzp2
!    ze(k) = z0 + zL/dble(nzp2-1)*dble(k) - 1.d0*zL/dble(nzp2-1)
     ze(k) = z0 + zL/dble(nzp2-1)*dble(k) - 0.5d0*zL/dble(nzp2-1)
    enddo
   case('stretch1')
    do k=1,nzp2
! KYLE IS THE 1.25 IN THIS LINE A TYPO or is the 1.75???
     ze(k) = zL/dble(2.d0)*dsinh( 1.25d0*( dble(k-1)/(dble(nzp2-1)/2.d0)-1.d0) )
    enddo
     z0 = ( ze(1)+ze(nzp2) )/2.d0
     ze(:) = ze(:)-z0
   case('percentstretch')
     boxflag = 0;
     ze(nzp2/2+1) = mindz/2.0d0
     ze(nzp2/2) = -mindz/2.0d0
     do i = 1,nzp2/2-1
       if (boxflag==1) then
       ! STRETCHED TOWARDS THE BOUNDARIES
         ze(nzp2/2+i+1)  = ze(nzp2/2+i)+( ze(nzp2/2+i)-ze(nzp2/2+i-1) )*zstretchfactor
         ze(nzp2/2-i)    = ze(nzp2/2-i+1)-( ze(nzp2/2-i+2)-ze(nzp2/2-i+1) )*zstretchfactor
       endif
       if (boxflag==0) then
       ! UNSTRETCHED CENTER REGION
         ze(nzp2/2+i+1) = ze(nzp2/2+i) + mindz
         ze(nzp2/2-i)   = ze(nzp2/2-i+1) - mindz
         if (ze(nzp2/2+i+1) > rz) then
           boxflag = 1;
         endif
       endif
     enddo
   case('custom')
    CenterLine = 193
    dze(CenterLine) = 1.d0/80.0d0
    kts1=194
    kts2=nzp2
    do k = kts1,kts2
     dze(k) = dze(k-1)*1.0d0
    enddo
    
    kts1=192
    kts2=191
    do k = kts1,kts2,-1
     dze(k) = dze(k+1) * 1.0d0
    enddo
    kts1=190
    kts2=161
    do k = kts1,kts2,-1
     dze(k) = dze(k+1) * (1 +  ( dtanh( 4.d0* (k-(kts1+kts2)/2.d0) / ( (kts2-kts1)/2.d0 ) ) *.015d0+.015d0 ))
    enddo
    kts1=160
    kts2=131
    do k = kts1,kts2,-1
     dze(k) = dze(k+1) * 1.03d0
    enddo
    kts1=130
    kts2=101
    do k = kts1,kts2,-1
     dze(k) = dze(k+1) * (1.03 -  ( dtanh( 4.d0* (k-(kts1+kts2)/2.d0) / ( (kts2-kts1)/2.d0 ) ) *.015d0+.015d0 ))
    enddo
    kts1=100
    kts2=61
    do k = kts1,kts2,-1
     dze(k) = dze(k+1) * 1.0d0
    enddo
    kts1=60
    kts2=31
    do k = kts1,kts2,-1
     dze(k) = dze(k+1) * (1 +  ( dtanh( 4.d0* (k-(kts1+kts2)/2.d0) / ( (kts2-kts1)/2.d0 ) ) *.015d0+.015d0 ))
    enddo
    kts1=30
    kts2=1
    do k = kts1,kts2,-1
     dze(k) = dze(k+1) * 1.03d0
    enddo

   ze(CenterLine) = 0.0d0 
   do k = CenterLine,nzp2
    ze(k) = ze(k-1) + dze(k)
   enddo
   do k= CenterLine-1,1,-1
    ze(k) = ze(k+1) - dze(k+1)
   enddo
    zL = ze(nzp2)-ze(1)
   case DEFAULT
   goto 1000
  end select

 !COMPUTE zc, dze and dzc, rdze, rdzc
 !zc(i)= ( ze(i)+ze(i-1) )/2.d0 !BY DEFINITION
 !dze=zc(i+1)-zc(i)             !BY DEFINITION
 !dzc=ze(i)-ze(i-1)             !BY DEFINITION

  
 do k=2,nzp2
  zc(k) = ( ze(k)+ze(k-1) )/2.d0
  dzc(k) = ze(k)-ze(k-1)
  rdzc(k) = 1.d0/dzc(k)
 enddo
! assign values for the first grid cell
 select case(GridType(3))
   case('uniform','stretch1','custom')
     dzc(1)=dzc(2)
     rdzc(1)=rdzc(2)
     zc(1) = ze(1)-0.5d0*dzc(1)
   case('percentstretch')
     dzc(1)=dzc(2)*zstretchfactor
     rdzc(1)= 1.d0/dzc(1)
     zc(1) = ze(1)-0.5d0*dzc(1) 
   case DEFAULT
     goto 1000
 end select

 do k=1,nzp2-1
  dze(k) = zc(k+1)-zc(k)
  rdze(k) = 1.d0/dze(k)
 enddo
! assign values for the last grid cell
 select case(GridType(3))
   case('uniform','stretch1','custom')
     dze(nzp2) = dze(nzp2-1)
     rdze(nzp2) = rdze(nzp2-1)
   case('percentstretch')
     dze(nzp2) = dze(nzp2-1)*zstretchfactor
     rdze(nzp2) = 1.d0/dzc(nzp2)
   case DEFAULT
     goto 1000
 end select

 !Ensure No Stretching at First and Last Cells in X3
  dzc(nzp2) = dzc(nzp2-1)
  rdzc(nzp2) = rdzc(nzp2-1)
  dze(1)=dze(2)
  rdze(1)=rdze(2)
  ze(nzp2)=ze(nzp2-1)+dzc(nzp2)
  ze(1)=ze(2)-dzc(2)
  zc(nzp2)=zc(nzp2-1)+dze(nzp2-1)
  zc(1)=zc(2)-dze(1)

 write(6,*)'I am done with z'


 !WRITE OUT GRID
  !X1-Grid
  write(gridfile,'(a)') 'x1_grid.in'
  open(unit=207,file=gridfile,status='unknown',form='formatted',iostat=s1)
  if (s1.NE.0) then
   close(207)
   write(IOUT,'(a,i4)') "ERROR Opening File: "//trim(gridfile)//" IOSTAT=",s1
   stat=1
   goto 1000
  endif
   write(207,'(a)') "#X1-GRID"
   write(207,'(a)') "#i xe(i) xc(i) dxc(i) dxc(i) ratio"
   write(207,'(i6)') nxp2
   write(207,'(f22.15)') xL
   write(207,'(f22.15)') x0
   write(207,*) Gridtype(1)
   do i=1,nxp2
    write(207,'(i5,4(2x,d15.8),2x,f8.6)') i,xe(i),xc(i),dxe(i),dxc(i),dxc(i)/dxc(i-1)
   enddo
  close(207)

  !X2-Grid
  write(gridfile,'(a)') 'x2_grid.in'
  open(unit=206,file=gridfile,status='unknown',form='formatted',iostat=s1)
  if (s1.NE.0) then
   close(206)
   write(IOUT,'(a,i4)') "ERROR Opening File: "//trim(gridfile)//" IOSTAT=",s1
   stat=2
   goto 1000
  endif
   write(206,'(a)') "#X2-GRID"
   write(206,'(a)') "#j ye(j) yc(j) dyc(j) dyc(j) ratio"
   write(206,'(i6)') nyp2
   write(206,'(f22.15)') yL
   write(206,'(f22.15)') y0
   write(206,*) Gridtype(2)
   do j=1,nyp2
    write(206,'(i5,4(2x,d15.8),2x,f8.6)') j,ye(j),yc(j),dye(j),dyc(j),dyc(j)/dyc(j-1)
   enddo
  close(206)
  
  !X3-Grid
  write(gridfile,'(a)') 'x3_grid.in'
  open(unit=205,file=gridfile,status='unknown',form='formatted',iostat=s1)
  if (s1.NE.0) then
   close(205)
   write(IOUT,'(a,i4)') "ERROR Opening File: "//trim(gridfile)//" IOSTAT=",s1
   stat=3
   goto 1000
  endif
   write(205,'(a)') "#X3-GRID"
   write(205,'(a)') "#k ze(k) zc(k) dzc(k) dzc(k) ratio"
   write(205,'(i6)') nzp2
   write(205,'(f22.15)') zL
   write(205,'(f22.15)') z0
   write(205,*) Gridtype(3)
   select case(GridType(3))
    case('uniform','stretch1')
     do k=1,nzp2
      write(205, '(i5,4(2x,d15.8),2x,f8.6)') k,ze(k),zc(k),dze(k),dzc(k),dzc(k-1)/dzc(k)
     enddo
    case('custom')
     do k=1,CenterLine
      write(205, '(i5,4(2x,d15.8),2x,f8.6)') k,ze(k),zc(k),dze(k),dzc(k),dzc(k)/dzc(k+1)
     enddo
     do k=CenterLine+1,nzp2
      write(205, '(i5,4(2x,d15.8),2x,f8.6)') k,ze(k),zc(k),dze(k),dzc(k),dzc(k)/dzc(k-1)
    enddo
   case DEFAULT
     goto 1000
   end select
  close(205)

 write(IOUT,'(a)') "GRID SETUP COMPLETED"
 stop
 1000 continue
 write(IOUT,'(a,i4)') "GRID SETUP FAILED, stat=",stat
 stop
end program grid_generation
