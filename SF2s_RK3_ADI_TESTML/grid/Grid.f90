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
!     Support is provided for uniform grids, hyperbolic sine stretching and
!     percent stretching at this time. The edge grid is defined first and 
!     all other values are derived from it. 
!@q

 use ntypes
 implicit none

 !Variables
 integer,parameter           :: IOUT=6,IOUTerr=0
 character(len=25),parameter :: inFILE='gridgen.ini'
 integer                     :: i,j,k, nx,ny,nz,nxp2,nyp2,nzp2,nx1,nx2,nx3
 integer                     :: CenterLine, kts1, kts2, kls1, kls2
 real(r8)                    :: xL, yL, zL, x0, y0, z0, dx_uni, dy_uni, dz_uni,xL1,xL2,xL3
 character(len=25)           :: Gridtype(1:3),locX0,locY0,locZ0
 character(len=5),parameter  :: defval='ERROR'
 character(len=100)          :: gridfile, block

 real(r8),allocatable,dimension(:),target :: xe,xc,dxe,dxc
 real(r8),allocatable,dimension(:),target :: ye,yc,dye,dyc
 real(r8),allocatable,dimension(:),target :: ze,zc,dze,dzc
 real(r8),allocatable,dimension(:),target :: rdxe,rdxc
 real(r8),allocatable,dimension(:),target :: rdye,rdyc
 real(r8),allocatable,dimension(:),target :: rdze,rdzc

 ! Percent stretching variables
 real(r8)                    :: rx, ry, rz
 real(r8)                    :: mindx, mindy, mindz, mindx1, mindx2, mindx3
 real(r8)                    :: xstretchfactor, ystretchfactor, zstretchfactor
 integer                     :: boxflag, x1blend, x2blend, x3blend, counter, x1outerblend, x2outerblend, x3outerblend 

 ! Local Variables
 integer                     :: s1,stat, zeta,N
 real(r8)                    :: x,y,z, ds, t, t0,slope,aa
 

![Domain]
 block='Domain'
 call scaniniint(infile,block,'NX',nx,defval,IOUTerr)
 call scaniniint(infile,block,'NY',ny,defval,IOUTerr)
 call scaniniint(infile,block,'NZ',nz,defval,IOUTerr)
 call scaniniint(infile,block,'NX1',nx1,defval,IOUTerr)
 call scaniniint(infile,block,'NX2',nx2,defval,IOUTerr)
 call scaniniint(infile,block,'NX3',nx3,defval,IOUTerr)
 nxp2=nx+2
 nyp2=ny+2
 nzp2=nz+2


!GRID ALLOCATION
 !X1-Direction
 allocate( xe(1:nxp2), xc(1:nxp2), dxe(1:nxp2), dxc(1:nxp2), rdxe(1:nxp2), rdxc(1:nxp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating X1 Grid Variables"
  goto 1000
 endif

 !X2-Direction
 allocate( ye(1:nyp2), yc(1:nyp2), dye(1:nyp2), dyc(1:nyp2), rdye(1:nyp2), rdyc(1:nyp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating X2 Grid Variables"
  goto 1000
 endif

 !X3-Direction
 allocate( ze(1:nzp2), zc(1:nzp2), dze(1:nzp2), dzc(1:nzp2), rdze(1:nzp2), rdzc(1:nzp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating X3 Grid Variables"
  goto 1000
 endif

![Grid]
 block='Grid' 
 call scaninireal(infile,block,'Lx',xL,defval,IOUTerr)
 call scaninireal(infile,block,'Lx1',xL1,defval,IOUTerr)
 call scaninireal(infile,block,'Lx2',xL2,defval,IOUTerr)
 call scaninireal(infile,block,'Lx3',xL3,defval,IOUTerr)
 call scaninireal(infile,block,'Ly',yL,defval,IOUTerr)
 call scaninireal(infile,block,'Lz',zL,defval,IOUTerr)
 call scaninichar(infile,block,'x0',locx0,defval,IOUTerr)
 call scaninichar(infile,block,'y0',locy0,defval,IOUTerr)
 call scaninichar(infile,block,'z0',locz0,defval,IOUTerr)
 call scaninichar(infile,block,'XGridtype',Gridtype(1),defval,IOUTerr)
 call scaninichar(infile,block,'YGridtype',Gridtype(2),defval,IOUTerr)
 call scaninichar(infile,block,'ZGridtype',Gridtype(3),defval,IOUTerr)

![PercentStretching]
 block='PercentStretching' 
 call scaninireal(infile,block,'rx',rx,defval,IOUTerr)
 call scaninireal(infile,block,'ry',ry,defval,IOUTerr)
 call scaninireal(infile,block,'rz',rz,defval,IOUTerr)
 call scaninireal(infile,block,'mindx',mindx,defval,IOUTerr)
 call scaninireal(infile,block,'mindy',mindy,defval,IOUTerr)
 call scaninireal(infile,block,'mindz',mindz,defval,IOUTerr)
 call scaninireal(infile,block,'percentstretchinginx',xstretchfactor,defval,IOUTerr)
 call scaninireal(infile,block,'percentstretchinginy',ystretchfactor,defval,IOUTerr)
 call scaninireal(infile,block,'percentstretchinginz',zstretchfactor,defval,IOUTerr)
 call scaniniint(infile,block,'x1blend',x1blend,defval,IOUTerr)
 call scaniniint(infile,block,'x2blend',x2blend,defval,IOUTerr)
 call scaniniint(infile,block,'x3blend',x3blend,defval,IOUTerr)
 call scaniniint(infile,block,'x1outerblend',x1outerblend,defval,IOUTerr)
 call scaniniint(infile,block,'x2outerblend',x2outerblend,defval,IOUTerr)
 call scaniniint(infile,block,'x3outerblend',x3outerblend,defval,IOUTerr)


!**********************************************************************
!****************************GRID ORIGIN*******************************
!**********************************************************************
 !X0
 select case(locX0)
  case('start')
   x0 = 0.d0 
  case('end')
   x0 = -( xL)
  case('center')
   x0 = -xL/2.d0 
  case('custom')
!   x0 = -xL/2.d0 + 2.5d0
   x0 = -6.5d0
  case DEFAULT
   goto 1000
 end select
 !Y0
 select case(locY0)
  case('start')
   y0 = 0.d0
  case('end')
   y0 = -( yL) 
  case('center')
   y0 = -yL/2.d0 
  case('custom')
   y0 = -1.0d0
  case DEFAULT
   goto 1000
 end select
 !Z0
 select case(locZ0)
  case('start')
   z0 = 0.d0
  case('end')
   z0 = -( zL) 
  case('center')
   z0 = -zL/2.d0 
  case('custom')
   z0 = 0.5!-0.15 Try this next
   case DEFAULT
   goto 1000
 end select

!**********************************************************************
!*******************************X1 GRID********************************
!**********************************************************************
  select case(GridType(1))
   case('uniformE')
    do i=1,nxp2
    ! define xe such that xe(1) = 0 and xe(nx+2) = L 
      xe(i) = x0 + xL/dble(nxp2-1)*dble(i-1)
      dx_uni=xL/dble(nxp2-1)
      GridType(1)='uniform'
    enddo
   case('uniformC')
    do i=1,nxp2
    ! define xe such that xc(1) = 0 and xc(nx+2) = L 
      xe(i) = x0 + xL/dble(nxp2-1)*dble(i) - 0.5d0*xL/dble(nxp2-1)
      dx_uni=xL/dble(nxp2-1)
      GridType(1)='uniform'
    enddo
   case('uniform_per_E')
    do i=1,nxp2
    ! define xe such that xe(1) = 0 and xe(nx+1) = L 
      xe(i) = x0 + xL/dble(nxp2-2)*dble(i-1)
      dx_uni=xL/dble(nxp2-2)
      GridType(1)='uniform'
    enddo
   case('uniform_per_C')
    do i=1,nxp2
    ! define xe such that xc(1) = 0 and xc(nx+1) = L 
      xe(i) = x0 + xL/dble(nxp2-2)*dble(i) - 0.5d0*xL/dble(nxp2-2)
      dx_uni=xL/dble(nxp2-2)
      GridType(1)='uniform'
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
     counter = 1
     do i = 1,nxp2/2-1
       if (boxflag==0) then
       ! UNSTRETCHED CENTER REGION
         xe(nxp2/2+i+1) = xe(nxp2/2+i) + mindx
         xe(nxp2/2-i)   = xe(nxp2/2-i+1) - mindx
         if (xe(nxp2/2+i+1) > rx) then
           boxflag = 2;
         endif
       elseif (boxflag==1) then
       ! STRETCHED TOWARDS THE BOUNDARIES
         xe(nxp2/2+i+1)  = xe(nxp2/2+i)+( xe(nxp2/2+i)-xe(nxp2/2+i-1) )*xstretchfactor
         xe(nxp2/2-i)    = xe(nxp2/2-i+1)-( xe(nxp2/2-i+2)-xe(nxp2/2-i+1) )*xstretchfactor
       elseif (boxflag==2) then
       ! blend between stretched and unstretched
         xe(nxp2/2+i+1)  = xe(nxp2/2+i)+( xe(nxp2/2+i)-xe(nxp2/2+i-1) ) &
                           * (1 + dtanh( 4* (counter-0.5d0*x1blend)/(0.5d0*x1blend) ) &
                           * (xstretchfactor-1.d0)*0.5d0 + (xstretchfactor-1.d0)*0.5d0)
         xe(nxp2/2-i)    = xe(nxp2/2-i+1)-( xe(nxp2/2-i+2)-xe(nxp2/2-i+1) ) &
                           * (1 + dtanh( 4* (counter-0.5d0*x1blend)/(0.5d0*x1blend) ) &
                           * (xstretchfactor-1.d0)*0.5d0 + (xstretchfactor-1.d0)*0.5d0)
         counter = counter+1 ! counting variable
         if (counter > x1blend-1) then
           boxflag = 1;
         endif
       endif
     enddo
   case('uniformstretch')
     boxflag = 0
     x0 = 0.0d0
     counter = 1
     do i = 1,nxp2
       if (boxflag==0) then
       ! UNIFORM REGION
         xe(i) = x0+ mindx*dble(i-1)
         if (xe(i) > rx) then
           boxflag = 2
         endif
       elseif (boxflag==1) then
       ! UNIFORM OUTER REGION
         xe(i)  = xe(i-1)+(xe(i-1)-xe(i-2))*xstretchfactor
         if (xe(i) > xL1) then
           boxflag = 3
           counter = 1
         endif
         
       elseif (boxflag==2)then
       ! STRETCHED AFTER THE UNIFORM REGION
!          xe(i)  = xe(i-1)+(xe(i-1)-xe(i-2) )*xstretchfactor
         xe(i)  = xe(i-1)+( xe(i-1)-xe(i-2) ) &
                           * (1 + dtanh( 4* (counter-0.5d0*x1blend)/(0.5d0*x1blend) ) &
                           * (xstretchfactor-1.d0)*0.5d0 + (xstretchfactor-1.d0)*0.5d0)
         counter = counter+1
         if (counter > x1blend-1) then
         boxflag = 1
         endif
        elseif (boxflag==3) then
        xe(i)  = xe(i-1)+( xe(i-1)-xe(i-2) ) &
                           * (1 - dtanh( 4* (counter-0.5d0*x1blend)/(0.5d0*x1blend) ) &
                           * (xstretchfactor-1.d0)*0.5d0 + (xstretchfactor-1.d0)*0.5d0)
         counter = counter+1
         if (counter > x1blend-1) then
         boxflag = 4
         endif
        elseif (boxflag==4) then
        xe(i)  = xe(i-1)+( xe(i-1)-xe(i-2) )
        endif
     GridType(1)='percentstretch'
     end do

   case('uniformstretchinsteps')
     boxflag = 0
     x0 = 0.0d0
     mindx1=1.0d0/dble(nx1)
     mindx2=1.0d0/dble(nx2)
     mindx3=1.0d0/dble(nx3)
     do i = 1,nxp2
       if (xe(i-1).lt.xL1) then
       ! UNIFORM REGION1
         xe(i) = x0+ mindx1*dble(i-1)
       elseif (xe(i-1).eq.xL1.or.xe(i-1).gt.xL1.and.xe(i-1).lt.xL2) then
       ! UNIFORM REGION2
         xe(i)  = xe(i-1)+mindx2
       else
       ! UNIFORM REGION3
         xe(i)  = xe(i-1)+mindx3
       endif
      GridType(1)='percentstretch'
     end do  
   case('stokes')
   		!zeta E [0,N]
   		!the extreme end point is kept outside the domain as the last velocity node is ignored in this code
   		!so the actual domain length ranges over grid point indices k=1,nz+1
   		N 	  = nx		!number of intervals inside the domain [0,1] = no. of grid points - 1
!    		xstretchfactor = 3.5d0 !higher the delta, higher the stretching towards center
   		do i=1,nxp2
   			zeta  = i-1
   			t	  = dble(zeta) / dble(N)
   			x 	  = 0.5d0 + ( dtanh(xstretchfactor*t) - dtanh(xstretchfactor*(1.d0-t)) )/ ( 2.d0*dtanh(xstretchfactor) )
   			xe(i) = x0 + xL * x	!scale the grid to make the box height zL = 2.0
   			!xe(i) = x0 + x	 	!default box height is 1.0
   		enddo
   
   case('cccustom')
   		!zeta E [0,N]
   		!the extreme end point is kept outside the domain as the last velocity node is ignored in this code
   		!so the actual domain length ranges over grid point indices k=1,nz+1
   		N 	  = nx		!number of intervals inside the domain [0,1] = no. of grid points - 1
   		slope = 1.d0/xstretchfactor !smaller the slope, higher the stretching, varies between 0 and 1
   		t0 = 0.5d0	!2.58d0/6.d0	! for 162x96x96 2.35d0/6.d0			!center of stretching, specified a fraction of the domain length
   		aa = (1.d0 - slope)/( (1.d0-t0)**3 + t0**3 )
   		
   		do i=1,nxp2
   			zeta  = i-1
   			t	  = dble(zeta) / dble(N)
   			x 	  = 0.5d0 + ( dtanh(xstretchfactor*t) - dtanh(xstretchfactor*(1.d0-t)) )/ ( 2.d0*dtanh(xstretchfactor) )
! 			x 	  = aa * ( (t-t0)**3 + t0**3 ) + slope*t
   			xe(i) = x0 + xL * x	!scale the grid to make the box height zL = 2.0
   			!xe(i) = x0 + x	 	!default box height is 1.0
   		enddo
   

   case('custom')	!slope
		CenterLine = nxp2/2 - 1
		kls1 = CenterLine - 74
		kts1 = CenterLine + 75
		kls2 = kls1 - 15
		kts2 = kts1 + 15
		dxe(CenterLine) = 2.d0/80.d0	!0.01d0 !xL/dble(nxp2-1)
		
		do i = CenterLine+1, kts1
			dxe(i) = dxe(i-1) * 1.0d0
		enddo
		do i = CenterLine-1, kls1,-1
		dxe(i) = dxe(i+1) * 1.0d0
		enddo
		
		ds = 5.d0/100.d0
		do i = kts1+1,kts2
			dxe(i) = dxe(i-1) * (1+ ( dtanh( 4.d0* (i-(kts1+kts2)/2.d0) / ( (kts2-kts1)/2.d0 ) ) *ds/2.d0 + ds/2.d0 ))
		enddo
		ds = 5.d0/100.d0
		do i = kls1-1,kls2,-1
		dxe(i) = dxe(i+1) * (1+ ( dtanh( 4.d0* (i-(kls1+kls2)/2.d0) / ( (kls2-kls1)/2.d0 ) ) *ds/2.d0 + ds/2.d0 ))
		enddo
		
		do i = kts2+1,nxp2-11
			dxe(i) = dxe(i-1)* 1.05d0
		enddo
		do i = nxp2-10,nxp2
			dxe(i) = dxe(i-1)* 1.0d0
		enddo
		
		do i = kls2-1,11,-1
		dxe(i) = dxe(i+1) * 1.05d0
		enddo
		
		do i = 10,1,-1
		dxe(i) = dxe(i+1) * 1.0d0
		enddo
		
		
		xe(CenterLine) = 6.25d0
		do i = CenterLine+1,nxp2
		xe(i) = xe(i-1) + dxe(i)
		enddo
		do i= CenterLine-1,1,-1
		xe(i) = xe(i+1) - dxe(i+1)
		enddo
		xL = xe(nxp2)-xe(1)
		
   		x0 = xe(1)
   
   case('sphere')
   		x0 = -1.d0
		CenterLine = 74	!nxp2/2
		kls1 = CenterLine - 25
		kts1 = CenterLine + 45
		kls2 = kls1 - 6
		kts2 = kts1 + 6
		dxe(CenterLine) = 0.2d0/40.d0	!0.01d0 !xL/dble(nxp2-1)
		
		do i = CenterLine+1, kts1
			dxe(i) = dxe(i-1) * 1.0d0
		enddo
		do i = CenterLine-1, kls1,-1
		dxe(i) = dxe(i+1) * 1.0d0
		enddo
		
		ds = 5.d0/100.d0
		do i = kts1+1,kts2
			dxe(i) = dxe(i-1) * (1+ ( dtanh( 4.d0* (i-(kts1+kts2)/2.d0) / ( (kts2-kts1)/2.d0 ) ) *ds/2.d0 + ds/2.d0 ))
		enddo
		ds = 5.d0/100.d0
		do i = kls1-1,kls2,-1
		dxe(i) = dxe(i+1) * (1+ ( dtanh( 4.d0* (i-(kls1+kls2)/2.d0) / ( (kls2-kls1)/2.d0 ) ) *ds/2.d0 + ds/2.d0 ))
		enddo
		
		do i = kts2+1,nxp2-4
			dxe(i) = dxe(i-1)* 1.05d0
		enddo
		do i = nxp2-3,nxp2
			dxe(i) = dxe(i-1)* 1.0d0
		enddo
		
		do i = kls2-1,11,-1
		dxe(i) = dxe(i+1) * 1.05d0
		enddo
		
		do i = 10,1,-1
		dxe(i) = dxe(i+1) * 1.0d0
		enddo
		
		
		xe(CenterLine) = 1.d0
		do i = CenterLine+1,nxp2
		xe(i) = xe(i-1) + dxe(i)
		enddo
		do i= CenterLine-1,1,-1
		xe(i) = xe(i+1) - dxe(i+1)
		enddo
		xL = xe(nxp2)-xe(1)
   
!    		do i=1, nxp2
!    			print *, "Xgrid:", i, dxe(i), xe(i)
!    		enddo
   
   case DEFAULT
   		print *, "The grid type is not correct"
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
!============= matt this may modify narsimha's stuff, check it out =============
! assign values for the first grid cell
 dxc(1)=dxc(2)
 rdxc(1)=rdxc(2)
 xc(1) = xe(1)-0.5d0*dxc(1)

 do j=1,nxp2-1
  dxe(j) = xc(j+1)-xc(j)
  rdxe(j) = 1.d0/dxe(j)
 enddo
! assign values for the last grid cell
 dxe(nxp2) = dxe(nxp2-1)
 rdxe(nxp2) = rdxe(nxp2-1)

 !Ensure No Stretching at First and Last Cells in X2
  dxc(nxp2) = dxc(nxp2-1)
  rdxc(nxp2) = rdxc(nxp2-1)
  dxe(1)=dxe(2)
  rdxe(1)=rdxe(2)
  xe(nxp2)=xe(nxp2-1)+dxc(nxp2)
  xe(1)=xe(2)-dxc(2)
  xc(nxp2)=xc(nxp2-1)+dxe(nxp2-1)
  xc(1)=xc(2)-dxe(1)

!============= matt this may modify narsimha's stuff, check it out =============

! do i=1,nxp2-1
!   print *, 'stretch factor in X direction', i, dxe(i)/dxe(i+1)
! enddo

!**********************************************************************
!*******************************X2 GRID********************************
!**********************************************************************
  select case(GridType(2))
   case('uniformE')
    do j=1,nyp2
    ! define ye such that ye(1) = 0 and ye(ny+2) = L 
      ye(j) = y0 + yL/dble(nyp2-1)*dble(j-1)
      dy_uni=yL/dble(nyp2-1)
      GridType(2)='uniform'
    enddo
   case('uniformC')
    do j=1,nyp2
    ! define ye such that yc(1) = 0 and yc(ny+2) = L 
      ye(j) = y0 + yL/dble(nyp2-1)*dble(j) - 0.5d0*yL/dble(nyp2-1)
      dy_uni=yL/dble(nyp2-1)
      GridType(2)='uniform'
    enddo
   case('uniform_per_E')
    do j=1,nyp2
    ! define ye such that ye(1) = 0 and ye(ny+1) = L 
      ye(j) = y0 + yL/dble(nyp2-2)*dble(j-1)
      dy_uni=yL/dble(nyp2-2)
      GridType(2)='uniform'
    enddo
   case('uniform_per_C')
    do j=1,nyp2
    ! define ye such that yc(1) = 0 and yc(ny+1) = L 
      ye(j) = y0 + yL/dble(nyp2-2)*dble(j) - 0.5d0*yL/dble(nyp2-2)
      dy_uni=yL/dble(nyp2-2)
      GridType(2)='uniform'
    enddo
   case('stretch1')
    do j=1,nyp2
     ye(j) = y0 + yL/(2.d0)*dsinh( 1.75d0*( dble(j-1)/(dble(nyp2-1)/2.d0)-1.d0) )
    enddo
     y0 = ( ye(1)+ye(nyp2) )/2.d0
     ye(:) = ye(:)-y0
   case('percentstretch')
     boxflag = 0;
     ye(nyp2/2+1) =  mindy/2.0d0
     ye(nyp2/2)   = -mindy/2.0d0
     counter = 1 ! counting variable
     do i = 1,nyp2/2-1
       if (boxflag==0) then
       ! UNSTRETCHED CENTER REGION
         ye(nyp2/2+i+1) = ye(nyp2/2+i)   + mindy
         ye(nyp2/2-i)   = ye(nyp2/2-i+1) - mindy
         if (ye(nyp2/2+i+1) > ry) boxflag=2
       elseif (boxflag==1) then
       ! STRETCHED TOWARDS THE BOUNDARIES
         ye(nyp2/2+i+1)  = ye(nyp2/2+i)  +( ye(nyp2/2+i)   - ye(nyp2/2+i-1) )*ystretchfactor
         ye(nyp2/2-i)    = ye(nyp2/2-i+1)-( ye(nyp2/2-i+2) - ye(nyp2/2-i+1) )*ystretchfactor
       elseif (boxflag==2) then
       ! blend between stretched and unstretched
         ye(nyp2/2+i+1)  = ye(nyp2/2+i)+( ye(nyp2/2+i)-ye(nyp2/2+i-1) ) &
                           * (1 + dtanh( 4.d0* (counter-0.5d0*x2blend)/(0.5d0*x2blend) ) &
                           * (ystretchfactor-1.d0)*0.5d0 + (ystretchfactor-1.d0)*0.5d0)
         ye(nyp2/2-i)    = ye(nyp2/2-i+1)-( ye(nyp2/2-i+2)-ye(nyp2/2-i+1) ) &
                           * (1 + dtanh( 4.d0* (counter-0.5d0*x2blend)/(0.5d0*x2blend) ) &
                           * (ystretchfactor-1.d0)*0.5d0 + (ystretchfactor-1.d0)*0.5d0)
         counter = counter+1 ! counting variable
         if (counter > x2blend-1) then
           boxflag = 1;
         endif
       endif
     enddo
   case('percentstretchtaper')
     boxflag = 0;
     ye(nyp2/2+1) =  mindy/2.0d0
     ye(nyp2/2)   = -mindy/2.0d0
     counter = 1 ! counting variable
     do i = 1,nyp2/2-1
       if (boxflag==0) then
       ! UNSTRETCHED CENTER REGION
         ye(nyp2/2+i+1) = ye(nyp2/2+i) + mindy
         ye(nyp2/2-i)   = ye(nyp2/2-i+1) - mindy
         if (ye(nyp2/2+i+1) > ry) then
           boxflag = 2;
         endif
       elseif (boxflag==1) then
       ! STRETCHED TOWARDS THE BOUNDARIES
         ye(nyp2/2+i+1)  = ye(nyp2/2+i)+( ye(nyp2/2+i)-ye(nyp2/2+i-1) )*ystretchfactor
         ye(nyp2/2-i)    = ye(nyp2/2-i+1)-( ye(nyp2/2-i+2)-ye(nyp2/2-i+1) )*ystretchfactor
         if (i > nyp2/2-1 - x2outerblend) boxflag=3
       elseif (boxflag==2) then
       ! blend between stretched and unstretched
         ye(nyp2/2+i+1)  = ye(nyp2/2+i)+( ye(nyp2/2+i)-ye(nyp2/2+i-1) ) &
                           * (1 + dtanh( 4.d0* (counter-0.5d0*x2blend)/(0.5d0*x2blend) ) &
                           * (ystretchfactor-1.d0)*0.5d0 + (ystretchfactor-1.d0)*0.5d0)
         ye(nyp2/2-i)    = ye(nyp2/2-i+1)-( ye(nyp2/2-i+2)-ye(nyp2/2-i+1) ) &
                           * (1 + dtanh( 4.d0* (counter-0.5d0*x2blend)/(0.5d0*x2blend) ) &
                           * (ystretchfactor-1.d0)*0.5d0 + (ystretchfactor-1.d0)*0.5d0)
         counter = counter+1 ! counting variable
         if (counter > x2blend-1) then
           boxflag = 1;
           counter = 1 ! counting variable, reset for tapering
         endif
       elseif (boxflag==3) then
       ! tapering at edge of domain 
         ye(nyp2/2+i+1)  = ye(nyp2/2+i)+( ye(nyp2/2+i)-ye(nyp2/2+i-1) ) &
                           * (1 - dtanh( 2.d0* (counter-0.5d0*x2outerblend)/(0.5d0*x2outerblend) ) &
                           * (ystretchfactor-1.d0)*0.5d0 + (ystretchfactor-1.d0)*0.5d0)
         ye(nyp2/2-i)    = ye(nyp2/2-i+1)-( ye(nyp2/2-i+2)-ye(nyp2/2-i+1) ) &
                           * (1 - dtanh( 2.d0* (counter-0.5d0*x2outerblend)/(0.5d0*x2outerblend) ) &
                           * (ystretchfactor-1.d0)*0.5d0 + (ystretchfactor-1.d0)*0.5d0)
         counter = counter+1 ! counting variable
       endif
     enddo
     GridType(2)='percentstretch'
   case('custom')
   		!zeta E [0,N]
   		!the extreme end point is kept outside the domain as the last velocity node is ignored in this code
   		!so the actual domain length ranges over grid point indices k=1,nz+1
   		N 	  = ny		!number of intervals inside the domain [0,1] = no. of grid points - 1
   		ds 	  = ystretchfactor/( dble(N)*dsinh(ystretchfactor) )
   		print *, "First grid spacing = ", ds
   		do j=1,nyp2
   			zeta  = j-1
   			t	  = dble(zeta) / dble(N)
   			y 	  = 0.5d0 + ( dtanh(ystretchfactor*t) - dtanh(ystretchfactor*(1.d0-t)) )/ ( 2.d0*dtanh(ystretchfactor) )
   			ye(j) = y0 + yL * y	!scale the grid to make the box height zL = 2.0
   			!ye(j) = y0 + y	 	!default box height is 1.0
   		enddo
   
   case('cccustom')
   		y0 = -1.d0
		CenterLine = nyp2/2
		kls1 = CenterLine - 24
		kts1 = CenterLine + 25 
		kls2 = kls1 - 6
		kts2 = kts1 + 6
		dye(CenterLine) = 0.2d0/30.d0	!0.01d0 !yL/dble(nyp2-1)
		
		do j = CenterLine+1, kts1
			dye(j) = dye(j-1) * 1.0d0
		enddo
		do j = CenterLine-1, kls1,-1
		dye(j) = dye(j+1) * 1.0d0
		enddo
		
		ds = 5.d0/100.d0
		do j = kts1+1,kts2
			dye(j) = dye(j-1) * (1+ ( dtanh( 4.d0* (j-(kts1+kts2)/2.d0) / ( (kts2-kts1)/2.d0 ) ) *ds/2.d0 + ds/2.d0 ))
		enddo
		ds = 5.d0/100.d0
		do j = kls1-1,kls2,-1
		dye(j) = dye(j+1) * (1+ ( dtanh( 4.d0* (j-(kls1+kls2)/2.d0) / ( (kls2-kls1)/2.d0 ) ) *ds/2.d0 + ds/2.d0 ))
		enddo
		
		do j = kts2+1,nyp2-11
			dye(j) = dye(j-1)* 1.05d0
		enddo
		do j = kls2-1,11,-1
		dye(j) = dye(j+1) * 1.05d0
		enddo
		
		do j = nyp2-10,nyp2
			dye(j) = dye(j-1)* 1.0d0
		enddo
		do j = 10,1,-1
		dye(j) = dye(j+1) * 1.0d0
		enddo
		
		
		ye(CenterLine) = 1.d0
		do j = CenterLine+1,nyp2
		ye(j) = ye(j-1) + dye(j)
		enddo
		do j= CenterLine-1,1,-1
		ye(j) = ye(j+1) - dye(j+1)
		enddo
		yL = ye(nyp2)-ye(1)
   
!    		do j=1, nyp2
!    			print *, "Ygrid:", j, dye(j), ye(j)
!    		enddo
    
    case DEFAULT
   		print *, "The grid type is not correct"
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

!============= matt this may modify narsimha's stuff, check it out =============
! assign values for the first grid cell
 dyc(1)=dyc(2)
 rdyc(1)=rdyc(2)
 yc(1) = ye(1)-0.5d0*dyc(1)
 
 do j=1,nyp2-1
  dye(j) = yc(j+1)-yc(j)
  rdye(j) = 1.d0/dye(j)
 enddo
! assign values for the last grid cell
 dye(nyp2) = dye(nyp2-1)
 rdye(nyp2) = rdye(nyp2-1)
 
 !Ensure No Stretching at First and Last Cells in X2
  dyc(nyp2) = dyc(nyp2-1)
  rdyc(nyp2) = rdyc(nyp2-1)
  dye(1)=dye(2)
  rdye(1)=rdye(2)
  ye(nyp2)=ye(nyp2-1)+dyc(nyp2)
  ye(1)=ye(2)-dyc(2)
  yc(nyp2)=yc(nyp2-1)+dye(nyp2-1)
  yc(1)=yc(2)-dye(1)
!============= matt this may modify narsimha's stuff, check it out =============

! do j=1,nyp2-1
!   print *, 'stretch factor in Y direction', j, dye(j)/dye(j+1)
! enddo

!**********************************************************************
!*******************************X3 GRID********************************
!**********************************************************************
  select case(GridType(3))
   case('uniformE')
    do k=1,nzp2
    ! define ze such that ze(1) = 0 and ze(nz+2) = L 
      ze(k) = z0 + zL/dble(nzp2-1)*dble(k-1)
      dz_uni=zL/dble(nzp2-1)
      GridType(3)='uniform'
    enddo
   case('uniformC')
    do k=1,nzp2
    ! define ze such that zc(1) = 0 and zc(nz+2) = L 
      ze(k) = z0 + zL/dble(nzp2-1)*dble(k) - 0.5d0*zL/dble(nzp2-1)
      dz_uni=zL/dble(nzp2-1)
      GridType(3)='uniform'
    enddo
   case('uniform_per_E')
    do k=1,nzp2
    ! define ze such that ze(1) = 0 and ze(nz+1) = L 
      ze(k) = z0 + zL/dble(nzp2-2)*dble(k-1)
      dz_uni=zL/dble(nzp2-2)
      GridType(3)='uniform'
    enddo
   case('uniform_per_C')
    do k=1,nzp2
    ! define ze such that zc(1) = 0 and zc(nz+1) = L 
      ze(k) = z0 + zL/dble(nzp2-2)*dble(k) - 0.5d0*zL/dble(nzp2-2)
      GridType(3)='uniform'
      dz_uni=zL/dble(nzp2-2)
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
     ze(nzp2/2+1) =  mindz/2.0d0
     ze(nzp2/2)   = -mindz/2.0d0
     counter=1
     do i = 1,nzp2/2-1
       if (boxflag==0) then
       ! UNSTRETCHED CENTER REGION
         ze(nzp2/2+i+1) = ze(nzp2/2+i)   + mindz
         ze(nzp2/2-i)   = ze(nzp2/2-i+1) - mindz
         if (ze(nzp2/2+i+1) > rz) boxflag = 2
       elseif (boxflag==1) then
       ! STRETCHED TOWARDS THE BOUNDARIES
         ze(nzp2/2+i+1)  = ze(nzp2/2+i)  +( ze(nzp2/2+i)-ze(nzp2/2+i-1) )*zstretchfactor
         ze(nzp2/2-i)    = ze(nzp2/2-i+1)-( ze(nzp2/2-i+2)-ze(nzp2/2-i+1) )*zstretchfactor
       elseif (boxflag==2) then
       ! STRETCHED TOWARDS THE BOUNDARIES
         ze(nzp2/2+i+1)  = ze(nzp2/2+i)+( ze(nzp2/2+i)-ze(nzp2/2+i-1) ) &
                           * (1 + dtanh( 4.d0* (counter-0.5d0*x3blend)/(0.5d0*x3blend) ) &
                           * (zstretchfactor-1.d0)*0.5d0 + (zstretchfactor-1.d0)*0.5d0)
         ze(nzp2/2-i)    = ze(nzp2/2-i+1)-( ze(nzp2/2-i+2)-ze(nzp2/2-i+1) ) &
                           * (1 + dtanh( 4.d0* (counter-0.5d0*x3blend)/(0.5d0*x3blend) ) &
                           * (zstretchfactor-1.d0)*0.5d0 + (zstretchfactor-1.d0)*0.5d0)
         counter = counter+1 ! counting variable
         if (counter > x3blend-1) boxflag = 1
       endif
     enddo
   case('percentstretchtaper')
     boxflag = 0;
     ze(nzp2/2+1) =  mindz/2.0d0
     ze(nzp2/2)   = -mindz/2.0d0
     counter=1
     do i = 1,nzp2/2-1
       if (boxflag==0) then
       ! UNSTRETCHED CENTER REGION
         ze(nzp2/2+i+1) = ze(nzp2/2+i)   + mindz
         ze(nzp2/2-i)   = ze(nzp2/2-i+1) - mindz
         if (ze(nzp2/2+i+1) > rz) then
           boxflag = 2
         endif
       elseif (boxflag==1) then
       ! STRETCHED TOWARDS THE BOUNDARIES
         ze(nzp2/2+i+1)  = ze(nzp2/2+i)  +( ze(nzp2/2+i)-ze(nzp2/2+i-1) )*zstretchfactor
         ze(nzp2/2-i)    = ze(nzp2/2-i+1)-( ze(nzp2/2-i+2)-ze(nzp2/2-i+1) )*zstretchfactor
         if (i > nzp2/2-1 - x3outerblend) boxflag=3
       elseif (boxflag==2) then
       ! STRETCHED TOWARDS THE BOUNDARIES
         ze(nzp2/2+i+1)  = ze(nzp2/2+i)+( ze(nzp2/2+i)-ze(nzp2/2+i-1) ) &
                           * (1 + dtanh( 4.d0* (counter-0.5d0*x3blend)/(0.5d0*x3blend) ) &
                           * (zstretchfactor-1.d0)*0.5d0 + (zstretchfactor-1.d0)*0.5d0)
         ze(nzp2/2-i)    = ze(nzp2/2-i+1)-( ze(nzp2/2-i+2)-ze(nzp2/2-i+1) ) &
                           * (1 + dtanh( 4.d0* (counter-0.5d0*x3blend)/(0.5d0*x3blend) ) &
                           * (zstretchfactor-1.d0)*0.5d0 + (zstretchfactor-1.d0)*0.5d0)
         counter = counter+1 ! counting variable
         if (counter > x3blend-1) then
           boxflag = 1
           counter = 1
         endif
       elseif (boxflag==3) then
       ! TAPERING TOWARDS THE BOUNDARIES
         ze(nzp2/2+i+1)  = ze(nzp2/2+i)+( ze(nzp2/2+i)-ze(nzp2/2+i-1) ) &
                           * (1 - dtanh( 2.d0* (counter-0.5d0*x3outerblend)/(0.5d0*x3outerblend) ) &
                           * (zstretchfactor-1.d0)*0.5d0 + (zstretchfactor-1.d0)*0.5d0)
         ze(nzp2/2-i)    = ze(nzp2/2-i+1)-( ze(nzp2/2-i+2)-ze(nzp2/2-i+1) ) &
                           * (1 - dtanh( 2.d0* (counter-0.5d0*x3outerblend)/(0.5d0*x3outerblend) ) &
                           * (zstretchfactor-1.d0)*0.5d0 + (zstretchfactor-1.d0)*0.5d0)
         counter = counter+1 ! counting variable
       endif
     enddo
      GridType(3)='percentstretch'
   case('channel')
   		!zeta E [0,N]
   		!the extreme end point is kept outside the domain as the last velocity node is ignored in this code
   		!so the actual domain length ranges over grid point indices k=1,nz+1
   		N 	  = nz		!number of intervals inside the domain [0,1] = no. of grid points - 1
   		ds 	  = zstretchfactor/( dble(N)*dsinh(zstretchfactor) ) 	!higher the delta, higher the near wall stretching
   		print *, "First grid spacing = ", ds
   		do k=1,nzp2
   			zeta  = k-1
   			! Double sided stretching function: symmetric about zeta = nz/2
   			z 	  = 0.5d0 * ( 1.0d0 + dtanh( zstretchfactor*(dble(zeta)/dble(N) - 0.5d0) ) / dtanh(0.5d0*zstretchfactor) )
   			ze(k) = z0 + zL * z	!scale the grid to make the channel height zL = 2.0
   			!ze(k) = z0 + z	 	!default channel height is 1.0
   		enddo
   
   case('ccustom')
   		!zeta E [0,N]
   		!the extreme end point is kept outside the domain as the last velocity node is ignored in this code
   		!so the actual domain length ranges over grid point indices k=1,nz+1
   		N 	  = nz		!number of intervals inside the domain [0,1] = no. of grid points - 1
   		ds 	  = zstretchfactor/( dble(N)*dsinh(zstretchfactor) )
   		print *, "First grid spacing = ", ds
   		do k=1,nzp2
   			zeta  = k-1
   			t	  = dble(zeta) / dble(N)
   			z 	  = 0.5d0 + ( dtanh(zstretchfactor*t) - dtanh(zstretchfactor*(1.d0-t)) )/ ( 2.d0*dtanh(zstretchfactor) )
   			! Single sided stretching function
   			z 	  = 1.0d0 + dtanh( zstretchfactor*(dble(zeta)/dble(N) - 1.d0) ) / dtanh(zstretchfactor)
   			ze(k) = z0 + zL * z	!scale the grid to make the box height zL = 2.0
   			!ze(k) = z0 + z	 	!default box height is 1.0
   		enddo
   		
!    		do k=1, nzp2
!    			print *, k, dze(k), ze(k)
!    		enddo
   		
   case('custom')	!slope
   		z0 = 0.46d0
		CenterLine = 70	!nzp2/2
		kls1 = CenterLine - 25
		kts1 = CenterLine + 20 
		kls2 = kls1 - 6
		kts2 = kts1 + 6
		dze(CenterLine) = 0.5d0/70.d0 !zL/dble(nzp2-1)
		
		do k = CenterLine+1, kts1
			dze(k) = dze(k-1) * 1.d0
		enddo
		do k = CenterLine-1, kls1,-1
		dze(k) = dze(k+1) * 1.d0
		enddo
		
		ds = 5.d0/100.d0
		do k = kts1+1,kts2
			dze(k) = dze(k-1) * (1+ ( dtanh( 4.d0* (k-(kts1+kts2)/2.d0) / ( (kts2-kts1)/2.d0 ) ) *ds/2.d0 + ds/2.d0 ))
		enddo
		ds = 0.d0/100.d0
		do k = kls1-1,kls2,-1
		dze(k) = dze(k+1) * (1+ ( dtanh( 4.d0* (k-(kls1+kls2)/2.d0) / ( (kls2-kls1)/2.d0 ) ) *ds/2.d0 + ds/2.d0 ))
		enddo
		
		do k = kts2+1,nzp2-15
			dze(k) = dze(k-1)* 1.05d0
		enddo
		do k = nzp2-14,nzp2
			dze(k) = dze(k-1)* 1.0d0
		enddo
		
		do k = kls2-1,11,-1
		dze(k) = dze(k+1) * 1.0d0
		enddo
		do k = 10,1,-1
		dze(k) = dze(k+1) * 1.0d0
		enddo
		
		
		ze(CenterLine) = 0.46d0
		do k = CenterLine+1,nzp2
		ze(k) = ze(k-1) + dze(k)
		enddo
		do k= CenterLine-1,1,-1
		ze(k) = ze(k+1) - dze(k+1)
		enddo
		zL = ze(nzp2)-ze(1)
   
   case('sphere')
   		z0 = -1.d0
		CenterLine = nzp2/2
		kls1 = CenterLine - 24
		kts1 = CenterLine + 25 
		kls2 = kls1 - 6
		kts2 = kts1 + 6
		dze(CenterLine) = 0.2d0/30.d0	!0.01d0 !zL/dble(nzp2-1)
		
		do k = CenterLine+1, kts1
			dze(k) = dze(k-1) * 1.0d0
		enddo
		do k = CenterLine-1, kls1,-1
		dze(k) = dze(k+1) * 1.0d0
		enddo
		
		ds = 5.d0/100.d0
		do k = kts1+1,kts2
			dze(k) = dze(k-1) * (1+ ( dtanh( 4.d0* (k-(kts1+kts2)/2.d0) / ( (kts2-kts1)/2.d0 ) ) *ds/2.d0 + ds/2.d0 ))
		enddo
		ds = 5.d0/100.d0
		do k = kls1-1,kls2,-1
		dze(k) = dze(k+1) * (1+ ( dtanh( 4.d0* (k-(kls1+kls2)/2.d0) / ( (kls2-kls1)/2.d0 ) ) *ds/2.d0 + ds/2.d0 ))
		enddo
		
		do k = kts2+1,nzp2-11
			dze(k) = dze(k-1)* 1.05d0
		enddo
		do k = kls2-1,11,-1
		dze(k) = dze(k+1) * 1.05d0
		enddo
		
		do k = nzp2-10,nzp2
			dze(k) = dze(k-1)* 1.0d0
		enddo
		do k = 10,1,-1
		dze(k) = dze(k+1) * 1.0d0
		enddo
		
		
		ze(CenterLine) = 1.d0
		do k = CenterLine+1,nzp2
		ze(k) = ze(k-1) + dze(k)
		enddo
		do k= CenterLine-1,1,-1
		ze(k) = ze(k+1) - dze(k+1)
		enddo
		zL = ze(nzp2)-ze(1)
   
   case DEFAULT
   		print *, "The grid type is not correct"
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
  dzc(1) =dzc(2)
  rdzc(1)=rdzc(2)
  zc(1)  = ze(1)-0.5d0*dzc(1)
   
 do k=1,nzp2-1
  dze(k) = zc(k+1)-zc(k)
  rdze(k) = 1.d0/dze(k)
 enddo
! assign values for the last grid cell
  dze(nzp2) = dze(nzp2-1)
  rdze(nzp2) = rdze(nzp2-1)
 
 !Ensure No Stretching at First and Last Cells in X3
  dzc(nzp2) = dzc(nzp2-1)
  rdzc(nzp2) = rdzc(nzp2-1)
  dze(1)=dze(2)
  rdze(1)=rdze(2)
  ze(nzp2)=ze(nzp2-1)+dzc(nzp2)
  ze(1)=ze(2)-dzc(2)
  zc(nzp2)=zc(nzp2-1)+dze(nzp2-1)
  zc(1)=zc(2)-dze(1)

 ! enforce fixed dx,rdx for uniform grids
  if(Gridtype(1).eq.'uniform') then 
    dxc=dx_uni
    dxe=dx_uni
   rdxc=1.d0/dx_uni
   rdxe=1.d0/dx_uni
  endif

  if(Gridtype(2).eq.'uniform') then 
    dyc=dy_uni
    dye=dy_uni
   rdyc=1.d0/dy_uni
   rdye=1.d0/dy_uni
  endif

  if(Gridtype(3).eq.'uniform') then 
    dzc=dz_uni
    dze=dz_uni
   rdzc=1.d0/dz_uni
   rdze=1.d0/dz_uni
  endif

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
   write(207,'(a1,a4,6(a24),x,a15)') '#','i','xe(i)','xc(i)','dxc(i)','dxc(i)','rdxe(i)','rdxc(i)','dxc(i)/dxc(i-1)'
   write(207,'(i6)') nxp2
   write(207,'(f22.16)') xc(nxp2)-xc(1)
   write(207,'(d22.16)') xc(1)
   write(207,*) Gridtype(1)
   write(207,'(i5,6(2x,d22.16),2x,a15)') 1,xe(1),xc(1),dxe(1),dxc(1),rdxe(1),rdxc(1),'n/a'
   do i=2,nxp2
    write(207,'(i5,6(2x,d22.16),2x,f15.8)') i,xe(i),xc(i),dxe(i),dxc(i),rdxe(i),rdxc(i),dxc(i)/dxc(i-1)
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
   write(206,'(a1,a4,6(a24),x,a15)') '#','j','ye(j)','yc(j)','dyc(j)','dyc(j)','rdye(j)','rdyc(j)','dyc(j)/dyc(j-1)'
   write(206,'(i6)') nyp2
   write(206,'(f22.16)') yc(nyp2)-yc(1)
   write(206,'(d22.16)') yc(1)
   write(206,*) Gridtype(2)
   write(206,'(i5,6(2x,d22.16),2x,a15)') 1,ye(1),yc(1),dye(1),dyc(1),rdye(1),rdyc(1),'n/a'
   do j=2,nyp2
    write(206,'(i5,6(2x,d22.16),2x,f15.8)') j,ye(j),yc(j),dye(j),dyc(j),rdye(j),rdyc(j),dyc(j)/dyc(j-1)
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
   write(205,'(a1,a4,6(a24),x,a15)') '#','k','ze(k)','zc(k)','dzc(k)','dzc(k)','rdze(k)','rdzc(k)','dzc(k)/dzc(k-1)'
   write(205,'(i6)') nzp2
   write(205,'(f22.16)') zc(nzp2)-zc(1)
   write(205,'(d22.16)') zc(1)
   write(205,*) Gridtype(3)
   select case(GridType(1))
    case('uniform','stretch1','percentstretch','uniformstretch','uniformstretchinsteps')
      write(205, '(i5,6(2x,d22.16),2x,a15)') 1,ze(1),zc(1),dze(1),dzc(1),rdze(1),rdzc(1),'n/a'
     do k=2,nzp2
      write(205, '(i5,6(2x,d22.16),2x,f15.8)') k,ze(k),zc(k),dze(k),dzc(k),rdze(k),rdzc(k),dzc(k)/dzc(k-1)
     enddo
    case('custom')
     do k=1,CenterLine
      write(205, '(i5,6(2x,d22.16),2x,f15.8)') k,ze(k),zc(k),dze(k),dzc(k),rdze(k),rdzc(k),dzc(k-1)/dzc(k)
     enddo
     do k=CenterLine+1,nzp2
      write(205, '(i5,6(2x,d22.16),2x,f15.8)') k,ze(k),zc(k),dze(k),dzc(k),rdze(k),rdzc(k),dzc(k)/dzc(k-1)
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

