












subroutine ghost(var,vtype,stat)
!@t
! \textbf{subroutine ghost(var,vtype,stat)}
!@h
!   Description:
!     Updates ghost values for parallel version or does the equivalent for 
!     the serial version.
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
!     The original boundary conditions are overwritten by ghosting and are
!     reapplied after ghosting is completed.
!@q

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez
 use IO,     only: IOUT
 use boundC
 implicit none
 
!Passed Variables
 real(r8),intent(inout)     :: var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 character(len=*)           :: vtype
 integer,intent(out)        :: stat

!Local Variables
 integer                    :: l, m, n, rt 
 integer                    :: err1,err2

 err1=0
 err2=0

!Make all BCs periodic just like ghosting would, they will be corrected 
 !in the rest of the subroutine
 !X1
 var(sx-1,:,:)=var(ex,:,:)
 var(ex+1,:,:)=var(sx,:,:)
 !X2
 var(:,sy-1,:)=var(:,ey,:)
 var(:,ey+1,:)=var(:,sy,:)
 !X3
 var(:,:,sz-1)=var(:,:,ez)
 var(:,:,ez+1)=var(:,:,sz)

!UPDATE DIRICHLET/NEUMANN BOUNDARY CONDITIONS SINCE GHOSTING MAKES THEM ALL PERIODIC
rt=0
select case(vtype)
 case('u','uc')
  l=1
 case('v','vc')
  l=2
 case('w','wc')
  l=3
 case('p')
  l=4
 case('r','rho','rf')
  l=5
  rt=1
! case('cfluc','fluc','rp')
!  l=6
 case('cfluc','fluc')
  l=6
 case('rp')
  l=6
  rt=1
 case('psource')
  l=7
 case('utemp','vtemp','wtemp','rtemp','s1temp')
  l=7
  !call calc_bound(var,vtype,VB(:,:,7),err1)
 case('scal1')
  l=8
  rt=1
 case('scal1temp')
  l=8
 case DEFAULT
  write(IOUT,'(a)') "BC TYPE NOT AVAILABLE (ghost.f90): "//trim(vtype)
end select

n=1 
 !X1-min
 m=1
  if ( TB(n,1,l).NE.3) then
   call bound_update(var,TB(n,m,l),m,n,VB(n,m,l),vtype,rt,err2 )
  elseif ( TB(n,1,l).EQ.3.AND.m.EQ.1 ) then !Periodic boundary, no need to call twice
   call bound_update(var,TB(n,m,l),m,n,VB(n,m,l),vtype,rt,err2 )
  endif

 !X1-max
 m=2
  if ( TB(n,1,l).NE.3) then
   call bound_update(var,TB(n,m,l),m,n,VB(n,m,l),vtype,rt,err2 )
  elseif ( TB(n,1,l).EQ.3.AND.m.EQ.1 ) then !Periodic boundary, no need to call twice
   call bound_update(var,TB(n,m,l),m,n,VB(n,m,l),vtype,rt,err2 )
  endif

n=2
 !X2-min
 m=1
  if ( TB(n,1,l).NE.3) then
   call bound_update(var,TB(n,m,l),m,n,VB(n,m,l),vtype,rt,err2 )
  elseif ( TB(n,1,l).EQ.3.AND.m.EQ.1 ) then !Periodic boundary, no need to call twice
   call bound_update(var,TB(n,m,l),m,n,VB(n,m,l),vtype,rt,err2 )
  endif

 !X2-max
 m=2
  if ( TB(n,1,l).NE.3) then
   call bound_update(var,TB(n,m,l),m,n,VB(n,m,l),vtype,rt,err2 )
  elseif ( TB(n,1,l).EQ.3.AND.m.EQ.1 ) then !Periodic boundary so only call once, no need to call twice
   call bound_update(var,TB(n,m,l),m,n,VB(n,m,l),vtype,rt,err2 )
  endif
! call MPI_BARRIER(comm3d,err1)
!  if (err1.NE.0.or.err2.NE.0) goto 9999

n=3
 !X3-min
 m=1
  if ( TB(n,1,l).NE.3) then
   call bound_update(var,TB(n,m,l),m,n,VB(n,m,l),vtype,rt,err2 )
  elseif ( TB(n,1,l).EQ.3.AND.m.EQ.1 ) then !Periodic boundary, no need to call twice
   call bound_update(var,TB(n,m,l),m,n,VB(n,m,l),vtype,rt,err2 )
  endif

 !X3-max
 m=2
  if ( TB(n,1,l).NE.3) then
   call bound_update(var,TB(n,m,l),m,n,VB(n,m,l),vtype,rt,err2 )
  elseif ( TB(n,1,l).EQ.3.AND.m.EQ.1 ) then !Periodic boundary so only call once, no need to call twice
   call bound_update(var,TB(n,m,l),m,n,VB(n,m,l),vtype,rt,err2 )
  endif

 !normal exit
 stat=max(err1,err2)
 return

 !Unclean
 9999 continue
 write(IOUT,'(a,2(1x,i4))') "ERROR (ghost.f90) for var: "//trim(vtype)," err1/err2: ",err1,err2
 stat=max(err1,err2)
 return

end subroutine Ghost

subroutine calc_bound(var,vtype,Vbound,stat)
!@t
! \textbf{subroutine calc\_bound(var,vtype,Vbound,stat)}
!@h
!   Description:
!     Does something with the boundary area and value????
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
!     KYLE CAN YOU ADD A FEW MORE DETAILS ON WHAT THIS DOES / HOW TO USE IT???
!@q

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez
 use grid,   only: dxe,dye,dze,dxc,dyc,dzc,xc,xe,yc,zc,ze,ye,ze
 use IO,     only: IOUT
 implicit none
 
 !Passed Variables
 real(r8),intent(in)           :: var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)          :: Vbound(1:3,1:2)
 integer,intent(out)           :: stat
 character(len=*),intent(in)   :: vtype
 !Local Variables
 real(r8)                      :: val, area
 integer                       :: i,j,k
 real(r8),dimension(:),pointer :: dxt,dyt,dzt,xt,yt,zt

 stat=0

 Vbound(:,:)=0.d0

 select case(vtype)
  case('u','utemp','uc')
   dxt=>dxc
   dyt=>dye
   dzt=>dze
   xt=>xe
   yt=>yc
   zt=>zc
  case('v','vtemp','vc')
   dxt=>dxe
   dyt=>dyc
   dzt=>dze
   xt=>xc
   yt=>ye
   zt=>zc
  case('w','wtemp','wc')
   dxt=>dxe
   dyt=>dye
   dzt=>dzc
   xt=>xc
   yt=>yc
   zt=>ze
  case('c','p','psource','fluc','rho','rp','rf','rtemp','cfluc','scal1','scal1temp')
   dxt=>dxe
   dyt=>dye
   dzt=>dze
   xt=>xc
   yt=>yc
   zt=>zc
  case DEFAULT
   write(IOUT,'(a)') "CALC BOUND TYPE NOT AVAILABLE: "//trim(vtype)
   return
 end select

 !X2-min
 val=0.d0
 area=0.d0
 do k=sz,ez
  do i=sx,ex
    val  = val + dxt(i)*dzt(k)*( var(i,sy+1,k)-var(i,sy,k) )/ ( yt(sy+1)-yt(sy) )
    area = area + dxt(i)*dzt(k)
  enddo
 enddo
 Vbound(2,1)=val/area
 
 !X2-max
 val=0.d0
 area=0.d0
 do k=sz,ez
  do i=sx,ex
    val  = val + dxt(i)*dzt(k)*( var(i,ey,k)-var(i,ey-1,k) )/( yt(ey)-yt(ey-1) )
    area = area + dxt(i)*dzt(k)
  enddo
 enddo
 Vbound(2,2)=val/area

 !X3-min
 val=0.d0
 area=0.d0
 do j=sy,ey
  do i=sx,ex
    val  = val + dxt(i)*dyt(j)*( var(i,j,sz+1)-var(i,j,sz) )/( zt(sz+1)-zt(sz) )
    area = area + dxt(i)*dyt(j)
  enddo
 enddo
 Vbound(3,1)=val/area

 !X3-max
 val=0.d0
 area=0.d0
 do j=sy,ey
  do i=sx,ex
    val  = val + dxt(i)*dyt(j)*( var(i,j,ez)-var(i,j,ez-1) )/( zt(ez)-zt(ez-1) )
    area = area + dxt(i)*dyt(j)
  enddo
 enddo
 Vbound(3,2)=val/area

 !X1-min
 val=0.d0
 area=0.d0
 do k=sz,ez
  do j=sy,ey
    val  = val +  dyt(j)*dzt(k)*( var(sx+1,j,k)-var(sx,j,k) )/( xt(sx+1)-xt(sx) )
    area = area + dyt(j)*dzt(k)
  enddo
 enddo
 Vbound(1,1)=val/area

 !X1-max
 val=0.d0
 area=0.d0
 do k=sz,ez
  do j=sy,ey
    val  = val + dyt(j)*dzt(k)*( var(ex,j,k)-var(ex-1,j,k) )/( xt(ex)-xt(ex-1) )
    area = area + dyt(j)*dzt(k)
  enddo
 enddo
 Vbound(1,2)=val/area

return
end subroutine calc_bound

subroutine bound_update(var,Btype,face,dir,value,vtype,rtype,stat)
!subroutine bound_update(var,Btype,face,dir,value,vtype,stat)
!@t
! \textbf{subroutine bound\_update(var,Btype,face,dir,value,vtype,stat)}
!@h
!   Description:
!     Updates the boundary conditions for a given variable.
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
!     KYLE CAN YOU ADD A FEW MORE DETAILS ON WHAT THIS DOES / HOW TO USE IT???
!@q

 use ntypes,  only: r8  
 use Domain,  only: sx,ex,sy,ey,sz,ez,EU,EV,EW,nx,ny,nz
 use IO,      only: IOUT
 use Grid,    only: dxe, dye, dze, dxc, dyc, dzc, xe, xc, ye, yc, ze, zc
 use boundc,  only: BND
  implicit none

 !Passed Variables
 real(r8),intent(inout)        :: var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)           :: value
 integer,intent(in)            :: Btype,dir,face, rtype
 integer,intent(out)           :: stat
 character(len=*),intent(in)   :: vtype
 !Local Variables
 integer                       :: ierr4, of1,of2,iU,iV,iW
 real(r8),dimension(:),pointer :: dxt,dyt,dzt,xt,yt,zt

 ierr4=0

 of1=0
 of2=0
 iU=0
 iV=0
 iW=0
 select case(vtype)
  case('u')
   dxt=>dxc
   dyt=>dye
   dzt=>dze
   xt=>xe
   yt=>yc
   zt=>zc
   of2=1
   iU=EU
  case('utemp')
   dxt=>dxc
   dyt=>dye
   dzt=>dze
   xt=>xe
   yt=>yc
   zt=>zc
   of2=1
   iU=EU
  case('v')
   dxt=>dxe
   dyt=>dyc
   dzt=>dze
   xt=>xc
   yt=>ye
   zt=>zc
   of2=1
   iV=EV
  case('vtemp')
   dxt=>dxe
   dyt=>dyc
   dzt=>dze
   xt=>xc
   yt=>ye
   zt=>zc
   of2=1
   iV=EV
  case('w')
   dxt=>dxe
   dyt=>dye
   dzt=>dzc
   xt=>xc
   yt=>yc
   zt=>ze
   of2=1
   iW=EW
  case('wtemp')
   dxt=>dxe
   dyt=>dye
   dzt=>dzc
   xt=>xc
   yt=>yc
   zt=>ze
   of2=1
   iW=EW
  case('c','p','psource','fluc','rho','rp','rf','uc','vc','wc','rtemp','cfluc','scal1','scal1temp','s1temp')
   dxt=>dxe
   dyt=>dye
   dzt=>dze
   xt=>xc
   yt=>yc
   zt=>zc
   of1=-1
  case DEFAULT
   write(IOUT,'(a)') "BOUND TYPE: "//trim(vtype)//" NOT AVAILABLE (bound_update)"
   return
 end select

!X1-Direction
 if (dir.EQ.1) then  !0
   if (Btype.EQ.1) then !1
    if ( BND(dir,face) .AND. face .EQ. 1 ) var(sx-1,:,:) = value
    if ( BND(dir,face) .AND. face .EQ. 2 ) var(ex+1-iU,:,:) = value
   elseif (Btype.EQ.2) then !1
    if ( BND(dir,face) .AND. face .EQ. 1 ) then 
      if (rtype.EQ.1) var(sx,:,:)   = var(sx+1,:,:)-value*dxt(sx+1+of1)
      var(sx-1,:,:) = var(sx,:,:)-value*dxt(sx+of1)
    endif
    if ( BND(dir,face) .AND. face .EQ. 2 ) then 
      if (rtype.EQ.1) var(ex-iU,:,:)   = var(ex-1-iU,:,:)+value*dxt(ex-1+of2-iU)
      var(ex+1-iU,:,:) = var(ex-iU,:,:)+value*dxt(ex+of2-iU)
    endif
   elseif (Btype.EQ.3) then !1
      var(sx-1,:,:) = var(ex-iU,:,:) 
      var(ex+1-iU,:,:) = var(sx,:,:)
   elseif (Btype.EQ.4) then !1
    if ( BND(dir,face) .AND. face .EQ. 1 ) var(sx-1,:,:) = var(sx,:,:)-value*dxt(sx+of1)
    if ( BND(dir,face) .AND. face .EQ. 2 ) var(ex+1-iU,:,:) = var(ex-iU,:,:)+value*dxt(ex+of2-iU)
   elseif (Btype.EQ.8) then !1
    if ( BND(dir,face) .AND. face .EQ. 1 ) var(sx-1,:,:) = 2.d0*var(sx,:,:)-var(sx+1,:,:)
    if ( BND(dir,face) .AND. face .EQ. 2 ) var(ex+1-iU,:,:) = 2.d0*var(ex-iU,:,:)-var(ex-iU-1,:,:)
  
 else !1
    write(IOUT,'(a16,i2,a24,i2)') "ERROR: BC TYPE: ", Btype, " is not (1,2,3) for dir, ",dir 
   endif !1

   if ( BND(dir,face) .AND. face .EQ. 2 .AND. iU.EQ.1 ) var(ex+1,:,:) = 0.d0 !SET EXTRA POINT TO ZERO

!X2-Direction
 elseif (dir.EQ.2) then !0
   if (Btype.EQ.1) then !1
    if ( BND(dir,face) .AND. face .EQ. 1 )  var(:,sy-1,:) = value
    if ( BND(dir,face) .AND. face .EQ. 2 ) var(:,ey+1-iV,:) = value
   elseif (Btype.EQ.2) then !1
    if ( BND(dir,face) .AND. face .EQ. 1 ) then 
      if (rtype.EQ.1) var(:,sy,:)   = var(:,sy+1,:)-value*dyt(sy+1+of1) !KYLE ARE THESE INDICES CORRECT???
      var(:,sy-1,:) = var(:,sy,:)-value*dyt(sy+of1)
    endif
    if ( BND(dir,face) .AND. face .EQ. 2 ) then 
      if (rtype.EQ.1)  var(:,ey-iV,:)   = var(:,ey-1-iV,:)+value*dyt(ey-1+of2-iV) !KYLE ARE THESE INDICES CORRECT???
      var(:,ey+1-iV,:) = var(:,ey-iV,:)+value*dyt(ey+of2-iV)
    endif
   elseif (Btype.EQ.3) then !1
      var(:,sy-1,:) = var(:,ey-iV,:) 
      var(:,ey+1-iV,:) = var(:,sy,:)
   elseif (Btype.EQ.4) then !1
    if ( BND(dir,face) .AND. face .EQ. 1 ) var(:,sy-1,:) = var(:,sy,:)-value*dyt(sy+of1)
    if ( BND(dir,face) .AND. face .EQ. 2 ) var(:,ey+1-iV,:) = var(:,ey-iV,:)+value*dyt(ey+of2-iV)
   else !1
    write(IOUT,'(a16,i2,a24,i2)') "ERROR: BC TYPE: ", Btype, " is not (1,2,3) for dir, ",dir 
   endif !1

   if ( BND(dir,face) .AND. face .EQ. 2 .AND. iV.EQ.1 ) var(:,ey+1,:) = 0.d0 !SET EXTRA POINT TO ZERO

!X3-Direction
 elseif (dir.EQ.3) then !0
   if (Btype.EQ.1) then !1
    if ( BND(dir,face) .AND. face .EQ. 1 ) var(:,:,sz-1) = value
    if ( BND(dir,face) .AND. face .EQ. 2 ) var(:,:,ez+1-iW) = value 
   elseif (Btype.EQ.2) then !1
    if ( BND(dir,face) .AND. face .EQ. 1 ) then 
      !Important: Have to force Neumman condition at 2 points for rho, 1 points only is unstable
      if (rtype.EQ.1) var(:,:,sz)   = var(:,:,sz+1)-value*dzt(sz+1+of1)
      var(:,:,sz-1) = var(:,:,sz)-value*dzt(sz+of1)
    endif
    if ( BND(dir,face) .AND. face .EQ. 2 ) then 
      !Important: Have to force Neumman condition at 2 points for rho, 1 points only is unstable
       if (rtype.EQ.1) var(:,:,ez-iW)   = var(:,:,ez-1-iW)+value*dzt(ez-1+of2-iW)
       var(:,:,ez+1-iW) = var(:,:,ez-iW)+value*dzt(ez+of2-iW)
    endif
  elseif (Btype.EQ.3) then !1
      var(:,:,sz-1) = var(:,:,ez-iW) 
      var(:,:,ez+1-iW) = var(:,:,sz)  
   elseif (Btype.EQ.4) then !1
    if ( BND(dir,face) .AND. face .EQ. 1 ) var(:,:,sz-1) = var(:,:,sz)-value*dzt(sz+of1)
    if ( BND(dir,face) .AND. face .EQ. 2 ) var(:,:,ez+1-iW) = var(:,:,ez-iW)+value*dzt(ez+of2-iW)
   !wall6
   elseif (Btype.EQ.6) then !1
    if ( BND(dir,face) .AND. face .EQ. 1 ) var(:,:,sz-1) = 2.d0*value-1.d0*var(:,:,sz)
    if ( BND(dir,face) .AND. face .EQ. 2 ) var(:,:,ez+1-iW) = 2.d0*value-1.d0*var(:,:,ez-iW)
   !wall7 
   elseif (Btype.EQ.7) then !1
!    if ( BND(dir,face) .AND. face .EQ. 1 ) var(:,:,sz-1) = -1.d0*var(:,:,sz+1)
!    if ( BND(dir,face) .AND. face .EQ. 2 ) var(:,:,ez+1-iW) = -1.d0*var(:,:,ez-iW-1)
    if ( BND(dir,face) .AND. face .EQ. 1 ) then
       var(:,:,sz) = 0.d0
!       var(:,:,sz-1) = 0.d0
       var(:,:,sz-1) = -1.d0*var(:,:,sz+1)
    endif
    if ( BND(dir,face) .AND. face .EQ. 2 ) then
       var(:,:,ez) = 0.d0
!       var(:,:,ez+1-iW) = 0.d0
       var(:,:,ez+1-iW) = -1.d0*var(:,:,ez-iW-1)
    endif
   else !1
    write(IOUT,'(a16,i2,a24,i2)') "ERROR: BC TYPE: ", Btype, " is not (1,2,3) for dir, ",dir 
   endif !1

   if ( BND(dir,face) .AND. face .EQ. 2 .AND. iW.EQ.1 ) var(:,:,ez+1) = 0.d0 !SET EXTRA POINT TO ZERO 

 else !0
  write(IOUT,'(a16,i2,a15)') "ERROR: DIRECTION: ", dir, " is not (1,2,3)" 
  stop
 endif !0
 stat=ierr4
 return
end subroutine bound_update




subroutine bc(infile,stat)
!@t
! \textbf{subroutine bc(infile,stat)}
!@h
!   Description:
!     Reads in BCs from an inifile and updates them in the code. 
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
!     KYLE CAN YOU ADD A FEW MORE DETAILS ON WHAT THIS DOES / HOW TO USE IT???
!@q

 use Domain, only: sx,ex,sy,ey,sz,ez,nx,ny,nz
 use boundC
 !use dd,  only: BND
 use IO, only: IOUT
 use parameters, only: denP1
 implicit none

!Passed Variables
 integer,intent(out) :: stat
 character(len=*)    :: infile

!Local Variables
 character(len=100)    :: BCtype, inBC, block
 character(len=5),parameter  :: defval='ERROR'

 integer                     :: IOUTL

 logical :: debug = .false.

 stat=0

 if (debug) then
  IOUTL=IOUT !output to screen or output file
 else
  IOUTL=0 !redirect to stderr
 endif

 !PRESSURE SOURCE TYPE AND VALUE
 TB(:,:,7)=1
 VB(:,:,7)=0 

!TB(direction,face,variable)=type of boundary condition
!VB(direction,face,variable)=value of boundary condition

!Set Boundary Condition types and values
!1=Dirichlet
!2=Neumann
!3=periodic
!4=P=0 at a wall 1/2 between si and si-1 or ei and ei+1

!SCAN THE INPUT FILE FOR VALUES
![BCs]
!*******************************************************************************
!**********************************START X1 BC's********************************
!*******************************************************************************
 block='BCs'
 call scaninichar(infile,block,'X1',BCtype,defval,IOUTL)
 select case(BCtype)
 case('Periodic','periodic','per')
  TB(1,:,:) = 3
  VB(1,:,:) = -1
 case('Read','read')  !READ X1 BC's
  block='X1min'
   !U1
   call scaninichar(infile,block,'U1TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,1,1)=1
    case('Neumann','neumann','neu')
     TB(1,1,1)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U1 AT X1min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U1VALUE',VB(1,1,1),defval,IOUTL)
   !U2
   call scaninichar(infile,block,'U2TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,1,2)=1
    case('Neumann','neumann','neu')
     TB(1,1,2)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U2 AT X1min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U2VALUE',VB(1,1,2),defval,IOUTL)
   !U3
   call scaninichar(infile,block,'U3TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,1,3)=1
    case('Neumann','neumann','neu')
     TB(1,1,3)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U3 AT X1min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U3VALUE',VB(1,1,3),defval,IOUTL)
   !P
   call scaninichar(infile,block,'PTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,1,4)=1
    case('Neumann','neumann','neu')
     TB(1,1,4)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR P AT X1min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'PVALUE',VB(1,1,4),defval,IOUTL)
   !RHO
   call scaninichar(infile,block,'RHOTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,1,5)=1
    case('Neumann','neumann','neu')
     TB(1,1,5)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR RHO AT X1min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'RHOVALUE',VB(1,1,5),defval,IOUTL)
   !FLUC
   call scaninichar(infile,block,'FLUCTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,1,6)=1
    case('Neumann','neumann','neu')
     TB(1,1,6)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR FLUC's AT X1min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'FLUCVALUE',VB(1,1,6),defval,IOUTL)
   !SCAL1
   call scaninichar(infile,block,'SCAL1TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,1,8)=1
    case('Neumann','neumann','neu')
     TB(1,1,8)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR SCAL1's AT X1min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'SCAL1VALUE',VB(1,1,8),defval,IOUTL)

  block='X1max'
   !U1
   call scaninichar(infile,block,'U1TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,2,1)=1
    case('Neumann','neumann','neu')
     TB(1,2,1)=2
    case('Etpl8','etpl8')
     TB(1,2,1)=8
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U1 AT X1max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U2VALUE',VB(1,1,2),defval,IOUTL)
   !U2
   call scaninichar(infile,block,'U2TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,2,2)=1
    case('Neumann','neumann','neu')
     TB(1,2,2)=2
    case('Etpl8','etpl8')
     TB(1,2,2)=8
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U2 AT X1max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U1VALUE',VB(1,2,2),defval,IOUTL)
   !U3
   call scaninichar(infile,block,'U3TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,2,3)=1
    case('Neumann','neumann','neu')
     TB(1,2,3)=2
    case('Etpl8','expl8')
     TB(1,2,3)=8
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U3 AT X1max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U3VALUE',VB(1,2,3),defval,IOUTL)
   !P
   call scaninichar(infile,block,'PTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,2,4)=1
    case('Neumann','neumann','neu')
     TB(1,2,4)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR P AT X1max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'PVALUE',VB(1,2,4),defval,IOUTL)
   !RHO
   call scaninichar(infile,block,'RHOTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,2,5)=1
    case('Neumann','neumann','neu')
     TB(1,2,5)=2
    case('Etpl8','etpl8')
     TB(1,2,5)=8
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR RHO AT X1max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'RHOVALUE',VB(1,2,5),defval,IOUTL)
   !FLUC
   call scaninichar(infile,block,'FLUCTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,2,6)=1
    case('Neumann','neumann','neu')
     TB(1,2,6)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR FLUC's AT X1max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'FLUCVALUE',VB(1,2,6),defval,IOUTL)
   !SCAL1
   call scaninichar(infile,block,'SCAL1TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(1,2,8)=1
    case('Neumann','neumann','neu')
     TB(1,2,8)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR SCAL1's AT X1max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'SCAL1VALUE',VB(1,2,8),defval,IOUTL)

 case DEFAULT
  write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR X1 DIRECTION"
  stop
 end select
!*******************************************************************************
!***********************************END X1 BC's*********************************
!*******************************************************************************

!*******************************************************************************
!**********************************START X2 BC's********************************
!*******************************************************************************
 block='BCs'
 call scaninichar(infile,block,'X2',BCtype,defval,IOUTL)
 select case(BCtype)
 case('Periodic','periodic','per')
  TB(2,:,:) = 3
  VB(2,:,:) = -1
 case('Read','read')  !READ X2 BC's
  block='X2min'
   !U1
   call scaninichar(infile,block,'U1TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,1,1)=1
    case('Neumann','neumann','neu')
     TB(2,1,1)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U1 AT X2min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U1VALUE',VB(2,1,1),defval,IOUTL)
   !U2
   call scaninichar(infile,block,'U2TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,1,2)=1
    case('Neumann','neumann','neu')
     TB(2,1,2)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U2 AT X2min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U2VALUE',VB(2,1,2),defval,IOUTL)
   !U3
   call scaninichar(infile,block,'U3TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,1,3)=1
    case('Neumann','neumann','neu')
     TB(2,1,3)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U3 AT X2min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U3VALUE',VB(2,1,3),defval,IOUTL)
   !P
   call scaninichar(infile,block,'PTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,1,4)=1
    case('Neumann','neumann','neu')
     TB(2,1,4)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR P AT X2min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'PVALUE',VB(2,1,4),defval,IOUTL)
   !RHO
   call scaninichar(infile,block,'RHOTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,1,5)=1
    case('Neumann','neumann','neu')
     TB(2,1,5)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR RHO AT X2min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'RHOVALUE',VB(2,1,5),defval,IOUTL)
   !FLUC
   call scaninichar(infile,block,'FLUCTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,1,6)=1
    case('Neumann','neumann','neu')
     TB(2,1,6)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR FLUC's AT X2min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'FLUCVALUE',VB(2,1,6),defval,IOUTL)
   !SCAL1
   call scaninichar(infile,block,'SCAL1TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,1,8)=1
    case('Neumann','neumann','neu')
     TB(2,1,8)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR SCAL1's AT X2min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'SCAL1VALUE',VB(2,1,8),defval,IOUTL)

  block='X2max'
   !U1
   call scaninichar(infile,block,'U1TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,2,1)=1
    case('Neumann','neumann','neu')
     TB(2,2,1)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U1 AT X2max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U1VALUE',VB(2,2,1),defval,IOUTL)
   !U2
   call scaninichar(infile,block,'U2TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,2,2)=1
    case('Neumann','neumann','neu')
     TB(2,2,2)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U2 AT X2max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U2VALUE',VB(2,2,2),defval,IOUTL)
   !U3
   call scaninichar(infile,block,'U3TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,2,3)=1
    case('Neumann','neumann','neu')
     TB(2,2,3)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U3 AT X2max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U3VALUE',VB(2,2,3),defval,IOUTL)
   !P
   call scaninichar(infile,block,'PTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,2,4)=1
    case('Neumann','neumann','neu')
     TB(2,2,4)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR P AT X2max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'PVALUE',VB(2,2,4),defval,IOUTL)
   !RHO
   call scaninichar(infile,block,'RHOTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,2,5)=1
    case('Neumann','neumann','neu')
     TB(2,2,5)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR RHO AT X2max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'RHOVALUE',VB(2,2,5),defval,IOUTL)
   !FLUC
   call scaninichar(infile,block,'FLUCTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,2,6)=1
    case('Neumann','neumann','neu')
     TB(2,2,6)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR FLUC's AT X2max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'FLUCVALUE',VB(2,2,6),defval,IOUTL)
   !SCAL1
   call scaninichar(infile,block,'SCAL1TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(2,2,8)=1
    case('Neumann','neumann','neu')
     TB(2,2,8)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR SCAL1's AT X2max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'SCAL1VALUE',VB(2,2,8),defval,IOUTL)
 case DEFAULT
  write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR X2 DIRECTION"
  stop
 end select
!*******************************************************************************
!***********************************END X2 BC's*********************************
!*******************************************************************************

!*******************************************************************************
!**********************************START X3 BC's********************************
!*******************************************************************************
 block='BCs'
 call scaninichar(infile,block,'X3',BCtype,defval,IOUTL)
 select case(BCtype)
 case('Periodic','periodic','per')
  TB(3,:,:) = 3
  VB(3,:,:) = -1
 case('Read','read')  !READ X3 BC's
  block='X3min'
   !U1
   call scaninichar(infile,block,'U1TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,1,1)=1
    case('Neumann','neumann','neu')
     TB(3,1,1)=2
    case('Wall6','wall6')
     TB(3,1,1)=6
    case('Wall7','wall7')
     TB(3,1,1)=7
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U1 AT X3min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U1VALUE',VB(3,1,1),defval,IOUTL)
   !U2
   call scaninichar(infile,block,'U2TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,1,2)=1
    case('Neumann','neumann','neu')
     TB(3,1,2)=2
    case('Wall6','wall6')
     TB(3,1,2)=6
    case('Wall7','wall7')
     TB(3,1,2)=7
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U2 AT X3min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U2VALUE',VB(3,1,2),defval,IOUTL)
  !U3
   call scaninichar(infile,block,'U3TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,1,3)=1
    case('Neumann','neumann','neu')
     TB(3,1,3)=2
    case('Wall6','wall6')
     TB(3,1,3)=6
    case('Wall7','wall7')
     TB(3,1,3)=7
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U3 AT X3min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U3VALUE',VB(3,1,3),defval,IOUTL)
   !P
   call scaninichar(infile,block,'PTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,1,4)=1
    case('Neumann','neumann','neu')
     TB(3,1,4)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR P AT X3min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'PVALUE',VB(3,1,4),defval,IOUTL)
   !RHO
   call scaninichar(infile,block,'RHOTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,1,5)=1
    case('Neumann','neumann','neu')
     TB(3,1,5)=2
    case('Calc','calc','c')
     TB(3,1,5)=4
    case('Wall6','wall6')
     TB(3,1,5)=6
    case('Wall7','wall7')
     TB(3,1,5)=7
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR RHO AT X3min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'RHOVALUE',VB(3,1,5),defval,IOUTL)
   !FLUC
   call scaninichar(infile,block,'FLUCTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,1,6)=1
    case('Neumann','neumann','neu')
     TB(3,1,6)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR FLUC's AT X3min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'FLUCVALUE',VB(3,1,6),defval,IOUTL)
   !SCAL1
   call scaninichar(infile,block,'SCAL1TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,1,8)=1
    case('Neumann','neumann','neu')
     TB(3,1,8)=2
    case('Calc','calc','c')
     TB(3,1,8)=4
    case('Wall6','wall6')
     TB(3,1,8)=6
    case('Wall7','wall7')
     TB(3,1,8)=7
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR SCAL1 AT X3min, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'SCAL1VALUE',VB(3,1,8),defval,IOUTL)

  block='X3max'
   !U1
   call scaninichar(infile,block,'U1TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,2,1)=1
    case('Neumann','neumann','neu')
     TB(3,2,1)=2
    case('Wall6','wall6')
     TB(3,2,1)=6
    case('Wall7','wall7')
     TB(3,2,1)=7
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U1 AT X3max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U1VALUE',VB(3,2,1),defval,IOUTL)
   !U2
   call scaninichar(infile,block,'U2TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,2,2)=1
    case('Neumann','neumann','neu')
     TB(3,2,2)=2
    case('Wall6','wall6')
     TB(3,2,2)=6
    case('Wall7','wall7')
     TB(3,2,2)=7
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U2 AT X3max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U2VALUE',VB(3,2,2),defval,IOUTL)
   !U3
   call scaninichar(infile,block,'U3TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,2,3)=1
    case('Neumann','neumann','neu')
     TB(3,2,3)=2
    case('Wall6','wall6')
     TB(3,2,3)=6
    case('Wall7','wall7')
     TB(3,2,3)=7
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR U3 AT X3max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'U3VALUE',VB(3,2,3),defval,IOUTL)
   !P
   call scaninichar(infile,block,'PTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,2,4)=1
    case('Neumann','neumann','neu')
     TB(3,2,4)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR P AT X3max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'PVALUE',VB(3,2,4),defval,IOUTL)
   !RHO
   call scaninichar(infile,block,'RHOTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,2,5)=1
    case('Neumann','neumann','neu')
     TB(3,2,5)=2
    case('Calc','calc','c')
     TB(3,2,5)=4
    case('Wall6','wall6')
     TB(3,2,5)=6
    case('Wall7','wall7')
     TB(3,2,5)=7
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR RHO AT X3max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'RHOVALUE',VB(3,2,5),defval,IOUTL)
   !FLUC
   call scaninichar(infile,block,'FLUCTYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,2,6)=1
    case('Neumann','neumann','neu')
     TB(3,2,6)=2
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR FLUC's AT X3max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'FLUCVALUE',VB(3,2,6),defval,IOUTL)
   !SCAL1
   call scaninichar(infile,block,'SCAL1TYPE',inBC,defval,IOUTL)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     TB(3,2,8)=1
    case('Neumann','neumann','neu')
     TB(3,2,8)=2
    case('Calc','calc','c')
     TB(3,2,8)=4
    case('Wall6','wall6')
     TB(3,2,8)=6
    case('Wall7','wall7')
     TB(3,2,8)=7
    case DEFAULT
     write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR SCAL1 AT X3max, FATAL! "
     stop
   end select
   call scaninireal(infile,block,'SCAL1VALUE',VB(3,2,8),defval,IOUTL)

  case DEFAULT
   write(IOUTL,'(a)') "NO VALID BC'S FOUND FOR X3 DIRECTION, FATAL!"
   stop

  end select 
   
!*******************************************************************************
!***********************************END X3 BC's*********************************
!*******************************************************************************
!Determine if the node contains any of the 6 Computational Domain Boundaries
!X1-min
 if (sx.LE.2) then
  BND(1,1) = .true.
 else
  BND(1,1) = .false.
 endif

!X1-max
 if (ex.GE.nx) then
  BND(1,2) = .true.
 else
  BND(1,2) = .false.
 endif

!X2-min
 if (sy.LE.2) then
  BND(2,1) = .true.
 else
  BND(2,1) = .false.
 endif

!X2-max
 if (ey.GE.ny) then
  BND(2,2) = .true.
 else
  BND(2,2) = .false.
 endif

!X3-min
 if (sz.LE.2) then
  BND(3,1) = .true.
 else
  BND(3,1) = .false.
 endif

!X3-max
 if (ez.GE.nz) then
  BND(3,2) = .true.
 else
  BND(3,2) = .false.
 endif

 write(IOUT,'(a)') "BC SETUP COMPLETED"
 stat=0 
 return

 1000 continue
 write(IOUT,'(a)') "BC SETUP FAILED"

return
end subroutine bc
