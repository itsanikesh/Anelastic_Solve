












subroutine mg_coeff(lvl,stat)
!@t
! \textbf{subroutine mg\_coeff(lvl,stat)}
!@h
!   Description:
!     Sets the coefficients for the pressure solver at a given grid level
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
!     Hardcodes that the pressure coefficient at the endpoints is equal to
!     zero for Neumann BCs on the finest grid.

! MATT, DOUBLE CHECK ABOVE STATEMENT ASK KYLE WHERE THE BOTTOM SECTION 
! COMES FROM
!@q

! pcf_1 -> i-1
! pcf_2 -> i+1
! pcf_3 -> j-1
! pcf_4 -> j+1
! pcf_5 -> k-1
! pcf_6 -> k+1

 use ntypes, only: r8,i4
 use mgVars
 implicit none

!Passed Variables
 integer,intent(in)        :: lvl
 integer,intent(out)       :: stat

!Local Variables
 integer                   :: i,j,k
 real(r8)                  :: dxEast, dxWest, dyBack,      &
                              dyFront, dzNorth, dzSouth
 integer                   :: nxm,nym,nzm,is,ie,js,je,ks,ke

 real(r8),dimension(:),pointer :: dxeL,dxcL,dyeL,dycL,dzeL,dzcL
 real(r8),dimension(:),pointer :: p1, p2, p3, p4, p5, p6 

!Indicies at grid level=lvl
 nxm=nxk(lvl) 
 nym=nyk(lvl) 
 nzm=nzk(lvl) 

 is=kpx1(lvl)
 ie=is+nxm+1
 js=kpx2(lvl)
 je=js+nym+1
 ks=kpx3(lvl)
 ke=ks+nzm+1

 dxeL=>dxek(is:ie)
 dxcL=>dxck(is:ie)
 dyeL=>dyek(js:je)
 dycL=>dyck(js:je)
 dzeL=>dzek(ks:ke)
 dzcL=>dzck(ks:ke)

 p1=>pcf_1(is:ie)
 p2=>pcf_2(is:ie)
 p3=>pcf_3(js:je)
 p4=>pcf_4(js:je)
 p5=>pcf_5(ks:ke)
 p6=>pcf_6(ks:ke)

!X1-direction
 do i=2,nxm
  dxEast=dxeL(i)
  dxWest=dxeL(i-1)
  p1(i)=1.0d0/(dxcL(i)*dxWest)
  p2(i)=1.0d0/(dxcL(i)*dxEast)
 enddo

!X2-direction
 do j=2,nym
  dyFront=dyeL(j)
  dyBack=dyeL(j-1)
  p3(j)=1.0d0/(dycL(j)*dyBack)
  p4(j)=1.0d0/(dycL(j)*dyFront)
 enddo

!X3-direction
 do k=2,nzm
  dzNorth=dzeL(k)
  dzSouth=dzeL(k-1)
  p5(k)=1.0d0/(dzcL(k)*dzSouth)
  p6(k)=1.0d0/(dzcL(k)*dzNorth)
 enddo

 !X1min
  ! Tbc(5)
 !X1max
  ! Tbc(6)
  !X2min
  ! Tbc(3)
 !X2max
  ! Tbc(1)
 !X3min
  ! Tbc(2)
 !X3max
  !Tbc(4)
  
 !2=Dirichlet
 !1=Neumann
 !0=Periodic


 if (lvl.EQ.ngrid) then

  !NEUMANN BC at X1-min
!  if ( Tbc(5).EQ.1) p1(2)=vbc(5)   !0.d0
  if ( Tbc(5).EQ.1) p1(2)=0.d0

  !NEUMANN BC at X1-max
!  if ( Tbc(6).EQ.1) p2(nxm)=vbc(6) !0.d0
  if ( Tbc(6).EQ.1) p2(nxm)=0.d0

  !NEUMANN BC at X2-min
!  if ( Tbc(3).EQ.1) p3(2)=vbc(3)   !0.d0
  if ( Tbc(3).EQ.1) p3(2)=0.d0

  !NEUMANN BC at X2-max
!  if ( Tbc(1).EQ.1) p4(nym)=vbc(1) !0.d0
  if ( Tbc(1).EQ.1) p4(nym)=0.d0

  !NEUMANN BC at X3-min
!  if ( Tbc(2).EQ.1) p5(2)=vbc(2)   !0.d0
  if ( Tbc(2).EQ.1) p5(2)=0.d0

  !NEUMANN BC at X3-max
!  if ( Tbc(4).EQ.1) p6(nzm)=vbc(4) !0.d0
  if ( Tbc(4).EQ.1) p6(nzm)=0.d0

 endif

 stat = 0
return
end
