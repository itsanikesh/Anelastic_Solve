subroutine mg_hydro_setup(gRHOtop,gRHObottom)
!@t
! \textbf{subroutine mg\_hydro\_setup(gRHOtop,gRHObottom)}
!@h
!   Description:
!     Set Hydrostatic Pressure at top and bottom
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       03/2009  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
!@h
!   Comments:
!     KYLE IS THIS SUBROUTINE WORKING???
!@q


 use ntypes,    only: r8,i4
 use mgVars,    only: phibc,bdMG,ngrid,vbc

 implicit none

!Passed Variables
 real(r8),intent(in)       :: gRHOtop,gRHObottom 

!Local Variables

 !X3-min/X3-max BCs set to dp/dz=0
 bdMG(3) = 1
 bdMG(7) = 2 !1
 vbc(2) = gRHObottom
 vbc(4) = 1.d0  !gRHOtop
 phibc(ngrid,2)=vbc(2)
 phibc(ngrid,4)=vbc(4)
 
 return
end subroutine mg_hydro_setup

subroutine mg_set_Phydro(ptop,pbottom)
!@t
! \textbf{subroutine mg\_set\_Phydro(ptop,pbottom)}
!@h
!   Description:
!     Set Hydrostatic Pressure at top and bottom
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
 
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       03/2009  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
!@h
!   Comments:
!     KYLE IS THIS SUBROUTINE WORKING???
!@q

 use ntypes,    only: r8,i4
 use mgVars,    only: phibc,bdMG,ngrid, IOUTmg,vbc

 implicit none

!Passed Variables
 real(r8),intent(in)       :: ptop,pbottom 

!Local Variables
!                   /                  ^  ^
!       -----vbc(4)/-----              | / i
!       |         /     |              |/
!       |        /      |         -----/-----> j
!     vbc(3)----+-----vbc(1)          /|
!       |      /        |            / |
!       |     /         |           /  |
!       -----/vbc(2)----|
!           /
!         vbc(5)
!
!1 means dp/dn=0
!2 means p = bc 

 !X3-min
 if (bdMG(3).eq.2.or.bdMG(3).eq.1) then
  bdMG(3)=2
  phibc(ngrid,2)=pbottom
  vbc(2)=pbottom
 endif
   
 !X3-max
 if (bdMG(7).eq.2.or.bdMG(7).eq.1) then
  bdMG(7)=2
  phibc(ngrid,4)=ptop
  vbc(4)= ptop
 endif

 write(IOUTmg,'(a,d22.15)') "     MGSOLVER (mg_hydro): X3min bc modified to:", pbottom
 write(IOUTmg,'(a,d22.15)') "     MGSOLVER (mg_hydro): X3max bc modified to:", ptop

 return
end subroutine mg_set_Phydro
