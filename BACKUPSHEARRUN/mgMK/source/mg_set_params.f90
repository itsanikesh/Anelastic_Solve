












subroutine mg_parameters(stat)
!@t
! \textbf{subroutine mg\_parameters(stat)}
!@h
!   Description:
!     Set the number of grid levels, number of sweeps per grid level,
!     boundary conditions and tolerance for the solver.
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
!     This subroutine is outdated. It has been replaced by the ptest.ini
!     file and test/inputptest.F90.
!@q

 use mgVars
 
 implicit none

 !Passed Variables
 integer,intent(out) :: stat

 !MG Boundary Conditions
!                   vbc(6)            k 
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
!tbc(xx)
! xx=0 for periodic
! xx=1 for Neumann (dp/dn=0)
! xx=2 for Dirichlet (p=value)

 !TYPE
 tbc(1)=0
 tbc(2)=0
 tbc(3)=0
 tbc(4)=0
 tbc(5)=0
 tbc(6)=0

 !VALUE
 vbc(1)=0
 vbc(2)=1
 vbc(3)=0
 vbc(4)=2
 vbc(5)=0
 vbc(6)=0


 !Max Cycles and Tolerance
 maxcy=1500
 tolmax=1.0d-8

 !MG Levels 
 nx1Levels=6
 nx2Levels=6
 nx3Levels=6 

 !Pre/Post relax iterations at each level
 ngrid=max(nx1Levels,nx2Levels,nx3Levels)
 allocate(ipre_relax(1:ngrid),ipost_relax(1:ngrid))
 ipre_relax(1) = 12
 ipost_relax(1) = 6
 ipre_relax(2) = 10
 ipost_relax(2) = 5
 ipre_relax(3) = 8
 ipost_relax(3) = 3
 ipre_relax(4) = 4
 ipost_relax(4) = 2
 ipre_relax(5) = 4
 ipost_relax(5) = 2
 ipre_relax(6) = 4
 ipost_relax(6) = 2

 Smoother='RBPGS'

 !Force mpi boundary data transfer at half-sweeps 
 strict=.true.

 !Output residual at each iteration
 verbose=.true.
 stat=0
return
end subroutine mg_parameters
