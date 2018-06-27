












subroutine grid_setup(stat)
!@t
! \textbf{subroutine initialize(stat)}
!@h
!   Description:
!     Loads the grid files and sets the diffusive timestep limit.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!     2.0       07/2008  Separated calculating diffusive time limit and 
!                        grid setup. [Matt de Stadler] 

 use ntypes, only: i4
 use IO,     only: IOUT
 implicit none

 !Passed Variables
 integer(i4),intent(out) :: stat

 !Local Variables
 integer(i4) :: ok1, ok2

 ok1=0
 ok2=0

 !SET INITIAL DENSITY FIELD
 call load_grid(ok1) 
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !ADD MEAN FLOW FIELD
 call calcdifflimit(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 stat=max(ok1,ok2)
 return

 9999 continue
 write(IOUT,*) 'ERROR loading grid, ok1, ok2 ', ok1, ok2
 stat=max(ok1,ok2)
 return
end subroutine grid_setup

subroutine calcdifflimit(stat)
!@t
! \textbf{subroutine calcdifflimit(stat)}
!@h
!   Description:
!     Calculates the diffusive timestep limit.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use ntypes
 use IO,     only: IOUT
 use Grid,   only: dxc,dyc,dzc
 use Parameters, only:  Ddt,rRe
 use LESmod, only:  nu_sgs_max
 implicit none

!Passed Variables
 integer,intent(out)         :: stat
 real(r8)                    :: dx1_min, dx2_min, dx3_min, dxi_min
 integer                     :: err1

 !CALCULATE maximum dt based on diffusive limit
 dx1_min=minval(dxc)
 dx2_min=minval(dyc)
 dx3_min=minval(dzc)
! dxi_min=min(dx1_min,dx2_min,dx3_min)

 Ddt=0.125d0/rRe&
     /(1.d0/dx1_min**2.d0+1.d0/dx2_min**2.d0+1.d0/dx3_min**2.d0)
 write(IOUT,'(a,e15.8)') "DIFFUSIVE TIME STEP LIMIT=  ",Ddt
 stat=0

 return
end subroutine calcdifflimit
