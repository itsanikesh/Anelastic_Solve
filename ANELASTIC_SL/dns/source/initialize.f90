












subroutine initialize(stat)
!@t
! \textbf{subroutine initialize(stat)}
!@h
!   Description:
!     Initializes a simulation.
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
!     Adds density, initial turbulence, mean flow, pre-relaxes and then 
!     writes a re-start file, in that order.
!@q

 use ntypes, only: i4
 use IO,     only: IOUT
 use FLOW
 use Parameters
 use Langmuir_params, only: langmuir
 use boundC, only: VB,TB
 use Grid
 use Domain
 use dd
 implicit none

 !Passed Variables
 integer(i4),intent(out) :: stat

 !Local Variables
 integer(i4) :: ok1, ok2

 ok1=0
 ok2=0

 !SET INITIAL DENSITY FIELD
 rho = 0.d0
 write(*,*)"Before adding density", myid
 call add_density(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999
 write(*,*)"Finish adding density", myid
 !SET INITIAL SCALAR1 FIELD
 scal1 = 0.d0
 call add_scalar1(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !SET INITIAL BACGROUND DENSITY FIELD
 densityBG = 0.d0
 call set_densityBG(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !CREATE CROPPED TURBULENT FIELD 
 if (turbICS) call init_turbulence(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !ADD MEAN FLOW FIELD
 call add_mean(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999
 
 !ADD LANGMUIR 
 call add_langmuir(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !RELAX FIELD
 call relax_ics(ok1) 
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !WRITE OUT INITIAL FIELD
 call write_flow(rest_file_write,ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 write(IOUT,*) "FLOW INITIALIZATION COMPLETE"
 return

 9999 continue
 write(IOUT,*) 'ERROR initialize, ok1, ok2 ', ok1, ok2
 stat=max(ok1,ok2)
 return
end subroutine initialize
