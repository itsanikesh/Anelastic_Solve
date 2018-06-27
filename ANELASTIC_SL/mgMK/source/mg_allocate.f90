












subroutine mg_allocate_1(ok)
!@t
! \textbf{subroutine mg\_allocate\_1(ok)}
!@h
!   Description:
!     Allocate arrays for the array offsets, number of grid points, start
!     and end of the domain, and the boundary conditions at each level.
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
!     An output message is sent to confirm that everything was allocated
!     properly.
!@q

  use ntypes, only: i4
  use mgVars
  implicit none

 !Passed Variables
 integer(i4),intent(out)  :: ok

 !Local Variables
 integer(i4) :: s1

 ok=0
 !Arrays for starting locations in 1d arrays
  allocate( kp3d(1:ngrid),stat=s1 )
  allocate( kpx1(1:ngrid),stat=s1 )
  allocate( kpx2(1:ngrid),stat=s1 )
  allocate( kpx3(1:ngrid),stat=s1 )
  allocate( kpbgn(1:ngrid),stat=s1 )
  allocate( kcbgn(1:ngrid),stat=s1 )
  allocate( nxk(1:ngrid),stat=s1 )
  allocate( nyk(1:ngrid),stat=s1 )
  allocate( nzk(1:ngrid),stat=s1 )
  allocate( sxk(1:ngrid),stat=s1 )
  allocate( exk(1:ngrid),stat=s1 )
  allocate( syk(1:ngrid),stat=s1 )
  allocate( eyk(1:ngrid),stat=s1 )
  allocate( szk(1:ngrid),stat=s1 )
  allocate( ezk(1:ngrid),stat=s1 ) 
  allocate( phibc(1:ngrid,1:6),stat=s1 ) 

   if (s1.NE.0) then
    write(IOUTmg,*) "Error Allocating multigrid integer arrays"
    goto 1000
   endif


 write(IOUTmg,'(a)') "ALLOCATING MG VARIABLES STAGE 1 COMPLETED"
  ok=0
  return

 1000 continue
 write(IOUTmg,'(a)') "ALLOCATING MG VARIABLES STAGE 1 FAILED" 
 ok=-1
 
return
end subroutine mg_allocate_1

subroutine mg_allocate_2(ok)
!@t
! \textbf{subroutine mg\_allocate\_2(ok)}
!@h
!   Description:
!     Allocate arrays for the pressure coefficients, grid points, grid 
!     spacings, and the work arrays.
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
!     An output message is sent to confirm that everything was allocated
!     properly.
!@q

  use ntypes, only: i4
  use mgVars

  implicit none

 !Passed Variables
 integer(i4),intent(out)  :: ok

 !Local Variables
 integer(i4) :: s1

 ok=0

!MULTIGRID ALLOCATION
!1d Arrays for Pressure Coefficients
 allocate( pcf_1(1:nworkX1),stat=s1 )
 allocate( pcf_2(1:nworkX1),stat=s1 )
 allocate( xek(1:nworkX1),stat=s1 )
 allocate( xck(1:nworkX1),stat=s1 )
 allocate( dxek(1:nworkX1),stat=s1 )
 allocate( dxck(1:nworkX1),stat=s1 )

 allocate( pcf_3(1:nworkX2),stat=s1 )
 allocate( pcf_4(1:nworkX2),stat=s1 )
 allocate( yek(1:nworkX2),stat=s1 )
 allocate( yck(1:nworkX2),stat=s1 )
 allocate( dyek(1:nworkX2),stat=s1 )
 allocate( dyck(1:nworkX2),stat=s1 )

 allocate( pcf_5(1:nworkX3),stat=s1 )
 allocate( pcf_6(1:nworkX3),stat=s1 )
 allocate( zek(1:nworkX3),stat=s1 )
 allocate( zck(1:nworkX3),stat=s1 )
 allocate( dzek(1:nworkX3),stat=s1 )
 allocate( dzck(1:nworkX3),stat=s1 )

 allocate( workc1(nwork3d),stat=s1)
 allocate( workc2(nwork3d),stat=s1)
 allocate( workc3(nwork3d),stat=s1)
 allocate( workc4(nwork3d),stat=s1)
 allocate( workc5(nwork3d),stat=s1)
 allocate( workc6(nwork3d),stat=s1)
 allocate( workc7(nwork3d),stat=s1)
 allocate( work1(nwork3d),stat=s1 )
 allocate( work2(nwork3d),stat=s1 )
 
 if (s1.NE.0) then
  write(IOUTmg,*) "Error Allocating WORK"
  goto 1000
 endif


 write(IOUTmg,'(a)') "ALLOCATING MG VARIABLES STAGE 2 COMPLETED"
  ok=0
  return

 1000 continue
 write(IOUTmg,'(a)') "ALLOCATING MG VARIABLES STAGE 2 FAILED" 
 ok=-1
 
return
end subroutine mg_allocate_2
