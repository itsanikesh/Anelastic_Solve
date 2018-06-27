












subroutine mg_check_size(ierr)
!@t
! \textbf{subroutine mg\_check\_size(ierr)}
!@h
!   Description:
!     Verifies that the number of processors in each direction divides the 
!     number of points so that restriction does not need inter-processor 
!     communication. Ensures the coarsest grid has at least one point in
!     each sub-domain. Verifies that coarsening takes place in all 
!     directions.
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
!     An output message is sent to confirm that everything is sized
!     properly.
!@q
 use ntypes, only: r8,i4
 use mgVars, only: nx1Cp, nx2Cp, nx3Cp, nx1Fp, nx2Fp, nx3Fp, ngrid, &
                     nx1Levels, nx2Levels, nx3Levels, IOUTmg

 implicit none

 !Passed Variables
 integer,intent(out)            :: ierr

 !Local Variables
 integer(i4)                    :: i,j,k
 integer(i4)                    :: stat


!The grid at the coarsest level must have at least one point in each subdomain, i.e. 
!nx1Points.ge.nx1procs
!nx2Points.ge.nx2procs
!nx3Points.ge.nx3procs
!It is possible to have nx1Levels.NE.nx2Levels.NE.nx3Levels 
  !meaning that no coarsifying will take place in some direction between some levels.

!Check that the block dimensions are correct
 i=nx1Cp*2**(nx1Levels-1)+1
 if ((nx1Fp+1).ne.i) then
  write(IOUTmg,'(a,i4,a,i4,a)') 'ERROR: mg_check_size: nx1Fp+1=',nx1Fp+1,'<>nx1Cp*2**&
            &  (nx1Levels-1)+1=',nx1Cp*2**(nx1Levels-1)+1,'-> adjust '
  stat=4
  goto 1000
 endif

 j=nx2Cp*2**(nx2Levels-1)+1
 if ((nx2Fp+1).ne.j) then
  write(IOUTmg,'(a,i4,a,i4,a)') 'ERROR: mg_check_size: nx2Fp+1=',nx2Fp+1,'<>nx2Cp*2**&
            &  (nx2Levels-1)+1=',nx2Cp*2**(nx2Levels-1)+1,'-> adjust '
  stat=5
  goto 1000
 endif

 k=nx3Cp*2**(nx3Levels-1)+1
 if ((nx3Fp+1).ne.k) then
  write(IOUTmg,'(a,i4,a,i4,a)') 'ERROR: mg_check_size: nx3Fp+1=',nx3Fp+1,'<>nx3Cp*2**&
            &  (nx3Levels-1)+1=',nx3Cp*2**(nx3Levels-1)+1,'-> adjust '
  stat=6
  goto 1000
 endif


!Check that coarsifying takes place in all directions at the finest grid level
 if (ngrid.gt.1) then
  if (nx1Levels.eq.1) then
   write(IOUTmg, '(a,i4,a,i4,a)') 'ERROR: mg_check_size: ngrid=',ngrid,' nx1Levels=',&
          &  nx1Levels,'no coarsifying at the finest grid level in x1-direction'
   stat=10
   goto 1000
  endif

  if (nx2Levels.eq.1) then
   write(IOUTmg,'(a,i4,a,i4,a)') 'ERROR: mg_check_size: ngrid=',ngrid,' nx2Levels=', &
          &  nx2Levels,'no coarsifying at the finest grid level in x2-direction' 
   stat=11
   goto 1000
  endif

  if (nx3Levels.eq.1) then
   write(IOUTmg,'(a,i4,a,i4,a)') 'ERROR: mg_check_size: ngrid=',ngrid,' nx3Levels=', &
          &  nx3Levels,'no coarsifying at the finest grid level in x3-direction'
   stat=12
   goto 1000
  endif
 endif

 ierr=0
 write(IOUTmg,'(a)') "MG CHECK SIZE COMPLETED"

 return

 1000 continue
 ierr=stat
 write(IOUTmg,'(a)') "MG CHECK SIZE FAILED"
 return
end
