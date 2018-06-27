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
#ifdef PARALLEL
 use mgVars, only: nx1procs, nx2procs, nx3procs
#endif

 implicit none

 !Passed Variables
 integer,intent(out)            :: ierr

 !Local Variables
 integer(i4)                    :: i,j,k
 integer(i4)                    :: stat

#ifdef PARALLEL
! Check that the number of processes in each direction divides the number 
! of points in that direction otherwise the restriction in 'MG_restr' would 
! not be complete and would require doing some inter-process data communication 
 if (mod(nx1Fp,nx1procs).ne.0) then
  write(IOUTmg,'(a,i4,a,i4)') 'ERROR: mg_check_size: nx1Fp=',nx1Fp,' is not a  & 
                            & multiple of nx1procs=',nx1procs
  stat=1
  goto 1000
 endif

 if (mod(nx2Fp,nx2procs).ne.0) then
  write(IOUTmg,'(a,i4,a,i4)') 'ERROR: mg_check_size: nx2Fp=',nx2Fp,' is not a  & 
                            & multiple of nx2procs=',nx2procs
  stat=2
  goto 1000
 endif

 if (mod(nx3Fp,nx3procs).ne.0) then
  write(IOUTmg,'(a,i4,a,i4)') 'ERROR: mg_check_size: nx3Fp=',nx3Fp,' is not a  & 
                            & multiple of nx3procs=',nx3procs
  stat=3
  goto 1000
 endif
#endif

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

#ifdef PARALLEL
!Check that the number of points at the coarser level is not smaller
!than the number of processes
 if (nx1Cp.lt.nx1procs) then
  write(IOUTmg,'(a,i4,a,i4,a)') 'ERROR: mg_check_size: nx1Cp=',nx1Cp,' < nx1procs=',&
          &  nx1procs,'there must be at least one grid point at the      & 
          &  coarsest grid level -> increase nx1CPoints and decrease nx1Levels'
  stat=7
  goto 1000
 endif
 
 if (nx2Cp.lt.nx2procs) then
  write(IOUTmg,'(a,i4,a,i4,a)') 'ERROR: mg_check_size: nx2Cp=',nx2Cp,' < nx2procs=',&
          &  nx2procs,'there must be at least one grid point at the      & 
          &   coarsest grid level -> increase nx2CPoints and decrease nx2Levels'
  stat=8
  goto 1000
 endif

 if (nx3Cp.lt.nx3procs) then
  write(IOUTmg, '(a,i4,a,i4,a)') 'ERROR: mg_check_size: nx3Cp=',nx3Cp,' < nx3procs=',&
          &  nx3procs,'there must be at least one grid point at the       &
          &   coarsest grid level -> increase nx3CPoints and decrease nx3Levels'
  stat=9
  goto 1000
 endif
#endif

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
