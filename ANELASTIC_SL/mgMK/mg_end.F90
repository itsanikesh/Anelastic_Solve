subroutine mg_end(ierr)
!@t
! \textbf{subroutine mg\_end(ierr)}
!@h
!   Description:
!     Free the MPI datatypes associated with the multigrid code
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

!Called in : main
!Calls     : MPI_TYPE_FREE
 use mgVars,   only: ngrid

#ifdef PARALLEL
 use mgVars,   only: kdatatype
#endif

 implicit none
!Passed Varialbes
 integer,intent(out)    :: ierr
!Local Variables
 integer   :: j,k

 ierr=0
#ifdef PARALLEL
 do k=1,ngrid-1
  do j=1,7
   call MPI_TYPE_FREE(kdatatype(j,k),ierr)
  end do
 end do
#endif
return
end subroutine mg_end
