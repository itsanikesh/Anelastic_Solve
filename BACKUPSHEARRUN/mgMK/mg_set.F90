subroutine mg_set(phi,rhs,phif,rhsf)
!@t
! \textbf{subroutine mg\_set(phi,rhs,phif,rhsf)}
!@h
!   Description:
!     Set the Fine grid values of phi and rhs in their respective work
!     vectors.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: r8
 use mgVars, only: sxk,exk,syk,eyk,szk,ezk,ngrid
 implicit none

!Passed Variables
 real(r8),intent(out)      :: phi(sxk(ngrid)-1:exk(ngrid)+1,syk(ngrid)-1:eyk(ngrid)+1,szk(ngrid)-1:ezk(ngrid)+1)
 real(r8),intent(out)      :: rhs(sxk(ngrid)-1:exk(ngrid)+1,syk(ngrid)-1:eyk(ngrid)+1,szk(ngrid)-1:ezk(ngrid)+1)
 real(r8),intent(in)       :: phiF(sxk(ngrid)-1:exk(ngrid)+1,syk(ngrid)-1:eyk(ngrid)+1,szk(ngrid)-1:ezk(ngrid)+1)
 real(r8),intent(in)       :: rhsF(sxk(ngrid)-1:exk(ngrid)+1,syk(ngrid)-1:eyk(ngrid)+1,szk(ngrid)-1:ezk(ngrid)+1)

!Local Variables
 integer                   :: sxf,exf,syf,eyf,szf,ezf
 integer                   :: i,j,k

 sxf=sxk(ngrid)
 exf=exk(ngrid)
 syf=syk(ngrid)
 eyf=eyk(ngrid)
 szf=szk(ngrid)
 ezf=ezk(ngrid)

 do k=szf-1,ezf+1
  do j=syf-1,eyf+1
   do i=sxf-1,exf+1
    phi(i,j,k)=phif(i,j,k)
    rhs(i,j,k)=rhsf(i,j,k)
   enddo
  enddo
 enddo

 return
end subroutine
