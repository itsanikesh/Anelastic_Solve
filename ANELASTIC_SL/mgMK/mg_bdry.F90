subroutine mg_bdry(lvl,phi,ghost,final)
!@t
! \textbf{subroutine mg\_bdry(lvl,phi,ghost,final)}
!@h
!   Description:
!     Enforce the Dirichlet or Neumann boundary conditions at a given level.
!     Periodic BCs are only enforced in this subroutine for the serial  
!     version, not the parallel one.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!     2.0       03/2009  Fixed dirichlet BC [Kyle A. Brucker] 

!   Language:
!     Fortran 90
!@h
!   Comments:
!     Both Neumann and Dirichlet boundary conditions can be non-zero.
!@q

 use ntypes, only: r8,i4
 use mgVars,    only: sxk,exk,syk,eyk,szk,ezk,phibc,bdMG,dxck,dyck,dzck,ngrid
#ifdef PARALLEL
 use mgVars,    only: smpistatus,commMG
#endif

 implicit none

!Passed Variables
 integer,intent(in)        :: lvl
 real(r8),intent(inout)    :: phi(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 logical,intent(in)        :: ghost, final
!Local Variables
 integer(i4)               :: sxm,exm,sym,eym,szm,ezm
 integer(i4)               :: i,j,k
 real(r8)                  :: phibcL(6)
#ifdef PARALLEL
 integer(i4)               :: ireq, req(52)
 integer(i4)               :: status1(smpistatus,52),ierr
#endif

 sxm=sxk(lvl)
 exm=exk(lvl)
 sym=syk(lvl)
 eym=eyk(lvl)
 szm=szk(lvl)
 ezm=ezk(lvl)
 phibcL(:)=phibc(lvl,:)

#ifdef PARALLEL
 !Exchange Ghost Cells
 if ( ghost ) then 
  ireq=0
  call gxch1linMG(lvl,phi,req,ireq)
  call gxch1corMG(lvl,phi,req,ireq)
  call gxch1plaMG(lvl,phi,req,ireq)
  call MPI_WAITALL(ireq,req,status1,ierr)
 endif

 if ( final ) then 
  ireq=0
  call gxch1linMG(lvl,phi,req,ireq)
  call gxch1corMG(lvl,phi,req,ireq)
  call MPI_WAITALL(ireq,req,status1,ierr)
 endif
#else
 !Make all BCs periodic just like ghosting would, they will be corrected 
 !in the rest of the subroutine
 !X1
 phi(sxm-1,:,:)=phi(exm,:,:)
 phi(exm+1,:,:)=phi(sxm,:,:)
 !X2
 phi(:,sym-1,:)=phi(:,eym,:)
 phi(:,eym+1,:)=phi(:,sym,:)
 !X3
 phi(:,:,szm-1)=phi(:,:,ezm)
 phi(:,:,ezm+1)=phi(:,:,szm)
#endif

!Apply Dirichlet and Neumann Boundary Conditions

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
!1 means dp/dn=value
!2 means p=value 

 !X2-max
 if (bdMG(1).eq.1) then
  do k=szm-1,ezm+1
   do i=sxm-1,exm+1
    phi(i,eym+1,k)=phi(i,eym,k)+phibcL(1)*dyck(eym)
   enddo
  enddo
 elseif (bdMG(1).eq.2) then
  do k=szm-1,ezm+1
   do i=sxm-1,exm+1
    phi(i,eym+1,k)=phibcL(1)
   enddo
  enddo
 endif

 !X3-min
 if (bdMG(3).eq.1) then
  do j=sym-1,eym+1
   do i=sxm-1,exm+1
    phi(i,j,szm-1)=phi(i,j,szm)-phibcL(2)*dzck(szm)
   enddo
  enddo
 elseif (bdMG(3).eq.2) then
  do j=sym-1,eym+1
   do i=sxm-1,exm+1
    phi(i,j,szm-1)=phibcL(2)
   enddo
  enddo
 endif

 !X2-min
 if (bdMG(5).eq.1) then
  do k=szm-1,ezm+1
   do i=sxm-1,exm+1
    phi(i,sym-1,k)=phi(i,sym,k)-phibcL(3)*dyck(sym)
   enddo
  enddo
 elseif (bdMG(5).eq.2) then
  do k=szm-1,ezm+1
   do i=sxm-1,exm+1
    phi(i,sym-1,k)=phibcL(3)
   enddo
  enddo
 endif

 !X3-max
 if (bdMG(7).eq.1) then
  do j=sym-1,eym+1
   do i=sxm-1,exm+1
    phi(i,j,ezm+1)=phi(i,j,ezm)+phibcL(4)*dzck(ezm)
   enddo
  enddo
 elseif (bdMG(7).eq.2) then
  do j=sym-1,eym+1
   do i=sxm-1,exm+1
    phi(i,j,ezm+1)=phibcL(4)
   enddo
  enddo
 endif

 !X1-min
 if (bdMG(9).eq.1) then
  do k=szm-1,ezm+1
   do j=sym-1,eym+1
    phi(sxm-1,j,k)=phi(sxm,j,k)-phibcL(5)*dxck(sxm)
   enddo
  enddo
 elseif (bdMG(9).eq.2) then
  do k=szm-1,ezm+1
   do j=sym-1,eym+1
    phi(sxm-1,j,k)=phibcL(5)
   enddo
  enddo
 endif

 !X1-max
 if (bdMG(18).eq.1) then
  do k=szm-1,ezm+1
   do j=sym-1,eym+1
    phi(exm+1,j,k)=phi(exm,j,k)+phibcL(6)*dxck(exm)
   enddo
  enddo
 elseif (bdMG(18).eq.2) then
  do k=szm-1,ezm+1
   do j=sym-1,eym+1
    phi(exm+1,j,k)=phibcL(6)
   enddo
  enddo
 endif
 return
end subroutine mg_bdry
