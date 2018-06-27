subroutine pgs(phi,src,iters,rf)
!subroutine pgs(phi,src,iters,rf,psfine)
!@t
! \textbf{subroutine pgs(phi,src,iters,rf,psfine)}
!@h
!   Description:
!     Perform point red-black Gauss-Seidel (RBPGS) smoothing with over
!     relaxation and Chebechev acceleration for a given number of iterations.
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
!     For Periodic, Neumann, and Dirichlet BC's. Adapted from bernard
!     bunner's (bunner@engin.umich.edu) mgd3 solver available at
!     www.mgnet.org. SLOWER than RBPGS for multigrid, even on finest
!     grid only, see Numerical Recipes in Fortran Second Edition by
!     Press et al. p. 866 for explanation why.
!@q

!RBPGS-- Red Black Point Gauss-Seidel with over relaxation and Chebechev acceleration.
!stype='SR','SOR','SORCH' respectively

!IF STRICT=.FALSE.
!Exchange the plane boundary values only once at the end. Since the 
! number of relaxation sweeps at each level is characteristically 
! small this does not damage the  convergence rate too badly. 
! Overall, I have found a significant reduction in execution time. 

 use ntypes, only: r8,i4
 use mgVars,    only: ngrid,nxk,nyk,nzk,sxk,exk,syk,eyk,szk,ezk,strict, &
                        pcf_1,pcf_2,pcf_3,pcf_4,pcf_5,pcf_6
 implicit none

!Passed Variables
 integer,intent(in)          :: iters
 real(r8),intent(inout)      :: phi(sxk(ngrid)-1:exk(ngrid)+1,syk(ngrid)-1:&
                                      eyk(ngrid)+1,szk(ngrid)-1:ezk(ngrid)+1)
 real(r8),intent(in)         :: src(sxk(ngrid)-1:exk(ngrid)+1,syk(ngrid)-1:&
                                      eyk(ngrid)+1,szk(ngrid)-1:ezk(ngrid)+1)
! character(len=*)            :: psfine
 real(r8)                    :: rf
!Local Variables
 integer                   :: rb,rbs,it,ipass,i,j,k
 real(r8)                  :: pcfsum
 integer                   :: sxm,exm,sym,eym,szm,ezm
 real(r8)                  :: omega1,omega2
 integer                   :: is,js,ks

 sxm=sxk(ngrid)
 exm=exk(ngrid)
 sym=syk(ngrid)
 eym=eyk(ngrid)
 szm=szk(ngrid)
 ezm=ezk(ngrid)
                                                                                                                             
! select case(psFine)
!  case('SR')
!   omega1 = 1.d0
!   omega2 = 0.d0
!  case('SOR')
   omega1 = rf
   omega2 = 1.d0-rf
!  case('SORCH')
!   omega1 = 1.d0
!   omega2 = 0.d0
!  case DEFAULT
!   write(IOUT,'(a)') "WARNING: fine grid solver type, "//trim(psFine)//", not defined using SR" 
!   omega1 = 1.d0
!   omega2 = 0.d0
!  end select


 is=0
 js=0
 ks=0

!Do iters sweeps in the subdomain; impose the wall derivative
!BC after each half-sweep
 do it=1,iters
  rb=mod(sxm,2)
  do ipass=1,2
   do k=szm,ezm
    rbs=rb
    do j=sym,eym
     do i=sxm+rb,exm,2
      pcfsum = -(pcf_1(i+is)+pcf_2(i+is)&
             +  pcf_3(j+js)+pcf_4(j+js)&
             +  pcf_5(k+ks)+pcf_6(k+ks))
      phi(i,j,k)=omega1*(src(i,j,k) -( pcf_1(i+is)*phi(i-1,j,k)    &
                            +  pcf_2(i+is)*phi(i+1,j,k)            &
                            +  pcf_3(j+js)*phi(i,j-1,k)            &
                            +  pcf_4(j+js)*phi(i,j+1,k)            &
                            +  pcf_5(k+ks)*phi(i,j,k-1)            &
                            +  pcf_6(k+ks)*phi(i,j,k+1) ) )        &
                           /pcfsum + omega2*phi(i,j,k)
     enddo
     rb=1-rb
    enddo
    rb=1-rbs
   enddo
   rb=1-mod(sxm,2)

! select case(psFine)
!  case('SORCH')
!   if (it.EQ.1.AND.ipass.EQ.0) then
!    omega1=1.d0/(1.d0-0.5d0*rf**2)
!   else
!    omega1=1.d0/(1.d0-0.25d0*rf**2*omega1)
!   endif
!    omega2=1.d0-omega1
! end select

   call mg_bdry(ngrid,phi,strict,.false.)

  enddo
 enddo

 !Impose Neumann and Dirichlet boundary conditions
 call mg_bdry(ngrid,phi,.true.,.false.)

 return
end subroutine
