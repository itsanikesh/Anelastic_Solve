












subroutine mg_relax(lvl,phi,src,iters)
!@t
! \textbf{subroutine mg\_relax(lvl,phi,src,iters)}
!@h
!   Description:
!     Perform point Gauss-Seidel (PGS) smoothing for a given number of
!     iterations at a given level.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     For Periodic, Neumann, and Dirichlet BC's. Adapted from bernard
!     bunner's (bunner@engin.umich.edu) mgd3 solver available at
!     www.mgnet.org.
!@q

 use ntypes,      only: r8,i4
 use mgVars,      only: phibc,nxk,nyk,nzk,sxk,exk,syk,eyk,szk,ezk, &
                        pcf_1,pcf_2,pcf_3,pcf_4,pcf_5,pcf_6, &
                        kpx1,kpx2,kpx3,strict
 implicit none

!Passed Variables
 integer(i4),intent(in)    :: iters, lvl
 real(r8),intent(inout)    :: phi(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                      eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: src(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                      eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)

!Local Variables
 integer(i4)               :: it,i,j,k
 real(r8)                  :: pcfsum
 integer(i4)               :: sxm,exm,sym,eym,szm,ezm
 integer(i4)               :: is, js, ks

 sxm=sxk(lvl)
 exm=exk(lvl)
 sym=syk(lvl)
 eym=eyk(lvl)
 szm=szk(lvl)
 ezm=ezk(lvl)

 is=kpx1(lvl)-1
 js=kpx2(lvl)-1
 ks=kpx3(lvl)-1

!Do iters sweeps in the subdomain; impose the wall derivative
!BC after each half-sweep
 do it=1,iters
   do k=szm,ezm
    do j=sym,eym
     do i=sxm,exm
      pcfsum =-(pcf_1(i+is)+pcf_2(i+is)&
             +  pcf_3(j+js)+pcf_4(j+js)&
             +  pcf_5(k+ks)+pcf_6(k+ks))

      phi(i,j,k)=(src(i,j,k) -( pcf_1(i+is)*phi(i-1,j,k)    &
                            +  pcf_2(i+is)*phi(i+1,j,k)            &
                            +  pcf_3(j+js)*phi(i,j-1,k)            &
                            +  pcf_4(j+js)*phi(i,j+1,k)            &
                            +  pcf_5(k+ks)*phi(i,j,k-1)            &
                            +  pcf_6(k+ks)*phi(i,j,k+1) ) )        &
                           /pcfsum 

     enddo
    enddo
   enddo

   !Impose Boundary conditions
   call mg_bdry(lvl,phi,strict,.false.)
 enddo

 !Impose Boundary conditions
 call mg_bdry(lvl,phi,.true.,.false.)

 return
end subroutine mg_relax

subroutine mg_relaxRB(lvl,cofc1,cofc2,cofc3,cofc4,cofc5,cofc6,cofc7,phi,src,iters)
!@t
! \textbf{subroutine mg\_relax(lvl,phi,src,iters)}
!@h
!   Description:
!     Perform point red-black Gauss-Seidel (RBPGS) smoothing for a given
!     number of iterations at a given level.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     For Periodic, Neumann, and Dirichlet BC's. Adapted from bernard
!     bunner's (bunner@engin.umich.edu) mgd3 solver available at
!     www.mgnet.org.
!@q

 use ntypes,      only: r8,i4
 use mgVars,      only: phibc,nxk,nyk,nzk,sxk,exk,syk,eyk,szk,ezk, &
                        pcf_1,pcf_2,pcf_3,pcf_4,pcf_5,pcf_6, &
                        kpx1,kpx2,kpx3,strict
 implicit none

!Passed Variables
 integer(i4),intent(in)    :: iters, lvl
 real(r8),intent(inout)    :: phi(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                      eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: src(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                      eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc1(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc2(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc3(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc4(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc5(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc6(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc7(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)

!Local Variables
 integer(i4)               :: rb,rbs,it,ipass,i,j,k
 real(r8)                  :: pcfsum
 integer(i4)               :: sxm,exm,sym,eym,szm,ezm
 integer(i4)               :: is, js, ks

 sxm=sxk(lvl)
 exm=exk(lvl)
 sym=syk(lvl)
 eym=eyk(lvl)
 szm=szk(lvl)
 ezm=ezk(lvl)

 is=kpx1(lvl)-1
 js=kpx2(lvl)-1
 ks=kpx3(lvl)-1

!Do iters sweeps in the subdomain; impose the wall derivative
!BC after each half-sweep
 do it=1,iters
  rb=mod(sxm,2)
  do ipass=1,2
   do k=szm,ezm
    rbs=rb
    do j=sym,eym
     do i=sxm+rb,exm,2
      pcfsum = cofc7(i,j,k)

      phi(i,j,k)=(src(i,j,k) -( cofc1(i,j,k)*phi(i-1,j,k)    &
                             +  cofc2(i,j,k)*phi(i+1,j,k)            &
                             +  cofc3(i,j,k)*phi(i,j-1,k)            &
                             +  cofc4(i,j,k)*phi(i,j+1,k)            &
                             +  cofc5(i,j,k)*phi(i,j,k-1)            &
                             +  cofc6(i,j,k)*phi(i,j,k+1) ) )        &
                             /pcfsum 

     enddo
     rb=1-rb
    enddo
    rb=1-rbs
   enddo
   rb=1-mod(sxm,2)

   !Impose Boundary conditions
   call mg_bdry(lvl,phi,strict,.false.)
  enddo
 enddo

 !Impose Boundary conditions
 call mg_bdry(lvl,phi,.true.,.false.)

 return
end subroutine mg_relaxRB

subroutine mg_relaxLx(lvl,phi,src,iters)
!@t
! \textbf{subroutine mg\_relaxLx(lvl,phi,src,iters)}
!@h
!   Description:
!     Perform line Jacobi smoothing for a given number of iterations at a 
!     given level. Lines are performed in the x direction.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       04/2009  Original code. [Matt de Stadler] 
!@h
!   Comments:
!     For Periodic, Neumann, and Dirichlet BC's. The lines are performed in
!     the x direction. This is the fastest in memory and the direction where
!     the solution is most tightly coupled. This algorithm works well for
!     stretched grids: sweeping should be done in the direction with the
!     finest spacing. Has the Thomas algorithm hardcoded in for computational
!     efficiency. For more restrictive tolerances ( > 1e-6 ) this can be 
!     slower than the RB Line Jacobi solver below.
!@q

 use ntypes,      only: r8,i4
 use mgVars,      only: phibc,nxk,nyk,nzk,sxk,exk,syk,eyk,szk,ezk, &
                        pcf_1,pcf_2,pcf_3,pcf_4,pcf_5,pcf_6, &
                        kpx1,kpx2,kpx3,strict
 implicit none

!Passed Variables
 integer(i4),intent(in)    :: iters, lvl
 real(r8),intent(inout)    :: phi(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                      eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: src(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                      eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)

!Local Variables
 integer(i4)               :: it,j,k, i
 real(r8)                  :: a(sxk(lvl):exk(lvl))
 real(r8)                  :: aa(sxk(lvl):exk(lvl))
 real(r8)                  :: b(sxk(lvl):exk(lvl))
 real(r8)                  :: c(sxk(lvl):exk(lvl))
 real(r8)                  :: rhs(sxk(lvl):exk(lvl))
 integer(i4)               :: sxm,exm,sym,eym,szm,ezm
 integer(i4)               :: is, js, ks

 sxm=sxk(lvl)
 exm=exk(lvl)
 sym=syk(lvl)
 eym=eyk(lvl)
 szm=szk(lvl)
 ezm=ezk(lvl)

 is=kpx1(lvl)-1
 js=kpx2(lvl)-1
 ks=kpx3(lvl)-1

 !coefficients for thomas algorithm
 !a = pcf1 , b = -(pcf1+pcf2+pcf3+pcf4+pcf5+pcf6), c = pcf2
 !rhs = src - (pcf6*pk+1 + pcf5*pk-1 + pcf4*pj+1 + pcf3*pj-1)

 a   = 0.d0   
 aa  = 0.d0   
 b   = 0.d0
 c   = 0.d0 
 rhs = 0.d0

 c = pcf_2(sxm+is:exm+is) ! Thomas algorithm superdiagonal
 a = pcf_1(sxm+is:exm+is) ! Thomas algorithm subdiagonal

 !Do iters sweeps in the subdomain; impose the wall derivative
 !BC after each half-sweep
 do it=1,iters
    do k=szm,ezm
     do j=sym,eym
       b = -(pcf_1(sxm+is:exm+is) + pcf_2(sxm+is:exm+is) + pcf_3(j+js) + pcf_4(j+js) + pcf_5(k+ks) + pcf_6(k+ks))

       rhs(sxm+1:exm-1) = src(sxm+1:exm-1,j,k) - ( pcf_5(k+ks)*phi(sxm+1:exm-1,j,k-1)+pcf_6(k+ks)*phi(sxm+1:exm-1,j,k+1) &
                          +  pcf_3(j+js)*phi(sxm+1:exm-1,j-1,k)+pcf_4(j+js)*phi(sxm+1:exm-1,j+1,k) )

       rhs(sxm) =  src(sxm,j,k) - ( pcf_5(k+ks)*phi(sxm,j,k-1)+pcf_6(k+ks)*phi(sxm,j,k+1) &
                          +  pcf_3(j+js)*phi(sxm,j-1,k)+pcf_4(j+js)*phi(sxm,j+1,k)       &
                          +  pcf_1(sxm+is)*phi(sxm-1,j,k) )

       rhs(exm) =  src(exm,j,k) - ( pcf_5(k+ks)*phi(exm,j,k-1)+pcf_6(k+ks)*phi(exm,j,k+1) &
                          +  pcf_3(j+js)*phi(exm,j-1,k)+pcf_4(j+js)*phi(exm,j+1,k)       &
                          +  pcf_1(exm+is)*phi(exm+1,j,k) )

       ! THOMAS ALGORITHM INLINED FOR COMPUTATIONAL EFFICIENCY
       ! downsweep
       do i = sxm, exm-1
         aa(i+1)  = -a(i+1) / b(i)
         b(i+1)   = b(i+1) + aa(i+1)*c(i)
         rhs(i+1) = rhs(i+1) + aa(i+1)*rhs(i)
       enddo
       ! solve at bottom right corner
       phi(exm,j,k) = rhs(exm) / b(exm)
       ! upsweep
       do i = exm-1,sxm,-1
         phi(i,j,k) = (rhs(i)-c(i)*phi(i+1,j,k)) / b(i)
       enddo

    enddo
   enddo

   !Impose Boundary conditions
   call mg_bdry(lvl,phi,strict,.false.)
 enddo

 !Impose Boundary conditions
 call mg_bdry(lvl,phi,.true.,.false.)

 return
end subroutine mg_relaxLx


subroutine mg_relaxLxRB(lvl,phi,src,iters)
!@t
! \textbf{subroutine mg\_relaxLxRB(lvl,phi,src,iters)}
!@h
!   Description:
!     Perform line red-black Gauss-Seidel smoothing for a given number of
!     iterations at a given level. Lines are performed in the x direction.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       04/2009  Original code. [Matt de Stadler] 
!@h
!   Comments:
!     For Periodic, Neumann, and Dirichlet BC's. The lines are performed in
!     the x direction. This is the fastest in memory and the direction where
!     the solution is most tightly coupled. This algorithm works well for
!     stretched grids: sweeping should be done in the direction with the
!     finest spacing. Has the Thomas algorithm hardcoded in for 
!     computational efficiency. For more restrictive tolerances ( > 1e-6 )
!     this can be faster than the Line Jacobi solver above.
!@q

 use ntypes,      only: r8,i4
 use mgVars,      only: phibc,nxk,nyk,nzk,sxk,exk,syk,eyk,szk,ezk, &
                        pcf_1,pcf_2,pcf_3,pcf_4,pcf_5,pcf_6, &
                        kpx1,kpx2,kpx3,strict
 implicit none

!Passed Variables
 integer(i4),intent(in)    :: iters, lvl
 real(r8),intent(inout)    :: phi(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                      eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: src(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                      eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)

!Local Variables
 integer(i4)               :: rb,it,ipass,j,k, i
 real(r8)                  :: a(sxk(lvl):exk(lvl))
 real(r8)                  :: aa(sxk(lvl):exk(lvl))
 real(r8)                  :: b(sxk(lvl):exk(lvl))
 real(r8)                  :: c(sxk(lvl):exk(lvl))
 real(r8)                  :: rhs(sxk(lvl):exk(lvl))
 integer(i4)               :: sxm,exm,sym,eym,szm,ezm
 integer(i4)               :: is, js, ks

 sxm=sxk(lvl)
 exm=exk(lvl)
 sym=syk(lvl)
 eym=eyk(lvl)
 szm=szk(lvl)
 ezm=ezk(lvl)

 is=kpx1(lvl)-1
 js=kpx2(lvl)-1
 ks=kpx3(lvl)-1

 !coefficients for thomas algorithm
 !a = pcf1 , b = -(pcf1+pcf2+pcf3+pcf4+pcf5+pcf6), c = pcf2
 !rhs = src - (pcf6*pk+1 + pcf5*pk-1 + pcf4*pj+1 + pcf3*pj-1)

 a   = 0.d0   
 aa  = 0.d0   
 b   = 0.d0
 c   = 0.d0 
 rhs = 0.d0

 c = pcf_2(sxm+is:exm+is) ! Thomas algorithm superdiagonal
 a = pcf_1(sxm+is:exm+is) ! Thomas algorithm subdiagonal

 !Do iters sweeps in the subdomain; impose the wall derivative
 !BC after each half-sweep
 do it=1,iters
  rb=mod(sym,2)
  do ipass=1,2
    do k=szm,ezm
     do j=sym+rb,eym,2
       b = -(pcf_1(sxm+is:exm+is) + pcf_2(sxm+is:exm+is) + pcf_3(j+js) + pcf_4(j+js) + pcf_5(k+ks) + pcf_6(k+ks))

       rhs(sxm+1:exm-1) = src(sxm+1:exm-1,j,k) - ( pcf_5(k+ks)*phi(sxm+1:exm-1,j,k-1)+pcf_6(k+ks)*phi(sxm+1:exm-1,j,k+1) &
                          +  pcf_3(j+js)*phi(sxm+1:exm-1,j-1,k)+pcf_4(j+js)*phi(sxm+1:exm-1,j+1,k) )

       rhs(sxm) =  src(sxm,j,k) - ( pcf_5(k+ks)*phi(sxm,j,k-1)+pcf_6(k+ks)*phi(sxm,j,k+1) &
                          +  pcf_3(j+js)*phi(sxm,j-1,k)+pcf_4(j+js)*phi(sxm,j+1,k)       &
                          +  pcf_1(sxm+is)*phi(sxm-1,j,k) )

       rhs(exm) =  src(exm,j,k) - ( pcf_5(k+ks)*phi(exm,j,k-1)+pcf_6(k+ks)*phi(exm,j,k+1) &
                          +  pcf_3(j+js)*phi(exm,j-1,k)+pcf_4(j+js)*phi(exm,j+1,k)       &
                          +  pcf_1(exm+is)*phi(exm+1,j,k) )

       ! THOMAS ALGORITHM INLINED FOR COMPUTATIONAL EFFICIENCY
       ! downsweep
       do i = sxm, exm-1
         aa(i+1)  = -a(i+1) / b(i)
         b(i+1)   = b(i+1) + aa(i+1)*c(i)
         rhs(i+1) = rhs(i+1) + aa(i+1)*rhs(i)
       enddo
       ! solve at bottom right corner
       phi(exm,j,k) = rhs(exm) / b(exm)
       ! upsweep
       do i = exm-1,sxm,-1
         phi(i,j,k) = (rhs(i)-c(i)*phi(i+1,j,k)) / b(i)
       enddo

    enddo
    rb=1-rb
   enddo
   rb=1-mod(sym,2)

   !Impose Boundary conditions
   call mg_bdry(lvl,phi,strict,.false.)
  enddo
 enddo

 !Impose Boundary conditions
 call mg_bdry(lvl,phi,.true.,.false.)

 return
end subroutine mg_relaxLxRB

subroutine mg_relaxLz(lvl,cofc1,cofc2,cofc3,cofc4,cofc5,cofc6,cofc7,phi,src,iters)
!@t
! \textbf{subroutine mg\_relaxLz(lvl,phi,src,iters)}
!@h
!   Description:
!     Perform line red-black Gauss-Seidel smoothing for a given number of
!     iterations at a given level. Lines are performed in the z direction.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       04/2009  Original code. [Matt de Stadler] 
!@h
!   Comments:
!     For Periodic, Neumann, and Dirichlet BC's. The lines are performed in
!     the z direction. This is the slowest in memory. This algorithm works
!     well for stretched grids: sweeping should be done in the direction
!     with the finest spacing. Requires Thomas() subroutine.

! WARNING: THIS HAS NOT BEEN TESTED SINCE IT WAS ADDED TO THE NEW CODE (5/6/09)
!          IT SHOULD BE VERIFIED BEFORE USAGE
!@q

 use ntypes,      only: r8,i4
 use mgVars,      only: phibc,nxk,nyk,nzk,sxk,exk,syk,eyk,szk,ezk, &
                        pcf_1,pcf_2,pcf_3,pcf_4,pcf_5,pcf_6, &
                        kpx1,kpx2,kpx3,strict
 implicit none

!Passed Variables
 integer(i4),intent(in)    :: iters, lvl
 real(r8),intent(in)       :: cofc1(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc2(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc3(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc4(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc5(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc6(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: cofc7(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(inout)    :: phi(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                      eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 real(r8),intent(in)       :: src(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                      eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)

!Local Variables
 integer(i4)               :: rb,it,ipass,i,j
 real(r8)                  :: a(szk(lvl):ezk(lvl))
 real(r8)                  :: b(szk(lvl):ezk(lvl))
 real(r8)                  :: c(szk(lvl):ezk(lvl))
 real(r8)                  :: rhs(szk(lvl):ezk(lvl))
 integer(i4)               :: sxm,exm,sym,eym,szm,ezm
 integer(i4)               :: is, js, ks

 sxm=sxk(lvl)
 exm=exk(lvl)
 sym=syk(lvl)
 eym=eyk(lvl)
 szm=szk(lvl)
 ezm=ezk(lvl)

 is=kpx1(lvl)-1
 js=kpx2(lvl)-1
 ks=kpx3(lvl)-1

!! coefficients for thomas algorithm
!! a = pcf5 , b = -(pcf1+pcf2+pcf3+pcf4+pcf5+pcf6), c = pcf6
!! rhs = src - (pcf2*pi+1 + pcf1*pi-1 + pcf4*pj+1 + pcf3*pj-1)

 a = 0.d0   
 b = 0.d0
 c = 0.d0 
 rhs = 0.d0

 
 c = pcf_6(szm+ks:ezm+ks) ! note c is not updated by the Thomas algorithm

 !Do iters sweeps in the subdomain; impose the wall derivative
 !BC after each half-sweep
 do it=1,iters
  rb=mod(sxm,2)
  do ipass=1,2
    do j=sym,eym
     do i=sxm+rb,exm,2
       a = pcf_5(szm+ks:ezm+ks)
       b = -(pcf_1(i+is) + pcf_2(i+is) + pcf_3(j+js) + pcf_4(j+js) + pcf_5(szm+ks:ezm+ks) + pcf_6(szm+ks:ezm+ks))

       rhs(szm+1:ezm-1) = src(i,j,szm+1:ezm-1) - ( pcf_1(i+is)*phi(i-1,j,szm+1:ezm-1)+pcf_2(i+is)*phi(i+1,j,szm+1:ezm-1) &
                          +  pcf_3(j+js)*phi(i,j-1,szm+1:ezm-1)+pcf_4(j+js)*phi(i,j+1,szm+1:ezm-1) )

       rhs(szm) = src(i,j,szm) - ( pcf_1(i+is)*phi(i-1,j,szm) + pcf_2(i+is)*phi(i+1,j,szm) &
                          +  pcf_3(j+js)*phi(i,j-1,szm) + pcf_4(j+js)*phi(i,j+1,szm)  &
                          +  pcf_5(szm+ks)*phi(i,j,szm-1) )

       rhs(ezm) = src(i,j,ezm) - ( pcf_1(i+is)*phi(i-1,j,ezm) + pcf_2(i+is)*phi(i+1,j,ezm) &
                          +  pcf_3(j+js)*phi(i,j-1,ezm) + pcf_4(j+js)*phi(i,j+1,ezm)  &
                          +  pcf_6(ezm+ks)*phi(i,j,ezm+1) )

     call Thomas(a,b,c,rhs,ezm-szm+1) ! a,b output here are not the values above, they must be redefined 
     phi(i,j,szm:ezm) = rhs

     enddo
     rb=1-rb
    enddo
   rb=1-mod(sxm,2)
   !Impose Boundary conditions
   call mg_bdry(lvl,phi,strict,.false.)
  enddo
 enddo

 !Impose Boundary conditions upon exit
 call mg_bdry(lvl,phi,.true.,.false.)

 return
end subroutine mg_relaxLz


subroutine Thomas(a,b,c,G,n)   
!@t
! \textbf{subroutine Thomas(a,b,c,G,n)}
!@h
!   Description:
!     Solve a Tridiagonal system of equations using the Thomas Algorithm.
!     This was adapted from Tom Bewley's Matlab code in Numerical
!     Renaissance. See section 2.3
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       04/2009  Original code. [Matt de Stadler]
!@h
!   Comments:
!     Note that c is not modified within the subroutine. The definition
!     of the input variables is given below.
!
!   (A|g) = | b1 c1                 | g1 |
!           | a2 b2 c2              | g2 |
!           |    .  .  .            |    |
!           |      .  .  .          |    |
!           |        an-1 bn-1 cn-1 |    |
!           |             an   bn   | gn |
!@q

 use ntypes,      only: r8,i4
implicit none

! Passed Variables
integer(i4),intent(in) :: n
real(r8),intent(in)    :: c(n)             ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
real(r8),intent(inout) :: a(n),b(n)    
real(r8),intent(inout) :: G(n)

! Local Variables
integer(i4) :: j

! sweep down
do j = 1,n-1
  a(j+1) = -a(j+1) / b(j)
  b(j+1) = b(j+1)  + a(j+1)*c(j)
  G(j+1) = G(j+1)  + a(j+1)*G(j)
enddo
G(n) = G(n) / b(n)

! sweep back up
do j = n-1,1,-1
  G(j) = (G(j) - c(j) * G(j+1)) / b(j)
enddo

end subroutine Thomas
