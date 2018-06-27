












subroutine mg_solver(phif,rhsf,iter,relmax)
!@t
! \textbf{subroutine mg\_solver(phif,rhsf,iter,relmax)}
!@h
!   Description:
!     Solves the Poisson equation del^2(p) = rhs for uniform and stretched
!     grids with cartesian coordinates.

!   Method:
!     Uses a second order centered parallel multigrid algorithm with full
!     volume weighting for prolongation and restriction.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!   Version   Date     Comment 
!   -------   ----     ------- 
!   1.0       07/2008  Original code. [Kyle A. Brucker] 
!   2.0       04/2009  Updated for stretched grids. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
!@h
!   Comments:
!     Adapted from bernard bunner's (bunner@engin.umich.edu) mgd3 solver
!     available at www.mgnet.org
!
!     The code will work well for unstretched and slightly stretched grids.
!     For highly stretched grids revisions need to be made to the 
!     restriction and prolongation subroutines. These revisions will require
!     significant extra storage and floating point operations. For details
!     on what needs to be done for this revision see UPDATE WHEN DOCUMENTING
!     IS FURTHER ALONG
!@q

 use ntypes, only:i4,r8
 use mgVars
 use mgPASS
 use Grid
 use Parameters, only:nstep
 use dd
 use domain
 use Flow, only:r

 implicit none

!Passed Variables
 real(r8),intent(inout)    :: phif(sxk(ngrid)-1:exk(ngrid)+1,syk(ngrid)-1:&
                                      eyk(ngrid)+1,szk(ngrid)-1:ezk(ngrid)+1)
 real(r8),intent(inout)    :: rhsf(sxk(ngrid)-1:exk(ngrid)+1,syk(ngrid)-1:&
                                      eyk(ngrid)+1,szk(ngrid)-1:ezk(ngrid)+1)
 integer(i4),intent(out)   :: iter
 real(r8),intent(out)      :: relmax
 integer(i4)                        :: nzf,nxm,nym,nzm
 integer(i4)                        :: sxm,exm,sym,eym,szm,ezm
 integer(i4)                        :: sxf,exf,syf,eyf,szf,ezf,nxf,nyf,ic,icf
 !LOOPING
 integer(i4)                        :: i,j,k
 integer(i4)                        :: kp3,kx1,kx2,kx3,kps!,kpbgn(20),kcbgn(20),kps

!Local Variables
 integer(i4)               :: ipf
 integer(i4)               :: ierr 


!rhsf should come in ghosted, since bc's on rhs are not known in general
!call mg_bdry(ngrid,rhsf,.true.,.true.)

! if(nstep.eq.1) then
        sxf=sxk(ngrid)
        exf=exk(ngrid)
        syf=syk(ngrid)
        eyf=eyk(ngrid)
        szf=szk(ngrid)
        ezf=ezk(ngrid)
        nxf=nxk(ngrid)
        nyf=nyk(ngrid)
        nzf=nzk(ngrid)
        ipf=kp3d(ngrid)
        call mgdpfpde(sxf,exf,syf,eyf,szf,ezf,nxf,nyf,nzf,workc1(ipf),workc2(ipf),workc3(ipf), &
                      workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf),r,xL,yL,zL,IOUT)
        do k=ngrid-1,1,-1
          sxm=sxk(k)
          exm=exk(k)
          sym=syk(k)
          eym=eyk(k)
          szm=szk(k)
          ezm=ezk(k)
          nxm=nxk(k)
          nym=nyk(k)
          nzm=nzk(k)
          ipf=kp3d(k)
          call mgdphpde(sxm,exm,sym,eym,szm,ezm,nxm,nym,nzm,workc1(ipf),workc2(ipf),workc3(ipf), &
                        workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf), &
                        sx,ex,sy,ey,sz,ez,nxf,nyf,nzf,r,bdMG,xL,yL,zL,IOUT)
        end do
! endif
!write(IOUTmg, *) "CALCULATED COEFF"

 call mg_bdry(ngrid,phif,.true.,.true.)

!write(IOUTmg, *) "CALCULATED BC"
 !Set phi,rhsf in work
 ipf=kp3d(ngrid)
 
 !phiF-->=work1(ipf)
 !rhsF-->=work2(ipf)
  call mg_set(work1(ipf),work2(ipf),phif,rhsf)
!write(IOUTmg, *) "CALCULATED MG SET"
 do iter=1,maxcy
  !phiF-->=work1(ipf)
  !rhsF-->=work2(ipf)
  !res--> =rhsF
!write(IOUTmg, *) "CALCULATED MG_CYCLE 1"
  call mg_cycle(workc1(ipf),workc2(ipf),workc3(ipf),workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf),rhsF,work1(ipf),work2(ipf),ngrid)
!write(IOUTmg, *) "CALCULATED MG_CYCLE 2"
  ipf=kp3d(ngrid)
  call mg_residual(phif,work1(ipf),relmax)
   if (verbose) write(IOUTmg,'(a,i5,a,e22.15)') "       Iteration: ",iter,"  RESIDUAL= ",relmax
   if (relmax.le.tolmax) goto 1000
 enddo

  !NOT CONVERGED in maxcy, issue an error message and quit
  write(IOUTmg,'(a,i6,a,f22.16)') "WARNING: failed to achieve &
           & convergence in  ",maxcy,"cycles, RESIDUAL=",relmax

 !CONVERGED
 1000  continue

 !UPDATE THE BOUDARY PLANES AND RETURN

 call mg_bdry(ngrid,phif,.true.,.true.)
 return 
end subroutine mg_solver

