












subroutine MG_init(stat)
!@h
! \textbf{subroutine MG\_init(stat)}
!@h
!   Description:
!     Initialize the parallel multigrid solver: subdomain indices, MPI
!     datatypes, boundary values for Dirichlet boundaries.
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
!     Based on Bernard Bunner's (bunner@engin.umich.edu) mgd3 solver
!     available at www.mgnet.org
!------------------------------------------------------------------------
!           |                       |
!        |--|--|-----|-----|-----|--|--|       fine
!        1  |  2     3     4     5  |  6
!           |                       |
!     |-----|-----|-----------|-----|-----|    coarse
!     1     |     2           3     |     4
!           |                       |
!          wall                    wall
!
!     The option to coarsify in one direction and not the other is available
!     except at the finest grid level, where coarsifying MUST take place
!     along all axes. However, it is possible to have
!     ngrid=nx1_lvls=nx2_lvls=kzr=1, with nx1F=nx, nx2F=ny, and nx3F=nz. In
!     this case, the code never enters 'mg_restr' and 'mg_cor' and all it
!     does is Gauss-Seidel iterate at the finest grid level. This can be
!     useful as a preliminary check.
!@q

 use ntypes, only:i4,r8
 use mgVars
 use mgPASS
 use Grid
 use dd
 use domain
 use Flow, only:r

 implicit none

 !Passed Variables
 integer(i4),intent(out)            :: stat

 !Local Variables
 integer(i4)                        :: nzf,nxm,nym,nzm
 integer(i4)                        :: sxm,exm,sym,eym,szm,ezm
 integer(i4)                        :: sxf,exf,syf,eyf,szf,ezf,nxf,nyf,ic,icf

 !LOOPING
 integer(i4)                        :: i,j,k
 integer(i4)                        :: kp3,kx1,kx2,kx3,kps 

 !IO
 integer                            :: ierr11

 nx1Fp=nx
 nx2Fp=ny
 nx3Fp=nz

 IOUTmg=IOUT
 mgDIRmg=mgDIR
 ngrid=max(nx1Levels, nx2Levels,nx3Levels)
 nx1Cp=nx1Fp/2**(nx1Levels-1)
 nx2Cp=nx2Fp/2**(nx2Levels-1)
 nx3Cp=nx3Fp/2**(nx3Levels-1)

 call mg_allocate_1(ierr11)

!Set mgd module variables to zero
 do k=1,ngrid
  nxk(k)=0
  nyk(k)=0
  nzk(k)=0
  sxk(k)=0
  exk(k)=0
  syk(k)=0
  eyk(k)=0
  szk(k)=0
  ezk(k)=0
  kp3d(k)=0
  kpx1(k)=0
  kpx2(k)=0
  kpx3(k)=0
 enddo

 call mg_check_size(ierr11)

!For all grid levels, set the indices 
  !Grid is coarsened in all directions by a factor of 2
  do k=1,ngrid
   nxk(k)=nx1Cp*2**(max(k+nx1Levels-ngrid,1)-1)+1
   nyk(k)=nx2Cp*2**(max(k+nx2Levels-ngrid,1)-1)+1
   nzk(k)=nx3Cp*2**(max(k+nx3Levels-ngrid,1)-1)+1
  enddo

  sxk(ngrid)=sx
  exk(ngrid)=ex
  syk(ngrid)=sy
  eyk(ngrid)=ey
  szk(ngrid)=sz
  ezk(ngrid)=ez

  nxf=nxk(ngrid)
  nyf=nyk(ngrid)
  nzf=nzk(ngrid)


  do k=ngrid-1,1,-1
   nxm=nxk(k)
   nym=nyk(k)
   nzm=nzk(k)
   if (nxm.lt.nxf) then
    sxk(k)=sxk(k+1)/2+1
    exk(k)=(exk(k+1)-1)/2+1
   else
    sxk(k)=sxk(k+1)
    exk(k)=exk(k+1)
   endif
   if (nym.lt.nyf) then
    syk(k)=syk(k+1)/2+1
    eyk(k)=(eyk(k+1)-1)/2+1
   else
    syk(k)=syk(k+1)
    eyk(k)=eyk(k+1)
   end if
   if (nzm.lt.nzf) then
    szk(k)=szk(k+1)/2+1
    ezk(k)=(ezk(k+1)-1)/2+1
   else
    szk(k)=szk(k+1)
    ezk(k)=ezk(k+1)
   end if
    nxf=nxm
    nyf=nym
    nzf=nzm

    nxf=nxm
    nyf=nym
    nzf=nzm
  enddo

!Set index's for 1d arrays and check 
!that they are of sufficient length 
 kp3=1
 kx1=1
 kx2=1
 kx3=1
 kps=1
 do k=ngrid,1,-1
  sxm=sxk(k)
  exm=exk(k)
  sym=syk(k)
  eym=eyk(k)
  szm=szk(k)
  ezm=ezk(k)
  nxm=nxk(k)
  nym=nyk(k)
  nzm=nzk(k)
  kp3d(k)=kp3
  kpx1(k)=kx1
  kpx2(k)=kx2
  kpx3(k)=kx3
  kpbgn(k)=kps
  kcbgn(k)=kpbgn(k)+(exm-sxm+3)*(eym-sym+3)*(ezm-szm+3)
  kps=kcbgn(k)+8*(exm-sxm+3)*(eym-sym+3)*(ezm-szm+3)   
  kx1=kpx1(k)+(nxm-2+3)
  kx2=kpx2(k)+(nym-2+3)
  kx3=kpx3(k)+(nzm-2+3)
  kp3=kp3d(k)+(exm-sxm+3)*(eym-sym+3)*(ezm-szm+3)
 enddo

 !ALLOCATE WORK ARRAYS NOW THAT CORRECT SIZE IS KNOWN
 nwork3d=kp3
 nworkx1=kx1
 nworkx2=kx2
 nworkx3=kx3

 call mg_allocate_2(ierr11)

 call mg_grids(ierr11)

! Set the boundary values to be used for the Dirichlet boundaries. 
! It is possible to assign 6 different constant values to the 6 
! different sides. The values are assigned at the finest grid level, 
! zero is assigned at all levels below
! vbc, phibc:
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


!Set bd(i) for the 6 faces coresponding to the information in Tbc
!bd(1)=X2max
!bd(3)=X3min
!bd(5)=X2min
!bd(7)=X3max
!bd(9)=X1min
!bd(18)=X1max


 !X1min
 if (sx.LE.2) then
   bdMG(9) = tbc(5)
 else 
   bdMG(9) = 0
 endif

 !X1max
 if (ex.GE.nx+1) then
   bdMG(18) = tbc(6)
 else 
   bdMG(18) = 0
 endif

 !X2min
 if (sy.LE.2) then
   bdMG(5) = tbc(3)
 else 
   bdMG(5) = 0
 endif

 !X2max
 if (ey.GE.ny+1) then
   bdMG(1) = tbc(1)
 else 
   bdMG(1) = 0
 endif

 !X3min
 if (sz.LE.2) then
   bdMG(3) = tbc(2)
 else 
   bdMG(3) = 0
 endif

 !X3max
 if (ez.GE.nz+1) then
   bdMG(7) = tbc(4)
 else 
   bdMG(7) = 0
 endif

 do j=1,6
   phibc(ngrid,j)=vbc(j)
 end do

 do k=ngrid-1,1,-1
  do j=1,6
   phibc(k,j)=0.0d0
  end do
 end do

 do i=1,26
  if (bdMG(i).GT.2.or.bdMG(i).LT.0) then
      write(IOUTmg,'(a)') "MG INITIALIZATION FAILED BAD BOUNDARY CONDITION CHECK",i
   goto 1000
  endif
 enddo

 stat=0
   write(IOUTmg,'(a)') "MG INITIALIZATION COMPLETED"
 return

 1000 continue
 stat=-1
   write(IOUTmg,'(a)') "MG INITIALIZATION FAILED"

 return
end
