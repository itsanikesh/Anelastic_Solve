program mg_test 
!@t
! \textbf{program mg\_test}
!@h
!   Description:
!     Test program for the pressure solver.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       02/2009  Original code. [Kyle A. Brucker] 
!     1.1       05/2009  New .ini file. [Matt de Stadler] 
!@h
!   Comments:
!     There are a number of tests of exact solutions that can be used to 
!     test the accuracy and speed of the pressure solver. One may also
!     compute a solution, write out and then test the solver against that
!     solution using the write_solution and startup subroutines. An optimal
!     domain decomposition can result in a 15% savings in total runtime,
!     this is a tool that can be used to quickly determine that. 
!@q
 use ntypes, only: i4, r8
 use mgVARS 
 use Domain 
 use Grid
#ifdef PARALLEL
 use dd
#endif
 use IO
 use ptester
 use Parameters, only: nstep

 implicit none

 !LOOPING
 integer(i4)  :: i,j,k, l, m

 !STATUS
 integer(i4)  :: ok1, ok2

!**********************************************************************
!***************************START MAIN*********************************
!**********************************************************************
#ifdef PARALLEL
 !MPI INITIALIZATION
 call MPI_INIT(ok1)

 call MPI_COMM_RANK(commx1x2x3,myid,ok1)
  call MPI_BARRIER(commx1x2x3,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 call MPI_COMM_SIZE(commx1x2x3,sizex1x2x3,ok1)
  call MPI_BARRIER(commx1x2x3,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

! Create a duplicate communicator required for load_grid
 call MPI_COMM_DUP(commx1x2x3,comm3d,ok1)
  call MPI_BARRIER(commx1x2x3,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999
#endif

 ! Read settings from ini file
 call read_inputptest(iniFILE,ok1)
 call mg_read_params(iniFILE,ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !******************
 !******IO**********
 !******************
#ifdef PARALLEL
 if (myid.EQ.0) then
  IOUT=6            ! write to screen
 else
  IOUT=0
 endif
#else
  IOUT=6            ! write to screen
#endif

 call allocation(ok1) ! Allocate grid variables
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !******************
 !******GRID********
 !******************
#ifdef PARALLEL
 if(myid.EQ.0) psolvetime= MPI_WTIME()
#endif
 call load_grid(ok1)
 call writeout_grid(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

#ifdef PARALLEL
 call MPI_Barrier( commx1x2x3,ok1)
 if(myid.EQ.0) psolvetime = MPI_WTIME() - psolvetime
 if(myid.EQ.0) write(6,*) 'time to read grid: ', psolvetime
#endif
 !******************
 !Pencil/Plane Setup
 !******************
#ifdef PARALLEL
 call pencil_plane_setup(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999
#endif

 !******************
 !****MPI SETUP*****
 !******************
#ifdef PARALLEL
 call mpi_setup(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999
#else
 sx  = 2          ! this section is enforce in mpi_setup so 
 sy  = 2          ! it must be defined manually for the 
 sz  = 2          ! serial case
 ex  = nx+1
 ey  = ny+1
 ez  = nz+1
#endif

 !******************
 !****SETUP TEST****
 !******************
 allocate( phi(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1),stat=ok1 )
 allocate( src(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1),stat=ok1 )
 allocate( error(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1),stat=ok1 )
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 phi(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)=0.0d0
 src(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)=0.0d0
 pi=4.0d0*datan(1.0d0)
 cx=1.0d0*pi*wk
 cy=1.0d0*pi*wk
 cz=1.0d0*pi*wk

 !**********************************
 !****SETUP AUTOMATIC TEST CASES****
 !**********************************
 
 select case(testcase)
 case('dirichletBCs')
! Test 1: del^2(p) = rhs, p(-Lz/2) = a, p(Lz/2) = b
!             p(z) = 0.5*rhs*z^2 + b1*z + c1
!               b1 = (a-b)/(zc(-Lz/2)-zc(Lz/2)) - 0.5*rhs*(zc(-Lz/2)+zc(Lz/2))
!               c1 = 0.5 * [ (a+b) - 0.5*rhs*(zc(-Lz/2)^2+zc(Lz/2)^2) - b1*(zc(-Lz/2)+zc(Lz/2)) ]

 tbc(:)=0   ! enforce that all boundaries are periodic, then update the dirichlet BC's
 a1 = 2.d0
 src(:,:,:)=a1

   select case(testdir1D)
    case('x1')
     tbc(5) = 2 ! xmin
     tbc(6) = 2 !  xmax
     vbc(5) = 1.d0
     vbc(6) = 2.d0

     b1 =  (vbc(5) - vbc(6))/(xc(1)-xc(nxp2)) - a1/2*(xc(1)+xc(nxp2)) 
     c1 =  (vbc(5) + vbc(6))/2.d0 - a1/4.d0*(xc(1)**2+xc(nxp2)**2) - b1/2.d0*(xc(1)+xc(nxp2)) 
     phi(:,:,:)= 0.5d0*(vbc(5)+vbc(6))
   case('x2')
     tbc(3) = 2 ! xmin
     tbc(1) = 2 !  xmax
     vbc(3) = 1.d0
     vbc(1) = 2.d0

     b1 =  (vbc(3) - vbc(1))/(yc(1)-yc(nyp2)) - a1/2*(yc(1)+yc(nyp2)) 
     c1 =  (vbc(3) + vbc(1))/2.d0 - a1/4.d0*(yc(1)**2+yc(nyp2)**2) - b1/2.d0*(yc(1)+yc(nyp2)) 
     phi(:,:,:)= 0.5d0*(vbc(3)+vbc(1))
 
   case('x3')
     tbc(2) = 2 ! zmin
     tbc(4) = 2 ! zmax
     vbc(2) = 1.d0
     vbc(4) = 2.d0

     b1 =  (vbc(2) - vbc(4))/(zc(1)-zc(nzp2)) - a1/2*(zc(1)+zc(nzp2)) 
     c1 =  (vbc(2) + vbc(4))/2.d0 - a1/4.d0*(zc(1)**2+zc(nzp2)**2) - b1/2.d0*(zc(1)+zc(nzp2)) 
     phi(:,:,:)= 0.5d0*(vbc(4)+vbc(2))
   end select

 case('linearpgrad')
! Test 2: del^2(p) = 0, p(-Lz/2) = a, p(Lz/2) = b
!               This is a special case of the above solution when rhs = 0
!               The solution is a linear gradient from a to b

 tbc(:)=0 ! enforce that all boundaries are periodic, then update the dirichlet BC's
 a1 = 0.d0
 src(:,:,:)=a1
   select case(testdir1D)
    case('x1')
     tbc(5) = 2 ! xmin
     tbc(6) = 2 ! xmax
     vbc(5) = 1.d0
     vbc(6) = 2.d0

     b1 =  (vbc(5) - vbc(6))/(xc(1)-xc(nxp2)) - a1/2*(xc(1)+xc(nxp2)) 
     c1 =  (vbc(5) + vbc(6))/2.d0 - a1/4.d0*(xc(1)**2+xc(nxp2)**2) - b1/2.d0*(xc(1)+xc(nxp2)) 
     phi(:,:,:)= 0.5d0*(vbc(6)+vbc(5))
    case('x2')
     tbc(3) = 2 ! ymin
     tbc(1) = 2 ! ymax
     vbc(3) = 1.d0
     vbc(1) = 2.d0

     b1 =  (vbc(3) - vbc(1))/(yc(1)-yc(nyp2)) - a1/2*(yc(1)+yc(nyp2)) 
     c1 =  (vbc(3) + vbc(1))/2.d0 - a1/4.d0*(yc(1)**2+yc(nyp2)**2) - b1/2.d0*(yc(1)+yc(nyp2)) 
     phi(:,:,:)= 0.5d0*(vbc(1)+vbc(3))
    case('x3')
     tbc(2) = 2 ! zmin
     tbc(4) = 2 ! zmax
     vbc(2) = 1.d0
     vbc(4) = 2.d0

     b1 =  (vbc(2) - vbc(4))/(zc(1)-zc(nzp2)) - a1/2*(zc(1)+zc(nzp2)) 
     c1 =  (vbc(2) + vbc(4))/2.d0 - a1/4.d0*(zc(1)**2+zc(nzp2)**2) - b1/2.d0*(zc(1)+zc(nzp2)) 
     phi(:,:,:)= 0.5d0*(vbc(4)+vbc(2))
   end select

 case('1dirichlet1NeumannBC')
! Test 3: del^2(p) = rhs, p(-Lz/2) = a, dp/dz(Lz/2) = b
!             p(z) = 0.5*rhs*z^2 + b1*z + c1
!               b1 = b-rhs*Lz/2         ! note: defining Lz/2 is hard for the stretched case
!                                       ! It is NOT correct currently for stretched grids
!               c1 = a-0.5*rhs*(-Lz/2)^2 + b1*(-Lz/2)

 tbc(:)=0 ! enforce that all boundaries are periodic, then update the dirichlet and Neumann BC's
 a1 = 2.d0
 src(:,:,:)=a1

   select case(testdir1D)
    case('x1')
     tbc(5) = 2 ! xmin
     tbc(6) = 1 ! xmax
     vbc(5) = 1.d0
     vbc(6) = 0.d0

     b1 =  vbc(6) - a1*0.5d0*(xc(nxp2)+xc(nxp2-1))
     c1 =  vbc(5) - a1*0.5d0*xc(1)**2 - b1*xc(1)
     phi(:,:,:)= 0.5d0*(vbc(6)+vbc(5))
    case('x2')
     tbc(3) = 2 ! ymin
     tbc(1) = 1 ! ymax
     vbc(3) = 1.d0
     vbc(1) = 0.d0

     b1 =  vbc(1) - a1*0.5d0*(yc(nyp2)+yc(nyp2-1))
     c1 =  vbc(3) - a1*0.5d0*yc(1)**2 - b1*yc(1)
     phi(:,:,:)= 0.5d0*(vbc(1)+vbc(3))
    case('x3')
     tbc(2) = 2 ! zmin
     tbc(4) = 1 ! zmax
     vbc(2) = 1.d0
     vbc(4) = 0.d0

     b1 =  vbc(4) - a1*0.5d0*(zc(nzp2)+zc(nzp2-1))
     c1 =  vbc(2) - a1*0.5d0*zc(1)**2 - b1*zc(1)
     phi(:,:,:)= 0.5d0*(vbc(4)+vbc(2))
   end select

 case('1dsine')
! Test 4: del^2(p) = -(c_z^2)sin(c_z*z), periodic BCs on all faces
!             p(z) = sin(c_z*z) + b1*z + c1
!             b1   = (sin(c_z*z(1))-sin(c_z)*z(nz)) / (z(nz) - z(1))
!             c1 is undetermined
!             Note that it is easier to solve this on a symmetric mesh, use ze instead of zc 

 tbc(:)=0 ! enforce that all boundaries are periodic

   select case(testdir1D)
    case('x1')
     cx=2.0d0*pi*wk
     cy=0.0d0*pi*wk
     cz=0.0d0*pi*wk
     b1 =  (dsin(cx*xe(1))-dsin(cx*xe(nxp2-1)))/(xe(nxp2-1)-xe(1))
     cnst=-(cx**2+cy**2+cz**2)

     do k=sz-1,ez+1
      do j=sy-1,ey+1
       do i=sx-1,ex+1
        src(i,j,k)= cnst*dsin(cx*xe(i))
       enddo
      enddo
     enddo
  call check(ok1,ok2) ! Barrier if parallel
     cx=0.0d0*pi*wk
     cy=2.0d0*pi*wk
     cz=0.0d0*pi*wk
     b1 =  (dsin(cy*ye(1))-dsin(cy*ye(nyp2-1)))/(ye(nyp2-1)-ye(1))
     cnst=-(cx**2+cy**2+cz**2)

     do k=sz-1,ez+1
      do j=sy-1,ey+1
       do i=sx-1,ex+1
        src(i,j,k)= cnst*dsin(cy*ye(j))
       enddo
      enddo
     enddo
  call check(ok1,ok2) ! Barrier if parallel
    case('x3')
     cx=0.0d0*pi*wk
     cy=0.0d0*pi*wk
     cz=2.0d0*pi*wk
     b1 =  (dsin(cz*ze(1))-dsin(cz*ze(nzp2-1)))/(ze(nzp2-1)-ze(1))
     cnst=-(cx**2+cy**2+cz**2)

     do k=sz-1,ez+1
      do j=sy-1,ey+1
       do i=sx-1,ex+1
        src(i,j,k)= cnst*dsin(cz*ze(k))
       enddo
      enddo
     enddo
  call check(ok1,ok2) ! Barrier if parallel
   end select

 case('3dsines')
! Test 5: del^2(p) = -(c_x^2+c_y^2+c_z^2)sin(c_x*x)sin(c_y*y)sin(c_z*z), periodic BCs on all faces
!         p(x,y,z) ~ sin(c_x*x)*sin(c_y*y)*sin(c_z*z)
!         Note that this is not exactly correct, the full solution to this problem is difficult to obtain 

 tbc(:)=0 ! enforce that all boundaries are periodic

 cx=2.0d0*pi*wk
 cy=4.0d0*pi*wk
 cz=16.0d0*pi*wk
 cnst=-(cx**2+cy**2+cz**2)

 do k=sz-1,ez+1
  do j=sy-1,ey+1
   do i=sx-1,ex+1
    src(i,j,k)=cnst*dsin(cx*xc(i))*dsin(cy*yc(j))*dsin(cz*zc(k)) 
   enddo
  enddo
 enddo
  call check(ok1,ok2) ! Barrier if parallel

 case('cfdhw')
! Test 6: del^2(p) = 0, p(-Ly/2) = a, p(Ly/2) = b
!                    p_z(-Lz/2) = p_z(Lz/2) = 0
! 	             p(y) = b1*y + c1
! This is is very similar to case linearpgrad in x2. BCs have been changed from
! periodic to neumann in the x3 direction. The initial conditions are different 
! as well

  tbc(:)=0   ! enforce that all boundaries are periodic, then update the dirichlet BC's
  a1 = 0
  src(:,:,:)=a1
  tbc(3) = 2 ! ymin
  tbc(1) = 2 ! ymax
  vbc(3) = 1.d0
  vbc(1) = 0.d0
  tbc(2) = 1 ! zmin
  tbc(4) = 1 ! zmax
  vbc(2) = 0.d0
  vbc(4) = 0.d0

  b1 =  (vbc(3) - vbc(1))/(yc(1)-yc(nyp2)) - a1/2*(yc(1)+yc(nyp2)) 
  c1 =  (vbc(3) + vbc(1))/2.d0 - a1/4.d0*(yc(1)**2+yc(nyp2)**2) - b1/2.d0*(yc(1)+yc(nyp2)) 

 do l=1,8
 do m=1,8
  do k=sz-1,ez+1
   do j=sy-1,ey+1
    do i=sx-1,ex+1
     phi(i,j,k)=dsin(2*pi*l*yc(j)/yL+2*pi*m*zc(k)/zL)
    enddo
   enddo
  enddo
 enddo
 enddo

end select

! fluctuations on the initial guess term
 if (phiflucs) then
   do k=sz-1,ez+1
    do j=sy-1,ey+1
     do i=sx-1,ex+1
       phi(i,j,k)= phi(i,j,k) + &
                0.25*(  dsin(cx*xc(i))*dsin(cy*yc(j))*dsin(cz*zc(k)) + & 
                  dsin(cx*2.d0*xc(i))*dsin(cy*2.d0*yc(j))*dsin(cz*2.d0*zc(k)) + & 
                  dsin(cx*4.d0*xc(i))*dsin(cy*4.d0*yc(j))*dsin(cz*4.d0*zc(k)) + & 
                  dsin(cx*8.d0*xc(i))*dsin(cy*8.d0*yc(j))*dsin(cz*8.d0*zc(k)) )
     enddo
    enddo
   enddo
 endif
  call check(ok1,ok2) ! Barrier if parallel

! fluctuations on the source term
 if (srcflucs) then
   do k=sz-1,ez+1
    do j=sy-1,ey+1
     do i=sx-1,ex+1
       src(i,j,k)= src(i,j,k) + &
                0.25*(  dsin(cx*xc(i))*dsin(cy*yc(j))*dsin(cz*zc(k)) + & 
                  dsin(cx*2.d0*xc(i))*dsin(cy*2.d0*yc(j))*dsin(cz*2.d0*zc(k)) + & 
                  dsin(cx*4.d0*xc(i))*dsin(cy*4.d0*yc(j))*dsin(cz*4.d0*zc(k)) + & 
                  dsin(cx*8.d0*xc(i))*dsin(cy*8.d0*yc(j))*dsin(cz*8.d0*zc(k)) )
     enddo
    enddo
   enddo
 endif
  call check(ok1,ok2) ! Barrier if parallel

! do k=sz-1,ez+1
!  do j=sy-1,ey+1
!   do i=sx-1,ex+1
!    src(i,j,k)=cnst*dsin(cx*xc(i))*dsin(cy*yc(j))*dsin(cz*zc(k)) !*dexp(-zc(k)**2/0.05d0)*1000000.d0
!   enddo
!  enddo
! enddo
!  call check(ok1,ok2) ! Barrier if parallel

 !******************
 !**INIT MG SOLVER**
 !******************
#ifdef PARALLEL
 if(myid.EQ.0) psolvetime= MPI_WTIME()
#endif
 call mg_init(ok1)
  call check(ok1,ok2) ! Barrier if parallel
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

#ifdef PARALLEL
 call MPI_Barrier( commx1x2x3,ok1)
 if(myid.EQ.0) psolvetime = MPI_WTIME() - psolvetime
 if(myid.EQ.0) write(6,*) 'mg init time: ', psolvetime
#endif

 ! TAKE INITIAL TIMING MEASUREMENT
#ifdef PARALLEL
 if(myid.EQ.0) psolvetime= MPI_WTIME()
#else
 call system_clock ( clock_start,clock_rate,j )
#endif

 !******************
 !****MG SOLVER*****
 !******************
 call mg_solver(phi,src,iterations,residual)
  write(IOUT,'(a24,i3,a28,e15.8)') 'MGKB Solver completed in: ',iterations,' iterations, with residual:',residual

 ! TAKE FINAL TIMING MEASUREMENT
#ifdef PARALLEL
 call MPI_Barrier( commx1x2x3,ok1)
 if(myid.EQ.0) psolvetime = MPI_WTIME() - psolvetime
#else
 call system_clock ( clock_stop,clock_rate,j )
 psolvetime = real(clock_stop - clock_start, kind = 8)/real( clock_rate, kind = 8)
#endif

 !*******************
 !**EXACT SOLUTIONS**
 !*******************

 locE=0.d0
 GlobE=0.d0
 error(:,:,:)=0.d0

 select case(testcase)
  case('dirichletBCs','linearpgrad','1dirichlet1NeumannBC')
   select case(testdir1D)
    case('x1')
     do k=sz,ez
      do j=sy,ey
       do i=sx,ex
        exact = a1*(xc(i)**2)/2.d0 + b1*xc(i) + c1
        locE=locE+dabs(phi(i,j,k)-exact)
        error(i,j,k)=phi(i,j,k)-exact
       enddo
      enddo
     enddo
    case('x2')
     do k=sz,ez
      do j=sy,ey
       do i=sx,ex
        exact = a1*(yc(j)**2)/2.d0 + b1*yc(j) + c1
        locE=locE+dabs(phi(i,j,k)-exact)
        error(i,j,k)=phi(i,j,k)-exact
       enddo
      enddo
     enddo
    case('x3')
     do k=sz,ez
      do j=sy,ey
       do i=sx,ex
        exact = a1*(zc(k)**2)/2.d0 + b1*zc(k) + c1
        locE=locE+dabs(phi(i,j,k)-exact)
        error(i,j,k)=phi(i,j,k)-exact
       enddo
      enddo
     enddo
   end select

  case('1dsine')
   select case(testdir1D)
    case('x1')
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
       exact=dsin(cx*xe(i))+b1*xe(i)
       locE=locE+dabs(phi(i,j,k)-exact)
       error(i,j,k)=phi(i,j,k)-exact
      enddo
     enddo
    enddo
    case('x2')
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
       exact=dsin(cy*ye(j))+b1*ye(j)
       locE=locE+dabs(phi(i,j,k)-exact)
       error(i,j,k)=phi(i,j,k)-exact
      enddo
     enddo
    enddo
    case('x3')
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
       exact=dsin(cz*ze(k))+b1*ze(k)
       locE=locE+dabs(phi(i,j,k)-exact)
       error(i,j,k)=phi(i,j,k)-exact
      enddo
     enddo
    enddo
   end select
 case('3dsines')
    ! note that this is not really correct, be careful when using this solution
    ! it has problems with large values of x,y,z. The solver is very sensitive
    ! to the true periodic length
  do k=sz,ez
  do j=sy,ey
   do i=sx,ex
    exact=dsin(cx*xc(i))*dsin(cy*yc(j))*dsin(cz*zc(k))
    locE=locE+dabs(phi(i,j,k)-exact)
    error(i,j,k)=phi(i,j,k)-exact
   enddo
  enddo
 enddo

 case('cfdhw')
     do k=sz,ez
      do j=sy,ey
       do i=sx,ex
        exact = a1*(yc(j)**2)/2.d0 + b1*yc(j) + c1
        locE=locE+dabs(phi(i,j,k)-exact)
        error(i,j,k)=phi(i,j,k)-exact
       enddo
      enddo
     enddo

end select


! do k=sz,ez
!  do j=sy,ey
!   do i=sx,ex
!    exact=dsin(cx*xc(i))*dsin(cy*yc(j))*dsin(cz*zc(k))
!    locE=locE+dabs(phi(i,j,k)-exact)
!    error(i,j,k)=phi(i,j,k)-exact
!   enddo
!  enddo
! enddo

#ifdef PARALLEL
 call MPI_ALLREDUCE(locE,globE,1,realtype,MPI_MAX,Commx1x2x3,ok1)
  call check(ok1,ok2) ! Barrier if parallel
  if (ok1.NE.0.or.ok2.NE.0) goto 9999
#endif

#ifdef PARALLEL
 write(IOUT,'(a12,e15.8,a12,e15.8)') 'Local error: ', locE/dble(nx*ny*nz),&
                                     ' Maximum error: ',globE/dble(nx*ny*nz)
#else
 write(IOUT,'(a12,e15.8)') 'Local error: ', locE/dble(nx*ny*nz)
#endif

! call startup(phi,'exsol',ok1)

 call write_solution(phi,'exsol',ok1)

 ! PRINT TIMING MEASUREMENT TO SCREEN
#ifdef PARALLEL
 if(myid.EQ.0) write(6,*) 'Pressure solver time to completion: ', psolvetime
#else
 write(6,*)  'Pressure solver time to completion: ', psolvetime
#endif

!nstep = 3

!PLANES
!x2-x3
 call write_plane(phi,1,nxp2/2,0,0,'phi',.true.,ok1)
 call write_plane(src,1,nxp2/2,0,0,'src',.true.,ok1)
 call write_plane(error,1,nxp2/2,0,0,'error',.true.,ok1)
!x1-x2
 call write_plane(phi,2,nyp2/2,0,0,'phi',.true.,ok1)
 call write_plane(src,2,nyp2/2,0,0,'src',.true.,ok1)
 call write_plane(error,2,nyp2/2,0,0,'error',.true.,ok1)
!x1-x3
 call write_plane(phi,3,nzp2/2,0,0,'phi',.true.,ok1)
 call write_plane(src,3,nzp2/2,0,0,'src',.true.,ok1)
 call write_plane(error,3,nzp2/2,0,0,'error',.true.,ok1)
  call check(ok1,ok2) ! Barrier if parallel
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

!PENCILS
 call write_pencil(phi,1,nyp2/2,nzp2/2,0,'phi',.true.,ok1)
 call write_pencil(src,1,nyp2/2,nzp2/2,0,'src',.true.,ok1)
 call write_pencil(error,1,nyp2/2,nzp2/2,0,'error',.true.,ok1)

 call write_pencil(phi,2,nxp2/2,nzp2/2,0,'phi',.true.,ok1)
 call write_pencil(src,2,nxp2/2,nzp2/2,0,'src',.true.,ok1)
 call write_pencil(error,2,nxp2/2,nzp2/2,0,'error',.true.,ok1)

 call write_pencil(phi,3,nxp2/2,nyp2/2,0,'phi',.true.,ok1)
 call write_pencil(src,3,nxp2/2,nyp2/2,0,'src',.true.,ok1)
 call write_pencil(error,3,nxp2/2,nyp2/2,0,'error',.true.,ok1)
  call check(ok1,ok2) ! Barrier if parallel
  if (ok1.NE.0.or.ok2.NE.0) goto 9999


 !CLEAN EXIT
 deallocate(phi,src,error,stat=ok1)
#ifdef PARALLEL
  call MPI_BARRIER(commx1x2x3,ok1)
  if (ok1.NE.0) goto 9999 
 call mpi_finalize(ok1)
  if (ok1.NE.0) goto 9999 
#endif
 write(IOUT,'(a)') "MG TEST FINISHED SUCCESSFULLY"
 stop

 !UNCLEAN EXIT
 9999 continue
 if (allocated(phi)) deallocate(phi)
 if (allocated(src)) deallocate(src)
 if (allocated(error)) deallocate(error)

 write(IOUT,'(a)') "ERROR IN TEST CODE ABORTING"
 stop
end program

#ifdef PARALLEL
subroutine pencil_plane_setup(stat)
!@t
! \textbf{setup\_grid(stat)}
!@h
!   Description:
!     creates transfer functions for use in writing planes with the
!     parallel version.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
 
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       02/2009  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     Not necessary for the serial version
!@q

  use ntypes, only: i4
  use Domain, only: nxp2,nyp2,nzp2
  use dd, only: PlnX1, PlnX2, PlnX3, PenX1, PenX2, PenX3
 implicit none

 !Passed Variables
 integer(i4),intent(out) :: stat

 !Local Variables
  integer(i4)        :: s1

 !Planes
 allocate( PlnX1(1:nyp2,1:nzp2,1:2),stat=s1 )
 allocate( PlnX2(1:nxp2,1:nzp2,1:2),stat=s1 )
 allocate( PlnX3(1:nxp2,1:nyp2,1:2),stat=s1 )
 !Pencils
 allocate( PenX1(1:nxp2,1:4),stat=s1 )
 allocate( PenX2(1:nyp2,1:4),stat=s1 )
 allocate( PenX3(1:nzp2,1:4),stat=s1 )

stat=s1
return
end subroutine pencil_plane_setup
#endif


subroutine writeout_grid(stat)
!@t
! \textbf{write\_grid(stat)}
!@h
!   Description:
!     Writes out the grid used.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       02/2009  Original code. [Kyle A. Brucker] 

use ntypes, only: i4, r8
 use Grid
#ifdef PARALLEL
 use dd, only: myid
#endif
 use Domain, only: nx,ny,nz,nxp2,nyp2,nzp2
 use IO, only: gridDIR
 implicit none

 integer,intent(out) :: stat
 real(r8) :: dxx,dyy,dzz
 integer :: i,j,k
 character(len=100) :: gridfile

 !WRITE OUT GRIDS
#ifdef PARALLEL
 if(myid.EQ.0) then ! write statement
#endif
 write(gridfile,'(a)') trim(gridDIR)//'/x1_grid.dat'
 open(unit=209,file=gridfile,status='unknown',form='formatted')
   write(209,'(a)') "#X1-GRID"
   write(209,'(a)') "#nxp2 Lx"
   write(209,'(a,i4,f12.4)') "#", nxp2, xL
   write(209,'(a)') "#i xe(i) xc(i) dxe(i) dxc(i)"
   do i=1,nxp2
    write(209,'(i3,4(1x,f15.8))') i,xe(i),xc(i),dxe(i),dxc(i)
   enddo
  close(209)

 write(gridfile,'(a)') trim(gridDIR)//'/x2_grid.dat'
 open(unit=208,file=gridfile,status='unknown',form='formatted')
   write(208,'(a)') "#X2-GRID"
   write(208,'(a)') "#nyp2 Ly"
   write(208,'(a,i4,f12.4)') "#", nyp2, yL
   write(208,'(a)') "#j ye(j) yc(j) dye(j) dyc(j)"
   do j=1,nyp2
    write(208,'(i3,4(1x,f15.8))') j,ye(j),yc(j),dye(j),dyc(j)
   enddo
  close(208)

 write(gridfile,'(a)') trim(gridDIR)//'/x3_grid.dat'
 open(unit=209,file=gridfile,status='unknown',form='formatted')
   write(209,'(a)') "#X3-GRID"
   write(209,'(a)') "#nzp2 Lz"
   write(209,'(a,i4,f12.4)') "#", nzp2, zL
   write(209,'(a)') "#k ze(k) zc(k) dze(k) dzc(k)"
   do k=1,nzp2
    write(209,'(i3,4(1x,f15.8))') k,ze(k),zc(k),dze(k),dzc(k)
   enddo
  close(209)

#ifdef PARALLEL
 endif ! write statement
#endif

 stat=0
return
end subroutine writeout_grid 

subroutine allocation(ok)
!@t
! \textbf{subroutine allocation(ok)}
!@h
!   Description:
!     Allocate the grid and flow variables.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
 
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
 

 use Domain
 use Grid
 use IO,     only: IOUT
 implicit none

!Passed Variables
 integer,intent(out)       :: ok

!Local Variables
 integer                    :: s1
  
!GRID ALLOCATION
 !X1-Direction
 allocate( xe(1:nxp2), stat=s1 )
 allocate( xc(1:nxp2), stat=s1 )
 allocate( dxe(1:nxp2), stat=s1 )
 allocate( dxc(1:nxp2), stat=s1 )
 allocate( rdxe(1:nxp2), stat=s1 )
 allocate( rdxc(1:nxp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating X1 Grid Variables" 
  goto 1000
 endif

 !X2-Direction
 allocate( ye(1:nyp2), stat=s1 )
 allocate( yc(1:nyp2), stat=s1 )
 allocate( dye(1:nyp2), stat=s1 )
 allocate( dyc(1:nyp2), stat=s1 )
 allocate( rdye(1:nyp2), stat=s1 )
 allocate( rdyc(1:nyp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating X2 Grid Variables"
  goto 1000
 endif

 !X3-Direction
 allocate( ze(1:nzp2), stat=s1 )
 allocate( zc(1:nzp2), stat=s1 )
 allocate( dze(1:nzp2), stat=s1 )
 allocate( dzc(1:nzp2), stat=s1 )
 allocate( rdze(1:nzp2), stat=s1 )
 allocate( rdzc(1:nzp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating X3 Grid Variables"
  goto 1000
 endif

 ok = s1
 write(IOUT,'(a)') "GRID ALLOCATION COMPLETED"
 return

 1000 continue
 ok = s1
 write(IOUT,'(a)') "GRID ALLOCATION FAILED"
 return

end subroutine allocation

subroutine check(ecode1,ecode2)
!@t
! \textbf{subroutine check(ecode1,ecode2)}
!@h
!   Description:
!     Use an MPI Barrier if parallel.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

#ifdef PARALLEL
 use dd, only: commx1x2x3
#endif
 implicit none

 integer,intent(IN)  :: ecode1
 integer,intent(OUT) :: ecode2

 ecode2 = 0
#ifdef PARALLEL
  call MPI_BARRIER(commx1x2x3,ecode2)
#endif
return
end subroutine check
