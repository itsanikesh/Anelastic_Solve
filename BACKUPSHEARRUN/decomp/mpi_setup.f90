subroutine mpi_setup(stat)
!@t
! \textbf{subroutine mpi\_setup(stat)}
!@h
!   Description:
!     This routine creates a 3D topology and 1d/2d sub-communicators.
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
!     Numbering from Bernard Bunner's MGD3 code
! plane i
!      6|     7     |8
!      ---------------                k
!       |           |               ^ 
!       |           |               |
!      5|    myid   |1              |
!       |           |              ------>
!       |           |               |     j
!      ---------------
!      4|     3     |2
!
! plane i-1                  plane i+1
!     15|    16     |17          24|     25    |26 
!      ---------------            --------------- 
!       |           |              |           |
!       |           |              |           |
!     14|     9     |10          23|     18    |19
!       |           |              |           |
!       |           |              |           |
!      ---------------            ---------------
!     13|    12     |11          22|     21    |20
!@q

 use ntypes, only: i4, r8
 use Domain, only: sx,ex,cex,sy,ey,sz,ez,nx,ny,nz,EU,EV,EW
 use dd 
 use IO

 implicit none

!Passed Variables
 integer(i4),intent(out)       :: stat  !0=sucess 

!Local Variables
 integer(i4)                   :: i,j,k, m0, m1,s1
 integer(i4)                   :: ngb(3)
 logical                       :: belongs(3)
 character(len=200)            :: outfile
 integer(i4)                   :: nxdim, nydim, nzdim
 logical                       :: cfile

 integer                       :: IOUTL

 logical :: debug = .false.

 stat=0

 if (debug) then
  IOUTL=IOUT !output to screen or output file
 else
  IOUTL=0 !redirect to stderr
 endif

!Check that the number of processes is correct
 if (sizex1x2x3.NE.(nxprocs*nyprocs*nzprocs)) then
  write(IOUT,'(a63,i4,a4,i4)') 'ERROR: numprocs <> (nxprocs*nyprocs*nzprocs)',&
                   sizex1x2x3,' <> ',nxprocs*nyprocs*nzprocs
  stat=-1
  goto 1000
 endif

!Create Cartesian topology with periodic boundary conditions
 dims(1)=nxprocs
 dims(2)=nyprocs
 dims(3)=nzprocs

 periods(1)=.true.
 periods(2)=.true.
 periods(3)=.true.

 call MPI_CART_CREATE(commx1x2x3,3,dims,periods,.true., &
                      comm3d,stat)
 call MPI_COMM_DUP(comm3d,comm3dp,stat)
 call MPI_COMM_DUP(comm3d,comm3dl,stat)
 call MPI_COMM_DUP(comm3d,comm3dc,stat)
 call MPI_COMM_RANK(comm3d,myid,stat)
 call MPI_CART_GET(comm3d,3,dims,periods,coords,stat)

! find the six neighboring blocks, m1 is shorthand for - 1, p1 for + 1
 call MPI_CART_SHIFT(comm3d,0,1,nbrx1m1, nbrx1p1, stat)
 call MPI_CART_SHIFT(comm3d,1,1,nbrx2m1, nbrx2p1, stat)
 call MPI_CART_SHIFT(comm3d,2,1,nbrx3m1, nbrx3p1, stat)

!Find the neighbors: Numbering from Bernard Bunner's MGD3 code
! plane i
!      6|     7     |8
!      ---------------                k
!       |           |               ^ 
!       |           |               |
!      5|    myid   |1              |
!       |           |              ------>
!       |           |               |     j
!      ---------------
!      4|     3     |2
!
! plane i-1                  plane i+1
!     15|    16     |17          24|     25    |26 
!      ---------------            --------------- 
!       |           |              |           |
!       |           |              |           |
!     14|     9     |10          23|     18    |19
!       |           |              |           |
!       |           |              |           |
!      ---------------            ---------------
!     13|    12     |11          22|     21    |20



!Neighbors which share a plane with 'myid'
 ngb(1)=coords(1)
 ngb(2)=coords(2)+1
 ngb(3)=coords(3)
 call MPI_CART_RANK(comm3d,ngb,neighbor(1),stat)
 ngb(1)=coords(1)
 ngb(2)=coords(2)
 ngb(3)=coords(3)-1
 call MPI_CART_RANK(comm3d,ngb,neighbor(3),stat)
 ngb(1)=coords(1)
 ngb(2)=coords(2)-1
 ngb(3)=coords(3)
 call MPI_CART_RANK(comm3d,ngb,neighbor(5),stat)
 ngb(1)=coords(1)
 ngb(2)=coords(2)
 ngb(3)=coords(3)+1
 call MPI_CART_RANK(comm3d,ngb,neighbor(7),stat)
 ngb(1)=coords(1)-1
 ngb(2)=coords(2)
 ngb(3)=coords(3)
 call MPI_CART_RANK(comm3d,ngb,neighbor(9),stat)
 ngb(1)=coords(1)+1
 ngb(2)=coords(2)
 ngb(3)=coords(3)
 call MPI_CART_RANK(comm3d,ngb,neighbor(18),stat)

!Neighbors which share a line with 'myid'
 ngb(1)=coords(1)
 ngb(2)=coords(2)+1
 ngb(3)=coords(3)-1
 call MPI_CART_RANK(comm3d,ngb,neighbor(2),stat)
 ngb(1)=coords(1)
 ngb(2)=coords(2)-1
 ngb(3)=coords(3)-1
 call MPI_CART_RANK(comm3d,ngb,neighbor(4),stat)
 ngb(1)=coords(1)
 ngb(2)=coords(2)-1
 ngb(3)=coords(3)+1
 call MPI_CART_RANK(comm3d,ngb,neighbor(6),stat)
 ngb(1)=coords(1)
 ngb(2)=coords(2)+1
 ngb(3)=coords(3)+1
 call MPI_CART_RANK(comm3d,ngb,neighbor(8),stat)
 ngb(1)=coords(1)-1
 ngb(2)=coords(2)+1
 ngb(3)=coords(3)
 call MPI_CART_RANK(comm3d,ngb,neighbor(10),stat)
 ngb(1)=coords(1)-1
 ngb(2)=coords(2)
 ngb(3)=coords(3)-1
 call MPI_CART_RANK(comm3d,ngb,neighbor(12),stat)
 ngb(1)=coords(1)-1
 ngb(2)=coords(2)-1
 ngb(3)=coords(3)
 call MPI_CART_RANK(comm3d,ngb,neighbor(14),stat)
 ngb(1)=coords(1)-1
 ngb(2)=coords(2)
 ngb(3)=coords(3)+1
 call MPI_CART_RANK(comm3d,ngb,neighbor(16),stat)
 ngb(1)=coords(1)+1
 ngb(2)=coords(2)+1
 ngb(3)=coords(3)
 call MPI_CART_RANK(comm3d,ngb,neighbor(19),stat)
 ngb(1)=coords(1)+1
 ngb(2)=coords(2)
 ngb(3)=coords(3)-1
 call MPI_CART_RANK(comm3d,ngb,neighbor(21),stat)
 ngb(1)=coords(1)+1
 ngb(2)=coords(2)-1
 ngb(3)=coords(3)
 call MPI_CART_RANK(comm3d,ngb,neighbor(23),stat)
 ngb(1)=coords(1)+1
 ngb(2)=coords(2)
 ngb(3)=coords(3)+1
 call MPI_CART_RANK(comm3d,ngb,neighbor(25),stat)

!Neighbors which share a corner with 'myid'
 ngb(1)=coords(1)-1
 ngb(2)=coords(2)+1
 ngb(3)=coords(3)-1
 call MPI_CART_RANK(comm3d,ngb,neighbor(11),stat)
 ngb(1)=coords(1)-1
 ngb(2)=coords(2)-1
 ngb(3)=coords(3)-1
 call MPI_CART_RANK(comm3d,ngb,neighbor(13),stat)
 ngb(1)=coords(1)-1
 ngb(2)=coords(2)-1
 ngb(3)=coords(3)+1
 call MPI_CART_RANK(comm3d,ngb,neighbor(15),stat)
 ngb(1)=coords(1)-1
 ngb(2)=coords(2)+1
 ngb(3)=coords(3)+1
 call MPI_CART_RANK(comm3d,ngb,neighbor(17),stat)
 ngb(1)=coords(1)+1
 ngb(2)=coords(2)+1
 ngb(3)=coords(3)-1
 call MPI_CART_RANK(comm3d,ngb,neighbor(20),stat)
 ngb(1)=coords(1)+1
 ngb(2)=coords(2)-1
 ngb(3)=coords(3)-1
 call MPI_CART_RANK(comm3d,ngb,neighbor(22),stat)
 ngb(1)=coords(1)+1
 ngb(2)=coords(2)-1
 ngb(3)=coords(3)+1
 call MPI_CART_RANK(comm3d,ngb,neighbor(24),stat)
 ngb(1)=coords(1)+1
 ngb(2)=coords(2)+1
 ngb(3)=coords(3)+1
 call MPI_CART_RANK(comm3d,ngb,neighbor(26),stat)

!Create 1D communicators
 !3.2 create X1 sub-communicator
 belongs(1) = .true.
 belongs(2) = .false.
 belongs(3) = .false.
 call MPI_CART_SUB(comm3d,belongs,commx1,stat)
 call MPI_COMM_RANK(commx1,rankx1,stat)
 call MPI_COMM_SIZE(commx1,sizex1,stat)

 !3.3 create X2 sub-communicator
 belongs(1) = .false.
 belongs(2) = .true.
 belongs(3) = .false.
 call MPI_CART_SUB(comm3D,belongs,commx2,stat)
 call MPI_COMM_RANK(commx2,rankx2,stat)
 call MPI_COMM_SIZE(commx2,sizex2,stat)

 !3.4 create X3 sub-communicator
 belongs(1) = .false.
 belongs(2) = .false.
 belongs(3) = .true.
 call MPI_CART_SUB(comm3D,belongs,commx3,stat)
 call MPI_COMM_RANK(commx3,rankx3,stat)
 call MPI_COMM_SIZE(commx3,sizex3,stat)

!Create 2D communicators
 !3.5 create X1-X2 sub-communicator
 belongs(1) = .true.
 belongs(2) = .true.
 belongs(3) = .false.
 call MPI_CART_SUB(comm3D,belongs,commx1x2,stat)
 call MPI_COMM_RANK(commx1x2,rankx1x2,stat)
 call MPI_COMM_SIZE(commx1x2,sizex1x2,stat)

 !3.6 create X2-X3 sub-communicator
 belongs(1) = .false.
 belongs(2) = .true.
 belongs(3) = .true.
 call MPI_CART_SUB(comm3D,belongs,commx2x3,stat)
 call MPI_COMM_RANK(commx2x3,rankx2x3,stat)
 call MPI_COMM_SIZE(commx2x3,sizex2x3,stat)

 !3.7 create X1-X3 sub-communicator
 belongs(1) = .true.
 belongs(2) = .false.
 belongs(3) = .true.
 call MPI_CART_SUB(comm3D,belongs,commx1x3,stat)
 call MPI_COMM_RANK(commx1x3,rankx1x3,stat)
 call MPI_COMM_SIZE(commx1x3,sizex1x3,stat)

 call MPI_BARRIER(comm3d,stat) !wait for all processes to finish 

!Find indices of subdomain 
 call MPE_DECOMP1D(nx,dims(1),coords(1),sx,ex)
  sx=sx+1
  ex=ex+1
  cex=sx+(ex-sx)/2

 call MPE_DECOMP1D(ny,dims(2),coords(2),sy,ey)
  sy=sy+1
  ey=ey+1
 call MPE_DECOMP1D(nz,dims(3),coords(3),sz,ez)
  sz=sz+1
  ez=ez+1

! WHAT DO WE USE THESE FOR KYLE???
 if ( ex.GT.nx-1 ) EU=1
 if ( ey.GT.ny-1 ) EV=1
 if ( ez.GT.nz-1 ) EW=1


!Check the array dimensions
 nxdim=int(dble(nx)/dble(nxprocs)+0.99)+2
 nydim=int(dble(ny)/dble(nyprocs)+0.99)+2
 nzdim=int(dble(nz)/dble(nzprocs)+0.99)+2

 if ((ex-sx+3).gt.nxdim) then
  write(IOUT,'(a13,i3,a7,i4,a12,i4)') 'ERROR: process:',myid,' nxdim=',nxdim,' < ex-sx+3=',ex-sx+3
  stat=1
  goto 1000
 endif
                                                                                                                             
 if ((ey-sy+3).gt.nydim) then
  write(IOUT,'(a13,i3,a7,i4,a12,i4)') 'ERROR: process:',myid,' nydim=',nydim,' < ey-sy+3=',ey-sy+3
  stat=2
  goto 1000
 endif
                                                                                                                             
 if ((ez-sz+3).gt.nzdim) then
  write(IOUT,'(a13,i3,a7,i4,a12,i4)') 'ERROR: process:',myid,' nzdim=',nzdim,' < ez-sz+3=',ez-sz+3
  stat=3
  goto 1000
 endif

 !FOR GHOSTING ALL VARIABLES EXCEPT PRESSURE
 call grid1_type(gtype,sx,ex,sy,ey,sz,ez)

 write(IOUTL,'(a)') "COORDINATES"
 write(IOUTL,130) coords(1),' ',coords(2),' ',coords(3)
 write(IOUTL,'(a)') "LOCAL INDICIES"
 write(IOUTL,140) 'sx=',sx,' ex=',ex,' sy=',sy,' ey=',ey,' sz=',sz,' ez=',ez
 write(IOUTL,'(a)') ""
 write(IOUTL,'(a)') "MPI DOMAIN DECOMPOSITION SETUP COMPLETED"
                                                                                                                             
 stat = 0
 return

 1000 continue
 write(IOUT,'(a)') "MPI DOMAIN DECOMPOSITION FAILED"
 return

 130  FORMAT( 10(1x,i4,a1) )
 140  FORMAT( 10(1x,a4,i5) )

end




subroutine MPE_DECOMP1D(n,numprocs,myid,s,e)
 implicit none

!Passed Variables
 integer :: nlocal
 integer :: deficit
 integer :: n, numprocs, myid, s, e

!------------------------------------------------------------------------
!  From the MPE library
!  This file contains a routine for producing a decomposition of a 1-d 
!  array when given a number of processors.  It may be used in "direct" 
!  product decomposition.  The values returned assume a "global" domain 
!  in [1:n]
!
! Code      : tmgd3
! Called in : mpi_setup
! Calls     : --
!------------------------------------------------------------------------

 nlocal  = n / numprocs
 s       = myid * nlocal + 1
 deficit = mod(n,numprocs)
 s       = s + min(myid,deficit)

 if (myid .lt. deficit) then
  nlocal = nlocal + 1
 endif

 e       = s + nlocal - 1

 if (e .gt. n .or. myid .eq. numprocs-1) e = n

 return
end

subroutine grid1_type(gtype,sxL,exL,syL,eyL,szL,ezL)
 use dd, only: realtype
 implicit none
      
!Passed Variables
 integer,intent(in)        :: sxL,exL,syL,eyL,szL,ezL
 integer,intent(out)       :: gtype(7)

!Local Variables
 integer                   :: i,ierr

!------------------------------------------------------------------------
! Define the 7 derived datatypes needed to communicate the boundary 
! data of (sxL-1:exL+1,syL-1:eyL+1,szL-1:ezL+1) arrays between 'myid' and
! its 26 neighbors.
!
! gtype(l): l=1 -> i=const planes
!           l=2 -> j=const planes
!           l=3 -> k=const planes
!           l=4 -> (i=const,j=const) lines
!           l=5 -> (i=const,k=const) lines
!           l=6 -> (j=const,k=const) lines
!           l=7 -> (i=const,j=const,k=const) corners
!
! Author    : Bernard Bunner (bunner@engin.umich.edu), January 1998
! Calls     : MPI_TYPE_CONTIGUOUS, MPI_TYPE_COMMIT, MPI_TYPE_VECTOR,
!             MPI_TYPE_EXTENT, MPI_TYPE_HVECTOR
!------------------------------------------------------------------------
!Datatype for one 1*1 corner (i=const,j=const,k=const)
    call MPI_TYPE_CONTIGUOUS(1,realtype,gtype(7),ierr)
    call MPI_TYPE_COMMIT(gtype(7),ierr)

!Datatype for one (i=const,j=const) line 
    call MPI_TYPE_VECTOR(ezL-szL+1,1,(exL-sxL+3)*(eyL-syL+3),realtype,&
                            gtype(4),ierr)
    call MPI_TYPE_COMMIT(gtype(4),ierr)

!Datatype for one (i=const,k=const) line
    call MPI_TYPE_VECTOR(eyL-syL+1,1,exL-sxL+3,realtype,gtype(5),ierr)
    call MPI_TYPE_COMMIT(gtype(5),ierr)
!Datatype for one (j=const,k=const) line
    call MPI_TYPE_CONTIGUOUS(exL-sxL+1,realtype,gtype(6),ierr)
    call MPI_TYPE_COMMIT(gtype(6),ierr)

!Datatype for one i=const plane
    call MPI_TYPE_EXTENT(realtype,i,ierr)
    call MPI_TYPE_HVECTOR(ezL-szL+1,1,(exL-sxL+3)*(eyL-syL+3)*i, &
                            gtype(5),gtype(1),ierr)
    call MPI_TYPE_COMMIT(gtype(1),ierr)

!Datatype for one j=const plane
    call MPI_TYPE_VECTOR(ezL-szL+1,exL-sxL+1,(exL-sxL+3)*(eyL-syL+3),&
                          realtype,gtype(2),ierr)
    call MPI_TYPE_COMMIT(gtype(2),ierr)

!Datatype for one k=const plane
    call MPI_TYPE_VECTOR(eyL-syL+1,exL-sxL+1,exL-sxL+3, &
                          realtype,gtype(3),ierr)
    call MPI_TYPE_COMMIT(gtype(3),ierr)

 return
end subroutine grid1_type
