subroutine ghostALL(var,vtype,stat)
!@t
! \textbf{subroutine ghostALL(var,vtype,stat)}
!@h
!   Description:
!     Updates ghost values for each processor in a parallel code.
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
!     Planes, lines and corners are all updated.
!@q

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez
 use dd,     only: neighbor, bd, comm3d,MPI_STATUS_SIZE,gtype 
 use IO,     only: IOUT
 implicit none
 
!Passed Variables
 real(r8),intent(inout)     :: var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 character(len=*)           :: vtype
 integer,intent(out)        :: stat

!Local Variables
 integer                    :: ireq, req(52), ierr, status1(MPI_STATUS_SIZE,52)
 integer                    :: l, m, n, rt 
 integer                    :: err1,err2

 err1=0
 err2=0

 ! gtype(l): l=1 -> i=const planes
 !           l=2 -> j=const planes
 !           l=3 -> k=const planes
 !           l=4 -> (i=const,j=const) lines
 !           l=5 -> (i=const,k=const) lines
 !           l=6 -> (j=const,k=const) lines
 !           l=7 -> (i=const,j=const,k=const) corners


 ireq=0
 call gxch1pla(sx,ex,sy,ey,sz,ez,var,comm3d,neighbor,      &
                    bd,gtype(1:3),req,ireq,IOUT)

 call MPI_WAITALL(ireq,req,status1,err1)

 ireq = 0 
 call gxch1lin(sx,ex,sy,ey,sz,ez,var,comm3d,neighbor,      &
                    bd,gtype(4:6),req,ireq,IOUT)

 call MPI_WAITALL(ireq,req,status1,err1)

 ireq = 0
 call gxch1cor(sx,ex,sy,ey,sz,ez,var,comm3d,neighbor,      &
                    bd,gtype(7),req,ireq,IOUT)

 call MPI_WAITALL(ireq,req,status1,err1)

 !normal exit
 stat=max(err1,err2)
 return

 !Unclean
 9999 continue
 write(IOUT,'(a,2(1x,i4))') "ERROR (ghost.f90) for var: "//trim(vtype)," err1/err2: ",err1,err2
 stat=max(err1,err2)
 return

end subroutine ghostALL

subroutine gxch1pla(sx,ex,sy,ey,sz,ez,a,comm3d,neighbor,bd,planetype,req&
                    ,ireq,IOUT)
!@t
! \textbf{subroutine gxch1pla(sx,ex,sy,ey,sz,ez,a,comm3d,neighbor,bd,planetype,req,ireq,IOUT)}
!@h
!   Description:
!     Subroutine to exchange planes of thickness 1 of boundary data between
!     'myid' and the 6 processes in the directions i+, i-, j+, j-, k+, k-. 
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
!     Based on mgd3 by Bernard Bunner (bunner@engin.umich.edu), January 1998
!     Can be used to communicate scalar (p, r,...) as well as vector 
!     quantities ((u,v,w), (ut,vt,wt)). The appropriate datatypes have 
!     been defined in the 'type_mpi' subroutine.
!
! plane i
!       |     7     |
!      ---------------                k
!       |           |               ^ 
!       |           |               |
!      5|    myid   |1              |
!       |           |              ------>
!       |           |               |     j
!      ---------------
!       |     3     |
!
! plane i-1                  plane i+1
!       |           |              |           | 
!      ---------------            --------------- 
!       |           |              |           |
!       |           |              |           |
!       |     9     |              |    18     |
!       |           |              |           |
!       |           |              |           |
!      ---------------            ---------------
!       |           |              |           |
!@q
! Code      : tmgd3, test program for 3D parallel multigrid solver
! Author    : Bernard Bunner (bunner@engin.umich.edu), January 1998
! Called in : setdens,gbackin, gadvect, gdrbsor, gprbsor, gveloc, 
!             mgdrelax, mgdrestr, mgdrtrsf, 
! Calls     : MPI_ISEND, MPI_IRECV (non-blocking version)
!             MPI_SENDRECV (blocking version)

 use ntypes, only: r8
 implicit none

!Passed Variables
 integer,intent(in)        :: sx,ex,sy,ey,sz,ez,comm3d
 integer,intent(in)        :: neighbor(26),bd(26),planetype(3)
 integer,intent(inout)     :: ireq
 integer,intent(inout)     :: req(52)
 integer,intent(in)        :: IOUT
 real(r8),intent(inout)    :: a(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variable
 integer                   :: ierr
 integer                   :: nc

!Planes i=constant
 !Send to 18
      if (bd(18).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(ex,sy,sz),1,planetype(1),neighbor(18),         &
                      0,comm3d,req(ireq),ierr)
      end if
!Receive from 9

      if (bd(9).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx-1,sy,sz),1,planetype(1),neighbor(9),        &
                      0,comm3d,req(ireq),ierr)
      end if

!Send to 9

      if (bd(9).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,sy,sz),1,planetype(1),neighbor(9),          &
                      1,comm3d,req(ireq),ierr)
      end if
!Receive from 18

      if (bd(18).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(ex+1,sy,sz),1,planetype(1),neighbor(18),       &
                      1,comm3d,req(ireq),ierr)
      end if

!Planes j=constant
!Send to 1

      if (bd(1).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,ey,sz),1,planetype(2),neighbor(1),          &
                      2,comm3d,req(ireq),ierr)
      end if

!Receive from 5

      if (bd(5).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx,sy-1,sz),1,planetype(2),neighbor(5),        &
                      2,comm3d,req(ireq),ierr)
      end if

!Send to 5

      if (bd(5).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,sy,sz),1,planetype(2),neighbor(5),          &
                      3,comm3d,req(ireq),ierr)
      end if

!Receive from 1

      if (bd(1).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx,ey+1,sz),1,planetype(2),neighbor(1),        &
                      3,comm3d,req(ireq),ierr)
      end if

!Planes k=constant
!Send to 7

      if (bd(7).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,sy,ez),1,planetype(3),neighbor(7),          &
                      4,comm3d,req(ireq),ierr)
      end if

!Receive from 3

      if (bd(3).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx,sy,sz-1),1,planetype(3),neighbor(3),        &
                      4,comm3d,req(ireq),ierr)
      end if

!Send to 3

      if (bd(3).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,sy,sz),1,planetype(3),neighbor(3),          &
                      5,comm3d,req(ireq),ierr)
      end if

!Receive from 7

      if (bd(7).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx,sy,ez+1),1,planetype(3),neighbor(7),        &
                      5,comm3d,req(ireq),ierr)
      end if

 return
end
subroutine gxch1lin(sx,ex,sy,ey,sz,ez,a,comm3d,neighbor,bd,linetype,&
                    req,ireq,IOUT)
!@t
! \textbf{subroutine gxch1lin(sx,ex,sy,ey,sz,ez,a,comm3d,neighbor,bd,linetype,req,ireq,IOUT)}
!@h
!   Description:
!     Subroutine to exchange lines of thickness 1 of boundary data between
!     'myid' and the 12 processes in the directions (i+,j+), (i+,j-),
!     (i-,j+), (i-,j-), (i+,k+), (i+,k-), (i-,k+), (i-,k-), (j+,k+),
!     (j+,k-), (j-,k+), (j-,k-).
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
!     Based on mgd3 by Bernard Bunner (bunner@engin.umich.edu), January 1998
!     Can be used to communicate scalar (p, r,...) as well as vector
!     quantities ((u,v,w), (ut,vt,wt)). The appropriate datatypes have been
!     defined in the 'type_mpi' subroutine.
!
! plane i
!      6|           |8
!      ---------------                k
!       |           |               ^ 
!       |           |               |
!       |    myid   |               |
!       |           |              ------>
!       |           |               |     j
!      ---------------
!      4|           |2
!
! plane i-1                  plane i+1
!       |    16     |              |     25    | 
!      ---------------            --------------- 
!       |           |              |           |
!       |           |              |           |
!     14|           |10          23|           |19
!       |           |              |           |
!       |           |              |           |
!      ---------------            ---------------
!       |    12     |              |     21    |
!
!@q

! Code      : tmgd3, test program for 3D parallel multigrid solver
! Author    : Bernard Bunner (bunner@engin.umich.edu), January 1998
! Called in : setdens, gbackin, gdrbsor, gprbsor, gveloc, mgdrestr, 
!             mgdsolver
! Calls     : MPI_ISEND, MPI_IRECV (non-blocking version)
!             MPI_SENDRECV (blocking version)

 use ntypes, only: r8
 implicit none

!Passed Variables
 integer,intent(in)        :: sx,ex,sy,ey,sz,ez,comm3d
 integer,intent(in)        :: neighbor(26),bd(26),linetype(3),IOUT
 integer,intent(out)       :: req(52)
 integer,intent(inout)     :: ireq
 real(r8),intent(inout)    :: a(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer                   :: ierr, nc

!--------------------------non-blocking----------------------------------
!Lines (i=constant,j=constant)
 
!Send to 19

      if (bd(19).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(ex,ey,sz),1,linetype(1),neighbor(19),          &
                      6,comm3d,req(ireq),ierr)

      end if

!Receive from 14

      if (bd(14).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx-1,sy-1,sz),1,linetype(1),neighbor(14),      &
                      6,comm3d,req(ireq),ierr)
      end if

!Send to 23

      if (bd(23).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(ex,sy,sz),1,linetype(1),neighbor(23),          &
                      7,comm3d,req(ireq),ierr)
      end if

!Receive from 10

      if (bd(10).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx-1,ey+1,sz),1,linetype(1),neighbor(10),      &
                      7,comm3d,req(ireq),ierr)
      end if

!Send to 14

      if (bd(14).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,sy,sz),1,linetype(1),neighbor(14),          &
                      8,comm3d,req(ireq),ierr)
      end if

!Receive from 19

      if (bd(19).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(ex+1,ey+1,sz),1,linetype(1),neighbor(19),      &
                      8,comm3d,req(ireq),ierr)
      end if

!Send to 10

      if (bd(10).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,ey,sz),1,linetype(1),neighbor(10),          &
                      9,comm3d,req(ireq),ierr)
      end if

!Receive from 23

      if (bd(23).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(ex+1,sy-1,sz),1,linetype(1),neighbor(23),      &
                      9,comm3d,req(ireq),ierr)
      end if

!Lines (i=constant,k=constant)

!Send to 25

      if (bd(25).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(ex,sy,ez),1,linetype(2),neighbor(25),          &
                      10,comm3d,req(ireq),ierr)
      end if

!Receive from 12

      if (bd(12).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx-1,sy,sz-1),1,linetype(2),neighbor(12),      &
                      10,comm3d,req(ireq),ierr)
      end if

!Send to 21

      if (bd(21).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(ex,sy,sz),1,linetype(2),neighbor(21),          &
                      11,comm3d,req(ireq),ierr)
      end if

!Receive from 16

      if (bd(16).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx-1,sy,ez+1),1,linetype(2),neighbor(16),      &
                      11,comm3d,req(ireq),ierr)
      end if

!Send to 12

      if (bd(12).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,sy,sz),1,linetype(2),neighbor(12),          &
                      12,comm3d,req(ireq),ierr)
      end if

!Receive from 25

      if (bd(25).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(ex+1,sy,ez+1),1,linetype(2),neighbor(25),      &
                      12,comm3d,req(ireq),ierr)
      end if

!Send to 16

      if (bd(16).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,sy,ez),1,linetype(2),neighbor(16),          &
                      13,comm3d,req(ireq),ierr)
      end if

!Receive from 21

      if (bd(21).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(ex+1,sy,sz-1),1,linetype(2),neighbor(21),      &
                      13,comm3d,req(ireq),ierr)
      end if

!Lines (j=constant,k=constant)

!Send to 8
      if (bd(8).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,ey,ez),1,linetype(3),neighbor(8),           &
                      14,comm3d,req(ireq),ierr)
      end if

!Receive from 4

      if (bd(4).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx,sy-1,sz-1),1,linetype(3),neighbor(4),       &
                      14,comm3d,req(ireq),ierr)
      end if

!Send to 2

      if (bd(2).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,ey,sz),1,linetype(3),neighbor(2),           &
                      15,comm3d,req(ireq),ierr)
      end if

!Receive from 6

      if (bd(6).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx,sy-1,ez+1),1,linetype(3),neighbor(6),       &
                      15,comm3d,req(ireq),ierr)
      end if

!Send to 4

      if (bd(4).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,sy,sz),1,linetype(3),neighbor(4),           &
                      16,comm3d,req(ireq),ierr)
      end if

!Receive from 8

      if (bd(8).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx,ey+1,ez+1),1,linetype(3),neighbor(8),       &
                      16,comm3d,req(ireq),ierr)
      end if

!Send to 6

      if (bd(6).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,sy,ez),1,linetype(3),neighbor(6),           &
                      17,comm3d,req(ireq),ierr)
      end if

!Receive from 2

      if (bd(2).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx,ey+1,sz-1),1,linetype(3),neighbor(2),       &
                      17,comm3d,req(ireq),ierr)
      end if

 return
end subroutine
subroutine gxch1cor(sx,ex,sy,ey,sz,ez,a,comm3d,neighbor,bd,cornertype,&
                    req,ireq,IOUT)
!@t
! \textbf{subroutine gxch1cor(sx,ex,sy,ey,sz,ez,a,comm3d,neighbor,bd,cornertype,req,ireq,IOUT)}
!@h
!   Description:
!     Subroutine to exchange 1*1*1 corners (i.e. points) of boundary data 
!     between 'myid' and the 8 processes (i+,j+,k+), (i+,j+,k-), (i+,j-,k+),
!    (i+,j-,k-), (i-,j+,k+), (i-,j+,k-), (i-,j-,k+), (i-,j-,k-). 
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
!     Based on mgd3 by Bernard Bunner (bunner@engin.umich.edu), January 1998
!     Can be used to communicate scalar (p, r,...) as well as vector
!     quantities ((u,v,w), (ut,vt,wt)). The appropriate datatypes have been
!     defined in the 'type_mpi' subroutine.
!
! plane i
!       |           | 
!      ---------------                k
!       |           |               ^ 
!       |           |               |
!       |    myid   |               |
!       |           |              ------>
!       |           |               |     j
!      ---------------
!       |           | 
!
! plane i-1                  plane i+1
!     15|           |17          24|           |26 
!      ---------------            --------------- 
!       |           |              |           |
!       |           |              |           |
!       |           |              |           |
!       |           |              |           |
!       |           |              |           |
!      ---------------            ---------------
!     13|           |11          22|           |20
!@q
! Code      : tmgd3, test program for 3D parallel multigrid solver
! Author    : Bernard Bunner (bunner@engin.umich.edu), January 1998
! Called in : gbackin, setdens, gdrbsor, gprbsor, gveloc, mgdrestr, 
!             mgdsolver
! Calls     : MPI_ISEND, MPI_IRECV (non-blocking version)
!             MPI_SENDRECV (blocking version)

 use ntypes, only: r8
 implicit none

!Passed Variables
 integer,intent(in)        :: sx,ex,sy,ey,sz,ez,comm3d
 integer,intent(in)        :: neighbor(26),bd(26),cornertype,IOUT
 integer,intent(out)       :: req(52)
 integer,intent(inout)     :: ireq
 real(r8),intent(inout)    :: a(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer                   :: ierr, nc

!--------------------------non-blocking----------------------------------
!Send to 26

      if (bd(26).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(ex,ey,ez),1,cornertype,neighbor(26),           &
                      18,comm3d,req(ireq),ierr)
      end if

!Receive from 13

      if (bd(13).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx-1,sy-1,sz-1),1,cornertype,neighbor(13),     &
                      18,comm3d,req(ireq),ierr)
      end if

!Send to 20

      if (bd(20).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(ex,ey,sz),1,cornertype,neighbor(20),           &
                      19,comm3d,req(ireq),ierr)
      end if

!Receive from 15

      if (bd(15).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx-1,sy-1,ez+1),1,cornertype,neighbor(15),     &
                      19,comm3d,req(ireq),ierr)
      end if

!Send to 24

      if (bd(24).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(ex,sy,ez),1,cornertype,neighbor(24),           &
                      20,comm3d,req(ireq),ierr)
      end if

!Receive from 11

      if (bd(11).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx-1,ey+1,sz-1),1,cornertype,neighbor(11),     &
                      20,comm3d,req(ireq),ierr)
      end if

!Send to 22

      if (bd(22).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(ex,sy,sz),1,cornertype,neighbor(22),           &
                      21,comm3d,req(ireq),ierr)
      end if

!Receive from 17

      if (bd(17).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sx-1,ey+1,ez+1),1,cornertype,neighbor(17),     &
                      21,comm3d,req(ireq),ierr)
      end if

!Send to 13

      if (bd(13).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,sy,sz),1,cornertype,neighbor(13),           &
                      22,comm3d,req(ireq),ierr)
      end if

!Receive from 26

      if (bd(26).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(ex+1,ey+1,ez+1),1,cornertype,neighbor(26),     &
                      22,comm3d,req(ireq),ierr)
      end if

!Send to 15

      if (bd(15).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,sy,ez),1,cornertype,neighbor(15),           &
                      23,comm3d,req(ireq),ierr)
      end if

!Receive from 20

      if (bd(20).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(ex+1,ey+1,sz-1),1,cornertype,neighbor(20),     &
                      23,comm3d,req(ireq),ierr)
      end if

!Send to 11

      if (bd(11).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,ey,sz),1,cornertype,neighbor(11),           &
                      24,comm3d,req(ireq),ierr)
      end if

!Receive from 24

      if (bd(24).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(ex+1,sy-1,ez+1),1,cornertype,neighbor(24),     &
                      24,comm3d,req(ireq),ierr)
      end if

!Send to 17

      if (bd(17).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sx,ey,ez),1,cornertype,neighbor(17),           &
                      25,comm3d,req(ireq),ierr)
      end if

!Receive from 22

      if (bd(22).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(ex+1,sy-1,sz-1),1,cornertype,neighbor(22),     &
                      25,comm3d,req(ireq),ierr)
      end if

 return
end subroutine 
