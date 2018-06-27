#ifdef PARALLEL
subroutine gxch1plaMG(lvl,a,req,ireq)
!@t
! \textbf{subroutine gxch1plaMG(lvl,a,req,ireq)}
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
! Calls     : MPI_ISEND, MPI_IRECV (non-blocking version)

  use ntypes,  only: r8,i4
  use mgVars, only: neighborMG, bdMG, kdatatype, commMG
  use mgVars, only: sxk,exk,syk,eyk,szk,ezk
 implicit none

!Passed Variables
 real(r8),intent(inout)     :: a(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                    eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 
 integer(i4),intent(in)    :: lvl
 integer(i4),intent(inout) :: ireq
 integer(i4),intent(inout) :: req(52)

!Local Variable
 integer(i4)               :: planetype(1:3)
 integer(i4)               :: ierr, sxL,exL,syL,eyL,szL,ezL

 planetype(1:3) = kdatatype(1:3,lvl)
 sxL=sxk(lvl)
 syL=syk(lvl)
 szL=szk(lvl)
 exL=exk(lvl)
 eyL=eyk(lvl)
 ezL=ezk(lvl)

!Planes i=constant
 !Send to 18
      if (bdMG(18).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(exL,syL,szL),1,planetype(1),neighborMG(18),         &
                      0,commMG,req(ireq),ierr)
      end if
!Receive from 9

      if (bdMG(9).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL-1,syL,szL),1,planetype(1),neighborMG(9),        &
                      0,commMG,req(ireq),ierr)
      end if

!Send to 9

      if (bdMG(9).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,syL,szL),1,planetype(1),neighborMG(9),          &
                      1,commMG,req(ireq),ierr)
      end if
!Receive from 18

      if (bdMG(18).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(exL+1,syL,szL),1,planetype(1),neighborMG(18),       &
                      1,commMG,req(ireq),ierr)
      end if

!Planes j=constant
!Send to 1

      if (bdMG(1).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,eyL,szL),1,planetype(2),neighborMG(1),          &
                      2,commMG,req(ireq),ierr)
      end if

!Receive from 5

      if (bdMG(5).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL,syL-1,szL),1,planetype(2),neighborMG(5),        &
                      2,commMG,req(ireq),ierr)
      end if

!Send to 5

      if (bdMG(5).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,syL,szL),1,planetype(2),neighborMG(5),          &
                      3,commMG,req(ireq),ierr)
      end if

!Receive from 1

      if (bdMG(1).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL,eyL+1,szL),1,planetype(2),neighborMG(1),        &
                      3,commMG,req(ireq),ierr)
      end if

!Planes k=constant
!Send to 7

      if (bdMG(7).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,syL,ezL),1,planetype(3),neighborMG(7),          &
                      4,commMG,req(ireq),ierr)
      end if

!Receive from 3

      if (bdMG(3).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL,syL,szL-1),1,planetype(3),neighborMG(3),        &
                      4,commMG,req(ireq),ierr)
      end if

!Send to 3

      if (bdMG(3).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,syL,szL),1,planetype(3),neighborMG(3),          &
                      5,commMG,req(ireq),ierr)
      end if

!Receive from 7

      if (bdMG(7).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL,syL,ezL+1),1,planetype(3),neighborMG(7),        &
                      5,commMG,req(ireq),ierr)
      end if
 return
end


subroutine gxch1linMG(lvl,a,req,ireq)
!@t
! \textbf{subroutine gxch1linMG(lvl,a,req,ireq)}
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
! Author    : Bernard Bunner (bunner@engin.umich.edu), January 1998
! Called in : mg_restr, mg_solver
!             
! Calls     : MPI_ISEND, MPI_IRECV (non-blocking version)

  use ntypes,  only: r8,i4
  use mgVars, only: neighborMG, bdMG, kdatatype, commMG
  use mgVars, only: sxk,exk,syk,eyk,szk,ezk
 implicit none

!Passed Variables
 real(r8),intent(inout)     :: a(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                    eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)

 integer(i4),intent(in)    :: lvl
 integer(i4),intent(inout) :: ireq
 integer(i4),intent(inout)   :: req(52)

!Local Variable
 integer(i4)               :: linetype(1:3)
 integer(i4)               :: ierr, sxL,exL,syL,eyL,szL,ezL

 linetype(1:3)=kdatatype(4:6,lvl)
 sxL=sxk(lvl)
 syL=syk(lvl)
 szL=szk(lvl)
 exL=exk(lvl)
 eyL=eyk(lvl)
 ezL=ezk(lvl)



!--------------------------non-blocking----------------------------------
!Lines (i=constant,j=constant)
 
!Send to 19

      if (bdMG(19).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(exL,eyL,szL),1,linetype(1),neighborMG(19),          &
                      6,commMG,req(ireq),ierr)

      end if

!Receive from 14

      if (bdMG(14).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL-1,syL-1,szL),1,linetype(1),neighborMG(14),      &
                      6,commMG,req(ireq),ierr)
      end if

!Send to 23

      if (bdMG(23).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(exL,syL,szL),1,linetype(1),neighborMG(23),          &
                      7,commMG,req(ireq),ierr)
      end if

!Receive from 10

      if (bdMG(10).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL-1,eyL+1,szL),1,linetype(1),neighborMG(10),      &
                      7,commMG,req(ireq),ierr)
      end if

!Send to 14

      if (bdMG(14).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,syL,szL),1,linetype(1),neighborMG(14),          &
                      8,commMG,req(ireq),ierr)
      end if

!Receive from 19

      if (bdMG(19).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(exL+1,eyL+1,szL),1,linetype(1),neighborMG(19),      &
                      8,commMG,req(ireq),ierr)
      end if

!Send to 10

      if (bdMG(10).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,eyL,szL),1,linetype(1),neighborMG(10),          &
                      9,commMG,req(ireq),ierr)
      end if

!Receive from 23

      if (bdMG(23).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(exL+1,syL-1,szL),1,linetype(1),neighborMG(23),      &
                      9,commMG,req(ireq),ierr)
      end if

!Lines (i=constant,k=constant)

!Send to 25

      if (bdMG(25).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(exL,syL,ezL),1,linetype(2),neighborMG(25),          &
                      10,commMG,req(ireq),ierr)
      end if

!Receive from 12

      if (bdMG(12).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL-1,syL,szL-1),1,linetype(2),neighborMG(12),      &
                      10,commMG,req(ireq),ierr)
      end if

!Send to 21

      if (bdMG(21).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(exL,syL,szL),1,linetype(2),neighborMG(21),          &
                      11,commMG,req(ireq),ierr)
      end if

!Receive from 16

      if (bdMG(16).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL-1,syL,ezL+1),1,linetype(2),neighborMG(16),      &
                      11,commMG,req(ireq),ierr)
      end if

!Send to 12

      if (bdMG(12).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,syL,szL),1,linetype(2),neighborMG(12),          &
                      12,commMG,req(ireq),ierr)
      end if

!Receive from 25

      if (bdMG(25).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(exL+1,syL,ezL+1),1,linetype(2),neighborMG(25),      &
                      12,commMG,req(ireq),ierr)
      end if

!Send to 16

      if (bdMG(16).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,syL,ezL),1,linetype(2),neighborMG(16),          &
                      13,commMG,req(ireq),ierr)
      end if

!Receive from 21

      if (bdMG(21).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(exL+1,syL,szL-1),1,linetype(2),neighborMG(21),      &
                      13,commMG,req(ireq),ierr)
      end if

!Lines (j=constant,k=constant)

!Send to 8
      if (bdMG(8).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,eyL,ezL),1,linetype(3),neighborMG(8),           &
                      14,commMG,req(ireq),ierr)
      end if

!Receive from 4

      if (bdMG(4).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL,syL-1,szL-1),1,linetype(3),neighborMG(4),       &
                      14,commMG,req(ireq),ierr)
      end if

!Send to 2

      if (bdMG(2).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,eyL,szL),1,linetype(3),neighborMG(2),           &
                      15,commMG,req(ireq),ierr)
      end if

!Receive from 6

      if (bdMG(6).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL,syL-1,ezL+1),1,linetype(3),neighborMG(6),       &
                      15,commMG,req(ireq),ierr)
      end if

!Send to 4

      if (bdMG(4).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,syL,szL),1,linetype(3),neighborMG(4),           &
                      16,commMG,req(ireq),ierr)
      end if

!Receive from 8

      if (bdMG(8).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL,eyL+1,ezL+1),1,linetype(3),neighborMG(8),       &
                      16,commMG,req(ireq),ierr)
      end if

!Send to 6

      if (bdMG(6).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,syL,ezL),1,linetype(3),neighborMG(6),           &
                      17,commMG,req(ireq),ierr)
      end if

!Receive from 2

      if (bdMG(2).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL,eyL+1,szL-1),1,linetype(3),neighborMG(2),       &
                      17,commMG,req(ireq),ierr)
      end if

 return
end subroutine

subroutine gxch1corMG(lvl,a,req,ireq)
!@t
! \textbf{subroutine gxch1corMG(lvl,a,req,ireq)}
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
! Author    : Bernard Bunner (bunner@engin.umich.edu), January 1998
! Called in : mgdrestr, mgdsolver
! Calls     : MPI_ISEND, MPI_IRECV (non-blocking version)


  use ntypes,  only: r8,i4
  use mgVars, only: neighborMG, bdMG, kdatatype, commMG
  use mgVars, only: sxk,exk,syk,eyk,szk,ezk
  implicit none

!Passed Variables
 real(r8),intent(inout)     :: a(sxk(lvl)-1:exk(lvl)+1,syk(lvl)-1:&
                                    eyk(lvl)+1,szk(lvl)-1:ezk(lvl)+1)
 
 integer(i4),intent(in)    :: lvl
 integer(i4),intent(inout) :: ireq
 integer(i4),intent(inout)   :: req(52)

!Local Variable
 integer(i4)               :: ierr, sxL,exL,syL,eyL,szL,ezL
 integer(i4)               :: cornertype

 cornertype=kdatatype(7,lvl)
 sxL=sxk(lvl)
 syL=syk(lvl)
 szL=szk(lvl)
 exL=exk(lvl)
 eyL=eyk(lvl)
 ezL=ezk(lvl)

!--------------------------non-blocking----------------------------------
!Send to 26

      if (bdMG(26).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(exL,eyL,ezL),1,cornertype,neighborMG(26),           &
                      18,commMG,req(ireq),ierr)
      end if

!Receive from 13

      if (bdMG(13).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL-1,syL-1,szL-1),1,cornertype,neighborMG(13),     &
                      18,commMG,req(ireq),ierr)
      end if

!Send to 20

      if (bdMG(20).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(exL,eyL,szL),1,cornertype,neighborMG(20),           &
                      19,commMG,req(ireq),ierr)
      end if

!Receive from 15

      if (bdMG(15).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL-1,syL-1,ezL+1),1,cornertype,neighborMG(15),     &
                      19,commMG,req(ireq),ierr)
      end if

!Send to 24

      if (bdMG(24).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(exL,syL,ezL),1,cornertype,neighborMG(24),           &
                      20,commMG,req(ireq),ierr)
      end if

!Receive from 11

      if (bdMG(11).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL-1,eyL+1,szL-1),1,cornertype,neighborMG(11),     &
                      20,commMG,req(ireq),ierr)
      end if

!Send to 22

      if (bdMG(22).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(exL,syL,szL),1,cornertype,neighborMG(22),           &
                      21,commMG,req(ireq),ierr)
      end if

!Receive from 17

      if (bdMG(17).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(sxL-1,eyL+1,ezL+1),1,cornertype,neighborMG(17),     &
                      21,commMG,req(ireq),ierr)
      end if

!Send to 13

      if (bdMG(13).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,syL,szL),1,cornertype,neighborMG(13),           &
                      22,commMG,req(ireq),ierr)
      end if

!Receive from 26

      if (bdMG(26).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(exL+1,eyL+1,ezL+1),1,cornertype,neighborMG(26),     &
                      22,commMG,req(ireq),ierr)
      end if

!Send to 15

      if (bdMG(15).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,syL,ezL),1,cornertype,neighborMG(15),           &
                      23,commMG,req(ireq),ierr)
      end if

!Receive from 20

      if (bdMG(20).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(exL+1,eyL+1,szL-1),1,cornertype,neighborMG(20),     &
                      23,commMG,req(ireq),ierr)
      end if

!Send to 11

      if (bdMG(11).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,eyL,szL),1,cornertype,neighborMG(11),           &
                      24,commMG,req(ireq),ierr)
      end if

!Receive from 24

      if (bdMG(24).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(exL+1,syL-1,ezL+1),1,cornertype,neighborMG(24),     &
                      24,commMG,req(ireq),ierr)
      end if

!Send to 17

      if (bdMG(17).eq.0) then
        ireq=ireq+1
        call MPI_ISEND(a(sxL,eyL,ezL),1,cornertype,neighborMG(17),           &
                      25,commMG,req(ireq),ierr)
      end if

!Receive from 22

      if (bdMG(22).eq.0) then
        ireq=ireq+1
        call MPI_IRECV(a(exL+1,syL-1,szL-1),1,cornertype,neighborMG(22),     &
                      25,commMG,req(ireq),ierr)
      end if


 return
end subroutine 



subroutine grid1_typeMG(gtype,sxL,exL,syL,eyL,szL,ezL)
!@t
! \textbf{subroutine grid1\_typeMG(gtype,sxL,exL,syL,eyL,szL,ezL)}
!@h
!   Description:
!     Define the 7 derived datatypes needed to communicate the boundary 
!     data of (sxL-1:exL+1,syL-1:eyL+1,szL-1:ezL+1) arrays between 'myid'
!     and its 26 neighborMGs.
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
!------------------------------------------------------------------------
!     gtype(l): l=1 -> i=const planes
!               l=2 -> j=const planes
!               l=3 -> k=const planes
!               l=4 -> (i=const,j=const) lines
!               l=5 -> (i=const,k=const) lines
!               l=6 -> (j=const,k=const) lines
!               l=7 -> (i=const,j=const,k=const) corners
!
!     Author    : Bernard Bunner (bunner@engin.umich.edu), January 1998
!     Calls     : MPI_TYPE_CONTIGUOUS, MPI_TYPE_COMMIT, MPI_TYPE_VECTOR,
!                 MPI_TYPE_EXTENT, MPI_TYPE_HVECTOR
!------------------------------------------------------------------------
!@q

 use ntypes, only: i4, r8
 use mgVars, only: MPIrealtype
 implicit none
      
!Passed Variables
 integer(i4),intent(in)        :: sxL,exL,syL,eyL,szL,ezL
 integer(i4),intent(out)       :: gtype(1:7)

!Local Variables
 integer(i4)                   :: i,ierr

!Datatype for one 1*1 corner (i=const,j=const,k=const)
    call MPI_TYPE_CONTIGUOUS(1,MPIrealtype,gtype(7),ierr)
    call MPI_TYPE_COMMIT(gtype(7),ierr)

!Datatype for one (i=const,j=const) line 
    call MPI_TYPE_VECTOR(ezL-szL+1,1,(exL-sxL+3)*(eyL-syL+3),MPIrealtype,&
                            gtype(4),ierr)
    call MPI_TYPE_COMMIT(gtype(4),ierr)

!Datatype for one (i=const,k=const) line
    call MPI_TYPE_VECTOR(eyL-syL+1,1,exL-sxL+3,MPIrealtype,gtype(5),ierr)
    call MPI_TYPE_COMMIT(gtype(5),ierr)
!Datatype for one (j=const,k=const) line
    call MPI_TYPE_CONTIGUOUS(exL-sxL+1,MPIrealtype,gtype(6),ierr)
    call MPI_TYPE_COMMIT(gtype(6),ierr)

!Datatype for one i=const plane
    call MPI_TYPE_EXTENT(MPIrealtype,i,ierr)
    call MPI_TYPE_HVECTOR(ezL-szL+1,1,(exL-sxL+3)*(eyL-syL+3)*i, &
                            gtype(5),gtype(1),ierr)
    call MPI_TYPE_COMMIT(gtype(1),ierr)

!Datatype for one j=const plane
    call MPI_TYPE_VECTOR(ezL-szL+1,exL-sxL+1,(exL-sxL+3)*(eyL-syL+3),&
                          MPIrealtype,gtype(2),ierr)
    call MPI_TYPE_COMMIT(gtype(2),ierr)

!Datatype for one k=const plane
    call MPI_TYPE_VECTOR(eyL-syL+1,exL-sxL+1,exL-sxL+3, &
                          MPIrealtype,gtype(3),ierr)
    call MPI_TYPE_COMMIT(gtype(3),ierr)

 return
end subroutine grid1_typeMG
#else
! serial version do nothing
subroutine empty
implicit none
return
end subroutine empty
#endif

