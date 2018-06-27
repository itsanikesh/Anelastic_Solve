subroutine startup(var,basename,err1)
!@t
! \textbf{subroutine startup(var,basename,err1)}
!@h
!   Description:
!     Load a previous solution from the pressure solver generated
!     by write\_solution.f90
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       04/2009  Original code. [Matt de Stadler] 
!@h
!  Comments:
!    The current domain decomposition must match the decomposition used by 
!    write_solution to generate the files. An error will result if this is
!    not the case. 
!@q

 use ntypes,     only: i4, r8
 use Domain,     only: sx,ex,sy,ey,sz,ez
 use IO,         only: IOUT
#ifdef PARALLEL
 use dd,         only: coords
#endif
 implicit none

!Passed Variables
 character(len=*)       :: basename
 integer,intent(out)     :: err1
 real(r8),intent(out)    :: var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer  :: s1, n_r(1:6),i
 character (len = 300 )  :: ICfile

 ICfile=basename
#ifdef PARALLEL
 call concati(ICfile,coords(1))
 call concat(ICfile,'_')
 call concati(ICfile,coords(2))
 call concat(ICfile,'_')
 call concati(ICfile,coords(3))
#endif

!Direct Access read
! open(310,file=ICfile,form='formatted',status='old',iostat=s1,action='read')
 open(310,file=ICfile,form='unformatted',status='old',iostat=s1,action='read')
  if (s1.NE.0) then
   write(IOUT,'(a23,a,a7,i4)') "ERROR OPENING FILE: ",trim(Icfile)," IOSTAT=",s1
   err1=1
   stop
  endif

!  read(310,'(i2)') n_r
  read(310) n_r
   if (n_r(1).NE.sx)   goto 1000 !Check to make sure dump is correct size
   if (n_r(2).NE.ex)   goto 1000
   if (n_r(3).NE.sy)   goto 1000
   if (n_r(4).NE.ey)   goto 1000
   if (n_r(5).NE.sz)   goto 1000
   if (n_r(6).NE.ez)   goto 1000
!  read(310,'(f10.5)') var
  read(310) var
 close(310)

 write(IOUT,'(a23,a15,a11)') "READ OF INITIAL FIELD: ",ICfile," COMPLETED"
 return

 err1=max(err1,s1)

 1000 continue
 close(310)
 write(IOUT,'(a36,3(1x,i4))') "ERROR: Data file size is [sx:ex,sy:ey,ez:ez]", &
                                      (n_r(i),i=1,6)
 write(IOUT,'(a36,3(1x,i4))') "ERROR: Run  size is [sx:ex,sy:ey,sz:ez]",sx,ex,&
                                  sy,ey,sz,ez

 close(unit=310)
 stop
end subroutine startup

