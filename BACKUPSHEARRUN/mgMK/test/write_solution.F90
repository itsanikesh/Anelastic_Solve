subroutine write_solution(var,basename,err1)
!@t
! \textbf{subroutine write\_solution(var,basename,err1)}
!@h
!   Description:
!     Write out a solution from the pressure solver 
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History:
!     Version   Date     Comment 
!     -------   ----     -------  
!     1.0       04/2009  Original code. [Matt de Stadler] 
!@h
!   Comments: 
!     This should be called after mg\_solver when the pressure solution is
!     available. The output file is in binary to save space. There is a
!     text version that can used for smaller files. A file is output for
!     the local solution from each processor.
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
 integer,intent(out)    :: err1
 real(r8),intent(in)    :: var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer  :: s1
 character (len = 50 )  :: ICfile

 err1=0

 ICfile=''
 call concat(ICfile,basename)
#ifdef PARALLEL
 call concati(ICfile,coords(1))
 call concat(ICfile,'_')
 call concati(ICfile,coords(2))
 call concat(ICfile,'_')
 call concati(ICfile,coords(3))
#endif
         
!Direct Access read
! open(310,file=ICfile,form='formatted',status='new',iostat=s1,action='write')
 open(310,file=ICfile,form='unformatted',status='new',iostat=s1,action='write')
  if (s1.NE.0) then
   write(IOUT,'(a23,a15,a7,i4)') "ERROR OPENING FILE: ",Icfile," IOSTAT=",s1
   err1=1
   goto 2000
  endif
   
!  write(310,'(i4)') sx,ex,sy,ey,sz,ez
  write(310) sx,ex,sy,ey,sz,ez
!  write(310,'(f10.5)') var
  write(310) var
 close(310)
   
 err1=max(err1,s1)
 write(IOUT,'(a22,a15,a11)') "WRITE OF BIG RESTART :",ICfile," COMPLETED"
 return
  
 2000 continue
 write(IOUT,'(a22,a15,a11)') "WRITE OF BIG RESTART :",ICfile," FAILED"
 return
end subroutine write_solution
