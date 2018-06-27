module VSBIN 
 integer,parameter                           :: r8=8
 integer                                     :: nstats, ngroups
 integer                                     :: nzp2
 logical,parameter                           :: debug=.false.
 real(8),allocatable,dimension(:,:)          :: STATS
 character(len=25),allocatable,dimension(:)  :: Sname, Gname
 integer,allocatable,dimension(:)            :: group
 real(r8),allocatable,dimension(:)           :: zc
 real(r8)                                    :: time

 contains

 subroutine Vshear_statBIN_read(nstep,statDIR,list)
  implicit none

  !Passed Variables
  integer,intent(in)                          :: nstep
  character(len=*),intent(in)                 :: statDIR
  logical,intent(in)                          :: list
  !Local Variables
  character(len=50)                           :: filen1
  integer                                     :: s1,k,i 
 
  !Statistics file
   write(filen1,'(a,i5.5,a)') trim(statDIR)//'/avg',nstep,'.bin'
   if (debug) write(6,*) filen1 
 
  open( unit=500,file=filen1,status='old',form='unformatted',iostat=s1 )
   read(500) time
   if (debug) write(6,*) time
   read(500) nzp2, nstats, ngroups
   if (debug) write(6,*) nzp2, nstats, ngroups
   allocate( zc(1:nzp2) )
   read(500) zc
   if (debug) write(6,*) zc(1),zc(nzp2)
   allocate( Sname(1:nstats) )
   read(500) Sname
   if (debug) write(6,*) Sname(1),Sname(nstats)
   allocate( Gname(1:ngroups) )
   read(500) Gname
   if (debug) write(6,*) Gname(1),Gname(ngroups)
   allocate( group(1:nstats) ) 
   read(500) group
   if (debug) write(6,*) group(1),group(nstats)
   allocate( STATS(1:nzp2,1:nstats) )
   read(500) STATS
   if (debug) write(6,*) STATS(1,1),STATS(nzp2,nstats)
  close(500)

  if ( list ) then
   write(6,*) " "
   write(6,'(a)') "*********************************************"
   write(6,'(a)') "************AVAILABLE STATISTICS*************"
   write(6,'(a)') "*********************************************"
   write(6,'(a)') "  #   Name                                   "
   do i=1,nstats
    write(6,'(i3,3x,a,3x,a)') i,trim(Sname(i)),trim(Gname( group(i) ))
   enddo
  endif


  return 
 end subroutine Vshear_statBIN_read


 subroutine TKwrite(basename,step,dtw)
  implicit none

  !Passed Variables
  character(len=*),intent(inout)                       :: basename
  integer,intent(in)                                   :: dtw, step
 
  !Local Variables
  character(len=300)   :: filen1, stemp
  integer              :: i,j,n,k,s1
  real(r8)             :: Scoord
  integer,parameter    :: iplane=1
  character(len=1024)  :: tkheader
  integer              :: offset

  if (dtw.EQ.1) Scoord=STATS(nzp2/2,46) !dtheta
  if (dtw.EQ.2) Scoord=STATS(nzp2/2,46) !dOmega
  if (dtw.EQ.0) Scoord=1.d0

  write(stemp,'(i10)') step
  offset=1
  do i=1,10
   if (stemp(i:i).EQ.' ') offset=offset+1
  enddo
  stemp=stemp(offset:10)
 
  write(filen1,'(a)') trim(basename)//'avg'//trim(stemp) 
  if (debug) write(6,*) filen1
 
  open( unit=501,file=filen1,status='new',form='formatted',iostat=s1 )
  
  !WRITE HEADER
  write(501,'(a8,f15.8)') "RTIME = ",time
  
  !CREATE THE GROUP HEADERS
  do j=1,ngroups
   tkheader="GROUP = "//trim(Gname(j))
   do n=1,nstats
    if (group(n).EQ.j) then
      tkheader=trim(tkheader)//' '//trim(Sname(n))
     endif
   enddo
    write(501,'(a)') trim(tkheader)
  enddo
 
  !CREATE THE MAIN HEADER
  tkheader='I J Z S1'
  do n=1,nstats
   tkheader=trim(tkheader)//' '//trim(Sname(n))
  enddo
  write(501,'(a)') trim(tkheader)
 
  do k=1,nzp2
   write(501,135) iplane,' ',k,' ',zc(k)/Scoord,' ',zc(k),' ',(STATS(k,n),n=1,nstats)
  enddo

  close(501)

  120 FORMAT( (A) )
  135 FORMAT( ( 2(I3,a1), 2(E15.8,a1), 150(2X,E15.8E3) ) )

 return
 end subroutine TKwrite

 subroutine VSBINend
  implicit none

  !Local Variables
  integer :: s1
 
  deallocate( zc, stat=s1 )
  deallocate( Sname, stat=s1 )
  deallocate( Gname, stat=s1 )
  deallocate( group, stat=s1 )
  deallocate( STATS, stat=s1 )
  if (s1.NE.0) write(6,*) "ERROR DEALLOCATING VSBINend"

  return
 end subroutine VSBINend

end module VSBIN 
