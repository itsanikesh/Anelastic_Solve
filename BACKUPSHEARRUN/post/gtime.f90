program GetTimes
 !This program writes a binary Tecplot or VTK file in single precision from a plane of data.
 !Must be linked to tecio.a
                                                                                                                             
 !Modification History
 !-----------------------------------------------------------------------
 ! 07/24/2008 - Original Version Kyle A. Brucker
 !------------------------------------------------------------------------
                                                                                                                             
 implicit none

 !REAL TYPES
 integer,parameter :: r8 = selected_real_kind(8)
 integer,parameter :: r4 = 4 

 !STATISTICS OUTPUT
 integer                                    :: nstats, ngroups
 integer                                    :: nstep
 integer                                    :: nyp2, nzp2
 logical                                    :: single
 real(r8),allocatable,dimension(:,:,:)      :: statsDP
 real(r4),allocatable,dimension(:,:,:)      :: statsSP
 integer                                    :: nstart, nend, nskip
 integer                                    :: stat
 logical                                    :: List

 !GRID
 real(r8),allocatable,dimension(:)          :: yc,zc, ye, ze
                                                                                                                             
 !TECPLOT
 real(r4),allocatable,dimension(:,:)        :: tecvar
 real(r4),allocatable,dimension(:,:)        :: ytec,ztec
 logical                                    :: tecplot_output
 character(len=150)                         :: ss1, ss2, ss3

 !VTK
 real(r4),allocatable,dimension(:,:)        :: vtkvar
 real(r4),allocatable,dimension(:)          :: yvtk,zvtk
 logical                                    :: paraview_output

 !FILENAMES
 character(len=80)                          :: InFileName, OutFileName 
 character(len=80)                          :: dataDIR, outputDIR, timefile
 character(len=80)                          :: basename
                                                                                                                             
 !OTHER STRINGS
 character(len=25)                          :: ss
 character(len=25)                          :: TITLE
 character(len=100)                         :: stemp
 integer                                    :: l1,l2,l3
 logical                                    :: slogical
 character(len=25),allocatable,dimension(:) :: Sname, Gname
 integer,allocatable,dimension(:)           :: group

 !LOOPING VARIABLES
 integer                                    :: i,j,k,n

 !PARAMETERS
 real(8)                                    :: time, delt, g, rho_0, Re, Pr 

 !STATUS VARIABLES
 integer                                    :: s1
 
 !DEBUG
 logical,parameter                          :: debug=.false.
! logical,parameter                          :: debug=.true.

 write(6,'(a)') 'Data Directory:'
 read(5,*) dataDir
  if ( debug ) write(6,*) dataDir

 write(6,'(a)') 'TimeFile:'
 read(5,*) timeFile 
  if ( debug ) write(6,*) TimeFile 

 write(6,'(a)') 'Base Name:'
 read(5,*) BaseName
  if ( debug ) write(6,*) BaseName

 write(6,'(a)') 'Start:'
 read(5,*) nstart
  if ( debug ) write(6,*) nstart

 write(6,'(a)') 'End:'
 read(5,*) nend
  if ( debug ) write(6,*) nend

 write(6,'(a)') 'Stride:'
 read(5,*) nskip
  if ( debug ) write(6,*) nskip

 do n=nstart,nend,nskip
  write(InFileName,'(a,a,i5.5)') trim(dataDIR),trim(basename),n
  if ( debug ) write(6,*) InFileName
  open(unit=500,file=InFileName,status='old',form='unformatted',iostat=s1,action='read')

  read(500) nstep
  read(500) time,delt,Re,Pr,g,rho_0
  close(500)
  open(unit=10,file=timefile,status='unknown',position='append',form='formatted')
  write(10,*) nstep,time
  close(10)

 enddo !LOOP OVER FILES

stop
end program


!  l1=1
!  l3=len(header)
! do i=1,nstats
!   stemp=header(l1:l3)
!   l2=scan(stemp,' ',slogical)
!   Names(i)=stemp(1:l2)
!   write(6,'(i3,3x,a)') i, trim(Names(i)) 
!   l1=l1+l2
!  enddo

