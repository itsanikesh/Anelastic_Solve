program Wake_Binary_to_Plot
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
 character(len=80)                          :: dataDIR, outputDIR
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

 write(6,'(a)') 'Output Directory:'
 read(5,*) outputDIR
  if ( debug ) write(6,*) outputDir

 write(6,'(a)') 'Tecplot:'
 read(5,*) tecplot_output
  if ( debug ) write(6,*) tecplot_output

 write(6,'(a)') 'Paraview:'
 read(5,*) paraview_output
  if ( debug ) write(6,*) paraview_output

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

 write(6,'(a)') 'Single/Double'
 read(5,*) single
  if ( debug ) write(6,*) single 

 read(5,*) stat
  if ( debug ) write(6,*) stat

 write(6,'(a)') 'List'
 read(5,*) List
  if ( debug ) write(6,*) List 

 do n=nstart,nend,nskip
  write(InFileName,'(a,a,i5.5)') trim(dataDIR),trim(basename),n
  if ( debug ) write(6,*) InFileName
  open(unit=500,file=InFileName,status='old',form='unformatted',iostat=s1,action='read')

  read(500) nstep
  read(500) time,delt,Re,Pr,g,rho_0
  read(500) nyp2,nzp2
   allocate( yc(1:nyp2) )
   allocate( ye(1:nyp2) )
   allocate( zc(1:nzp2) )
   allocate( ze(1:nzp2) )
  read(500) yc
  read(500) ye
  read(500) zc
  read(500) ze
  read(500) nstats, ngroups

  allocate( Sname(1:nstats) )
  allocate( Gname(1:ngroups) )
  allocate( group(1:nstats)  ) 
  read(500) Sname
  read(500) Gname
  read(500) group

   if (single) then
    allocate( statsSP(1:nyp2,1:nzp2,1:nstats) )
    read(500) statsSP
   else 
    allocate( statsDP(1:nyp2,1:nzp2,1:nstats) )
    read(500) statsDP
   endif

 if ( list ) then 
 write(6,*) " "
 write(6,'(a)') "*********************************************"
 write(6,'(a)') "************AVAILABLE STATISTICS*************"
 write(6,'(a)') "*********************************************"
 write(6,'(a)') "  #   Name                                   "
 do i=1,nstats
  write(6,'(i3,3x,a,3x,a)') i,trim(Sname(i))!,trim(Gname( group(i) ))
 enddo
 stop
 endif

 allocate( yvtk(1:nyp2) )
 allocate( zvtk(1:nzp2) )
  yvtk(:) = yc(:)
  zvtk(:) = zc(:)

 allocate( vtkvar(1:nyp2,1:nzp2) )
  if ( single ) then
   vtkvar(:,:) = statsSP(:,:,stat)
  else
   vtkvar(:,:) = statsDP(:,:,stat)
  endif

 allocate( ytec(1:nyp2,1:nzp2) )
 allocate( ztec(1:nyp2,1:nzp2) )
 do k=1,nzp2
  do j=1,nyp2
   ytec(j,k) = yc(j)
   ztec(j,k) = zc(k)
  enddo
 enddo
 allocate( tecvar(1:nyp2,1:nzp2) )
  if ( single ) then
   tecvar(:,:) = statsSP(:,:,stat)
  else
   tecvar(:,:) = statsDP(:,:,stat)
  endif

 if (tecplot_output) then
  write(ss1,'(f12.6)') time
  ss2='y z '//trim( Sname(stat) )
  write(ss3,'(a,a,i5.5,a4)') trim(outputDIR),trim(SName(stat))//"_",n,".plt"

  call open_tec_binary(time,nyp2,nzp2,1,ss1,ss2,ss3,'./',0,0)
  call write_tec_binary(nyp2,nzp2,1,ytec,0)
  call write_tec_binary(nyp2,nzp2,1,ztec,0)
  if (single) then 
   call write_tec_binary(nyp2,nzp2,1,tecvar,0)
  else
   call write_tec_binary(nyp2,nzp2,1,tecvar,1)
  endif

  call close_tec_binary
 endif


 if (paraview_output) then
 !*********************************************
 !***************WRITE VTK FILE****************
 !*********************************************

 write(OutFileName,'(a,a,i5.5,a4)') trim(outputDIR),trim(SName(stat))//"_",n,".vtk" 

 open(unit=13,file=OutFileName,access='stream',form='unformatted',status='new',&
         convert='big_endian',iostat=s1)
                                                                                                                             
  !HEADER: note termination with char(10)
  write(13) "# vtk DataFile Version 3.0"//char(10)
  write(13) Sname(stat)//char(10)
  write(13) "BINARY"//char(10)
  write(13) "DATASET RECTILINEAR_GRID"//char(10)
  write(ss,fmt='(A10,3I5)') "DIMENSIONS",1,nyp2,nzp2
  write(13) ss//char(10)

  !X-grid
  write(ss,fmt='(A13,I6,A6)') "X_COORDINATES",1," float"
  write(13) char(10)//ss//char(10)
  write(13) real(time) 

  !Y-grid
  write(ss,fmt='(A13,I6,A6)') "Y_COORDINATES",nyp2," float"
  write(13) char(10)//ss//char(10)
  do j = 1, nyp2
   write(13) yvtk(j)
  enddo
                                                                                                                             
  !Z-grid
  write(ss,fmt='(A13,I6,A6)') "Z_COORDINATES",nzp2," float"
  write(13) char(10)//ss//char(10)
  do k = 1, nzp2
   write(13) zvtk(k)
  enddo
                                                                                                                             
  !Field
  write(ss,fmt='(A10,I15)') "POINT_DATA",nyp2*nzp2
  write(13) char(10)//ss//char(10)
  write(13) "SCALARS Fraction float 1"//char(10)
  write(13) "LOOKUP_TABLE default"//char(10)
  write(13) vtkvar
                                                                                                                             
 !Close VTK File
 close(13)

 endif

 deallocate( yc,ye,zc,ze,vtkvar,yvtk,zvtk,tecvar,ytec,ztec,Sname,&
             Gname,group )

 if (allocated(statsSP) ) deallocate(statsSP)
 if (allocated(statsDP) ) deallocate(statsDP)


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

