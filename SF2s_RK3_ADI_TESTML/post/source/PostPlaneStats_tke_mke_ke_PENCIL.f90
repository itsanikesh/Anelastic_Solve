












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
 logical                                    :: single, calctke, calcmke, calcKE
 real(r8),allocatable,dimension(:,:)        :: statsDP
 real(r8),allocatable,dimension(:,:)        :: statsSP
 integer                                    :: nstart, nend, nskip
 integer                                    :: stat
 logical                                    :: List

 !GRID
 real(r8),allocatable,dimension(:)          :: yc,zc, ye, ze,dze,dye
                                                                                                                             
 !STRIP TKE, MKE, KE
 real(r8),allocatable,dimension(:)        :: tke,mke,KE,u1p,u2p,u3p,U1,U2,U3,tempF
 real(r8)				    :: Stemp
 !TECPLOT
 real(r8),allocatable,dimension(:)        :: tecvar
 real(r8),allocatable,dimension(:)        :: ytec,ztec,N2
 logical                                    :: tecplot_output
 character(len=150)                         :: ss1, ss2, ss3

 !VTK
 real(r8),allocatable,dimension(:)        :: vtkvar
 real(r8),allocatable,dimension(:)          :: yvtk,zvtk
 logical                                    :: paraview_output
 real(4)				    :: netmomentum,dA,netmomentump,netmomentumn
 !FILENAMES
 character(len=80)                          :: InFileName, OutFileName 
 character(len=80)                          :: dataDIR, outputDIR
 character(len=80)                          :: basename
                                                                                                                             
 !OTHER STRINGS
 character(len=25)                          :: ss
 character(len=25)                          :: TITLE
! character(len=100)                         :: stemp
 integer                                    :: l1,l2,l3
 logical                                    :: slogical
 character(len=25),allocatable,dimension(:) :: Sname, Gname
 integer,allocatable,dimension(:)           :: group

 !LOOPING VARIABLES
 integer                                    :: i,j,k,n

 !PARAMETERS
 real(8)                                    :: time, delt, g, rho_0, Re, Pr, rRe 

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

 write(6,'(a)') 'Calculate tke'
 read(5,*) calctke
  if ( debug ) write(6,*) calctke

 write(6,'(a)') 'Calculate mke'
 read(5,*) calcmke
  if ( debug ) write(6,*) calcmke

 write(6,'(a)') 'Calculate KE'
 read(5,*) calcKE
  if ( debug ) write(6,*) calcKE

rRe=1.0d0/1000000.0d0

 do n=nstart,nend,nskip
 write(*,'(a,a,i5.5,a)'),trim(dataDIR),trim(basename),n,'.bin'
  write(InFileName,'(a,a,i5.5,a)') trim(dataDIR),trim(basename),n,'.bin'
  if ( debug ) write(6,*) InFileName
  open(unit=500,file=InFileName,status='old',form='unformatted',iostat=s1,action='read')
  write(*,*),"WRITTEN TILL HERE"
  read(500) time
  read(500) nzp2, nstats, ngroups
   allocate( zc(1:nzp2) )
  read(500) zc
  allocate( Sname(1:nstats) )
  allocate( Gname(1:ngroups) )
  allocate( group(1:nstats)  ) 
  read(500) Sname
  read(500) Gname
  read(500) group

   if (single) then
    allocate( statsSP(1:nzp2,1:nstats) )
    read(500) statsSP
   else 
    allocate( statsDP(1:nzp2,1:nstats) )
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


 allocate( zvtk(1:nzp2) )
  zvtk(:) = zc(:)

 allocate( vtkvar(1:nzp2) )
  if ( single ) then
   vtkvar(:) = statsSP(:,stat)
  else
   vtkvar(:) = statsDP(:,stat)
  endif

 allocate( ztec(1:nzp2) )
 allocate( N2(1:nzp2))
 do k=1,nzp2
   ztec(k) = zc(k)
 enddo

 allocate( tecvar(1:nzp2) )
  if ( single ) then
   tecvar(:) = statsSP(:,stat)
  else
   tecvar(:) = statsDP(:,stat)
  endif

 ! calculate tke, mke and KE if needed
  if (calctke) then 
    allocate( tke(1:nzp2) )
    Sname(stat)='tke'
    if ( single ) then
     tke(:) = 0.5d0 * ( statsSP(:,6)*statsSP(:,6) &
                        + statsSP(:,7)*statsSP(:,7) &
                        + statsSP(:,8)*statsSP(:,8) )
     tecvar = tke
     vtkvar = tke
    else
     tke(:) = 0.5d0 * ( statsDP(:,6)*statsDP(:,6) &
                        + statsDP(:,7)*statsDP(:,7) &
                        + statsDP(:,8)*statsDP(:,8) )
     tecvar = tke
     vtkvar = tke
    endif
  endif
  if (calcmke) then 
    allocate( mke(1:nzp2) )
    Sname(stat)='mke'
    if ( single ) then
     mke(:) = 0.5d0 * ( statsSP(:,1)*statsSP(:,1) &
                        + statsSP(:,2)*statsSP(:,2) &
                        + statsSP(:,3)*statsSP(:,3) )
     tecvar = mke
     vtkvar = mke
    else
     mke(:) = 0.5d0 * ( statsDP(:,1)*statsDP(:,1) &
                        + statsDP(:,2)*statsDP(:,2) &
                        + statsDP(:,3)*statsDP(:,3) )
     tecvar = mke
     vtkvar = mke
    endif
  endif
  if (calcKE) then 
    allocate( KE(1:nzp2) )
    Sname(stat)='KE'
    if ( single ) then
     KE(:)  = 0.5d0 * ( statsSP(:,1)*statsSP(:,1) &
                        + statsSP(:,2)*statsSP(:,2) &
                        + statsSP(:,3)*statsSP(:,3) &
                        + statsSP(:,6)*statsSP(:,6) &
                        + statsSP(:,7)*statsSP(:,7) &
                        + statsSP(:,8)*statsSP(:,8) )
     tecvar = KE
     vtkvar = KE
    else
     KE(:)  = 0.5d0 * ( statsDP(:,1)*statsDP(:,1) &
                        + statsDP(:,2)*statsDP(:,2) &
                        + statsDP(:,3)*statsDP(:,3) &
                        + statsDP(:,6)*statsDP(:,6) &
                        + statsDP(:,7)*statsDP(:,7) &
                        + statsDP(:,8)*statsDP(:,8) )
     tecvar = KE
     vtkvar = KE
    endif
  endif
write(*,*)"CALCULATED TILL HERE"
 if (tecplot_output) then
 write(6,'(a)') 'Tecplot libaries not linked, change USETEC flag to -DUSETEC in arch.in file'
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
  write(ss,fmt='(A10,3I5)') "DIMENSIONS",1,nzp2
  write(13) ss//char(10)

  !X-grid
  write(ss,fmt='(A13,I6,A6)') "X_COORDINATES",1," float"
  write(13) char(10)//ss//char(10)
  write(13) real(time) 

  !Y-grid
                                                                                                                             
  !Z-grid
  write(ss,fmt='(A13,I6,A6)') "Z_COORDINATES",nzp2," float"
  write(13) char(10)//ss//char(10)
  do k = 1, nzp2
   write(13) zvtk(k)
  enddo
                                                                                                                             
  !Field
  write(ss,fmt='(A10,I15)') "POINT_DATA",nzp2
  write(13) char(10)//ss//char(10)
  write(13) "SCALARS Fraction float 1"//char(10)
  write(13) "LOOKUP_TABLE default"//char(10)
  write(13) vtkvar
                                                                                                                             
 !Close VTK File
 close(13)

 endif

 deallocate(zc,vtkvar,zvtk,tecvar,ztec,N2,Sname,&
             Gname,group )

 if (allocated(statsSP) ) deallocate(statsSP)
 if (allocated(statsDP) ) deallocate(statsDP)
 if (allocated(tke) ) deallocate(tke)
 if (allocated(mke) ) deallocate(mke)
 if (allocated(KE) )  deallocate(KE)
 

 enddo !LOOP OVER FILES

stop
end program


