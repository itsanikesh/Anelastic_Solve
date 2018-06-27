program Binary_Plane_to_Plot
 !This program writes a binary Tecplot or VTK file in single precision from a plane of data.
 !Must be linked to tecio.a
                                                                                                                             
 !Modification History
 !-----------------------------------------------------------------------
 ! 08/05/2008 - Original Version Kyle A. Brucker
 !------------------------------------------------------------------------
                                                                                                                             
 implicit none

 !REAL TYPES
 integer,parameter :: r8 = 8 
 integer,parameter :: r4 = 4 

 !PLANE OUTPUT
 integer                                    :: nstep
 integer                                    :: np1, np2
 logical                                    :: single
 real(r8),allocatable,dimension(:,:)        :: DPplane
 real(r4),allocatable,dimension(:,:)        :: SPplane
 integer                                    :: nstart, nend, nskip
 integer                                    :: dir, index1,iu, iv, iw 
 logical                                    :: List,oldnew
 real(r8)                                   :: cloc,eloc
 real(4)                                    :: cL,eL
 !GRID
 real(r8),allocatable,dimension(:)          :: gc1, gc2, ge1, ge2
                                                                                                                             
 !TECPLOT
 real(r4),allocatable,dimension(:,:)        :: tecvar
 real(r4),allocatable,dimension(:,:)        :: g1tec,g2tec
 logical                                    :: tecplot_output
 character(len=150)                         :: ss1, ss2, ss3

 !VTK
 real(r4),allocatable,dimension(:,:)        :: vtkvar
 real(r4),allocatable,dimension(:)          :: g1vtk,g2vtk
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
! logical,parameter                          :: debug=.false.
 logical,parameter                          :: debug=.true.

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

 write(6,'(a)') 'Old/New'
 read(5,*) oldnew
  if ( debug ) write(6,*) oldnew

 do n=nstart,nend,nskip
  write(InFileName,'(a,a,i6.6,a)') trim(dataDIR),trim(basename)//"_n",n,".pln"
  if ( debug ) write(6,*) InFileName
  open(unit=500,file=InFileName,status='old',form='unformatted',iostat=s1,action='read')

 read(500) nstep,time,delt,g,rho_0,Re,Pr
 read(500) dir,index1, iu, iv, iw
 if (oldnew) read(500) cloc, eloc
 read(500) np1, np2

 allocate (gc1(1:np1))
 allocate (ge1(1:np1))
 allocate (gc2(1:np2))
 allocate (ge2(1:np2))

 read(500) gc1, ge1
 read(500) gc2, ge2

 if (single ) then
  allocate( SPplane(1:np1,1:np2),STAT=s1 )
  read(500)  SPplane
 else
  allocate( DPplane(1:np1,1:np2),STAT=s1 )
  read(500)  DPplane
 endif

 allocate( g1vtk(1:np1) )
 allocate( g2vtk(1:np2) )

 if (dir.EQ.1) then

  if (iv.EQ.1) then
   g1vtk=ge1
  else
   g1vtk=gc1
  endif

  if (iw.EQ.1) then
   g2vtk=ge2
  else
   g2vtk=gc2
  endif

 elseif (dir.EQ.2) then

  if (iu.EQ.1) then
   g1vtk=ge1
  else
   g1vtk=gc1
  endif

  if (iw.EQ.1) then
   g2vtk=ge2
  else
   g2vtk=gc2
  endif

 elseif (dir.EQ.3) then

  if (iu.EQ.1) then
   g1vtk=ge1
  else
   g1vtk=gc1
  endif

  if (iv.EQ.1) then
   g2vtk=ge2
  else
   g2vtk=gc2
  endif

 endif

 allocate( g1tec(1:np1,1:np2) )
 allocate( g2tec(1:np1,1:np2) )

 do k=1,np2
  do j=1,np1
   g1tec(j,k) = g1vtk(j)
   g2tec(j,k) = g2vtk(k)
  enddo
 enddo

 allocate( tecvar(1:np1,1:np2) )
 allocate( vtkvar(1:np1,1:np2) )
 if ( single ) then
  vtkvar(:,:) = SPplane(:,:)
  tecvar(:,:) = SPplane(:,:)
 else
  vtkvar(:,:) = real(DPplane(:,:))
  tecvar(:,:) = real(DPplane(:,:))
 endif

 if (tecplot_output) then
  write(ss1,'(f12.6)') time
  if (dir.EQ.1) ss2='x2 x3 var'
  if (dir.EQ.2) ss2='x1 x3 var'
  if (dir.EQ.3) ss2='x1 x2 var'
  write(ss3,'(a,a,i5.5,a4)') trim(outputDIR),trim(basename)//"_n",n,".plt"
  call open_tec_binary(time,1,np1,np2,ss1,ss2,ss3,'./',0,0)
  call write_tec_binary(1,np1,np2,g1tec,0)
  call write_tec_binary(1,np1,np2,g2tec,0)
  call write_tec_binary(1,np1,np2,tecvar,0)
  call close_tec_binary
 endif

 if (paraview_output) then
 !*********************************************
 !***************WRITE VTK FILE****************
 !*********************************************
 if (oldnew) then
  cL=real(cloc)
  eL=real(eloc)
 else
  cL=real(index1)
  eL=real(index1)
 endif

 write(OutFileName,'(a,a,i5.5,a4)') trim(outputDIR),trim(basename)//"_n",n,".vtk" 

 open(unit=13,file=OutFileName,access='stream',form='unformatted',status='new',&
         convert='big_endian',iostat=s1)

  !HEADER: note termination with char(10)
  write(13) "# vtk DataFile Version 3.0"//char(10)
  write(13) trim(BaseName)//char(10)
  write(13) "BINARY"//char(10)
  write(13) "DATASET RECTILINEAR_GRID"//char(10)

  if (dir.EQ.1) then 
   write(ss,fmt='(A10,3I5)') "DIMENSIONS",1,np1,np2
   write(13) ss//char(10)
   !X-grid
   write(ss,fmt='(A13,I6,A6)') "X_COORDINATES",1," float"
   write(13) char(10)//ss//char(10)
   if (iu.EQ.1) then
    write(13) eL
   else
    write(13) cL
   endif
   !Y-grid
   write(ss,fmt='(A13,I6,A6)') "Y_COORDINATES",np1," float"
   write(13) char(10)//ss//char(10)
   do j = 1, np1
    write(13) g1vtk(j)
   enddo
   !Z-grid
   write(ss,fmt='(A13,I6,A6)') "Z_COORDINATES",np2," float"
   write(13) char(10)//ss//char(10)
   do k = 1, np2
    write(13) g2vtk(k)
   enddo
  elseif (dir.EQ.2) then 
   write(ss,fmt='(A10,3I5)') "DIMENSIONS",np1,1,np2
   write(13) ss//char(10)
   !X-grid
   write(ss,fmt='(A13,I6,A6)') "X_COORDINATES",np1," float"
   write(13) char(10)//ss//char(10)
   do i = 1, np1
    write(13) g1vtk(i)
   enddo
   !Y-grid
   write(ss,fmt='(A13,I6,A6)') "Y_COORDINATES",1," float"
   write(13) char(10)//ss//char(10)
   if (iv.EQ.1) then
    write(13) eL
   else
    write(13) cL
   endif
   !Z-grid
   write(ss,fmt='(A13,I6,A6)') "Z_COORDINATES",np2," float"
   write(13) char(10)//ss//char(10)
   do k = 1, np2
    write(13) g2vtk(k)
   enddo
  elseif (dir.EQ.3) then 
   write(ss,fmt='(A10,3I5)') "DIMENSIONS",np1,np2,1
   write(13) ss//char(10)
   !X-grid
   write(ss,fmt='(A13,I6,A6)') "X_COORDINATES",np1," float"
   write(13) char(10)//ss//char(10)
   do i = 1, np1
    write(13) g1vtk(i)
   enddo
   !Y-grid
   write(ss,fmt='(A13,I6,A6)') "Y_COORDINATES",np2," float"
   write(13) char(10)//ss//char(10)
   do j = 1, np2
    write(13) g2vtk(j)
   enddo
   !Z-grid
   write(ss,fmt='(A13,I6,A6)') "Z_COORDINATES",1," float"
   write(13) char(10)//ss//char(10)
   if (iw.EQ.1) then
    write(13) eL
   else
    write(13) cL
   endif
  endif

   !Field
   write(ss,fmt='(A10,I15)') "POINT_DATA",np1*np2
   write(13) char(10)//ss//char(10)
   write(13) "SCALARS Fraction float 1"//char(10)
   write(13) "LOOKUP_TABLE default"//char(10)
   write(13) vtkvar
   !Close VTK File
  close(13)

 endif

 deallocate( gc1,ge1,gc2,ge2,vtkvar,g1vtk,g2vtk,tecvar,g1tec,g2tec )

 if (allocated(SPplane) ) deallocate(SPplane)
 if (allocated(DPplane) ) deallocate(DPplane)

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

