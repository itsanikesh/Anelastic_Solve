program spectra_1D_kx
 implicit none
 include 'fftw3.f'
 
 integer :: n1, n2, dir, nstep, s1, index1, loc, iu, iv, iw, i, n
 real(8) :: time, delt, g, rho_0, Re, Pr, cL, eL
 logical :: single,oldnew
 real(8),allocatable,dimension(:,:)     :: dataDP
 real(4),allocatable,dimension(:,:)     :: dataSP
 real(8),allocatable,dimension(:)       :: g1c,g2c,g1e,g2e
 real(8),allocatable,dimension(:)       :: pencilDP
 real(4),allocatable,dimension(:)       :: pencilSP
 complex(8),allocatable,dimension(:)    :: SpecDP
 complex(8),allocatable,dimension(:,:)  :: SpecStore
 integer(8)                             :: plan
 real(8)                                :: xL, dxM
 real(8),allocatable,dimension(:)       :: kx, mkx, Espec
 integer                                :: ishell, iconj
 real(8)                                :: Pi
 integer                                :: x2loc,x3loc,iter,nspec,nl,nu
 character(len=50)                      :: base,suffix,fname,var1,var2,dataDIR,outputDIR
 logical,parameter                      :: debug=.true. 

 Pi =4.d0*datan(1.d0)
 suffix='.pln'
 nspec=9
 nl=-nspec/2-1
 nu=nspec/2-1

 write(6,'(a)') 'Data Directory:'
 read(5,*) dataDir
  if ( debug ) write(6,*) dataDir

 write(6,'(a)') 'Output Directory:'
 read(5,*) outputDIR
  if ( debug ) write(6,*) outputDir

 write(6,'(a)') 'Var:'
 read(5,*) base
  if ( debug ) write(6,*) base

 write(6,'(a)') 'Iteration:'
  read(5,*) iter
   if ( debug ) write(6,*) iter

 write(6,'(a)') 'x2loc:'
 read(5,*) x2loc
  if ( debug ) write(6,*) x2loc

 write(6,'(a)') 'x3loc:'
 read(5,*) x3loc
  if ( debug ) write(6,*) x3loc

 write(6,'(a)') 'Single/Double'
 read(5,*) single
  if ( debug ) write(6,*) single 

 write(6,'(a)') 'Old/New'
 read(5,*) oldnew
  if ( debug ) write(6,*) oldnew

 var1=trim(dataDIR)//trim(base)//'_j'
 var2=trim(dataDIR)//trim(base)//'_k'

 write(fname,'(a,i4.4,a,i5.5,a)') trim(var1),x2loc,'_n',iter,trim(suffix)
 if (debug) write(6,*) fname
 open(unit=100,file=fname,status='old',form='unformatted',iostat=s1)
  read(100) nstep ,time ,delt ,g,rho_0 ,Re,Pr
  read(100) dir,index1, iu, iv, iw
  if (oldnew) read(100) cL,eL 
  read(100) n1, n2

  allocate ( g1c(1:n1) )
  allocate ( g2c(1:n2) )
  allocate ( g1e(1:n1) )
  allocate ( g2e(1:n2) )

  read(100) g1c,g1e
  read(100) g2c,g2e

  if (single) then
   allocate( dataSP(1:n1,1:n2) )
   read(100) dataSP
  else 
   allocate( dataDP(1:n1,1:n2) )
   read(100) dataDP
  endif

  allocate( pencilDP(1:n1) )
  allocate( SpecDP(0:n1/2) )
  allocate( SpecStore(0:n1/2,2*nspec) )

  call dfftw_plan_dft_r2c_1d(plan,n1,pencilDP,SpecDP,FFTW_ESTIMATE)

  loc=1
  do n=nl,nu
   if (single) then
    pencilDP(:) = dataSP(:,x3loc+n)
   else 
    pencilDP(:) = dataDP(:,x3loc+n)
   endif
   call dfftw_execute(plan)
   SpecDP=SpecDP/dble(n1)
   SpecStore(:,loc) = SpecDP
   loc=loc+1
  enddo 
 close(100)
 deallocate(g1c,g1e,g2c,g2e)
 if (allocated(dataSP)) deallocate(dataSP)
 if (allocated(dataDP)) deallocate(dataDP)

 write(fname,'(a,i4.4,a,i5.5,a)') trim(var2),x3loc,'_n',iter,trim(suffix)
 if (debug) write(6,*) fname
 open(unit=100,file=fname,status='old',form='unformatted',iostat=s1)
  read(100) nstep,time,delt,g,rho_0,Re,Pr
  read(100) dir,index1, iu, iv, iw
  if (oldnew) read(100) cL,eL
  read(100) n1, n2

  allocate ( g1c(1:n1) )
  allocate ( g2c(1:n2) )
  allocate ( g1e(1:n1) )
  allocate ( g2e(1:n2) )

  read(100) g1c,g1e
  read(100) g2c,g2e

  if (single) then
   allocate( dataSP(1:n1,1:n2) )
   read(100) dataSP
  else 
   allocate( dataDP(1:n1,1:n2) )
   read(100) dataDP
  endif

  do n=nl,nu
   if (single) then
    pencilDP(:) = dataSP(:,x2loc+n)
   else 
    pencilDP(:) = dataDP(:,x2loc+n)
   endif
   call dfftw_execute(plan)
   SpecDP=SpecDP/dble(n1)
   SpecStore(:,loc) = SpecDP
   loc=loc+1
  enddo 
 close(100)

!*********************************************************************************
!**********************Setup Wave and Modified Wave Numbers***********************
!*********************************************************************************
 xL = g1c(n1)-g1c(1)
 dxM=xL/dble(n1)
 write(6,*) "Physical Length:",xL
 allocate( kx(0:n1-1) )
 allocate( mkx(0:n1-1) )

 do i=0,n1-1 !1
  kx(i) = 2.d0*Pi*dble(i)/xL
  if (i.GT.(n1)/2) kx(i) = 2.d0*Pi*dble(i-n1)/xL
 enddo  !1
 do i=0,n1-1
  mkx(i) = dsin(kx(i)*dxm)/dxm
 enddo

!*********************************************************************************
!******************************Compute kx spectra*********************************
!*********************************************************************************

 allocate( Espec(0:n1-1) )
 Espec=0.d0
 loc=1
 do n=1,2*nspec
 do i=0,n1-1
  ishell = i !(idint(2.0d0*dsqrt(kx(i)**2))+1)/2
  if (i.GT.n1/2) then
   iconj = n1-i
   Espec(ishell) = Espec(ishell)&
                  +  dble( dconjg( SpecStore(iconj,loc) )*SpecStore(iconj,loc) )
  else
   Espec(ishell) = Espec(ishell) &
                  +  dble( dconjg( SpecStore(i,loc) )*SpecStore(i,loc) )
  endif
 enddo

 loc=loc+1
 enddo
 !**********************************************
 !************WRITE SPECTRA TO FILE*************
 !**********************************************
 write(fname,'(a,i5.5)') trim(outputDIR)//trim(base)//'_spec.',iter

 open(unit=13,file=fname,form='formatted',status='unknown',iostat=s1)
 write(13,*) "# Time:",time
 do i=0,n1/2 
  write(13,'(2(1x,e15.8))') dble(i),Espec(i)/dble(2*nspec+1)
 enddo

 close(13)


stop
end program
