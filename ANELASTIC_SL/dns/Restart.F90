subroutine read_flow(in_file,in_iter,stat)
!@t
! \textbf{subroutine read\_flow(in\_file,in\_iter,stat)}
!@h
!   Description:
!     Reads in a restart file.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use IO, only: IObig

 implicit none

 !Passed Variables
 character(len=*),intent(in) :: in_file
 integer,intent(in)          :: in_iter
 integer,intent(out)         :: stat

 !Local Variables
 integer                     :: ok 

 ok=0
#ifdef PARALLEL
 if (IObig) then
  call start_big(in_file,in_iter,ok)
 else
  call start_small(in_file,in_iter,ok)
 endif
#else
 call start_small(in_file,in_iter,ok)
#endif

 stat=ok

return
end subroutine read_flow

subroutine write_flow(out_file,stat)
!@t
! \textbf{subroutine write\_flow(out\_file,stat)}
!@h
!   Description:
!     Writes a restart file.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use IO, only: IObig

 implicit none

 !Passed Variables
 character(len=*),intent(in) :: out_file
 integer,intent(out)         :: stat

 !Local Variables
 integer                     :: ok 
 ok=0

#ifdef PARALLEL
 if (IObig) then
  call write_flow_big(out_file,ok)
 else
  call write_flow_small(out_file,ok)
 endif
#else
 call write_flow_small(out_file,ok)
#endif

 stat=ok

return
end subroutine write_flow

subroutine Greadfile(ICfile,u,v,w,p,r,scal1,nx,ny,nz,  &
                         n_time,time,delt,g,rho_0,scal1_0,Re,Pr,Sc,stat)
!@t
! \textbf{subroutine Greadfile(ICfile,u,v,w,p,r,scal1,nx,ny,nz,n\_time,time,delt,g,rho\_0,scal1\_0,Re,Pr,Sc,stat)}
!@h
!   Description:
!     Reads in a restart file line by line.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use ntypes, only: r8
 use IO,     only: IOUT
 implicit none

!Passed Variables
 character(len=*),intent(in)     :: ICfile
 integer,intent(in)              :: nx, ny, nz
 integer,intent(out)             :: n_time
 real(r8),intent(out)            :: time,delt
 real(r8),intent(out)            :: g,rho_0,scal1_0,Re,Pr,Sc
 real(r8),intent(out)            :: u(1:nx,1:ny,1:nz)
 real(r8),intent(out)            :: v(1:nx,1:ny,1:nz)
 real(r8),intent(out)            :: w(1:nx,1:ny,1:nz)
 real(r8),intent(out)            :: p(1:nx,1:ny,1:nz)
 real(r8),intent(out)            :: r(1:nx,1:ny,1:nz)
 real(r8),intent(out)            :: scal1(1:nx,1:ny,1:nz)
 integer,intent(out)             :: stat

!Local Variables
 integer                     :: n_r(1:3), s1

!Direct Access read
 open(310,file=ICfile,form='unformatted',status='old',iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR OPENING FILE: "//trim(Icfile)//" IOSTAT=",s1
   stat=1
   goto 2000
  endif
  read(310) n_time,time,delt,g,rho_0,scal1_0,Re,Pr,Sc
  read(310) n_r 
   if (n_r(1).NE.nx)   goto 1000 !Check to make sure dump is correct size
   if (n_r(2).NE.ny)   goto 1000
   if (n_r(3).NE.nz)   goto 1000
  read(310) u 
  read(310) v
  read(310) w
  read(310) p
  read(310) r 
  read(310) scal1
 close(310)

 write(IOUT,'(a)') "READ OF INITIAL FIELD: "//trim(Icfile)//" COMPLETED"
 return

 1000 continue
 close(310)
 write(IOUT,'(a36,3x(i4,a1))') "ERROR: Data file size is [nx,ny,nz]", &
                                      n_r(1),' ',n_r(2),' ',n_r(3)
 write(IOUT,'(a36,3x(i4,a1))') "ERROR: Run  size is [nx,ny,nz]", nx, &
                                   ' ',ny,' ',nz
 stat = 1
 stop

 2000 continue
 close(310)
 stat = 2
 stop
end subroutine Greadfile

#ifdef PARALLEL
subroutine start_big(basename,ntime,err1)
!@t
! \textbf{subroutine start\_big(basename,ntime,err1)}
!@h
!   Description:
!     Starts a simulation with data from one large restart file.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use ntypes,     only: r8
 use Flow,       only: u,v,w,p,rho,scal1
 use Domain,     only: sx,ex,sy,ey,sz,ez
 use Parameters, only: g,rho_0,scal1_0,Re,Pr,Sc,time,delt,nstep,rRe,rPr,rSc,icparam
 use IO,         only: IOUT
 use dd,         only: coords
 implicit none

!Passed Variables
 character(len=*)       :: basename 
 integer,intent(in)     :: ntime
 integer,intent(out)    :: err1

!Local Variables
 integer  :: s1,stat, n_r(1:6),i
 real(r8) :: g_IN,rho_0_IN,scal1_0_IN,Re_IN,Pr_IN,Sc_IN,time_IN,delt_IN
 integer  :: nstep_IN 
 character (len = 300 )  :: ICfile 

 ICfile=basename
 call concati(ICfile,coords(1))
 call concat(ICfile,'_')
 call concati(ICfile,coords(2))
 call concat(ICfile,'_')
 call concati(ICfile,coords(3))
 call concat(ICfile,'.')
 call concati(ICfile,ntime)

!Direct Access read
 open(310,file=ICfile,form='unformatted',status='old',iostat=s1,action='read')
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR OPENING FILE: "//trim(Icfile)//" IOSTAT=",s1
   stat=1
   stop
  endif

  read(310) nstep_IN,time_IN,delt_IN,g_IN,rho_0_IN,scal1_0_IN,Re_IN,Pr_IN,Sc_IN
  read(310) n_r
   if (n_r(1).NE.sx)   goto 1000 !Check to make sure dump is correct size
   if (n_r(2).NE.ex)   goto 1000
   if (n_r(3).NE.sy)   goto 1000
   if (n_r(4).NE.ey)   goto 1000 
   if (n_r(5).NE.sz)   goto 1000
   if (n_r(6).NE.ez)   goto 1000
  read(310) u
  read(310) v
  read(310) w
  read(310) p
  read(310) rho
  read(310) scal1
 close(310)
 
 if (icparam) then
  Re=Re_IN
  rRe=1.d0/Re
  Pr=Pr_IN
  rPr=1.d0/Pr
  Sc=Sc_IN
  rSc=1.d0/Sc
  nstep=nstep_IN
  time=time_IN
  delt=delt_IN
  g=g_IN
  rho_0=rho_0_IN
  scal1_0=scal1_0_IN
 endif
 err1=0
 write(IOUT,'(a)') "READ OF INITIAL FIELD: "//trim(ICfile)//" COMPLETED"
 return
                                                                                                                             
 1000 continue
 close(310)
 write(IOUT,'(a36,3(1x,i4))') "ERROR: Data file size is [sx:ex,sy:ey,ez:ez]", &
                                      (n_r(i),i=1,6)
 write(IOUT,'(a36,3(1x,i4))') "ERROR: Run  size is [sx:ex,sy:ey,sz:ez]",sx,ex,&
                                  sy,ey,sz,ez
 
 close(unit=310)
 stop
end subroutine start_big
#endif

subroutine start_small(basename,ntime,err1)
!@t
! \textbf{subroutine start\_small(basename,ntime,err1)}
!@h
!   Description:
!     Starts a simulation from multiple distributed restart files.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use ntypes,     only: r8
 use Flow,       only: u,v,w,p,rho,scal1
 use Domain,     only: nxp2,nyp2,nzp2,sx,ex,sy,ey,sz,ez
 use Parameters, only: g,rho_0,scal1_0,Re,Pr,Sc,time,delt,nstep,rRe,rPr,rSc,icparam
 use IO,         only: IOUT
#ifdef PARALLEL
 use dd,         only: myid,comm3d,realtype, inttype
#endif
 implicit none

 !Passed Variables
 character(len=*)       :: basename
 integer,intent(in)     :: ntime 
 integer,intent(out)    :: err1

 !Local Variables
 integer  :: s1,stat
 real(r8) :: g_IN,rho_0_IN,scal1_0_IN,Re_IN,Pr_IN,Sc_IN,time_IN, delt_IN
 integer  :: nstep_IN 
 character (len = 300 )  :: ICfile

#ifdef PARALLEL
 real(r8),allocatable,dimension(:,:,:) :: uF,vF,wF,pF,rF,scal1F,tmp1
 if (myid.eq.0) then
  call deallocate_temps(stat)
  if (err1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR: Deallocating Temps, stat:",s1
   stat=1
   goto 1000
  endif
  allocate( uF(1:nxp2,1:nyp2,1:nzp2), stat=s1 )
  allocate( vF(1:nxp2,1:nyp2,1:nzp2), stat=s1 )
  allocate( wF(1:nxp2,1:nyp2,1:nzp2), stat=s1 )
  allocate( pF(1:nxp2,1:nyp2,1:nzp2), stat=s1 )
  allocate( rF(1:nxp2,1:nyp2,1:nzp2), stat=s1 )
  allocate( scal1F(1:nxp2,1:nyp2,1:nzp2), stat=s1 )
  allocate( tmp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR: Allocating full fields, stat:",s1
   stat=2
   goto 1000
  endif

  ICfile=basename
  call concati(ICfile,ntime)

  call Greadfile(ICfile,uF,vF,wF,pF,rF,scal1F,nxp2,nyp2,nzp2,&
                  nstep_IN,time_IN,delt_IN,g_IN,rho_0_IN,scal1_0_IN,Re_IN,Pr_IN,Sc_IN,stat)

 endif 

 if (myid.eq.0) then
   call Distribute3dM(uF,u,tmp1,0,stat)
 else
   call Distribute3dS(u,0,stat)
 endif
 write(IOUT,*) "U1 DISTRIBUTED"
 call MPI_BARRIER(comm3d,stat)

 if (myid.eq.0) then
  call Distribute3dM(vF,v,tmp1,0,stat)
 else
  call Distribute3dS(v,0,stat)
 endif
 write(IOUT,*) "U2 DISTRIBUTED"
 call MPI_BARRIER(comm3d,stat)

 if (myid.eq.0) then
  call Distribute3dM(wF,w,tmp1,0,stat)
 else
  call Distribute3dS(w,0,stat)
 endif
 write(IOUT,*) "U3 DISTRIBUTED"
 call MPI_BARRIER(comm3d,stat)

 if (myid.eq.0) then
  call Distribute3dM(pF,p,tmp1,0,stat)
 else
  call Distribute3dS(p,0,stat)
 endif
 write(IOUT,*) "P DISTRIBUTED"
 call MPI_BARRIER(comm3d,stat)

 if (myid.eq.0) then
  call Distribute3dM(rF,rho,tmp1,0,stat)
 else
  call Distribute3dS(rho,0,stat)
 endif
 write(IOUT,*) "RHO DISTRIBUTED"
 call MPI_BARRIER(comm3d,stat)

 if (myid.eq.0) then
  call Distribute3dM(scal1F,scal1,tmp1,0,stat)
 else
  call Distribute3dS(scal1,0,stat)
 endif
 write(IOUT,*) "SCAL1 DISTRIBUTED"
 call MPI_BARRIER(comm3d,stat)


 if (myid.EQ.0) then
   deallocate( uF, stat=s1 )
   deallocate( vF, stat=s1 )
   deallocate( wF, stat=s1 )
   deallocate( pF, stat=s1 )
   deallocate( rF, stat=s1 )
   deallocate( scal1F, stat=s1 )
   deallocate( tmp1, stat=s1 )
   if (s1.NE.0) then
    write(IOUT,'(a,i4)') "ERROR: Deallocating full fields in  &
                  & Distribute3d, stat:",s1

    stat=2
    goto 1000
   endif
   call allocate_temps(stat)
  if (err1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR ALLOCATING TEMPS IN `startup` FATAL!"
   goto 2000
  endif
  endif

 
 if (icparam) then
 if (myid.eq.0) then
  Re=Re_IN
  Pr=Pr_IN
  Sc=Sc_IN
  nstep=nstep_IN
  time=time_IN
  delt=delt_IN
  g=g_IN
  rho_0=rho_0_IN
  scal1_0=scal1_0_IN
 endif

  call MPI_BCAST(Re,1,realtype,0,comm3d,stat)
  rRe=1.d0/Re
  call MPI_BCAST(Pr,1,realtype,0,comm3d,stat)
  rPr=1.d0/Pr
  call MPI_BCAST(Sc,1,realtype,0,comm3d,stat)
  rSc=1.d0/Sc
  call MPI_BCAST(nstep,1,inttype,0,comm3d,stat)
  call MPI_BCAST(time,1,realtype,0,comm3d,stat)
  call MPI_BCAST(delt,1,realtype,0,comm3d,stat)
  call MPI_BCAST(g,1,realtype,0,comm3d,stat)
  call MPI_BCAST(rho_0,1,realtype,0,comm3d,stat)
  call MPI_BCAST(scal1_0,1,realtype,0,comm3d,stat)
 endif

#else

 ICfile=basename
 call concati(ICfile,ntime)

 call Greadfile(ICfile,u,v,w,p,rho,scal1,nxp2,nyp2,nzp2,&
                  nstep_IN,time_IN,delt_IN,g_IN,rho_0_IN,scal1_0_IN,Re_IN,Pr_IN,Sc_IN,stat)
 if (icparam) then
  Re=Re_IN
  Pr=Pr_IN
  Sc=Sc_IN
  nstep=nstep_IN
  time=time_IN
  delt=delt_IN
  g=g_IN
  rho_0=rho_0_IN
  scal1_0=scal1_0_IN
  rRe=1.d0/Re
  rPr=1.d0/Pr
  rSc=1.d0/Sc
 endif

#endif

 call ghost(u,'u',stat     )
 call ghost(v,'v',stat     )
 call ghost(w,'w',stat     )
 call ghost(p,'p',stat     )
 call ghost(rho,'rho',stat )
 call ghost(scal1,'scal1',stat )

 write(IOUT,'(a)') "START FROM SERIAL DUMP COMPLETED" 
 err1=stat
 return
 1000 continue
 write(IOUT,'(a)') "START FROM SERIAL DUMP FAILED" 
 err1=stat
 return
 
 2000 stop
end subroutine start_small

#ifdef PARALLEL
subroutine write_flow_big(basename,err1)
!@t
! \textbf{subroutine write\_flow\_big(basename,err1)}
!@h
!   Description:
!     Write a restart file for each processor for a parallel simulation.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use ntypes,     only: r8
 use Flow,       only: u,v,w,p,rho,scal1
 use Domain,     only: sx,ex,sy,ey,sz,ez
 use Parameters, only: g, rho_0,scal1_0,Re,Pr,Sc,time,delt,nstep,rRe,rPr,rSc,icparam
 use IO,         only: IOUT,flowDIR
 use dd,         only: coords
 implicit none

!Passed Variables
 character(len=*),intent(in)       :: basename 
 integer,intent(out)               :: err1

!Local Variables
 integer  :: s1
 character (len = 250 )            :: ICfile 
 logical,parameter                 :: debug=.false.

 if (debug) call check_point('write_flow_big#1',.false.)


 err1=0

 ICfile=flowDIR
 call concat(ICfile,basename)
 call concati(ICfile,coords(1))
 call concat(ICfile,'_')
 call concati(ICfile,coords(2))
 call concat(ICfile,'_')
 call concati(ICfile,coords(3))
 call concat(ICfile,'.')
 call concati(ICfile,nstep)
                                                                                                                             
!Direct Access Fortran Binary
 open(310,file=ICfile,form='unformatted',status='new',iostat=s1,action='write')
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR OPENING FILE: "//trim(Icfile)//" IOSTAT=",s1
   err1=1
   goto 2000
  endif
   if (debug) call check_point('write_flow_big#2',.false.)

  write(310) nstep,time,delt,g,rho_0,scal1_0,Re,Pr,Sc
  write(310) sx,ex,sy,ey,sz,ez 
  write(310) u
  write(310) v
  write(310) w
  write(310) p
  write(310) rho
  write(310) scal1
 close(310,iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR CLOSING FILE: "//trim(Icfile)//" IOSTAT=",s1
   err1=1
   goto 2000
  endif

    if (debug) call check_point('write_flow_big#3',.false.)

 err1=max(err1,s1)
 write(IOUT,'(a)') "WRITE OF BIG RESTART :"//trim(ICfile)//" COMPLETED"
 return

 2000 continue
 write(IOUT,'(a)') "WRITE OF BIG RESTART :"//trim(ICfile)//" FAILED"
 return
end subroutine write_flow_big
#endif

subroutine write_flow_small(basename,stat)
!@t
! \textbf{subroutine write\_flow\_small(basename,stat)}
!@h
!   Description:
!     Writes one large restart file for multiple processors.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use ntypes, only: r8
 use Flow
 use Domain
 use IO, only: IOUT, flowDIR
 use Parameters, only: Re,Pr,Sc,g,rho_0,scal1_0,time,nstep,delt 
#ifdef PARALLEL
 use dd, only: comm3d, myid, MPI_STATUS_SIZE
#endif
 implicit none

 !Passed Variables
 integer,intent(out)                   :: stat
 character(len=*),intent(in)           :: basename

 !Local Variables
 real(r8),allocatable,dimension(:,:,:) :: uF, tmp1
 character(len=250)                     :: ICfile
 integer                               :: s1, err
 logical,parameter                     :: debug=.false.

#ifdef PARALLEL
 stat=0
 err=0
 s1=0
   if (debug) call check_point('write_flow_small#mpi_1',.false.)

 if (myid.eq.0) then
  call deallocate_temps(err)
  if (err.NE.0) then
   write(IOUT,'(a)') "ERROR: DEALLOCATION OF TEMPS IN `write_flow` FAILED"
   stat=1
   goto 1000
  endif
  allocate( uF(1:nxp2,1:nyp2,1:nzp2), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR: Allocating full field, stat:",s1
   stat=2
   goto 1000
  endif

  allocate( tmp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR: Allocating temp field, stat:",s1
   stat=2
   goto 1000
  endif

  ICfile=flowDIR
  call concat(ICfile,basename)
  call concati(ICfile,nstep)
  open(310,file=ICfile,form='unformatted',status='unknown',iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR OPENING FILE: "//trim(ICfile)//" IOSTAT: ",s1
   stat=3
   goto 1000
  endif
  write(310) nstep,time,delt,g,rho_0,scal1_0,Re,Pr,Sc
  write(310) nxp2, nyp2, nzp2 
 endif
   if (debug) call check_point('write_flow_small#mpi_2',.false.)

 !Gather U
  if (myid.eq.0) then
   call Gather3dM(u,uF,tmp1,0,err)
  else
   call Gather3dS(u,0,err)
  endif
   call MPI_BARRIER(comm3d,err)
   if (debug) call check_point('write_flow_small#mpi_3',.false.)

 !Write U
  if(myid.eq.0) then 
   write(310) uF 
  endif
   call MPI_BARRIER(comm3d,err)
   if (debug) call check_point('write_flow_small#mpi_4',.false.)

 !Gather V
  if (myid.eq.0) then
   call Gather3dM(v,uF,tmp1,0,err)
  else
   call Gather3dS(v,0,err)
  endif
   call MPI_BARRIER(comm3d,err)

 !Write V
  if(myid.eq.0) then
   write(310) uF 
 endif
   call MPI_BARRIER(comm3d,err)

 !Gather W
  if (myid.eq.0) then
   call Gather3dM(w,uF,tmp1,0,err)
  else
   call Gather3dS(w,0,err)
  endif
   call MPI_BARRIER(comm3d,err)
 !Write W
   if(myid.eq.0) then
    write(310) uF 
   endif

 !Gather P
  if (myid.eq.0) then
   call Gather3dM(p,uF,tmp1,0,err)
  else
   call Gather3dS(p,0,err)
  endif
   call MPI_BARRIER(comm3d,err)

 !Write P
   if(myid.eq.0) then
    write(310) uF 
   endif

 !Gather rho
  if (myid.eq.0) then
   call Gather3dM(rho,uF,tmp1,0,err)
  else
   call Gather3dS(rho,0,err)
  endif
  call MPI_BARRIER(comm3d,err)

 !Write rho 
   if(myid.eq.0) then 
    write(310) uF
   endif
  call MPI_BARRIER(comm3d,err)

 !Gather scal1
  if (myid.eq.0) then
   call Gather3dM(scal1,uF,tmp1,0,err)
  else
   call Gather3dS(scal1,0,err)
  endif
  call MPI_BARRIER(comm3d,err)

 !Write scal1 
   if(myid.eq.0) then 
    write(310) uF
   endif
  call MPI_BARRIER(comm3d,err)

 !deallocate/reallocate variables
  if(myid.eq.0) then
   deallocate( uF, stat=s1 )
   if (s1.NE.0) then
    write(IOUT,'(a,i4)') "ERROR: Deallocating full field, stat:",s1
    goto 1000
   endif

   deallocate( tmp1, stat=s1 )
   if (s1.NE.0) then
    write(IOUT,'(a,i4)') "ERROR: Deallocating tmp1, stat:",s1
    goto 1000
   endif
   call allocate_temps(err)
   if (err.NE.0) then
    write(IOUT,'(a)') "ERROR ALLOCATING TEMPS IN `write_flow` FATAL!"
    stat=4
    goto 2000
   endif
    close(310)
    write(IOUT,'(a)') "WRITE OF SMALL RESTART :"//trim(ICfile)//" COMPLETED"
   endif

   call MPI_BARRIER(comm3d,err)
  
#else
 err=0
 ICfile=flowDIR
 call concat(ICfile,basename)
 call concati(ICfile,nstep)
  open(310,file=ICfile,form='unformatted',status='unknown',iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR OPENING FILE: "//trim(ICfile)//" IOSTAT: ",s1
   stat=3
   goto 1000
  endif
  write(310) nstep,time,delt,g,rho_0,scal1_0,Re,Pr,Sc
  write(310) nxp2, nyp2, nzp2 
  write(310) u
  write(310) v
  write(310) w
  write(310) p
  write(310) rho
  write(310) scal1
 close(310,iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a,i4)') "ERROR CLOSING FILE: "//trim(ICfile)//" IOSTAT: ",s1
   stat=4
   goto 1000
  endif

#endif

 stat=max(err,s1)
 1000 continue
 return
 2000 continue 
 write(IOUT,'(a,i4)') "WRITE OF SMALL RESTART :"//trim(ICfile)//" FAILED STAT: ",s1
 return
end subroutine write_flow_small
