












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
 call start_small(in_file,in_iter,ok)

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

 call write_flow_small(out_file,ok)

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
 implicit none

 !Passed Variables
 integer,intent(out)                   :: stat
 character(len=*),intent(in)           :: basename

 !Local Variables
 real(r8),allocatable,dimension(:,:,:) :: uF, tmp1
 character(len=250)                     :: ICfile
 integer                               :: s1, err
 logical,parameter                     :: debug=.false.

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


 stat=max(err,s1)
 1000 continue
 return
 2000 continue 
 write(IOUT,'(a,i4)') "WRITE OF SMALL RESTART :"//trim(ICfile)//" FAILED STAT: ",s1
 return
end subroutine write_flow_small
