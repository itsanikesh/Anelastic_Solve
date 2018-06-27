program Sturb
!@t
! \textbf{program Sturb}
!@h
!   Description:
!     Main program for solving the time dependent Navier-Stokes equations
!     using the Boussinesq approximation.

!   Method:
!     Uses a staggered-grid finite-volume method where vector quantities
!     Are stored at cell faces and scalar quantities are stored at the
!     grid centers. The spatial derivatives are evaluated using a second
!     order central difference stencil and the temporal integration is 
!     performed with the low-storage explicit 3rd-order Runge-Kutta 
!     method of Williamson (1980). A parallel multi-grid solver with 
!     variable coefficients is used to solve the Poisson pressure 
!     equation.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!     2.0       05/2009  Major Update.  [Kyle A. Brucker]

!   Language:
!     Fortran 90
!@h
!   Comments:
!     A sponge region near the boundaries is available to control spurious
!     reflections from internal waves and other disturbances propagating
!     out of the domain.
!@q

 !Program
 use ntypes, only: i4, r8
 use FLOW,   only: u,v,w,p,rho,scal1,densityBG
 use IO
 use domain, only: EU,EV,EW
#ifdef PARALLEL
 use dd
#else
 use domain, only: sx,ex,cex,sy,ey,sz,ez,nx,ny,nz,EU,EV,EW
#endif
 use Parameters
 use Coriolis_Params
 implicit none

 character(len=25),parameter :: iniFILE='Sturb.ini'
 real(r8)                    :: time1,time2
#ifndef PARALLEL
 real(4)                     :: time14, eval(2), etime
 integer                     :: myid
#endif
 logical                     :: cfile
 integer(i4)                 :: ok1,ok2

#ifdef PARALLEL
 !INITIALIZE MPI
 call MPI_INIT(ok1)
  call MPI_BARRIER(commx1x2x3,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 call MPI_COMM_RANK(commx1x2x3,myid,ok1)
  call MPI_BARRIER(commx1x2x3,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 call MPI_COMM_SIZE(commx1x2x3,sizex1x2x3,ok1)
  call MPI_BARRIER(commx1x2x3,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !CALCULATE INITIAL TIME
 time1 = MPI_WTIME()
#else
 myid=0
 time1 = 0.d0 ! is this valid? the form below is more consistent.
! time14 = etime(eval)
! time2 = dble(time14)
#endif

 !INITIALIZE IO TO SCREEN OR LOG FILE
 call initIO(myid,iniFILE,ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !READ PARAMETERS 
 call read_input(iniFILE,ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

#ifdef PARALLEL 
 !SETUP MPI TOPOLOGY
 call mpi_setup(ok1) 
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999
#else
 sx=2
 ex=nx+1
 sy=2
 ey=ny+1
 sz=2
 ez=nz+1
 cex=(ex-sx)/2+2
 EU=1
 EV=1
 EW=1
#endif
 EU=0
 EV=0
 EW=0

 !SET TIME MARCHING SCHEME
   RK3=.false.
   RK3CN=.true.
   pcorrect=.false.
   LESflag=1.d0
   Texp=0.0d-4
   Scon=7.6d-4 

 !ALLOCATE FLOW VARIABLES
 call allocation(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !READ GRID
 call grid_setup(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !SETUP BOUNDARY CONDITIONS
 call bc(iniFILE,ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !SET MULTIGRID PARAMETERS FROM INPUT FILE
 call mg_read_params(iniFILE,ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !INITIALIZE MULTIGRID
 call mg_init(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !CREATE FLOW 
 if (.not. restart) then   
  call initialize(ok1)
   call check(ok1,ok2)
   if (ok1.NE.0.or.ok2.NE.0) goto 9999
 endif

 !ALLOCATE LES VARIABLES
 call allocation_LES(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !RESTART FROM SAVED STATE 
 if (restart) then
   call restartSIM(ok1) 
   call check(ok1,ok2)
   if (ok1.NE.0.or.ok2.NE.0) goto 9999
  !SET INITIAL DENSITY FIELD
!  rho = 0.d0
!  call add_density(ok1)
!  call check(ok1,ok2)
!   if (ok1.NE.0.or.ok2.NE.0) goto 9999
 !SET INITIAL SCALAR1 FIELD
!  scal1 = 0.d0
!  call add_scalar1(ok1)
!  call check(ok1,ok2)
!  if (ok1.NE.0.or.ok2.NE.0) goto 9999
  !SET INITIAL BACGROUND DENSITY FIELD
!  densityBG = 0.d0
  call set_densityBG(ok1)
!  call check(ok1,ok2)
!  if (ok1.NE.0.or.ok2.NE.0) goto 9999
   
 endif

!why again? already done this in initialize above
!set rho' to zero
! if (.not. restart) then
!  rho=0.d0
!  scal1=0.d0
!  p=0.d0
!  call add_density(ok1)
!  call check(ok1,ok2)
!  if (ok1.NE.0.or.ok2.NE.0) goto 9999
!  call add_scalar1(ok1)
!  call check(ok1,ok2)
!  if (ok1.NE.0.or.ok2.NE.0) goto 9999
!  call add_mean(ok1)
!  call check(ok1,ok2)
!  if (ok1.NE.0.or.ok2.NE.0) goto 9999


! endif

 !WRITE OUT RUN PARAMETERS AND GO
 write(IOUT,'(a)') "***********************************************************"
 write(IOUT,'(a)') "***************************BEGIN***************************"
 write(IOUT,'(a)') "***********************************************************"
 write(IOUT,'(a)')         "FLOW TYPE       = "//trim(flow_type)
 write(IOUT,'(a)')         "DENSITY PROFILE = "//trim(density_profile)
 write(IOUT,'(a17,f15.8)') "CFL             = ",CFL
 write(IOUT,'(a17,f15.8)') "MAX dt          = ",dtmax
 write(IOUT,'(a17,e15.8)') "REYNOLDS NUMBER = ",Re
 write(IOUT,'(a17,f15.8)') "PRANDTL NUMBER  = ",Pr
 write(IOUT,'(a17,f15.8)') "SCHMIDT NUMBER  = ",Sc
 write(IOUT,'(a17,f15.8)') "VISCOSITY       = ",rRe
 write(IOUT,'(a17,f15.8)') "RHO DIFF        = ",rRe/Pr
 write(IOUT,'(a17,f15.8)') "SCAL1 DIFF      = ",rRe/Sc
 write(IOUT,'(a17,f15.8)') "GRAVITY         = ",g
 write(IOUT,'(a17,f15.8)') "Lattitude       = ",lat
 write(IOUT,'(a17,f15.8)') "F-Plane         = ",fPln
 write(IOUT,'(a17,f15.8)') "Ft-Plane        = ",ftPln
 write(IOUT,'(a17,f15.8)') "B-Plane         = ",bPln
 write(IOUT,'(a17,f15.8)') "REF DENSITY     = ",rho_0
 write(IOUT,'(a17,f15.8)') "REF SCALAR1     = ",scal1_0
 write(IOUT,'(a17,f15.8)') "STARTING TIME   = ",time
 write(IOUT,'(a17,i5)')    "STARTING STEP   = ",bstep
 write(IOUT,'(a17,i5)')    "ENDING STEP     = ",estep
 write(IOUT,'(a17,i5)')    "stat start int  = ",szint
 write(IOUT,'(a17,i5)')    "stat end int    = ",ezint

 !GHOST VARIABLES SERIAL VERSION GHOST=APPLY BCS
 call ghost(u,'u',ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 call ghost(v,'v',ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 call ghost(w,'w',ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 call ghost(p,'p',ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 call ghost(rho,'rho',ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 call ghost(scal1,'scal1',ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !SETUP SPONGE REGION WHICH DEPENDS ON ICS/BCS
 !MUST OCCUR AFTER GHOSTING TO ENSURE THE FIELD 
 !THE SPONGE IS DAMPING TOWARDS IS THE ACTUAL t=t0
 !FIELD
 call sponge_setup(iniFILE,ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999
 
 !COMPUTE INITIAL STATISTICS
  if (init_stats) call statistics(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 if (init_stats) call statistics_small(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !SKIP SOLVER IF IC ONLY
 if (iconly) goto 8888

 !SET PRESSURE TO ZERO
 if (.not.restart .and. pzero) p(:,:,:) = 0.d0 

 !MAIN NAVIER-STOKES SOLVER
 call NSsolver(ok1)
  call check(ok1,ok2)
  if (ok1.NE.0.or.ok2.NE.0) goto 9999

 !CALCULATE RUN TIME
#ifdef PARALLEL
 time2 = MPI_WTIME()
#else
 time14 = etime(eval) 
 time2=dble(time14)
#endif 
 !CLEAN EXIT
 8888 continue
 call mg_end(ok1)
  if (ok1.NE.0) goto 9999

 write(IOUT,'(a)') 
 write(IOUT,'(a)') "***********************************************************"
 write(IOUT,'(a)') "*************************FINISHED**************************"
 write(IOUT,'(a)') "***********************************************************"
 write(IOUT,'(a)') 
 write(IOUT,'(a)') 
 if( .not. iconly ) write(IOUT,'(i5,a,f15.4)') estep-bstep+1," Iterations completed in (wall time): ",time2-time1

 inquire(unit=IOUT,opened=cfile)
 if (cfile) close(IOUT)
#ifdef PARALLEL
 call MPI_FINALIZE(ok1)
#endif 

 stop 

 !UNCLEAN EXIT
 9999 continue
 write(6,*)    'ERROR ABORTING, ok1, ok2 ', ok1, ok2
 write(IOUT,*) 'ERROR ABORTING, ok1, ok2 ', ok1, ok2
 inquire(unit=IOUT,opened=cfile)
 if (IOUT.NE.6.AND.cfile) close(IOUT)
 call mg_end(ok1)
#ifdef PARALLEL
 call MPI_FINALIZE(ok1)
#endif
 stop !Not Clean Exit

end program Sturb

subroutine initIO(myid,infile,stat)
!@t
! \textbf{subroutine initIO(myid,infile,stat)}
!@h
!   Description:
!     Initialize file input and output.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: i4
 use IO, only: IOUT
 implicit none

 !Passed Variables
 integer(i4),intent(in)  :: myid
 character(len=*)        :: infile
 integer(i4),intent(out) :: stat
 !Local Variables
 integer(i4)        :: IOUT_MASTER, IOUT_SLAVE
 character(len=100) :: tdir,outfile
 integer            :: s1

 stat=0
 s1=0

  tdir='./'
  call scaniniint(infile,'IO','IOUT_MASTER',IOUT_MASTER,'ERROR',0)
  call scaniniint(infile,'IO','IOUT_SLAVE',IOUT_SLAVE,'ERROR',0)
  call scaninichar(infile,'Output','ResultDIR',tdir,'ERROR',0)

  !If IOUT NE to 6 (screen output) Open file for output of messages
  if (myid.EQ.0) IOUT=IOUT_MASTER
  if (myid.NE.0) IOUT=IOUT_SLAVE

  if (IOUT_SLAVE.NE.6.AND.IOUT_SLAVE.NE.0.AND.IOUT_SLAVE.NE.-1) then
!  if (IOUT.NE.6.AND.IOUT.NE.0) then
   open(IOUT,file=outfile,status='unknown',form='formatted',iostat=s1)
   write(outfile,'(a,i5.5,a)') trim(tdir)//"node_",myid,".log"
   if (s1.NE.0) then
    write(6,'(3(a,i4))') "ERROR: Cannot open output file, iostat: ",s1," on node: ",myid," with IOSTAT=",s1
   endif
  endif
 stat=max(stat,s1)
 return
end subroutine initIO


subroutine restartSIM(stat)
!@t
! \textbf{subroutine restartSIM(stat)}
!@h
!   Description:
!     Restart a simulation.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
 use Flow, only: Vmodel, Rmodel, Smodel
 use LESmod
 use Domain,     only: sz,ez,nzp2
 use grid,       only: zc
 use IO
 use ntypes, only: i4,r8
 use parameters, only: rho_0,g,rhoMIN,rhoMAX,density_profile,denP1,denP2,denP3,DX3c,clip
 use parameters, only: scal1_0,scal1MIN,scal1MAX,scal1_profile,scal1P1,scal1P2,scal1P3,scal1X3c
 use parameters, only: bstep,estep,nstep,rest_file_load,rest_iter,relax_time
 implicit none

 !Passed Variables
 integer(i4) :: stat

 !Local Variables
 integer(i4) :: iErestart
 real(r8)    :: zz
 real(r8)    :: Um,Uu,Ul,Ud,Zmu,Zul,Zld,Dmu,Dul,Dld

 call read_flow(rest_file_load,rest_iter,iErestart)

  bstep=nstep+1
  estep=estep+nstep
 !STRAT RESTART NEED TO DO THE FOLLOWING AFTER INTERPOLATION AND 
 !MOST LIKELY AFTER ANY MODIFICATION TO THE VELOCITY FIELD.
 !HERE RELAX TIME MEANS TIME FOR WHICH g=0, AFTER WHICH
 !RHO=<rho> 
 if (relax_time.GT.0.d0) call relax_ics(iErestart) 

 select case (Vmodel)
  case('DNS')
   write(IOUT,*)"Vmodel = DNS"
  case('SSM')
   if (nstep.GT.les_start) then
    write(IOUT,*)"Vmodel = SSM on"
    call SSM
    call SGSdiff(stat)
   endif
  case('DSM')
   if (nstep.GT.les_start) then
    write(IOUT,*)"Vmodel = DSM on"
    call DSM
    call SGSdiff(stat)
   endif
  case('DMM')
   if (nstep.GT.les_start) then
    write(IOUT,*)"Vmodel = DMM on"
    call DMM
    call SGSdiff(stat)
   endif
  case DEFAULT
   write(IOUT,'(a)') "ABORTING restartSIM: "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
 end select

 select case (Rmodel)
  case('DNSrho')
   write(IOUT,*)"Rmodel = DNSrho"
  case('SSMrho')
   if (nstep.GT.les_start) then
    write(IOUT,*)"Rmodel = SSMrho on"
    call SSMrho
   endif
  case('DSMrho')
   if (nstep.GT.les_start) then
    write(IOUT,*)"Rmodel = DSMrho on"
    call DSMrho
   endif
  case DEFAULT
   write(IOUT,'(a)') "ABORTING NS solver: "//trim(Rmodel)//" NOT IMPLEMENTED"
   stat=1
 end select
 
select case (Smodel)
  case('DNSscal1')
   write(IOUT,*)"Smodel = DNSscal1"
  case('SSMscal1')
   if (nstep.GT.les_start) then
    write(IOUT,*)"Smodel = SSMscal1 on"
    call SSMscal1
   endif
  case('DSMscal1')
   if (nstep.GT.les_start) then
    write(IOUT,*)"Smodel = DSMscal1 on"
    call DSMscal1
   endif
  case DEFAULT
   write(IOUT,'(a)') "ABORTING NS solver: "//trim(Smodel)//" NOT IMPLEMENTED"
   stat=1
 end select


  select case(density_profile)
   case('Linear')
    if (clip) then
     rhoMin=denP1*zc(nzp2)
     rhoMax=denP1*zc(1)
     write(IOUT,'(a,2(1x,f12.6))') "     TWO LAYER RHO MIN/MAX USED FOR CLIPPING: ",rhoMIN,rhoMAX
    endif
   case('TwoLayer')
    if (clip) then
     rhoMin=rho_0-0.5d0*denP1
     rhoMax=rho_0+0.5d0*denP1
     write(IOUT,'(a,2(1x,f12.6))') "     TWO LAYER RHO MIN/MAX USED FOR CLIPPING: ",rhoMIN,rhoMAX
    endif
   case('TwoLayerX')
    if (clip) then
     rhoMin=rho_0
     rhoMax=rho_0+denP1
     write(IOUT,'(a,2(1x,f12.6))') "     TWO LAYER RHO MIN/MAX USED FOR CLIPPING: ",rhoMIN,rhoMAX
    endif
   case('Temperature')
    if (clip) then
     Um=0.0;Uu=denP1;Ul=Uu;Ud=Uu;
     Zmu=-1;Zul=-5.d0;Zld=-10.d0;
     Dmu=0.1d0;Dul=0.1;Dld=0.1;
     zz=zc(nzp2)
     rhoMin=0.d0+(Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                       +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dmu*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld)));

     zz=zc(1)
     rhoMax=0.d0+(Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dmu*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld)));

    write(IOUT,'(a,2(1x,f12.6))') "     Temperature RHO MIN/MAX USED FOR CLIPPING: ",rhoMIN,rhoMAX
    endif
   case('Jdeep')
    if (clip) then
    Um=-denP1;Uu=-denP2;Ul=-denP2;Ud=-denP2;
    Zmu=0.d0;Zul=-5.d0;Zld=-10.d0;
    Dmu=0.1d0;Dul=0.1d0;Dld=0.1d0;
    zz=zc(nzp2)
    rhoMin=1.d0 + 1.0/9.81*((Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))
    zz=zc(1)
    rhoMax=1.d0 + 1.0/9.81*((Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))
    write(IOUT,'(a,2(1x,f12.6))') "     JDEEP RHO MIN/MAX USED FOR CLIPPING: ",rhoMIN,rhoMAX
   endif

   case DEFAULT
    write(IOUT,'(a)') "ABORTING SETTING RHO MAX/MIN for CLIPPING - "//trim(density_profile)//" NOT IMPLEMENTED"
    stat=1
   return
  end select

  select case(scal1_profile)
   case('Linear')
   if (clip) then
     scal1Min=scal1P1*zc(nzp2)
     scal1Max=scal1P1*zc(1)
    write(IOUT,'(a,2(1x,f12.6))') "     LINEAR SCAL1 MIN/MAX USED FOR CLIPPING:",scal1MIN,scal1MAX
   endif
   case('TwoLayer')
    if (clip) then
     scal1Min=0.d0*scal1P1
     scal1Max=scal1P1
     write(IOUT,'(a,2(1x,f12.6))') "   TWO_LAYER_Z SCAL1 MIN/MAX USED FOR SCAL1 CLIPPING: ",scal1MIN,scal1MAX
    endif
   case('TwoLayerX')
    if (clip) then
     scal1Min=rho_0
     scal1Max=rho_0+scal1P1
     write(IOUT,'(a,2(1x,f12.6))') "   TWO_LAYER_X SCAL1 MIN/MAX USED FOR SCAL1 CLIPPING: ",scal1MIN,scal1MAX
    endif
   case('Salinity')
    if (clip) then
     Um=0.d0;Uu=scal1P2;Ul=Uu;Ud=Uu;
     Zmu=-1.d0;Zul=-5.d0;Zld=-10.d0;
     Dmu=0.1d0;Dul=0.1;Dld=0.1;
     zz=zc(nzp2)
     scal1Min=((Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))
     zz=zc(1)
     scal1Max=((Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))

     write(IOUT,'(a,2(1x,f12.6))') "   SAINITY: SCAL1 MIN/MAX USED FOR SCAL1 CLIPPING: " 
    endif
   case('Jdeep')
    if (clip) then
    Um=-scal1P1;Uu=-scal1P2;Ul=-scal1P2;Ud=-scal1P2;
    Zmu=0.0;Zul=-5.d0;Zld=-10.d0;
    Dmu=0.1;Dul=0.1;Dld=0.1;
    zz=zc(nzp2)
    scal1Min=1.d0 + 1.0/9.81*((Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))
    zz=zc(1)
    scal1Max=1.d0 + 1.0/9.81*((Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))
    write(IOUT,'(a,2(1x,f12.6))') "     JDEEP SCAL1 MIN/MAX USED FOR CLIPPING: ",scal1MIN,scal1MAX
   endif

   case DEFAULT
    write(IOUT,'(a)') "ABORTING SETTING SCAL1 MAX/MIN for CLIPPING - "//trim(scal1_profile)//" NOT IMPLEMENTED"
    stat=1
   return
  end select

stat=iErestart
return
end subroutine restartSIM

subroutine check(ecode1,ecode2)
!@t
! \textbf{subroutine check(ecode1,ecode2)}
!@h
!   Description:
!     Use an MPI Barrier if parallel.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

#ifdef PARALLEL
 use dd, only: commx1x2x3 
#endif
 implicit none

 integer,intent(IN)  :: ecode1
 integer,intent(OUT) :: ecode2

 ecode2 = 0
#ifdef PARALLEL
  call MPI_BARRIER(commx1x2x3,ecode2)
#endif
return
end subroutine check
