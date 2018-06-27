












subroutine read_input(infile,stat)
!@t
! \textbf{subroutine read\_input(infile,stat)}
!@h
!   Description:
!     Reads input values from a .ini file.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use Domain
 use Grid
 use IO
 use Parameters
 use Coriolis_params
 use Langmuir_params
 use Flow, only: Vmodel, Rmodel, Smodel
 use LESmod 
 implicit none

!Passed Variables
 character(len=*),intent(in) :: infile
 integer(i4),intent(out)     :: stat

!Local Variables
 integer                     :: s1, idum, idumVar(1:100), err1
 character(len=5),parameter  :: defval='ERROR'
 character(len=50)           :: block
 integer                     :: IOUTL

 logical :: debug=.false.

 err1=0

 if (debug) then
  IOUTL=IOUT !output to screen or output file
 else
  IOUTL=0 !redirect to stderr
 endif

!**********************************************************
!*******FILE,Block,VarName,Value,defaultValue,IOUTL*********
!**********************************************************
![Restart]
 block='Restart'
 call scaninilogical(infile,block,'Restart',restart,defval,IOUTL)
 call scaninichar(infile,block,'Fnametowrite',rest_file_write,defval,IOUTL)
 if (restart) then
 call scaninichar(infile,block,'Fnametoload',rest_file_load,defval,IOUTL)
 call scaniniint(infile,block,'iter',rest_iter,defval,IOUTL)
 endif
 icparam=.false. !if true the following will be replaced with the values in the dump file
 if (restart) icparam=.true.!and no initial conditions will be set 


![Parameters]
 block='Parameters'
 call scaninichar(infile,block,'Flow',flow_type,defval,IOUTL)
 call scaninichar(infile,block,'Strat',density_profile,defval,IOUTL)
 call scaninichar(infile,block,'Scal1',scal1_profile,defval,IOUTL)
 call scaninireal(infile,block,'Reynolds',Re,defval,IOUTL)
 call scaninireal(infile,block,'Prandtl',Pr,defval,IOUTL)
 call scaninireal(infile,block,'Schmidt',Sc,defval,IOUTL)
 call scaninireal(infile,block,'CFL',cfl,defval,IOUTL)
 call scaninireal(infile,block,'RefDensity',rho_0,defval,IOUTL)
 call scaninireal(infile,block,'RefScalar1',scal1_0,defval,IOUTL)
 call scaninireal(infile,block,'DeltaT_max',dtmax,defval,IOUTL)
 call scaninilogical(infile,block,'Clip',clip,defval,IOUTL)
 call scaninilogical(infile,block,'IConly',IConly,defval,IOUTL)

![Coriolis]
 block='Coriolis'
 call scaninilogical(infile,block,'Coriolis',coriolis,defval,IOUTL)
 call scaninireal(infile,block,'Lat',lat,defval,IOUTL)
 call scaninireal(infile,block,'fPln',fPln,defval,IOUTL)
 call scaninireal(infile,block,'ftPln',ftPln,defval,IOUTL)
 call scaninireal(infile,block,'bPln',bPln,defval,IOUTL)

![Langmuir]
 block='Langmuir'
 call scaninilogical(infile,block,'Langmuir',langmuir,defval,IOUTL)
 call scaninichar(infile,block,'StokesDrift',drift_type,defval,IOUTL)
 call scaninireal(infile,block,'Us_amp',us_amp,defval,IOUTL)
 call scaninireal(infile,block,'Vs_amp',vs_amp,defval,IOUTL)
 call scaninireal(infile,block,'Drift_wlength',drift_wlength,defval,IOUTL)
 call scaninireal(infile,block,'Drift_wnumber',drift_wnumber,defval,IOUTL)

![Relax]
 block='Relax'
 call scaninireal(infile,block,'RelaxTime',relax_time,defval,IOUTL)
 call scaninilogical(infile,block,'FixMean',RfixMean,defval,IOUTL)
 call scaniniint(infile,block,'PreStats',wpre_stats,defval,IOUTL)

!Model
 block='Model'
 call scaninichar(infile,block,'Vel_model',Vmodel,defval,IOUTL)
 call scaninichar(infile,block,'Rho_model',Rmodel,defval,IOUTL)
 call scaninichar(infile,block,'Scal1_model',Smodel,defval,IOUTL)
 call scaninireal(infile,block,'SSM_const',SSM_const,defval,IOUTL)
 call scaninireal(infile,block,'Pr_eddy',Pr_sgs,defval,IOUTL)
 call scaninireal(infile,block,'Sc_eddy',Sc_sgs,defval,IOUTL)
 call scaninireal(infile,block,'TestGrid_ratio',r_dgt_dg,defval,IOUTL)
 call scaniniint(infile,block,'xfilter',xfil,defval,IOUTL)
 call scaniniint(infile,block,'yfilter',yfil,defval,IOUTL)
 call scaniniint(infile,block,'zfilter',zfil,defval,IOUTL)
 call scaninichar(infile,block,'Grid_size',les_grid_size,defval,IOUTL)
 call scaniniint(infile,block,'les_start_step',les_start,defval,IOUTL)
 call scaniniint(infile,block,'les_call_freq',les_freq,defval,IOUTL)


![Gravity]
 block='Gravity'
 call scaninireal(infile,block,'Ginit',g,defval,IOUTL)
 call scaninireal(infile,block,'Gfinal',g_orig,defval,IOUTL)
 call scaninireal(infile,block,'GT1',gt1,defval,IOUTL)
 call scaninireal(infile,block,'GT2',gt2,defval,IOUTL)

![InitPressure]
 block='InitPressure'
 call scaninilogical(infile,block,'Pzero',pzero,defval,IOUTL)


![Mean]
 block='Mean'
 call scaninireal(infile,block,'Param1',MeanP1,defval,IOUTL)
 call scaninireal(infile,block,'Param2',MeanP2,defval,IOUTL)
 call scaninireal(infile,block,'Param3',MeanP3,defval,IOUTL)
 call scaninireal(infile,block,'Param4',MeanP4,defval,IOUTL)
 call scaninireal(infile,block,'X1center',MX1c,defval,IOUTL)
 call scaninireal(infile,block,'X2center',MX2c,defval,IOUTL)
 call scaninireal(infile,block,'X3center',MX3c,defval,IOUTL)

![Density]
 block='Density'
 call scaninireal(infile,block,'Param1',denP1,defval,IOUTL)
 call scaninireal(infile,block,'Param2',denP2,defval,IOUTL)
 call scaninireal(infile,block,'Param3',denP3,defval,IOUTL)
 call scaninireal(infile,block,'X3center',DX3c,defval,IOUTL)

![Scalar1]
 block='Scalar1'
 call scaninireal(infile,block,'Param1',scal1P1,defval,IOUTL)
 call scaninireal(infile,block,'Param2',scal1P2,defval,IOUTL)
 call scaninireal(infile,block,'Param3',scal1P3,defval,IOUTL)
 call scaninireal(infile,block,'X3center',scal1X3c,defval,IOUTL)

![BroadBand]
  block='BroadBand'
  call scaninireal(infile,block,'Amplify',cropP1,defval,IOUTL)
  call scaninireal(infile,block,'Radius',cropP2,defval,IOUTL)
  call scaninilogical(infile,block,'turbICS',turbICS,defval,IOUTL)

![ISO]
  block='ISO'
  call scaninireal(infile,block,'K0',k0,defval,IOUTL)
  call scaninireal(infile,block,'K1',k1,defval,IOUTL)
  call scaninireal(infile,block,'Eps0',eps0,defval,IOUTL)
  call scaniniint(infile,block,'Spectrum',bbspect,defval,IOUTL)

![Output]
 block='Output'
 call scaninichar(infile,block,'ResultDIR',resultDIR,defval,IOUTL)
 call scaninichar(infile,block,'TempDIR',tempDIR,defval,IOUTL)
 call scaninichar(infile,block,'PenDIR',penDIR,defval,IOUTL)
 call scaninichar(infile,block,'PlnDIR',plnDIR,defval,IOUTL)
 call scaninichar(infile,block,'FlowDIR',flowDIR,defval,IOUTL)
 call scaninichar(infile,block,'StatDIR',statDIR,defval,IOUTL)
 call scaninichar(infile,block,'GridDIR',gridDIR,defval,IOUTL)
 call scaninichar(infile,block,'MGDIR',MGDIR,defval,IOUTL)
 call scaninichar(infile,block,'RelaxDIR',RelaxDIR,defval,IOUTL)
 call scaninilogical(infile,block,'WriteSponge',write_sponge,defval,IOUTL)
 call scaninichar(infile,block,'FileExt',ext,defval,IOUTL)
! call scaninilogical(infile,block,'WriteGrid',write_grid,defval,IOUTL)
! call scaninilogical(infile,block,'GridWeights',write_weights,defval,IOUTL)
 call scaninilogical(infile,block,'TKstat',tkstat,defval,IOUTL)
 call scaninilogical(infile,block,'StatBIN',statbin,defval,IOUTL)
 call scaninilogical(infile,block,'BigIO',IOBig,defval,IOUTL)

![Domain]
 block='Domain'
 call scaniniint(infile,block,'NX',nx,defval,IOUTL)
 call scaniniint(infile,block,'NY',ny,defval,IOUTL)
 call scaniniint(infile,block,'NZ',nz,defval,IOUTL)
! call scaninilogical(infile,block,'XGridread',readGrid(1),defval,IOUTL)
! call scaninilogical(infile,block,'YGridread',readGrid(2),defval,IOUTL)
! call scaninilogical(infile,block,'ZGridread',readGrid(3),defval,IOUTL)


![Iteration]
 block='Iteration'
 call scaniniint(infile,block,'Start',bstep,defval,IOUTL)
 call scaniniint(infile,block,'End',estep,defval,IOUTL)
 call scaniniint(infile,block,'CheckDIV',checkdiv,defval,IOUTL)
 call scaniniint(infile,block,'Stats',wstats,defval,IOUTL)
 call scaniniint(infile,block,'StatsSmall',wstats_small,defval,IOUTL)
 call scaniniint(infile,block,'szint',szint,defval,IOUTL)
 call scaniniint(infile,block,'ezint',ezint,defval,IOUTL)
 call scaniniint(infile,block,'Restart',wflow,defval,IOUTL)
 call scaniniint(infile,block,'Planes',wplanes,defval,IOUTL)
 call scaniniint(infile,block,'Pencils',wpencils,defval,IOUTL)
 call scaninilogical(infile,block,'FirstStat',init_stats,defval,IOUTL)

![Planes]
 !CALL scaninimint first with a dummy variable to get niplanes
 !then allocate and recall if niplane.GT.0
  !niplanes now has the number of planes that we want... allocate an array and recall

!iplanes
 idum=-1
 call scaninimint(infile,'Planes','iPlanes',idumVar,idum,defval,IOUTL)
 niplanes=idum
 if (niplanes.GT.0) then
  allocate( iplanes(1:niplanes),stat=s1 )
  call scaninimint(infile,'Planes','iPlanes',iplanes,niplanes,defval,IOUTL)
 endif
!jplanes
 idum=-1
 call scaninimint(infile,'Planes','jPlanes',idumVar,idum,defval,IOUTL)
 njplanes=idum
 if (njplanes.GT.0) then
  allocate( jplanes(1:njplanes),stat=s1 )
  call scaninimint(infile,'Planes','jPlanes',jplanes,njplanes,defval,IOUTL)
 endif
!kplanes
 idum=-1
 call scaninimint(infile,'Planes','kPlanes',idumVar,idum,defval,IOUTL)
 nkplanes=idum
 if (nkplanes.GT.0) then
  allocate( kplanes(1:nkplanes),stat=s1 )
  call scaninimint(infile,'Planes','kPlanes',kplanes,nkplanes,defval,IOUTL)
 endif

![Lines]
 !CALL scaninimint first with a dummy variable to get nilines
 !then allocate and recall if niplane.GT.0
  !nilines now has the number of lines that we want... allocate an array and recall

!ilines
 idum=-1
 call scaninimint(infile,'Lines','iLines',idumVar,idum,defval,IOUTL)
 nilines=idum
 if (nilines.GT.0) then
  allocate( ilines(1:nilines),stat=s1 )
  call scaninimint(infile,'Lines','iLines',ilines,nilines,defval,IOUTL)
 endif

!jlines
 idum=-1
 call scaninimint(infile,'Lines','jLines',idumVar,idum,defval,IOUTL)
 njlines=idum
 if (njlines.GT.0) then
  allocate( jlines(1:njlines),stat=s1 )
  call scaninimint(infile,'Lines','jLines',jlines,njlines,defval,IOUTL)
 endif

!klines
 idum=-1
 call scaninimint(infile,'Lines','kLines',idumVar,idum,defval,IOUTL)
 nklines=idum
 if (nklines.GT.0) then
  allocate( klines(1:nklines),stat=s1 )
  call scaninimint(infile,'Lines','kLines',klines,nklines,defval,IOUTL)
 endif

!SET THE FOLLOWING
 rRe=1.d0/Re
 rPr=1.d0/Pr
 rSc=1.d0/Sc
 nxp2=nx+2
 nyp2=ny+2
 nzp2=nz+2

 stat = err1 
 write(IOUTL,'(a)') "READ OF PARAMETERS COMPLETED"

return
end subroutine read_input
