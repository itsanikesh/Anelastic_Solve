subroutine read_inputptest(infile,stat)
!@t
! \textbf{subroutine read\_inputptest(infile,stat)}
!@h
!   Description:
!     Read input values from the ptest.ini file.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History:
!     Version   Date     Comment 
!     -------   ----     -------  
!     1.0       05/2009  Original code. [Matt de Stadler] 
!@h
!   Comments: 
!     This is called as part of the pressure test program. It is based on
!     dns/input.F90. 
!@q

 use ntypes, only: r8,i4
 use Domain
 use Grid
 use IO
 use Parameters
 use ptester
#ifdef PARALLEL
 use dd, only: nxprocs,nyprocs,nzprocs,myid,commx1x2x3
#endif
 implicit none

!Passed Variables
 character(len=*),intent(in) :: infile
 integer(i4),intent(out)     :: stat

!Local Variables
 integer                     :: s1, idum, idumVar(1:100), err1
 character(len=5),parameter  :: defval='ERROR'
 character(len=50)           :: block
 integer                     :: IOUTL

 logical :: debug = .false.

 err1=0

 if (debug) then
  IOUTL=IOUT !output to screen or output file
 else
  IOUTL=0 !redirect to stderr
 endif

!**********************************************************
!*******FILE,Block,VarName,Value,defaultValue,IOUTL*********
!**********************************************************
![Output]
 block='Output'
 call scaninichar(infile,block,'PenDIR',penDIR,defval,IOUTL)
 call scaninichar(infile,block,'PlnDIR',plnDIR,defval,IOUTL)
 call scaninichar(infile,block,'GridDIR',gridDIR,defval,IOUTL)
 call scaninichar(infile,block,'MGDIR',MGDIR,defval,IOUTL)

 call scaninichar(infile,block,'FileExt',ext,defval,IOUTL)
 call scaninilogical(infile,block,'WriteGrid',write_grid,defval,IOUTL)

![Domain]
 block='Domain'
 call scaniniint(infile,block,'NX',nx,defval,IOUTL)
 call scaniniint(infile,block,'NY',ny,defval,IOUTL)
 call scaniniint(infile,block,'NZ',nz,defval,IOUTL)

![TestCase]
 block='TestCase'
 call scaninichar(infile,block,'testcase',testcase,defval,IOUTL)
 call scaninichar(infile,block,'testdir1D',testdir1D,defval,IOUTL)
 call scaninilogical(infile,block,'srcflucs',srcflucs,defval,IOUTL)
 call scaninilogical(infile,block,'phiflucs',phiflucs,defval,IOUTL)

#ifdef PARALLEL
![Decomposition]
 block='Decomposition'
 call scaniniint(infile,block,'X1Procs',nxprocs,defval,IOUTL)
 call scaniniint(infile,block,'X2Procs',nyprocs,defval,IOUTL)
 call scaniniint(infile,block,'X3Procs',nzprocs,defval,IOUTL)
#endif

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
 nxp2=nx+2
 nyp2=ny+2
 nzp2=nz+2

#ifdef PARALLEL
 call MPI_BARRIER(commx1x2x3,err1)
#endif
 stat = err1 
 write(IOUTL,'(a)') "READ OF PARAMETERS COMPLETED"

return
end subroutine read_inputptest
