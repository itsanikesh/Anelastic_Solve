












subroutine mg_read_params(infile,stat)
!@t
! \textbf{subroutine mg\_read\_params(infile,stat)}
!@h
!   Description:
!     Read parameters for the multigrid program from an ini file.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       03/2009  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use mgVars
 
 implicit none

 !Passed Variables
 integer,intent(out) :: stat
 character(len=*) :: infile

 !Local Variables
 integer                     :: idum,s1
 character(len=5),parameter  :: defval='ERROR'
 character(len=50)           :: block,inBC,BCtype
 integer                     :: IOUT

 logical :: debug = .false.

 stat=0

 if (debug) then
  IOUT=IOUTMG !output to screen or output file
 else
  IOUT=0 !redirect to stderr
 endif
 
![MGsolver]
 block='MGsolver'

 !Smoother Red Black Point Gauss Sidel / Line Jacobi
 call scaninichar(infile,block,'Smoother',Smoother,defval,IOUT)

 !Force mpi boundary data transfer at half-sweeps 
 call scaninilogical(infile,block,'Strict',strict,defval,IOUT)

 !Outnput residual at each iteration
 call scaninilogical(infile,block,'Verbose',verbose,defval,IOUT)

 !MG Levels 
 call scaniniint(infile,block,'nxlevels',nx1Levels,defval,IOUT)
 call scaniniint(infile,block,'nylevels',nx2Levels,defval,IOUT)
 call scaniniint(infile,block,'nzlevels',nx3Levels,defval,IOUT)

 !MG Cycles 
 call scaniniint(infile,block,'maxcy',maxcy,defval,IOUT)

 !MG Tolerance
 call scaninireal(infile,block,'tolmax',tolmax,defval,IOUT)
 ngrid=max(nx1Levels,nx2Levels,nx3Levels)
 allocate( ipre_relax(ngrid),stat=s1 )
 allocate( ipost_relax(ngrid),stat=s1 )

 idum=ngrid
 call scaninimint(infile,block,'PreRelax',ipre_relax,idum,defval,IOUT)
 if (idum.NE.ngrid) then 
  write(IOUT,'(a33,i4,a16,i4)') 'ERROR IN MGsolver/PreRelax ngrid=',ngrid,' LEVELS INPUT=',idum
  stop
 endif
  idum=ngrid
 call scaninimint(infile,block,'PstRelax',ipost_relax,idum,defval,IOUT)
 if (idum.NE.ngrid) then 
  write(IOUT,'(a33,i4,a16,i4)') 'ERROR IN MGsolver/PstRelax ngrid=',ngrid,' LEVELS INPUT=',idum
  stop
 endif


 !MG Boundary Conditions
!                   vbc(6)            k 
!                   /                  ^  ^
!       -----vbc(4)/-----              | / i
!       |         /     |              |/
!       |        /      |         -----/-----> j
!     vbc(3)----+-----vbc(1)          /|
!       |      /        |            / |
!       |     /         |           /  |
!       -----/vbc(2)----|
!           /
!         vbc(5)
!
!tbc(xx)
! xx=0 for periodic
! xx=1 for Neumann (dp/dn=0)
! xx=2 for Dirichlet (p=0)  !(where the wall is located halfway between sx-1,sx or a half cell outside ex+1

![BCs]
!*******************************************************************************
!**********************************START X1 BC's********************************
!*******************************************************************************
 block='BCs'
 call scaninichar(infile,block,'X1',BCtype,defval,IOUT)
 select case(BCtype)
  case('Periodic','periodic','per')
   tbc(5) = 0
   tbc(6) = 0
   vbc(5) = -1
   vbc(6) = -1
  case('Read','read')  !READ X1 BC's
   block='X1min'
   call scaninichar(infile,block,'PTYPE',inBC,defval,IOUT)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     tbc(5)=2
    case('Neumann','neumann','neu')
     tbc(5)=1
    case DEFAULT
     write(IOUT,'(a)') "NO VALID BC'S FOUND FOR P AT X1min, FATAL! "
     stop
   end select
  call scaninireal(infile,block,'PVALUE',vbc(5),defval,IOUT)
   block='X1max'
   call scaninichar(infile,block,'PTYPE',inBC,defval,IOUT)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     tbc(6)=2
    case('Neumann','neumann','neu')
     tbc(6)=1
    case DEFAULT
     write(IOUT,'(a)') "NO VALID BC'S FOUND FOR P AT X1max, FATAL! "
     stop
   end select
  call scaninireal(infile,block,'PVALUE',vbc(6),defval,IOUT)
 end select

!*******************************************************************************
!**********************************START X2 BC's********************************
!*******************************************************************************
 block='BCs'
 call scaninichar(infile,block,'X2',BCtype,defval,IOUT)
 select case(BCtype)
  case('Periodic','periodic','per')
   tbc(1) = 0
   tbc(3) = 0
   vbc(1) = -1
   vbc(3) = -1
  case('Read','read')  !READ X2 BC's
   block='X2min'
   call scaninichar(infile,block,'PTYPE',inBC,defval,IOUT)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     tbc(3)=2
    case('Neumann','neumann','neu')
     tbc(3)=1
    case DEFAULT
     write(IOUT,'(a)') "NO VALID BC'S FOUND FOR P AT X2min, FATAL! "
     stop
   end select
  call scaninireal(infile,block,'PVALUE',vbc(3),defval,IOUT)
   block='X2max'
   call scaninichar(infile,block,'PTYPE',inBC,defval,IOUT)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     tbc(1)=2
    case('Neumann','neumann','neu')
     tbc(1)=1
    case DEFAULT
     write(IOUT,'(a)') "NO VALID BC'S FOUND FOR P AT X2max, FATAL! "
     stop
   end select
  call scaninireal(infile,block,'PVALUE',vbc(1),defval,IOUT)
 end select

!*******************************************************************************
!**********************************START X3 BC's********************************
!*******************************************************************************
 block='BCs'
 call scaninichar(infile,block,'X3',BCtype,defval,IOUT)
 select case(BCtype)
  case('Periodic','periodic','per')
   tbc(2) = 0
   tbc(4) = 0
   vbc(2) = -1
   vbc(4) = -1
  case('Read','read')  !READ X3 BC's
   block='X3min'
   call scaninichar(infile,block,'PTYPE',inBC,defval,IOUT)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     tbc(2)=2
    case('Neumann','neumann','neu')
     tbc(2)=1
    case DEFAULT
     write(IOUT,'(a)') "NO VALID BC'S FOUND FOR P AT X3min, FATAL! "
     stop
   end select
  call scaninireal(infile,block,'PVALUE',vbc(2),defval,IOUT)
   block='X3max'
   call scaninichar(infile,block,'PTYPE',inBC,defval,IOUT)
   select case(inBC)
    case('Dirichlet','dirichlet','dir')
     tbc(4)=2
    case('Neumann','neumann','neu')
     tbc(4)=1
    case DEFAULT
     write(IOUT,'(a)') "NO VALID BC'S FOUND FOR P AT X3max, FATAL! "
     stop
   end select
  call scaninireal(infile,block,'PVALUE',vbc(4),defval,IOUT)
 end select

 stat=0
return
end subroutine mg_read_params
