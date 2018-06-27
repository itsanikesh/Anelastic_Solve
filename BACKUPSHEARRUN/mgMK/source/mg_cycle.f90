












subroutine mg_cycle(workc1,workc2,workc3,workc4,workc5,workc6,workc7,res,work1,work2,kcur)
!@t
! \textbf{subroutine mg\_cycle(res,work1,work2,kcur)}
!@h
!   Description:
!     Perform one V-cycle.
!@q
!  Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!  Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!  Comments:
!    Based on Bernard Bunner's (bunner@engin.umich.edu) mgd3 solver
!    available at www.mgnet.org
!@q

! THERE SEEMS TO BE A BIG SLOWDOWN IN MG_RELAX_RB WHEN MG_RELAX IS ALSO AN OPTIONS
! THIS NEEDS TO BE UNDERSTOOD!!!

 use ntypes, only: r8,i4
 use mgVars, only: sxk,exk,syk,eyk,szk,ezk,nxk,nyk,nzk,ngrid,kp3d,nwork3d,nwork,ipre_relax,ipost_relax,Smoother, IOUTmg
 implicit none

 !Passed Variables
 integer(i4),intent(in)    :: kcur
 real(r8),intent(out)      :: res(sxk(ngrid)-1:exk(ngrid)+1,syk(ngrid)-1:eyk(ngrid)+1,szk(ngrid)-1:ezk(ngrid)+1)
 real(r8),intent(in)       :: workc1(nwork3d),workc2(nwork3d),workc3(nwork3d),workc4(nwork3d),workc5(nwork3d),workc6(nwork3d),workc7(nwork3d)
 real(r8),intent(inout)    :: work1(nwork3d),work2(nwork3d)

 !Local Variables
 integer(i4)                   :: ipf,ipc, lC, lF
 integer(i4)                   :: klevel
 integer(i4)                   :: err1
 integer(i4)                   :: sxf,exf,syf,eyf,szf,ezf,nxf,nyf,nzf,icf
 integer(i4)                   :: kpbgn(20),kcbgn(20)

 !DOWN SWEEP
  klevel=kcur
  lC=klevel-1
  lF=klevel
  ipf=kp3d(lF)      
  ipc=kp3d(lC)     
  !Finest grid level 
  !phiF=work1(ipf)
  !rhsF=work2(ipf) 
  select case(Smoother)
	case('RBPGS')
        sxf=sxk(klevel)
        exf=exk(klevel)
        syf=syk(klevel)
        eyf=eyk(klevel)
        szf=szk(klevel)
        ezf=ezk(klevel)
        nxf=nxk(klevel)
        nyf=nyk(klevel)
        nzf=nzk(klevel)
!        write(IOUTmg,*)"CALCULATED RELAXRB 1"
        call mg_relaxRB(lF,workc1(ipf),workc2(ipf),workc3(ipf),workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf),work1(ipf),work2(ipf),ipre_relax(lF))
!        write(IOUTmg,*)"CALCULATED RELAXRB 2"
	case('PGS')
	call mg_relax(lF,work1(ipf),work2(ipf),ipre_relax(lF) )
	case('LJ')
	call mg_relaxLz(lF,workc1(ipf),workc2(ipf),workc3(ipf),workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf),work1(ipf),work2(ipf),ipre_relax(lF))
	case DEFAULT
		write(IOUTmg,*) "SMOOTHER: "//trim(Smoother)//" UNKNOWN ABORTING"
		stop
   end select
  !Calculate residual and restrict it to klevel-1
   !phiC=work1(ipc) 
   !rhsC=work2(ipc)
   !phiF=work1(ipf)
   !rhsF=work2(ipf)
   !residual=res 
   call mg_restr( lC,lF,work1(ipc),work2(ipC),workc1(ipf),workc2(ipf),workc3(ipf),workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf),&
                  work1(ipf),work2(ipF),res ) 
!   write(IOUTmg,*)"CALCULATED RESTR"
 do klevel=kcur-1,2,-1
  lC=klevel-1
  lF=klevel
  ipf=kp3d(lF)      
  ipc=kp3d(lC)     

   !Relax at current level
   select case(Smoother)
	case('RBPGS')
	call mg_relaxRB(lF,workc1(ipf),workc2(ipf),workc3(ipf),workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf),work1(ipf),work2(ipf),ipre_relax(lF))
	case('PGS')
	call mg_relax(lF,work1(ipf),work2(ipf),ipre_relax(lF) )
	case('LJ')
	call mg_relaxLz(lF,workc1(ipf),workc2(ipf),workc3(ipf),workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf),work1(ipf),work2(ipf),ipre_relax(lF))
	case DEFAULT
	write(IOUTmg,*) "SMOOTHER: "//trim(Smoother)//" UNKNOWN ABORTING"
		stop
    end select

  !Calculate residual and restrict it to klevel-1
   !phiC=work1(ipc) 
   !rhsC=work2(ipc)
   !phiF=work1(ipf)
   !rhsF=work2(ipf)
   !residual=res 
   call mg_restr( lC,lF,work1(ipc),work2(ipC),workc1(ipf),workc2(ipf),workc3(ipf),workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf),&
                  work1(ipf),work2(ipF),res ) 

 enddo

  !PreRelax at coursest Level
  klevel=1
  lC=klevel 
  ipc=kp3d(lC)      

  select case(Smoother)
	case('RBPGS')
	call mg_relaxRB(klevel,workc1(ipc),workc2(ipc),workc3(ipc),workc4(ipc),workc5(ipc),workc6(ipc),workc7(ipc),work1(ipc),work2(ipc),ipre_relax(lC))
	case('PGS')
	call mg_relax(lF,work1(ipf),work2(ipf),ipre_relax(lF) )
	case('LJ')
	call mg_relaxLz(klevel,workc1(ipf),workc2(ipf),workc3(ipf),workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf),work1(ipc),work2(ipc),ipre_relax(lC))
	case DEFAULT
	write(IOUTmg,*) "SMOOTHER: "//trim(Smoother)//" UNKNOWN ABORTING"
		stop
  end select
 !UPSWEEP
 do klevel=1,kcur-1
  lF=klevel+1
  lC=klevel
  ipf=kp3d(lF)
  ipc=kp3d(lC) 

 !Post-relax
  !phiC=work1(ipC)
  !rhsC=work2(ipC) 
  select case(Smoother)
	case('RBPGS')
        call mg_relaxRB(klevel,workc1(ipc),workc2(ipc),workc3(ipc),workc4(ipc),workc5(ipc),workc6(ipc),workc7(ipc),work1(ipc),work2(ipc),ipost_relax(lC))
	case('PGS')
	call mg_relax(lF,work1(ipf),work2(ipf),ipre_relax(lF) )
	case('LJ')
	call mg_relaxLz(klevel,workc1(ipf),workc2(ipf),workc3(ipf),workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf),work1(ipc),work2(ipc),ipost_relax(lC))
	case DEFAULT
	write(IOUTmg,*) "SMOOTHER: "//trim(Smoother)//" UNKNOWN ABORTING"
		stop
  end select
 !Inject course correction to fine grid 
  !phiC=work1(ipC)
  !phiF=work1(ipF) 
  call mg_cor(lC,lF,work1(ipf),work1(ipc))

 enddo

 !Post-relax and finest level
 lF=kcur
 ipf=kp3d(lF) 
 select case(Smoother)
	case('RBPGS')
        call mg_relaxRB(lF,workc1(ipf),workc2(ipf),workc3(ipf),workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf),work1(ipf),work2(ipf),ipost_relax(lF))
	case('PGS')
	call mg_relax(lF,work1(ipf),work2(ipf),ipre_relax(lF) )
	case('LJ')
	call mg_relaxLz(lF,workc1(ipf),workc2(ipf),workc3(ipf),workc4(ipf),workc5(ipf),workc6(ipf),workc7(ipf),work1(ipf),work2(ipf),ipost_relax(lF))
	case DEFAULT
	write(IOUTmg,*) "SMOOTHER: "//trim(Smoother)//" UNKNOWN ABORTING"
	stop
 end select
 return
end
