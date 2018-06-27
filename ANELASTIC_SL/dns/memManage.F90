subroutine allocation(ok)
!@t
! \textbf{subroutine allocation(ok)}
!@h
!   Description:
!     Allocate the grid and flow variables.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
!@h
!   Comments:
!     An output message is sent to confirm that everything was allocated
!     properly.
!@q

 use Domain, only: sx,ex,sy,ey,sz,ez, nxp2, nyp2, nzp2
 use Grid
 use Flow
 use LESmod
 use transient
 use Langmuir_params, only: u_stokes, v_stokes, dus_dz, dvs_dz
 use Spng, only: phiX1, phiX2, phiX2, X1inf, X2inf, X3inf
 use Parameters, only: u1ICg,u2ICg,u3ICg,rhoICg,scal1ICg
 use stat_params, only: GSTATS_store, GSTATS_current
#ifdef PARALLEL
 use dd,     only: PlnX1,PlnX2, PlnX3, PenX1, PenX2, PenX3, &
                               nxprocs, nyprocs, nzprocs
#endif
 use IO,     only: IOUT
 implicit none

!Passed Variables
 integer,intent(out)       :: ok

!Local Variables
 integer                    :: s1

!GRID ALLOCATION
 !X1-Direction
 allocate( xe(1:nxp2), stat=s1 )
 allocate( xc(1:nxp2), stat=s1 )
 allocate( dxe(1:nxp2), stat=s1 )
 allocate( dxc(1:nxp2), stat=s1 )
 allocate( rdxe(1:nxp2), stat=s1 )
 allocate( rdxc(1:nxp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating X1 Grid Variables"
  goto 1000
 endif

 !X2-Direction
 allocate( ye(1:nyp2), stat=s1 )
 allocate( yc(1:nyp2), stat=s1 )
 allocate( dye(1:nyp2), stat=s1 )
 allocate( dyc(1:nyp2), stat=s1 )
 allocate( rdye(1:nyp2), stat=s1 )
 allocate( rdyc(1:nyp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating X2 Grid Variables"
  goto 1000
 endif

 !X3-Direction
 allocate( ze(1:nzp2), stat=s1 )
 allocate( zc(1:nzp2), stat=s1 )
 allocate( dze(1:nzp2), stat=s1 )
 allocate( dzc(1:nzp2), stat=s1 )
 allocate( rdze(1:nzp2), stat=s1 )
 allocate( rdzc(1:nzp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating X3 Grid Variables"
  goto 1000
 endif

!FLOW VARIABLE ALLOCATION
 if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM' &
  .or.Rmodel.EQ.'DNSrho'.or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho' &
  .or.Smodel.EQ.'DNSscal1'.or.Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then

 allocate( u(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating u"
  goto 1000
 endif

 allocate( v(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating v"
  goto 1000
 endif

 allocate( w(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating w"
  goto 1000
 endif

 allocate( p(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating p"
  goto 1000
 endif

 allocate( rho(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating rho"
  goto 1000
 endif

 allocate( r(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating r"
  goto 1000
 endif

 allocate( pzz(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating pzz"
  goto 1000
 endif

 allocate( at(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating at"
  goto 1000
 endif

 allocate( tz(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating tz"
  goto 1000
 endif

 allocate( pz(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating pz"
  goto 1000
 endif

 allocate( qv0(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating qv0"
  goto 1000
 endif

 allocate( ALPHA(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating ALPHA"
  goto 1000
 endif

 allocate( ALPHA_0(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating ALPHA_0"
  goto 1000
 endif

 allocate( qv(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating qv"
  goto 1000
 endif

 allocate( ql(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating ql"
  goto 1000
 endif

 allocate( qr(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating qr"
  goto 1000
 endif

 allocate( ar(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating ar"
  goto 1000
 endif

 allocate( cr(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating cr"
  goto 1000
 endif

 allocate( er(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating er"
  goto 1000
 endif

 allocate( vrr(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating vrr"
  goto 1000
 endif

 allocate( scal1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating scal1"
  goto 1000
 endif
 endif
 
 allocate( densityBG(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating densityBG"
  goto 1000
 endif

 allocate( u1ICg(1:nzp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating u1ICg"
  goto 1000
 endif

 allocate( u2ICg(1:nzp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating u2ICg"
  goto 1000
 endif

 allocate( u3ICg(1:nzp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating u3ICg"
  goto 1000
 endif

 allocate( rhoICg(1:nzp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating rhoICg"
  goto 1000
 endif

 allocate( scal1ICg(1:nzp2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating scal1ICg"
  goto 1000
 endif
 

!LANGMUIR ALLOCATION
 allocate( u_stokes(sz-1:ez+1,1:2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating u_stokes"
  goto 1000
 endif

 allocate( v_stokes(sz-1:ez+1,1:2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating v_stokes"
  goto 1000
 endif

 allocate( dus_dz(sz-1:ez+1,1:2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating dus_dz"
  goto 1000
 endif
 
 allocate( dvs_dz(sz-1:ez+1,1:2), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating dvs_dz"
  goto 1000
 endif


!STATISTICS
 allocate( GSTATS_store(1:nzp2,1:250), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating GSTATS_store"
  goto 1000
 endif

 allocate( GSTATS_current(1:nzp2,1:250), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating GSTATS_current"
  goto 1000
 endif


!TRANSIENT VARIABLE ALLOCATION
 allocate( transtmp(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating transtmp"
  goto 1000
 endif

 allocate( transtmp1(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating transtmp1"
  goto 1000
 endif

 allocate( transtmp2(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating transtmp2"
  goto 1000
 endif

 allocate( transtmp3(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating transtmp3"
  goto 1000
 endif

 allocate( transtmp4(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating transtmp4"
  goto 1000
 endif

 allocate( dkdt(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating dkdt"
  goto 1000
 endif
 
 allocate( k_old(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating k_old"
  goto 1000
 endif
 
 allocate( dMKEdt(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating dMKEdt"
  goto 1000
 endif
 
 allocate( MKE_old(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating MKE_old"
  goto 1000
 endif

 allocate( dU1dt(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating dU1dt"
  goto 1000
 endif
 
 allocate( U1_old(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating U1_old"
  goto 1000
 endif

 allocate( dU2dt(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating dU2dt"
  goto 1000
 endif
 
 allocate( U2_old(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating U2_old"
  goto 1000
 endif

 allocate( dTdt(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating dTdt"
  goto 1000
 endif
 
 allocate( T_old(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating T_old"
  goto 1000
 endif

 allocate( dSdt(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating dSdt"
  goto 1000
 endif
 
 allocate( S_old(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating S_old"
  goto 1000
 endif

 allocate( dt2dt(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating dt2dt"
  goto 1000
 endif
 
 allocate( t2_old(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating t2_old"
  goto 1000
 endif

 allocate( ds2dt(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating ds2dt"
  goto 1000
 endif
 
 allocate( s2_old(sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating s2_old"
  goto 1000
 endif

#ifdef PARALLEL
 allocate( PlnX1(1:nyp2,1:nzp2,1:2),stat=s1 )
 allocate( PlnX2(1:nxp2,1:nzp2,1:2),stat=s1 )
 allocate( PlnX3(1:nxp2,1:nyp2,1:2),stat=s1 )
 !Pencils
 allocate( PenX1(1:nxp2,1:4),stat=s1 )
 allocate( PenX2(1:nyp2,1:4),stat=s1 )
 allocate( PenX3(1:nzp2,1:4),stat=s1 )
#endif


 call allocate_temps(s1)
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating temps `allocation`"
  goto 1000
 endif
 write(IOUT,'(a)') "ALLOCATION COMPLETED"
 return

 1000 continue 
 ok = s1
 write(IOUT,'(a)') "ALLOCATION FAILED"

 return
end subroutine allocation


subroutine allocation_LES(ok)
!@t
! \textbf{subroutine allocation_LES(ok)}
!@h
!   Description:
!     Allocate the grid and flow variables.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
!@h
!   Comments:
!     An output message is sent to confirm that everything was allocated
!     properly.
!@q

 use Domain, only: sx,ex,sy,ey,sz,ez, nxp2, nyp2, nzp2
 use Grid
 use Flow
 use LESmod
 use transient
 use Langmuir_params, only: u_stokes, v_stokes, dus_dz, dvs_dz
 use Spng, only: phiX1, phiX2, phiX2, X1inf, X2inf, X3inf
 use Parameters, only: u1ICg,u2ICg,u3ICg,rhoICg,scal1ICg
#ifdef PARALLEL
 use dd,     only: PlnX1,PlnX2, PlnX3, PenX1, PenX2, PenX3, &
                               nxprocs, nyprocs, nzprocs
#endif
 use IO,     only: IOUT
 implicit none

!Passed Variables
 integer,intent(out)       :: ok

!Local Variables
 integer                    :: s1

 if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM' &
 .or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho' &
 .or.Smodel.EQ.'SSMscal1'.or.Rmodel.EQ.'DSMscal1') then
!LES VARIABLE ALLOCATION
  allocate( S11(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES S11 "
   goto 1000
  endif 

  allocate( S22(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES S22 "
   goto 1000
  endif

  allocate( S33(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES S33 "
   goto 1000
  endif

  allocate( S12(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES S12 "
   goto 1000
  endif

  allocate( S13(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
   if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES S13 "
   goto 1000
  endif

  allocate( S23(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES S23 "
   goto 1000
  endif

  allocate( modS(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES modS"
   goto 1000
  endif

  allocate( modSp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES modSp"
   goto 1000
  endif

  allocate( fmodS(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES fmodS"
   goto 1000
  endif

  allocate( fmodSp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES fmodSp"
   goto 1000
  endif

  allocate( delg(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES delg"
   goto 1000
  endif

  allocate( un_gt(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES un_gt"
   goto 1000
  endif

  allocate( vn_gt(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES vn_gt"
   goto 1000
  endif

  allocate( wn_gt(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES wn_gt"
   goto 1000
  endif

  allocate( Csgs(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES Csgs"
   goto 1000
  endif

  allocate( CTsgs(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES CTsgs"
   goto 1000
  endif

  allocate( CSsgs(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES CSsgs"
   goto 1000
  endif

  allocate( nuT(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES nuT"
   goto 1000
  endif

  allocate( kappaT(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES kappaT"
   goto 1000
  endif

  allocate( nappaT(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES nappaT"
   goto 1000
  endif

  allocate( nusm(sz-1:ez+1), stat=s1 )
  nusm=0.d0
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES nusm"
   goto 1000
  endif

  allocate( kappasm(sz-1:ez+1), stat=s1 )
  kappasm=0.d0
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES kappasm"
   goto 1000
  endif

  allocate( nappasm(sz-1:ez+1), stat=s1 )
  nappasm=0.d0
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES nappasm"
   goto 1000
  endif

 elseif (Vmodel.NE.'DNS'.and.Rmodel.NE.'DNSrho'.and.Smodel.NE.'DNSscal1') then 
  write(IOUT,'(a)')"ABORTING ALLOCATION DSM: "//trim(Vmodel)//" or "//trim(Rmodel)//" or "//trim(Smodel)//"NOT IMPLEMENTED"
  s1=1
  return
 endif

 if (Vmodel.EQ.'DMM') then
  allocate( un_gtgt(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES un_gtgt"
   goto 1000
  endif

  allocate( vn_gtgt(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES vn_gtgt"
   goto 1000
  endif

 allocate( wn_gtgt(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (s1.NE.0) then
   write(IOUT,*) "Error Allocating LES wn_gtgt"
   goto 1000
  endif
 elseif (Vmodel.NE.'DNS'.and.Vmodel.NE.'SSM'.and.Vmodel.NE.'DSM'&
    .and.Rmodel.NE.'DNSrho'.and.Rmodel.NE.'SSMrho'.and.Rmodel.NE.'DSMrho'& 
    .and.Smodel.NE.'DNSscal1'.and.Smodel.NE.'SSMscal1'.and.Smodel.NE.'DSMscal1') then 
   write(IOUT,'(a)') "ABORTING ALLOCATION DMM: "//trim(Vmodel)//" or "//trim(Rmodel)//" or "//trim(Smodel)//"NOT IMPLEMENTED"
   s1=1
   return
 endif


 write(IOUT,'(a)') "LES ALLOCATION COMPLETED"
 return

 1000 continue 
 ok = s1
 write(IOUT,'(a)') "LES ALLOCATION FAILED"

 return
end subroutine allocation_LES

subroutine deallocate_temps(ok)
!@t
! \textbf{subroutine deallocate\_temps(ok)}
!@h
!   Description:
!     Deallocate temporary variables u1_tmp2, u2_tmp2, u3_tmp2, r_tmp1, and
!     r_tmp2.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use Flow,   only: u1_tmp2,u2_tmp2,u3_tmp2,r_tmp1,r_tmp2,q_tmp1,q_tmp2,q_tmp3,scal1_tmp1,scal1_tmp2,Vmodel,Rmodel,Smodel
 use LESmod, only: lestmp, lestmp1, lestmp2, lestmp3, lestmp4
 use IO,     only: IOUT
 implicit none

!Passed Variables
 integer,intent(out)       :: ok

!Local Variables
 integer                   :: s1
 s1=0
 ok=0

 if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM' &
  .or.Rmodel.EQ.'DNSrho'.or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho' &
  .or.Smodel.EQ.'DNSscal1'.or.Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then
  if ( allocated(u1_tmp2) ) deallocate( u1_tmp2, stat=s1)
  if ( allocated(u2_tmp2) ) deallocate( u2_tmp2, stat=s1)
  if ( allocated(u3_tmp2) ) deallocate( u3_tmp2, stat=s1)
  if ( allocated(r_tmp1) ) deallocate( r_tmp1, stat=s1)
  if ( allocated(r_tmp2) ) deallocate( r_tmp2, stat=s1)
  if ( allocated(q_tmp1) ) deallocate( q_tmp1, stat=s1)
  if ( allocated(q_tmp2) ) deallocate( q_tmp2, stat=s1)
  if ( allocated(q_tmp3) ) deallocate( q_tmp3, stat=s1)
  if ( allocated(scal1_tmp1) ) deallocate( scal1_tmp1, stat=s1)
  if ( allocated(scal1_tmp2) ) deallocate( scal1_tmp2, stat=s1)
 else
  write(IOUT,'(a)') "ABORTING DEALLOCATE_TEMPS"//trim(Vmodel)//"or"//trim(Rmodel)//"or"//trim(Smodel)//"NOT IMPLEMENTED"
  s1=1
  return
 endif
 if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM'&
 .or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho' &
 .or.Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then
  if ( allocated(lestmp) ) deallocate( lestmp, stat=s1)
  if ( allocated(lestmp1) ) deallocate( lestmp1, stat=s1)
  if ( allocated(lestmp2) ) deallocate( lestmp2, stat=s1)
  if ( allocated(lestmp3) ) deallocate( lestmp3, stat=s1)
  if ( allocated(lestmp4) ) deallocate( lestmp4, stat=s1)
 elseif (Vmodel.NE.'DNS'.or.Rmodel.NE.'DNSrho'.or.Smodel.NE.'DNSscal1') then 
  write(IOUT,'(a)') "ABORTING DEALLOCATE_TEMPS "//trim(Vmodel)//"or"//trim(Rmodel)//"or"//trim(Smodel)//"NOT IMPLEMENTED"
  s1=1
  return
 endif

 if (s1.NE.0) write(IOUT,'(a31,i5)') "ERROR DEALLOCATING TEMPS: stat=",s1
 ok=s1
 return
end subroutine


subroutine allocate_temps(ok)
!@t
! \textbf{subroutine allocate\_temps(ok)}
!@h
!   Description:
!     Allocate temporary variables u1_tmp2, u2_tmp2, u3_tmp2, r_tmp1, and
!     r_tmp2.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use Domain, only: sx,ex,sy,ey,sz,ez
 use Flow,   only: u1_tmp2,u2_tmp2,u3_tmp2,r_tmp1,r_tmp2,q_tmp1,q_tmp2,q_tmp3,scal1_tmp1,scal1_tmp2,Vmodel,Rmodel,Smodel
 use LESmod, only: lestmp,lestmp1,lestmp2,lestmp3,lestmp4
 use IO,     only: IOUT
 implicit none

!Passed Variables
 integer,intent(out)       :: ok

!Local Variables
 integer                    :: s1
 ok=0
 s1=0
 
 if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM'&
  .or.Rmodel.EQ.'DNSrho'.or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho' &
  .or.Smodel.EQ.'DNSscal1'.or.Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then
  if (.not. allocated(u1_tmp2) ) allocate( u1_tmp2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(u2_tmp2) ) allocate( u2_tmp2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(u3_tmp2) ) allocate( u3_tmp2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(r_tmp1)  ) allocate( r_tmp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(r_tmp2)  ) allocate( r_tmp2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(q_tmp1)  ) allocate( q_tmp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(q_tmp2)  ) allocate( q_tmp2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(q_tmp3)  ) allocate( q_tmp3(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(scal1_tmp1)  ) allocate( scal1_tmp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(scal1_tmp2)  ) allocate( scal1_tmp2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 else
  write(IOUT,'(a)') "ABORTING ALLOCATE_TEMPS "//trim(Vmodel)//"or"//trim(Rmodel)//"or"//trim(Smodel)//"NOT IMPLEMENTED"
  s1 =1
  return
 endif
 
 if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM'&
 .or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho' &
 .or.Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then
  if (.not. allocated(lestmp)  ) allocate( lestmp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(lestmp1)  ) allocate( lestmp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(lestmp2)  ) allocate( lestmp2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(lestmp3)  ) allocate( lestmp3(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
  if (.not. allocated(lestmp4)  ) allocate( lestmp4(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 elseif (Vmodel.NE.'DNS'.or.Rmodel.NE.'DNSrho'.or.Smodel.NE.'DNSscal1') then 
  write(IOUT,'(a)') "ABORTING ALLOCATE_TEMPS "//trim(Vmodel)//"or"//trim(Rmodel)//"or"//trim(Smodel)//" NOT IMPLEMENTED"
  s1=1
  return
 endif

 if (s1.NE.0) write(IOUT,'(a,i5)') "ERROR ALLOCATING TEMPS: stat=",s1
  ok=s1
 return
end subroutine

subroutine zero_temps
!@t
! \textbf{subroutine mg\_allocate\_1(ok)}
!@h
!   Description:
!     Set temporary variables u1_tmp2, u2_tmp2, u3_tmp2, r_tmp1, and
!     r_tmp2 to zero.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
 use FLOW, only: u1_tmp2,u2_tmp2,u3_tmp2,r_tmp1,r_tmp2,scal1_tmp1,scal1_tmp2,Vmodel,Rmodel,Smodel
 use LESmod, only: lestmp, lestmp1, lestmp2, lestmp3, lestmp4
 use IO,   only : IOUT
 implicit none
 
 integer :: s1

 if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM'&
 .or.Rmodel.EQ.'DNSrho'.or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho'&
 .or.Smodel.EQ.'DNSscal1'.or.Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then
  u1_tmp2(:,:,:)=0.d0
  u2_tmp2(:,:,:)=0.d0
  u3_tmp2(:,:,:)=0.d0
  r_tmp1(:,:,:)=0.d0
  r_tmp2(:,:,:)=0.d0
  scal1_tmp1(:,:,:)=0.d0
  scal1_tmp2(:,:,:)=0.d0
 else
  write(IOUT,'(a)') "ABORTING ZERO_TEMPS "//trim(Vmodel)//" or "//trim(Rmodel)//" or "//trim(Smodel)//" NOT IMPLEMENTED"
  s1=1
  return
 endif
 
 if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM'&
 .or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho'&
 .or.Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then
  lestmp(:,:,:)=0.d0
  lestmp1(:,:,:)=0.d0
  lestmp2(:,:,:)=0.d0
  lestmp3(:,:,:)=0.d0
  lestmp4(:,:,:)=0.d0
 elseif (Vmodel.NE.'DNS'.or.Rmodel.NE.'DNSrho'.or.Smodel.NE.'DNSscal1') then 
  write(IOUT,'(a)') "ABORTING ZERO_TEMPS "//trim(Vmodel)//" or "//trim(Rmodel)//"or"//trim(Smodel)//" NOT IMPLEMENTED"
  s1=1
  return
 endif
 return
end subroutine zero_temps
