subroutine NSsolver(stat)
!@t
! \textbf{subroutine NSsolver(stat)}
!@h
!   Description:
!     Solves the unsteady Navier Stokes equations. I NEED TO UPDATE THIS KYLE

!   Method:
!     The low storage RK3 method of Williamson (1980 Journal of Computational Physics)
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

 use ntypes,     only: i4, r8
 use IO,         only: IOUT, checkDIV
 use Flow
 use LESmod
 use transient
 use domain,     only: sz,ez,sx,ex,sy,ey
 use Parameters, only: bstep, estep, nstep, delt, cfl, time, Ddt, dtmax, rhoMIN, rhoMAX, scal1MIN, scal1MAX, clip
 use Parameters, only: rRe,rPr,rSc 
 implicit none

 !Passed Variables
 integer,intent(out)              :: stat

 !Local Variables
 integer                          :: ok, i, j, k,err1
 real(r8)                         :: a1, a2, a3, ec,dt_rk,told,told2,cfl_spec,clipMAX,clipMIN
 logical                          :: wdivplanes=.false.
 logical,parameter                :: debug=.false.
 real(r8)  :: mean(sz-1:ez+1), rms(sz-1:ez+1), temp(sz-1:ez+1), EDIFFtol

told=1.0d-16
told2=1.d-16


if (debug) call check_point('NSsolver#0',.false.)

!LOW STORAGE RK3 UPDATE 
!(Williamson 1980 Journal of Computational Physics)
!ROUND OFF CORRECTION NOT IMPLEMENTED
!USE DOUBLE PRECISION 

!Le and Moin JCP 1991 solve the pressure projection only at the last time step
!this reduces the global accuracy of the RK3 update Dt^2 from Dt^3.  May save
!considerable expense, set psolve to .false. in the first two rk substeps  

cfl_spec = cfl
do nstep=bstep,estep

 !Ensure temporary variables are empty
 call zero_temps  
 
 !let clf increase with time from 0.25 to 1 from t=0 to tf
!  cfl = 0.75d0+(cfl_spec-0.75d0)*0.5d0*(1.d0+tanh((time-2.5d0)/0.5d0))
 
 !Calculate time step
 call calcdt(delt,ok)
 if (delt.GE.dtmax) delt=dtmax
  if (ok.NE.0) goto 1000
 write(IOUT,*) ""
 write(IOUT,'(i6,a9,f16.8,a7,f16.8,a7,f16.8)') nstep,"   time=",time,"   dt=",delt,"   CFL=",cfl

 !Calculate gravity (may be ramped up smoothly from zero)
 call gravity(ok)
  if (ok.NE.0) stop


 select case (Vmodel) 
  case('DNS')
   write(IOUT,*)"Vmodel = DNS"
  case('SSM')
   if (nstep.GE.les_start.and.mod(nstep,les_freq).EQ.0) then
    write(IOUT,*)"Vmodel = SSM on"
    call SSM 
   endif
  case('DSM')
   if (nstep.GE.les_start.and.mod(nstep,les_freq).EQ.0) then
    write(IOUT,*)"Vmodel = DSM on"
    call DSM 
   endif
  case('DMM')
   if (nstep.GE.les_start.and.mod(nstep,les_freq).EQ.0) then 
    write(IOUT,*)"Vmodel = DMM on"
    call DMM 
   endif
  case DEFAULT
   write(IOUT,'(a)') "ABORTING NS solver: "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
 end select

 select case (Rmodel) 
  case('DNSrho')
   write(IOUT,*)"Rmodel = DNSrho"
  case('SSMrho')
   if (nstep.GE.les_start.and.mod(nstep,les_freq).EQ.0) then 
    write(IOUT,*)"Rmodel = SSMrho on"
    call SSMrho 
   endif
  case('DSMrho')
   if (nstep.GE.les_start.and.mod(nstep,les_freq).EQ.0) then 
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
   if (nstep.GE.les_start.and.mod(nstep,les_freq).EQ.0) then 
    write(IOUT,*)"Smodel = SSMscal1 on"
    call SSMscal1 
   endif
  case('DSMscal1')
   if (nstep.GE.les_start.and.mod(nstep,les_freq).EQ.0) then 
    write(IOUT,*)"Smodel = DSMscal1 on"
    call DSMscal1 
   endif
  case DEFAULT
   write(IOUT,'(a)') "ABORTING NS solver: "//trim(Smodel)//" NOT IMPLEMENTED"
   stat=1
 end select


   !filter to assist LES
  if (time.GT.0.d0.and.mod(nstep,50).EQ.0) then
   if (Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then
    call zero_temps
    EDIFFtol = -0.d0*rRe*rPr
    call filterHF(rho,r_tmp2,2,1,1,1,'rho',kappaT,EDIFFtol,err1)
    !call filter(rho,r_tmp2,2,1,1,1,'rho',err1)
    rho=r_tmp2
    call ghost(rho,'rho',err1)
    write(IOUT,*)"Filtering rho in XYZ dirs: special FIL!"
   endif
  endif
  if (time.GT.0.d0.and.mod(nstep,50).EQ.0) then
   if (Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then
    call zero_temps
    EDIFFtol = -0.d0*rRe*rSc
    call filterHF(scal1,scal1_tmp2,2,1,1,1,'scal1',nappaT,EDIFFtol,err1)
    !call filter(scal1,scal1_tmp2,2,1,1,0,'scal1',err1)
    scal1=scal1_tmp2
    call ghost(scal1,'scal1',err1)
    write(IOUT,*)"Filtering scal1 in XYZ dirs: special FIL!"
   endif
  endif
 
 !Filter density in region near surface: 5 pts
 !if (time.GT.10.d10.and.mod(nstep,10).EQ.0) then
 !  if (Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then
 !   call zero_temps
 !   call filterSURF(scal1,scal1_tmp2,2,1,1,1,'scal1',err1)
 !   scal1=scal1_tmp2
 !   call ghost(scal1,'scal1',err1)
 !   write(IOUT,*)"Filtering scal1 in XYZ dirs: SURF FIL!"
 !  endif
 ! endif

! if (nstep.GT.100.d0)  call updateMINMAX(scal1,scal1MIN,scal1Max,stat)
 
 !Ensure temporary variables are empty
 call zero_temps  

 !First sub-step

 !Get subgrid diffusivity
 if (nstep.GT.les_start) then
 select case(Vmodel)
  case('SSM','DSM','DMM')
   call SGSdiff(err1)
  case('DNS')
   !do nothing and continue
  case DEFAULT
   write(IOUT,'(a)') "ABORTING NS solver-subgrid diff : "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
 end select
 endif

 a1 = 0.d0
 a2 = 1.d0
 a3 = 1.d0/3.d0
 ec = -1.d0/6.d0
 dt_rk = 1.d0/3.d0*delt
 call rk_ss(a1,a2,a3,ec,dt_rk,delt,.true.,ok)
  if (ok.NE.0) goto 1000
  if (debug) call check_point('NSsolver#1',.false.)
  if (clip) then
   call clipVS(rho,rhoMIN,rhoMAX,stat)
   call ghost(rho,'rho',stat)
   write(IOUT,'(a17,f15.8,a10,f15.8)')"Clipping: scal1MIN=",scal1MIN,"   scal1MAX=",scal1MAX
   call clipVS(scal1,scal1MIN,scal1MAX,stat)
   call ghost(scal1,'scal1',stat)
  endif

 !Second sub-step
 !Get subgrid diffusivity
 if (nstep.GT.les_start) then
 select case(Vmodel)
  case('SSM','DSM','DMM')
   call SGSdiff(err1)
  case('DNS')
   !do nothing and continue
  case DEFAULT
   write(IOUT,'(a)') "ABORTING NS solver-subgrid diff : "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
 end select
 endif
 a1 = -5.d0/9.d0
 a2 = 1.d0
 a3 = 15.d0/16.d0
 ec = -10.d0/3.d0
 dt_rk = 5.d0/12.d0*delt
 call rk_ss(a1,a2,a3,ec,dt_rk,delt,.true.,ok)
  if (ok.NE.0) goto 1000 
  if (debug) call check_point('NSsolver#2',.false.)
  if (clip) then
   call clipVS(rho,rhoMIN,rhoMAX,stat)
   call ghost(rho,'rho',stat)
   write(IOUT,'(a17,f15.8,a10,f15.8)')"Clipping: scal1MIN=",scal1MIN,"   scal1MAX=",scal1MAX
   call clipVS(scal1,scal1MIN,scal1MAX,stat)
   call ghost(scal1,'scal1',stat)
  endif

 !Third sub-step
 !Get subgrid diffusivity
 if (nstep.GT.les_start) then
 select case(Vmodel)
  case('SSM','DSM','DMM')
   call SGSdiff(err1)
  case('DNS')
   !do nothing and continue
  case DEFAULT
   write(IOUT,'(a)') "ABORTING NS solver-subgrid diff : "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
 end select
 endif
 a1 = -153.d0/128.d0
 a2 = 1.d0
 a3 = 8.d0/15.d0
 ec = 15.d0/8.d0
 dt_rk = 1.d0/4.d0*delt
 call rk_ss(a1,a2,a3,ec,dt_rk,delt,.true.,ok)
  if (ok.NE.0) goto 1000
  if (debug) call check_point('NSsolver#3',.false.)
  if (clip) then
   call clipVS(rho,rhoMIN,rhoMAX,stat)
   call ghost(rho,'rho',stat)
   write(IOUT,'(a17,f15.8,a10,f15.8)')"Clipping: scal1MIN=",scal1MIN,"   scal1MAX=",scal1MAX
   call clipVS(scal1,scal1MIN,scal1MAX,stat)
   call ghost(scal1,'scal1',stat)
  endif

  told2 = told
  told = time
  time=time+delt

 !Check Divergence
 if (mod(nstep,checkDIV).EQ.0) then
  call divergence(u,v,w,r_tmp1,wdivplanes,ok)
  if (ok.NE.0) goto 1000 
 endif
 if (debug) call check_point('NSsolver#4',.false.)

 !OUTPUT RESULTS
 call output(.false.,ok)
  if (ok.NE.0) goto 1000 
  if (debug) call check_point('NSsolver#5',.false.)

 !Update dk/dt
 !INTERPOLATE VELOCITIES TO CELL CENTERS
  u1_tmp2 = 0.d0
  u2_tmp2 = 0.d0
  u3_tmp2 = 0.d0
  r_tmp1 = 0.d0
  call center_velocity(u,u1_tmp2,1 )
  call center_velocity(v,u2_tmp2,2 )
  call center_velocity(w,u3_tmp2,3 )
  
 !mean
  call avgX1X2(u1_tmp2,transtmp1,'uc')
  call avgX1X2(u2_tmp2,transtmp2,'vc')
  call avgX1X2(rho,transtmp3,'rf')
  call avgX1X2(scal1,transtmp4,'sf')
  dU1dt = (transtmp1-U1_old ) / (time-told)
  U1_old = transtmp1
  dU2dt = (transtmp2-U2_old ) / (time-told)
  U2_old = transtmp2
  dTdt = (transtmp3-T_old ) / (time-told)
  T_old = transtmp3
  dSdt = (transtmp4-S_old ) / (time-told)
  S_old = transtmp4
  transtmp=transtmp1+transtmp2
  dMKEdt = (transtmp-MKE_old ) / (time-told)
  MKE_old = transtmp

  !u'
   call avgX1X2(u1_tmp2,mean,'uc')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      u1_tmp2(i,j,k)=u1_tmp2(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(u1_tmp2,'cfluc',err1)

  !v'
   call avgX1X2(u2_tmp2,mean,'vc')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      u2_tmp2(i,j,k)=u2_tmp2(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(u2_tmp2,'cfluc',err1)

  !w'  
   call avgX1X2(u3_tmp2,mean,'wc')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      u3_tmp2(i,j,k)=u3_tmp2(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(u3_tmp2,'cfluc',err1)

 !<u1'u1>
  r_tmp1 = u1_tmp2*u1_tmp2
  call avgX1X2(r_tmp1,transtmp1,'uc')
 !<u2'u2'> 
  r_tmp1 = u2_tmp2*u2_tmp2
  call avgX1X2(r_tmp1,transtmp2,'vc')
 !<u3'u3'> 
  r_tmp1 = u3_tmp2*u3_tmp2
  call avgX1X2(r_tmp1,transtmp3,'wc')

 !TKE 
  transtmp = 1.d0/2.d0*(transtmp1+transtmp2+transtmp3)
  dkdt = ( transtmp-k_old ) / (time-told)
  k_old = transtmp

 !t'  
   call avgX1X2(rho,mean,'rf')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      r_tmp1(i,j,k)=rho(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(r_tmp1,'cfluc',err1)

 !<t't'>
  r_tmp1 = r_tmp1*r_tmp1
  call avgX1X2(r_tmp1,transtmp,'rf')
  dt2dt = ( transtmp-t2_old) / (time-told)
  t2_old = transtmp

 !s'  
   call avgX1X2(scal1,mean,'sf')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      r_tmp1(i,j,k)=scal1(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(r_tmp1,'cfluc',err1)

 !<s's'>
  r_tmp1 = r_tmp1*r_tmp1
  call avgX1X2(r_tmp1,transtmp,'sf')
  ds2dt = ( transtmp-s2_old) / (time-told)
  s2_old = transtmp
  
 !Explicitly damp the pressure then call psponge to move the region where p is forced to zero 
 !around otherwise the region near the corners where the divergence builds up just moves
! r_tmp1=p
! var=4 
! do dir=1,3
!  do face=1,2
!   call Sponge(p,r_tmp1,4,dir,face,err)
!  enddo
! enddo
!endif
! call psponge(pstep)
! pstep = pstep + 1
! if (pstep.GE.10) pstep = 1
enddo

1000 continue
stat=ok
return
end subroutine NSsolver

! ec does nothing why is it here KYLE?
subroutine rk_ss(a1,a2,a3,ec,dt_rk,dt,psolve,stat)
!@t
! \textbf{subroutine rk\_ss(a1,a2,a3,ec,dt,psolve,stat)}
!@h
!   Description:
!     Perform one RK sub-step.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes,    only: r8,i4
 use Flow 
 use LESmod
 use IO,        only: IOUT
 use Parameters, only: RK3,RK3CN,pcorrect,delt
 use domain,    only: sx,ex,sy,ey,sz,ez,nyp2, nxp2, nzp2
 implicit none

!Passed Variables
 real(r8),intent(in)                   :: a1, a2, a3, ec, dt_rk, dt
 integer(i4),intent(out)               :: stat
 logical,intent(in)                    :: psolve

!Local Variables
 integer                               :: err1
 integer(i4)                           :: niters
 real(r8)                              :: resmax
 integer                               :: s1
 real(r8),allocatable,dimension(:,:,:) :: Ftemp
 logical,parameter                     :: debug=.false.
 real(r8)                              :: iflag
 integer(i4)                           :: i,j,k
 real(r8)                              :: qsat, Exner, delta_q, Latv, c

 if (debug) call check_point('rk_ss#0',.false.)
 err1=0
 s1=0

 allocate( Ftemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error allocating Ftemp in rk_ss"
  goto 1000
 endif

 r_tmp1(:,:,:) = 0.d0

 !X1-momentum 
 call u1_rhs(u,v,w,p,rho,scal1,Ftemp,err1)
 if (RK3)  call Psource2(Ftemp,u,r_tmp1,dt,1,err1)
 call var_advance(Ftemp,u1_tmp2,a1,a2,dt)
  if (debug) call write_plane(Ftemp,1,nxp2/2,0,1,'NS1',.false.,err1)
  if (debug) call check_point('rk_ss#1',.false.)

 !X2-momentum 
 call u2_rhs(u,v,w,p,rho,scal1,Ftemp,err1)
 if (RK3) call Psource2(Ftemp,v,r_tmp1,dt,2,err1)
 call var_advance(Ftemp,u2_tmp2,a1,a2,dt)
  if (debug) call write_plane(Ftemp,1,nxp2/2,0,1,'NS2',.false.,err1)
  if (debug) call check_point('rk_ss#2',.false.)

 !X3-momentum 
 call u3_rhs(u,v,w,p,rho,scal1,Ftemp,err1)
 if (RK3) call Psource2(Ftemp,w,r_tmp1,dt,3,err1)
 call var_advance(Ftemp,u3_tmp2,a1,a2,dt)
  if (debug) call write_plane(Ftemp,1,nxp2/2,0,1,'NS3',.false.,err1)
  if (debug) call check_point('rk_ss#3',.false.)

 !Density/Temperature/Passive Scalar
 iflag = 0.0
 call rho_rhs(u,v,w,at,rcond,pzz,er,iflag,rho,scal1,Ftemp,err1)
 call var_advance(Ftemp,r_tmp2,a1,a2,dt) 
  if (debug) call write_plane(r_tmp2,1,nxp2/2,0,1,'NSr',.false.,err1)
  if (debug) call check_point('rk_ss#8',.false.)

 call qv_rhs(u,v,w,rcond,er,iflag,qv,scal1,Ftemp,err1)
 call var_advance(Ftemp,q_tmp2,a1,a2,dt)
  if (debug) call write_plane(q_tmp2,1,nxp2/2,0,1,'NSr',.false.,err1)
  if (debug) call check_point('rk_ss#8',.false.)

! call ql_rhs(u,v,w,rcond,ar,cr,iflag,ql,Ftemp,err1)
! call var_advance(Ftemp,q_tmp1,a1,a2,dt)
!  if (debug) call write_plane(q_tmp1,1,nxp2/2,0,1,'NSr',.false.,err1)
!  if (debug) call check_point('rk_ss#8',.false.)

 !Density/Temperature/Passive Scalar
! call scal1_rhs(u,v,w,rho,scal1,Ftemp,err1)
! call var_advance(Ftemp,scal1_tmp2,a1,a2,dt) 
!  if (debug) call write_plane(scal1_tmp2,1,nxp2/2,0,1,'NSscal1',.false.,err1)
!  if (debug) call check_point('rk_ss#9',.false.)
 
 call ghost(u1_tmp2,'utemp',err1)
 call ghost(u2_tmp2,'vtemp',err1)
 call ghost(u3_tmp2,'wtemp',err1)
 call ghost(r_tmp2,'rtemp',err1)
 call ghost(q_tmp2,'qtemp',err1)
 call ghost(q_tmp1,'qtemp',err1)
 call ghost(scal1_tmp2,'s1temp',err1)
 
!Viscous Implicit
 if (RK3CN) then
  call var_advance_CN_PL(u1_tmp2,u,1.d0,a3,1.d0,dt_rk,1)
  call var_advance_CN_PL(u2_tmp2,v,1.d0,a3,1.d0,dt_rk,2)
  call var_advance_CN_PL(u3_tmp2,w,1.d0,a3,1.d0,dt_rk,3)
  r_tmp1 = 0.d0
  call Psource(u,v,w,r_tmp1,dt_rk,err1)
 endif
 if (RK3CN) call var_advance_CN_PL(r_tmp2,iflag0_rho,1.d0,a3,1.d0,dt_rk,5)
 if (RK3CN) call var_advance_CN_PL(scal1_tmp2,scal1,1.d0,a3,1.d0,dt_rk,8)
 if (RK3CN) call var_advance_CN_PL(q_tmp2,iflag0_qv,1.d0,a3,1.d0,dt_rk,9)
! if (RK3CN) call var_advance_CN_PL(q_tmp1,iflag0_ql,1.d0,a3,1.d0,dt_rk,10)

 do k=sz-1,ez+1
  do j=sy-1,ey+1
   do i=sx-1,ex+1
    pzz(i,j,k) = pz(i,j,k)+p(i,j,k)
    Exner = (pzz(i,j,k)/101325.0)**(287.0/1004.0)
    at(i,j,k) = iflag0_rho(i,j,k)*Exner
    Latv = 2.5*(10.0)**6.0 - (1885.0-4186.0)*(at(i,j,k)-273.15)
    qsat = 3.80/(pzz(i,j,k)*0.01)*exp(17.27*(at(i,j,k)-273.0)/(at(i,j,k)-36.0))
    if(iflag0_qv(i,j,k).gt.qsat.or.iflag0_qv(i,j,k).eq.qsat) then
     delta_q = (iflag0_qv(i,j,k)-qsat)/(1.0+(qsat*Latv**2.0)/(1004.0*461.0*at(i,j,k)**2.0))
     rcond(i,j,k) = delta_q/delt
    elseif(iflag0_qv(i,j,k).lt.qsat) then
     rcond(i,j,k) = 0.0
    endif
   enddo
  enddo
 enddo
 !write(*,*), maxval(iflag0_rho(:,:,:)),maxval(r_tmp2(:,:,:))
 rcond = 0
 iflag = 1.0
 call rho_rhs(u,v,w,at,rcond,pzz,er,iflag,rho,scal1,Ftemp,err1)
 call var_advance(Ftemp,r_tmp2,a1,a2,dt)
 if (debug) call write_plane(r_tmp2,1,nxp2/2,0,1,'NSr',.false.,err1)
 if (debug) call check_point('rk_ss#8',.false.)

 call qv_rhs(u,v,w,rcond,er,iflag,qv,scal1,Ftemp,err1)
 call var_advance(Ftemp,q_tmp2,a1,a2,dt)
  if (debug) call write_plane(q_tmp2,1,nxp2/2,0,1,'NSr',.false.,err1)
  if (debug) call check_point('rk_ss#8',.false.)

! call ql_rhs(u,v,w,rcond,ar,cr,iflag,ql,Ftemp,err1)
! call var_advance(Ftemp,q_tmp1,a1,a2,dt)
!  if (debug) call write_plane(q_tmp1,1,nxp2/2,0,1,'NSr',.false.,err1)
!  if (debug) call check_point('rk_ss#8',.false.)

! call qr_rhs(u,v,w,er,ar,cr,vrr,qr,Ftemp,err1)
! call var_advance(Ftemp,q_tmp3,a1,a2,dt)
!  if (debug) call write_plane(q_tmp3,1,nxp2/2,0,1,'NSr',.false.,err1)
!  if (debug) call check_point('rk_ss#8',.false.)

 call ghost(r_tmp2,'rtemp',err1)
 call ghost(q_tmp2,'qtemp',err1)
! call ghost(q_tmp1,'qtemp',err1)
! call ghost(q_tmp3,'qtemp',err1)

 if (RK3CN) call var_advance_CN_PL(r_tmp2,rho,1.d0,a3,1.d0,dt_rk,5) 
 if (RK3CN) call var_advance_CN_PL(q_tmp2, qv,1.d0,a3,1.d0,dt_rk,9)
! if (RK3CN) call var_advance_CN_PL(q_tmp1, ql,1.d0,a3,1.d0,dt_rk,10)
! if (RK3CN) call var_advance_CN_PL(q_tmp3, qr,1.d0,a3,1.d0,dt_rk,11)

! write(*,*),"ACTUAL RHO",maxval(rho(:,:,:)) 
 do k=sz-1,ez+1
  do j=sy-1,ey+1
   do i=sx-1,ex+1
!    Exner = (pzz(i,j,k)/101325.0)**(287.0/1004.0)
!    at(i,j,k) = rho(i,j,k)*Exner
!    Latv = 2.5*(10.0)**6.0 - (1885.0-4186.0)*(at(i,j,k)-273.15)
!    qsat = 3.80/(pzz(i,j,k)*0.01)*exp(17.27*(at(i,j,k)-273.0)/(at(i,j,k)-36.0))
!    if(ql(i,j,k).gt.0.001) then
!     ar(i,j,k) = 0.001*(ql(i,j,k)-0.001)
!     cr(i,j,k) = 2.2*ql(i,j,k)*qr(i,j,k)**(0.875)
!     c = 1.6+124.9*(0.001*r(i,j,k)*qr(i,j,k))**(0.2046)
!     er(i,j,k) = 1.0/(r(i,j,k)*0.001) &
!                 *(1.0-qv(i,j,k)/qsat)*c*(0.001*r(i,j,k)*qr(i,j,k))**(0.525) &
!                 /(5.4*100000.0+2.55*1000000/(pzz(i,j,k)*0.01*qsat))
!     vrr(i,j,k) = 0.01*3634*(0.001*r(i,j,k)*qr(i,j,k))**(0.1346)*(r(i,j,k)/r(i,j,sz))**(-0.5) !!!!!!DO NOT SPLIT IN Z!!!!!!!
!    else
     ar(i,j,k) = 0.0
     cr(i,j,k) = 0.0
     er(i,j,k) = 0.0
     vrr(i,j,k) = 0.0
!    endif
   enddo
  enddo
 enddo
 !Pressure Poisson Equation
  if (RK3) Ftemp = p 
  if (RK3CN) Ftemp = p
  if (RK3CN.and.pcorrect) Ftemp = 0.d0
  call mg_solver(Ftemp,r_tmp1,niters,resmax)
  write(IOUT,'(a20,i3,a11,e22.15)') "    MG Iterations: ",niters,"  RESIDUAL= ",resmax
  if (debug) call write_plane(p,1,nxp2/2,0,1,'p1',.false.,err1)
  if (debug) call write_pencil(p,3,nxp2/2,nyp2/2,0,'p',.false.,err1)
  if (debug) call check_point('rk_ss#6',.false.)

 !Corrector steps: project pressure gradient 
  if (RK3) then
   p=Ftemp
   call ghost(p,'p',err1)
   call Pgrad(p,u1_tmp2,u2_tmp2,u3_tmp2,dt)
!   call ghost(u1_tmp2,'utemp',err1)
!   call ghost(u2_tmp2,'vtemp',err1)
!   call ghost(u3_tmp2,'wtemp',err1)
!   call ghost(r_tmp2,'rtemp',err1)
!   call ghost(scal1_tmp2,'s1temp',err1)
   call var_advance(u1_tmp2,u,1.d0,a3,1.d0)
   call var_advance(u2_tmp2,v,1.d0,a3,1.d0)
   call var_advance(u3_tmp2,w,1.d0,a3,1.d0)
   call var_advance(r_tmp2,rho,1.d0,a3,1.d0)
   call var_advance(scal1_tmp2,scal1,1.d0,a3,1.d0)
  endif
  if (RK3CN) then
   if (pcorrect) then
    p = Ftemp + p
    call Pgrad(Ftemp,u,v,w,dt_rk)
   else
    p = Ftemp 
    call Pgrad(p,u,v,w,dt_rk)
   endif
   call ghost(u1_tmp2,'utemp',err1)
   call ghost(u2_tmp2,'vtemp',err1)
   call ghost(u3_tmp2,'wtemp',err1)
  endif

  if (debug) call write_plane(u,1,nxp2/2,0,1,'NSa1',.false.,err1)
  if (debug) call write_plane(v,1,nxp2/2,0,1,'NSa2',.false.,err1)
  if (debug) call write_plane(w,1,nxp2/2,0,1,'NSa3',.false.,err1)
  if (debug) call check_point('rk_ss#7',.false.)

 deallocate(Ftemp,stat=s1)
 if (s1.NE.0) then
  write(IOUT,*) "Error de-allocating Ftemp in rk_ss"
  goto 1000
 endif


 !NOTE: subdomains must pass boundary data since the information in the ghost cells is used 
 !in the next substep
 call ghost(u,'u',err1)
 call ghost(v,'v',err1)
 call ghost(w,'w',err1)
 call ghost(rho,'rho',err1)
 call ghost(scal1,'scal1',err1)
 call ghost(qv,'qv',err1)
! call ghost(ql,'ql',err1)
! call ghost(qr,'qr',err1)

  if (debug) call check_point('rk_ss#11',.false.)

 1000 continue
 stat=max(err1,s1)
 return
end subroutine rk_ss

subroutine u1_rhs(un,vn,wn,pn,rhon,scal1n,Utemp,stat)
!@t
! \textbf{subroutine u1\_rhs(un,vn,wn,Utemp,stat)}
!@h
!   Description:
!     Updates the right hand side for u1.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes,     only: r8
 use Domain,     only: sx,ex,sy,ey,sz,ez,EU,nzp2
 use Grid,       only: rdxe,rdxc,rdye,rdyc,rdze,rdzc
 use Grid,       only: dxe,dxc,dye,dyc,dze,dzc,ze,zc,yc
 use Parameters, only: rRe, Rsponge, MX3c,RK3,RK3CN,LESflag,time
 use Langmuir_params,   only: langmuir, u_stokes, v_stokes, dus_dz, dvs_dz
 use Coriolis_params
 use ratios,     only: r_1_2
 use Flow,       only: Vmodel,r
 use LESmod,     only: S11,S12,S13,modS,Csgs,delg,r_dgt_dg,nuT
 use LESmod,     only: lestmp,lestmp1,lestmp2,lestmp3,lestmp4,xfil,yfil,zfil
 use IO,  only: IOUT
 use DD,  only: comm3d,sizeX3,coords
 implicit none

!Passed Variables
 real(r8),intent(in)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: pn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: rhon(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: scal1n(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: Utemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)        :: stat

!Local Variables
 integer                   :: i,j,k
 real(r8)                  :: uaver1, uaver2, vaver1, vaver2, waver1, waver2, raver1, raver2, Saver, nuTaver1, nuTaver2
 integer                   :: dir,face,var,err
 real(r8)                  :: tmp1, tmp2, tmp3, tmp4, del_g, wst1, wst2
 real(r8)            :: mean(sz-1:ez+1),rms(sz-1:ez+1),Deddy(sz-1:ez+1)
 real(r8)            :: meanU(sz-1:ez+1),meanV(sz-1:ez+1) 
 real(r8)            :: meanXZ(sx-1:ex+1,sz-1:ez+1)

 real(r8)            :: Um,Uu,Ul,Ud,Zmu,Zul,Zld,Dmu,Dul,Dld,zz


 Utemp=0.d0

 if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
  !*********************************
  !   Linear Terms                 *
  !*********************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex-EU
     !d2u/dx2
      Utemp(i,j,k) = Utemp(i,j,k) + rRe*( ( un(i+1,j,k) - un(i,j,k) )*rdxc(i+1) &
                   -( un(i,j,k) - un(i-1,j,k) )*rdxc(i) )*rdxe(i)               &
     !d2u/dy2
                                  + rRe*( ( un(i,j+1,k) - un(i,j,k) )*rdye(j)   &
                  -( un(i,j,k) - un(i,j-1,k) )*rdye(j-1) )*rdyc(j)             
     enddo
    enddo
   enddo
  
   if (RK3) then
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex-EU
       !d2u/dz2
       Utemp(i,j,k) = Utemp(i,j,k) + rRe*( ( un(i,j,k+1) - un(i,j,k) )*rdze(k)   &
                                          -( un(i,j,k) - un(i,j,k-1) )*rdze(k-1) )*rdzc(k)
      enddo 
     enddo
    enddo
  endif
    
  !*********************************
  !*  Non-linear Terms             *
  !*********************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex-EU
     !d(u*u)/dx
     !Fully 2nd Order [Bewley EQ. 13.3]
      uaver2 = r_1_2*( un(i+1,j,k) + un(i,j,k) )
      uaver1 = r_1_2*( un(i,j,k) + un(i-1,j,k) )
      Utemp(i,j,k) = Utemp(i,j,k) + (uaver1*uaver1 - uaver2*uaver2 )*rdxe(i)

     !d(w*u)/dz
     !Quasi 2nd Order [Bewley EQ. 13.4]
      uaver2 = r_1_2*(un(i,j,k+1) + un(i,j,k))       
      uaver1 = r_1_2*(un(i,j,k) + un(i,j,k-1))      
      raver2 = r_1_2*(r(i,j,k+1) + r(i,j,k))
      raver1 = r_1_2*(r(i,j,k) + r(i,j,k-1)) 
      waver2 = r_1_2*(wn(i+1,j,k) + wn(i,j,k))       
      waver1 = r_1_2*(wn(i+1,j,k-1) + wn(i,j,k-1))   
      Utemp(i,j,k) = Utemp(i,j,k) + ( raver1*uaver1*waver1 - raver2*uaver2*waver2 )*rdzc(k)/r(i,j,k)

     !d(u*v)/dy
     !Quasi 2nd Order [Bewley EQ. 13.4]
      uaver2 = r_1_2*(un(i,j+1,k) + un(i,j,k))    
      uaver1 = r_1_2*(un(i,j,k) + un(i,j-1,k))
      vaver2 = r_1_2*(vn(i+1,j,k) + vn(i,j,k))
      vaver1 = r_1_2*(vn(i+1,j-1,k) + vn(i,j-1,k))
      Utemp(i,j,k) = Utemp(i,j,k) + ( uaver1*vaver1 - uaver2*vaver2 )*rdyc(j)

     enddo
    enddo
   enddo


  !*********************************
  !   Coriolis terms                *
  !*********************************
   if (coriolis) then
    do k=sz,ez; do j=sy,ey; do i=sx,ex-EU
     !f and Beta
     tmp1 = 0.5d0*( vn(i,j,k) + vn(i,j-1,k) )
     tmp2 = 0.5d0*( vn(i+1,j,k) + vn(i+1,j-1,k) )
     tmp3 = tmp1 + r_1_2*dxc(i)*rdxe(i)*(tmp2-tmp1)
     tmp1 = fPln*2.d0*7.292d-5*sin(lat) & 
           +bPln*2.d0*7.292d-5*cos(lat)*yc(j)/6.378d6
     Utemp(i,j,k) = Utemp(i,j,k) + tmp1*( tmp3 + v_stokes(k,1) ) 
     !f tilde
     tmp1 = 0.5d0*( wn(i,j,k) + wn(i,j,k-1) )
     tmp2 = 0.5d0*( wn(i+1,j,k) + wn(i+1,j,k-1) )
     tmp3 = tmp1 + r_1_2*dxc(i)*rdxe(i)*(tmp2-tmp1)
     tmp1 = ftPln*2.d0*7.292d-5*cos(lat)
     Utemp(i,j,k) = Utemp(i,j,k) - tmp1*tmp3  
    enddo; enddo; enddo
   endif

  !*********************************
  !   Langmuir terms               *
  !*********************************
   if (langmuir) then 
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex-EU
       !dv_dx
       tmp1 = r_1_2*( vn(i,j,k) + vn(i,j-1,k) )
       tmp2 = r_1_2*( vn(i+1,j,k) + vn(i+1,j-1,k) )
       tmp3 = (tmp2 - tmp1) * rdxe(i)
       !du_dy
       tmp1 = un(i,j-1,k) + r_1_2*dyc(j-1)*rdye(j-1)*( un(i,j,k) - un(i,j-1,k) )
       tmp2 = un(i,j,k) + r_1_2*dyc(j)*rdye(j)*( un(i,j+1,k) - un(i,j,k) )
       tmp4 = (tmp2 - tmp1) * rdyc(j)
       !add vs*omg3
       Utemp(i,j,k) = Utemp(i,j,k) + v_stokes(k,1)*(tmp3-tmp4) 
       !du_dx
       tmp1 = r_1_2*( un(i-1,j,k) + un(i,j,k))
       tmp2 = r_1_2*( un(i+1,j,k) + un(i,j,k))
       tmp3 = (tmp2 - tmp1)*rdxe(i)
       !substract us*du_dx
       Utemp(i,j,k) = Utemp(i,j,k)-  u_stokes(k,1)*tmp3 
       !dv_dx
       tmp1 = r_1_2*( vn(i,j,k) + vn(i,j-1,k) )
       tmp2 = r_1_2*( vn(i+1,j,k) + vn(i+1,j-1,k) )
       tmp3 = (tmp2 - tmp1) * rdxe(i)
       !substract vs*dv_dx
       Utemp(i,j,k) = Utemp(i,j,k) - v_stokes(k,1)*tmp3
      enddo
     enddo
    enddo
   endif

  if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
   !************************************
   !*     LES TERMS                    * 
   !************************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex-EU
     !-dtau11/dx 
      Utemp(i,j,k) = Utemp(i,j,k) - LESflag*2.d0*( nuT(i+1,j,k)*(un(i+1,j,k)-un(i,j,k))*rdxc(i+1) &
                                          -nuT(i,j,k)*(un(i,j,k)-un(i-1,j,k))*rdxc(i) )*rdxe(i)
 
     !-dtau12/dy
      nuTaver1 = 0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j-1,k)+nuT(i+1,j-1,k) )
      nuTaver2 = 0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j+1,k)+nuT(i+1,j+1,k) )
      Utemp(i,j,k) = Utemp(i,j,k) - LESflag*1.d0*( nuTaver2*(un(i,j+1,k)-un(i,j,k))*rdye(j) &
                                          -nuTaver1*(un(i,j,k)-un(i,j-1,k))*rdye(j-1) )*rdyc(j)
      Utemp(i,j,k) = Utemp(i,j,k) - LESflag*1.d0*( nuTaver2*(vn(i+1,j+1,k)-vn(i,j+1,k))*rdxe(i) &
                                          -nuTaver1*(vn(i+1,j,k)-vn(i,j,k))*rdxe(i) )*rdyc(j)

     !-dtau13/dz: 1st portion marchs with CN  
      nuTaver1 = 0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k-1)+nuT(i+1,j,k-1) )
      nuTaver2 = 0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k+1)+nuT(i+1,j,k+1) )
      raver2 = r_1_2*(r(i,j,k+1) + r(i,j,k))
      raver1 = r_1_2*(r(i,j,k) + r(i,j,k-1))
      !Utemp(i,j,k) = Utemp(i,j,k) - LESflag*1.d0*( nuTaver2*(un(i,j,k+1)-un(i,j,k))*rdze(k)&
      !                                    -nuTaver1*(un(i,j,k)-un(i,j,k))*rdze(k-1) )*rdzc(k)
      Utemp(i,j,k) = Utemp(i,j,k) - LESflag*1.d0*( raver2*nuTaver2*(wn(i+1,j,k)-wn(i,j,k))*rdxe(i) &
                                          -raver1*nuTaver1*(wn(i+1,j,k-1)-wn(i,j,k-1))*rdxe(i) )*rdzc(k)/r(i,j,k)
     enddo
    enddo
   enddo
   
   if (RK3) then
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex-EU
       nuTaver1 = 0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k-1)+nuT(i+1,j,k-1) )
       nuTaver2 = 0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k+1)+nuT(i+1,j,k+1) )
       Utemp(i,j,k) = Utemp(i,j,k) - LESflag*1.d0*( nuTaver2*(un(i,j,k+1)-un(i,j,k))*rdze(k)&
                                           -nuTaver1*(un(i,j,k)-un(i,j,k))*rdze(k-1) )*rdzc(k)
      enddo
     enddo
    enddo
   endif
  
  elseif (Vmodel.NE.'DNS') then
   write(IOUT,'(a)') "ABORTING U1_RHS, Vmodel = " //trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif
  
  if (Vmodel.EQ.'DMM') then
   !************************************
   !Scale-similarity part: -tauSS_1j=-d( (ug_1*ug_j)_g - ugg_1*ugg_j)/dx_j
   !************************************

  do k = sz,ez; do j = sy,ey; do i = sx,ex-EU
   !-d(tauSS_11)/dx
    tmp2 = S11(i+1,j,k)
    tmp1 = S11(i,j,k)
    Utemp(i,j,k) = Utemp(i,j,k) - (tmp2-tmp1)*rdxe(i)

   !-dtauSS_12)/dy
    tmp2 = S12(i,j,k) 
    tmp1 = S12(i,j-1,k) 
    Utemp(i,j,k) = Utemp(i,j,k) - (tmp2-tmp1)*rdyc(j)

   !-d(tauSS_13)/dz
    tmp2 = S13(i,j,k) 
    tmp1 = S13(i,j,k-1) 
    Utemp(i,j,k) = Utemp(i,j,k) - (tmp2-tmp1)*rdzc(k)
   enddo; enddo; enddo

  elseif (Vmodel.NE.'DNS'.and.Vmodel.NE.'SSM'.and.Vmodel.NE.'DSM') then
   write(IOUT,'(a)') "ABORTING U1_RHS, Vmodel = " //trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
  return
 endif
   
 if (Rsponge) then
    !*********************************
    !*  Sponge Terms                 *
    !*********************************
    var=1 !u=Flow(:,:,:,5)
    do dir=1,3
     do face=1,2
      call Sponge(Utemp,un,var,dir,face,err)
     enddo
    enddo
   endif
  else
   write(IOUT,'(a)') "ABORTING U1_RHS, Vmodel = " //trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif

 call ghost(Utemp,'utemp',err)
 stat=err
 return
end subroutine u1_rhs


subroutine u2_rhs(un,vn,wn,pn,rhon,scal1n,Vtemp,stat)
!@t
! \textbf{subroutine u2\_rhs(un,vn,wn,Vtemp,stat)}
!@h
!   Description:
!     Updates the right hand side for u2.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
 
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
 
!   Language:
!     Fortran 90

 use ntypes,     only: r8
 use Domain,     only: sx,ex,sy,ey,sz,ez,EV,nzp2
 use Grid,       only: rdxe,rdxc,rdye,rdyc,rdze,rdzc
 use Grid,       only: dxe,dxc,dye,dyc,dze,dzc,zc,ye
 use Parameters, only: rRe, Rsponge,RK3,RK3CN,LESflag,time
 use Coriolis_params
 use Langmuir_params, only: langmuir, u_stokes, v_stokes
 use ratios,      only: r_1_2
 use Flow,       only: Vmodel,r
 use LESmod,     only: S12,S22,S23,modS,Csgs,delg,r_dgt_dg,nuT
 use LESmod,     only: lestmp,lestmp1,lestmp2,lestmp3,lestmp4,xfil,yfil,zfil
 use IO,         only: IOUT
 use DD,         only: sizeX3,comm3d,coords
 implicit none

!Passed Variables
 real(r8),intent(in)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: pn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: rhon(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: scal1n(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: Vtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)        :: stat
!Local Variables
 integer                    :: i,j,k
 real(r8)                   :: uaver1, uaver2, vaver1, vaver2, waver1, raver1, raver2, waver2, nuTaver1, nuTaver2, Saver
 integer                    :: dir,face,var,err
 real(r8)                   :: tmp1, tmp2, tmp3, tmp4, del_g
 real(r8)                   :: meanU(sz-1:ez+1),meanV(sz-1:ez+1) 
 real(r8)                   :: Deddy(sz-1:ez+1) 

 Vtemp = 0.d0

 if(Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
  !*********************************
  !  Wind stress                   *
  !*********************************
!  meanU=0.d0; meanV=0.d0
!  call avgX1X2(un,meanU,'u')
!  call avgX1X2(vn,meanV,'v')
!  if (coords(3).eq.sizeX3-1 .and. time.GE.0.0d0) then
!   do j=sy,ey-EV; do i=sx,ex
!    tmp1 = 0.1d0/0.0033*(vn(i+1,j,ez)-meanV(ez))
!    tmp2 = -0.0d0**2.d0*(1.d0-tmp1)
!    Vtemp(i,j,ez) = Vtemp(i,j,ez) + tmp2*rdzc(ez) 
!   enddo; enddo
!  endif

   !***********************************
   !*      Linear Terms               *
   !*********************************** 
   do k=sz,ez 
    do j=sy,ey-EV
     do i=sx,ex
     !d2v/dx2
      Vtemp(i,j,k) = Vtemp(i,j,k) + rRe*( ( vn(i+1,j,k) - vn(i,j,k) )*rdxe(i) &
                   -( vn(i,j,k) - vn(i-1,j,k) )*rdxe(i-1) )*rdxc(i) &
     !d2v/dy2
                   + rRe*( ( vn(i,j+1,k) - vn(i,j,k) )*rdyc(j+1) &
                   - ( vn(i,j,k) - vn(i,j-1,k) )*rdyc(j) )*rdye(j) 
     !d2v/dz2: march with CN
     !             + rRe*( ( vn(i,j,k+1) - vn(i,j,k) )*rdze(k) &
     !             - ( vn(i,j,k) - vn(i,j,k-1) )*rdze(k-1) )*rdzc(k)
     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez 
     do j=sy,ey-EV
      do i=sx,ex
     !d2v/dz2
      Vtemp(i,j,k) = Vtemp(i,j,k) + rRe*( ( vn(i,j,k+1) - vn(i,j,k) )*rdze(k) &
                                        - ( vn(i,j,k) - vn(i,j,k-1) )*rdze(k-1) )*rdzc(k)
      enddo
     enddo
    enddo
   endif
   
 
   !***********************************
   !*       Non-linear Terms          *
   !*********************************** 
   do k=sz,ez 
    do j=sy,ey-EV
     do i=sx,ex
     !v*dv/dy
      vaver1 = 0.5d0*( vn(i,j+1,k) + vn(i,j,k) )
      vaver2 = 0.5d0*( vn(i,j,k) + vn(i,j-1,k) )
      Vtemp(i,j,k) = Vtemp(i,j,k) - ( vaver1*vaver1 - vaver2*vaver2 )*rdye(j)
     !u*dv/dx
      vaver1 = 0.5d0*(vn(i+1,j,k) + vn(i,j,k))
      vaver2 = 0.5d0*(vn(i,j,k) + vn(i-1,j,k))
      uaver1 = 0.5d0*(un(i,j+1,k) + un(i,j,k))
      uaver2 = 0.5d0*(un(i-1,j+1,k) + un(i-1,j,k))
      Vtemp(i,j,k) = Vtemp(i,j,k) - ( uaver1*vaver1 - uaver2*vaver2 )*rdxc(i)
     !w*dv/dz
      vaver1 = 0.5d0*(vn(i,j,k+1) + vn(i,j,k))
      vaver2 = 0.5d0*(vn(i,j,k) + vn(i,j,k-1))
      raver1 = 0.5d0*(r(i,j,k+1) + r(i,j,k))
      raver2 = 0.5d0*(r(i,j,k) + r(i,j,k-1))
      waver1 = 0.5d0*(wn(i,j+1,k) + wn(i,j,k))
      waver2 = 0.5d0*(wn(i,j+1,k-1) + wn(i,j,k-1))
      Vtemp(i,j,k) = Vtemp(i,j,k) - ( raver1*waver1*vaver1 - raver2*waver2*vaver2 )*rdzc(k)/r(i,j,k)
     enddo
    enddo
   enddo

   !***********************************
   !*       Coriolis Term             *
   !*********************************** 
   if (coriolis) then
    !only f and Beta
    do k=sz,ez; do j=sy,ey-EV; do i=sx,ex
     tmp1 = 0.5d0*( un(i,j,k) + un(i-1,j,k) )
     tmp2 = 0.5d0*( un(i,j+1,k) + un(i-1,j+1,k) )
     tmp3 = tmp1 + 0.5d0*dyc(j)*rdye(j)*(tmp2-tmp1)
     tmp1 = fPln*2.d0*7.292d-5*sin(lat) & 
           +bPln*2.d0*7.292d-5*cos(lat)*ye(j)/6.378d6
     Vtemp(i,j,k) = Vtemp(i,j,k) - tmp1*(tmp3 + u_stokes(k,1))
    enddo; enddo; enddo
   endif

   !***********************************
   !*       Langmuir Terms            *
   !*********************************** 
   if (langmuir) then
   do k=sz,ez 
    do j=sy,ey-EV
     do i=sx,ex
      !dv_dx
      tmp1 = vn(i-1,j,k) + 0.5d0*dxc(i-1)*rdxe(i-1)*(vn(i,j,k)-vn(i-1,j,k))
      tmp2 = vn(i,j,k) + 0.5d0*dxc(i)*rdxe(i)*(vn(i+1,j,k)-vn(i,j,k))
      tmp3 = (tmp2-tmp1)*rdxc(i)
      !du_dy
      tmp1 = 0.5d0*( un(i,j,k) + un(i-1,j,k) )
      tmp2 = 0.5d0*( un(i-1,j+1,k) + un(i,j+1,k))
      tmp4 = (tmp2-tmp1)*rdye(j)
      !substract us*omg_3
      Vtemp(i,j,k) = Vtemp(i,j,k) - u_stokes(k,1)*(tmp3-tmp4)
      !du_dy
      tmp1 = 0.5d0*( un(i,j,k) + un(i-1,j,k) )
      tmp2 = 0.5d0*( un(i-1,j+1,k) + un(i,j+1,k))
      tmp4 = (tmp2-tmp1)*rdye(j)
      !substract us*du_dy
      Vtemp(i,j,k) = Vtemp(i,j,k) - u_stokes(k,1)*tmp4
      !dv_dy
      tmp1 = 0.5d0*( vn(i,j-1,k)+vn(i,j,k) )
      tmp2 = 0.5d0*( vn(i,j,k)+vn(i,j+1,k) )
      tmp3 = (tmp2-tmp1)*rdye(j)
      !substract vs*dv_dy
      Vtemp(i,j,k) = Vtemp(i,j,k) - v_stokes(k,1)*tmp3
     enddo
    enddo
   enddo
   endif

  if(Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
  !************************************
  !*      LES TERMS                   *
  !************************************

   do k=sz,ez
    do j=sy,ey-EV
     do i=sx,ex
     !-dtau21/dx
      nuTaver1 = 0.25d0*( nuT(i,j,k)+nuT(i-1,j,k)+nuT(i,j+1,k)+nuT(i-1,j+1,k) )
      nuTaver2 = 0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j+1,k)+nuT(i+1,j+1,k) )
      Vtemp(i,j,k) = Vtemp(i,j,k) - LESflag*1.d0*( nuTaver2*(vn(i+1,j,k)-vn(i,j,k))*rdxe(i) &
                                          -nuTaver1*(vn(i,j,k)-vn(i-1,j,k))*rdxe(i-1) )*rdxc(i)
      Vtemp(i,j,k) = Vtemp(i,j,k) - LESflag*1.d0*( nuTaver2*(un(i+1,j+1,k)-un(i+1,j,k))*rdye(j) &
                                          -nuTaver1*(un(i,j+1,k)-un(i,j,k))*rdye(j) )*rdxc(i)
     !-dtau22/dy
      Vtemp(i,j,k) = Vtemp(i,j,k) - LESflag*2.d0*( nuT(i,j+1,k)*(vn(i,j+1,k)-vn(i,j,k))*rdyc(j+1) &
                                          -nuT(i,j,k)*(vn(i,j,k)-vn(i,j-1,k))*rdyc(j) )*rdye(j) 

     !-dtau23/dz : first portion marchs with CN
      nuTaver1 = 0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k-1)+nuT(i,j+1,k-1) )
      nuTaver2 = 0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k+1)+nuT(i,j+1,k+1) )
      raver1 = 0.5d0*(r(i,j,k+1) + r(i,j,k))
      raver2 = 0.5d0*(r(i,j,k) + r(i,j,k-1))
     ! Vtemp(i,j,k) = Vtemp(i,j,k) - LESflag*1.d0*( nuTaver2*(vn(i,j,k+1)-vn(i,j,k))*rdze(k) &
     !                                     -nuTaver1*(vn(i,j,k)-vn(i,j,k-1))*rdze(k-1) )*rdzc(k)
      Vtemp(i,j,k) = Vtemp(i,j,k) - LESflag*1.d0*( raver2*nuTaver2*(wn(i,j+1,k)-wn(i,j,k))*rdye(j) &
                                          -raver1*nuTaver1*(wn(i,j+1,k-1)-wn(i,j,k-1))*rdye(j) )*rdzc(k)/r(i,j,k)
     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez
     do j=sy,ey-EV
      do i=sx,ex
     !-dtau23/dz : first portion marchs with CN
       nuTaver1 = 0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k-1)+nuT(i,j+1,k-1) )
       nuTaver2 = 0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k+1)+nuT(i,j+1,k+1) )
       Vtemp(i,j,k) = Vtemp(i,j,k) - LESflag*1.d0*( nuTaver2*(vn(i,j,k+1)-vn(i,j,k))*rdze(k) &
                                          -nuTaver1*(vn(i,j,k)-vn(i,j,k-1))*rdze(k-1) )*rdzc(k)
      enddo
     enddo
    enddo 
   endif

  elseif (Vmodel.NE.'DNS') then
   write(IOUT,'(a)') "ABORTING U2_RHS, VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif
  
  if(Vmodel.EQ.'DMM') then
   !**************************************
   !Scale-similarity part: -tauSS_2j=-d( (ug_2*ug_j)_g - ugg_2*ugg_j)/dx_j
   !**************************************
   do k = sz,ez; do j = sy,ey-EV; do i = sx,ex
   !-dtauSS_21)/dx
    tmp2 = S12(i,j,k)
    tmp1 = S12(i-1,j,k)
    Vtemp(i,j,k) = Vtemp(i,j,k) - (tmp2-tmp1)*rdxc(i)
  
   !-d(tauSS_22)/dy
    tmp2 = S22(i,j+1,k)
    tmp1 = S22(i,j,k)
   Vtemp(i,j,k) = Vtemp(i,j,k) - (tmp2-tmp1)*rdye(j)

   !-d(tauSS_23)/dz
    tmp2 = S23(i,j,k)
    tmp1 = S23(i,j,k-1)
    Vtemp(i,j,k) = Vtemp(i,j,k) - (tmp2-tmp1)*rdzc(k)
   enddo; enddo; enddo

  elseif (Vmodel.NE.'DNS'.and.Vmodel.NE.'SSM'.and.Vmodel.NE.'DSM') then
   write(IOUT,'(a)') "ABORTING U2_RHS, VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
  return
 endif

   if (Rsponge) then
   !***********************************
   !*  Sponge Terms                   *
   !*********************************** 
    var=2 
    do dir=1,3
     do face=1,2
      call Sponge(Vtemp,vn,var,dir,face,err) 
     enddo
    enddo
   endif
  else
   write(IOUT,'(a)') "ABORTING U2_RHS, VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif

 call ghost(Vtemp,'vtemp',err)
 stat=err
 return
end subroutine u2_rhs 

subroutine u3_rhs(un,vn,wn,pn,rhon,scal1n,Wtemp,stat)
!@t
! \textbf{subroutine u3\_rhs(un,vn,wn,Wtemp,stat)}
!@h
!   Description:
!     Updates the right hand side for u3.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
 
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
 
!   Language:
!     Fortran 90

 use Parameters, only: rRe, g, flow_type, Rsponge, RK3, RK3CN, Texp, Scon, LESflag, time, evv
 use Coriolis_params
 use Langmuir_params, only: langmuir, u_stokes, v_stokes, dus_dz, dvs_dz
 use ntypes,     only: r8 
 use Domain,     only: sx,ex,sy,ey,sz,ez,EW,nzp2
 use Grid,       only: rdxe,rdxc,rdye,rdyc,rdze,rdzc
 use Grid,       only: dxe,dxc,dye,dyc,dze,dzc,zc
 use ratios,     only: r_1_2
 use Flow,       only: densityBG, Vmodel,r,ALPHA,ALPHA_0,at,qv,ql
 use LESmod,     only: S13,S23,S33,modS,modSp,Csgs,delg,r_dgt_dg,nuT
 use LESmod,     only: lestmp,lestmp1,lestmp2,lestmp2,lestmp3,lestmp4,xfil,yfil,zfil
 use IO,         only: IOUT
 use DD,         only: coords,sizeX3
 implicit none

!Passed Variables
 real(r8),intent(in)       :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)       :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)       :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)       :: pn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)       :: rhon(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)       :: scal1n(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)    :: Wtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)       :: stat

!Local Variables
 integer                   :: i,j,k 
 real(r8)                  :: uaver1, uaver2, vaver1, vaver2, waver1, waver2, Saver, nuTaver1, nuTaver2
 integer                   :: dir,face,var,err
 integer,parameter         :: DEBUG=-1
 real(r8)                  :: rho_Z(sz-1:ez+1) 
 real(r8)                  :: scal1_Z(sz-1:ez+1)
 real(r8)                  :: Deddy(sz-1:ez+1)
 real(r8)                  :: meanU(sz-1:ez+1)
 real(r8)                  :: meanV(sz-1:ez+1)
 real(r8)                  :: mean(sz-1:ez+1)
 real(r8)                  :: rms(sz-1:ez+1)
 real(r8)                  :: tmp1, tmp2, tmp3, tmp4, del_g, gtmp

 Wtemp = 0.d0
! ql = 0.0
 if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
  !************************************
  !*   Boussinesq Term                *
  !************************************ 
   do k=sz-1,ez+1; do j=sy-1,ey+1; do i=sx-1,ex+1
    ALPHA(I,J,K) = at(i,j,k)*(1.0+qv(i,j,k)/evv)/(1.0+qv(i,j,k))!+ql(i,j,k))
   enddo; enddo; enddo
!   call ghost(ALPHA,'qr',err)
!   call ghost(ALPHA_0,'qr',err)
   do k=sz,ez-EW; do j=sy,ey; do i=sx,ex
    Wtemp(i,j,k) = Wtemp(i,j,k)+r_1_2*g*((ALPHA(I,J,K+1)-ALPHA_0(I,J,K+1))+(ALPHA(I,J,K)-ALPHA_0(I,J,K)))/ &
                                        (0.5*(ALPHA_0(I,J,K+1)+ALPHA_0(I,J,K)))
   enddo; enddo; enddo

  !************************************
  !*   Boussinesq Term                *
  !************************************ 
 !  call density_mean(rhon,scal1n,scal1_Z,err)
 !  do k=sz,ez-EW; do j=sy,ey; do i=sx,ex
 !   Wtemp(i,j,k) = Wtemp(i,j,k)+r_1_2*g*( (rhon(i,j,k+1)-densityBG(i,j,k+1)) + &
 !                                         (rhon(i,j,k)  -densityBG(i,j,k  )))/ &
 !                                    (r_1_2*(densityBG(i,j,k+1)+densityBG(i,j,k)))
 !  enddo; enddo; enddo
 
   !***********************************
   !*       Coriolis Term             *
   !*********************************** 
   if (coriolis) then
    !only f tilde
    do k=sz,ez-EW; do j=sy,ey; do i=sx,ex
     tmp1 = 0.5d0*( un(i,j,k) + un(i,j,k-1) )
     tmp2 = 0.5d0*( un(i,j+1,k) + un(i,j+1,k-1) )
     tmp3 = tmp1 + 0.5d0*dzc(k)*rdze(k)*(tmp2-tmp1)
     tmp1 = ftPln*2.d0*7.292d-5*cos(lat)
     Wtemp(i,j,k) = Wtemp(i,j,k) + tmp1*(tmp3 + u_stokes(k,1))
    enddo; enddo; enddo
   endif

  !************************************
  !*     Linear Terms                 *
  !************************************ 
   do k=sz,ez-EW
    do j=sy,ey
     do i=sx,ex
     !d2w/dx2
      Wtemp(i,j,k) = Wtemp(i,j,k) + rRe*( ( wn(i+1,j,k) - wn(i,j,k) )*rdxe(i) &
                   - ( wn(i,j,k) - wn(i-1,j,k) )*rdxe(i-1) )*rdxc(i)&
     !d2w/dy2
                   + rRe*( ( wn(i,j+1,k) - wn(i,j,k) )*rdye(j) &
                   - ( wn(i,j,k) - wn(i,j-1,k) )*rdye(j-1) )*rdyc(j)
     !d2w/dz2: march with CN
     !             + rRe*( ( wn(i,j,k+1) - wn(i,j,k) )*rdzc(k+1)  &
     !             -( wn(i,j,k) - wn(i,j,k-1) )*rdzc(k) )*rdze(k) 

     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez-EW
     do j=sy,ey
      do i=sx,ex
      !d2w/dz2
       Wtemp(i,j,k) = Wtemp(i,j,k) + rRe*( ( wn(i,j,k+1) - wn(i,j,k) )*rdzc(k+1)  &
                                        -( wn(i,j,k) - wn(i,j,k-1) )*rdzc(k) )*rdze(k) 
      enddo
     enddo
    enddo
   endif
    
  !************************************
  !*  Non-linear Terms                *
  !************************************
   do k=sz,ez-EW
    do j=sy,ey
     do i=sx,ex
     !w*dw/dz
      waver1 = 0.5d0*( wn(i,j,k+1) + wn(i,j,k) )
      waver2 = 0.5d0*( wn(i,j,k) + wn(i,j,k-1) )
      Wtemp(i,j,k) = Wtemp(i,j,k) - ( r(i,j,k)*waver1*waver1 - r(i,j,k+1)*waver2*waver2 )*rdze(k)/(0.5*(r(i,j,k)+r(i,j,k+1)))
     !u*dw/dx
      waver1 = 0.5d0*(wn(i+1,j,k) + wn(i,j,k))
      waver2 = 0.5d0*(wn(i,j,k) + wn(i-1,j,k))
      uaver1 = 0.5d0*(un(i,j,k+1) + un(i,j,k))
      uaver2 = 0.5d0*(un(i-1,j,k+1) + un(i-1,j,k))
      Wtemp(i,j,k) = Wtemp(i,j,k) - ( uaver1*waver1 - uaver2*waver2 )*rdxc(i)
     !v*dw/dy
      waver1 = 0.5d0*(wn(i,j+1,k) + wn(i,j,k))
      waver2 = 0.5d0*(wn(i,j,k) + wn(i,j-1,k))
      vaver1 = 0.5d0*(vn(i,j,k+1) + vn(i,j,k))
      vaver2 = 0.5d0*(vn(i,j-1,k+1) + vn(i,j-1,k))
      Wtemp(i,j,k) = Wtemp(i,j,k) - ( vaver1*waver1 - vaver2*waver2 )*rdyc(j)
     enddo
    enddo
   enddo

  !************************************
  !*   Langmuir Terms                 *
  !************************************ 
   if (langmuir) then
   do k=sz,ez-EW
    do j=sy,ey
     do i=sx,ex
      !du_dz
      tmp1 = 0.5d0*( un(i-1,j,k)+un(i,j,k) )
      tmp2 = 0.5d0*( un(i-1,j,k+1)+un(i,j,k+1) )
      tmp3 = (tmp2-tmp1)*rdze(k)
      !dw_dx
      tmp1 = wn(i-1,j,k)+0.5d0*dxc(i-1)*rdxe(i-1)*(wn(i,j,k)-wn(i-1,j,k))
      tmp2 = wn(i,j,k)+0.5d0*dxc(i)*rdxe(i)*(wn(i+1,j,k)-wn(i,j,k))
      tmp4 = (tmp2-tmp1)*rdxc(i) 
      !plus us*omg_2
      Wtemp(i,j,k) = Wtemp(i,j,k) + u_stokes(k,2)*(tmp3-tmp4)
      !dw_dy
      tmp1 = wn(i,j-1,k)+0.5d0*dyc(j-1)*rdye(j-1)*(wn(i,j,k)-wn(i,j-1,k))
      tmp2 = wn(i,j,k)+0.5d0*dyc(j)*rdye(j)*(wn(i,j+1,k)-wn(i,j,k))
      tmp3 = (tmp2-tmp1)*rdyc(j)
      !dv_dz
      tmp1 = 0.5d0*( vn(i,j-1,k)+vn(i,j,k) )
      tmp2 = 0.5d0*( vn(i,j-1,k+1)+vn(i,j,k+1) )
      tmp4 = (tmp2-tmp1)*rdze(k)
      !substract vs*omg_1
      Wtemp(i,j,k) = Wtemp(i,j,k) - v_stokes(k,2)*(tmp3-tmp4)
      !substract us*dus_dz and substract vs*dvs_dz
      !Wtemp(i,j,k) = Wtemp(i,j,k) - u_stokes(k,2)*dus_dz(k,2) &
      !                            - v_stokes(k,2)*dvs_dz(k,2)
      !u at w-point
      tmp1 = un(i-1,j,k)+0.5d0*dzc(k)*rdze(k)*(un(i-1,j,k+1)-un(i-1,j,k))
      tmp2 = un(i,j,k)+0.5d0*dzc(k)*rdze(k)*(un(i,j,k+1)-un(i,j,k))
      tmp3 = 0.5d0*(tmp1+tmp2)
      !substract u*dus_dz
      Wtemp(i,j,k) = Wtemp(i,j,k) - dus_dz(k,2)*tmp3
      !du_dz
      tmp1 = 0.5d0*( un(i-1,j,k)+un(i,j,k) )
      tmp2 = 0.5d0*( un(i-1,j,k+1)+un(i,j,k+1) )
      tmp3 = (tmp2-tmp1)*rdze(k)
      !substract us*du_dz
      Wtemp(i,j,k) = Wtemp(i,j,k) - u_stokes(k,2)*tmp3
      !v at w-point
      tmp1 = vn(i,j-1,k)+0.5d0*dzc(k)*rdze(k)*(vn(i,j-1,k+1)-vn(i,j-1,k))
      tmp2 = vn(i,j,k)+0.5d0*dzc(k)*rdze(k)*(vn(i,j,k+1)-vn(i,j,k))
      tmp3 = 0.5d0*(tmp1+tmp2)
      !substract v*dvs_dz
      Wtemp(i,j,k) = Wtemp(i,j,k) - dvs_dz(k,2)*tmp3
      !dv_dz
      tmp1 = 0.5d0*( vn(i,j-1,k)+vn(i,j,k) )
      tmp2 = 0.5d0*( vn(i,j-1,k+1)+vn(i,j,k+1) )
      tmp4 = (tmp2-tmp1)*rdze(k)
      !substract vs*dv_dz
      Wtemp(i,j,k) = Wtemp(i,j,k) - v_stokes(k,2)*tmp4
     enddo
    enddo
   enddo
  endif


 if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
  !************************************
  !*      LES        TERMS            *  
  !************************************
   do k=sz,ez-EW
    do j=sy,ey
     do i=sx,ex
     !-dtau31/dx
      nuTaver1 = 0.25d0*( nuT(i,j,k)+nuT(i-1,j,k)+nuT(i,j,k+1)+nuT(i-1,j,k+1) )
      nuTaver2 = 0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k+1)+nuT(i+1,j,k+1) )
      Wtemp(i,j,k) = Wtemp(i,j,k) - LESflag*1.d0*( nuTaver2*(wn(i+1,j,k)- wn(i,j,k))*rdxe(i) &
                                          -nuTaver1*(wn(i,j,k)-wn(i-1,j,k))*rdxe(i-1) )*rdxc(i)
      Wtemp(i,j,k) = Wtemp(i,j,k) - LESflag*1.d0*( nuTaver2*(un(i+1,j,k+1)-un(i+1,j,k))*rdze(k) &
                                          -nuTaver1*(un(i,j,k+1)-un(i,j,k))*rdze(k) )*rdxc(i)
     !-dtau32/dy
      nuTaver1 = 0.25d0*( nuT(i,j,k)+nuT(i,j-1,k)+nuT(i,j,k+1)+nuT(i,j-1,k+1) )
      nuTaver2 = 0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k+1)+nuT(i,j+1,k+1) )
      Wtemp(i,j,k) = Wtemp(i,j,k) - LESflag*1.d0*( nuTaver2*(wn(i,j+1,k)-wn(i,j,k))*rdye(j) &
                                          -nuTaver1*(wn(i,j,k)-wn(i,j-1,k))*rdye(j-1) )*rdyc(j)
      Wtemp(i,j,k) = Wtemp(i,j,k) - LESflag*1.d0*( nuTaver2*(vn(i,j+1,k+1)-vn(i,j+1,k))*rdze(k) &
                                          -nuTaver1*(vn(i,j,k+1)-vn(i,j,k))*rdze(k) )*rdyc(j)

     !-dtau33/dz: march with CN  
     !Wtemp(i,j,k) = Wtemp(i,j,k) - LESflag*2.d0*( nuT(i,j,k+1)*(wn(i,j,k+1)-wn(i,j,k))*rdzc(k+1)   &
     !                                    -nuT(i,j,k)*(wn(i,j,k)-wn(i,j,k-1))*rdzc(k) )*rdze(k)
     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez-EW
     do j=sy,ey
      do i=sx,ex
      !-dtau33/dz: march with CN  
       Wtemp(i,j,k) = Wtemp(i,j,k) - LESflag*2.d0*( nuT(i,j,k+1)*(wn(i,j,k+1)-wn(i,j,k))*rdzc(k+1)   &
                                          -nuT(i,j,k)*(wn(i,j,k)-wn(i,j,k-1))*rdzc(k) )*rdze(k)
      enddo
     enddo
    enddo
   endif
   
 elseif (Vmodel.NE.'DNS') then
  write(IOUT,'(a)') "ABORTING U3_RHS, VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif

 if (Vmodel.EQ.'DMM') then
  !*************************************
  !Scale-similarity part: -tauSS_3j=-d( (ug_3*ug_j)_g - ugg_3*ugg_j)/dx_j
  !*************************************
  do k = sz,ez-EW; do j = sy,ey; do i = sx,ex
  !-dtauSS_31)/dx
   tmp2 = S13(i,j,k)
   tmp1 = S13(i-1,j,k)
   Wtemp(i,j,k) = Wtemp(i,j,k) - (tmp2 - tmp1) * rdxc(i)
  
  !-d(tauSS_32)/dy
   tmp2 = S23(i,j,k)
   tmp1 = S23(i,j-1,k)
   Wtemp(i,j,k) = Wtemp(i,j,k) - (tmp2 - tmp1) * rdyc(j)
  
  !-d(tauSS_33)/dz
   tmp2 = S33(i,j,k+1)
   tmp1 = S33(i,j,k)
   Wtemp(i,j,k) = Wtemp(i,j,k) - (tmp2 - tmp1) * rdze(k)
  enddo; enddo; enddo

 elseif (Vmodel.NE.'DNS'.and.Vmodel.NE.'SSM'.and.Vmodel.NE.'DSM') then
  write(IOUT,'(a)') "ABORTING U3_RHS, VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif

  if (Rsponge) then
  !*********************************
  !*  Sponge Terms                 *
  !*********************************
   var=3 !w=Flow(:,:,:,3)
   do dir=1,3
    do face=1,2
     call Sponge(Wtemp,wn,var,dir,face,err)
    enddo
   enddo
  endif
 else
  write(IOUT,'(a)') "ABORTING U3_RHS, VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif

 call ghost(Wtemp,'wtemp',err)
 stat=err
 return
end subroutine u3_rhs

subroutine density_mean(rhoFull,scal1Full,density_avg_1d_Z,stat)
!@t
! \textbf{subroutine rho\_mean(rhoFull,rho\_avg\_1d\_Z,stat)}
!@h
!   Description:
!     Calculates the mean value of the density: rho.
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
!     Automatically ghosts rho.
!@q

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez
 use grid,   only: dxc,dyc
 use IO,     only: IOUT
 use ratios, only: r_1_2
 use parameters, only: Texp, Scon
#ifdef PARALLEL
 use dd,     only: MPI_SUM,realtype,commx1x2,sizex1x2
#endif
 implicit none

!Passed Variables
 real(r8),intent(in)           :: rhoFull(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1 )
 real(r8),intent(in)           :: scal1Full(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1 )
 real(r8),intent(out)          :: density_avg_1d_Z(sz-1:ez+1 )
 integer,intent(out)           :: stat

!Local Variables
 integer                       :: i,j,k
 integer                       :: ierr
 real(r8)                      :: Area, rsum
 real(r8)                      :: density_Z_global(sz-1:ez+1)
 integer                       :: rsize

 logical,parameter             :: debug=.false.


 density_avg_1d_Z(:)=0.d0

 Area = 0.d0
 do j=sy-1,ey+1
  do i=sx-1,ex+1
   Area = Area + dxc(i)*dyc(j)
  enddo
 enddo

 do k=sz-1,ez+1
  rsum=0.0d0
  do j=sy-1,ey+1
   do i=sx-1,ex+1
    rsum = rsum + (-rhoFull(i,j,k)*Texp+scal1Full(i,j,k)*Scon)*dxc(i)*dyc(j)
   enddo
  enddo
   density_avg_1d_Z(k) = rsum/Area
 enddo

#ifdef PARALLEL
 rsize=size(density_Z_global)
 call MPI_ALLREDUCE(density_avg_1d_Z(:),density_Z_global(:),rsize,realtype,MPI_SUM,commx1x2,ierr)
 density_avg_1d_Z(:)=density_Z_global(:)/dble(sizex1x2)
#endif

 stat = 0
return
end subroutine density_mean

subroutine rho_mean(rhoFull,rho_avg_1d_Z,stat)
!@t
! \textbf{subroutine rho\_mean(rhoFull,rho\_avg\_1d\_Z,stat)}
!@h
!   Description:
!     Calculates the mean value of the density: rho.
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
!     Automatically ghosts rho.
!@q

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez
 use grid,   only: dxc,dyc
 use IO,     only: IOUT
 use ratios, only: r_1_2
#ifdef PARALLEL
 use dd,     only: MPI_SUM,realtype,commx1x2,sizex1x2
#endif
 implicit none

!Passed Variables
 real(r8),intent(in)           :: rhoFull(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1 )
 real(r8),intent(out)          :: rho_avg_1d_Z(sz-1:ez+1 )
 integer,intent(out)           :: stat

!Local Variables
 integer                       :: i,j,k
 integer                       :: ierr
 real(r8)                      :: Area, rsum
 real(r8)                      :: rho_Z_global(sz-1:ez+1)
 integer                       :: rsize

 logical,parameter             :: debug=.false.

 call ghost(rhoFull,'rf',ierr)    ! KYLE WHY IS THIS NECESSARY?

 rho_avg_1d_Z(:)=0.d0

 Area = 0.d0
 do j=sy-1,ey+1
  do i=sx-1,ex+1
   Area = Area + dxc(i)*dyc(j)
  enddo
 enddo

 do k=sz-1,ez+1
  rsum=0.0d0
  do j=sy-1,ey+1
   do i=sx-1,ex+1
    rsum = rsum + rhoFull(i,j,k)*dxc(i)*dyc(j)
   enddo
  enddo
   rho_avg_1d_Z(k) = rsum/Area
 enddo

#ifdef PARALLEL
 rsize=size(rho_Z_global)
 call MPI_ALLREDUCE(rho_avg_1d_Z(:),rho_Z_global(:),rsize,realtype,MPI_SUM,commx1x2,ierr)
 rho_avg_1d_Z(:)=rho_Z_global(:)/dble(sizex1x2)
#endif

 stat = 0
return
end subroutine rho_mean


subroutine scal1_mean(scal1Full,scal1_avg_1d_Z,stat)
!@t
! \textbf{subroutine scal1\_mean(scal1Full,scal1\_avg\_1d\_Z,stat)}
!@h
!   Description:
!     Calculates the mean value of the scalar1: scal1.
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
!     Automatically ghosts rho.
!@q

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez
 use grid,   only: dxc,dyc
 use IO,     only: IOUT
 use ratios, only: r_1_2
#ifdef PARALLEL
 use dd,     only: MPI_SUM,realtype,commx1x2,sizex1x2
#endif
 implicit none

!Passed Variables
 real(r8),intent(in)           :: scal1Full(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1 )
 real(r8),intent(out)          :: scal1_avg_1d_Z(sz-1:ez+1 )
 integer,intent(out)           :: stat

!Local Variables
 integer                       :: i,j,k
 integer                       :: ierr
 real(r8)                      :: Area, rsum
 real(r8)                      :: scal1_Z_global(sz-1:ez+1)
 integer                       :: rsize

 logical,parameter             :: debug=.false.

 call ghost(scal1Full,'scal1',ierr)    ! KYLE WHY IS THIS NECESSARY?

 scal1_avg_1d_Z(:)=0.d0

 Area = 0.d0
 do j=sy-1,ey+1
  do i=sx-1,ex+1
   Area = Area + dxc(i)*dyc(j)
  enddo
 enddo

 do k=sz-1,ez+1
  rsum=0.0d0
  do j=sy-1,ey+1
   do i=sx-1,ex+1
    rsum = rsum + scal1Full(i,j,k)*dxc(i)*dyc(j)
   enddo
  enddo
   scal1_avg_1d_Z(k) = rsum/Area
 enddo

#ifdef PARALLEL
 rsize=size(scal1_Z_global)
 call MPI_ALLREDUCE(scal1_avg_1d_Z(:),scal1_Z_global(:),rsize,realtype,MPI_SUM,commx1x2,ierr)
 scal1_avg_1d_Z(:)=scal1_Z_global(:)/dble(sizex1x2)
#endif

 stat = 0
return
end subroutine scal1_mean

subroutine rho_rhs(un,vn,wn,at,rcond,pzz,er,iflag,rhon,scal1n,Rtemp,stat)
!@t
! \textbf{subroutine rho\_rhs(un,vn,wn,rhon,Rtemp,stat)}
!@h
!   Description:
!     Updates rho using the scalar equation. KYLE?
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
 
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
 
!   Language:
!     Fortran 90

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez,nzp2
 use Grid,   only: rdxc, rdyc, rdzc, rdxe, rdye, rdze
 use Grid,   only: dxc, dyc, dzc, dxe, dye, dze,ze,xc,zc
 use Parameters, only: rRe, rPr, Rsponge, flow_type, time, RK3, RK3CN, LESflag
 use Flow,   only: Rmodel,r
 use LESmod, only: kappaT, kappasm
 use IO, only: IOUT
 use DD,  only: comm3d,sizeX3,coords
 implicit none

!Passed Variables
 real(r8),intent(in)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: at(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: rcond(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: pzz(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: er(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: iflag
 real(r8),intent(in)        :: rhon(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: scal1n(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)       :: Rtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)        :: stat

!Local Variables
 integer                    :: i,j,k,l
 real(r8)                   :: rhoaver1, rhoaver2, kappaTaver1, kappaTaver2, raver1, raver2
 real(r8)                   :: D,Deddy(sz-1:ez+1) 
 real(r8)                   :: Hprof(sz-1:ez+1),Iabsor_coef(1:9),Iw_coef(1:9)
 integer                    :: dir,face,var,ierr
 real(r8)                   :: tmp1, tmp2, tmp3, tmp4, del_g, Hflux,diurtime, Exner, Latv

 Rtemp = 0.d0
 D = rRe*rPr

 if (Rmodel.EQ.'DNSrho'.or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then

  !************************************
  !*       Linear Terms               *
  !************************************ 
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
     !d2rho/dx2
      Rtemp(i,j,k) = Rtemp(i,j,k) + D*( ( rhon(i+1,j,k) - rhon(i,j,k) )*rdxe(i) -  &
                 ( rhon(i,j,k)- rhon(i-1,j,k) )*rdxe(i-1) )*rdxc(i) &
     !d2rho/dy2
                   + D*( ( rhon(i,j+1,k) - rhon(i,j,k) )* rdye(j) - &
                 ( rhon(i,j,k) - rhon(i,j-1,k) )*rdye(j-1) )*rdyc(j) 
     !d2rho/dz2: march with CN
     !             + D*( ( rhon(i,j,k+1) - rhon(i,j,k) )*rdze(k) - &
     !           ( rhon(i,j,k) - rhon(i,j,k-1) )*rdze(k-1) )*rdzc(k)
     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
     !d2rho/dx2
      Rtemp(i,j,k) = Rtemp(i,j,k) + D*( ( rhon(i,j,k+1) - rhon(i,j,k) )*rdze(k) - &
                                        ( rhon(i,j,k) - rhon(i,j,k-1) )*rdze(k-1) )*rdzc(k)
      enddo
     enddo
    enddo
   endif
  
 !*********************************
  !*  Non-linear Terms             *
  !*********************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
     !-d(rho*u)/dx
      rhoaver2 = 0.5d0*( rhon(i+1,j,k) + rhon(i,j,k) )
      rhoaver1 = 0.5d0*( rhon(i,j,k) + rhon(i-1,j,k) )
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( un(i-1,j,k)*rhoaver1 - un(i,j,k)*rhoaver2 )*rdxc(i)
     !-d(rho*v)/dy
      rhoaver2 = 0.5d0*(rhon(i,j+1,k) + rhon(i,j,k))
      rhoaver1 = 0.5d0*(rhon(i,j,k) + rhon(i,j-1,k))
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( vn(i,j-1,k)*rhoaver1 - vn(i,j,k)*rhoaver2 )*rdyc(j)
     !-d(rho*w)/dz
      rhoaver2 = 0.5d0*(rhon(i,j,k+1) + rhon(i,j,k))
      rhoaver1 = 0.5d0*(rhon(i,j,k) + rhon(i,j,k-1))
      raver2 = 0.5d0*(r(i,j,k+1) + r(i,j,k))
      raver1 = 0.5d0*(r(i,j,k) + r(i,j,k-1))
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( raver1*wn(i,j,k-1)*rhoaver1 - raver2*wn(i,j,k)*rhoaver2 )*rdzc(k)/r(i,j,k)
     enddo
    enddo
   enddo

  !*********************************
  !*  Extra RHS Terms             *
  !********************************* 
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
       Exner = (pzz(i,j,k)/101325.0)**(287.0/1004.0)
        Latv = 2.5*(10.0)**6.0 - (1885.0-4186.0)*(at(i,j,k)-273.15)
      Rtemp(i,j,k) = Rtemp(i,j,k) - er(i,j,k)*Latv/(Exner*1004.0) &
                                  + Latv/(Exner*1004.0)*rcond(i,j,k)*iflag
     enddo
    enddo
   enddo

 else
  write(IOUT,'(a)') "ABORTING RHO_RHS, RMODEL =  "//trim(Rmodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif

 if (Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then
  !*********************************
  !*      LES Terms                *
  !*********************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      !-dQ1/dx
      kappaTaver2 = 0.5d0*(kappaT(i+1,j,k)+kappaT(i,j,k))
      kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i-1,j,k))
      Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i+1,j,k)-rhon(i,j,k))*rdxe(i)&
                                          -kappaTaver1*(rhon(i,j,k)-rhon(i-1,j,k))*rdxe(i-1) )*rdxc(i)
      !-dQ2/dy 
      kappaTaver2 = 0.5d0*(kappaT(i,j+1,k)+kappaT(i,j,k))
      kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i,j-1,k))
      Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i,j+1,k)-rhon(i,j,k))*rdye(j)&
                                          -kappaTaver1*(rhon(i,j,k)-rhon(i,j-1,k))*rdye(j-1) )*rdyc(j)
      !-dQ3/dz: march with CN  
      !kappaTaver2 = 0.5d0*(kappaT(i,j,k+1)+kappaT(i,j,k))
      !kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i,j,k-1))
      !Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i,j,k+1)-rhon(i,j,k))*rdze(k)&
      !                                    -kappaTaver1*(rhon(i,j,k)-rhon(i,j,k-1))*rdze(k-1) )*rdzc(k)   
     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
      !-dQ3/dz: march with CN  
       kappaTaver2 = 0.5d0*(kappaT(i,j,k+1)+kappaT(i,j,k))
       kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i,j,k-1))
       Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i,j,k+1)-rhon(i,j,k))*rdze(k)&
                                           -kappaTaver1*(rhon(i,j,k)-rhon(i,j,k-1))*rdze(k-1) )*rdzc(k)   
      enddo
     enddo
    enddo
   endif

 elseif (Rmodel.NE.'DNSrho') then
  write(IOUT,'(a)') "ABORTING RHO_RHS, RMODEL =  "//trim(Rmodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif

   if (Rsponge) then
   !***********************************
   !*  Sponge Terms                   *
   !***********************************
   var=5 !rho=Flow(:,:,:,5)
   do dir=1,3
    do face=1,2
     call Sponge(Rtemp,rhon,var,dir,face,ierr)
    enddo
   enddo
  endif

 call ghost(Rtemp,'rtemp',ierr)
 stat = ierr
 return
end subroutine rho_rhs

subroutine qv_rhs(un,vn,wn,rcond,er,iflag,rhon,scal1n,Rtemp,stat)
!@t
! \textbf{subroutine rho\_rhs(un,vn,wn,rhon,Rtemp,stat)}
!@h
!   Description:
!     Updates rho using the scalar equation. KYLE?
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez,nzp2
 use Grid,   only: rdxc, rdyc, rdzc, rdxe, rdye, rdze
 use Grid,   only: dxc, dyc, dzc, dxe, dye, dze,ze,xc,zc
 use Parameters, only: rRe, rPr, Rsponge, flow_type, time, RK3, RK3CN, LESflag
 use Flow,   only: Rmodel,r
 use LESmod, only: kappaT, kappasm
 use IO, only: IOUT
 use DD,  only: comm3d,sizeX3,coords
 implicit none

!Passed Variables
 real(r8),intent(in)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: rcond(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: er(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: iflag
 real(r8),intent(in)        :: rhon(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: scal1n(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)       :: Rtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)        :: stat

!Local Variables
 integer                    :: i,j,k,l
 real(r8)                   :: rhoaver1, rhoaver2, kappaTaver1, kappaTaver2, raver1, raver2
 real(r8)                   :: D,Deddy(sz-1:ez+1)
 real(r8)                   :: Hprof(sz-1:ez+1),Iabsor_coef(1:9),Iw_coef(1:9)
 integer                    :: dir,face,var,ierr
 real(r8)                   :: tmp1, tmp2, tmp3, tmp4, del_g, Hflux,diurtime, Exner, Latv

 Rtemp = 0.d0
 D = rRe*rPr

 if (Rmodel.EQ.'DNSrho'.or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then

  !************************************
  !*       Linear Terms               *
  !************************************ 
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
     !d2rho/dx2
      Rtemp(i,j,k) = Rtemp(i,j,k) + D*( ( rhon(i+1,j,k) - rhon(i,j,k) )*rdxe(i) -  &
                 ( rhon(i,j,k)- rhon(i-1,j,k) )*rdxe(i-1) )*rdxc(i) &
     !d2rho/dy2
                   + D*( ( rhon(i,j+1,k) - rhon(i,j,k) )* rdye(j) - &
                 ( rhon(i,j,k) - rhon(i,j-1,k) )*rdye(j-1) )*rdyc(j)
     !d2rho/dz2: march with CN
     !             + D*( ( rhon(i,j,k+1) - rhon(i,j,k) )*rdze(k) - &
     !           ( rhon(i,j,k) - rhon(i,j,k-1) )*rdze(k-1) )*rdzc(k)
     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
     !d2rho/dx2
      Rtemp(i,j,k) = Rtemp(i,j,k) + D*( ( rhon(i,j,k+1) - rhon(i,j,k) )*rdze(k) - &
                                        ( rhon(i,j,k) - rhon(i,j,k-1) )*rdze(k-1) )*rdzc(k)
      enddo
     enddo
    enddo
   endif

 !*********************************
  !*  Non-linear Terms             *
  !*********************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
     !-d(rho*u)/dx
      rhoaver2 = 0.5d0*( rhon(i+1,j,k) + rhon(i,j,k) )
      rhoaver1 = 0.5d0*( rhon(i,j,k) + rhon(i-1,j,k) )
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( un(i-1,j,k)*rhoaver1 - un(i,j,k)*rhoaver2 )*rdxc(i)
     !-d(rho*v)/dy
      rhoaver2 = 0.5d0*(rhon(i,j+1,k) + rhon(i,j,k))
      rhoaver1 = 0.5d0*(rhon(i,j,k) + rhon(i,j-1,k))
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( vn(i,j-1,k)*rhoaver1 - vn(i,j,k)*rhoaver2 )*rdyc(j)
     !-d(rho*w)/dz
      rhoaver2 = 0.5d0*(rhon(i,j,k+1) + rhon(i,j,k))
      rhoaver1 = 0.5d0*(rhon(i,j,k) + rhon(i,j,k-1))
      raver2 = 0.5d0*(r(i,j,k+1) + r(i,j,k))
      raver1 = 0.5d0*(r(i,j,k) + r(i,j,k-1))
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( raver1*wn(i,j,k-1)*rhoaver1 - raver2*wn(i,j,k)*rhoaver2 )*rdzc(k)/r(i,j,k)
     enddo
    enddo
   enddo

  !*********************************
  !*  Extra RHS Terms             *
  !********************************* 
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      Rtemp(i,j,k) = Rtemp(i,j,k) + er(i,j,k) &
                                  - rcond(i,j,k)*iflag
     enddo
    enddo
   enddo

 else
  write(IOUT,'(a)') "ABORTING RHO_RHS, RMODEL =  "//trim(Rmodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif

 if (Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then
  !*********************************
  !*      LES Terms                *
  !*********************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      !-dQ1/dx
      kappaTaver2 = 0.5d0*(kappaT(i+1,j,k)+kappaT(i,j,k))
      kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i-1,j,k))
      Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i+1,j,k)-rhon(i,j,k))*rdxe(i)&
                                          -kappaTaver1*(rhon(i,j,k)-rhon(i-1,j,k))*rdxe(i-1) )*rdxc(i)
      !-dQ2/dy 
      kappaTaver2 = 0.5d0*(kappaT(i,j+1,k)+kappaT(i,j,k))
      kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i,j-1,k))
      Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i,j+1,k)-rhon(i,j,k))*rdye(j)&
                                          -kappaTaver1*(rhon(i,j,k)-rhon(i,j-1,k))*rdye(j-1) )*rdyc(j)
      !-dQ3/dz: march with CN  
      !kappaTaver2 = 0.5d0*(kappaT(i,j,k+1)+kappaT(i,j,k))
      !kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i,j,k-1))
      !Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i,j,k+1)-rhon(i,j,k))*rdze(k)&
      !                                    -kappaTaver1*(rhon(i,j,k)-rhon(i,j,k-1))*rdze(k-1) )*rdzc(k)   
     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
      !-dQ3/dz: march with CN  
       kappaTaver2 = 0.5d0*(kappaT(i,j,k+1)+kappaT(i,j,k))
       kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i,j,k-1))
       Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i,j,k+1)-rhon(i,j,k))*rdze(k)&
                                           -kappaTaver1*(rhon(i,j,k)-rhon(i,j,k-1))*rdze(k-1) )*rdzc(k)
      enddo
     enddo
    enddo
   endif

 elseif (Rmodel.NE.'DNSrho') then
  write(IOUT,'(a)') "ABORTING RHO_RHS, RMODEL =  "//trim(Rmodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif

   if (Rsponge) then
   !***********************************
   !*  Sponge Terms                   *
   !***********************************
   var=9 !rho=Flow(:,:,:,5)
   do dir=1,3
    do face=1,2
     call Sponge(Rtemp,rhon,var,dir,face,ierr)
    enddo
   enddo
  endif

 call ghost(Rtemp,'rtemp',ierr)
 stat = ierr
 return
end subroutine qv_rhs

subroutine ql_rhs(un,vn,wn,rcond,ar,cr,iflag,rhon,Rtemp,stat)
!@t
! \textbf{subroutine rho\_rhs(un,vn,wn,rhon,Rtemp,stat)}
!@h
!   Description:
!     Updates rho using the scalar equation. KYLE?
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez,nzp2
 use Grid,   only: rdxc, rdyc, rdzc, rdxe, rdye, rdze
 use Grid,   only: dxc, dyc, dzc, dxe, dye, dze,ze,xc,zc
 use Parameters, only: rRe, rPr, Rsponge, flow_type, time, RK3, RK3CN, LESflag
 use Flow, only: Rmodel, r
 use LESmod, only: kappaT, kappasm
 use IO, only: IOUT
 use DD,  only: comm3d,sizeX3,coords
 implicit none

!Passed Variables
 real(r8),intent(in)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: rhon(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: rcond(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: ar(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: cr(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)       :: Rtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: iflag
 integer,intent(out)        :: stat

!Local Variables
 integer                    :: i,j,k,l
 real(r8)                   :: rhoaver1, rhoaver2, kappaTaver1, kappaTaver2
 real(r8)                   :: D,Deddy(sz-1:ez+1)
 real(r8)                   :: Hprof(sz-1:ez+1),Iabsor_coef(1:9),Iw_coef(1:9)
 integer                    :: dir,face,var,ierr
 real(r8)                   :: tmp1, tmp2, tmp3, tmp4, del_g, Hflux,diurtime

 Rtemp = 0.d0
 D = rRe*rPr

 if (Rmodel.EQ.'DNSrho'.or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then
  !************************************
  !*       Forcing                    *
  !************************************ 
  !************************************
  !*       Linear Terms               *
  !************************************ 
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
     !d2rho/dx2
      Rtemp(i,j,k) = Rtemp(i,j,k) + D*( ( rhon(i+1,j,k) - rhon(i,j,k) )*rdxe(i) -  &
                 ( rhon(i,j,k)- rhon(i-1,j,k) )*rdxe(i-1) )*rdxc(i) &
     !d2rho/dy2
                   + D*( ( rhon(i,j+1,k) - rhon(i,j,k) )* rdye(j) - &
                 ( rhon(i,j,k) - rhon(i,j-1,k) )*rdye(j-1) )*rdyc(j)
     !d2rho/dz2: march with CN
     !             + D*( ( rhon(i,j,k+1) - rhon(i,j,k) )*rdze(k) - &
     !           ( rhon(i,j,k) - rhon(i,j,k-1) )*rdze(k-1) )*rdzc(k)
     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
     !d2rho/dx2
      Rtemp(i,j,k) = Rtemp(i,j,k) + D*( ( rhon(i,j,k+1) - rhon(i,j,k) )*rdze(k) - &
                                        ( rhon(i,j,k) - rhon(i,j,k-1) )*rdze(k-1) )*rdzc(k)
      enddo
     enddo
    enddo
   endif

 !*********************************
  !*  Non-linear Terms             *
  !*********************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
     !-d(rho*u)/dx
      rhoaver2 = 0.5d0*( rhon(i+1,j,k) + rhon(i,j,k) )
      rhoaver1 = 0.5d0*( rhon(i,j,k) + rhon(i-1,j,k) )
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( un(i-1,j,k)*rhoaver1 - un(i,j,k)*rhoaver2 )*rdxc(i)
     !-d(rho*v)/dy
      rhoaver2 = 0.5d0*(rhon(i,j+1,k) + rhon(i,j,k))
      rhoaver1 = 0.5d0*(rhon(i,j,k) + rhon(i,j-1,k))
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( vn(i,j-1,k)*rhoaver1 - vn(i,j,k)*rhoaver2 )*rdyc(j)
     !-d(rho*w)/dz
      rhoaver2 = 0.5d0*(rhon(i,j,k+1) + rhon(i,j,k))
      rhoaver1 = 0.5d0*(rhon(i,j,k) + rhon(i,j,k-1))
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( 0.5d0*(r(i,j,k) + r(i,j,k-1))*wn(i,j,k-1)*rhoaver1 - 0.5d0*(r(i,j,k) + r(i,j,k+1))*wn(i,j,k)*rhoaver2 )*rdzc(k)/r(i,j,k)
     enddo
    enddo
   enddo

 !*********************************
  !*  Extra RHS Terms             *
  !********************************* 
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      Rtemp(i,j,k) = Rtemp(i,j,k) - ar(i,j,k) - cr(i,j,k) &
                                  + rcond(i,j,k)*iflag
     enddo
    enddo
   enddo

 else
  write(IOUT,'(a)') "ABORTING RHO_RHS, RMODEL =  "//trim(Rmodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif

 if (Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then
  !*********************************
  !*      LES Terms                *
  !*********************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      !-dQ1/dx
      kappaTaver2 = 0.5d0*(kappaT(i+1,j,k)+kappaT(i,j,k))
      kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i-1,j,k))
      Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i+1,j,k)-rhon(i,j,k))*rdxe(i)&
                                          -kappaTaver1*(rhon(i,j,k)-rhon(i-1,j,k))*rdxe(i-1) )*rdxc(i)
      !-dQ2/dy 
      kappaTaver2 = 0.5d0*(kappaT(i,j+1,k)+kappaT(i,j,k))
      kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i,j-1,k))
      Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i,j+1,k)-rhon(i,j,k))*rdye(j)&
                                          -kappaTaver1*(rhon(i,j,k)-rhon(i,j-1,k))*rdye(j-1) )*rdyc(j)
      !-dQ3/dz: march with CN  
      !kappaTaver2 = 0.5d0*(kappaT(i,j,k+1)+kappaT(i,j,k))
      !kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i,j,k-1))
      !Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i,j,k+1)-rhon(i,j,k))*rdze(k)&
      !                                    -kappaTaver1*(rhon(i,j,k)-rhon(i,j,k-1))*rdze(k-1) )*rdzc(k)   
     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
      !-dQ3/dz: march with CN  
       kappaTaver2 = 0.5d0*(kappaT(i,j,k+1)+kappaT(i,j,k))
       kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i,j,k-1))
       Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i,j,k+1)-rhon(i,j,k))*rdze(k)&
                                           -kappaTaver1*(rhon(i,j,k)-rhon(i,j,k-1))*rdze(k-1) )*rdzc(k)
      enddo
     enddo
    enddo
   endif

 elseif (Rmodel.NE.'DNSrho') then
  write(IOUT,'(a)') "ABORTING RHO_RHS, RMODEL =  "//trim(Rmodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif
   if (Rsponge) then
   !***********************************
   !*  Sponge Terms                   *
   !***********************************
   var=10 !rho=Flow(:,:,:,5)
   do dir=1,3
    do face=1,2
     call Sponge(Rtemp,rhon,var,dir,face,ierr)
    enddo
   enddo
  endif

 call ghost(Rtemp,'rtemp',ierr)
 stat = ierr
 return
end subroutine ql_rhs

subroutine qr_rhs(un,vn,wn,er,ar,cr,vrr,rhon,Rtemp,stat)
!@t
! \textbf{subroutine rho\_rhs(un,vn,wn,rhon,Rtemp,stat)}
!@h
!   Description:
!     Updates rho using the scalar equation. KYLE?
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez,nzp2
 use Grid,   only: rdxc, rdyc, rdzc, rdxe, rdye, rdze
 use Grid,   only: dxc, dyc, dzc, dxe, dye, dze,ze,xc,zc
 use Parameters, only: rRe, rPr, Rsponge, flow_type, time, RK3, RK3CN, LESflag
 use Flow, only: Rmodel,r
 use LESmod, only: kappaT, kappasm
 use IO, only: IOUT
 use DD,  only: comm3d,sizeX3,coords
 implicit none

!Passed Variables
 real(r8),intent(in)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: er(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: ar(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: cr(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vrr(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: rhon(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)       :: Rtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)        :: stat

!Local Variables
 integer                    :: i,j,k,l
 real(r8)                   :: rhoaver1, rhoaver2, kappaTaver1, kappaTaver2
 real(r8)                   :: D,Deddy(sz-1:ez+1)
 real(r8)                   :: Hprof(sz-1:ez+1),Iabsor_coef(1:9),Iw_coef(1:9)
 integer                    :: dir,face,var,ierr
 real(r8)                   :: tmp1, tmp2, tmp3, tmp4, del_g, Hflux,diurtime

 Rtemp = 0.d0
 D = rRe*rPr

 if (Rmodel.EQ.'DNSrho'.or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then
  !************************************
  !*       Forcing                    *
  !************************************ 
  !************************************
  !*       Linear Terms               *
  !************************************ 
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
     !d2rho/dx2
      Rtemp(i,j,k) = Rtemp(i,j,k) + D*( ( rhon(i+1,j,k) - rhon(i,j,k) )*rdxe(i) -  &
                 ( rhon(i,j,k)- rhon(i-1,j,k) )*rdxe(i-1) )*rdxc(i) &
     !d2rho/dy2
                   + D*( ( rhon(i,j+1,k) - rhon(i,j,k) )* rdye(j) - &
                 ( rhon(i,j,k) - rhon(i,j-1,k) )*rdye(j-1) )*rdyc(j)
     !d2rho/dz2: march with CN
     !             + D*( ( rhon(i,j,k+1) - rhon(i,j,k) )*rdze(k) - &
     !           ( rhon(i,j,k) - rhon(i,j,k-1) )*rdze(k-1) )*rdzc(k)
     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
     !d2rho/dx2
      Rtemp(i,j,k) = Rtemp(i,j,k) + D*( ( rhon(i,j,k+1) - rhon(i,j,k) )*rdze(k) - &
                                        ( rhon(i,j,k) - rhon(i,j,k-1) )*rdze(k-1) )*rdzc(k)
      enddo
     enddo
    enddo
   endif

 !*********************************
  !*  Non-linear Terms             *
  !*********************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
     !-d(rho*u)/dx
      rhoaver2 = 0.5d0*( rhon(i+1,j,k) + rhon(i,j,k) )
      rhoaver1 = 0.5d0*( rhon(i,j,k) + rhon(i-1,j,k) )
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( un(i-1,j,k)*rhoaver1 - un(i,j,k)*rhoaver2 )*rdxc(i)
     !-d(rho*v)/dy
      rhoaver2 = 0.5d0*(rhon(i,j+1,k) + rhon(i,j,k))
      rhoaver1 = 0.5d0*(rhon(i,j,k) + rhon(i,j-1,k))
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( vn(i,j-1,k)*rhoaver1 - vn(i,j,k)*rhoaver2 )*rdyc(j)
     !-d(rho*w)/dz
      rhoaver2 = 0.5d0*(rhon(i,j,k+1) + rhon(i,j,k))
      rhoaver1 = 0.5d0*(rhon(i,j,k) + rhon(i,j,k-1))
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( 0.5d0*(r(i,j,k) + r(i,j,k-1))*wn(i,j,k-1)*rhoaver1 - 0.5d0*(r(i,j,k) + r(i,j,k+1))*wn(i,j,k)*rhoaver2 )*rdzc(k)/r(i,j,k)

      rhoaver2 = 0.5d0*(vrr(i,j,k+1) + vrr(i,j,k))
      rhoaver1 = 0.5d0*(vrr(i,j,k) + vrr(i,j,k-1))
      Rtemp(i,j,k) = Rtemp(i,j,k) + ( 0.5d0*(r(i,j,k) + r(i,j,k-1))*wn(i,j,k-1)*rhoaver1 - 0.5d0*(r(i,j,k) + r(i,j,k+1))*wn(i,j,k)*rhoaver2 )*rdzc(k)/r(i,j,k)

     enddo
    enddo
   enddo

 !*********************************
  !*  Extra RHS Terms             *
  !********************************* 
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      Rtemp(i,j,k) = Rtemp(i,j,k) -er(i,j,k) + ar(i,j,k) + cr(i,j,k)
     enddo
    enddo
   enddo

 else
  write(IOUT,'(a)') "ABORTING RHO_RHS, RMODEL =  "//trim(Rmodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif

 if (Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then
  !*********************************
  !*      LES Terms                *
  !*********************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      !-dQ1/dx
      kappaTaver2 = 0.5d0*(kappaT(i+1,j,k)+kappaT(i,j,k))
      kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i-1,j,k))
      Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i+1,j,k)-rhon(i,j,k))*rdxe(i)&
                                          -kappaTaver1*(rhon(i,j,k)-rhon(i-1,j,k))*rdxe(i-1) )*rdxc(i)
      !-dQ2/dy 
      kappaTaver2 = 0.5d0*(kappaT(i,j+1,k)+kappaT(i,j,k))
      kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i,j-1,k))
      Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i,j+1,k)-rhon(i,j,k))*rdye(j)&
                                          -kappaTaver1*(rhon(i,j,k)-rhon(i,j-1,k))*rdye(j-1) )*rdyc(j)
      !-dQ3/dz: march with CN  
      !kappaTaver2 = 0.5d0*(kappaT(i,j,k+1)+kappaT(i,j,k))
      !kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i,j,k-1))
      !Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i,j,k+1)-rhon(i,j,k))*rdze(k)&
      !                                    -kappaTaver1*(rhon(i,j,k)-rhon(i,j,k-1))*rdze(k-1) )*rdzc(k)   
     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
      !-dQ3/dz: march with CN  
       kappaTaver2 = 0.5d0*(kappaT(i,j,k+1)+kappaT(i,j,k))
       kappaTaver1 = 0.5d0*(kappaT(i,j,k)+kappaT(i,j,k-1))
       Rtemp(i,j,k) = Rtemp(i,j,k) - LESflag*1.d0*( kappaTaver2*(rhon(i,j,k+1)-rhon(i,j,k))*rdze(k)&
                                           -kappaTaver1*(rhon(i,j,k)-rhon(i,j,k-1))*rdze(k-1) )*rdzc(k)
      enddo
     enddo
    enddo
   endif

 elseif (Rmodel.NE.'DNSrho') then
  write(IOUT,'(a)') "ABORTING RHO_RHS, RMODEL =  "//trim(Rmodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif
   if (Rsponge) then
   !***********************************
   !*  Sponge Terms                   *
   !***********************************
   var=11 !rho=Flow(:,:,:,5)
   do dir=1,3
    do face=1,2
     call Sponge(Rtemp,rhon,var,dir,face,ierr)
    enddo
   enddo
  endif

 call ghost(Rtemp,'rtemp',ierr)
 stat = ierr
 return
end subroutine qr_rhs

subroutine scal1_rhs(un,vn,wn,rhon,scal1n,Stemp,stat)
!@t
! \textbf{subroutine rho\_rhs(un,vn,wn,scal1n,Stemp,stat)}
!@h
!   Description:
!     Updates rho using the scalar equation. KYLE?
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
 
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
 
!   Language:
!     Fortran 90

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez,nzp2
 use Grid,   only: rdxc, rdyc, rdzc, rdxe, rdye, rdze
 use Grid,   only: dxc, dyc, dzc, dxe, dye, dze, ze,xc,zc
 use Parameters, only: rRe,rPr,rSc,Rsponge,flow_type, time, RK3, RK3CN,LESflag
 use Flow,   only: Smodel
 use LESmod, only: nappaT, nappasm
 use IO,     only: IOUT
 use DD,     only: coords,sizeX3
 implicit none

!Passed Variables
 real(r8),intent(in)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: rhon(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: scal1n(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)       :: Stemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)        :: stat

!Local Variables
 integer                    :: i,j,k
 real(r8)                   :: scal1aver1, scal1aver2, nappaTaver1, nappaTaver2
 real(r8)                   :: D, Deddy(sz-1:ez+1),Scal1_Z(sz-1:ez+1),Sflux
 integer                    :: dir,face,var,err,ierr
 real(r8)                   :: tmp1, tmp2, tmp3, tmp4, del_g

 Stemp = 0.d0
 D = rRe*rSc

!Extra-diffusion

 if (Smodel.EQ.'DNSscal1'.or.Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then
 
  !************************************
  !*       Forcing                    *
  !************************************ 
!  if (coords(3).eq.sizeX3-1 .and.time.LT.1.d10) then
!   Sflux = 1.d-6  !positive,unstable
!   do k=nzp2-4,ez; do j=sy,ey; do i=sx,ex
!    Stemp(i,j,k) = Stemp(i,j,k) - (0.d0 - Sflux)/1000.d0*rdzc(k)
!   enddo; enddo; enddo
!  endif

  !************************************
  !*       Linear Terms               *
  !************************************ 
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
     !d2scal1/dx2
      Stemp(i,j,k) = Stemp(i,j,k) + D*( ( scal1n(i+1,j,k) - scal1n(i,j,k) )*rdxe(i) -  &
                 ( scal1n(i,j,k)- scal1n(i-1,j,k) )*rdxe(i-1) )*rdxc(i) &
     !d2scal1/dy2
                   + D*( ( scal1n(i,j+1,k) - scal1n(i,j,k) )* rdye(j) - &
                 ( scal1n(i,j,k) - scal1n(i,j-1,k) )*rdye(j-1) )*rdyc(j) 
     !d2scal1/dz2: march with CN
     !             + D*( ( scal1n(i,j,k+1) - scal1n(i,j,k) )*rdze(k) - &
     !           ( scal1n(i,j,k) - scal1n(i,j,k-1) )*rdze(k-1) )*rdzc(k)
     enddo
    enddo
   enddo
   if (RK3) then 
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
      !d2scal1/dz2
       Stemp(i,j,k) = Stemp(i,j,k) + D*( ( scal1n(i,j,k+1) - scal1n(i,j,k) )*rdze(k) - &
                ( scal1n(i,j,k) - scal1n(i,j,k-1) )*rdze(k-1) )*rdzc(k)
      enddo
     enddo
    enddo
   endif

  !*********************************
  !*  Non-linear Terms             *
  !*********************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
     !-d(scal1*u)/dx
      scal1aver2 = 0.5d0*( scal1n(i+1,j,k) + scal1n(i,j,k) )
      scal1aver1 = 0.5d0*( scal1n(i,j,k) + scal1n(i-1,j,k) )
      Stemp(i,j,k) = Stemp(i,j,k) + ( un(i-1,j,k)*scal1aver1 - un(i,j,k)*scal1aver2 )*rdxc(i)
     !-d(rho*v)/dy
      scal1aver2 = 0.5d0*(scal1n(i,j+1,k) + scal1n(i,j,k))
      scal1aver1 = 0.5d0*(scal1n(i,j,k) + scal1n(i,j-1,k))
      Stemp(i,j,k) = Stemp(i,j,k) + ( vn(i,j-1,k)*scal1aver1 - vn(i,j,k)*scal1aver2 )*rdyc(j)
     !-d(rho*w)/dz
      scal1aver2 = 0.5d0*(scal1n(i,j,k+1) + scal1n(i,j,k))
      scal1aver1 = 0.5d0*(scal1n(i,j,k) + scal1n(i,j,k-1))
      Stemp(i,j,k) = Stemp(i,j,k) + ( wn(i,j,k-1)*scal1aver1 - wn(i,j,k)*scal1aver2 )*rdzc(k)
     enddo
    enddo
   enddo

 else
  write(IOUT,'(a)') "ABORTING SCAL1_RHS, SMODEL =  "//trim(Smodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif

 if (Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then
 !*********************************
  !*      LES Terms                *
  !*********************************
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      !-dQS1/dx 
      nappaTaver2 = 0.5d0*(nappaT(i+1,j,k)+nappaT(i,j,k))
      nappaTaver1 = 0.5d0*(nappaT(i,j,k)-nappaT(i-1,j,k))
      Stemp(i,j,k) = Stemp(i,j,k) - LESflag*1.0*( nappaTaver2*(scal1n(i+1,j,k)-scal1n(i,j,k))*rdxe(i)&
                                         -nappaTaver1*(scal1n(i,j,k)-scal1n(i-1,j,k))*rdxe(i-1) )*rdxc(i)
      !-dQS2/dy 
      nappaTaver2 = 0.5d0*(nappaT(i,j+1,k)+nappaT(i,j,k))
      nappaTaver1 = 0.5d0*(nappaT(i,j,k)-nappaT(i,j-1,k))
      Stemp(i,j,k) = Stemp(i,j,k) - LESflag*1.0*( nappaTaver2*(scal1n(i,j+1,k)-scal1n(i,j,k))*rdye(j)&
                                         -nappaTaver1*(scal1n(i,j,k)-scal1n(i,j-1,k))*rdye(j-1) )*rdyc(j)
      !-dQS3/dz: march with CN
      !nappaTaver2 = 0.5d0*(nappaT(i,j,k+1)+nappaT(i,j,k))
      !nappaTaver1 = 0.5d0*(nappaT(i,j,k)-nappaT(i,j,k-1))
      !Stemp(i,j,k) = Stemp(i,j,k) - LESflag*1.0*( nappaTaver2*(scal1n(i,j,k+1)-scal1n(i,j,k))*rdze(k)&
      !                                   -nappaTaver1*(scal1n(i,j,k)-scal1n(i,j,k-1))*rdze(k-1) )*rdzc(k)
     enddo
    enddo
   enddo
   if (RK3) then
    do k=sz,ez
     do j=sy,ey
      do i=sx,ex
      !-dQS3/dz: march with CN
       nappaTaver2 = 0.5d0*(nappaT(i,j,k+1)+nappaT(i,j,k))
       nappaTaver1 = 0.5d0*(nappaT(i,j,k)-nappaT(i,j,k-1))
       Stemp(i,j,k) = Stemp(i,j,k) - LESflag*1.0*( nappaTaver2*(scal1n(i,j,k+1)-scal1n(i,j,k))*rdze(k)&
                                          -nappaTaver1*(scal1n(i,j,k)-scal1n(i,j,k-1))*rdze(k-1) )*rdzc(k)
      enddo
     enddo
    enddo
   endif

 elseif (Smodel.NE.'DNSscal1') then
  write(IOUT,'(a)') "ABORTING SCAL1_RHS, SMODEL =  "//trim(Smodel)//" NOT IMPLEMENTED"
  stat=1
  return
 endif

   if (Rsponge) then
   !***********************************
   !*  Sponge Terms                   *
   !***********************************
   var=8 !scal1=Flow(:,:,:,8) 
   do dir=1,3
    do face=1,2
     call Sponge(Stemp,scal1n,var,dir,face,ierr)
    enddo
   enddo
  endif
 call ghost(Stemp,'scal1temp',ierr)
 stat = ierr
 return
end subroutine scal1_rhs


subroutine Pgrad(pn,Utemp,Vtemp,Wtemp,dt)
!@t
! \textbf{subroutine Pgrad(pn,Utemp,Vtemp,Wtemp,factor)}
!@h
!   Description:
!     Adds the impact of the pressure gradient turn on the velocity terms.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
 
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
 
!   Language:
!     Fortran 90

 use ntypes, only: r8
 use Grid, only:rdxe, rdye, rdze
 use Flow, only:r
 use Domain, only: sx,ex,sy,ey,sz,ez, EU, EV, EW
 implicit none

!Passed Variables
 real(r8),intent(inout)     :: pn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: Utemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: Vtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: Wtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: dt

!Local Variables
 integer                   :: i,j,k,err1

 do k=sz,ez
  do j=sy,ey
   do i=sx,ex-EU
    Utemp(i,j,k)=Utemp(i,j,k) - ( pn(i+1,j,k)/r(i,j,k)-pn(i,j,k)/r(i,j,k) )*rdxe(i)*dt
   enddo
  enddo
 enddo
 call ghost(Utemp,'utemp',err1)

 do k=sz,ez
  do j=sy,ey-EV
   do i=sx,ex
    Vtemp(i,j,k)=Vtemp(i,j,k) - ( pn(i,j+1,k)/r(i,j,k)-pn(i,j,k)/r(i,j,k) )*rdye(j)*dt
   enddo
  enddo
 enddo
 call ghost(Vtemp,'vtemp',err1)

 do k=sz,ez-EW
  do j=sy,ey
   do i=sx,ex
    Wtemp(i,j,k)=Wtemp(i,j,k) - ( pn(i,j,k+1)/r(i,j,k+1)-pn(i,j,k)/r(i,j,k) )*rdze(k)*dt
   enddo
  enddo
 enddo
 call ghost(Wtemp,'wtemp',err1)

return
end subroutine Pgrad

subroutine Psource2(Uitemp,ui,SRC,dt,term,stat)
!@t
! \textbf{subroutine Psource2(Uitemp,ui,SRC,dt,term,stat)}
!@h
!   Description:
!     Updates the pressure source term by term.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
 
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
 
!   Language:
!     Fortran 90

 use ntypes, only: r8
 use Flow, only:r
 use Grid, only: rdxc, rdyc, rdzc
 use Domain, only: sx,ex,sy,ey,sz,ez,nzp2
 implicit none

 !Passed Variables
 real(r8),intent(in)        :: ui(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: Uitemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: SRC(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: dt
 integer,intent(in)         :: term
 integer,intent(out)        :: stat

 !Local Variables
 integer                    :: i,j,k
 integer                    :: ierr
 real(r8)                   :: rdt

 rdt=1.d0/dt

 if (term.EQ.1) then

  do k=sz,ez
   do j=sy,ey
    do i=sx,ex
     !Calculate d(du_i/dx_i)/dt and add it as a correction to d(H_i)/dx_i so that new field is divergence free
     SRC(i,j,k) = SRC(i,j,k) +  ( ui(i,j,k)-ui(i-1,j,k) )*rdxc(i)*rdt  + ( Uitemp(i,j,k) - Uitemp(i-1,j,k) )*rdxc(i) 
    enddo
   enddo
  enddo

 elseif (term.EQ.2) then

  do k=sz,ez
   do j=sy,ey
    do i=sx,ex
     !Calculate d(du_i/dx_i)/dt and add it as a correction to d(H_i)/dx_i so that new field is divergence free
     SRC(i,j,k) = SRC(i,j,k) + ( ui(i,j,k)-ui(i,j-1,k) )*rdyc(j)*rdt  + ( Uitemp(i,j,k) - Uitemp(i,j-1,k) )*rdyc(j) 
    enddo
   enddo
  enddo

 elseif (term.EQ.3) then

  do k=sz,ez
   do j=sy,ey
    do i=sx,ex
     !Calculate d(du_i/dx_i)/dt and add it as a correction to d(H_i)/dx_i so that new field is divergence free
     SRC(i,j,k) = SRC(i,j,k) + ( ui(i,j,k)-ui(i,j,k-1) )*rdzc(k)*rdt  + ( Uitemp(i,j,k) - Uitemp(i,j,k-1) )*rdzc(k) 
    enddo
   enddo
  enddo
 else
  stat=1
  return
 endif
 call ghost(SRC,'psource',ierr)
 stat=ierr
return
end subroutine Psource2

subroutine Psource(Utemp,Vtemp,Wtemp,SRC,dt,stat)
!@t
! \textbf{subroutine Psource(Utemp,Vtemp,Wtemp,u,v,w,SRC,dt,stat)}
!@h
!   Description:
!     Updates the pressure source term all at once. KYLE?
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: r8
 use Flow, only:r
 use Grid, only: rdxc, rdyc, rdzc
 use Domain, only: sx,ex,sy,ey,sz,ez,nzp2
 implicit none

 !Passed Variables
 real(r8),intent(in)        :: Utemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: Vtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: Wtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)       :: SRC(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: dt
 integer,intent(out)        :: stat

 !Local Variables
 integer                    :: i,j,k
 integer                    :: ierr
 real(r8)                   :: rdt

 SRC(:,:,:) = 0.d0

 rdt=1.d0/dt

 do k=sz,ez
  do j=sy,ey
   do i=sx,ex
      !Calculate d(du_i/dx_i)/dt and add it as a correction to d(H_i)/dx_i so that new field is divergence free
      SRC(i,j,k) =  SRC(i,j,k) + ( (r(i,j,k)*Utemp(i,j,k) - r(i,j,k)*Utemp(i-1,j,k))*rdxc(i) & 
                     +(r(i,j,k)*Vtemp(i,j,k) - r(i,j,k)*Vtemp(i,j-1,k))*rdyc(j) & 
                     +(0.5*(r(i,j,k+1)+r(i,j,k))*Wtemp(i,j,k) - 0.5*(r(i,j,k)+r(i,j,k-1))*Wtemp(i,j,k-1))*rdzc(k) )*rdt
   enddo
  enddo
 enddo

 call ghost(SRC,'psource',ierr)

 stat=ierr
return
end subroutine psource

subroutine var_advance(veltemp1,veltemp2,f1,f2,f3)
!@t
! \textbf{subroutine var\_advance(veltemp1,veltemp2,f1,f2,f3)}
!@h
!   Description:
!     Advances a variable. KYLE?
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez
 implicit none
 
!Passed Variables
 real(r8),intent(in)        :: veltemp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: veltemp2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: f1, f2, f3

!Local Variables
 integer                   :: i,j,k

 do k=sz,ez
  do j=sy,ey
   do i=sx,ex
     veltemp2(i,j,k)=f1*veltemp2(i,j,k)+f2*veltemp1(i,j,k)*f3
    enddo
   enddo
  enddo

 return 
end subroutine var_advance

subroutine var_advance_CN(veltemp1,veltemp2,f1,f2,f3,rkdt,var)
!@t
! \textbf{subroutine var\_advance(veltemp1,veltemp2,f1,f2,f3)}
!@h
!   Description:
!     Advances a variable. KYLE?
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
  
!   Modification History
!     Version   Date     Comment 
!     -------   ----     -------  
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
  
!   Language:
!     Fortran 90
   
 use ntypes, only: r8
 use Flow, only: Vmodel, Rmodel, Smodel
 use LESmod
 use Grid  
 use Domain, only: sx,ex,sy,ey,sz,ez
 use boundC, only: TB,VB
 use Parameters, only: delt,rRe,rPr,rSc
 use IO,     only: IOUT
 implicit none
!Passed Variables
 real(r8),intent(in)        :: veltemp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: veltemp2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: f1, f2, f3,rkdt
 integer, intent(in)        :: var
!Local Variables
 real(r8),allocatable,dimension(:)     :: sub,diag,sup,rhs
 real(r8)                              :: tmp1,tmp2,tmp3,tmp4,del_g, Deddy(sz-1:ez+1)
 integer                               :: i,j,k,l,m,n,tsz,tez,s1,stat

!Extra-diffusion
 if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'SSM') then
  Deddy=0.d0 
  select case (var)
   case(1,2,3)
     Deddy(ez) = 1.2d-3
     Deddy(ez-1) = 1.2d-3
!    do k = sz-1,ez+1
!     Deddy(k) = 1.2d-3*dexp(-((ze(k)+0.d0)/0.3d0)**2.d0) 
!    enddo
   case(5)
     Deddy(ez) = 1.2d-3
     Deddy(ez-1) = 1.2d-3
!    do k = sz-1,ez+1
!     Deddy(k) = 1.2d-3*dexp(-((ze(k)+0.d0)/0.3d0)**2.d0) 
!    enddo
   case(8)
     Deddy(ez) = 1.2d-3
     Deddy(ez-1) = 1.2d-3
!    do k = sz-1,ez+1
!     Deddy(k) = 1.2d-3*dexp(-((ze(k)+0.d0)/0.3d0)**2.d0) 
!    enddo
   case DEFAULT
    write(IOUT,*) "Incorrect var id in var_advance_CN extra diffusion: ABORT"
    stop    
  end select
 endif

!Compute rhs with the viscous term, call thomas, get updated value for each ij pencil
 do i=sx,ex
  do j=sy,ey
!Setup the coefficients of the tridiagonal matrix 
 if (var==1) then
  n = (ez+1)-(sz-1)+1
  tsz = sz-1
  tez = ez+1
  allocate( sub(tsz:tez), stat=s1)
  allocate( sup(tsz:tez), stat=s1)
  allocate( diag(tsz:tez), stat=s1)
  allocate( rhs(tsz:tez), stat=s1)
  do k=tsz,tez
   sub(k)= 0.d0
   sup(k)= 0.d0
   diag(k) = 0.d0
   rhs(k) = 0.d0
  enddo
  !viscous term: d2u_dz2
  if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
   do k = sz,ez
    sub(k) = -0.5d0*rkdt*rRe*rdze(k-1)*rdzc(k)
    sup(k) = -0.5d0*rkdt*rRe*rdze(k)*rdzc(k)
    diag(k) = 1.d0+0.5d0*rkdt*rRe*rdzc(k)*(rdze(k)+rdze(k-1))
   enddo
  !extra diffusion
   do k = tsz+1,tez-1
    sub(k) = sub(k)-0.5d0*rkdt*Deddy(k)*rdze(k-1)*rdzc(k)
    sup(k) = sup(k)-0.5d0*rkdt*Deddy(k)*rdze(k)*rdzc(k)
    diag(k) = diag(k)+0.5d0*rkdt*Deddy(k)*rdzc(k)*(rdze(k)+rdze(k-1))
   enddo
  else
   write(IOUT,'(a)') "ABORTING ADVANCE CN U1 LHS, VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif
  !subgrid term: the implicit part of d(tau_13)/dz 
  !interp. modS, Csgs, del_g to tau_13 points
  if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
   do k = sz,ez
   !tmp1 = -nu_sgs at tau_13(i,j,k-1) points
    tmp1 = -0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k-1)+nuT(i+1,j,k-1) )
   !tmp2 = nu_sgs at tau_13(i,j,k) points
    tmp2 = -0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k+1)+nuT(i+1,j,k+1) )

    sub(k) = sub(k) - 0.5d0*rkdt*tmp1*rdze(k-1)*rdzc(k)
    sup(k) = sup(k) - 0.5d0*rkdt*tmp2*rdze(k)*rdzc(k)
    diag(k) = diag(k) + 0.5d0*rkdt*( tmp1*rdze(k-1)*rdzc(k) &
                                     +tmp2*rdze(k)*rdzc(k) )
   enddo
  elseif (Vmodel.NE.'DNS') then
   write(IOUT,'(a)') "ABORTING ADVANCE CN U1 LHS, VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif

 elseif (var==2) then
  n = (ez+1)-(sz-1)+1
  tsz = sz-1
  tez = ez+1
  allocate( sub(tsz:tez), stat=s1)
  allocate( sup(tsz:tez), stat=s1)
  allocate( diag(tsz:tez), stat=s1)
  allocate( rhs(tsz:tez), stat=s1)
  do k=tsz,tez
   sub(k)= 0.d0
   sup(k)= 0.d0
   diag(k) = 0.d0
   rhs(k) = 0.d0
  enddo
  !viscous term: d2v_dz2
  if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
   do k = sz,ez
    sub(k) = -0.5d0*rkdt*rRe*rdze(k-1)*rdzc(k)
    sup(k) = -0.5d0*rkdt*rRe*rdze(k)*rdzc(k)
    diag(k) = 1.d0+0.5d0*rkdt*rRe*rdzc(k)*(rdze(k)+rdze(k-1))
   enddo
  !extra diffusion
   do k = tsz+1,tez-1
    sub(k) = sub(k)-0.5d0*rkdt*Deddy(k)*rdze(k-1)*rdzc(k)
    sup(k) = sup(k)-0.5d0*rkdt*Deddy(k)*rdze(k)*rdzc(k)
    diag(k) = diag(k)+0.5d0*rkdt*Deddy(k)*rdzc(k)*(rdze(k)+rdze(k-1))
   enddo
  else
   write(IOUT,'(a)') "ABORTING ADVANCE CN U2 LHS, VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif
  !subgrid term: the implicit part of d(tau_23)/dz 
  !interp. modS, Csgs, del_g to tau_23 points
  if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
   do k = sz,ez
   !tmp1 = -nu_sgs at tau_23(i,j,k-1) points
    tmp1 = -0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k-1)+nuT(i,j+1,k-1) )
   !tmp2 = -nu_sgs at tau_23(i,j,k) points
    tmp2 = -0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k+1)+nuT(i,j+1,k+1) )

    sub(k) = sub(k) - 0.5d0*rkdt*tmp1*rdze(k-1)*rdzc(k)
    sup(k) = sup(k) - 0.5d0*rkdt*tmp2*rdze(k)*rdzc(k)
    diag(k) = diag(k) + 0.5d0*rkdt*( tmp1*rdze(k-1)*rdzc(k) &
                                     +tmp2*rdze(k)*rdzc(k) )
   enddo
  elseif (Vmodel.NE.'DNS') then
   write(IOUT,'(a)') "ABORTING ADVANCE CN U2 LHS, VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif

 elseif (var==3) then
  n = ez-(sz-1)+1
  tsz = sz-1
  tez = ez
  allocate( sub(tsz:tez), stat=s1 )
  allocate( sup(tsz:tez), stat=s1 )
  allocate( diag(tsz:tez), stat=s1 )
  allocate( rhs(tsz:tez), stat=s1 )
  do k=tsz,tez
   sub(k) = 0.d0
   sup(k) = 0.d0
   diag(k) = 0.d0
   rhs(k)  = 0.d0
  enddo
  !viscous term: d2w_dz2
  if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
   do k = tsz+1,tez-1
    sub(k) = -0.5d0*rkdt*rRe*rdze(k)*rdzc(k)
    sup(k) = -0.5d0*rkdt*rRe*rdze(k)*rdzc(k+1)
    diag(k) = 1.d0+0.5d0*rkdt*rRe*rdze(k)*(rdzc(k)+rdzc(k+1))
   enddo
  !extra diffusion
   do k = tsz+1,tez-1
    sub(k) = sub(k)-0.5d0*rkdt*Deddy(k)*rdze(k-1)*rdzc(k)
    sup(k) = sup(k)-0.5d0*rkdt*Deddy(k)*rdze(k)*rdzc(k)
    diag(k) = diag(k)+0.5d0*rkdt*Deddy(k)*rdzc(k)*(rdze(k)+rdze(k-1))
   enddo
  else
   write(IOUT,'(a)') "ABORTING ADVANCE CN U3_LHS "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif
  !subgrid term: - nu_sgs d2wdz2
  !no interp. since tau_33, del_g, modS and Csgs are at P-point
  if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
   do k = tsz+1,tez-1
    tmp1 = -nuT(i,j,k)
    tmp2 = -nuT(i,j,k+1)
    sub(k) = sub(k) - 0.5d0*rkdt*tmp1*rdze(k)*rdzc(k)
    sup(k) = sup(k) - 0.5d0*rkdt*tmp2*rdze(k)*rdzc(k+1)
    diag(k) = diag(k) + 0.5d0*rkdt*( tmp1*rdze(k)*rdzc(k) & 
                                    +tmp2*rdze(k)*rdzc(k+1) )
   enddo
  elseif (Vmodel.NE.'DNS') then
   write(IOUT,'(a)') "ABORTING ADVANCE CN U3_LHS "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif
 
 elseif (var==5) then
  n = (ez+1)-(sz-1)+1
  tsz = sz-1
  tez = ez+1
  allocate( sub(tsz:tez), stat=s1)
  allocate( sup(tsz:tez), stat=s1)
  allocate( diag(tsz:tez), stat=s1)
  allocate( rhs(tsz:tez), stat=s1)
  do k=tsz,tez
   sub(k)= 0.d0
   sup(k)= 0.d0
   diag(k) = 0.d0
   rhs(k) = 0.d0
  enddo
  !Diffusive term: d2r_dz2
  if (Rmodel.EQ.'DNSrho'.or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then
   do k = tsz+1,tez-1
    sub(k) = -0.5d0*rkdt*rRe*rPr*rdze(k-1)*rdzc(k)
    sup(k) = -0.5d0*rkdt*rRe*rPr*rdze(k)*rdzc(k)
    diag(k) = 1.d0+0.5d0*rkdt*rRe*rPr*rdzc(k)*(rdze(k)+rdze(k-1))
   enddo
  !extra diffusion
   do k = tsz+1,tez-1
    sub(k) = sub(k)-0.5d0*rkdt*Deddy(k)*rdze(k-1)*rdzc(k)
    sup(k) = sup(k)-0.5d0*rkdt*Deddy(k)*rdze(k)*rdzc(k)
    diag(k) = diag(k)+0.5d0*rkdt*Deddy(k)*rdzc(k)*(rdze(k)+rdze(k-1))
   enddo
  else
   write(IOUT,'(a)') "ABORTING ADVANCE CN RHO_LHS, RMODEL = "//trim(Rmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif
  !subgrid term: -kappaT d2rdz2
  !interp. CTsgs, modS, and del_g to Q_3 location
  if (Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then
   do k = tsz+1,tez-1
   !tmp1 = -k_sgs at Q_3(i,j,k-1)
    tmp1 = -0.5d0*( kappaT(i,j,k-1)+kappaT(i,j,k) )
   !tmp2 = -k_sgs at Q_3(i,j,k) 
    tmp2 = -0.5d0*( kappaT(i,j,k)+kappaT(i,j,k+1) )

    sub(k) = sub(k) - 0.5d0*rkdt*tmp1*rdze(k-1)*rdzc(k)
    sup(k) = sup(k) - 0.5d0*rkdt*tmp2*rdze(k)*rdzc(k)
    diag(k) = diag(k) + 0.5d0*rkdt*( tmp1*rdze(k-1)*rdzc(k) &
                                    +tmp2*rdze(k)*rdzc(k) )
   enddo
  elseif (Rmodel.NE.'DNSrho') then
   write(IOUT,'(a)') "ABORTING ADVANCE CN RHO_LHS, RMODEL = "//trim(Rmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif

 elseif (var==8) then
  n = (ez+1)-(sz-1)+1
  tsz = sz-1
  tez = ez+1
  allocate( sub(tsz:tez), stat=s1)
  allocate( sup(tsz:tez), stat=s1)
  allocate( diag(tsz:tez), stat=s1)
  allocate( rhs(tsz:tez), stat=s1)
  do k=tsz,tez
   sub(k)= 0.d0
   sup(k)= 0.d0
   diag(k) = 0.d0
   rhs(k) = 0.d0
  enddo
  !Diffusive term: d2scal1_dz2
  if (Smodel.EQ.'DNSscal1'.or.Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then
   do k = tsz+1,tez-1
    sub(k) = -0.5d0*rkdt*rRe*rSc*rdze(k-1)*rdzc(k)
    sup(k) = -0.5d0*rkdt*rRe*rSc*rdze(k)*rdzc(k)
    diag(k) = 1.d0+0.5d0*rkdt*rRe*rSc*rdzc(k)*(rdze(k)+rdze(k-1))
   enddo
  !extra diffusion
   do k = tsz+1,tez-1
    sub(k) = sub(k)-0.5d0*rkdt*Deddy(k)*rdze(k-1)*rdzc(k)
    sup(k) = sup(k)-0.5d0*rkdt*Deddy(k)*rdze(k)*rdzc(k)
    diag(k) = diag(k)+0.5d0*rkdt*Deddy(k)*rdzc(k)*(rdze(k)+rdze(k-1))
   enddo
  else
   write(IOUT,'(a)') "ABORTING ADVANCE CN SCAL1_LHS, SMODEL = "//trim(Smodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif
  !subgrid term: - nappa_sgs d2sdz2
  !interp. CSsgs, modS, and del_g to Q_3 location
  if (Smodel.EQ.'SSMscal1'.or.Rmodel.EQ.'DSMscal1') then
   do k = tsz+1,tez-1
   !tmp1 = -n_sgs at QS_3(i,j,k-1)
    tmp4 = -0.5d0*( nappaT(i,j,k-1)+nappaT(i,j,k) )
   !tmp2 = -n_sgs at QS_3(i,j,k) 
    tmp4 = -0.5d0*( nappaT(i,j,k)+nappaT(i,j,k+1) )

    sub(k) = sub(k) - 0.5d0*rkdt*tmp1*rdze(k-1)*rdzc(k)
    sup(k) = sup(k) - 0.5d0*rkdt*tmp2*rdze(k)*rdzc(k)
    diag(k) = diag(k) + 0.5d0*rkdt*( tmp1*rdze(k-1)*rdzc(k) &
                                    +tmp2*rdze(k)*rdzc(k) )
   enddo
  elseif (Smodel.NE.'DNSscal1') then
   write(IOUT,'(a)') "ABORTING ADVANCE CN SCAL1_LHS, SMODEL = "//trim(Smodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif

 else
  write(IOUT,'(a)') "ABORTING ADVANCE CN LHS, field ",var," IS NOT IMPLEMENTED"
  stat=1
  return
 endif


 do k=tsz+1,tez-1
    if (var==1) then
     !d2u/dz2
     if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then 
      rhs(k)=f1*veltemp2(i,j,k)+f2*veltemp1(i,j,k)*f3 &
           + rkdt*0.5d0*rRe*( ( veltemp2(i,j,k+1) - veltemp2(i,j,k) )*rdze(k)   &
           -( veltemp2(i,j,k) - veltemp2(i,j,k-1) )*rdze(k-1) )*rdzc(k) 
      !extra diffusion
      rhs(k) = rhs(k) + rkdt*0.5d0*Deddy(k)*( ( veltemp2(i,j,k+1) - veltemp2(i,j,k) )* rdze(k) &
            -( veltemp2(i,j,k) - veltemp2(i,j,k-1) )*rdze(k-1) )*rdzc(k)
     else
      write(IOUT,'(a)') "ABORTING ADVANCE CN U1_RHS, VMODEL =  "//trim(Vmodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif
     !subgid
     if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then 
     !tmp1 = -nu_sgs at tau_13(i,j,k-1) points
      tmp1 = -0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k-1)+nuT(i+1,j,k-1) )
     !tmp2 = -nu_sgs at tau_13(i,j,k) points
      tmp2 = -0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k+1)+nuT(i+1,j,k+1) )

      rhs(k) = rhs(k) + rkdt*0.5d0*( tmp2*(veltemp2(i,j,k+1)-veltemp2(i,j,k))*rdze(k) &
                                     -tmp1*(veltemp2(i,j,k)-veltemp2(i,j,k-1))*rdze(k-1) )*rdzc(k)

     elseif (Vmodel.NE.'DNS') then
      write(IOUT,'(a)') "ABORTING ADVANCE CN U1_RHS, VMODEL =  "//trim(Vmodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif
    elseif (var==2) then
     !d2v/dz2
     if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then 
      rhs(k)=f1*veltemp2(i,j,k)+f2*veltemp1(i,j,k)*f3 &
           + rkdt*0.5d0*rRe*( ( veltemp2(i,j,k+1) - veltemp2(i,j,k) )*rdze(k)   &
           -( veltemp2(i,j,k) - veltemp2(i,j,k-1) )*rdze(k-1) )*rdzc(k) 
      !extra diffusion
      rhs(k) = rhs(k) + rkdt*0.5d0*Deddy(k)*( ( veltemp2(i,j,k+1) - veltemp2(i,j,k) )* rdze(k) &
            -( veltemp2(i,j,k) - veltemp2(i,j,k-1) )*rdze(k-1) )*rdzc(k)
     else
      write(IOUT,'(a)') "ABORTING ADVANCE CN U2_RHS, VMODEL =  "//trim(Vmodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif
     !subgid 
     if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then 
     !tmp1 = -nu_sgs at tau_23(i,j,k-1) points
      tmp1 = -0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k-1)+nuT(i,j+1,k-1) )
     !tmp2 = -nu_sgs at tau_23(i,j,k) points
      tmp2 = -0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k+1)+nuT(i,j+1,k+1) )
      
      rhs(k) = rhs(k) + rkdt*0.5d0*( tmp2*(veltemp2(i,j,k+1)-veltemp2(i,j,k))*rdze(k)&
                                     -tmp1*(veltemp2(i,j,k)-veltemp2(i,j,k-1))*rdze(k-1) )*rdzc(k)
     elseif (Vmodel.NE.'DNS') then
      write(IOUT,'(a)') "ABORTING ADVANCE CN U2_RHS, VMODEL =  "//trim(Vmodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif
    elseif (var==3) then
     !d2w/dz2
     if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then 
      rhs(k)=f1*veltemp2(i,j,k)+f2*veltemp1(i,j,k)*f3 &
            + rkdt*0.5d0*rRe*( ( veltemp2(i,j,k+1) - veltemp2(i,j,k) )*rdzc(k+1)  &
            -( veltemp2(i,j,k) - veltemp2(i,j,k-1) )*rdzc(k) )*rdze(k) 
      !extra diffusion
      rhs(k) = rhs(k) + rkdt*0.5d0*Deddy(k)*( ( veltemp2(i,j,k+1) - veltemp2(i,j,k) )* rdze(k) &
            -( veltemp2(i,j,k) - veltemp2(i,j,k-1) )*rdze(k-1) )*rdzc(k)
     !subgid 
     else 
      write(IOUT,'(a)') "ABORTING ADVANCE CN U3_RHS "//trim(Vmodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif
     if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then 
     !tmp1 = -nu_sgs(i,j,k)
      tmp1 = -nuT(i,j,k)
     !tmp2 = -nu_sgs(i,j,k+1)
      tmp2 = -nuT(i,j,k+1)
      rhs(k) = rhs(k) + rkdt*0.5d0*( tmp2*(veltemp2(i,j,k+1)-veltemp2(i,j,k))*rdzc(k+1)&
                                    -tmp2*(veltemp2(i,j,k)-veltemp2(i,j,k-1))*rdzc(k) )*rdze(k)
     elseif (Vmodel.NE.'DNS') then
      write(IOUT,'(a)') "ABORTING ADVANCE CN U3_RHS "//trim(Vmodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif
    elseif (var==5) then
     !d2rho/dz2
     if (Rmodel.EQ.'DNSrho'.or.Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then 
      rhs(k)=f1*veltemp2(i,j,k)+f2*veltemp1(i,j,k)*f3 &
            + rkdt*0.5d0*rRe*rPr*( ( veltemp2(i,j,k+1) - veltemp2(i,j,k) )* rdze(k) &
            -( veltemp2(i,j,k) - veltemp2(i,j,k-1) )*rdze(k-1) )*rdzc(k)

      !extra diffusion
      rhs(k) = rhs(k) + rkdt*0.5d0*Deddy(k)*( ( veltemp2(i,j,k+1) - veltemp2(i,j,k) )* rdze(k) &
            -( veltemp2(i,j,k) - veltemp2(i,j,k-1) )*rdze(k-1) )*rdzc(k)
     else
      write(IOUT,'(a)') "ABORTING ADVANCE CN RHO_RHS, RMODEL =  "//trim(Rmodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif
     !subgid
     if (Rmodel.EQ.'SSMrho'.or.Rmodel.EQ.'DSMrho') then 
     !tmp1 = -k_sgs at Q_3(i,j,k-1)
      tmp1 = -0.5d0*( kappaT(i,j,k-1)+kappaT(i,j,k) )
     !tmp2 = -k_sgs at Q_3(i,j,k) 
      tmp2 = -0.5d0*( kappaT(i,j,k)+kappaT(i,j,k+1) )
 
      rhs(k) = rhs(k) + rkdt*0.5d0*( tmp2*(veltemp2(i,j,k+1)-veltemp2(i,j,k))*rdze(k) &
                                    -tmp1*(veltemp2(i,j,k)-veltemp2(i,j,k-1))*rdze(k-1) )*rdzc(k)
     elseif (Rmodel.NE.'DNSrho') then
      write(IOUT,'(a)') "ABORTING ADVANCE CN RHO_RHS, RMODEL =  "//trim(Rmodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif
    elseif (var==8) then
     !d2scal1/dz2
     if (Smodel.EQ.'DNSscal1'.or.Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then 
      rhs(k)=f1*veltemp2(i,j,k)+f2*veltemp1(i,j,k)*f3 &
            + rkdt*0.5d0*rRe*rSc*( ( veltemp2(i,j,k+1) - veltemp2(i,j,k) )* rdze(k) &
            -( veltemp2(i,j,k) - veltemp2(i,j,k-1) )*rdze(k-1) )*rdzc(k)

      !extra diffusion
      rhs(k) = rhs(k) + rkdt*0.5d0*Deddy(k)*( ( veltemp2(i,j,k+1) - veltemp2(i,j,k) )* rdze(k) &
            -( veltemp2(i,j,k) - veltemp2(i,j,k-1) )*rdze(k-1) )*rdzc(k)
     else
      write(IOUT,'(a)') "ABORTING ADVANCE CN SCAL1_RHS, SMODEL =  "//trim(Smodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif
     !subgid
     if (Smodel.EQ.'SSMscal1'.or.Smodel.EQ.'DSMscal1') then 
     !tmp1 = -k_sgs at QS_3(i,j,k-1)
      tmp1 = -0.5d0*( nappaT(i,j,k-1)+nappaT(i,j,k) )
     !tmp2 = -k_sgs at QS_3(i,j,k) 
      tmp2 = -0.5d0*( nappaT(i,j,k)+nappaT(i,j,k+1) )
 
      rhs(k) = rhs(k) + rkdt*0.5d0*( tmp2*(veltemp2(i,j,k+1)-veltemp2(i,j,k))*rdze(k) &
                                    -tmp1*(veltemp2(i,j,k)-veltemp2(i,j,k-1))*rdze(k-1) )*rdzc(k)
     elseif (Smodel.NE.'DNSscal1') then
      write(IOUT,'(a)') "ABORTING ADVANCE CN SCAL1_RHS, SMODEL =  "//trim(Smodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif

    else
      write(IOUT,'(a)') "ABORTING ADVANCE CN RHS, FIELD ",var,"IS NOT IMPLEMENTED"
      stat=1
      return
     
    endif
  enddo

 !enforce BC for u1
   if (var==1) then
    !Bottom 
    !Dirichlet
    if (TB(3,1,var)==1) then
     sub(tsz)= 0.d0
     diag(tsz)= 1.d0
     sup(tsz)= 0.0
     rhs(tsz)= VB(3,1,var)
    !Neumann
    else if (TB(3,1,var)==2) then
     sub(tsz)= 0.d0
     diag(tsz)= -1.d0
     sup(tsz)= 1.d0
     rhs(tsz)= dzc(tsz)*VB(3,1,var)
    !Wall6
    else if (TB(3,1,var)==6) then
     sub(tsz)= 0.d0
     diag(tsz)= 1.d0
     sup(tsz)= 1.d0
     rhs(tsz)= 2.d0*VB(3,1,var)
    !Wall7
    else if (TB(3,1,var)==7) then
     continue
    else
     write(IOUT,'(a)') "Incorrect BC for u1 at bottom in var_advance_CN"
     stop
    end if
!Top wall
    !Dirichlet
    if (TB(3,2,var)==1) then
     sub(tez)= 0.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez) = VB(3,2,var)
    !Neumann
    else if (TB(3,2,var)==2) then
     sub(tez)= -1.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez) = dzc(tez)*VB(3,2,var)
    !Wall6
    else if (TB(3,2,var)==6) then
     sub(tez)= 1.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez)= 2.d0*VB(3,2,var)
    !Wall7
    else if (TB(3,2,var)==7) then
     continue
    else
     write(IOUT,'(a)') "Incorrect BC for u1 at top in var_advance_CN"
     stop
    end if
 !enforce BC for u2
   else if (var==2) then
    !Bottom
    !Dirichlet
    if (TB(3,1,var)==1) then
     sub(tsz)= 0.d0
     diag(tsz)= 1.d0
     sup(tsz)= 0.d0
     rhs(tsz) = VB(3,1,var)
    !Neumann
    else if (TB(3,1,var)==2) then
     sub(tsz)= 0.d0
     diag(tsz)= -1.d0
     sup(tsz)= 1.d0
     rhs(tsz) = dzc(tsz)*VB(3,1,var)
    !Wall6
    else if (TB(3,1,var)==6) then
     sub(tsz)= 0.d0
     diag(tsz)= 1.d0
     sup(tsz)= 1.d0
     rhs(tsz)= 2.d0*VB(3,1,var)
    !Wall7
    else if (TB(3,1,var)==7) then
     continue
    else
     write(IOUT,'(a)') "Incorrect BC for u2 at bottom in var_advance_CN"
     stop
    end if
 !Top
    !Dirichlet
    if (TB(3,2,var)==1) then
     sub(tez)= 0.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez) = VB(3,2,var)
   !Neumann
    else if (TB(3,2,var)==2) then
     sub(tez)= -1.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez) = VB(3,2,var)*dzc(tez)
    !Wall6
    else if (TB(3,2,var)==6) then
     sub(tez)= 1.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez)= 2.d0*VB(3,2,var)
    !Wall7
    else if (TB(3,2,var)==7) then
      continue
    else
     write(IOUT,'(a)') "Incorrect BC for u2 at top in var_advance_CN"
     stop
    end if
!enforce BC for u3
   else if (var==3) then
    !Bottom
    !Dirichlet
    if (TB(3,1,var)==1) then
     sub(tsz)= 0.d0
     diag(tsz)= 1.d0
     sup(tsz)= 0.d0
     rhs(tsz)= VB(3,1,var)
    !Neumann
    else if (TB(3,1,var)==2) then
     sub(tsz)= 0.d0
     diag(tsz)= -1.d0
     sup(tsz)= 1.d0
     rhs(tsz) = VB(3,1,var)*dzc(tsz)
    !Wall6
    else if (TB(3,1,var)==6) then
     sub(tsz)= 0.d0
     diag(tsz)= 1.d0
     sup(tsz)= 1.d0
     rhs(tsz)= 2.d0*VB(3,1,var)
    !Wall7
    else if (TB(3,1,var)==7) then
      continue
    else
     write(IOUT,'(a)') "Incorrect BC for u3 at bottom in var_advance_CN"
     stop
    end if
 !Top
    !Dirichlet
    if (TB(3,2,var)==1) then
     sub(tez)= 0.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez) = VB(3,2,var)
    !Neumann
    else if (TB(3,2,var)==2) then
     sub(tez)= -1.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez) = VB(3,2,var)*dzc(tez)
    !Wall6
    else if (TB(3,2,var)==6) then
     sub(tez)= 1.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez)= 2.d0*VB(3,2,var)
    !Wall7
    else if (TB(3,2,var)==7) then
     sub(tez)= 0.d0
     diag(tez)=1.d0
     sup(tez)= 0.d0
     rhs(tez)= 0.d0
     continue
    else
     write(IOUT,'(a)') "Incorrect BC for u3 at top in var_advance_CN"
     stop
    end if

 !enforce BC for rho
   else if (var==5) then
!Bottom
    !Dirichlet
    if (TB(3,1,var)==1) then
     sub(tsz)= 0.d0
     diag(tsz)= 1.d0
     sup(tsz)= 1.d0
     rhs(tsz) = 2.d0*VB(3,1,var)
    !Neumann
    else if (TB(3,1,var)==2) then
     sub(tsz)= 0.d0
     diag(tsz)= -1.d0
     sup(tsz)= 1.d0
     rhs(tsz)= VB(3,1,var)*dzc(k)
     !Important: also enforce BC for one point above the suface for code stability
     sub(tsz+1)= 0.d0
     diag(tsz+1)= -1.d0
     sup(tsz+1)= 1.d0
     rhs(tsz+1)= VB(3,1,var)*dzc(k+1)
    !Wall6
    else if (TB(3,1,var)==6) then
     sub(tsz)= 0.d0
     diag(tsz)= 1.d0
     sup(tsz)= 1.d0
     rhs(tsz)= 2.d0*VB(3,1,var)
    else
     write(IOUT,'(a)') "Incorrect BC for rho at bottom in var_advance_CN"
     stop
    end if
 !Top
    !Dirichlet
    if (TB(3,2,var)==1) then
     sub(tez)= 1.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez)= 2.d0*VB(3,2,var)
    !Neumann
    else if (TB(3,2,var)==2) then
     sub(tez)= -1.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez)= VB(3,2,var)*dzc(k)
     !Important: also enforce BC for one point below the suface for code stability
     sub(tez-1)= -1.d0
     diag(tez-1)= 1.d0
     sup(tez-1)= 0.d0
     rhs(tez-1)= VB(3,2,var)*dzc(k-1)
    !Wall6
    else if (TB(3,2,var)==6) then
     sub(tez)= 1.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez)= 2.d0*VB(3,2,var)
    else
     write(IOUT,'(a)') "Incorrect BC for rho at top in var_advance_CN"
     stop
    end if

 !enforce BC for scal1
   else if (var==8) then
!Bottom
    !Dirichlet
    if (TB(3,1,var)==1) then
     sub(tsz)= 0.d0
     diag(tsz)= 1.d0
     sup(tsz)= 1.d0
     rhs(tsz) = 2.d0*VB(3,1,var)
    !Neumann
    else if (TB(3,1,var)==2) then
     sub(tsz)= 0.d0
     diag(tsz)= -1.d0
     sup(tsz)= 1.d0
     rhs(tsz)= VB(3,1,var)*dzc(k)
     !Important: also enforce BC for one point above the suface for code stability 
     sub(tsz+1)= 0.d0
     diag(tsz+1)= -1.d0
     sup(tsz+1)= 1.d0
     rhs(tsz+1)= VB(3,1,var)*dzc(k+1)
    !Wall6
    else if (TB(3,1,var)==6) then
     sub(tsz)= 0.d0
     diag(tsz)= 1.d0
     sup(tsz)= 1.d0
     rhs(tsz)= 2.d0*VB(3,1,var)
    else
     write(IOUT,'(a)') "Incorrect BC for scal1 at bottom in var_advance_CN"
     stop
    end if
 !Top
    !Dirichlet
    if (TB(3,2,var)==1) then
     sub(tez)= 1.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez)= 2.d0*VB(3,2,var)
    !Neumann
    else if (TB(3,2,var)==2) then
     sub(tez)= -1.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez)= VB(3,2,var)*dzc(k)
     !Important: also enforce BC for one point below the suface for code stability
     sub(tez-1)= -1.d0
     diag(tez-1)= 1.d0
     sup(tez-1)= 0.d0
     rhs(tez-1)= VB(3,2,var)*dzc(k-1)
    !Wall6
    else if (TB(3,2,var)==6) then
     sub(tez)= 1.d0
     diag(tez)= 1.d0
     sup(tez)= 0.d0
     rhs(tez)= 2.d0*VB(3,2,var)
    else
     write(IOUT,'(a)') "Incorrect BC for scal1 at top in var_advance_CN"
     stop
    end if

   end if
  
   call Thomas(sub,diag,sup,rhs,n)
   do k=tsz,tez
    veltemp2(i,j,k) = rhs(k)
   enddo

  deallocate(sub, stat=s1)
  deallocate(sup, stat=s1)
  deallocate(diag, stat=s1)
  deallocate(rhs, stat=s1)
  enddo
 enddo

 return
end subroutine var_advance_CN


subroutine var_advance_CN_PL(veltemp1,veltemp2,f1,f2,f3,rkdt,var)
!@t
! \textbf{subroutine var\_advance(veltemp1,veltemp2,f1,f2,f3)}
!@h
!   Description:
!     Advances a variable. KYLE?
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     -------  
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: r8
 use Flow,   only: Vmodel, Rmodel, Smodel, p, r
 use LESmod
 use Grid
 use DD,     only: comm3d,sizeX3,coords
 use Domain, only: sx,ex,sy,ey,sz,ez
 use boundC, only: TB,VB,BND
 use Parameters, only: delt,rRe,rPr,rSc,LESflag, pcorrect
 use IO,     only: IOUT
 implicit none
!Passed Variables
 real(r8),intent(inout)     :: veltemp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: veltemp2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: f1, f2, f3,rkdt
 integer, intent(in)        :: var
!Local Variables
 real(r8),allocatable,dimension(:,:,:) :: sub,diag,sup,rhs
 real(r8)                              :: tmp1,tmp2,tmp3,tmp4,del_g, Deddy(sz-1:ez+1),raver1,raver2
 integer                   :: i,j,k,l,m,s1,stat,err,EW


!Extra-diffusion
   Deddy = 0.d0
   if (coords(3).EQ.sizeX3-1) then
    if (var.EQ.1.or.var.EQ.2.or.var.EQ.3) then
     do k = sz-1, ez+1
      Deddy(k) = 0.d0*8.1518d-6*exp(-(zc(k)/0.1d0)**2.d0)
     enddo
     elseif (var.EQ.5.or.var.EQ.8.or.var.EQ.9.or.var.EQ.10.or.var.EQ.11) then
     do k = sz-1, ez+1
      Deddy(k) = 0.d0*1.d0/0.1d0*8.1518d-6*exp(-(zc(k)/0.1d0)**2.d0)
     enddo
    endif
   endif
   Deddy = 0.d0

   allocate( sub(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1)
   allocate( sup(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1)
   allocate( diag(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1)
   allocate( rhs(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1)
   sub(:,:,:)=0.d0
   sup(:,:,:)=0.d0
   diag(:,:,:)=0.d0
   rhs(:,:,:)=0.d0
!Compute rhs with the viscous term, call thomas, get updated value for each ij pencil
!Setup the coefficients of the tridiagonal matrix 
 select case (var)
  case(1,2)
   EW = 0
  case(3)
   EW = 0
  case(5)
   EW = 0
  case(8)
   EW = 0
  case(9)
   EW = 0
  case(10)
   EW = 0
  case(11)
   EW = 0
  case DEFAULT
   write(IOUT,'(a)') "ABORTING ADVANCE CN, VARID IS NOT VALID"
   stat=1
   return
  end select

 do j=sy-1,ey+1
 do i=sx-1,ex+1

 select case (var) 
  case(1,2,5,8,9,10,11)
   !viscous term: d2u_dz2 and extra vis
   if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
   do k = sz,ez
    if (var.EQ.1.or.var.EQ.2) then
     raver1 = 0.5*(r(i,j,k+1)+r(i,j,k))
     raver2 = 0.5*(r(i,j,k)+r(i,j,k-1))
     tmp1 = (rRe + Deddy(k))*raver1/r(i,j,k)
     tmp2 = (rRe + Deddy(k))*raver2/r(i,j,k) 
    elseif (var.EQ.5.or.var.EQ.9.or.var.EQ.10.or.var.EQ.11) then
     raver1 = 0.5*(r(i,j,k+1)+r(i,j,k))
     raver2 = 0.5*(r(i,j,k)+r(i,j,k-1))
     tmp1 = (rRe*rPr + Deddy(k))*raver1/r(i,j,k) !+ kappasm(k)
     tmp2 = (rRe*rPr + Deddy(k))*raver2/r(i,j,k) !+ kappasm(k)
    elseif (var.EQ.8) then
     tmp1 = rRe*rSc + Deddy(k) !+ nappasm(k)
     tmp2 = rRe*rSc + Deddy(k) !+ nappasm(k)
    endif
    sub(i,j,k) = -0.5d0*rkdt*tmp1*rdze(k-1)*rdzc(k)
    sup(i,j,k) = -0.5d0*rkdt*tmp2*rdze(k)*rdzc(k)
    diag(i,j,k) = 1.d0+0.5d0*rkdt*(tmp1*rdze(k-1)+tmp2*rdze(k))*rdzc(k)
   enddo
  else
   write(IOUT,'(a)') "ABORTING ADVANCE CN U1 LHS, VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif
  !subgrid term: the implicit part of d(tau_13)/dz 
  if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
   do k = sz,ez
     raver1 = 0.5*(r(i,j,k+1)+r(i,j,k))
     raver2 = 0.5*(r(i,j,k)+r(i,j,k-1))
    if (var.EQ.1) then
     tmp1 = -0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k-1)+nuT(i+1,j,k-1) )*raver1/r(i,j,k)
     tmp2 = -0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k+1)+nuT(i+1,j,k+1) )*raver2/r(i,j,k)
    elseif (var.EQ.2) then
     tmp1 = -0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k-1)+nuT(i,j+1,k-1) )*raver1/r(i,j,k)
     tmp2 = -0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k+1)+nuT(i,j+1,k+1) )*raver2/r(i,j,k)
    elseif (var.EQ.5.or.var.EQ.9.or.var.EQ.10.or.var.EQ.11) then
     tmp1 = -0.5d0*( kappaT(i,j,k)+kappaT(i,j,k-1) )*raver1/r(i,j,k)
     tmp2 = -0.5d0*( kappaT(i,j,k)+kappaT(i,j,k+1) )*raver2/r(i,j,k)
    elseif (var.EQ.8) then
     tmp1 = -0.5d0*( nappaT(i,j,k)+nappaT(i,j,k-1) )
     tmp2 = -0.5d0*( nappaT(i,j,k)+nappaT(i,j,k+1) )
    endif
    sub(i,j,k) = sub(i,j,k) - LESflag*0.5d0*rkdt*tmp1*rdze(k-1)*rdzc(k)
    sup(i,j,k) = sup(i,j,k) - LESflag*0.5d0*rkdt*tmp2*rdze(k)*rdzc(k)
    diag(i,j,k) = diag(i,j,k) + LESflag*0.5d0*rkdt*(tmp1*rdze(k-1)+tmp2*rdze(k))*rdzc(k) 
   enddo

  elseif (Vmodel.NE.'DNS') then
   write(IOUT,'(a)') "ABORTING ADVANCE CN LHS ",var," VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif

 case (3) 
  !viscous term: d2w_dz2
  if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
   do k = sz,ez
    tmp1 = (rRe + Deddy(k))*r(i,j,k)/(0.5*(r(i,j,k+1)+r(i,j,k)))
    tmp2 = (rRe + Deddy(k+1))*r(i,j,k+1)/(0.5*(r(i,j,k+1)+r(i,j,k)))
    sub(i,j,k) = -0.5d0*rkdt*tmp1*rdze(k)*rdzc(k)
    sup(i,j,k) = -0.5d0*rkdt*tmp2*rdze(k)*rdzc(k+1)
    diag(i,j,k) = 1.d0+0.5d0*rkdt*(tmp1*rdzc(k)+tmp2*rdzc(k+1))*rdze(k)
   enddo
  else
   write(IOUT,'(a)') "ABORTING ADVANCE CN LHS ",var," VMODEL = "//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif
  !subgrid term: the whole d(tau_33)/dz = d(2*nuT dw/dz)/dz
  if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
   do k = sz,ez
    tmp1 = -nuT(i,j,k)*r(i,j,k)/(0.5*(r(i,j,k+1)+r(i,j,k)))
    tmp2 = -nuT(i,j,k+1)*r(i,j,k+1)/(0.5*(r(i,j,k+1)+r(i,j,k)))
    sub(i,j,k) = sub(i,j,k) - LESflag*1.d0*rkdt*tmp1*rdze(k)*rdzc(k)
    sup(i,j,k) = sup(i,j,k) - LESflag*1.d0*rkdt*tmp2*rdze(k)*rdzc(k+1)
    diag(i,j,k) = diag(i,j,k) + LESflag*1.d0*rkdt*(tmp1*rdzc(k)+tmp2*rdzc(k+1))*rdze(k)
   enddo
  elseif (Vmodel.NE.'DNS') then
   write(IOUT,'(a)') "ABORTING ADVANCE CN LHS ",var," VMODEL="//trim(Vmodel)//" NOT IMPLEMENTED"
   stat=1
   return
  endif

  case DEFAULT
  write(IOUT,'(a)') "ABORTING ADVANCE CN LHS, field ",var," IS NOT IMPLEMENTED"
   stat=1
   return
  end select
 
 enddo !i
 enddo !j

!RIGHT HAND SIDE
 do j=sy-1,ey+1
 do i=sx-1,ex+1
 do k=sz,ez
  select case (var)
   case (1,2,5,8,9,10,11)
     if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
      rhs(i,j,k)=f1*veltemp2(i,j,k)+f2*veltemp1(i,j,k)*f3
      
     !predicted pressure
      if (var.EQ.1.and.pcorrect) then
       rhs(i,j,k) = rhs(i,j,k) - rkdt*( p(i+1,j,k)-p(i,j,k) )*rdxe(i) 
      elseif (var.EQ.2.and.pcorrect) then
        rhs(i,j,k) = rhs(i,j,k) - rkdt*( p(i,j+1,k)-p(i,j,k) )*rdye(j) 
      endif
    
     !Viscous
       raver1 = 0.5*(r(i,j,k+1)+r(i,j,k))
       raver2 = 0.5*(r(i,j,k)+r(i,j,k-1))
      if (var.EQ.1.or.var.EQ.2) then
       tmp1 = (rRe + Deddy(k))*raver1/r(i,j,k)
       tmp2 = (rRe + Deddy(k))*raver2/r(i,j,k) 
      elseif (var.EQ.5.or.var.EQ.9.or.var.EQ.10.or.var.EQ.11) then
       tmp1 = (rRe*rPr + Deddy(k))*raver1/r(i,j,k) !+ kappasm(k)
       tmp2 = (rRe*rPr + Deddy(k))*raver2/r(i,j,k) !+ kappasm(k)
      elseif (var.EQ.8) then
       tmp1 = rRe*rSc + Deddy(k) !+ nappasm(k)
       tmp2 = rRe*rSc + Deddy(k) !+ nappasm(k) 
      endif
      rhs(i,j,k) = rhs(i,j,k) + rkdt*0.5d0*( tmp2*(veltemp2(i,j,k+1)-veltemp2(i,j,k))*rdze(k) &
                                            -tmp1*(veltemp2(i,j,k)-veltemp2(i,j,k-1))*rdze(k-1) )*rdzc(k)
     else
      write(IOUT,'(a)') "ABORTING ADVANCE CN RHS ",var," VMODEL =  "//trim(Vmodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif

     !subgid
     if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
      if (var.EQ.1.) then
       tmp1 = -0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k-1)+nuT(i+1,j,k-1) )*raver1/r(i,j,k)
       tmp2 = -0.25d0*( nuT(i,j,k)+nuT(i+1,j,k)+nuT(i,j,k+1)+nuT(i+1,j,k+1) )*raver2/r(i,j,k)
      elseif (var.EQ.2) then
       tmp1 = -0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k-1)+nuT(i,j+1,k-1) )*raver1/r(i,j,k)
       tmp2 = -0.25d0*( nuT(i,j,k)+nuT(i,j+1,k)+nuT(i,j,k+1)+nuT(i,j+1,k+1) )*raver2/r(i,j,k)
      else if (var.EQ.5.or.var.EQ.9.or.var.EQ.10.or.var.EQ.11) then
       tmp1 = -0.5d0*( kappaT(i,j,k)+kappaT(i,j,k-1) )*raver1/r(i,j,k)
       tmp2 = -0.5d0*( kappaT(i,j,k)+kappaT(i,j,k+1) )*raver2/r(i,j,k)
      else if (var.EQ.8) then
       tmp1 = -0.5d0*( nappaT(i,j,k)+nappaT(i,j,k-1) )
       tmp2 = -0.5d0*( nappaT(i,j,k)+nappaT(i,j,k+1) )
      endif
      rhs(i,j,k) = rhs(i,j,k) + LESflag*rkdt*0.5d0*( tmp2*(veltemp2(i,j,k+1)-veltemp2(i,j,k))*rdze(k) &
                                                    -tmp1*(veltemp2(i,j,k)-veltemp2(i,j,k-1))*rdze(k-1) )*rdzc(k)


     elseif (Vmodel.NE.'DNS') then
      write(IOUT,'(a)') "ABORTING ADVANCE CN RHS ",var," VMODEL =  "//trim(Vmodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif
 
   case (3) 
     if (Vmodel.EQ.'DNS'.or.Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
      rhs(i,j,k)=f1*veltemp2(i,j,k)+f2*veltemp1(i,j,k)*f3 
      
      !predicted pressure
      if (pcorrect) rhs(i,j,k) = rhs(i,j,k) - rkdt*( p(i,j,k+1)-p(i,j,k) )*rdze(k) 
      
      !Viscous
      tmp1 = (rRe + Deddy(k))*r(i,j,k)/(0.5*(r(i,j,k+1)+r(i,j,k)))
      tmp2 = (rRe + Deddy(k+1))*r(i,j,k+1)/(0.5*(r(i,j,k+1)+r(i,j,k)))
      rhs(i,j,k) = rhs(i,j,k) + rkdt*0.5d0*( tmp2*(veltemp2(i,j,k+1)-veltemp2(i,j,k))*rdzc(k+1) &
                                            -tmp1*(veltemp2(i,j,k)-veltemp2(i,j,k-1))*rdzc(k) )*rdze(k)
     else
      write(IOUT,'(a)') "ABORTING ADVANCE CN RHS ",var," VMODEL =  "//trim(Vmodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif

     !subgid 
     if (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
      tmp1 = -nuT(i,j,k)*r(i,j,k)/(0.5*(r(i,j,k+1)+r(i,j,k)))
      tmp2 = -nuT(i,j,k+1)*r(i,j,k+1)/(0.5*(r(i,j,k+1)+r(i,j,k)))
      rhs(i,j,k) = rhs(i,j,k) + LESflag*rkdt*1.d0*( tmp2*(veltemp2(i,j,k+1)-veltemp2(i,j,k))*rdzc(k+1)&
                                           -tmp1*(veltemp2(i,j,k)-veltemp2(i,j,k-1))*rdzc(k) )*rdze(k)
     elseif (Vmodel.NE.'DNS') then
      write(IOUT,'(a)') "ABORTING ADVANCE CN RHS ",var," VMODEL =  "//trim(Vmodel)//" NOT IMPLEMENTED"
      stat=1
      return
     endif

    case DEFAULT 
      write(IOUT,'(a)') "ABORTING ADVANCE CN RHS, FIELD ",var,"IS NOT IMPLEMENTED"
      stat=1
      return
    end select

  enddo !k
 enddo !j
enddo !i


!DO NOT Ghost LHS, DO ghost RHS with different var type
   call ghost(sub,'cfluc',err)
   call ghost(diag,'cfluc',err)
   call ghost(sup,'cfluc',err)
   call ghost(rhs,'cfluc',err)

!Bottom
  if (TB(3,1,var)==1 .and. BND(3,1)) then
   !Dirichlet 
    sub(:,:,sz-1)= 0.d0
    diag(:,:,sz-1)= 1.d0
    sup(:,:,sz-1)= 0.0
    rhs(:,:,sz-1)= VB(3,1,var)
  else if (TB(3,1,var)==2.and.BND(3,1)) then
   !Neumann
   if (var==3) then
    sub(:,:,sz-1)= 0.d0
    diag(:,:,sz-1)= -1.d0
    sup(:,:,sz-1)= 1.d0
    rhs(:,:,sz-1)= dzc(sz-1)*VB(3,1,var)
   elseif (var==5.or.var==8.or.var==9.or.var==10.or.var==11) then
    sub(:,:,sz-1)= 0.d0
    diag(:,:,sz-1)= -1.d0
    sup(:,:,sz-1)= 1.d0
    rhs(:,:,sz-1)= dze(sz-1)*VB(3,1,var)
    sub(:,:,sz)= 0.d0
    diag(:,:,sz)= -1.d0
    sup(:,:,sz)= 1.d0
    rhs(:,:,sz)= dze(sz)*VB(3,1,var)
   else
    sub(:,:,sz-1)= 0.d0
    diag(:,:,sz-1)= -1.d0
    sup(:,:,sz-1)= 1.d0
    rhs(:,:,sz-1)= dze(sz-1)*VB(3,1,var)
   endif
  else if (TB(3,1,var)==6 .and. BND(3,1)) then
   !Wall6:var(sz-1)=2*VB(3,1,var)-var(sz)
    sub(:,:,sz-1)= 0.d0
    diag(:,:,sz-1)= 1.d0
    sup(:,:,sz-1)= 1.d0
    rhs(:,:,sz-1)= 2.d0*VB(3,1,var)
  else if (BND(3,1)) then
    write(IOUT,'(a)') "Incorrect BC for bottom for variable", var,"in var_advance_CN"
    stop
  endif

!Top
  if (TB(3,2,var)==1 .and. BND(3,2)) then
   !Dirichlet 
    sub(:,:,ez+1)= 0.d0
    diag(:,:,ez+1)= 1.d0
    sup(:,:,ez+1)= 0.0
    rhs(:,:,ez+1)= VB(3,2,var)
  else if (TB(3,2,var)==2 .and. BND(3,2)) then
   !Neumann
    if (var==3) then
     sub(:,:,ez+1)= -1.d0
     diag(:,:,ez+1)= 1.d0
     sup(:,:,ez+1)= 0.d0
     rhs(:,:,ez+1)= dzc(ez+1)*VB(3,2,var)
    elseif (var==5.or.var==8.or.var==9.or.var==10.or.var==11) then
     sub(:,:,ez+1)= -1.d0
     diag(:,:,ez+1)= 1.d0
     sup(:,:,ez+1)= 0.d0
     rhs(:,:,ez+1)= dze(ez+1)*VB(3,2,var)
     sub(:,:,ez)= -1.d0
     diag(:,:,ez)= 1.d0
     sup(:,:,ez)= 0.d0
     rhs(:,:,ez)= dze(ez)*VB(3,2,var)
    else 
     sub(:,:,ez+1)= -1.d0
     diag(:,:,ez+1)= 1.d0
     sup(:,:,ez+1)= 0.d0
     rhs(:,:,ez+1)= dze(ez+1)*VB(3,2,var)
    endif
  else if (TB(3,2,var)==6 .and. BND(3,2)) then
   !Wall6:var(ez+1)=2*VB(3,2,var)-var(ez)
    sub(:,:,ez+1)= 1.d0
    diag(:,:,ez+1)= 1.d0
    sup(:,:,ez+1)= 0.d0
    rhs(:,:,ez+1)= 2.d0*VB(3,2,var)
  else if (TB(3,2,var)==7 .and. BND(3,2)) then
   !Wall7:var(ez)=0.d0;var(ez+1)=0.d0
    sub(:,:,ez+1)= 0.d0
    diag(:,:,ez+1)= 1.d0
    sup(:,:,ez+1)= 0.d0
    rhs(:,:,ez+1)= 0.d0
    sub(:,:,ez)= 0.d0
    diag(:,:,ez)= 1.d0
    sup(:,:,ez)= 0.d0
    rhs(:,:,ez)= 0.d0
  else if (BND(3,2)) then
    write(IOUT,'(a)') "Incorrect BC for top for variable", var,"in var_advance_CN"
    stop    
  endif

if (sizeX3.EQ.1) then
  do j=sy,ey
   do i=sx,ex
    call Thomas(sub(i,j,:),diag(i,j,:),sup(i,j,:),rhs(i,j,:),ez-sz+3)
   enddo
  enddo
else 
  call thomas_MPI_forward_2d_z(sub,diag,sup,rhs,stat)
  call thomas_MPI_backward_2d_z(diag,sup,rhs,stat)
endif
 
 if (var==1) then
   call ghost(rhs,'u',err)
 elseif (var==2) then
   call ghost(rhs,'v',err)
 elseif (var==3) then
   call ghost(rhs,'w',err)
 elseif (var==5) then
   call ghost(rhs,'rho',err)
 elseif (var==8) then
   call ghost(rhs,'scal1',err)
 elseif (var==9) then
   call ghost(rhs,'qv',err)
 elseif (var==10) then
   call ghost(rhs,'ql',err)
 elseif (var==11) then
   call ghost(rhs,'qr',err)
  else
   write(IOUT,'(a)') "ABORTING GHOSTING CN RHS, FIELD ",var,"IS NOT IMPLEMENTED"
   stat=1
   return
  endif
  veltemp2 = rhs

  deallocate(sub, stat=s1)
  deallocate(sup, stat=s1)
  deallocate(diag, stat=s1)
  deallocate(rhs, stat=s1)

return
end subroutine var_advance_CN_PL


subroutine rk_ss_LARGE_MEMORY(a1,a2,a3,ec,dt,psolve,stat)
!@t
! \textbf{subroutine rk\_ss\_LARGE\_MEMORY(a1,a2,a3,ec,dt,psolve,stat)}
!@h
!   Description:
!     This version is antiquated and only left in for readibility.
!    (i.e. the version rk_ss above does the exact same thing with less
!      memory, but it is harder to understand by just looking at it)
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes,    only: r8,i4
 use Flow 
 use IO,        only: IOUT
 use domain,    only: sx,ex,sy,ey,sz,ez,nyp2, nxp2, nzp2

 implicit none

!Passed Variables
 real(r8),intent(in)                   :: a1, a2, a3, ec, dt
 integer(i4),intent(out)               :: stat
 logical,intent(in)                    :: psolve

!Local Variables
 integer                               :: err1
 integer(i4)                           :: niters
 real(r8)                              :: resmax
 real(r8)                              :: iflag
 integer                               :: s1
 real(r8),allocatable,dimension(:,:,:) :: u1_tmp1,u2_tmp1,u3_tmp1 
 logical,parameter                     :: debug=.false.

 err1=0
 s1=0

 allocate( u1_tmp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error allocating u1_tmp1 in rk_ss"
  goto 1000
 endif
 allocate( u2_tmp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error allocating u2_tmp1 in rk_ss"
  goto 1000
 endif

 allocate( u3_tmp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error allocating u3_tmp1 in rk_ss"
  goto 1000
 endif

  if (debug) call check_point('rk_ss#0',.false.)

 r_tmp1(:,:,:) = 0.d0

 !X1-momentum 
 call u1_rhs(u,v,w,rho,u1_tmp1,err1)
  if ( debug ) call write_plane(u1_tmp1,1,nxp2/2,0,0,'rkss_u1',.false.,err1)
  if (debug) call check_point('rk_ss#1',.false.)

 !X2-momentum 
 call u2_rhs(u,v,w,rho,u2_tmp1,err1)
  if ( debug ) call write_plane(u2_tmp1,1,nxp2/2,0,0,'rkss_u2',.false.,err1)
  if (debug) call check_point('rk_ss#2',.false.)

 !X3-momentum 
 call u3_rhs(u,v,w,rho,u3_tmp1,err1)
  if ( debug ) call write_plane(u3_tmp1,1,nxp2/2,0,0,'rkss_u3',.false.,err1)
  if (debug) call check_point('rk_ss#3',.false.)

 if (psolve) then

  !Pressure Source
  call psource(u1_tmp1,u2_tmp1,u3_tmp1,u,v,w,r_tmp1,dt,err1)
   if ( debug ) call write_plane(r_tmp1,1,nxp2/2,0,0,'source',.false.,err1)
   if ( debug ) call write_plane(r_tmp1,2,nyp2/2,0,0,'source',.false.,err1)
   if (debug) call check_point('rk_ss#4',.false.)

  !Pressure Poisson Equation 
  call mg_solver(p,r_tmp1,niters,resmax)
  write(IOUT,'(a20,i3,a11,e22.15)') "    MG Iterations: ",niters,"  RESIDUAL= ",resmax
   if ( debug ) call write_plane(p,1,nxp2/2,0,0,'p1',.false.,err1)
   if (debug) call check_point('rk_ss#5',.false.)

  !Pressure Gradient 
  call Pgrad(p,u1_tmp1,u2_tmp1,u3_tmp1,1.d0)
   if ( debug ) call write_plane(u1_tmp2,1,nxp2/2,0,0,'rkss_u1a',.false.,err1)
   if ( debug ) call write_plane(u2_tmp2,1,nxp2/2,0,0,'rkss_u2a',.false.,err1)
   if ( debug ) call write_plane(u3_tmp2,1,nxp2/2,0,0,'rkss_u3a',.false.,err1)
   if (debug) call check_point('rk_ss#6',.false.)

 endif

 !x_i 
 call var_advance(u1_tmp1,u1_tmp2,a1,a2,dt) 
 call var_advance(u2_tmp1,u2_tmp2,a1,a2,dt) 
 call var_advance(u3_tmp1,u3_tmp2,a1,a2,dt) 
  if (debug) call check_point('rk_ss#7',.false.)

 !Density/Temperature/Passive Scalar
 call rho_rhs(u,v,w,at,rcond,pzz,er,iflag,rho,r_tmp1,err1)
 call var_advance(r_tmp1,r_tmp2,a1,a2,dt) 
  if ( debug ) call write_plane(r_tmp1,1,nxp2/2,0,0,'NSr',.false.,err1)
  if (debug) call check_point('rk_ss#8',.false.)

 !NOTE: u1_tmp2,u2_tmp2,u3_tmp2 contain information that will be used at the next substep
 !therefore subdomains must pass boundary data
 call ghost(u1_tmp2,'utemp',err1)
 call ghost(u2_tmp2,'vtemp',err1)
 call ghost(u3_tmp2,'wtemp',err1)
 call ghost(r_tmp2,'rtemp',err1)
  if (debug) call check_point('rk_ss#9',.false.)

 !q_i
 call var_advance(u1_tmp2,u,1.d0,a3,1.d0) 
 call var_advance(u2_tmp2,v,1.d0,a3,1.d0) 
 call var_advance(u3_tmp2,w,1.d0,a3,1.d0) 
 call var_advance(r_tmp2,rho,1.d0,a3,1.d0) 
  if (debug) call check_point('rk_ss#10',.false.)

 !NOTE: subdomains must pass boundary data since the information in the ghost cells is used 
 !in the next substep
 call ghost(u,'u',err1)
 call ghost(v,'v',err1)
 call ghost(w,'w',err1)
 call ghost(rho,'rho',err1)
  if (debug) call check_point('rk_ss#11',.false.)

 deallocate(u1_tmp1,u2_tmp1,u3_tmp1,stat=s1)
 if (s1.NE.0) then
  write(IOUT,*) "Error de-allocating Ftemp in rk_ss"
  goto 1000
 endif
  if (debug) call check_point('rk_ss#12',.false.)

 1000 continue
 stat=max(err1,s1)
 return
end subroutine rk_ss_LARGE_MEMORY
