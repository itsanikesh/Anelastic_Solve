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
 use Parameters, only: bstep, estep, nstep, delt, cfl, time, Ddt, rhoMIN, rhoMAX, clip
 implicit none

 !Passed Variables
 integer,intent(out)              :: stat

 !Local Variables
 integer                          :: ok, i, j, k,err1
 real(r8)                         :: a1, a2, a3, ec, told,told2
 logical                          :: wdivplanes=.false.
 logical,parameter                :: debug=.false.
 logical                          :: model
 real(r8)  :: mean(sz-1:ez+1), rms(sz-1:ez+1), temp(sz-1:ez+1)

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

do nstep=bstep,estep

 !Ensure temporary variables are empty
 call zero_temps  

 !let clf increase monotonic with time from 0.25 to 1 from t=0 to t=30
 if (time.LT.30) then
  cfl = 0.25+0.75*time/30.d0
 else
  cfl = 1.d0
 endif

 !Calculate time step
 call calcdt(delt,ok)
  if (ok.NE.0) goto 1000
 write(IOUT,*) ""
 write(IOUT,'(i5,a9,f15.8,a7,f15.8)') nstep,"    time=",time,"    dt=",delt

 !Calculate gravity (may be ramped up smoothly from zero)
 call gravity(ok)
  if (ok.NE.0) stop

! if (nstep.GT.10) then
!  SSM_const = 0.1d0**2.d0
!  Pr_sgs = 0.70 
!  call SSM
!  call SSMrho       
!  endif

 if (mod(nstep,5).EQ.0) then
   r_dgt_dg = dsqrt(5.0)
   xfil=1
   yfil=1
   zfil=1
!   call DSM
   call DMM
   call DSMrho
 endif
  if (time.LT.15.d0) then
   model=.false.
  else if(time.GT.15.d0.and.time.LT.150.d0) then
   model=.true.
  else
   goto 1000
  endif
  write(IOUT,*)'Model: ',model


 !First sub-step
 a1 = 0.d0
 a2 = 1.d0
 a3 = 1.d0/3.d0
 ec = -1.d0/6.d0
 call rk_ss(a1,a2,a3,ec,delt,.true.,model,ok)
  if (ok.NE.0) goto 1000
  if (debug) call check_point('NSsolver#1',.false.)
  if (clip)  call clipVS(rho,rhoMIN,rhoMAX,stat)
 
 !Second sub-step
 a1 = -5.d0/9.d0
 a2 = 1.d0
 a3 = 15.d0/16.d0
 ec = -10.d0/3.d0
 call rk_ss(a1,a2,a3,ec,delt,.true.,model,ok)
  if (ok.NE.0) goto 1000 
  if (debug) call check_point('NSsolver#2',.false.)
  if (clip)  call clipVS(rho,rhoMIN,rhoMAX,stat)

 !Third sub-step
 a1 = -153.d0/128.d0
 a2 = 1.d0
 a3 = 8.d0/15.d0
 ec = 15.d0/8.d0
 call rk_ss(a1,a2,a3,ec,delt,.true.,model,ok)
  if (ok.NE.0) goto 1000
  if (debug) call check_point('NSsolver#3',.false.)
  if (clip)  call clipVS(rho,rhoMIN,rhoMAX,stat)

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

  told2 = told
  told = time
  time=time+delt
  
! write(6,*)"passed output" 
 !Update dk/dt
 !INTERPOLATE VELOCITIES TO CELL CENTERS
  u1_tmp2 = 0.d0
  u2_tmp2 = 0.d0
  u3_tmp2 = 0.d0
  r_tmp1 = 0.d0
  call center_velocity(u,u1_tmp2,1 )
  call center_velocity(v,u2_tmp2,2 )
  call center_velocity(w,u3_tmp2,3 )

  !u'
   call avgX1X2(u1_tmp2,mean,rms,'uc')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      u1_tmp2(i,j,k)=u1_tmp2(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(u1_tmp2,'cfluc',err1)

  !v'
   call avgX1X2(u2_tmp2,mean,rms,'vc')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      u2_tmp2(i,j,k)=u2_tmp2(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(u2_tmp2,'cfluc',err1)

  !w'  
   call avgX1X2(u3_tmp2,mean,rms,'wc')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      u3_tmp2(i,j,k)=u3_tmp2(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(u3_tmp2,'cfluc',err1)
! write(6,*) "pass rms in computing dk/dt"

 !<u1'u1'>
  r_tmp1 = u1_tmp2*u1_tmp2
  call avgX1X2(r_tmp1,transtmp1,rms,'uc')
 !<u2'u2'> 
  r_tmp1 = u2_tmp2*u2_tmp2
  call avgX1X2(r_tmp1,transtmp2,rms,'vc')
 !<u3'u3'> 
  r_tmp1 = u3_tmp2*u3_tmp2
  call avgX1X2(r_tmp1,transtmp3,rms,'wc')
 
! write(6,*)"Pass uiui"

 !TKE 
 do k=sz,ez
  transtmp(k) = 1.d0/2.d0*(transtmp1(k)+transtmp2(k)+transtmp3(k))
  dkdt(k) = ( transtmp(k)-rk_old(k) ) / (time-told)
  dkdt_c(k) = ( transtmp(k)-rk_old2(k) ) / (time-told2)
  rk_old2(k) = rk_old(k)
  rk_old(k) = transtmp(k)
 enddo
! write(6,*) "Pass tke"

 !R11
  transtmp = transtmp1
  dR11dt = ( transtmp-R11_old ) / (time - told)
  dR11dt_c = ( transtmp-R11_old2 ) / (time - told2)
  R11_old2 = R11_old
  R11_old = transtmp

 !R22
  transtmp = transtmp2
  dR22dt = (transtmp-R22_old) / (time - told)
  dR22dt_c= (transtmp-R22_old2) / (time - told2)
  R22_old2 = R22_old
  R22_old = transtmp

 !R33
  transtmp = transtmp3
  dR33dt = (transtmp-R33_old) / (time - told)
  dR33dt_c = (transtmp-R33_old2) / (time - told2)
  R33_old2 = R33_old
  R33_old = transtmp

 !R12
  r_tmp1=u1_tmp2*u2_tmp2
  call avgX1X2(r_tmp1,transtmp,rms,'cfluc')
  dR12dt = (transtmp-R12_old) / (time - told)
  dR12dt_c = (transtmp-R12_old2) / (time - told2)
  R12_old2 = R12_old
  R12_old = transtmp

 !R13
  r_tmp1=u1_tmp2*u3_tmp2
  call avgX1X2(r_tmp1,transtmp,rms,'cfluc')
  dR13dt = (transtmp-R13_old) / (time - told)
  dR13dt_c = (transtmp-R13_old2) / (time - told2)
  R13_old2 = R13_old
  R13_old = transtmp

 !R23
  r_tmp1=u2_tmp2*u3_tmp2
  call avgX1X2(r_tmp1,transtmp,rms,'cfluc')
  dR23dt = (transtmp-R23_old) / (time - told)
  dR23dt_c = (transtmp-R23_old2) / (time - told2)
  R23_old2 = R23_old
  R23_old = transtmp
 
!  write(6,*) "Pass reynolds stress components"

 !rho'  
   call avgX1X2(rho,mean,rms,'wc')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      r_tmp1(i,j,k)=rho(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(r_tmp1,'cfluc',err1)

 !<rho'rho'>
  r_tmp1 = r_tmp1*r_tmp1
  call avgX1X2(r_tmp1,transtmp4,rms,'uc')
  r_tmp1 = sqrt(r_tmp1)
  transtmp = transtmp4
  dr2dt = ( transtmp-rr_old) / (time-told)
  dr2dt_c = ( transtmp-rr_old2) / (time-told2)
  rr_old2 = rr_old
  rr_old = transtmp

 !RU1
  u1_tmp2=u1_tmp2*r_tmp1
  call avgX1X2(u1_tmp2,transtmp,rms,'cfluc')
  dRU1dt = (transtmp-RU1_old) / (time - told)
  dRU1dt_c = (transtmp-RU1_old2) / (time - told2)
  RU1_old2 = RU1_old
  RU1_old = transtmp

 !RU2
  u2_tmp2=u2_tmp2*r_tmp1
  call avgX1X2(u2_tmp2,transtmp,rms,'cfluc')
  dRU2dt = (transtmp-RU2_old) / (time - told)
  dRU2dt_c = (transtmp-RU2_old2) / (time - told2)
  RU2_old2 = RU2_old
  RU2_old = transtmp

 !RU3
  u3_tmp2=u3_tmp2*r_tmp1
  call avgX1X2(u3_tmp2,transtmp,rms,'cfluc')
  dRU3dt = (transtmp-RU3_old) / (time - told)
  dRU3dt_c = (transtmp-RU3_old2) / (time - told2)
  RU3_old2 = RU3_old
  RU3_old = transtmp
 
!  write(6,*)"passs NS rho component"
  
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
subroutine rk_ss(a1,a2,a3,ec,dt,psolve,model,stat)
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
 use domain,    only: sx,ex,sy,ey,sz,ez,nyp2, nxp2, nzp2
 implicit none

!Passed Variables
 real(r8),intent(in)                   :: a1, a2, a3, ec, dt
 integer(i4),intent(out)               :: stat
 logical,intent(in)                    :: psolve, model

!Local Variables
 integer                               :: err1
 integer(i4)                           :: niters
 real(r8)                              :: resmax
 integer                               :: s1
 real(r8),allocatable,dimension(:,:,:) :: Ftemp
 logical,parameter                     :: debug=.false.

 if (debug) call check_point('rk_ss#0',.false.)
 err1=0
 s1=0

 allocate( Ftemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 if (s1.NE.0) then
  write(IOUT,*) "Error allocating Ftemp in rk_ss"
  goto 1000
 endif

 r_tmp1(:,:,:) = 0.d0
 call straincal(u,v,w,err1)
 !X1-momentum 
 call u1_rhs(u,v,w,Ftemp,model,err1)
 if (psolve) call Psource2(Ftemp,u,r_tmp1,dt,1,err1)
 call var_advance(Ftemp,u1_tmp2,a1,a2,dt)
  if ( debug ) call write_plane(Ftemp,1,nxp2/2,0,1,'NS1',.false.,err1)
  if (debug) call check_point('rk_ss#1',.false.)

 !X2-momentum 
 call u2_rhs(u,v,w,Ftemp,model,err1)
 if (psolve) call Psource2(Ftemp,v,r_tmp1,dt,2,err1)
 call var_advance(Ftemp,u2_tmp2,a1,a2,dt)
  if ( debug ) call write_plane(Ftemp,1,nxp2/2,0,1,'NS2',.false.,err1)
  if (debug) call check_point('rk_ss#2',.false.)

 !X3-momentum 
 call u3_rhs(u,v,w,rho,Ftemp,model,err1)
 if (psolve) call Psource2(Ftemp,w,r_tmp1,dt,3,err1)
 call var_advance(Ftemp,u3_tmp2,a1,a2,dt)
  if ( debug ) call write_plane(Ftemp,1,nxp2/2,0,1,'NS3',.false.,err1)
  if (debug) call check_point('rk_ss#3',.false.)

 deallocate(Ftemp,stat=s1)
 if (s1.NE.0) then
  write(IOUT,*) "Error de-allocating Ftemp in rk_ss"
  goto 1000
 endif
  if (debug) call check_point('rk_ss#4',.false.)

 !Pressure Source Calculated Piecewise
  if (debug) call write_plane(r_tmp1,1,nxp2/2,0,1,'source',.false.,err1)
  if (debug) call write_plane(r_tmp1,2,nyp2/2,0,1,'source',.false.,err1)
  if (debug) call check_point('rk_ss#5',.false.)

 !Pressure Poisson Equation 
 if (psolve) then
 call mg_solver(p,r_tmp1,niters,resmax)
 write(IOUT,'(a20,i3,a11,e22.15)') "    MG Iterations: ",niters,"  RESIDUAL= ",resmax
  if ( debug ) call write_plane(p,1,nxp2/2,0,1,'p1',.false.,err1)
  if ( debug ) call write_pencil(p,3,nxp2/2,nyp2/2,0,'p',.false.,err1)
  if (debug) call check_point('rk_ss#6',.false.)

 !Pressure Gradient 
 call Pgrad(p,u1_tmp2,u2_tmp2,u3_tmp2,dt)
  if ( debug ) call write_plane(u1_tmp2,1,nxp2/2,0,1,'NSa1',.false.,err1)
  if ( debug ) call write_plane(u2_tmp2,1,nxp2/2,0,1,'NSa2',.false.,err1)
  if ( debug ) call write_plane(u3_tmp2,1,nxp2/2,0,1,'NSa3',.false.,err1)
   if (debug) call check_point('rk_ss#7',.false.)
 endif

 !Density/Temperature/Passive Scalar
 call rho_rhs(u,v,w,rho,r_tmp1,model,err1)
 call var_advance(r_tmp1,r_tmp2,a1,a2,dt) 
  if ( debug ) call write_plane(r_tmp1,1,nxp2/2,0,1,'NSr',.false.,err1)
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

 1000 continue
 stat=max(err1,s1)
 return
end subroutine rk_ss

subroutine u1_rhs(un,vn,wn,Utemp,model,stat)
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
 use Domain,     only: sx,ex,sy,ey,sz,ez,EU
 use Grid,       only: rdxe,rdxc,rdye,rdyc,rdze,rdzc
 use Grid,       only: dxe,dxc,dye,dyc,dze,dzc
 use Parameters, only: rRe, Rsponge
 use ratios,     only: r_1_2
 use LESmod
 use IO, only: IOUT
 implicit none

!Passed Variables
 real(r8),intent(in)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: Utemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 logical,intent(in)         :: model
 integer,intent(out)        :: stat

!Local Variables
 integer                   :: i,j,k
 real(r8)                  :: uaver1, uaver2, vaver1, vaver2, waver1, waver2
 integer                   :: dir,face,var,err
 real(r8)                  :: tmp1, tmp2, tmp3, tmp4, del_g


  Utemp=0.d0

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
                 -( un(i,j,k) - un(i,j-1,k) )*rdye(j-1) )*rdyc(j)             &
    !d2u/dz2
                                + rRe*( ( un(i,j,k+1) - un(i,j,k) )*rdze(k)   &
                 -( un(i,j,k) - un(i,j,k-1) )*rdze(k-1) )*rdzc(k)
   enddo
  enddo
 enddo

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
    waver2 = r_1_2*(wn(i+1,j,k) + wn(i,j,k))       
    waver1 = r_1_2*(wn(i+1,j,k-1) + wn(i,j,k-1))   
    Utemp(i,j,k) = Utemp(i,j,k) + ( uaver1*waver1 - uaver2*waver2 )*rdzc(k)

    !Fully 2nd Order [Bewley EQ. 13.6]
    !R1=           !dz(k+1/2)/dz(k) 
    !R2=           !dz(k-1/2)/dz(k)
    !R3=           !dx(i+1/2)/dx(i)
    !R4=           !dx(i-1/2)/dx(i)
    !uaver2 =      !u_k+1/2
    !uaver1 =      !u_k-1/2
    !waver2 =      !w_i+1/2
    !waver1 =      !w_i-1/2
    !Utemp(i,j,k) =

    !d(u*v)/dy
    !Quasi 2nd Order [Bewley EQ. 13.4]
    uaver2 = r_1_2*(un(i,j+1,k) + un(i,j,k))    
    uaver1 = r_1_2*(un(i,j,k) + un(i,j-1,k))
    vaver2 = r_1_2*(vn(i+1,j,k) + vn(i,j,k))
    vaver1 = r_1_2*(vn(i+1,j-1,k) + vn(i,j-1,k))
    Utemp(i,j,k) = Utemp(i,j,k) + ( uaver1*vaver1 - uaver2*vaver2 )*rdyc(j)

    !Fully 2nd Order [Bewley EQ. 13.6]
    !R1=           !dy(j+1/2)/dy(j) 
    !R2=           !dy(j-1/2)/dy(j)
    !R3=           !dx(i+1/2)/dx(i)
    !R4=           !dx(i-1/2)/dx(i)
    !uaver2 =      !u_j+1/2
    !uaver1 =      !u_j-1/2
    !vaver2 =      !v_i+1/2
    !vaver1 =      !v_i-1/2
    !Utemp(i,j,k) =

   enddo
  enddo
 enddo

if (model) then  
!**************************
!*     LES TERMS          * 
!**************************
! Normal stresses/strain are computed at cell center (P-point)
! Shear stresses/strain are computed at the faces.
! Strain mag is at P-point. Need to interp modS from P-point to U-point
!  to get shear stress. This interp is for uniform grid only. Need to fix for stretched grid.
! Csgs(i,j,k) is avail at P-point.
! del_g(i,j,k) is at P-points. Locally, no interp for del_g
! del_g(i,j,k) = sqrt( dxc(i)**2 + dyc(j)**2 + dzc(k)**2 ) for free shear flows
!  or del_g(i,j,k) = ( dxc(i) * dyc(j) * dzc(k) )**(1.d0/3.d0) for wall flows
! del_gt = sqrt(6.d0)*del_g 

!dtau11/dx 
!*******************************
! P-point          U-point        P-point  
! tau11(i,j,k)     un(i,j,k)      tau11(i+1,j,k)
! S11(i,j,k)       <--dxe(i)-->   S11(i+1,j,k)
! modS(i,j,k)      <--dxe(i)-->   modS(i+1,j,k)
! Csgs(i,j,k)      <--dxe(i)-->   Csgs(i+1,j,k) no interp      
! del_g is at P point
!*******************************
 do k = sz,ez
  do j = sy,ey
   do i = sx,ex-EU
!   tmp1 = tau11 at (i,j,k) 
    del_g =  ( dxc(i) * dyc(j)* dzc(k) )**(1.d0/3.d0)
    tmp1 = -2.d0 * Csgs(i,j,k) * del_g**2.d0 * modS(i,j,k) * S11(i,j,k)
!   tmp2 = tau11 at (i+1,j,k) 
    del_g =  ( dxc(i+1) * dyc(j)* dzc(k) )**(1.d0/3.d0)
    tmp2 = -2.d0 * Csgs(i+1,j,k) * del_g**2.d0 * modS(i+1,j,k) * S11(i+1,j,k)
    Utemp(i,j,k) = Utemp(i,j,k) - (tmp2 - tmp1) * rdxe(i)
   enddo
  enddo
 enddo

!dtau12/dy
!*******************************
! V-plane              U-point       V-plane  
! tau12(i,j-1,k)       un(i,j,k)     tau12(i,j,k)
! S12(i,j-1,k)        <--dyc(j)-->   S12(i,j,k)
! modS(i,j,k) and Csgs(i,j,k) has to be interp to tau12 loc. Need correction for stretched grid
!*******************************
 do k = sz,ez
  do j = sy,ey
   do i = sx,ex-EU
!   tmp4 =  Csgs at location tau12(i,j-1,k)
    tmp4 = .25d0*( Csgs(i,j,k) + Csgs(i+1,j,k) + Csgs(i,j-1,k) + Csgs(i+1,j-1,k) )
!   tmp3 =  modS at location tau12(i,j-1,k)
    tmp3 = .25d0*( modS(i,j,k) + modS(i+1,j,k) + modS(i,j-1,k) + modS(i+1,j-1,k) )
!   tmp1 = tau12 at (i,j-1,k) 
    del_g =  ( dxe(i) * dye(j-1)* dzc(k) )**(1.d0/3.d0)
    tmp1 = -2.d0 * tmp4 * del_g**2.d0 * tmp3 * S12(i,j-1,k)
!   tmp4 =  Csgs at location tau12(i,j,k)
    tmp4 = .25d0*( Csgs(i,j,k) + Csgs(i+1,j,k) + Csgs(i,j+1,k) + Csgs(i+1,j+1,k) )
!   tmp3 = strain mag at location tau12(i,j,k)
    tmp3 = .25d0*( modS(i,j,k) + modS(i+1,j,k) + modS(i,j+1,k) + modS(i+1,j+1,k) )
!   tmp2 = tau12 at (i,j,k)
    del_g =  ( dxe(i) * dye(j)* dzc(k) )**(1.d0/3.d0)
    tmp2 = -2.d0 * tmp4 * del_g**2.d0 * tmp3 * S12(i,j,k)
    Utemp(i,j,k) = Utemp(i,j,k) - (tmp2 - tmp1) * rdyc(j)
   enddo
  enddo
 enddo

!dtau13/dz
!*******************************
! W-plane              U-point       W-plane  
! tau13(i,j,k-1)       un(i,j,k)     tau13(i,j,k)
! S13(i,j,k-1,5)      <--dzc(k)-->   S13(i,j,k,5)
! modS(i,j,k) and Csgs(i,j,k)has to be interpolated to tau13 location.
! possible NON-uniform grid in z-direction --> Interpolation error      
!*******************************
 do k = sz,ez
  do j = sy,ey
   do i = sx,ex-EU
!   tmp4 = Csgs at location tau13(i,j,k-1)
    tmp4 = .25d0*( Csgs(i,j,k) + Csgs(i+1,j,k) + Csgs(i,j,k-1) + Csgs(i+1,j,k-1) )
!   tmp3 =  strain mag at location tau13(i,j,k-1)
    tmp3 = .25d0*( modS(i,j,k) + modS(i+1,j,k) + modS(i,j,k-1) + modS(i+1,j,k-1) )
!   tmp1 = tau13 at (i,j,k-1)
    del_g =  ( dxe(i) * dyc(j)* dze(k-1) )**(1.d0/3.d0)
    tmp1 = -2.d0 * tmp4 * del_g**2.d0 * tmp3 * S13(i,j,k-1)
!   tmp4 = Csgs at location tau13(i,j,k)
    tmp4 = .25d0*( Csgs(i,j,k) + Csgs(i+1,j,k) + Csgs(i,j,k+1) + Csgs(i+1,j,k+1) )
!   tmp3 = strain mag at location tau12(i,j,k)
    tmp3 = .25d0*( modS(i,j,k) + modS(i+1,j,k) + modS(i,j,k+1) + modS(i+1,j,k+1) )
!   tmp2 = tau13 at (i,j,k)
    del_g =  ( dxe(i) * dyc(j)* dze(k) )**(1.d0/3.d0)
    tmp2 = -2.d0 * tmp4  * del_g**2.d0 * tmp3 * S13(i,j,k)
    Utemp(i,j,k) = Utemp(i,j,k) - (tmp2 - tmp1) * rdzc(k)
   enddo
  enddo
 enddo

!**************************************
!Scale-similarity part: -tauSS_1j=-d( (ug_1*ug_j)_g - ugg_1*ugg_j)/dx_j
!**************************************
 lestmp=0.d0
 lestmp1=0.d0
 lestmp2=0.d0
 lestmp3=0.d0
 lestmp4=0.d0
!tauSS_11 at tau11 point, stored in lestmp1
 call center_velocity(un,lestmp,1)
 call filter(lestmp,lestmp1,1,xfil,yfil,zfil,err)
 call ghost(lestmp1,'u',err)
 lestmp=lestmp*lestmp
 call filter(lestmp,lestmp4,1,xfil,yfil,zfil,err)
 lestmp1=lestmp4-lestmp1*lestmp1

 
!tauSS_12 at tau12 point, stored in lestmp2
 lestmp=0.d0
 lestmp4=0.d0
 call filter(un,lestmp,1,xfil,yfil,zfil,err)
 call ghost(lestmp,'u',err)
 call filter(vn,lestmp4,1,zfil,yfil,zfil,err)
 call ghost(lestmp4,'v',err)
 do k =sz,ez
  do j=sy,ey
   do i=sx,ex-EU
    lestmp2(i,j,k) = r_1_2*(lestmp(i,j,k)+lestmp(i,j+1,k)) &
                   * r_1_2*(lestmp4(i,j,k)+lestmp4(i+1,j,k))
   enddo
  enddo
 enddo
 call ghost(lestmp2,'w',err)
 do k =sz,ez
  do j=sy,ey
   do i=sx,ex-EU
    lestmp(i,j,k) = r_1_2*(un(i,j,k)+un(i,j+1,k)) &
                   * r_1_2*(vn(i,j,k)+vn(i+1,j,k))
   enddo
  enddo
 enddo
 call ghost(lestmp,'w',err)
 call filter(lestmp,lestmp4,1,xfil,yfil,zfil,err)
 lestmp2=lestmp4-lestmp2

!tauSS_13 at tau13 point, stored in lestmp3
 lestmp=0.d0
 lestmp4=0.d0
 call filter(un,lestmp,1,xfil,yfil,zfil,err)
 call ghost(lestmp,'u',err)
 call filter(wn,lestmp4,1,xfil,yfil,zfil,err)
 call ghost(lestmp4,'w',err)

 do k =sz,ez
  do j=sy,ey
   do i=sx,ex-EU
    lestmp3(i,j,k) = r_1_2*(lestmp(i,j,k)+lestmp(i,j,k+1)) &
                   * r_1_2*(lestmp4(i,j,k)+lestmp4(i+1,j,k))
   enddo
  enddo
 enddo
 call ghost(lestmp3,'w',err)
 do k =sz,ez
  do j=sy,ey
   do i=sx,ex-EU
    lestmp(i,j,k) = r_1_2*(un(i,j,k)+un(i,j,k+1)) &
                   * r_1_2*(wn(i,j,k)+wn(i+1,j,k))
   enddo
  enddo
 enddo
 call ghost(lestmp,'w',err)
 call filter(lestmp,lestmp4,1,xfil,yfil,zfil,err)
 lestmp3=lestmp4-lestmp3

 call ghost(lestmp1,'w',err)
 call ghost(lestmp2,'w',err)
 call ghost(lestmp3,'w',err)

!-d(tauSS_11)/dx
 do k = sz,ez
  do j = sy,ey  
   do i = sx,ex-EU
    tmp2 = lestmp1(i+1,j,k)
    tmp1 = lestmp1(i,j,k)
    Utemp(i,j,k) = Utemp(i,j,k) - (tmp2 - tmp1) * rdxe(i) 
   enddo
  enddo
 enddo

!-dtauSS_12)/dy
 do k = sz,ez
  do j = sy,ey  
   do i = sx,ex-EU
    tmp2 = lestmp2(i,j,k)
    tmp1 = lestmp2(i,j-1,k)
    Utemp(i,j,k) = Utemp(i,j,k) - (tmp2 - tmp1) * rdyc(j) 
   enddo
  enddo
 enddo

!-d(tauSS_13)/dz
 do k = sz,ez
  do j = sy,ey  
   do i = sx,ex-EU
    tmp2 = lestmp3(i,j,k)
    tmp1 = lestmp3(i,j,k-1)
    Utemp(i,j,k) = Utemp(i,j,k) - (tmp2 - tmp1) * rdzc(k) 
   enddo
  enddo
 enddo
 write(IOUT,*)"urhs-model:", model
endif !end LES

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

 call ghost(Utemp,'utemp',err)

 stat=err
 return
end subroutine u1_rhs


subroutine u2_rhs(un,vn,wn,Vtemp,model,stat)
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
 use Domain,     only: sx,ex,sy,ey,sz,ez,EV
 use Grid,       only: rdxe,rdxc,rdye,rdyc,rdze,rdzc
 use Grid,       only: dxe,dxc,dye,dyc,dze,dzc
 use Parameters, only: rRe, Rsponge
 use LESmod
 use IO,         only: IOUT
 use ratios,     only: r_1_2
 implicit none

!Passed Variables
 real(r8),intent(in)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: Vtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 logical,intent(in)         :: model
 integer,intent(out)        :: stat
!Local Variables
 integer                    :: i,j,k
 real(r8)                   :: uaver1, uaver2, vaver1, vaver2, waver1, waver2
 integer                    :: dir,face,var,err
 real(r8)                  :: tmp1, tmp2, tmp3, tmp4, del_g

 Vtemp = 0.d0

!*********************************
!*  Linear Terms                 *
!********************************* 
 do k=sz,ez 
  do j=sy,ey-EV
   do i=sx,ex
   !d2v/dx2
    Vtemp(i,j,k) = Vtemp(i,j,k) + rRe*( ( vn(i+1,j,k) - vn(i,j,k) )*rdxe(i) &
                 -( vn(i,j,k) - vn(i-1,j,k) )*rdxe(i-1) )*rdxc(i) &
   !d2v/dy2
                 + rRe*( ( vn(i,j+1,k) - vn(i,j,k) )*rdyc(j+1) &
                 - ( vn(i,j,k) - vn(i,j-1,k) )*rdyc(j) )*rdye(j) &

   !d2v/dz2
                 + rRe*( ( vn(i,j,k+1) - vn(i,j,k) )*rdze(k) &
                 - ( vn(i,j,k) - vn(i,j,k-1) )*rdze(k-1) )*rdzc(k)
   enddo
  enddo
 enddo

!*********************************
!*  Non-linear Terms             *
!********************************* 
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
    waver1 = 0.5d0*(wn(i,j+1,k) + wn(i,j,k))
    waver2 = 0.5d0*(wn(i,j+1,k-1) + wn(i,j,k-1))
    Vtemp(i,j,k) = Vtemp(i,j,k) - ( waver1*vaver1 - waver2*vaver2 )*rdzc(k)
   enddo
  enddo
 enddo

!LES
if (model) then
!**************************
!*      LES TERMS         *
!**************************
! Normal stresses/strain are computed at cell center (P-point)
! Shear stresses/strain are computed at the faces.
! Strain mag is at P-point. Need to interp modS from P-point to U-point
!  to get shear stress. This interp is for uniform grid only. Need to fix for stretched grid.
! Csgs(i,j,k) is avail at P-point.
! del_g(i,j,k) = sqrt( dxc(i)**2 + dyc(j)**2 + dzc(k)**2 ) for free shear flows
!  or del_g(i,j,k) = ( dxc(i)*dyc(j)*dzc(k) )**(1.d0/3.d0) for wall flows

!dtau22/dy
!*******************************
! P-point          V-point        P-point  
! tau22(i,j,k)     vn(i,j,k)    tau22(i,j+1,k)
! S22(i,j,k)     <--dye(j)-->   S22(i,j+1,k)
! modS(i,j,k)    <--dye(j)-->   modS(i,j+1,k)
! Csgs(i,j,k)    <--dye(j)-->   Csgs(i,j+1,k) no interp      
!*******************************
 do k = sz,ez
  do j = sy,ey-EV
   do i = sx,ex
!   tmp1 = tau22 at (i,j,k) 
    del_g =  ( dxc(i) * dyc(j)* dzc(k) )**(1.d0/3.d0)
    tmp1 = -2.d0 * Csgs(i,j,k) * del_g**2.d0 * modS(i,j,k) * S22(i,j,k)
!   tmp2 = tau22 at (i,j+1,k) 
    del_g =  ( dxc(i) * dyc(j+1)* dzc(k) )**(1.d0/3.d0)
    tmp2 = -2.d0 * Csgs(i,j+1,k) *  del_g**2.d0 * modS(i,j+1,k) * S22(i,j+1,k)
    Vtemp(i,j,k) = Vtemp(i,j,k) - (tmp2 - tmp1) * rdye(j)
   enddo
  enddo
 enddo

!dtau21/dx
!*******************************
! U-plane              V-point       U-plane  
! tau12(i-1,j,k)       vn(i,j,k)     tau12(i,j,k)
! S12(i-1,j,k)      <--dxc(i)-->   S12(i,j,k)
! modS(i,j,k) and Csgs(i,j,k) have to be interp to tau12 loc. Need correction for stretched grid
!*******************************
 do k = sz,ez
  do j = sy,ey-EV
   do i = sx,ex
!   tmp4 =  Csgs at location tau12(i-1,j,k)
    tmp4 = .25d0*( Csgs(i,j,k) + Csgs(i,j+1,k) + Csgs(i-1,j,k) + Csgs(i-1,j+1,k) )
!   tmp3 =  modS at location tau12(i-1,j,k)
    tmp3 = .25d0*( modS(i,j,k) + modS(i,j+1,k) + modS(i-1,j,k) + modS(i-1,j+1,k) )
!   tmp1 = tau12 at (i-1,j,k) 
    del_g =  ( dxe(i-1) * dye(j)* dzc(k) )**(1.d0/3.d0)
    tmp1 = -2.d0 * tmp4 * del_g**2.d0 * tmp3 * S12(i-1,j,k)
!   tmp4 =  Csgs at location tau12(i,j,k)
    tmp4 = .25d0*( Csgs(i,j,k) + Csgs(i+1,j,k) + Csgs(i,j+1,k) + Csgs(i+1,j+1,k) )
!   tmp3 = strain mag at location tau12(i,j,k)
    tmp3 = .25d0*( modS(i,j,k) + modS(i+1,j,k) + modS(i,j+1,k) + modS(i+1,j+1,k) )
!   tmp2 = tau12 at (i,j,k)
    del_g =  ( dxe(i) * dye(j)* dzc(k) )**(1.d0/3.d0)
    tmp2 = -2.d0 * tmp4 * del_g**2.d0 * tmp3 * S12(i,j,k)
    Vtemp(i,j,k) = Vtemp(i,j,k) - (tmp2 - tmp1) * rdxc(i)
   enddo
  enddo
 enddo

!dtau23/dz
!*******************************
! W-plane              V-point       W-plane  
! tau23(i,j,k-1)       vn(i,j,k)     tau23(i,j,k)
! S23(i,j,k-1,6)      <--dzc(k)-->   S23(i,j,k,6)
! modS(i,j,k) and Csgs(i,j,k) have to be interp to tau23 location.
! possible NON-uniform grid in z-direction --> Interp error      
!*******************************
 do k = sz,ez
  do j = sy,ey-EV
   do i = sx,ex
!   tmp4 = Csgs at location tau23(i,j,k-1)
    tmp4 = .25d0*( Csgs(i,j,k) + Csgs(i,j+1,k) + Csgs(i,j,k-1) + Csgs(i,j+1,k-1) )
!   tmp3 =  strain mag at location tau13(i,j,k-1)
    tmp3 = .25d0*( modS(i,j,k) + modS(i,j+1,k) + modS(i,j,k-1) + modS(i,j+1,k-1) )
!   tmp1 = tau23 at (i,j,k-1)
    del_g =  ( dxc(i) * dye(j)* dze(k-1) )**(1.d0/3.d0)
    tmp1 = -2.d0 * tmp4 * del_g**2.d0 * tmp3 * S23(i,j,k-1)
!   tmp4 = Csgs at location tau23(i,j,k)
    tmp4 = .25d0*( Csgs(i,j,k) + Csgs(i,j+1,k) + Csgs(i,j,k+1) + Csgs(i+1,j,k+1) )
!   tmp3 = strain mag at location tau12(i,j,k) 
    tmp3 = .25d0*( modS(i,j,k) + modS(i+1,j,k) + modS(i,j,k+1) + modS(i+1,j,k+1) )
!   tmp2 = tau23 at (i,j,k)
    del_g =  ( dxc(i) * dye(j)* dze(k) )**(1.d0/3.d0)
    tmp2 = -2.d0 * tmp4  * del_g**2.d0 * tmp3 * S23(i,j,k)
    Vtemp(i,j,k) = Vtemp(i,j,k) - (tmp2 - tmp1) * rdzc(k)
   enddo
  enddo
 enddo

!**************************************
!Scale-similarity part: -tauSS_1j=-d( (ug_1*ug_j)_g - ugg_1*ugg_j)/dx_j
!**************************************
 lestmp=0.d0
 lestmp1=0.d0
 lestmp2=0.d0
 lestmp3=0.d0
 lestmp4=0.d0
!tauSS_22 at tau22 point, stored in lestmp1
 call center_velocity(vn,lestmp,2)
 call filter(lestmp,lestmp1,1,xfil,yfil,zfil,err)
 call ghost(lestmp1,'v',err)
 lestmp=lestmp*lestmp
 call filter(lestmp,lestmp4,1,xfil,yfil,zfil,err)
 lestmp1=lestmp4-lestmp1*lestmp1

!tauSS_21 at tau21 point, stored in lestmp2
 call filter(vn,lestmp,1,xfil,yfil,zfil,err)
 call ghost(lestmp,'v',err)
 call filter(un,lestmp4,1,xfil,yfil,zfil,err)
 call ghost(lestmp4,'u',err)

 do k =sz,ez
  do j=sy,ey-EV
   do i=sx,ex
    lestmp2(i,j,k) = r_1_2*(lestmp(i,j,k)+lestmp(i+1,j,k)) &
                   * r_1_2*(lestmp4(i,j,k)+lestmp4(i,j+1,k))
   enddo
  enddo
 enddo
 call ghost(lestmp2,'w',err)
 do k =sz,ez
  do j=sy,ey-EV
   do i=sx,ex
    lestmp(i,j,k) = r_1_2*(vn(i,j,k)+vn(i+1,j,k)) &
                   * r_1_2*(un(i,j,k)+un(i,j+1,k))
   enddo
  enddo
 enddo
 call ghost(lestmp,'w',err)
 call filter(lestmp,lestmp4,1,xfil,yfil,zfil,err)
 lestmp2=lestmp4-lestmp2

!tauSS_23 at tau23 point, stored in lestmp3
 call filter(vn,lestmp,1,xfil,yfil,zfil,err)
 call ghost(lestmp,'v',err)
 call filter(wn,lestmp4,1,xfil,yfil,zfil,err)
 call ghost(lestmp4,'w',err)
 do k =sz,ez
  do j=sy,ey-EV
   do i=sx,ex
    lestmp3(i,j,k) = r_1_2*(lestmp(i,j,k)+lestmp(i,j,k+1)) &
                   * r_1_2*(lestmp4(i,j,k)+lestmp4(i,j+1,k))
   enddo
  enddo
 enddo
 call ghost(lestmp3,'w',err)
 do k =sz,ez
  do j=sy,ey-EV
   do i=sx,ex
    lestmp(i,j,k) = r_1_2*(vn(i,j,k)+vn(i,j,k+1)) &
                   * r_1_2*(wn(i,j,k)+wn(i,j+1,k))
   enddo
  enddo
 enddo
 call ghost(lestmp,'w',err)
 call filter(lestmp,lestmp4,1,xfil,yfil,zfil,err)
 lestmp3=lestmp4-lestmp3

 call ghost(lestmp1,'w',err)
 call ghost(lestmp2,'w',err)
 call ghost(lestmp3,'w',err)
!-d(tauSS_22)/dy
 do k = sz,ez
  do j = sy,ey-EV  
   do i = sx,ex
    tmp2 = lestmp1(i,j+1,k)
    tmp1 = lestmp1(i,j,k)
    Vtemp(i,j,k) = Vtemp(i,j,k) - (tmp2 - tmp1) * rdye(j) 
   enddo
  enddo
 enddo

!-dtauSS_21)/dx
 do k = sz,ez
  do j = sy,ey-EV  
   do i = sx,ex
    tmp2 = lestmp2(i,j,k)
    tmp1 = lestmp2(i-1,j,k)
    Vtemp(i,j,k) = Vtemp(i,j,k) - (tmp2 - tmp1) * rdxc(i) 
   enddo
  enddo
 enddo

!-d(tauSS_23)/dz
 do k = sz,ez
  do j = sy,ey-EV  
   do i = sx,ex
    tmp2 = lestmp3(i,j,k)
    tmp1 = lestmp3(i,j,k-1)
    Vtemp(i,j,k) = Vtemp(i,j,k) - (tmp2 - tmp1) * rdzc(k) 
   enddo
  enddo
 enddo
 write(IOUT,*)"vrhs-model:",model
endif !for LES

if (Rsponge) then
!*********************************
!*  Sponge Terms                 *
!********************************* 
 var=2 
 do dir=1,3
  do face=1,2
    call Sponge(Vtemp,vn,var,dir,face,err) 
  enddo
 enddo
endif
!#if PARALLEL
 call ghost(Vtemp,'vtemp',err)
!#endif

 stat=err
return
end subroutine u2_rhs 

subroutine u3_rhs(un,vn,wn,rhon,Wtemp,model,stat)
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

 use Parameters, only: rRe, g, flow_type, Rsponge
 use ntypes,     only: r8 
 use Domain,     only: sx,ex,sy,ey,sz,ez,EW
 use Grid,       only: rdxe,rdxc,rdye,rdyc,rdze,rdzc
 use Grid,       only: dxe,dxc,dye,dyc,dze,dzc
 use ratios,     only: r_1_2
 use LESmod
 use IO,         only: IOUT
 implicit none

!Passed Variables
 real(r8),intent(in)       :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)       :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)       :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)    :: Wtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 logical,intent(in)        :: model
 integer,intent(out)       :: stat
 real(r8),intent(in)       :: rhon(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) !inout or in? in means you can not modify it

!Local Variables
 integer                   :: i,j,k
 real(r8)                  :: uaver1, uaver2, vaver1, vaver2, waver1, waver2
 integer                   :: dir,face,var,err
 integer,parameter         :: DEBUG=-1
 real(r8)                  :: rho_Z(sz-1:ez+1)
 real(r8)                  :: tmp1, tmp2, tmp3, tmp4, del_g

 Wtemp = 0.d0

!*********************************
!*  Linear Terms                 *
!********************************* 
 do k=sz,ez-EW
  do j=sy,ey
   do i=sx,ex
   !d2w/dx2
    Wtemp(i,j,k) = Wtemp(i,j,k) + rRe*( ( wn(i+1,j,k) - wn(i,j,k) )*rdxe(i) &
                 - ( wn(i,j,k) - wn(i-1,j,k) )*rdxe(i-1) )*rdxc(i)&
   !d2w/dy2
                 + rRe*( ( wn(i,j+1,k) - wn(i,j,k) )*rdye(j) &
                 - ( wn(i,j,k) - wn(i,j-1,k) )*rdye(j-1) )*rdyc(j)&
   !d2w/dz2
                 + rRe*( ( wn(i,j,k+1) - wn(i,j,k) )*rdzc(k+1)  &
                 -( wn(i,j,k) - wn(i,j,k-1) )*rdzc(k) )*rdze(k) 
   enddo
  enddo
 enddo

!*********************************
!*  Non-linear Terms             *
!*********************************
 do k=sz,ez-EW
  do j=sy,ey
   do i=sx,ex
   !w*dw/dz
    waver1 = 0.5d0*( wn(i,j,k+1) + wn(i,j,k) )
    waver2 = 0.5d0*( wn(i,j,k) + wn(i,j,k-1) )
    Wtemp(i,j,k) = Wtemp(i,j,k) - ( waver1*waver1 - waver2*waver2 )*rdze(k)
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

 !*********************************
 !*  Boussinesq Term               *
 !********************************* 

 if (g.GT.0.d0.OR.g.LT.0.d0) then

  call rho_mean(rhon,rho_Z,err)
 
   do k=sz,ez-EW
    do j=sy,ey
     do i=sx,ex
      Wtemp(i,j,k) = Wtemp(i,j,k)-r_1_2*( (rhon(i,j,k+1) - rho_Z(k+1)) + (rhon(i,j,k)-rho_Z(k)) )*g
     enddo
    enddo
   enddo

 endif

!LES
if  (model) then
!**************************
!*      LES TERMS         *  
!**************************
! Normal stresses/strain are computed at cell center (P-point)
! Shear stresses/strain are computed at the faces.
! Strain mag is at P-point. Need to interp modS from P-point to U-point
!  to get shear stress. This interp is for uniform grid only. Need to fix for stretched grid.
! Csgs(i,j,k) is avail at P-point.
! del_g(i,j,k) = sqrt( (dxc(i)+dxc(i+1))**2 + (dyc(j)+dyc(j+1))**2 + (dzc(k)+dze(k+1))**2 ) for free shear flows
!  or del_g(i,j,k) = ( (dxc(i)+dxc(i+1)) * (dyc(j)+dyc(j+1)) * (dzc(k)+dzc(k+1)) )**(1.d0/3.d0) for wall flows
    
!dtau33/dz
!*******************************
! P-point          W-point        P-point  
! tau33(i,j,k)     wn(i,j,k)      tau33(i,j,k+1)
! S33(i,j,k,3)     <--dze(k)-->   S33(i,j,k+1,3)
! modS(i,j,k)    <--dze(k)-->   modS(i,j,k+1)
! Csgs(i,j,k)      <--dze(k)-->   Csgs(i,j,k+1) no interp      
!*******************************
 do k = sz,ez-EW
  do j = sy,ey
   do i = sx,ex
!   tmp1 = tau33 at (i,j,k) 
    del_g =  ( dxc(i) * dyc(j)* dzc(k) )**(1.d0/3.d0)
    tmp1 = -2.d0 * Csgs(i,j,k) * del_g**2.d0 * modS(i,j,k) * S33(i,j,k)
!   tmp2 = tau33 at (i,j,k+1) 
    del_g =  ( dxc(i) * dyc(j)* dzc(k+1) )**(1.d0/3.d0)
    tmp2 = -2.d0 * Csgs(i,j,k+1) * del_g**2.d0 * modS(i,j,k+1) * S33(i,j,k+1)
    Wtemp(i,j,k) = Wtemp(i,j,k) - (tmp2 - tmp1) * rdze(k)
   enddo
  enddo
 enddo

!dtau31/dx
!*******************************
! U-plane              W-point       U-plane  
! tau13(i-1,j,k)       wn(i,j,k)     tau13(i,j,k)
! S13(i-1,j,k)      <--dzc(k)-->     S13(i,j,k)
! modS(i,j,k) and Csgs(i,j,k) has to be interp to tau13 loc. Need correction for stretched grid
!*******************************
 do k = sz,ez-EW
  do j = sy,ey
   do i = sx,ex
!   tmp4 =  Csgs at location tau13(i-1,j,k)
    tmp4 = .25d0*( Csgs(i,j,k) + Csgs(i-1,j,k) + Csgs(i,j,k+1) + Csgs(i-1,j,k+1) )
!   tmp3 =  modS at location tau13(i-1,j,k)
    tmp3 = .25d0*( modS(i,j,k) + modS(i-1,j,k) + modS(i,j,k+1) + modS(i-1,j,k+1) )
!   tmp1 = tau13 at (i-1,j,k) 
    del_g =  ( dxe(i-1) * dyc(j)* dze(k) )**(1.d0/3.d0)
    tmp1 = -2.d0 * tmp4 * del_g**2.d0 * tmp3 * S13(i-1,j,k)
!   tmp4 =  Csgs at location tau13(i,j,k)
    tmp4 = .25d0*( Csgs(i,j,k) + Csgs(i+1,j,k) + Csgs(i,j,k+1) + Csgs(i+1,j,k+1) )
!   tmp3 = strain mag at location tau13(i,j,k)
    tmp3 = .25d0*( modS(i,j,k) + modS(i+1,j,k) + modS(i,j,k+1) + modS(i+1,j,k+1) )
!   tmp2 = tau13 at (i,j,k)
    del_g =  ( dxe(i) * dyc(j)* dze(k) )**(1.d0/3.d0)
    tmp2 = -2.d0 * tmp4 * del_g**2.d0 * tmp3 * S13(i,j,k)
    Wtemp(i,j,k) = Wtemp(i,j,k) - (tmp2 - tmp1) * rdxc(i)
   enddo
  enddo
 enddo

!dtau32/dy
!*******************************
! V-plane              W-point       V-plane  
! tau23(i,j-1,k)       wn(i,j,k)     tau23(i,j,k)
! S23(i,j-1,k)        <--dyc(j)-->     S23(i,j,k)
! modS(i,j,k) and Csgs(i,j,k)has to be interpolated to tau23 location.
! possible NON-uniform grid in z-direction --> Interpolation error      
!*******************************
 do k = sz,ez-EW
  do j = sy,ey
   do i = sx,ex
!   tmp4 = Csgs at location tau23(i,j-1,k)
    tmp4 = .25d0*( Csgs(i,j,k) + Csgs(i,j-1,k) + Csgs(i,j,k+1) + Csgs(i,j-1,k+1) )
!   tmp3 =  strain mag at location tau23(i,j-1,k)
    tmp3 = .25d0*( modS(i,j,k) + modS(i,j-1,k) + modS(i,j,k+1) + modS(i,j-1,k+1) )
!   tmp1 = tau23 at (i,j-1,k)
    del_g =  ( dxc(i) * dye(j-1)* dze(k) )**(1.d0/3.d0)
    tmp1 = -2.d0 * tmp4 * del_g**2.d0 * tmp3 * S23(i,j-1,k)
!   tmp4 = Csgs at location tau23(i,j,k)
    tmp4 = .25d0*( Csgs(i,j,k) + Csgs(i,j+1,k) + Csgs(i,j,k+1) + Csgs(i,j+1,k+1) )
!   tmp3 = strain mag at location tau23(i,j,k)
    tmp3 = .25d0*( modS(i,j,k) + modS(i,j+1,k) + modS(i,j,k+1) + modS(i,j+1,k+1) )
!   tmp2 = tau23 at (i,j,k)
    del_g =  ( dxc(i) * dye(j)* dze(k) )**(1.d0/3.d0)
    tmp2 = -2.d0 * tmp4  * del_g**2.d0 * tmp3 * S23(i,j,k)
    Wtemp(i,j,k) = Wtemp(i,j,k) - (tmp2 - tmp1) * rdyc(j)
   enddo
  enddo
 enddo

!**************************************
!Scale-similarity part: -tauSS_1j=-d( (ug_1*ug_j)_g - ugg_1*ugg_j)/dx_j
!**************************************
 lestmp=0.d0
 lestmp1=0.d0
 lestmp2=0.d0
 lestmp3=0.d0
 lestmp4=0.d0
!tauSS_33 at tau33 point, stored in lestmp1
 call center_velocity(wn,lestmp,3)
 call filter(lestmp,lestmp1,1,xfil,yfil,zfil,err)
 call ghost(lestmp1,'w',err)
 lestmp=lestmp*lestmp
 call filter(lestmp,lestmp4,1,xfil,yfil,zfil,err)
 lestmp1=lestmp4-lestmp1*lestmp1

!tauSS_31 at tau31 point, stored in lestmp2
 call filter(wn,lestmp,1,xfil,yfil,zfil,err)
 call ghost(lestmp,'w',err)
 call filter(un,lestmp4,1,xfil,yfil,zfil,err)
 call ghost(lestmp4,'u',err)

 do k =sz,ez-EW
  do j=sy,ey
   do i=sx,ex
    lestmp2(i,j,k) = r_1_2*(lestmp(i,j,k)+lestmp(i+1,j,k)) &
                   * r_1_2*(lestmp4(i,j,k)+lestmp4(i,j,k+1))
   enddo
  enddo
 enddo
 call ghost(lestmp2,'w',err)
 do k =sz,ez-EW
  do j=sy,ey
   do i=sx,ex
    lestmp(i,j,k) = r_1_2*(wn(i,j,k)+wn(i+1,j,k)) &
                   * r_1_2*(un(i,j,k)+un(i,j,k+1))
   enddo
  enddo
 enddo
 call ghost(lestmp,'w',err)
 call filter(lestmp,lestmp4,1,xfil,yfil,zfil,err)
 lestmp2=lestmp4-lestmp2

!tauSS_32 at tau32 point, stored in lestmp3
 call filter(wn,lestmp,1,xfil,yfil,zfil,err)
 call ghost(lestmp,'w',err)
 call filter(vn,lestmp4,1,xfil,yfil,zfil,err)
 call ghost(lestmp4,'v',err)

 do k =sz,ez-EW
  do j=sy,ey
   do i=sx,ex
    lestmp3(i,j,k) = r_1_2*(lestmp(i,j,k)+lestmp(i,j+1,k)) &
                   * r_1_2*(lestmp4(i,j,k)+lestmp4(i,j,k+1))
   enddo
  enddo
 enddo
 call ghost(lestmp3,'w',err)
 do k =sz,ez-EW
  do j=sy,ey
   do i=sx,ex
    lestmp(i,j,k) = r_1_2*(wn(i,j,k)+wn(i,j+1,k)) &
                  * r_1_2*(vn(i,j,k)+vn(i,j,k+1))
   enddo
  enddo
 enddo
 call ghost(lestmp,'w',err)
 call filter(lestmp,lestmp4,1,xfil,yfil,zfil,err)
 lestmp3=lestmp4-lestmp3


 call ghost(lestmp1,'w',err)
 call ghost(lestmp2,'w',err)
 call ghost(lestmp3,'w',err)
!-d(tauSS_33)/dz
 do k = sz,ez-EW
  do j = sy,ey  
   do i = sx,ex
    tmp2 = lestmp1(i,j,k+1)
    tmp1 = lestmp1(i,j,k)
    Wtemp(i,j,k) = Wtemp(i,j,k) - (tmp2 - tmp1) * rdze(k) 
   enddo
  enddo
 enddo

!-dtauSS_31)/dx
 do k = sz,ez-EW
  do j = sy,ey  
   do i = sx,ex
    tmp2 = lestmp2(i,j,k)
    tmp1 = lestmp2(i-1,j,k)
    Wtemp(i,j,k) = Wtemp(i,j,k) - (tmp2 - tmp1) * rdxc(i) 
   enddo
  enddo
 enddo

!-d(tauSS_32)/dy
 do k = sz,ez-EW
  do j = sy,ey  
   do i = sx,ex
    tmp2 = lestmp3(i,j,k)
    tmp1 = lestmp3(i,j-1,k)
    Wtemp(i,j,k) = Wtemp(i,j,k) - (tmp2 - tmp1) * rdyc(j) 
   enddo
  enddo
 enddo
 write(IOUT,*)"wrhs-model:",model
endif !for LES

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

 call ghost(Wtemp,'wtemp',err)

 stat=err
return
end subroutine u3_rhs


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


subroutine rho_rhs(un,vn,wn,rhon,Rtemp,model,stat)
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
 use Domain, only: sx,ex,sy,ey,sz,ez
 use Grid,   only: rdxc, rdyc, rdzc, rdxe, rdye, rdze
 use Grid,   only: dxc, dyc, dzc, dxe, dye, dze
 use Parameters, only: rRe, rPr, Rsponge, flow_type
 use LESmod   
 use IO, only: IOUT
 use ratios, only: r_1_2
 implicit none

!Passed Variables
 real(r8),intent(in)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: rhon(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)       :: Rtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 logical,intent(in)         :: model
 integer,intent(out)        :: stat

!Local Variables
 integer                    :: i,j,k
 real(r8)                   :: rhoaver1, rhoaver2
 real(r8)                   :: D
 integer                    :: dir,face,var,ierr
 real(r8)                  :: tmp1, tmp2, tmp3, tmp4, del_g

 Rtemp = 0.d0

 D = rRe*rPr

 !*********************************
 !*  Linear Terms                 *
 !********************************* 
 do k=sz,ez
  do j=sy,ey
   do i=sx,ex
    !d2rho/dx2
    Rtemp(i,j,k) = D*( ( rhon(i+1,j,k) - rhon(i,j,k) )*rdxe(i) -  &
                 ( rhon(i,j,k)- rhon(i-1,j,k) )*rdxe(i-1) )*rdxc(i) &

    !d2rho/dy2
                + D*( ( rhon(i,j+1,k) - rhon(i,j,k) )* rdye(j) - &
                ( rhon(i,j,k) - rhon(i,j-1,k) )*rdye(j-1) )*rdyc(j) &
    !d2rho/dz2
                + D*( ( rhon(i,j,k+1) - rhon(i,j,k) )*rdze(k) - &
                ( rhon(i,j,k) - rhon(i,j,k-1) )*rdze(k-1) )*rdzc(k)
   enddo
  enddo
 enddo

 !*********************************
 !*  Non-linear Terms             *
 !*********************************
 do k=sz,ez
  do j=sy,ey
   do i=sx,ex
   !-rho*du/dx
    rhoaver2 = 0.5d0*( rhon(i+1,j,k) + rhon(i,j,k) )
    rhoaver1 = 0.5d0*( rhon(i,j,k) + rhon(i-1,j,k) )
    Rtemp(i,j,k) = Rtemp(i,j,k) + ( un(i-1,j,k)*rhoaver1 - un(i,j,k)*rhoaver2 )*rdxc(i)
   !-rho*dv/dy
    rhoaver2 = 0.5d0*(rhon(i,j+1,k) + rhon(i,j,k))
    rhoaver1 = 0.5d0*(rhon(i,j,k) + rhon(i,j-1,k))
    Rtemp(i,j,k) = Rtemp(i,j,k) + ( vn(i,j-1,k)*rhoaver1 - vn(i,j,k)*rhoaver2 )*rdyc(j)
   !-rho*dw/dz
    rhoaver2 = 0.5d0*(rhon(i,j,k+1) + rhon(i,j,k))
    rhoaver1 = 0.5d0*(rhon(i,j,k) + rhon(i,j,k-1))
    Rtemp(i,j,k) = Rtemp(i,j,k) + ( wn(i,j,k-1)*rhoaver1 - wn(i,j,k)*rhoaver2 )*rdzc(k)
   enddo
  enddo
 enddo

!LES
if (model) then
 !*********************************
 !*   LES Terms                   *
 !*********************************
 do k=sz,ez
  do j=sy,ey
   do i=sx,ex
!   tmp1 = Q1 at (i-1,j,k) 
    tmp4 = .5d0*( modS(i-1,j,k)+modS(i,j,k) )
    tmp3 = .5d0*( CTsgs(i-1,j,k)+CTsgs(i,j,k) )
    del_g =  ( dxe(i-1) * dyc(j)* dzc(k) )**(1.d0/3.d0)
    tmp1 = -1.d0 * tmp3 * del_g**2.d0 * tmp4 * (rhon(i,j,k)-rhon(i-1,j,k))*rdxe(i-1)
!   tmp2 = Q1 at (i,j,k) 
    tmp4 = .5d0*( modS(i,j,k)+modS(i+1,j,k) )
    tmp3 = .5d0*( CTsgs(i,j,k)+CTsgs(i+1,j,k) )
    del_g =  ( dxe(i) * dyc(j)* dzc(k) )**(1.d0/3.d0)
    tmp2 = -1.d0 * tmp3 * del_g**2.d0 * tmp4 * (rhon(i+1,j,k)-rhon(i,j,k))*rdxe(i)
    !dQ1/dx   
    Rtemp(i,j,k) = Rtemp(i,j,k) - (tmp2-tmp1)*rdxc(i)

!   tmp1 = Q2 at (i,j-1,k) 
    tmp4 = .5d0*( modS(i,j-1,k)+modS(i,j,k) )
    tmp3 = .5d0*( CTsgs(i,j-1,k)+CTsgs(i,j,k) )
    del_g =  ( dxc(i) * dye(j-1)* dzc(k) )**(1.d0/3.d0)
    tmp1 = -1.d0 * tmp3 * del_g**2.d0 * tmp4 * (rhon(i,j,k)-rhon(i,j-1,k))*rdye(j-1)
!   tmp2 = Q2 at (i,j,k) 
    tmp4 = .5d0*( modS(i,j,k)+modS(i,j+1,k) )
    tmp3 = .5d0*( CTsgs(i,j,k)+CTsgs(i,j+1,k) )
    del_g =  ( dxc(i) * dye(j)* dzc(k) )**(1.d0/3.d0)
    tmp2 = -1.d0 * tmp3 * del_g**2.d0 * tmp4 * (rhon(i,j+1,k)-rhon(i,j,k))*rdye(j)
    !dQ2/dy   
    Rtemp(i,j,k) = Rtemp(i,j,k) - (tmp2-tmp1)/dyc(j)

!   tmp1 = Q3 at (i,j,k-1) 
    tmp4 = .5d0*( modS(i,j,k-1)+modS(i,j,k) )
    tmp3 = .5d0*( CTsgs(i,j,k-1)+CTsgs(i,j,k) )
    del_g =  ( dxc(i) * dyc(j)* dze(k-1) )**(1.d0/3.d0)
    tmp1 = -1.d0 * tmp3 * del_g**2.d0 * tmp4 * (rhon(i,j,k)-rhon(i,j,k-1))*rdze(k-1)
!   tmp2 = Q3 at (i,j,k) 
    tmp4 = .5d0*( modS(i,j,k)+modS(i,j,k+1) )
    tmp3 = .5d0*( CTsgs(i,j,k)+CTsgs(i,j,k+1) )
    del_g =  ( dxc(i) * dyc(j)* dze(k) )**(1.d0/3.d0)
    tmp2 = -1.d0 * tmp3 * del_g**2.d0 * tmp4 * (rhon(i,j,k+1)-rhon(i,j,k))*rdze(k)
    !dQ3/dz   
    Rtemp(i,j,k) = Rtemp(i,j,k) - (tmp2-tmp1)/dzc(k)
   enddo
  enddo
 enddo
 write(IOUT,*)"rhorhs-model:",model
endif !for LES


 if (Rsponge) then
 !*********************************
 !*  Sponge Terms                 *
 !*********************************
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

subroutine Pgrad(pn,Utemp,Vtemp,Wtemp,factor)
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
 use Domain, only: sx,ex,sy,ey,sz,ez, EU, EV, EW
 implicit none

!Passed Variables
 real(r8),intent(in)        :: pn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: Utemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: Vtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)     :: Wtemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: factor

!Local Variables
 integer                   :: i,j,k,err1

 do k=sz,ez
  do j=sy,ey
   do i=sx,ex-EU
    Utemp(i,j,k)=Utemp(i,j,k) - ( pn(i+1,j,k)-pn(i,j,k) )*rdxe(i)*factor
   enddo
  enddo
 enddo
 call ghost(Utemp,'utemp',err1)

 do k=sz,ez
  do j=sy,ey-EV
   do i=sx,ex
    Vtemp(i,j,k)=Vtemp(i,j,k) - ( pn(i,j+1,k)-pn(i,j,k) )*rdye(j)*factor
   enddo
  enddo
 enddo
 call ghost(Vtemp,'vtemp',err1)

 do k=sz,ez-EW
  do j=sy,ey
   do i=sx,ex
    Wtemp(i,j,k)=Wtemp(i,j,k) - ( pn(i,j,k+1)-pn(i,j,k) )*rdze(k)*factor
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

subroutine Psource(Utemp,Vtemp,Wtemp,u,v,w,SRC,dt,stat)
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
 use Grid, only: rdxc, rdyc, rdzc
 use Domain, only: sx,ex,sy,ey,sz,ez,nzp2
 implicit none

 !Passed Variables
 real(r8),intent(in)        :: u(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: v(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)        :: w(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
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
      SRC(i,j,k) = ( (u(i,j,k)-u(i-1,j,k))*rdxc(i) &
                     +(v(i,j,k)-v(i,j-1,k))*rdyc(j) & 
                     +(w(i,j,k)-w(i,j,k-1))*rdzc(k) )*rdt &
                     +( (Utemp(i,j,k) - Utemp(i-1,j,k))*rdxc(i) & 
                     +  (Vtemp(i,j,k) - Vtemp(i,j-1,k))*rdyc(j) & 
                     +  (Wtemp(i,j,k) - Wtemp(i,j,k-1))*rdzc(k) )
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
 call u1_rhs(u,v,w,u1_tmp1,err1)
  if ( debug ) call write_plane(u1_tmp1,1,nxp2/2,0,0,'rkss_u1',.false.,err1)
  if (debug) call check_point('rk_ss#1',.false.)

 !X2-momentum 
 call u2_rhs(u,v,w,u2_tmp1,err1)
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
 call rho_rhs(u,v,w,rho,r_tmp1,err1)
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
