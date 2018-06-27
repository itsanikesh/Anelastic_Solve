subroutine statistics(stat)
!@t
! \textbf{subroutine statistics(stat)}
!@h
!   Description:
!     Determine whether the simulation is of a wake or shear layer and
!     call the appropriate statistics package.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use IO,         only: IOUT
 use Parameters, only: flow_type
 use Flow,       only: Vmodel, Rmodel
 implicit none

 !Passed Variables
  integer,intent(out) :: stat
 !Local Variables
  integer             :: ok

 ok=0

  select case(flow_type)
   case('Vshear','Channel')
    if (Vmodel.EQ.'DNS') then
     call statistics_vshear_DNS(ok)
    elseif (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
     call statistics_vshear_EDDY(ok)
    else
     write(IOUT,'(a30,a)') "STATISTICS NOT AVAILABLE FOR MODEL TYPE: ",trim(flow_type)
     ok=1
    endif
   case('Twake','wake','SPwake','PRPwake')
!    call statistics_wake(ok)
   case DEFAULT
    write(IOUT,'(a30,a)') "STATISTICS NOT AVAILABLE FOR FLOW TYPE: ",trim(flow_type)
    ok=1
   end select

 stat=ok
return
end subroutine statistics

subroutine statistics_small(stat)
!@t
! \textbf{subroutine statistics\_small(stat)}
!@h
!   Description:
!     Determine whether the simulation is of a wake or shear layer and
!     call the appropriate small statistics package.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use IO,         only:IOUT
 use Parameters, only: flow_type
 implicit none

 !Passed Variables
  integer,intent(out) :: stat
 !Local Variables
  integer             :: ok

 ok=0

  select case(flow_type)
   case('Vshear','Channel')
    call statistics_vshear_small(ok)
   case('Twake','Wake','SPwake','PRPwake')
!   call statistics_wake_small(ok)
   case DEFAULT
    write(IOUT,'(a30,a)') "STATISTICS NOT AVAILABLE FOR FLOW TYPE: ",trim(flow_type)
    ok=1
   end select

 stat=ok
return
end subroutine statistics_small

#ifndef PARALLEL
subroutine statistics_vshear(ok)
 implicit none
 integer,intent(out) :: ok
 ok=0
 return
end subroutine

subroutine statistics_vshear_small(ok)
 implicit none
 integer,intent(out) :: ok
 ok=0
 return
end subroutine
subroutine statistics_wake(ok)
 implicit none
 integer,intent(out) :: ok
 ok=0
 return
end subroutine

subroutine statistics_wake_small(ok)
 implicit none
 integer,intent(out) :: ok
 ok=0
 return
end subroutine

#else

subroutine statistics_vshear_EDDY(ok)
!@t
! \textbf{subroutine statistics\_vshear(ok)}
!@h
!   Description:
!     Calculate all statistics for the vertical shear layer.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!     1.0       08/2009  updated. [Hieu T. Pham] 

 use ntypes, only: r8 
 use IO,     only: IOUT, statDIR, statbin, tkstat
 use Flow,   only: u,v,w,p,rho,u1_tmp2,u2_tmp2,u3_tmp2,r_tmp1,r_tmp2,scal1,scal1_tmp2,Vmodel
 use LESmod
 use transient
 use domain, only: sx,ex,sy,ey,sz,ez,nzp2,nyp2
 use Spng,   only: x3spng
 use grid   
 use parameters, only: time,rRe,rSc,nstep, rho_0, g, rPr, szint, ezint, Texp, Scon 
 use stat_params, only: GSTATS_store, GSTATS_start_time, GSTATS_end_time, GSTATS_current, GSTATS_old_time
 use dd,     only: myid,coords,rankx3,comm3d
 use ratios
 implicit none

!Passed Variables
 integer,intent(out) :: ok

!Local Variables
 integer,parameter :: nstats=250


 real(r8)          :: STATS(sz-1:ez+1,1:nstats)
 real(r8)          :: GSTATS(1:nzp2,1:nstats)
 character(len=25) :: Sname(1:nstats) 
 integer           :: stat

 integer  :: err1, i, j, k, s1 , sloc, n, plane, inttmp
 integer  :: rholoc(sz-1:ez+1),rank(sz-1:ez+1)
 real(r8) :: mean(sz-1:ez+1), rms(sz-1:ez+1), temp(sz-1:ez+1),input(sz-1:ez+1)
 real(r8) :: tempF(1:nzp2), d_theta, del_g
 character(len=150) :: filen1
 real(r8) :: scalatmp,maxerr
 character(len=2048) :: tkheader

 real(r8),allocatable,dimension(:,:,:) :: Ftemp
 logical,parameter             :: debug=.false.

 if (debug) call check_point('stat#0',.false.)

 allocate( Ftemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 ) 
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating Ftemp in stat1d"
  goto 1000
 endif

 Sname="NA"

!INTERPOLATE VELOCITIES TO CELL CENTERS
  u1_tmp2 = 0.d0
  u2_tmp2 = 0.d0
  u3_tmp2 = 0.d0
  STATS = 0.d0
  GSTATS = 0.d0
  call center_velocity(u,u1_tmp2,1 )
  call center_velocity(v,u2_tmp2,2 ) 
  call center_velocity(w,u3_tmp2,3 )
!******************************************************************
!********GROUP 1 -- MEAN, RMS, VERTICAL GRADIENT*******************
!******************************************************************
 !<U1>,<u1'> 
  sloc=1;Sname(sloc)="U1"
  sloc=8;Sname(sloc)="u1'"
  call avgrmsX1X2(u1_tmp2,STATS(:,1),STATS(:,8),'uc')

 !<U2>,<u2'> 
  sloc=2;Sname(sloc)="U2"
  sloc=9;Sname(sloc)="u2'"
  call avgrmsX1X2(u2_tmp2,STATS(:,2),STATS(:,9),'vc')

 !<U3>,<u3'> 
  sloc=3;Sname(sloc)="U3"
  sloc=10;Sname(sloc)="u3'"
  call avgrmsX1X2(u3_tmp2,STATS(:,3),STATS(:,10),'wc')

 !<P>,<p'> 
  sloc=4;Sname(sloc)="P"
  sloc=11;Sname(sloc)="p'"
  call avgrmsX1X2(p,STATS(:,4),STATS(:,11),'p')

 !<T>,<t'> 
  sloc=5;Sname(sloc)="T"
  sloc=12;Sname(sloc)="t'"
  call avgrmsX1X2(rho,STATS(:,5),STATS(:,12),'rf')

 !<S>,<s'> 
  sloc=6;Sname(sloc)="S"
  sloc=13;Sname(sloc)="s'"
  call avgrmsX1X2(scal1,STATS(:,6),STATS(:,13),'sf')

 !<RHO>,<rho'>
  r_tmp1 = -Texp*rho+Scon*scal1
  sloc=7;Sname(sloc)="RHO"
  sloc=14;Sname(sloc)="rho'"
  call avgrmsX1X2(r_tmp1,STATS(:,7),STATS(:,14),'cfluc')

 !d<U1>/dx3
  !sloc 1=U1
   sloc=15;Sname(sloc)="dU1dx3"
   do k=sz,ez
    STATS(k,sloc)=(0.5d0*(STATS(k+1,1)+STATS(k,1))&
                  -0.5d0*(STATS(k,1)+STATS(k-1,1)))*rdzc(k)
   enddo

 !d<U2>/dx3
  !sloc 2=U2
   sloc=16;Sname(sloc)="dU2dx3"
   do k=sz,ez
    STATS(k,sloc)=(0.5d0*(STATS(k+1,2)+STATS(k,2))&
                  -0.5d0*(STATS(k,2)+STATS(k-1,2)))*rdzc(k)
   enddo

 !d<T>/dx3
  !sloc 5=<T>
   sloc=17;Sname(sloc)="dTdx3"
   do k=sz,ez
    STATS(k,sloc)=(0.5d0*(STATS(k+1,5)+STATS(k,5))&
                  -0.5d0*(STATS(k,5)+STATS(k-1,5)))*rdzc(k)
   enddo

 !d<S>/dx3
  !sloc 6=<S>
   sloc=18;Sname(sloc)="dSdx3"
   do k=sz,ez
    STATS(k,sloc)=(0.5d0*(STATS(k+1,6)+STATS(k,6))&
                  -0.5d0*(STATS(k,6)+STATS(k-1,6)))*rdzc(k)
   enddo
 
 !d<RHO>/dx3
  !sloc 7=<RHO>
   sloc=19;Sname(sloc)="dRHOdx3"
   do k=sz,ez
    STATS(k,sloc)=(0.5d0*(STATS(k+1,7)+STATS(k,7))&
                  -0.5d0*(STATS(k,7)+STATS(k-1,7)))*rdzc(k)
   enddo

 !Nt2 
  !sloc 17=dTdx3 
   sloc=20
   Sname(sloc)="Nt2"
   STATS(:,sloc)=g/rho_0*Texp*STATS(:,17)

 !Ns2 
  !sloc 18=dSdx3 
   sloc=21
   Sname(sloc)="Ns2"
   STATS(:,sloc)=-g/rho_0*Scon*STATS(:,18)

 !N2 
  !sloc 20=Nt2; sloc 21=Ns2 
   sloc=22
   Sname(sloc)="N2"
   STATS(:,sloc)= STATS(:,20) + STATS(:,21)

 !invtanRig
  !sloc 15=dU1dx3; sloc 16=dU2dx3; sloc 22= N2
    sloc=23
    Sname(sloc)="invtanRig"
    STATS(:,sloc)= atan( STATS(:,22)/(STATS(:,15)**2.d0+STATS(:,16)**2.d0) )

  if (debug) call check_point('stat#1',.false.)

!******************************************************************
!*********GROUP 2 -- VORTICITY: MEAN, RMS, MAGNITUDE***************
!******************************************************************
 !<OMG1>,<omg1'> 
   call vorticity(u1_tmp2,r_tmp2,1)
   sloc=24;Sname(sloc)="OMG1"
   sloc=28;Sname(sloc)="omg1'"
   call avgrmsX1X2(u1_tmp2,STATS(:,24),STATS(:,28),'cfluc')

 !<OMG2>,<omg2'> 
   call vorticity(u2_tmp2,r_tmp2,2)
   sloc=25;Sname(sloc)="OMG2"
   sloc=29;Sname(sloc)="omg2'"
   call avgrmsX1X2(u2_tmp2,STATS(:,25),STATS(:,29),'cfluc')

 !<OMG3>,<omg3'> 
   call vorticity(u3_tmp2,r_tmp2,3)
   sloc=26;Sname(sloc)="OMG3"
   sloc=30;Sname(sloc)="omg3'"
   call avgrmsX1X2(u3_tmp2,STATS(:,26),STATS(:,30),'cfluc')

 !<OMG_MAG>,<omg_mag'>
    r_tmp1 = dsqrt( u1_tmp2**2+u2_tmp2**2+u3_tmp2**2 ) 
    sloc=27;Sname(sloc)="OMG_MAG"
    sloc=31;Sname(sloc)="omg_mag'"
   call avgrmsX1X2(r_tmp1,STATS(:,27),STATS(:,31),'cfluc')
    
  if (debug) call check_point('stat#2',.false.)

!******************************************************************
!*********GROUP 3 -- CROSS CORRELAIONS: DOUBLE, TRIPLE*************
!***Subtract mean from cell centered quantities and store in ******
!****************u1_tmp2, u2_tmp2, u3_tmp2 ************************
!******************************************************************
  u1_tmp2 = 0.d0
  u2_tmp2 = 0.d0
  u3_tmp2 = 0.d0
  call center_velocity(u,u1_tmp2,1 )
  call center_velocity(v,u2_tmp2,2 ) 
  call center_velocity(w,u3_tmp2,3 )

  !u'
   call avgX1X2(u1_tmp2,mean,'uc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u1_tmp2(i,j,k)=u1_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u1_tmp2,'cfluc',err1) 
 
  !v'
   call avgX1X2(u2_tmp2,mean,'vc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u2_tmp2(i,j,k)=u2_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u2_tmp2,'cfluc',err1)

  !w'  
   call avgX1X2(u3_tmp2,mean,'wc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u3_tmp2(i,j,k)=u3_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u3_tmp2,'cfluc',err1)

  !<u1'u2'>
   sloc=32;Sname(sloc)="u1'u2'"
   r_tmp1=u1_tmp2*u2_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u1'u3'>
   sloc=33;Sname(sloc)="u1'u3'"
   r_tmp1=u1_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2'u3'>
   sloc=34;Sname(sloc)="u2'u3'"
   r_tmp1 = u2_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !p'
   call avgX1X2(p,mean,'c')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      Ftemp(i,j,k) =p(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(r_tmp2,'cfluc',err1) 

  !<u1'p'>
   sloc=35;Sname(sloc)="u1'p'"
   r_tmp1=u1_tmp2*Ftemp
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2'p'>
   sloc=36;Sname(sloc)="u2'p'"
   r_tmp1 = u2_tmp2*Ftemp
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u3'p'>
   sloc=37;Sname(sloc)="u3'p'"
   r_tmp1 = u3_tmp2*Ftemp
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !T'
   call avgX1X2(rho,mean,'rf')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      r_tmp2(i,j,k)=rho(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(Ftemp,'cfluc',err1)

  !<u1'T'>
   sloc=38;Sname(sloc)="u1't'"
   r_tmp1= u1_tmp2*r_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2'T'>
   sloc=39;Sname(sloc)="u2't'"
   r_tmp1 = u2_tmp2*r_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u3'T'>
   sloc=40;Sname(sloc)="u3't'"
   r_tmp1 = u3_tmp2*r_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !S'
   call avgX1X2(scal1,mean,'sf')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      scal1_tmp2(i,j,k)=scal1(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(Ftemp,'cfluc',err1)

  !<u1'S'>
   sloc=41;Sname(sloc)="u1's'"
   r_tmp1= u1_tmp2*scal1_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2'S'>
   sloc=42;Sname(sloc)="u2's'"
   r_tmp1 = u2_tmp2*scal1_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u3'S'>
   sloc=43;Sname(sloc)="u3's'"
   r_tmp1 = u3_tmp2*scal1_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u1'u1'u3'>
   sloc=44;Sname(sloc)="u1'u1'u3'"
   r_tmp1 = u1_tmp2*u1_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2'u2'u3'>
   sloc=45;Sname(sloc)="u2'u2'u3'"
   r_tmp1 = u2_tmp2*u2_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u3'u3'u3'>
   sloc=46;Sname(sloc)="u3'u3'u3'"
   r_tmp1 = u3_tmp2*u3_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u1'u2'u3'>
   sloc=47;Sname(sloc)="u1'u2'u3'"
   r_tmp1 = u1_tmp2*u2_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u1'u3'u3'>
   sloc=48;Sname(sloc)="u1'u3'u3'"
   r_tmp1 = u1_tmp2*u3_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2'u3'u3'>
   sloc=49;Sname(sloc)="u2'u3'u3'"
   r_tmp1 = u2_tmp2*u3_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<T'u1'u3'>
   sloc=50;Sname(sloc)="t'u1'u3'"
   r_tmp1 = r_tmp2*u1_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')
 
  !<T'u2'u3'>
   sloc=51;Sname(sloc)="t'u2'u3'"
   r_tmp1 = r_tmp2*u2_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<T'u3'u3'>
   sloc=52;Sname(sloc)="t'u3'u3'"
   r_tmp1 = r_tmp2*u3_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<T'T'u3'>
   sloc=53;Sname(sloc)="t'rho'u3'"
   r_tmp1 = r_tmp2*r_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<S'u1'u3'>
   sloc=54;Sname(sloc)="s'u1'u3'"
   r_tmp1 = scal1_tmp2*u1_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')
 
  !<S'u2'u3'>
   sloc=55;Sname(sloc)="s'u2'u3'"
   r_tmp1 = scal1_tmp2*u2_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<S'u3'u3'>
   sloc=56;Sname(sloc)="s'u3'u3'"
   r_tmp1 = scal1_tmp2*u3_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<S'S'u3'>
   sloc=57;Sname(sloc)="s's'u3'"
   r_tmp1 = scal1_tmp2*scal1_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u1's13'>
   !s'_13=1/2*(prl_u1'_prl_x3 + prl_u3'_prl_x1)
   sloc=58;Sname(sloc)="u1's13'"
   call deriv(u1_tmp2,r_tmp1,3)
   call deriv(u3_tmp2,Ftemp,1)
   r_tmp1=u1_tmp2*(0.5d0*(Ftemp+r_tmp1))
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2's23'>
   !s'_23=1/2*(prl_u2'_prl_x3 + prl_u3'_prl_x2)
   sloc=59;Sname(sloc)="u2's23'"
   call deriv(u3_tmp2,Ftemp,2)
   call deriv(u2_tmp2,r_tmp1,3)
   r_tmp1=u2_tmp2*(r_1_2*(Ftemp+r_tmp1))
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u3's33'>
   !s'_33=(prl_u3'_prl_x3)
   sloc=60;Sname(sloc)="u3's33'"
   call deriv(u3_tmp2,r_tmp1,3)
   r_tmp1=u3_tmp2*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')


  if (debug) call check_point('stat#3',.false.)
!******************************************************************
!*******GROUP 4 -- TKE DISSIPATION -2*nu*<sij'sij'>****************
!**** u1_tmp2, u2_tmp2, u3_tmp2 store 3D fluctuating fields********
!******************************************************************
  !Dissipation Components!
  !epsilon11=-2*rRe*(prl_u1'_prl_x1)**2
   sloc=61;Sname(sloc)="eps11"
   call deriv(u1_tmp2,r_tmp1,1)
   r_tmp1=-2.d0*rRe*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon12=-rRe*( (prl_u1'_prl_x2)**2 + prl_u1'_prl_x2*prl_u2'_prl_x1 )
   sloc=62;Sname(sloc)="eps12"
   call deriv(u1_tmp2,r_tmp1,2)
   call deriv(u2_tmp2,Ftemp,1)
   Ftemp=r_tmp1*Ftemp 
   r_tmp1=-rRe*(r_tmp1*r_tmp1+Ftemp)
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon13=-rRe*( (prl_u1'_prl_x3)**2 + prl_u1'_prl_x3*prl_u3'_prl_x1 )
   sloc=63;Sname(sloc)="eps13"
   call deriv(u1_tmp2,r_tmp1,3)
   call deriv(u3_tmp2,Ftemp,1)
   Ftemp=r_tmp1*Ftemp
   r_tmp1=-rRe*(r_tmp1*r_tmp1+Ftemp)
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon21=-rRe*( (prl_u2'_prl_x1)**2 + prl_u2'_prl_x1*prl_u1'_prl_x2 )
   sloc=64;Sname(sloc)="eps21"
   call deriv(u2_tmp2,r_tmp1,1)
   call deriv(u1_tmp2,Ftemp,2)
   Ftemp=r_tmp1*Ftemp
   r_tmp1=-rRe*(r_tmp1*r_tmp1+Ftemp)
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon22=-2*rRe*(prl_u2'_prl_x2)**2
   sloc=65;Sname(sloc)="eps22"
   call deriv(u2_tmp2,r_tmp1,2)
   r_tmp1=-2.d0*rRe*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon23=-rRe*( (prl_u2'_prl_x3)**2 + prl_u2'_prl_x3*prl_u3'_prl_x2 )
   sloc=66;Sname(sloc)="eps23"
   call deriv(u2_tmp2,r_tmp1,3)
   call deriv(u3_tmp2,Ftemp,2)
   Ftemp=r_tmp1*Ftemp
   r_tmp1=-rRe*(r_tmp1*r_tmp1+Ftemp)
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon31=-rRe*( (prl_u3'_prl_x1)**2 + prl_u3'_prl_x1*prl_u1'_prl_x3)
   sloc=67;Sname(sloc)="eps31"
   call deriv(u3_tmp2,r_tmp1,1)
   call deriv(u1_tmp2,Ftemp,3)
   Ftemp=r_tmp1*Ftemp
   r_tmp1=-rRe*(r_tmp1*r_tmp1+Ftemp)
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon32=-rRe*( (prl_u3'_prl_x2)**2 + prl_u3'_prl_x3*prl_u2'_prl_x3)
   sloc=68;Sname(sloc)="eps32"
   call deriv(u3_tmp2,r_tmp1,2)
   call deriv(u2_tmp2,Ftemp,3)
   Ftemp=r_tmp1*Ftemp
   r_tmp1=-rRe*(r_tmp1*r_tmp1+Ftemp)
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon33=-2*rRe*(prl_u3'_prl_x3)**2
   sloc=69;Sname(sloc)="eps33"
   call deriv(u3_tmp2,r_tmp1,3)
   r_tmp1=-2.d0*rRe*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')


  if (debug) call check_point('stat#4',.false.)
!******************************************************************
!******************GROUP 5 -- SCALAR  DISSIPATION  ****************
!**** u1_tmp2, u2_tmp2, u3_tmp2 store vel fluctuating fields*******
!************r_tmp2, scal1_tmp2: T,S  fluctuating fields***********
!****************** r_tmp1, Ftemp: free arrays*********************
!******************************************************************
  !T'
   call avgX1X2(rho,mean,'rf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    r_tmp2(i,j,k)=rho(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(r_tmp2,'cfluc',err1)

  !Tepsilon_1=-2/Re/Pr*(prl_T'_prl_x1)**2
   sloc=70;Sname(sloc)="Teps1"
   call deriv(r_tmp2,r_tmp1,1)
   r_tmp1=-2.d0*rRe*rPr*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !Tepsilon_2=-2/Re/Pr*(prl_T'_prl_x2)**2
   sloc=71;Sname(sloc)="Teps2"
   call deriv(r_tmp2,r_tmp1,2)
   r_tmp1=-2.d0*rRe*rPr*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !Tepsilon_3=-2/Re/Pr(prl_T'_prl_x3)**2
   sloc=72;Sname(sloc)="Teps3"
   call deriv(r_tmp2,r_tmp1,3)
   r_tmp1=-2.d0*rRe*rPr*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !S'
   call avgX1X2(scal1,mean,'rf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    scal1_tmp2(i,j,k)=scal1(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(scal1_tmp2,'cfluc',err1)

  !Sepsilon_1=-2/Re/Pr*(prl_S'_prl_x1)**2
   sloc=73;Sname(sloc)="Seps1"
   call deriv(scal1_tmp2,r_tmp1,1)
   r_tmp1=-2.d0*rRe*rSc*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !Sepsilon_2=-2/Re/Pr*(prl_S'_prl_x2)**2
   sloc=74;Sname(sloc)="Seps2"
   call deriv(scal1_tmp2,r_tmp1,2)
   r_tmp1=-2.d0*rRe*rSc*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !Sepsilon_3=-2/Re/Pr(prl_S'_prl_x3)**2
   sloc=75;Sname(sloc)="Seps3"
   call deriv(scal1_tmp2,r_tmp1,3)
   r_tmp1=-2.d0*rRe*rSc*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  if (debug) call check_point('stat#5',.false.)

!**********************************************************************
!***************GROUP 6 -- LES SUBGRID*********************************
!**********compute all stats at cell center****************************
!**********************************************************************
 !<modS>
  sloc=26; Sname(sloc)="modS"
  sloc=27; Sname(sloc)="modS'"
  call avgrmsX1X2(modS,STATS(:,26),STATS(:,27),'cfluc')
 
 !<Csgs>
  sloc=76; Sname(sloc)="Csgs"
  call avgX1X2(Csgs,STATS(:,sloc),'cfluc')
 
 !Nusgs 
  sloc=77; Sname(sloc)="Nusgs"
  call avgX1X2(nuT,STATS(:,sloc),'cfluc')

 !<CTsgs>
  sloc=78; Sname(sloc)="CTsgs"
  call avgX1X2(CTsgs,STATS(:,sloc),'cfluc')

 !Kappasgs 
  sloc=79; Sname(sloc)="Kappasgs"
  call avgX1X2(kappaT,STATS(:,sloc),'cfluc')

 !<CSsgs>
  sloc=80; Sname(sloc)="CSsgs"
  call avgX1X2(CSsgs,STATS(:,sloc),'cfluc')

 !Nappasgs 
  sloc=81; Sname(sloc)="Nappasgs"
  call avgX1X2(nappaT,STATS(:,sloc),'cfluc')

  u1_tmp2 = 0.d0
  u2_tmp2 = 0.d0
  u3_tmp2 = 0.d0
  lestmp = 0.d0
  lestmp1 = 0.d0
  lestmp2 = 0.d0
  lestmp3 = 0.d0
  lestmp4 = 0.d0
  un_gt = 0.d0
  vn_gt = 0.d0
  wn_gt = 0.d0
  call center_velocity(u,u1_tmp2,1 )
  call center_velocity(v,u2_tmp2,2 ) 
  call center_velocity(w,u3_tmp2,3 )

!compute shear stress at cell center - Csgs and strain is already at cell center
!lestmp1 stores S11, lestmp2 stores S22, lestmp3 stores S33
!un_gt stores S12, vn_gt stores S13, wn_gt stored S23
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp1(i,j,k) = 2.d0*nuT(i,j,k)*(u(i,j,k)-u(i-1,j,k))*rdxc(i)
   lestmp2(i,j,k) = 2.d0*nuT(i,j,k)*(v(i,j,k)-v(i,j-1,k))*rdyc(j)
   lestmp3(i,j,k) = 2.d0*nuT(i,j,k)*(w(i,j,k)-w(i,j,k-1))*rdzc(k)
   un_gt(i,j,k) = 2.d0*nuT(i,j,k)*0.5d0*( (u(i,j+1,k) - u(i,j,k))*rdye(j) &
                                          +(v(i+1,j,k) - v(i,j,k))*rdxe(i) )
   vn_gt(i,j,k) = 2.d0*nuT(i,j,k)*0.5d0*( (u(i,j,k+1) - u(i,j,k))*rdze(k) &
                                          +(w(i+1,j,k) - w(i,j,k))*rdxe(i) )
   wn_gt(i,j,k) = 2.d0*nuT(i,j,k)*0.5d0*( (v(i,j,k+1) - v(i,j,k))*rdze(k) &
                                          +(w(i,j+1,k) - w(i,j,k))*rdye(j) )
  enddo; enddo; enddo
  call ghost(lestmp1,'cfluc',err1)
  call ghost(lestmp2,'cfluc',err1)
  call ghost(lestmp3,'cfluc',err1)
  call ghost(un_gt,'cfluc',err1)
  call ghost(vn_gt,'cfluc',err1)
  call ghost(wn_gt,'cfluc',err1)

 !<tau11>,<tau11'>
  sloc=82; Sname(sloc)="TAU11"
  sloc=88; Sname(sloc)="tau11'"
  call avgrmsX1X2(lestmp1,STATS(:,82),STATS(:,88),'cfluc')
  
 !<tau22>,<tau22'>
  sloc=83; Sname(sloc)="TAU22"
  sloc=89; Sname(sloc)="tau22'"
  call avgrmsX1X2(lestmp2,STATS(:,83),STATS(:,89),'cfluc')

 !<tau33>,<tau33'>
  sloc=84; Sname(sloc)="TAU33"
  sloc=90; Sname(sloc)="tau33'"
  call avgrmsX1X2(lestmp3,STATS(:,84),STATS(:,90),'cfluc')
   
 !<tau12>,<tau12'>
  sloc=85; Sname(sloc)="TAU12"
  sloc=91; Sname(sloc)="tau12'"
  call avgrmsX1X2(un_gt,STATS(:,85),STATS(:,91),'cfluc')

 !<tau13>,<tau13'>
  sloc=86; Sname(sloc)="TAU13"
  sloc=92; Sname(sloc)="tau13'"
  call avgrmsX1X2(vn_gt,STATS(:,86),STATS(:,92),'cfluc')

 !<tau23>,<tau23'>
  sloc=87; Sname(sloc)="TAU23"
  sloc=93; Sname(sloc)="tau23'"
  call avgrmsX1X2(wn_gt,STATS(:,87),STATS(:,93),'cfluc')

 !Fluctuating velocity and stress
  !u'
  call avgX1X2(u1_tmp2,mean,'uc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   u1_tmp2(i,j,k)=u1_tmp2(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(u1_tmp2,'cfluc',err1) 
 
  !v'
  call avgX1X2(u2_tmp2,mean,'vc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   u2_tmp2(i,j,k)=u2_tmp2(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(u2_tmp2,'cfluc',err1)

  !w'  
  call avgX1X2(u3_tmp2,mean,'wc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   u3_tmp2(i,j,k)=u3_tmp2(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(u3_tmp2,'cfluc',err1)

  !tau11'  
  call avgX1X2(lestmp1,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp1(i,j,k)=lestmp1(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(lestmp1,'cfluc',err1)
  
  !tau22'  
  call avgX1X2(lestmp2,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp2(i,j,k)=lestmp2(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(lestmp2,'cfluc',err1)

  !tau33'  
  call avgX1X2(lestmp3,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp3(i,j,k)=lestmp3(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(lestmp3,'cfluc',err1)

  !tau12'  
  call avgX1X2(un_gt,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   un_gt(i,j,k)=un_gt(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(un_gt,'cfluc',err1)
  
  !tau13'  
  call avgX1X2(vn_gt,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   vn_gt(i,j,k)=vn_gt(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(vn_gt,'cfluc',err1)
  
  !tau23'  
  call avgX1X2(wn_gt,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   wn_gt(i,j,k)=wn_gt(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(wn_gt,'cfluc',err1)

 !Subgrid dissipation
 !Esgs11=-tau11'*du1'/dx1
  sloc=94; Sname(sloc)="Esgs11"
  call deriv(u1_tmp2,r_tmp1,1)
  r_tmp1=-r_tmp1*lestmp1
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !Esgs12=-tau12'*du1'/dx2
  sloc=95; Sname(sloc)="Esgs12"
  call deriv(u1_tmp2,r_tmp1,2)
  r_tmp1=-r_tmp1*un_gt
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !Esgs13=-tau13'*du1'/dx3
  sloc=96; Sname(sloc)="Esgs13"
  call deriv(u1_tmp2,r_tmp1,3)
  r_tmp1=-r_tmp1*vn_gt
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !Esgs21=-tau21'*du2'/dx1
  sloc=97; Sname(sloc)="Esgs21"
  call deriv(u2_tmp2,r_tmp1,1)
  r_tmp1=-r_tmp1*un_gt
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !Esgs22=-tau22'*du2'/dx2
  sloc=98; Sname(sloc)="Esgs22"
  call deriv(u2_tmp2,r_tmp1,2)
  r_tmp1=-r_tmp1*lestmp2
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !Esgs23=-tau23'*du2'/dx3
  sloc=99; Sname(sloc)="Esgs23"
  call deriv(u2_tmp2,r_tmp1,3)
  r_tmp1=-r_tmp1*wn_gt
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !Esgs31=-tau31'*du3'/dx1
  sloc=100; Sname(sloc)="Esgs31"
  call deriv(u3_tmp2,r_tmp1,1)
  r_tmp1=-r_tmp1*vn_gt
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !Esgs32=-tau32'*du3'/dx2
  sloc=101; Sname(sloc)="Esgs32"
  call deriv(u3_tmp2,r_tmp1,2)
  r_tmp1=-r_tmp1*wn_gt
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !Esgs33=-tau33'*du3'/dx3
  sloc=102; Sname(sloc)="Esgs33"
  call deriv(u3_tmp2,r_tmp1,3)
  r_tmp1=-r_tmp1*lestmp3
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !Subgrid transport
 !Tsgs131=tau13'*u1'
  sloc=103; Sname(sloc)="tau13'u1'"
  r_tmp1=vn_gt*u1_tmp2
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !Tsgs232=tau23'*u2'
  sloc=104; Sname(sloc)="tau23'u2'"
  r_tmp1=wn_gt*u2_tmp2
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')
 
 !Tsgs333=tau33'*u3'
  sloc=105; Sname(sloc)="tau33'u3'"
  r_tmp1=lestmp3*u3_tmp2
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')


!For scale-similarity stress
 if (Vmodel.EQ.'DMM') then
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    lestmp1(i,j,k) = S11(i,j,k)
    lestmp2(i,j,k) = S22(i,j,k)
    lestmp3(i,j,k) = S33(i,j,k)
    un_gt(i,j,k) = 0.25*(S12(i+1,j+1,k)+S12(i,j+1,k)+S12(i+1,j,k)+S12(i,j,k)) 
    vn_gt(i,j,k) = 0.25*(S13(i+1,j,k+1)+S13(i+1,j,k)+S13(i,j,k+1)+S13(i,j,k)) 
    wn_gt(i,j,k) =  0.25*(S23(i,j+1,k+1)+S23(i,j+1,k)+S23(i,j,k+1)+S23(i,j,k)) 
   enddo; enddo; enddo

  call ghost(lestmp1,'cfluc',err1)
  call ghost(lestmp2,'cfluc',err1)
  call ghost(lestmp3,'cfluc',err1)
  call ghost(un_gt,'cfluc',err1)
  call ghost(vn_gt,'cfluc',err1)
  call ghost(wn_gt,'cfluc',err1)

 !<tauSS11>,<tauSS11'>
  sloc=227; Sname(sloc)="TAUSS11"
  sloc=233; Sname(sloc)="tauSS11'"
  call avgrmsX1X2(lestmp1,STATS(:,227),STATS(:,233),'cfluc')
  
 !<tauSS22>,<tauSS22'>
  sloc=228; Sname(sloc)="TAUSS22"
  sloc=234; Sname(sloc)="tauSS22'"
  call avgrmsX1X2(lestmp2,STATS(:,228),STATS(:,234),'cfluc')

 !<tauSS33>,<tauSS33'>
  sloc=229; Sname(sloc)="TAUSS33"
  sloc=235; Sname(sloc)="tauSS33'"
  call avgrmsX1X2(lestmp3,STATS(:,229),STATS(:,235),'cfluc')
   
 !<tauSS12>,<tauSS12'>
  sloc=230; Sname(sloc)="TAUSS12"
  sloc=236; Sname(sloc)="tauSS12'"
  call avgrmsX1X2(un_gt,STATS(:,230),STATS(:,236),'cfluc')

 !<tauSS13>,<tauSS13'>
  sloc=231; Sname(sloc)="TAUSS13"
  sloc=237; Sname(sloc)="tauSS13'"
  call avgrmsX1X2(vn_gt,STATS(:,231),STATS(:,237),'cfluc')

 !<tauSS23>,<tauSS23'>
  sloc=232; Sname(sloc)="TAUSS23"
  sloc=238; Sname(sloc)="tauSS23'"
  call avgrmsX1X2(wn_gt,STATS(:,232),STATS(:,238),'cfluc')

 !Fluctuating velocity (already computed) and stress
  !u'
  !v'
  !w'  

  !tau11'  
  call avgX1X2(lestmp1,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp1(i,j,k)=lestmp1(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(lestmp1,'cfluc',err1)
  
  !tau22'  
  call avgX1X2(lestmp2,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp2(i,j,k)=lestmp2(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(lestmp2,'cfluc',err1)

  !tau33'  
  call avgX1X2(lestmp3,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp3(i,j,k)=lestmp3(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(lestmp3,'cfluc',err1)

  !tau12'  
  call avgX1X2(un_gt,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   un_gt(i,j,k)=un_gt(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(un_gt,'cfluc',err1)
  
  !tau13'  
  call avgX1X2(vn_gt,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   vn_gt(i,j,k)=vn_gt(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(vn_gt,'cfluc',err1)
  
  !tau23'  
  call avgX1X2(wn_gt,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   wn_gt(i,j,k)=wn_gt(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(wn_gt,'cfluc',err1)

 !Subgrid dissipation
 !EsgsSS11=-tau11'*du1'/dx1
  sloc=239; Sname(sloc)="EsgsSS11"
  call deriv(u1_tmp2,r_tmp1,1)
  r_tmp1=-r_tmp1*lestmp1
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !EsgsSS12=-tau12'*du1'/dx2
  sloc=240; Sname(sloc)="EsgsSS12"
  call deriv(u1_tmp2,r_tmp1,2)
  r_tmp1=-r_tmp1*un_gt
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !EsgsSS13=-tau13'*du1'/dx3
  sloc=241; Sname(sloc)="EsgsSS13"
  call deriv(u1_tmp2,r_tmp1,3)
  r_tmp1=-r_tmp1*vn_gt
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !EsgsSS21=-tau21'*du2'/dx1
  sloc=242; Sname(sloc)="EsgsSS21"
  call deriv(u2_tmp2,r_tmp1,1)
  r_tmp1=-r_tmp1*un_gt
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !EsgsSS22=-tau22'*du2'/dx2
  sloc=243; Sname(sloc)="EsgsSS22"
  call deriv(u2_tmp2,r_tmp1,2)
  r_tmp1=-r_tmp1*lestmp2
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !EsgsSS23=-tau23'*du2'/dx3
  sloc=244; Sname(sloc)="EsgsSS23"
  call deriv(u2_tmp2,r_tmp1,3)
  r_tmp1=-r_tmp1*wn_gt
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !EsgsSS31=-tau31'*du3'/dx1
  sloc=245; Sname(sloc)="EsgsSS31"
  call deriv(u3_tmp2,r_tmp1,1)
  r_tmp1=-r_tmp1*vn_gt
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !EsgsSS32=-tau32'*du3'/dx2
  sloc=246; Sname(sloc)="EsgsSS32"
  call deriv(u3_tmp2,r_tmp1,2)
  r_tmp1=-r_tmp1*wn_gt
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !EsgsSS33=-tau33'*du3'/dx3
  sloc=247; Sname(sloc)="EsgsSS33"
  call deriv(u3_tmp2,r_tmp1,3)
  r_tmp1=-r_tmp1*lestmp3
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !Subgrid transport
 !TsgsSS131=tauSS13'*u1'
  sloc=248; Sname(sloc)="tauSS13'u1'"
  r_tmp1=vn_gt*u1_tmp2
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !TsgsSS232=tauSS23'*u2'
  sloc=249; Sname(sloc)="tauSS23'u2'"
  r_tmp1=wn_gt*u2_tmp2
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')
 
 !TsgsSS333=tauSS33'*u3'
  sloc=250; Sname(sloc)="tauSS33'u3'"
  r_tmp1=lestmp3*u3_tmp2
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

endif !for DMM if statement at line 906

!subgrid heat flux
  lestmp1=0.d0
  lestmp2=0.d0
  lestmp3=0.d0

  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp1(i,j,k) = kappaT(i,j,k)*(rho(i+1,j,k)-rho(i-1,j,k))/(xc(i+1)-xc(i-1))
   lestmp2(i,j,k) = kappaT(i,j,k)*(rho(i,j+1,k)-rho(i,j-1,k))/(yc(j+1)-yc(j-1))
   lestmp3(i,j,k) = kappaT(i,j,k)*(rho(i,j,k+1)-rho(i,j,k-1))/(zc(k+1)-zc(k-1))
  enddo; enddo; enddo
   call ghost(lestmp1,'cfluc',err1)
   call ghost(lestmp2,'cfluc',err1)
   call ghost(lestmp3,'cfluc',err1)

 !<QT1>,<qt1'> 
  sloc=106; Sname(sloc)="QT1"
  sloc=109; Sname(sloc)="qt1'"
  call avgrmsX1X2(lestmp1,STATS(:,106),STATS(:,109),'cfluc')

!<QT2>,<qt2'> 
  sloc=107; Sname(sloc)="QT2"
  sloc=110; Sname(sloc)="qt2'"
  call avgrmsX1X2(lestmp2,STATS(:,107),STATS(:,110),'cfluc')

!<QT3>,<qt3'> 
  sloc=108; Sname(sloc)="QT3"
  sloc=111; Sname(sloc)="qt3'"
  call avgrmsX1X2(lestmp3,STATS(:,108),STATS(:,111),'cfluc')

 !t'
  call avgX1X2(rho,mean,'rf')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   r_tmp2(i,j,k)=rho(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(r_tmp2,'cfluc',err1)

 !qt1'
  call avgX1X2(lestmp1,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp1(i,j,k)=lestmp1(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(lestmp1,'cfluc',err1)
 
 !qt2'
  call avgX1X2(lestmp2,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp2(i,j,k)=lestmp2(i,j,k)-mean(k)
  enddo; enddo; enddo
 call ghost(lestmp2,'cfluc',err1)
 
 !qt3'
  call avgX1X2(lestmp3,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp3(i,j,k)=lestmp3(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(lestmp3,'cfluc',err1)

 !ET1sgs=-2*<qt1'*dt'dx1>
  sloc=112; Sname(sloc)="ET1sgs"
  call deriv(r_tmp2,r_tmp1,1)
  r_tmp1=-2.d0*lestmp1*r_tmp1
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !ET2sgs=-2*<qt2'*dt'dx2>
  sloc=113; Sname(sloc)="ET2sgs"
  call deriv(r_tmp2,r_tmp1,2)
  r_tmp1=-2.d0*lestmp2*r_tmp1
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !ET3sgs=-2*<qt3'*dt'dx3>
  sloc=114; Sname(sloc)="ET3sgs"
  call deriv(r_tmp2,r_tmp1,3)
  r_tmp1=-2.d0*lestmp3*r_tmp1
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !TTsgs=<t'qt3'>
  r_tmp1=lestmp3*r_tmp2
  sloc=115; Sname(sloc)="qt3't'"
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

!subgrid salt flux
  lestmp1=0.d0
  lestmp2=0.d0
  lestmp3=0.d0

  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp1(i,j,k) = nappaT(i,j,k)*(scal1(i+1,j,k)-scal1(i-1,j,k))/(xc(i+1)-xc(i-1))
   lestmp2(i,j,k) = nappaT(i,j,k)*(scal1(i,j+1,k)-scal1(i,j-1,k))/(yc(j+1)-yc(j-1))
   lestmp3(i,j,k) = nappaT(i,j,k)*(scal1(i,j,k+1)-scal1(i,j,k-1))/(zc(k+1)-zc(k-1))
  enddo; enddo; enddo
   call ghost(lestmp1,'cfluc',err1)
   call ghost(lestmp2,'cfluc',err1)
   call ghost(lestmp3,'cfluc',err1)

 !<QS1>,<qs1'> 
  sloc=116; Sname(sloc)="QS1"
  sloc=119; Sname(sloc)="qs1'"
  call avgrmsX1X2(lestmp1,STATS(:,116),STATS(:,119),'cfluc')

!<QS2>,<qs2'> 
  sloc=117; Sname(sloc)="QS2"
  sloc=120; Sname(sloc)="qs2'"
  call avgrmsX1X2(lestmp2,STATS(:,117),STATS(:,120),'cfluc')

!<QS3>,<qs3'> 
  sloc=118; Sname(sloc)="QS3"
  sloc=121; Sname(sloc)="qs3'"
  call avgrmsX1X2(lestmp3,STATS(:,118),STATS(:,121),'cfluc')

 !s'
  call avgX1X2(rho,mean,'sf')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   scal1_tmp2(i,j,k)=scal1(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(scal1_tmp2,'cfluc',err1)

 !qs1'
  call avgX1X2(lestmp1,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp1(i,j,k)=lestmp1(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(lestmp1,'cfluc',err1)
 
 !qs2'
  call avgX1X2(lestmp2,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp2(i,j,k)=lestmp2(i,j,k)-mean(k)
  enddo; enddo; enddo
 call ghost(lestmp2,'cfluc',err1)
 
 !qs3'
  call avgX1X2(lestmp3,mean,'cfluc')
  do k=sz,ez; do j=sy,ey; do i=sx,ex
   lestmp3(i,j,k)=lestmp3(i,j,k)-mean(k)
  enddo; enddo; enddo
  call ghost(lestmp3,'cfluc',err1)

 !ES1sgs=-2*<qs1'*ds'dx1>
  sloc=122; Sname(sloc)="ES1sgs"
  call deriv(scal1_tmp2,r_tmp1,1)
  r_tmp1=-2.d0*lestmp1*r_tmp1
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !ES2sgs=-2*<qs2'*ds'dx2>
  sloc=123; Sname(sloc)="ET2sgs"
  call deriv(scal1_tmp2,r_tmp1,2)
  r_tmp1=-2.d0*lestmp2*r_tmp1
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !ES3sgs=-2*<qs3'*ds'dx3>
  sloc=124; Sname(sloc)="ET3sgs"
  call deriv(scal1_tmp2,r_tmp1,3)
  r_tmp1=-2.d0*lestmp3*r_tmp1
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

 !TSsgs=<qs3's'>
  r_tmp1=lestmp3*scal1_tmp2
  sloc=125; Sname(sloc)="qs3's'"
  call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  if (debug) call check_point('stat#6',.false.)

!**********************************************************************
!********************GROUP 7 -- TKE BUDGET****************************
!**********************************************************************
  !TKE=1/2<u1'u'1+u2'u2'+u3'u3'>
   !sloc 8=<u1'>; sloc 9=<u2'>; sloc 10=<u3'>
   sloc=126; Sname(sloc)="TKE" 
   STATS(:,sloc)=0.5d0*(STATS(:,8)**2+STATS(:,9)**2+STATS(:,10)**2)

  !dTKE/dt
   sloc=127; Sname(sloc)="dkdt"
   STATS(:,sloc)=dkdt(:)

  !PROD=-<u1'u3'><dU1dx3> - <u2'u3'><dU2dx3>
   !sloc 33=<u1'u3'>; sloc 15=d<U1>/dx3
   !sloc 34=<u2'u3'>; sloc 16=d<U2>/dx3
   sloc=128; Sname(sloc)="Prod"
   STATS(:,sloc)=-STATS(:,33)*STATS(:,15) &
                 -STATS(:,34)*STATS(:,16) 

  !DISS 
   !sloc 61-69 = eps_ij (already minus)
   sloc=129; Sname(sloc)="Diss"
   do i=61,69
    STATS(:,sloc)=STATS(:,sloc)+STATS(:,i)
   enddo

  !BFLUXT=g/rho_0*Texp*<t'u3'>
   !sloc 40=<t'u3'>
   sloc=130; Sname(sloc)="BfluxT"
   STATS(:,sloc)=g/rho_0*Texp*STATS(:,40)

  !BFLUXS=g/rho_0*Scon*<s'u3'>
   !sloc 43=<s'u3'>
   sloc=131; Sname(sloc)="BfluxS"
   STATS(:,sloc)=-g/rho_0*Scon*STATS(:,43)
  
  !dTRANS=dT3/dx3 
   !T3=1/2*<ui'ui'u3'>+<p'u3'>/rho_0-2*rRe*<ui'si3'>
   !sloc 44,45,46 = <ui'ui'u3'>
   !sloc 37 = <p'u3'>
   !sloc 58,59,60=<ui'si3'>
   sloc=132; Sname(sloc)="dTrans"
   temp(:)=0.5d0*(STATS(:,44)+STATS(:,45)+STATS(:,46)) &
                +STATS(:,37)/rho_0 &
                -2.d0*rRe*(STATS(:,58)+STATS(:,59)+STATS(:,60))
   do k=sz,ez
    STATS(k,sloc)=(temp(k+1)-temp(k-1))/(zc(k+1)-zc(k-1))
   enddo     

  !subgrid Dissipation
   !sloc  94-102 = Esgs_ij (9 components already minus)
   !sloc 239-247 = EsgsSS_ij (9 components already minus)
   sloc=133; Sname(sloc)="Disssgs"
   do i=94,102
    STATS(:,sloc)=STATS(:,sloc)-STATS(:,i)
   enddo
   do i=239,247
    STATS(:,sloc)=STATS(:,sloc)-STATS(:,i)
   enddo

  !subgrid Transport = d/dt (<tau13'u1'+tau23'u2'+tau33'u3')
   !sloc 103=<tau13'u1'>
   !sloc 104=<tau23'u2'>
   !sloc 105=<tau33'u3'>
   !sloc 248=<tau13SS'u1'>
   !sloc 249=<tau23SS'u2'>
   !sloc 250=<tau33SS'u3'>

   sloc=134 
   Sname(sloc)="dTsgs"
   temp= STATS(:,103)+STATS(:,104)+STATS(:,105) &
       + STATS(:,248)+STATS(:,249)+STATS(:,250) 
   do k=sz,ez
    STATS(k,sloc)=(temp(k+1)-temp(k-1))/(zc(k+1)-zc(k-1))
   enddo    

  if (debug) call check_point('stat#7',.false.)

!******************************************************************
!*******************GROUP 8 -- T'2 BUDGET*************************
!******************************************************************
  !dTdt 
   sloc=135; Sname(sloc)="dt'2dt"
   STATS(:,sloc)=dt2dt

  !TPROD=-2<t'u3'>d<T>/dx3
   !sloc 40=<t'u3'>; sloc 17=d<T>/dx3
   sloc=136; Sname(sloc)="TProd"
   STATS(:,sloc)=-2.d0*STATS(:,40)*STATS(:,17)

  !TDISS
   !sloc 70,71,72 = Teps1,Teps2,Teps3
   sloc=137; Sname(sloc)="TDiss"
   STATS(:,sloc) = STATS(:,70)+STATS(:,71)+STATS(:,72)

  !TdTRANS=d(TR3)/dx3
   !TR3 = <t't'u3'>-rRe*rPr*d<t't'>/dx3
   !sloc 53=<t't'u3'>
   !sloc 12=<t'>
   sloc=138;Sname(sloc)="TdTrans"
   do k=sz,ez
    STATS(k,sloc) = (STATS(k+1,53)-STATS(k-1,53))/(zc(k+1)-zc(k-1)) &
               -rRe*rPr*( (STATS(k+1,12)**2.d0-STATS(k,12)**2.d0)*rdzc(k+1)& 
                         -(STATS(k,12)**2.d0-STATS(k-1,12)**2.d0)*rdzc(k) )*rdze(k)
   enddo
   
  !TEsgs
   !sloc 112,113,114 = ET1sgs,ET2sgs,ET3sgs
   sloc=139; Sname(sloc)="TEsgs"
   STATS(:,sloc) = -STATS(:,112)-STATS(:,113)-STATS(:,114)

  !TdTsgs
   !sloc 115 = qt3't'
   sloc=140; Sname(sloc)="TdTsgs"
   do k=sz,ez
    STATS(k,sloc)=(STATS(k+1,115)-STATS(k-1,115))/(zc(k+1)-zc(k-1))
   enddo
  
  !TPET = -0.5*g*Texp/dTx3*<t't'>
   !sloc 12=<t'>; sloc 17 = <dTdx3>
   sloc=141; Sname(sloc)="TPET"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*STATS(k,12)**2.d0
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   
   
  !dTPETdt=-0.5*g*Texp/dTdx3*dt2'dt 
   sloc=142; Sname(sloc)="dTPETdt"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*dt2dt(k)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   
   
  !TPETPROD=-2<t'u3'>d<T>/dx3
   !sloc 136=TProd
   sloc=143; Sname(sloc)="TPETProd"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*STATS(k,136)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  !TPETDISS
   !sloc 137="TDiss"
   sloc=144; Sname(sloc)="TPETDiss"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*STATS(k,137)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  !TPETdTRANS=d(TR3)/dx3
   !sloc 138 = TdTrans
   sloc=145;Sname(sloc)="TPETdTrans"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*STATS(k,138)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   
   
  !TPETEsgs
   !sloc 139=TEsgs
   sloc=146; Sname(sloc)="TPETEsgs"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*STATS(k,139)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  !TPETdTsgs
   !sloc 140=TdTsgs
   sloc=147; Sname(sloc)="TPETdTsgs"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*STATS(k,140)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  if (debug) call check_point('stat#8',.false.)

!******************************************************************
!*******************GROUP 9 -- S'2 BUDGET**************************
!******************************************************************
  !ds'2dt 
   sloc=148; Sname(sloc)="ds'2dt"
   STATS(:,sloc)=ds2dt

  !SProd=-2<s'u3'>d<S>/dx3
   !sloc 43=<s'u3'>; sloc 18=d<T>/dx3
   sloc=149; Sname(sloc)="SProd"
   STATS(:,sloc)=-2.d0*STATS(:,43)*STATS(:,18)

  !SDiss
   !sloc 73,74,75 = Seps1,Seps2,Seps3
   sloc=150; Sname(sloc)="SDiss"
   STATS(:,sloc) = STATS(:,73)+STATS(:,74)+STATS(:,75)

  !SdTRANS=d(SR3)/dx3
   !SR3 = <s's'u3'>-rRe*rPr*d<s's'>/dx3
   !sloc 57=<s's'u3'>; sloc 13=<s'>
   sloc=151;Sname(sloc)="SdTrans"
   do k=sz,ez
    STATS(k,sloc) = (STATS(k+1,57)-STATS(k-1,57))/(zc(k+1)-zc(k-1)) &
               -rRe*rSc*( (STATS(k+1,13)**2.d0-STATS(k,13)**2.d0)*rdzc(k+1) &
                         -(STATS(k,13)**2.d0-STATS(k-1,13)**2.d0)*rdzc(k) )*rdze(k)
   enddo
   
  !SEsgs
   !sloc 122,123,124 = ES1sgs,ES2sgs,ES3sgs
   sloc=152; Sname(sloc)="SEsgs"
   STATS(:,sloc) = -STATS(:,122)-STATS(:,123)-STATS(:,124)

  !SdTsgs
   !sloc 125 = qs3't'
   sloc=153; Sname(sloc)="SdTsgs"
   do k=sz,ez
    STATS(k,sloc)=(STATS(k+1,125)-STATS(k-1,125))/(zc(k+1)-zc(k-1))
   enddo
  
  !TPES = -0.5*g*Scon/dSx3*<s's'>
   !sloc 13=<s'>; sloc 18 = <dSdx3>
   sloc=154; Sname(sloc)="TPES"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*STATS(k,13)**2.d0
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   
   
  !dTPESdt=-0.5*g*Scon/dSdx3*dt2'dt 
   sloc=155; Sname(sloc)="dTPESdt"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*ds2dt(k)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   
   
  !TPESPROD=-2<s'u3'>d<S>/dx3
   !sloc 149=SProd
   sloc=156; Sname(sloc)="TPESProd"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*STATS(k,149)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  !TPESDISS
   !sloc 150="SDiss"
   sloc=157; Sname(sloc)="TPESDiss"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*STATS(k,150)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  !TPESdTRANS=d(TR3)/dx3
   !sloc 151 = TdTrans
   sloc=158;Sname(sloc)="TPESdTrans"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*STATS(k,151)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   
   
  !TPESEsgs
   !sloc 152=TEsgs
   sloc=159; Sname(sloc)="TPESEsgs"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*STATS(k,152)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  !TPESdTsgs
   !sloc 153=TdTsgs
   sloc=160; Sname(sloc)="TPESdTsgs"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*STATS(k,153)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  if (debug) call check_point('stat#9',.false.)

!******************************************************************
!*******************GROUP 10 -- MKE BUDGET*************************
!******************************************************************
 !MKE = 0.5(<u>^2+<v>^2+<w>^2)
  !sloc 1=<u>; sloc 2=<v>; sloc 3=<w>
  sloc=161; Sname(sloc)="MKE"
  STATS(:,sloc)=0.5d0*(STATS(:,1)**2.d0+STATS(:,2)**2.d0+STATS(:,3)**2.d0)

 !dMKEdt 
  sloc=162; Sname(sloc)="dMKEdt"
  STATS(:,sloc)= dMKEdt(:)
  
 !MProd = -Prod
  !sloc 128 = Prod
  sloc=163; Sname(sloc)="MProd"
  STATS(:,sloc) = -STATS(:,128)

 !MDiss = rRe*(dU1dx3^2+dU2dx3^2)
  !sloc 15=dU1dx3; sloc 16=dU2dx3
  sloc=164; Sname(sloc)="MDiss"
  STATS(:,sloc) = rRe*( STATS(:,15)**2.d0 + STATS(:,16)**2.d0)

 !MdTrans
  !MTrans = <U1><u1'u3'>+<U2><u2'u3'> - rRe (<U1>dU1dx3 + <V>dU2dx3)
  !sloc 1=<U1>; sloc 2=<U2>; sloc 15=dU1dx3; sloc 16=dU2dx3; 
  !sloc 33=<u1'u3'>; sloc 34=<u2'u3'>
  sloc=165; Sname(sloc)="MdTrans"
  do k=sz,ez
   STATS(k,sloc)=(STATS(k+1,1)*STATS(k+1,33)-STATS(k-1,1)*STATS(k-1,33))/(zc(k+1)-zc(k-1)) &
                +(STATS(k+1,2)*STATS(k+1,34)-STATS(k-1,2)*STATS(k-1,34))/(zc(k+1)-zc(k-1))&
                -rRe*(STATS(k,1)*STATS(k,15)+STATS(k,2)*STATS(k,15))  
  enddo      

 !MPsgs = <TAU13>dU1dx3 + <TAU23>dU2dx3
  !sloc 86=TAU13; sloc 87=TAU23
  !sloc 15=dU1dx3; sloc 16=dU2dx3
  sloc=166; Sname(sloc)="MPsgs"
  STATS(:,sloc)=STATS(:,86)*STATS(:,15)+STATS(:,87)*STATS(:,16)
 
 !MdTsgs 
  !MTsgs = <U1><TAU13> + <U2><TAU23>
  !sloc 1=<U1>; sloc 2=<U2>;
  !sloc 86=TAU13; sloc 87=TAU23
  sloc=167; Sname(sloc)="MdTsgs"
  do k=sz,ez
   STATS(k,sloc)=(STATS(k+1,1)*STATS(k+1,86)-STATS(k-1,1)*STATS(k-1,86))/(zc(k+1)-zc(k-1)) &
                +(STATS(k+1,2)*STATS(k+1,87)-STATS(k-1,2)*STATS(k-1,87))/(zc(k+1)-zc(k-1))
  enddo


  if (debug) call check_point('stat#10',.false.)

!******************************************************************
!*******************GROUP 11 -- MEAN BUDGET********************
!******************************************************************
 !dU1dt
  sloc=168; Sname(sloc)="dU1dt"
  STATS(:,sloc) = dU1dt
  
 !U1tflux = -d/dx3(<u1'u3'>)
  !sloc 33= <u1'u3'>
  sloc=169; Sname(sloc)="U1tflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,33)-STATS(k-1,33))/(zc(k+1)-zc(k-1))
  enddo 
 
 !U1vflux = rRe*d2/dz2 (<U1>)
  !sloc 1=<U1>
  sloc=170; Sname(sloc)="U1vflux"
  do k=sz,ez
   STATS(k,sloc)=rRe*( (STATS(k+1,1)-STATS(k,1))*rdzc(k+1) &
                      -(STATS(k,1)-STATS(k-1,1))*rdzc(k) )*rdze(k)
  enddo 

 !U1sgsflux = -d/dz(<TAU13>)
  !sloc 86=<TAU13>
  sloc=171; Sname(sloc)="U1sgsflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,86)-STATS(k-1,86))/(zc(k+1)-zc(k-1))
  enddo 
 
 !dU2dt
  sloc=172; Sname(sloc)="dU2dt"
  STATS(:,sloc) = dU2dt
  
 !U2tflux = -d/dx3(<u2'u3'>)
  !sloc 34= <u2'u3'>
  sloc=173; Sname(sloc)="U2tflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,34)-STATS(k-1,34))/(zc(k+1)-zc(k-1))
  enddo 
 
 !U2vflux = rRe*d2/dz2 (<U2>)
  !sloc 2=<U2>
  sloc=174; Sname(sloc)="U2vflux"
  do k=sz,ez
   STATS(k,sloc)=rRe*( (STATS(k+1,2)-STATS(k,2))*rdzc(k+1) &
                      -(STATS(k,2)-STATS(k-1,2))*rdzc(k) )*rdze(k)
  enddo 

 !U2sgsflux = -d/dz(<TAU13>)
  !sloc 87=<TAU23>
  sloc=175; Sname(sloc)="U2sgsflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,87)-STATS(k-1,87))/(zc(k+1)-zc(k-1))
  enddo 

 !dTdt
  sloc=176; Sname(sloc)="dTdt"
  STATS(:,sloc) = dTdt
  
 !Ttflux = -d/dx3(<t'u3'>)
  !sloc 40= <t'u3'>
  sloc=177; Sname(sloc)="Ttflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,40)-STATS(k-1,40))/(zc(k+1)-zc(k-1))
  enddo 
 
 !Tvflux = rRe*rPr*d2/dz2 (<T>)
  !sloc 5=<T>
  sloc=178; Sname(sloc)="Tvflux"
  do k=sz,ez
   STATS(k,sloc)=rRe*rPr*( (STATS(k+1,5)-STATS(k,5))*rdzc(k+1) &
                          -(STATS(k,5)-STATS(k-1,5))*rdzc(k) )*rdze(k)
  enddo 

 !Tsgsflux = -d/dz(<QT3>)
  !sloc 108=<QT3>
  sloc=179; Sname(sloc)="Tsgsflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,108)-STATS(k-1,108))/(zc(k+1)-zc(k-1))
  enddo 

 !dSdt
  sloc=180; Sname(sloc)="dSdt"
  STATS(:,sloc) = dSdt
  
 !Stflux = -d/dx3(<s'u3'>)
  !sloc 43= <s'u3'>
  sloc=181; Sname(sloc)="Stflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,43)-STATS(k-1,43))/(zc(k+1)-zc(k-1))
  enddo 
 
 !Svflux = rRe*rSc*d2/dz2 (<S>)
  !sloc 6=<S>
  sloc=182; Sname(sloc)="Svflux"
  do k=sz,ez
   STATS(k,sloc)=rRe*rSc*( (STATS(k+1,6)-STATS(k,6))*rdzc(k+1) &
                          -(STATS(k,6)-STATS(k-1,6))*rdzc(k) )*rdze(k)
  enddo 

 !Ssgsflux = -d/dz(<QS3>)
  !sloc 118=<QS3>
  sloc=183; Sname(sloc)="Tsgsflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,118)-STATS(k-1,118))/(zc(k+1)-zc(k-1))
  enddo 


!Assemble global statistics to masternode
  call MPI_BARRIER(comm3d,err1)
!  do k=1,nstats
!   if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(STATS(:,k),GSTATS(:,k),0,err1)
!   if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(STATS(:,k),0,err1)
!   call MPI_BARRIER(comm3d,err1)
!  enddo
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_allstatM(nstats,STATS,GSTATS,0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_allstatS(nstats,STATS,0,err1)
  call MPI_BARRIER(comm3d,err1)

  if (debug) call check_point('stat#11',.false.)

 if (myid.EQ.0) then  !BEGIN MASTER NODE
!******************************************************************
!****************GROUP 12 -- LENGTH SCALES*************************
!******************************************************************
  !domega
   !sloc 15=dU1dx3 
   sloc=184; Sname(sloc)="d_omega"
   scalatmp=0.d0
   do k=nzp2-12,2,-1 
    if (dabs(GSTATS(k,15)).GT.scalatmp) scalatmp = dabs( GSTATS(k,15) )
   enddo
   do k=2,nzp2-12
    if (dabs(dabs(GSTATS(k,15))-scalatmp).LT.1d-16) GSTATS(:,sloc) = zc(k)
   enddo

  !dtheta
   !sloc 1=<U1>
   sloc=185; Sname(sloc)="d_theta"
   scalatmp=0.d0
   do k=szint,ezint
    scalatmp = scalatmp + ( 0.5d0 - GSTATS(k+1,1)**2.d0 - GSTATS(k,1)**2.d0)*dzc(k)*0.5d0
   enddo
   GSTATS(:,sloc) = scalatmp
   d_theta = scalatmp

  !d_omega_T
   !1/(dTdx3)_max
   !sloc 17=dTdx3
   sloc=186;Sname(sloc)="d_omega_T"
   scalatmp = 0.d0
   do k=nzp2-12,2,-1
    if (dabs(GSTATS(k,17)).GT.scalatmp) scalatmp = dabs( GSTATS(k,17))
   enddo
   do k=2,nzp2-12
    if (dabs(abs(GSTATS(k,17))-scalatmp).LT.1d-16) GSTATS(:,sloc) = zc(k)
   enddo

  !d_omega_S
   !1/(dSdx3)_max
   !sloc 18=dSdx3
   sloc=187; Sname(sloc)="d_omega_S"
   scalatmp = 0.d0
   do k=nzp2-12,2,-1
    if (dabs(GSTATS(k,18)).GT.scalatmp) scalatmp = dabs( GSTATS(k,18))
   enddo
   do k=2,nzp2-12
    if (dabs(dabs(GSTATS(k,18))-scalatmp).LT.1d-16) GSTATS(:,sloc) = zc(k)
   enddo

  !Reb = (Diss + Disssgs)/(nu N2)
   !sloc 129=Diss; sloc 133=Disssgs
   !sloc 22=N2
   sloc=188; Sname(sloc)="Reb"
   do k=1,nzp2
    if (GSTATS(k,22).GT.1d-16) then
     GSTATS(k,sloc)=(abs(GSTATS(k,129))+abs(GSTATS(k,133)))/(rRe*GSTATS(k,22))
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo
    
  !Ozmidov Scale
   !((Diss+Dissgs)/<N>^3)^(1/2)
   !sloc 129=Diss; sloc 133=Disssgs  (minus)
   !sloc 22=N^2 
   sloc=189; Sname(sloc)="Lozmo"
   do k=1,nzp2
    if (abs(GSTATS(k,22)).GT.1d-16) then
     GSTATS(k,sloc)=((abs(GSTATS(k,129))+abs(GSTATS(k,133)))/abs(GSTATS(k,22))**1.5d0)**0.5d0
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo

  !Ellison scale LellisonT =<t'>/<dTdx3>
   !sloc 12=<t'>; sloc 17=<dTdx3>
   sloc=190; Sname(sloc)="LellisT"
   do k=1,nzp2
    if (abs(GSTATS(k,17)).GT.1d-16) then
     GSTATS(k,sloc) = GSTATS(k,12)/abs(GSTATS(k,17))
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo

  !Ellison scale LellisonS =<s'>/<dSdx3>
   !sloc 13=<s'>; sloc 18=<dTdx3>
   sloc=191; Sname(sloc)="LellisS"
   do k=1,nzp2
    if (abs(GSTATS(k,18)).GT.1d-16) then
     GSTATS(k,sloc) = GSTATS(k,13)/abs(GSTATS(k,18))
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo

  !eta=(nu^3/(Diss+Dissgs))^(1/4) 
   !sloc 129=Diss; sloc 133=Disssgs  (minus)
   sloc=192; Sname(sloc)="Lkolmo"
   do k=1,nzp2
    scalatmp = abs(GSTATS(k,129))+abs(GSTATS(k,133))
    if (scalatmp.GT.1d-16) then
     GSTATS(k,sloc)=(rRe**3.d0/scalatmp)**0.25d0
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo

  !etaT=(kappaT^3/(TPETDiss+TPETDissgs))^(1/4) 
   !sloc 144=TPETDiss; sloc 146=TPETEsgs  (minus)
   sloc=193; Sname(sloc)="LkolmoT"
   do k=1,nzp2
    scalatmp = abs(GSTATS(k,144))+abs(GSTATS(k,146))
    if (scalatmp.GT.1d-16) then
     GSTATS(k,sloc)=((rRe*rPr)**3.d0/scalatmp)**0.25d0
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo

  !etaS=(nu^3/(TPESDiss+TPESDissgs))^(1/4) 
   !sloc 157=TPESDiss; sloc 159=TPESEsgs  (minus)
   sloc=194; Sname(sloc)="LkolmoS"
   do k=1,nzp2
    scalatmp = abs(GSTATS(k,157))+abs(GSTATS(k,159))
    if (scalatmp.GT.1d-16) then
     GSTATS(k,sloc)=((rRe*rSc)**3.d0/scalatmp)**0.25d0
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo

  !Energy containing scale: Len = (2/3*TKE)^(3/2)/(Diss+Disssgs)
   !sloc 126 = TKE
   !sloc 129=Diss; sloc 133=Disssgs  (minus)
   sloc=195; Sname(sloc)="Len"
   GSTATS(:,sloc)=(2.d0/3.d0*GSTATS(:,126))**(3.d0/2.d0)/(abs(GSTATS(:,129))+abs(GSTATS(:,133)))

  !Buoyancy scale: Lbuoy=<w'>/N
   !sloc 10=<w'>; sloc 22=N^2
   sloc=196;Sname(sloc)="Lbuoy"
   GSTATS(:,sloc)=GSTATS(:,10)/GSTATS(:,22)**0.5d0

!**********************************************************************
!********************GROUP 13 -- intTKEBUDGET**************************
!**********************************************************************
  !intTKE
   !sloc 126=TKE 
   sloc = 197; Sname(sloc)="intTKE"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,126)+GSTATS(k,126))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intdTKE/dt
   !sloc 127=dTKEdt
   sloc=198; Sname(sloc)="intdTKEdt"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,127)+GSTATS(k,127))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intProd
   !sloc 128=Prod
   sloc=199; Sname(sloc)="intProd"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,128)+GSTATS(k,128))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intDISS
   !sloc 129=Diss
   sloc=200; Sname(sloc)="intDiss"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,129)+GSTATS(k,129))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intBfluxT
   !sloc 130=BfluxT
   sloc=201; Sname(sloc)="intBfluxT"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,130)+GSTATS(k,130))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intBfluxS
   !sloc 131=BfluxS
   sloc=202; Sname(sloc)="intBfluxS"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,131)+GSTATS(k,131))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intdTrans 
   !sloc 132= dTrans 
   sloc=203; Sname(sloc)="intdTrans"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,132)+GSTATS(k,132))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !Subgrid Diss
   !sloc 133 = Disssgs
   sloc=204; Sname(sloc)="intDisssgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,133)+GSTATS(k,133))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !Subgrid Trans
   !sloc 134 = dTsgs
   sloc=205; Sname(sloc)="intdTsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,134)+GSTATS(k,134))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPET
   !sloc 141=TPET 
   sloc=206; Sname(sloc)="intTPET"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,141)+GSTATS(k,141))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intdTPETdt
   !sloc 142=dTPETdt
   sloc=207; Sname(sloc)="intdTPETdt"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,142)+GSTATS(k,142))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

 !intTPETProd
   !sloc 143= TPETProd
   sloc=208; Sname(sloc)="intTPETProd"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,143)+GSTATS(k,143))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPETDiss
   !sloc 144=TPETDiss
   sloc=209; Sname(sloc)="intTPETDiss"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,144)+GSTATS(k,144))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPETdTrans
   !sloc 145=TPETdTrans 
   sloc=210; Sname(sloc)="intTPETdTrans"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,145)+GSTATS(k,145))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPETEsgs
   !sloc 146 = TPETEsgs
   sloc=211; Sname(sloc)="intTPETEsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,146)+GSTATS(k,146))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPETdTsgs
   !sloc 147 = TPETdTsgs
   sloc=212; Sname(sloc)="intTPETdTsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,147)+GSTATS(k,17))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPES
   !sloc 154=TPES 
   sloc=213; Sname(sloc)="intTPES"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,154)+GSTATS(k,154))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intdTPESdt
   !sloc 155=dTPESdt
   sloc=214; Sname(sloc)="intdTPESdt"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,155)+GSTATS(k,155))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

 !intTPESProd
   !sloc 156= TPESProd
   sloc=215; Sname(sloc)="intTPESProd"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,156)+GSTATS(k,156))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPESDiss
   !sloc 157=TPESDiss
   sloc=216; Sname(sloc)="intTPESDiss"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,157)+GSTATS(k,157))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPESdTrans
   !sloc 158=TPETdTrans 
   sloc=217; Sname(sloc)="intTPESdTrans"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,158)+GSTATS(k,158))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPESEsgs
   !sloc 159 = TPESEsgs
   sloc=218; Sname(sloc)="intTPESEsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,159)+GSTATS(k,159))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPESdTsgs
   !sloc 160 = TPESdTsgs
   sloc=219; Sname(sloc)="intTPESdTsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,160)+GSTATS(k,160))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intMKE
   !sloc 161=MKE 
   sloc=220; Sname(sloc)="intMKE"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,161)+GSTATS(k,161))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intdMKEdt
   !sloc 162=dMKEdt
   sloc=221; Sname(sloc)="intdMKEdt"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,162)+GSTATS(k,162))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

 !intMProd
   !sloc 163= MProd
   sloc=222; Sname(sloc)="intMProd"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,163)+GSTATS(k,163))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intMDiss
   !sloc 164=MDiss
   sloc=223; Sname(sloc)="intMDiss"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,164)+GSTATS(k,164))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intMdTrans
   !sloc 165=MdTrans 
   sloc=224; Sname(sloc)="intMdTrans"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,165)+GSTATS(k,165))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intMPsgs
   !sloc 166 = MPsgs
   sloc=225; Sname(sloc)="intMPsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,166)+GSTATS(k,166))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intMdTsgs
   !sloc 167 = MdTsgs
   sloc=226; Sname(sloc)="intMdTsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,167)+GSTATS(k,167))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp


!******************end computing statistics***************************


  if (statbin ) then
  !Create output filename
  write(filen1,'(a,i5.5,a)') trim(statDIR)//'avg',nstep,'.bin'
  open( unit=500,file=filen1,status='unknown',form='unformatted',iostat=s1 )
   write(500) time
   write(500) nzp2, nstats
   write(500) zc
   write(500) Sname
   write(500) GSTATS  
  close(500)

 write(IOUT,'(a18,f10.5)') "FULL VSHEAR STATISTICS BINARY WRITTEN AT: ",time

 endif

 if (tkstat) then
!****************************************************************************
!*******************************TK STAT WRITE********************************
!****************************************************************************
!write out mean and rms quantities in TKSTAT format
 !Create output filename
 filen1 = statdir
 call concat(filen1,'avg')
 call concati(filen1,nstep)
                                                                                                                             
 open( unit=500,file=filen1,status='unknown',form='formatted',iostat=s1 )
  !WRITE HEADER
  write(500,'(a8,f16.8)') "RTIME = ",time

  !CREATE THE MAIN HEADER
  tkheader='I J Z S1'
  do n=1,nstats
   tkheader=trim(tkheader)//' '//trim(Sname(n))
  enddo
  write(500,'(a)') trim(tkheader)

  do k=1,nzp2
   plane=1
   write(500,135) i,' ',k,' ',zc(k),' ',zc(k)/d_theta,' ',(GSTATS(k,n),n=1,nstats)
  enddo
 close(500)
                                                                                                                             
 write(IOUT,'(a18,f15.8)') "TKSTATS WRITTEN AT: ",time

 !Time averaging
! GSTATS_current = 0.d0
! GSTATS_start_time = 1.394187611152192d5 
! GSTATS_end_time = 2.788375222304385d5
! if (time.LT.GSTATS_start_time) then
!  GSTATS_store = GSTATS
!  GSTATS_current = GSTATS
! else 
!  GSTATS_store = GSTATS_store + GSTATS*(time - GSTATS_old_time) 
!  GSTATS_current = GSTATS_store/(time-GSTATS_start_time)
!  write(IOUT,'(a18,f15.8)') "COMPUTING TIME_AVERAGE_TKSTATS WRITTEN AT: ",time
! endif
! GSTATS_old_time = time

 !Create output filename for time averaging
! filen1 = statdir
! call concat(filen1,'Tavg')
! call concati(filen1,nstep)
! open( unit=501,file=filen1,status='unknown',form='formatted',iostat=s1 )
  !WRITE HEADER
!  write(501,'(a8,f16.8)') "RTIME = ",time

 !CREATE THE MAIN HEADER
!  tkheader='I J Z S1'
!  do n=1,nstats
!   tkheader=trim(tkheader)//' '//trim(Sname(n))
!  enddo
!  write(501,'(a)') trim(tkheader)

!  do k=1,nzp2
!   plane=1
!   write(501,135) i,' ',k,' ',zc(k),' ',zc(k)/d_theta,' ',(GSTATS_current(k,n),n=1,nstats)
!  enddo
! close(501)
! write(IOUT,'(a18,f15.8)') "TIME_AVERAGE_TKSTATS WRITTEN AT: ",time

 endif
 
  !write out statistics header
  filen1 = statdir
  call concat(filen1,'tkstat_header')
  if (nstep < 1) then
   open(unit=510,file=filen1,status='unknown',form='formatted',iostat=s1 )
   do n=1,nstats
    write(510,140) n,sname(n)
   enddo
   close(510)
  endif

 endif !END MASTER ONLY

 if ( allocated(Ftemp) ) deallocate(Ftemp,stat=s1)
 if (s1.NE.0) then
  write(IOUT,*) "Error De-allocating Ftemp in stat1d"
  goto 1000
 endif


 120 FORMAT( (A) )
 135 FORMAT( ( 2(I3,a1), 2(E15.8,a1), 250(2X,E15.8E3) ) )
 140 FORMAT( I5,2X,A15 )

 1000 continue
 ok=max(s1,err1)
 return
end subroutine statistics_vshear_EDDY


subroutine statistics_vshear_small(ok)
!@t
! \textbf{subroutine statistics\_vshear\_small(ok)}
!@h
!   Description:
!     Calculate the small statistics for the vertical shear layer:
!     u,v,w,p,rho,u',v',w',p',rho'.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use ntypes, only: r8
 use Flow,   only: u,v,w,p,rho,scal1
 use domain, only: sz,ez,nzp2, sy, ey, nyp2
 use grid,   only: zc, ze, zL, dze
 use dd,     only: myid,rankx3,coords,comm3d
 use IO,     only: statdir,IOUT, tkstat, statbin
 use parameters, only: nstep,g
 implicit none

!Passed Variables
 integer :: ok

!Local Variables
 integer,parameter :: nstats=16
 integer  :: err1, j, k, s1, sloc
 real(r8) :: mean(sz-1:ez+1), rms(sz-1:ez+1), scalatmp
 real(r8) :: STATS(1:nzp2,1:nstats)
 character(len=25) :: Sname
 character(len=500) :: filen1

 err1=0
 s1=0
 STATS=0.d0
 !<U1>,<u1'> 
  call avgrmsX1X2(u,mean,rms,'u')
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(mean,STATS(:,1),0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(mean,0,err1)
  call MPI_BARRIER(comm3d,err1)
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(rms,STATS(:,2),0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(rms,0,err1)
  call MPI_BARRIER(comm3d,err1)

 !<U2>,<u2'> 
  call avgrmsX1X2(v,mean,rms,'v')
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(rms,STATS(:,4),0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(rms,0,err1)
  call MPI_BARRIER(comm3d,err1)
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(mean,STATS(:,3),0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(mean,0,err1)
  call MPI_BARRIER(comm3d,err1)

 !<U3>,<u3'> 
  call avgrmsX1X2(w,mean,rms,'w')
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)    call gather1d_statM(rms,STATS(:,6),0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)    call gather1d_statS(rms,0,err1)
  call MPI_BARRIER(comm3d,err1)
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(mean,STATS(:,5),0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(mean,0,err1)
  call MPI_BARRIER(comm3d,err1)

 !<P>,<p'> 
  call avgrmsX1X2(p,mean,rms,'cfluc')
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(rms,STATS(:,8),0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(rms,0,err1)
  call MPI_BARRIER(comm3d,err1)
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(mean,STATS(:,7),0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(mean,0,err1)
  call MPI_BARRIER(comm3d,err1)

 !<RHO>,<rho'> 
  call avgrmsX1X2(rho,mean,rms,'rf')
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(rms,STATS(:,10),0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(rms,0,err1)
  call MPI_BARRIER(comm3d,err1)
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(mean,STATS(:,9),0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(mean,0,err1)
  call MPI_BARRIER(comm3d,err1)

 !<SCAL1>,<scal1'> 
  call avgrmsX1X2(scal1,mean,rms,'sf')
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(rms,STATS(:,12),0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(rms,0,err1)
  call MPI_BARRIER(comm3d,err1)
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(mean,STATS(:,11),0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(mean,0,err1)
  call MPI_BARRIER(comm3d,err1)

if (myid.EQ.0) then
  !S^2
   !sloc 1=<U1>
   sloc = 13
   do k=sz,ez
    scalatmp = dze(k-1)/dze(k)*STATS(k+1,1)+(1.d0-dze(k-1)/dze(k))*STATS(k,1)
    STATS(k,sloc)=(scalatmp-STATS(k-1,1))/(2.d0*dze(k-1))
   enddo
   STATS(ez,sloc)=(STATS(ez+1,1)-STATS(ez,1))/(dze(ez))
   STATS(:,sloc) = STATS(:,sloc)**2.d0

  !Nr^2
   !sloc 9=<RHO>
   sloc=14
   do k=sz,ez
    scalatmp = dze(k-1)/dze(k)*STATS(k+1,9)+(1.d0-dze(k-1)/dze(k))*STATS(k,9)
    STATS(k,sloc)=(scalatmp-STATS(k-1,9))/(2.d0*dze(k-1))
   enddo
   STATS(ez,sloc)=(STATS(ez+1,9)-STATS(ez,9))/(dze(ez))
   STATS(:,sloc) = 9.81*1.7d-4*STATS(:,sloc)

  !Ns^2
   !sloc 11=<SCAL1>
   sloc=15
   do k=sz,ez
    scalatmp = dze(k-1)/dze(k)*STATS(k+1,11)+(1.d0-dze(k-1)/dze(k))*STATS(k,11)
    STATS(k,sloc)=(scalatmp-STATS(k-1,11))/(2.d0*dze(k-1))
   enddo
   STATS(ez,sloc)=(STATS(ez+1,11)-STATS(ez,11))/(dze(ez))
   STATS(:,sloc) = -9.81*7.6d-4*STATS(:,sloc)

  !Rig
   !sloc 13=S^2
   !sloc 14=Nr^2
   !sloc 15=Ns^2
   sloc=16
   STATS(:,sloc) = 0.d0
   scalatmp = 0.d0
   do k=sz,ez
    if (STATS(k,13)==0.d0) then
     scalatmp = 1.d-10
    else
     scalatmp = STATS(k,13)
    endif
    STATS(k,sloc)= (STATS(k,14)+STATS(k,15))/scalatmp
  enddo
 if (statbin) then

  filen1 = statDIR
  call concat(filen1,'vshear_')
  call concati(filen1,nstep)
  call concat(filen1,'.bin')

  open(unit=500,file=filen1,status='unknown',form='unformatted',iostat=s1)
   write(500) nyp2,nzp2
   write(500) zc
   write(500) Sname
   write(500) STATS
  close(unit=500)

  write(IOUT,'(a44,i6)') "SMALL VSHEAR STATISTICS (BINARY) WRITTEN AT: ",nstep
 endif

 if (tkstat) then
 filen1 = statDIR
  call concat(filen1,'vel_')
  call concati(filen1,nstep)
  open(unit=501,file=filen1,status='unknown',form='formatted')

   write(501,'(a)') "#VERTICAL SHEAR STATISTICS"
   write(501,'(a)') "#nzp2 Lz"
   write(501,'(a,i4,f12.4)') "#",nzp2, zL
   write(501,'(a)') "#zc(k),<U1>,<u1'>,<U2>,<u2'>,<U3>,<u3'>,<P>,<p'>,<RHO>,<rho'>,<SCAL1>,<scal1'>,S2,Nr2,Ns2,Rig"

   do k=1,nzp2
    write(501,130) zc(k),' ',STATS(k,1),' ',STATS(k,2),&
                         ' ',STATS(k,3),' ',STATS(k,4),&
                         ' ',STATS(k,5),' ',STATS(k,6),&
                         ' ',STATS(k,7),' ',STATS(k,8),&
                         ' ',STATS(k,9),' ',STATS(k,10),&
                         ' ',STATS(k,11),' ',STATS(k,12),&
                         ' ',STATS(k,13),' ',STATS(k,14),&
                         ' ',STATS(k,15),' ',STATS(k,16)
   enddo
  close(501)
  write(IOUT,'(a30,i6)') "SMALL VSHEAR STATS WRITTEN AT: ",nstep

 endif

 endif !END MASTER ONLY

 130  FORMAT( 17(1x,E16.8E3,a1) )
 ok=max(err1,s1)
 return
end subroutine statistics_vshear_small


subroutine gather1d_allStatM(nstat,varL,OutPencil,myidM,ok)
 use ntypes, only: r8
 use dd,     only: myid, commx3, sizex3, coords, nzprocs,&
                   MPI_STATUS_SIZE, inttype, realtype
 use Domain, only: sx,ex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none
                                                                                                                             
!Passed Variables
 integer,intent(in)                  :: myidM
 integer,intent(in)                  :: nstat 
 real(r8),intent(in)                 :: varL(sz-1:ez+1,1:nstat)
 real(r8),intent(out)                :: OutPencil(1:nzp2,1:nstat)
 integer,intent(out)                 :: ok
                                                                                                                             
!Local Variables
 integer                             :: Tsize, Rcoords(3), s1, status1(MPI_STATUS_SIZE),ierr
 integer                             :: k,n, kstart, k2, ks, ke
 real(r8),allocatable,dimension(:,:) :: Temp_Recv, Temp_Send
                                                                                                                             
 s1=0
 ierr=0
                                                                                                                             
 allocate( Temp_Recv(sz-1:ez+1,1:nstat), STAT=s1 )
 Tsize=size(Temp_recv)

 do n=0,sizex3-1 !1

  if (n.Eq.myidM) then
   Rcoords=coords
   Temp_Recv=varL
   OutPencil(1,:)=Temp_Recv(sz-1,:)
  else
   call MPI_RECV(Rcoords,3,inttype,n,2,commx3,status1,ierr)
   call MPI_RECV(Temp_Recv,Tsize,realtype,n,1,commx3,status1,ierr)
  endif
  !Determine Block of Data to recieve
  kstart = Rcoords(3)*(nzp2-2)/nzprocs
  !Determine if there is boundary data 
  ks=0
  ke=0
  if ( Rcoords(3).EQ.0      )   ks=1
  if ( Rcoords(3).EQ.sizex3-1 ) ke=1

  !UnPack Data
  do k=sz-ks,ez+ke
   k2=kstart+k
   OutPencil(k2,:)=Temp_Recv(k,:)
  enddo
 
 enddo !1

 deallocate(Temp_Send,STAT=s1)
 deallocate(Temp_Recv,STAT=s1)

 ok=max(ierr,s1)
 return
end subroutine gather1d_allstatM


subroutine gather1d_allstatS(nstat,varL,myidM,ok)
 use ntypes, only: r8
 use dd,     only: commx3, coords,MPI_STATUS_SIZE, inttype, realtype
 use domain, only: sz,ez
 implicit none
                                                                                                                             
!Passed Variables
 integer,intent(in)        :: myidM
 integer,intent(in)        :: nstat 
 real(r8),intent(in)       :: varL(sz-1:ez+1,1:nstat)
 integer,intent(out)       :: ok
                                                                                                                             
!Local Variables
 integer                            :: Tsize,ierr
                                                                                                                             
 ierr=0
 Tsize=size(varL)
 call MPI_SEND(coords,3,inttype,myidM,2,commx3,ierr)
 call MPI_SEND(varL,Tsize,realtype,myidM,1,commx3,ierr)
                                                                                                                             
                                                                                                                             
ok=ierr
return
end subroutine gather1d_allstatS

subroutine gather1d_StatM(varL,OutPencil,myidM,ok)
 use ntypes, only: r8
 use dd,     only: myid, commx3, sizex3, coords, nzprocs,&
                   MPI_STATUS_SIZE, inttype, realtype
 use Domain, only: sx,ex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none
                                                                                                                             
!Passed Variables
 integer,intent(in)                  :: myidM
 real(r8),intent(in)                 :: varL(sz-1:ez+1)
 real(r8),intent(out)                :: OutPencil(1:nzp2)
 integer,intent(out)                 :: ok
                                                                                                                             
!Local Variables
 integer                             :: Tsize, Rcoords(3), s1, status1(MPI_STATUS_SIZE),ierr
 integer                             :: k,n, kstart, k2, ks, ke
 real(r8),allocatable,dimension(:)   :: Temp_Recv, Temp_Send
                                                                                                                             
 s1=0
 ierr=0
                                                                                                                             
 allocate( Temp_Recv(sz-1:ez+1), STAT=s1 )
 Tsize=size(Temp_recv)

 do n=0,sizex3-1 !1

  if (n.Eq.myidM) then
   Rcoords=coords
   Temp_Recv=varL
   OutPencil(1)=Temp_Recv(sz-1)
  else
   call MPI_RECV(Rcoords,3,inttype,n,2,commx3,status1,ierr)
   call MPI_RECV(Temp_Recv,Tsize,realtype,n,1,commx3,status1,ierr)
  endif
  !Determine Block of Data to recieve
  kstart = Rcoords(3)*(nzp2-2)/nzprocs
  !Determine if there is boundary data 
  ks=0
  ke=0
  if ( Rcoords(3).EQ.0      )   ks=1
  if ( Rcoords(3).EQ.sizex3-1 ) ke=1

  !UnPack Data
  do k=sz-ks,ez+ke
   k2=kstart+k
   OutPencil(k2)=Temp_Recv(k)
  enddo
 
 enddo !1

 deallocate(Temp_Send,STAT=s1)
 deallocate(Temp_Recv,STAT=s1)

 ok=max(ierr,s1)
 return
end subroutine gather1d_statM


subroutine gather1d_statS(varL,myidM,ok)
 use ntypes, only: r8
 use dd,     only: commx3, coords,MPI_STATUS_SIZE, inttype, realtype
 use domain, only: sz,ez
 implicit none
                                                                                                                             
!Passed Variables
 integer,intent(in)        :: myidM
 real(r8),intent(in)       :: varL(sz-1:ez+1)
 integer,intent(out)       :: ok
                                                                                                                             
!Local Variables
 integer                            :: Tsize,ierr
                                                                                                                             
 ierr=0
 Tsize=size(varL)
 call MPI_SEND(coords,3,inttype,myidM,2,commx3,ierr)
 call MPI_SEND(varL,Tsize,realtype,myidM,1,commx3,ierr)
                                                                                                                             
                                                                                                                             
ok=ierr
return
end subroutine gather1d_statS


subroutine statistics_vshear_DNS(ok)
!@t
! \textbf{subroutine statistics\_vshear(ok)}
!@h
!   Description:
!     Calculate all statistics for the vertical shear layer.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!     1.0       08/2009  updated. [Hieu T. Pham] 

 use ntypes, only: r8 
 use IO,     only: IOUT, statDIR, statbin, tkstat
 use Flow,   only: u,v,w,p,rho,u1_tmp2,u2_tmp2,u3_tmp2,r_tmp1,r_tmp2,scal1,scal1_tmp2,Vmodel
 use LESmod
 use transient
 use domain, only: sx,ex,sy,ey,sz,ez,nzp2,nyp2
 use Spng,   only: x3spng
 use grid   
 use parameters, only: time,rRe,rSc,nstep, rho_0, g, rPr, szint, ezint, Texp, Scon 
 use dd,     only: myid,coords,rankx3,comm3d
 use ratios
 implicit none

!Passed Variables
 integer,intent(out) :: ok

!Local Variables
 integer,parameter :: nstats=250


 real(r8)          :: STATS(sz-1:ez+1,1:nstats)
 real(r8)          :: GSTATS(1:nzp2,1:nstats)
 character(len=25) :: Sname(1:nstats) 
 integer           :: stat

 integer  :: err1, i, j, k, s1 , sloc, n, plane, inttmp
 integer  :: rholoc(sz-1:ez+1),rank(sz-1:ez+1)
 real(r8) :: mean(sz-1:ez+1), rms(sz-1:ez+1), temp(sz-1:ez+1),input(sz-1:ez+1)
 real(r8) :: tempF(1:nzp2), d_theta, del_g
 character(len=150) :: filen1
 real(r8) :: scalatmp,maxerr
 character(len=2048) :: tkheader

 real(r8),allocatable,dimension(:,:,:) :: Ftemp
 logical,parameter             :: debug=.false.

 if (debug) call check_point('stat#0',.false.)

 allocate( Ftemp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 ) 
 if (s1.NE.0) then
  write(IOUT,*) "Error Allocating Ftemp in stat1d"
  goto 1000
 endif

 Sname="NA"

!INTERPOLATE VELOCITIES TO CELL CENTERS
  u1_tmp2 = 0.d0
  u2_tmp2 = 0.d0
  u3_tmp2 = 0.d0
  STATS = 0.d0
  call center_velocity(u,u1_tmp2,1 )
  call center_velocity(v,u2_tmp2,2 ) 
  call center_velocity(w,u3_tmp2,3 )
!******************************************************************
!********GROUP 1 -- MEAN, RMS, VERTICAL GRADIENT*******************
!******************************************************************
 !<U1>,<u1'> 
  sloc=1;Sname(sloc)="U1"
  sloc=8;Sname(sloc)="u1'"
  call avgrmsX1X2(u1_tmp2,STATS(:,1),STATS(:,8),'uc')

 !<U2>,<u2'> 
  sloc=2;Sname(sloc)="U2"
  sloc=9;Sname(sloc)="u2'"
  call avgrmsX1X2(u2_tmp2,STATS(:,2),STATS(:,9),'vc')

 !<U3>,<u3'> 
  sloc=3;Sname(sloc)="U3"
  sloc=10;Sname(sloc)="u3'"
  call avgrmsX1X2(u3_tmp2,STATS(:,3),STATS(:,10),'wc')

 !<P>,<p'> 
  sloc=4;Sname(sloc)="P"
  sloc=11;Sname(sloc)="p'"
  call avgrmsX1X2(p,STATS(:,4),STATS(:,11),'p')

 !<T>,<t'> 
  sloc=5;Sname(sloc)="T"
  sloc=12;Sname(sloc)="t'"
  call avgrmsX1X2(rho,STATS(:,5),STATS(:,12),'rf')

 !<S>,<s'> 
  sloc=6;Sname(sloc)="S"
  sloc=13;Sname(sloc)="s'"
  call avgrmsX1X2(scal1,STATS(:,6),STATS(:,13),'sf')

 !<RHO>,<rho'>
  r_tmp1 = -Texp*rho+Scon*scal1
  sloc=7;Sname(sloc)="RHO"
  sloc=14;Sname(sloc)="rho'"
  call avgrmsX1X2(r_tmp1,STATS(:,7),STATS(:,14),'cfluc')

 !d<U1>/dx3
  !sloc 1=U1
   sloc=15;Sname(sloc)="dU1dx3"
   do k=sz,ez
    STATS(k,sloc)=(STATS(k+1,1)-STATS(k-1,1))/(zc(k+1)-zc(k-1))
   enddo

 !d<U2>/dx3
  !sloc 2=U2
   sloc=16;Sname(sloc)="dU2dx3"
   do k=sz,ez
    STATS(k,sloc)=(STATS(k+1,2)-STATS(k-1,2))/(zc(k+1)-zc(k-1))
   enddo

 !d<T>/dx3
  !sloc 5=<T>
   sloc=17;Sname(sloc)="dTdx3"
   do k=sz,ez
    STATS(k,sloc)=(STATS(k+1,5)-STATS(k-1,5))/(zc(k+1)-zc(k-1))
   enddo

 !d<S>/dx3
  !sloc 6=<S>
   sloc=18;Sname(sloc)="dSdx3"
   do k=sz,ez
    STATS(k,sloc)=(STATS(k+1,6)-STATS(k-1,6))/(zc(k+1)-zc(k-1))
   enddo
 
 !d<RHO>/dx3
  !sloc 7=<RHO>
   sloc=19;Sname(sloc)="dRHOdx3"
   do k=sz,ez
    STATS(k,sloc)=(STATS(k+1,7)-STATS(k-1,7))/(zc(k+1)-zc(k-1))
   enddo

 !Nt2 
  !sloc 17=dTdx3 
   sloc=20
   Sname(sloc)="Nt2"
   STATS(:,sloc)=g/rho_0*Texp*STATS(:,17)

 !Ns2 
  !sloc 18=dSdx3 
   sloc=21
   Sname(sloc)="Ns2"
   STATS(:,sloc)=-g/rho_0*Scon*STATS(:,18)

 !N2 
  !sloc 20=Nt2; sloc 21=Ns2 
   sloc=22
   Sname(sloc)="N2"
   STATS(:,sloc)= STATS(:,20) + STATS(:,21)

 !invtanRig
  !sloc 15=dU1dx3; sloc 16=dU2dx3; sloc 22= N2
    sloc=23
    Sname(sloc)="invtanRig"
    STATS(:,sloc)= atan( STATS(:,22)/(STATS(:,15)**2.d0+STATS(:,16)**2.d0) )

  if (debug) call check_point('stat#1',.false.)

!******************************************************************
!*********GROUP 2 -- VORTICITY: MEAN, RMS, MAGNITUDE***************
!******************************************************************
 !<OMG1>,<omg1'> 
   call vorticity(u1_tmp2,r_tmp2,1)
   sloc=24;Sname(sloc)="OMG1"
   sloc=28;Sname(sloc)="omg1'"
   call avgrmsX1X2(u1_tmp2,STATS(:,24),STATS(:,28),'cfluc')

 !<OMG2>,<omg2'> 
   call vorticity(u2_tmp2,r_tmp2,2)
   sloc=25;Sname(sloc)="OMG2"
   sloc=29;Sname(sloc)="omg2'"
   call avgrmsX1X2(u2_tmp2,STATS(:,25),STATS(:,29),'cfluc')

 !<OMG3>,<omg3'> 
   call vorticity(u3_tmp2,r_tmp2,3)
   sloc=26;Sname(sloc)="OMG3"
   sloc=30;Sname(sloc)="omg3'"
   call avgrmsX1X2(u3_tmp2,STATS(:,26),STATS(:,30),'cfluc')

 !<OMG_MAG>,<omg_mag'>
    r_tmp1 = dsqrt( u1_tmp2**2+u2_tmp2**2+u3_tmp2**2 ) 
    sloc=27;Sname(sloc)="OMG_MAG"
    sloc=31;Sname(sloc)="omg_mag'"
   call avgrmsX1X2(r_tmp1,STATS(:,27),STATS(:,31),'cfluc')
    
  if (debug) call check_point('stat#2',.false.)

!******************************************************************
!*********GROUP 3 -- CROSS CORRELAIONS: DOUBLE, TRIPLE*************
!***Subtract mean from cell centered quantities and store in ******
!****************u1_tmp2, u2_tmp2, u3_tmp2 ************************
!******************************************************************
  u1_tmp2 = 0.d0
  u2_tmp2 = 0.d0
  u3_tmp2 = 0.d0
  call center_velocity(u,u1_tmp2,1 )
  call center_velocity(v,u2_tmp2,2 ) 
  call center_velocity(w,u3_tmp2,3 )

  !u'
   call avgX1X2(u1_tmp2,mean,'uc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u1_tmp2(i,j,k)=u1_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u1_tmp2,'cfluc',err1) 
 
  !v'
   call avgX1X2(u2_tmp2,mean,'vc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u2_tmp2(i,j,k)=u2_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u2_tmp2,'cfluc',err1)

  !w'  
   call avgX1X2(u3_tmp2,mean,'wc')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    u3_tmp2(i,j,k)=u3_tmp2(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(u3_tmp2,'cfluc',err1)

  !<u1'u2'>
   sloc=32;Sname(sloc)="u1'u2'"
   r_tmp1=u1_tmp2*u2_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u1'u3'>
   sloc=33;Sname(sloc)="u1'u3'"
   r_tmp1=u1_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2'u3'>
   sloc=34;Sname(sloc)="u2'u3'"
   r_tmp1 = u2_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !p'
   call avgX1X2(p,mean,'c')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      Ftemp(i,j,k) =p(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(r_tmp2,'cfluc',err1) 

  !<u1'p'>
   sloc=35;Sname(sloc)="u1'p'"
   r_tmp1=u1_tmp2*Ftemp
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2'p'>
   sloc=36;Sname(sloc)="u2'p'"
   r_tmp1 = u2_tmp2*Ftemp
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u3'p'>
   sloc=37;Sname(sloc)="u3'p'"
   r_tmp1 = u3_tmp2*Ftemp
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !T'
   call avgX1X2(rho,mean,'rf')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      r_tmp2(i,j,k)=rho(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(Ftemp,'cfluc',err1)

  !<u1'T'>
   sloc=38;Sname(sloc)="u1'T'"
   r_tmp1= u1_tmp2*r_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2'T'>
   sloc=39;Sname(sloc)="u2'T'"
   r_tmp1 = u2_tmp2*r_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u3'T'>
   sloc=40;Sname(sloc)="u3'T'"
   r_tmp1 = u3_tmp2*r_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !S'
   call avgX1X2(scal1,mean,'sf')
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      scal1_tmp2(i,j,k)=scal1(i,j,k)-mean(k)
     enddo
    enddo
   enddo
   call ghost(Ftemp,'cfluc',err1)

  !<u1'S'>
   sloc=41;Sname(sloc)="u1'S'"
   r_tmp1= u1_tmp2*scal1_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2'S'>
   sloc=42;Sname(sloc)="u2'S'"
   r_tmp1 = u2_tmp2*scal1_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u3'S'>
   sloc=43;Sname(sloc)="u3'S'"
   r_tmp1 = u3_tmp2*scal1_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u1'u1'u3'>
   sloc=44;Sname(sloc)="u1'u1'u3'"
   r_tmp1 = u1_tmp2*u1_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2'u2'u3'>
   sloc=45;Sname(sloc)="u2'u2'u3'"
   r_tmp1 = u2_tmp2*u2_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u3'u3'u3'>
   sloc=46;Sname(sloc)="u3'u3'u3'"
   r_tmp1 = u3_tmp2*u3_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u1'u2'u3'>
   sloc=47;Sname(sloc)="u1'u2'u3'"
   r_tmp1 = u1_tmp2*u2_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u1'u3'u3'>
   sloc=48;Sname(sloc)="u1'u3'u3'"
   r_tmp1 = u1_tmp2*u3_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2'u3'u3'>
   sloc=49;Sname(sloc)="u2'u3'u3'"
   r_tmp1 = u2_tmp2*u3_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<T'u1'u3'>
   sloc=50;Sname(sloc)="T'u1'u3'"
   r_tmp1 = r_tmp2*u1_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')
 
  !<T'u2'u3'>
   sloc=51;Sname(sloc)="T'u2'u3'"
   r_tmp1 = r_tmp2*u2_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<T'u3'u3'>
   sloc=52;Sname(sloc)="T'u3'u3'"
   r_tmp1 = r_tmp2*u3_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<T'T'u3'>
   sloc=53;Sname(sloc)="T'rho'u3'"
   r_tmp1 = r_tmp2*r_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<S'u1'u3'>
   sloc=54;Sname(sloc)="S'u1'u3'"
   r_tmp1 = scal1_tmp2*u1_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')
 
  !<S'u2'u3'>
   sloc=55;Sname(sloc)="S'u2'u3'"
   r_tmp1 = scal1_tmp2*u2_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<S'u3'u3'>
   sloc=56;Sname(sloc)="S'u3'u3'"
   r_tmp1 = scal1_tmp2*u3_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<S'S'u3'>
   sloc=57;Sname(sloc)="S'S'u3'"
   r_tmp1 = scal1_tmp2*scal1_tmp2*u3_tmp2
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u1's13'>
   !s'_13=1/2*(prl_u1'_prl_x3 + prl_u3'_prl_x1)
   sloc=58;Sname(sloc)="u1's13'"
   call deriv(u1_tmp2,r_tmp1,3)
   call deriv(u3_tmp2,Ftemp,1)
   r_tmp1=u1_tmp2*(0.5d0*(Ftemp+r_tmp1))
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u2's23'>
   !s'_23=1/2*(prl_u2'_prl_x3 + prl_u3'_prl_x2)
   sloc=59;Sname(sloc)="u2's23'"
   call deriv(u3_tmp2,Ftemp,2)
   call deriv(u2_tmp2,r_tmp1,3)
   r_tmp1=u2_tmp2*(r_1_2*(Ftemp+r_tmp1))
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !<u3's33'>
   !s'_33=(prl_u3'_prl_x3)
   sloc=60;Sname(sloc)="u3's33'"
   call deriv(u3_tmp2,r_tmp1,3)
   r_tmp1=u3_tmp2*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')


  if (debug) call check_point('stat#3',.false.)
!******************************************************************
!*******GROUP 4 -- TKE DISSIPATION -2*nu*<sij'sij'>****************
!**** u1_tmp2, u2_tmp2, u3_tmp2 store 3D fluctuating fields********
!******************************************************************
  !Dissipation Components!
  !epsilon11=-2*rRe*(prl_u1'_prl_x1)**2
   sloc=61;Sname(sloc)="eps11"
   call deriv(u1_tmp2,r_tmp1,1)
   r_tmp1=-2.d0*rRe*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon12=-rRe*( (prl_u1'_prl_x2)**2 + prl_u1'_prl_x2*prl_u2'_prl_x1 )
   sloc=62;Sname(sloc)="eps12"
   call deriv(u1_tmp2,r_tmp1,2)
   call deriv(u2_tmp2,Ftemp,1)
   Ftemp=r_tmp1*Ftemp 
   r_tmp1=-rRe*(r_tmp1*r_tmp1+Ftemp)
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon13=-rRe*( (prl_u1'_prl_x3)**2 + prl_u1'_prl_x3*prl_u3'_prl_x1 )
   sloc=63;Sname(sloc)="eps13"
   call deriv(u1_tmp2,r_tmp1,3)
   call deriv(u3_tmp2,Ftemp,1)
   Ftemp=r_tmp1*Ftemp
   r_tmp1=-rRe*(r_tmp1*r_tmp1+Ftemp)
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon21=-rRe*( (prl_u2'_prl_x1)**2 + prl_u2'_prl_x1*prl_u1'_prl_x2 )
   sloc=64;Sname(sloc)="eps21"
   call deriv(u2_tmp2,r_tmp1,1)
   call deriv(u1_tmp2,Ftemp,2)
   Ftemp=r_tmp1*Ftemp
   r_tmp1=-rRe*(r_tmp1*r_tmp1+Ftemp)
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon22=-2*rRe*(prl_u2'_prl_x2)**2
   sloc=65;Sname(sloc)="eps22"
   call deriv(u2_tmp2,r_tmp1,2)
   r_tmp1=-2.d0*rRe*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon23=-rRe*( (prl_u2'_prl_x3)**2 + prl_u2'_prl_x3*prl_u3'_prl_x2 )
   sloc=66;Sname(sloc)="eps23"
   call deriv(u2_tmp2,r_tmp1,3)
   call deriv(u3_tmp2,Ftemp,2)
   Ftemp=r_tmp1*Ftemp
   r_tmp1=-rRe*(r_tmp1*r_tmp1+Ftemp)
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon31=-rRe*( (prl_u3'_prl_x1)**2 + prl_u3'_prl_x1*prl_u1'_prl_x3)
   sloc=67;Sname(sloc)="eps31"
   call deriv(u3_tmp2,r_tmp1,1)
   call deriv(u1_tmp2,Ftemp,3)
   Ftemp=r_tmp1*Ftemp
   r_tmp1=-rRe*(r_tmp1*r_tmp1+Ftemp)
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon32=-rRe*( (prl_u3'_prl_x2)**2 + prl_u3'_prl_x3*prl_u2'_prl_x3)
   sloc=68;Sname(sloc)="eps32"
   call deriv(u3_tmp2,r_tmp1,2)
   call deriv(u2_tmp2,Ftemp,3)
   Ftemp=r_tmp1*Ftemp
   r_tmp1=-rRe*(r_tmp1*r_tmp1+Ftemp)
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !epsilon33=-2*rRe*(prl_u3'_prl_x3)**2
   sloc=69;Sname(sloc)="eps33"
   call deriv(u3_tmp2,r_tmp1,3)
   r_tmp1=-2.d0*rRe*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')


  if (debug) call check_point('stat#4',.false.)
!******************************************************************
!******************GROUP 5 -- SCALAR  DISSIPATION  ****************
!**** u1_tmp2, u2_tmp2, u3_tmp2 store vel fluctuating fields*******
!************r_tmp2, scal1_tmp2: T,S  fluctuating fields***********
!****************** r_tmp1, Ftemp: free arrays*********************
!******************************************************************
  !T'
   call avgX1X2(rho,mean,'rf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    r_tmp2(i,j,k)=rho(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(r_tmp2,'cfluc',err1)

  !Tepsilon_1=-2/Re/Pr*(prl_T'_prl_x1)**2
   sloc=70;Sname(sloc)="Teps1"
   call deriv(r_tmp2,r_tmp1,1)
   r_tmp1=-2.d0*rRe*rPr*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !Tepsilon_2=-2/Re/Pr*(prl_T'_prl_x2)**2
   sloc=71;Sname(sloc)="Teps2"
   call deriv(r_tmp2,r_tmp1,2)
   r_tmp1=-2.d0*rRe*rPr*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !Tepsilon_3=-2/Re/Pr(prl_T'_prl_x3)**2
   sloc=72;Sname(sloc)="Teps3"
   call deriv(r_tmp2,r_tmp1,3)
   r_tmp1=-2.d0*rRe*rPr*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !S'
   call avgX1X2(scal1,mean,'rf')
   do k=sz,ez; do j=sy,ey; do i=sx,ex
    scal1_tmp2(i,j,k)=scal1(i,j,k)-mean(k)
   enddo; enddo; enddo
   call ghost(scal1_tmp2,'cfluc',err1)

  !Sepsilon_1=-2/Re/Pr*(prl_S'_prl_x1)**2
   sloc=73;Sname(sloc)="Seps1"
   call deriv(scal1_tmp2,r_tmp1,1)
   r_tmp1=-2.d0*rRe*rSc*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !Sepsilon_2=-2/Re/Pr*(prl_S'_prl_x2)**2
   sloc=74;Sname(sloc)="Seps2"
   call deriv(scal1_tmp2,r_tmp1,2)
   r_tmp1=-2.d0*rRe*rSc*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  !Sepsilon_3=-2/Re/Pr(prl_S'_prl_x3)**2
   sloc=75;Sname(sloc)="Seps3"
   call deriv(scal1_tmp2,r_tmp1,3)
   r_tmp1=-2.d0*rRe*rSc*r_tmp1*r_tmp1
   call avgX1X2(r_tmp1,STATS(:,sloc),'cfluc')

  if (debug) call check_point('stat#5',.false.)

!**********************************************************************
!***************GROUP 6 -- LES SUBGRID*********************************
!**********compute all stats at cell center****************************
!**********************************************************************
 !<Csgs>
  sloc=76; Sname(sloc)="Csgs"
 
 !Nusgs 
  sloc=77; Sname(sloc)="Nusgs"

 !<CTsgs>
  sloc=78; Sname(sloc)="CTsgs"

 !Kappasgs 
  sloc=79; Sname(sloc)="Kappasgs"

 !<CSsgs>
  sloc=80; Sname(sloc)="CSsgs"

 !Nappasgs 
  sloc=81; Sname(sloc)="Nappasgs"

 !<tau11>,<tau11'>
  sloc=82; Sname(sloc)="TAU11"
  sloc=88; Sname(sloc)="tau11'"
  
 !<tau22>,<tau22'>
  sloc=83; Sname(sloc)="TAU22"
  sloc=89; Sname(sloc)="tau22'"

 !<tau33>,<tau33'>
  sloc=84; Sname(sloc)="TAU33"
  sloc=90; Sname(sloc)="tau33'"
   
 !<tau12>,<tau12'>
  sloc=85; Sname(sloc)="TAU12"
  sloc=91; Sname(sloc)="tau12'"

 !<tau13>,<tau13'>
  sloc=86; Sname(sloc)="TAU13"
  sloc=92; Sname(sloc)="tau13'"

 !<tau23>,<tau23'>
  sloc=87; Sname(sloc)="TAU23"
  sloc=93; Sname(sloc)="tau23'"


 !Subgrid dissipation
 !Esgs11=-tau11'*du1'/dx1
  sloc=94; Sname(sloc)="Esgs11"

 !Esgs12=-tau12'*du1'/dx2
  sloc=95; Sname(sloc)="Esgs12"

 !Esgs13=-tau13'*du1'/dx3
  sloc=96; Sname(sloc)="Esgs13"

 !Esgs21=-tau21'*du2'/dx1
  sloc=97; Sname(sloc)="Esgs21"

 !Esgs22=-tau22'*du2'/dx2
  sloc=98; Sname(sloc)="Esgs22"

 !Esgs23=-tau23'*du2'/dx3
  sloc=99; Sname(sloc)="Esgs23"

 !Esgs31=-tau31'*du3'/dx1
  sloc=100; Sname(sloc)="Esgs31"

 !Esgs32=-tau32'*du3'/dx2
  sloc=101; Sname(sloc)="Esgs32"

 !Esgs33=-tau33'*du3'/dx3
  sloc=102; Sname(sloc)="Esgs33"

 !Subgrid transport
 !Tsgs131=tau13'*u1'
  sloc=103; Sname(sloc)="tau13'u1'"

 !Tsgs232=tau23'*u2'
  sloc=104; Sname(sloc)="tau23'u2'"
 
 !Tsgs333=tau33'*u3'
  sloc=105; Sname(sloc)="tau33'u3'"

 !<QT1>,<qt1'> 
  sloc=106; Sname(sloc)="QT1"
  sloc=109; Sname(sloc)="qt1'"

!<QT2>,<qt2'> 
  sloc=107; Sname(sloc)="QT2"
  sloc=110; Sname(sloc)="qt2'"

!<QT3>,<qt3'> 
  sloc=108; Sname(sloc)="QT3"
  sloc=111; Sname(sloc)="qt3'"

 !ET1sgs=-2*<qt1'*dt'dx1>
  sloc=112; Sname(sloc)="ET1sgs"

 !ET2sgs=-2*<qt2'*dt'dx2>
  sloc=113; Sname(sloc)="ET2sgs"

 !ET3sgs=-2*<qt3'*dt'dx3>
  sloc=114; Sname(sloc)="ET3sgs"

 !TTsgs=<t'qt3'>
  sloc=115; Sname(sloc)="qt3't'"

 !<QS1>,<qs1'> 
  sloc=116; Sname(sloc)="QS1"
  sloc=119; Sname(sloc)="qs1'"

!<QS2>,<qs2'> 
  sloc=117; Sname(sloc)="QS2"
  sloc=120; Sname(sloc)="qs2'"

!<QS3>,<qs3'> 
  sloc=118; Sname(sloc)="QS3"
  sloc=121; Sname(sloc)="qs3'"

 !ES1sgs=-2*<qs1'*ds'dx1>
  sloc=122; Sname(sloc)="ES1sgs"

 !ES2sgs=-2*<qs2'*ds'dx2>
  sloc=123; Sname(sloc)="ET2sgs"

 !ES3sgs=-2*<qs3'*ds'dx3>
  sloc=124; Sname(sloc)="ET3sgs"

 !TSsgs=<qs3's'>
  sloc=125; Sname(sloc)="TSsgs"

  if (debug) call check_point('stat#6',.false.)

!**********************************************************************
!********************GROUP 7 -- TKE BUDGET****************************
!**********************************************************************
  !TKE=1/2<u1'u'1+u2'u2'+u3'u3'>
   !sloc 8=<u1'>; sloc 9=<u2'>; sloc 10=<u3'>
   sloc=126; Sname(sloc)="TKE" 
   STATS(:,sloc)=0.5d0*(STATS(:,8)**2+STATS(:,9)**2+STATS(:,10)**2)

  !dTKE/dt
   sloc=127; Sname(sloc)="dkdt"
   STATS(:,sloc)=dkdt(:)

  !PROD=-<u1'u3'><dU1dx3> - <u2'u3'><dU2dx3>
   !sloc 33=<u1'u3'>; sloc 15=d<U1>/dx3
   !sloc 34=<u2'u3'>; sloc 16=d<U2>/dx3
   sloc=128; Sname(sloc)="Prod"
   STATS(:,sloc)=-STATS(:,33)*STATS(:,15) &
                 -STATS(:,34)*STATS(:,16) 

  !DISS 
   !sloc 61-69 = eps_ij
   sloc=129; Sname(sloc)="Diss"
   do i=61,69
    STATS(:,sloc)=STATS(:,sloc)+STATS(:,i)
   enddo

  !BFLUXT=g/rho_0*Texp*<t'u3'>
   !sloc 40=<t'u3'>
   sloc=130; Sname(sloc)="BfluxT"
   STATS(:,sloc)=g/rho_0*Texp*STATS(:,40)

  !BFLUXS=g/rho_0*Scon*<s'u3'>
   !sloc 44=<s'u3'>
   sloc=131; Sname(sloc)="BfluxS"
   STATS(:,sloc)=-g/rho_0*Scon*STATS(:,43)
  
  !dTRANS=dT3/dx3 
   !T3=1/2*<ui'ui'u3'>+<p'u3'>/rho_0-2*rRe*<ui'si3'>
   !sloc 44,45,46 = <ui'ui'u3'>
   !sloc 37 = <p'u3'>
   !sloc 58,59,60=<ui'si3'>
   sloc=132; Sname(sloc)="dTrans"
   temp(:)=0.5d0*(STATS(:,44)+STATS(:,45)+STATS(:,46)) &
                +STATS(:,37)/rho_0 &
                -2.d0*rRe*(STATS(:,58)+STATS(:,59)+STATS(:,60))
   do k=sz,ez
    STATS(k,sloc)=(temp(k+1)-temp(k-1))/(zc(k+1)-zc(k-1))
   enddo     

  !subgrid Dissipation
   !sloc  94-102 = Esgs_ij (9 components)
   sloc=133; Sname(sloc)="Disssgs"
   do i=94,102
    STATS(:,sloc)=-STATS(:,sloc)-STATS(:,i)
   enddo

  !subgrid Transport = d/dt (<tau13'u1'+tau23'u2'+tau33'u3')
   !sloc 103=<tau13'u1'>
   !sloc 104=<tau23'u2'>
   !sloc 105=<tau33'u3'>
   sloc=134 
   Sname(sloc)="dTsgs"
   temp= STATS(:,103)+STATS(:,104)+STATS(:,105)
   do k=sz,ez
    STATS(k,sloc)=(temp(k+1)-temp(k-1))/(zc(k+1)-zc(k-1))
   enddo    

  if (debug) call check_point('stat#7',.false.)

!******************************************************************
!*******************GROUP 8 -- T'2 BUDGET*************************
!******************************************************************
  !dTdt 
   sloc=135; Sname(sloc)="dt'2dt"
   STATS(:,sloc)=dt2dt

  !TPROD=-2<t'u3'>d<T>/dx3
   !sloc 40=<t'u3'>; sloc 17=d<T>/dx3
   sloc=136; Sname(sloc)="TProd"
   STATS(:,sloc)=-2.d0*STATS(:,40)*STATS(:,17)

  !TDISS
   !sloc 70,71,72 = Teps1,Teps2,Teps3
   sloc=137; Sname(sloc)="TDiss"
   STATS(:,sloc) = STATS(:,70)+STATS(:,71)+STATS(:,72)

  !TdTRANS=d(TR3)/dx3
   !TR3 = <t't'u3'>-rRe*rPr*d<t't'>/dx3
   !sloc 53=<t't'u3'>
   !sloc 12=<t'>
   sloc=138;Sname(sloc)="TdTrans"
   do k=sz,ez
    STATS(k,sloc) = (STATS(k+1,53)-STATS(k-1,53))/(zc(k+1)-zc(k-1)) &
               -rRe*rPr*( (STATS(k+1,12)**2.d0-STATS(k,12)**2.d0)*rdzc(k+1)& 
                         -(STATS(k,12)**2.d0-STATS(k-1,12)**2.d0)*rdzc(k) )*rdze(k)
   enddo
   
  !TEsgs
   !sloc 112,113,114 = ET1sgs,ET2sgs,ET3sgs
   sloc=139; Sname(sloc)="TEsgs"
   STATS(:,sloc) = -STATS(:,112)-STATS(:,113)-STATS(:,114)

  !TdTsgs
   !sloc 115 = qt3't'
   sloc=140; Sname(sloc)="TdTsgs"
   do k=sz,ez
    STATS(k,sloc)=(STATS(k+1,115)-STATS(k-1,115))/(zc(k+1)-zc(k-1))
   enddo
  
  !TPET = -0.5*g*Texp/dTx3*<t't'>
   !sloc 12=<t'>; sloc 17 = <dTdx3>
   sloc=141; Sname(sloc)="TPET"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*STATS(k,12)**2.d0
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   
   
  !dTPETdt=-0.5*g*Texp/dTdx3*dt2'dt 
   sloc=142; Sname(sloc)="dTPETdt"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*dt2dt(k)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   
   
  !TPETPROD=-2<t'u3'>d<T>/dx3
   !sloc 136=TProd
   sloc=143; Sname(sloc)="TPETProd"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*STATS(k,136)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  !TPETDISS
   !sloc 137="TDiss"
   sloc=144; Sname(sloc)="TPETDiss"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*STATS(k,137)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  !TPETdTRANS=d(TR3)/dx3
   !sloc 138 = TdTrans
   sloc=145;Sname(sloc)="TPETdTrans"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*STATS(k,138)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   
   
  !TPETEsgs
   !sloc 139=TEsgs
   sloc=146; Sname(sloc)="TPETEsgs"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*STATS(k,139)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  !TPETdTsgs
   !sloc 140=TdTsgs
   sloc=147; Sname(sloc)="TPETdTsgs"
   do k=sz-1,ez+1
    if (abs(STATS(k,17)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Texp/STATS(k,17)*STATS(k,140)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  if (debug) call check_point('stat#8',.false.)

!******************************************************************
!*******************GROUP 9 -- S'2 BUDGET**************************
!******************************************************************
  !ds'2dt 
   sloc=148; Sname(sloc)="ds'2dt"
   STATS(:,sloc)=ds2dt

  !SProd=-2<s'u3'>d<S>/dx3
   !sloc 43=<s'u3'>; sloc 18=d<T>/dx3
   sloc=149; Sname(sloc)="SProd"
   STATS(:,sloc)=-2.d0*STATS(:,43)*STATS(:,18)

  !SDiss
   !sloc 73,74,75 = Seps1,Seps2,Seps3
   sloc=150; Sname(sloc)="SDiss"
   STATS(:,sloc) = STATS(:,73)+STATS(:,74)+STATS(:,75)

  !SdTRANS=d(SR3)/dx3
   !SR3 = <s's'u3'>-rRe*rPr*d<s's'>/dx3
   !sloc 57=<s's'u3'>; sloc 13=<s'>
   sloc=151;Sname(sloc)="SdTrans"
   do k=sz,ez
    STATS(k,sloc) = (STATS(k+1,57)-STATS(k-1,57))/(zc(k+1)-zc(k-1)) &
               -rRe*rSc*( (STATS(k+1,13)**2.d0-STATS(k,13)**2.d0)*rdzc(k+1) &
                         -(STATS(k,13)**2.d0-STATS(k-1,13)**2.d0)*rdzc(k) )*rdze(k)
   enddo
   
  !SEsgs
   !sloc 122,123,124 = ES1sgs,ES2sgs,ES3sgs
   sloc=152; Sname(sloc)="SEsgs"
   STATS(:,sloc) = -STATS(:,122)-STATS(:,123)-STATS(:,124)

  !SdTsgs
   !sloc 125 = qs3't'
   sloc=153; Sname(sloc)="SdTsgs"
   do k=sz,ez
    STATS(k,sloc)=(STATS(k+1,125)-STATS(k-1,125))/(zc(k+1)-zc(k-1))
   enddo
  
  !TPES = -0.5*g*Scon/dSx3*<s's'>
   !sloc 13=<s'>; sloc 18 = <dSdx3>
   sloc=154; Sname(sloc)="TPES"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*STATS(k,13)**2.d0
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   
   
  !dTPESdt=-0.5*g*Scon/dSdx3*dt2'dt 
   sloc=155; Sname(sloc)="dTPESdt"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*ds2dt(k)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   
   
  !TPESPROD=-2<s'u3'>d<S>/dx3
   !sloc 149=SProd
   sloc=156; Sname(sloc)="TPESProd"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*STATS(k,149)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  !TPESDISS
   !sloc 150="SDiss"
   sloc=157; Sname(sloc)="TPESDiss"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*STATS(k,150)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  !TPESdTRANS=d(TR3)/dx3
   !sloc 151 = TdTrans
   sloc=158;Sname(sloc)="TPESdTrans"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*STATS(k,151)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   
   
  !TPESEsgs
   !sloc 152=TEsgs
   sloc=159; Sname(sloc)="TPESEsgs"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*STATS(k,152)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  !TPESdTsgs
   !sloc 153=TdTsgs
   sloc=160; Sname(sloc)="TPESdTsgs"
   do k=sz-1,ez+1
    if (abs(STATS(k,18)).GT.1d-16) then
     STATS(k,sloc)=-0.5*g*Scon/STATS(k,18)*STATS(k,153)
    else
     STATS(k,sloc)=0.0d0
    endif
   enddo   

  if (debug) call check_point('stat#9',.false.)

!******************************************************************
!*******************GROUP 10 -- MKE BUDGET*************************
!******************************************************************
 !MKE = 0.5(<u>^2+<v>^2+<w>^2)
  !sloc 1=<u>; sloc 2=<v>; sloc 3=<w>
  sloc=161; Sname(sloc)="MKE"
  STATS(:,sloc)=0.5d0*(STATS(:,1)**2.d0+STATS(:,2)**2.d0+STATS(:,3)**2.d0)

 !dMKEdt 
  sloc=162; Sname(sloc)="dMKEdt"
  STATS(:,sloc)= dMKEdt(:)
  
 !MProd = -Prod
  !sloc 128 = Prod
  sloc=163; Sname(sloc)="MProd"
  STATS(:,sloc) = -STATS(:,128)

 !MDiss = rRe*(dU1dx3^2+dU2dx3^2)
  !sloc 15=dU1dx3; sloc 16=dU2dx3
  sloc=164; Sname(sloc)="MDiss"
  STATS(:,sloc) = rRe*( STATS(:,15)**2.d0 + STATS(:,16)**2.d0)

 !MdTrans
  !MTrans = <U1><u1'u3'>+<U2><u2'u3'> - rRe (<U1>dU1dx3 + <V>dU2dx3)
  !sloc 1=<U1>; sloc 2=<U2>; sloc 15=dU1dx3; sloc 16=dU2dx3; 
  !sloc 33=<u1'u3'>; sloc 34=<u2'u3'>
  sloc=165; Sname(sloc)="MdTrans"
  do k=sz,ez
   STATS(k,sloc)=(STATS(k+1,1)*STATS(k+1,33)-STATS(k-1,1)*STATS(k-1,33))/(zc(k+1)-zc(k-1)) &
                +(STATS(k+1,2)*STATS(k+1,34)-STATS(k-1,2)*STATS(k-1,34))/(zc(k+1)-zc(k-1))&
                -rRe*(STATS(k,1)*STATS(k,15)+STATS(k,2)*STATS(k,15))  
  enddo      

 !MPsgs = <TAU13>dU1dx3 + <TAU23>dU2dx3
  !sloc 86=TAU13; sloc 87=TAU23
  !sloc 15=dU1dx3; sloc 16=dU2dx3
  sloc=166; Sname(sloc)="MPsgs"
  STATS(:,sloc)=STATS(:,86)*STATS(:,15)+STATS(:,87)*STATS(:,16)
 
 !MdTsgs 
  !MTsgs = <U1><TAU13> + <U2><TAU23>
  !sloc 1=<U1>; sloc 2=<U2>;
  !sloc 86=TAU13; sloc 87=TAU23
  sloc=167; Sname(sloc)="MdTsgs"
  do k=sz,ez
   STATS(k,sloc)=(STATS(k+1,1)*STATS(k+1,86)-STATS(k-1,1)*STATS(k-1,86))/(zc(k+1)-zc(k-1)) &
                +(STATS(k+1,2)*STATS(k+1,87)-STATS(k-1,2)*STATS(k-1,87))/(zc(k+1)-zc(k-1))
  enddo


  if (debug) call check_point('stat#10',.false.)

!******************************************************************
!*******************GROUP 11 -- MEAN BUDGET********************
!******************************************************************
 !dU1dt
  sloc=168; Sname(sloc)="dU1dt"
  STATS(:,sloc) = dU1dt
  
 !U1tflux = -d/dx3(<u1'u3'>)
  !sloc 33= <u1'u3'>
  sloc=169; Sname(sloc)="U1tflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,33)-STATS(k-1,33))/(zc(k+1)-zc(k-1))
  enddo 
 
 !U1vflux = rRe*d2/dz2 (<U1>)
  !sloc 1=<U1>
  sloc=170; Sname(sloc)="U1vflux"
  do k=sz,ez
   STATS(k,sloc)=rRe*( (STATS(k+1,1)-STATS(k,1))*rdzc(k+1) &
                      -(STATS(k,1)-STATS(k-1,1))*rdzc(k) )*rdze(k)
  enddo 

 !U1sgsflux = -d/dz(<TAU13>)
  !sloc 86=<TAU13>
  sloc=171; Sname(sloc)="U1sgsflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,86)-STATS(k-1,86))/(zc(k+1)-zc(k-1))
  enddo 
 
 !dU2dt
  sloc=172; Sname(sloc)="dU2dt"
  STATS(:,sloc) = dU2dt
  
 !U2tflux = -d/dx3(<u2'u3'>)
  !sloc 34= <u2'u3'>
  sloc=173; Sname(sloc)="U2tflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,34)-STATS(k-1,34))/(zc(k+1)-zc(k-1))
  enddo 
 
 !U2vflux = rRe*d2/dz2 (<U2>)
  !sloc 2=<U2>
  sloc=174; Sname(sloc)="U2vflux"
  do k=sz,ez
   STATS(k,sloc)=rRe*( (STATS(k+1,2)-STATS(k,2))*rdzc(k+1) &
                      -(STATS(k,2)-STATS(k-1,2))*rdzc(k) )*rdze(k)
  enddo 

 !U2sgsflux = -d/dz(<TAU13>)
  !sloc 87=<TAU23>
  sloc=175; Sname(sloc)="U2sgsflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,87)-STATS(k-1,87))/(zc(k+1)-zc(k-1))
  enddo 

 !dTdt
  sloc=176; Sname(sloc)="dTdt"
  STATS(:,sloc) = dTdt
  
 !Ttflux = -d/dx3(<t'u3'>)
  !sloc 40= <t'u3'>
  sloc=177; Sname(sloc)="Ttflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,40)-STATS(k-1,40))/(zc(k+1)-zc(k-1))
  enddo 
 
 !Tvflux = rRe*rPr*d2/dz2 (<T>)
  !sloc 5=<T>
  sloc=178; Sname(sloc)="Tvflux"
  do k=sz,ez
   STATS(k,sloc)=rRe*rPr*( (STATS(k+1,5)-STATS(k,5))*rdzc(k+1) &
                          -(STATS(k,5)-STATS(k-1,5))*rdzc(k) )*rdze(k)
  enddo 

 !Tsgsflux = -d/dz(<QT3>)
  !sloc 108=<QT3>
  sloc=179; Sname(sloc)="Tsgsflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,108)-STATS(k-1,108))/(zc(k+1)-zc(k-1))
  enddo 

 !dSdt
  sloc=180; Sname(sloc)="dSdt"
  STATS(:,sloc) = dSdt
  
 !Stflux = -d/dx3(<s'u3'>)
  !sloc 43= <s'u3'>
  sloc=181; Sname(sloc)="Stflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,43)-STATS(k-1,43))/(zc(k+1)-zc(k-1))
  enddo 
 
 !Svflux = rRe*rSc*d2/dz2 (<S>)
  !sloc 6=<S>
  sloc=182; Sname(sloc)="Svflux"
  do k=sz,ez
   STATS(k,sloc)=rRe*rSc*( (STATS(k+1,6)-STATS(k,6))*rdzc(k+1) &
                          -(STATS(k,6)-STATS(k-1,6))*rdzc(k) )*rdze(k)
  enddo 

 !Ssgsflux = -d/dz(<QS3>)
  !sloc 118=<QS3>
  sloc=183; Sname(sloc)="Tsgsflux"
  do k=sz,ez
   STATS(k,sloc)= -(STATS(k+1,118)-STATS(k-1,118))/(zc(k+1)-zc(k-1))
  enddo 


!Assemble global statistics to masternode
  call MPI_BARRIER(comm3d,err1)
!  do k=1,nstats
!   if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statM(STATS(:,k),GSTATS(:,k),0,err1)
!   if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_statS(STATS(:,k),0,err1)
!   call MPI_BARRIER(comm3d,err1)
!  enddo
  if (rankx3.EQ.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_allstatM(nstats,STATS,GSTATS,0,err1)
  if (rankx3.NE.0.AND.coords(1).EQ.0.AND.coords(2).EQ.0)  call gather1d_allstatS(nstats,STATS,0,err1)
  call MPI_BARRIER(comm3d,err1)

  if (debug) call check_point('stat#11',.false.)

 if (myid.EQ.0) then  !BEGIN MASTER NODE
!******************************************************************
!****************GROUP 12 -- LENGTH SCALES*************************
!******************************************************************
  !domega
   !sloc 15=dU1dx3 
   sloc=184; Sname(sloc)="d_omega"
   scalatmp=0.d0
   do k=1,nzp2 
    if (dabs(GSTATS(k,15)).GT.scalatmp) scalatmp = abs( GSTATS(k,15) )
   enddo
   GSTATS(:,sloc) = 1.d0 / scalatmp

  !dtheta
   !sloc 1=<U1>
   sloc=185; Sname(sloc)="d_theta"
   scalatmp=0.d0
   do k=szint,ezint
    scalatmp = scalatmp + ( 0.5d0 - GSTATS(k+1,1)**2.d0 - GSTATS(k,1)**2.d0)*dzc(k)*0.5d0
   enddo
   GSTATS(:,sloc) = scalatmp
   d_theta = scalatmp

  !d_omega_T
   !1/(dTdx3)_max
   !sloc 17=dTdx3
   sloc=186;Sname(sloc)="d_omega_T"
   scalatmp = 0.d0
   do k=1,nzp2
    if (dabs(GSTATS(k,17)).GT.scalatmp) scalatmp = dabs( GSTATS(k,17))
   enddo
   GSTATS(:,sloc) = 1.d0 / scalatmp

  !d_omega_S
   !1/(dSdx3)_max
   !sloc 18=dSdx3
   sloc=187; Sname(sloc)="d_omega_S"
   scalatmp = 0.d0
   do k=1,nzp2
    if (dabs(GSTATS(k,18)).GT.scalatmp) scalatmp = dabs( GSTATS(k,18))
   enddo
   GSTATS(:,sloc) = 1.d0 / scalatmp

  !Reb = (Diss + Disssgs)/(nu N2)
   !sloc 129=Diss; sloc 133=Disssgs
   !sloc 22=N2
   sloc=188; Sname(sloc)="Reb"
   do k=1,nzp2
    if (GSTATS(k,22).GT.1d-16) then
     GSTATS(k,sloc)=(abs(GSTATS(k,129))+abs(GSTATS(k,133)))/(rRe*STATS(k,22))
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo
    
  !Ozmidov Scale
   !((Diss+Dissgs)/<N>^3)^(1/2)
   !sloc 129=Diss; sloc 133=Disssgs  (minus)
   !sloc 22=N^2 
   sloc=189; Sname(sloc)="Lozmo"
   do k=1,nzp2
    if (GSTATS(k,22).GT.1d-16) then
     GSTATS(k,sloc)=((abs(GSTATS(k,129))+abs(GSTATS(k,133)))/STATS(k,22)**3.d0)**0.5d0
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo

  !Ellison scale LellisonT =<t'>/<dTdx3>
   !sloc 12=<t'>; sloc 17=<dTdx3>
   sloc=190; Sname(sloc)="LellisT"
   do k=1,nzp2
    if (GSTATS(k,17).GT.1d-16) then
     GSTATS(:,sloc) = GSTATS(:,12)/STATS(:,17)
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo

  !Ellison scale LellisonS =<s'>/<dSdx3>
   !sloc 13=<s'>; sloc 18=<dTdx3>
   sloc=191; Sname(sloc)="LellisS"
   do k=1,nzp2
    if (GSTATS(k,18).GT.1d-16) then
     GSTATS(:,sloc) = GSTATS(:,13)/STATS(:,18)
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo

  !eta=(nu^3/(Diss+Dissgs))^(1/4) 
   !sloc 129=Diss; sloc 133=Disssgs  (minus)
   sloc=192; Sname(sloc)="Lkolmo"
   do k=1,nzp2
    scalatmp = abs(GSTATS(k,129))+abs(GSTATS(k,133))
    if (scalatmp.GT.1d-16) then
     GSTATS(k,sloc)=(rRe**3.d0/scalatmp)**0.25d0
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo

  !etaT=(kappaT^3/(TPETDiss+TPETDissgs))^(1/4) 
   !sloc 144=TPETDiss; sloc 146=TPETEsgs  (minus)
   sloc=193; Sname(sloc)="LkolmoT"
   do k=1,nzp2
    scalatmp = abs(GSTATS(k,144))+abs(GSTATS(k,146))
    if (scalatmp.GT.1d-16) then
     GSTATS(k,sloc)=((rRe*rPr)**3.d0/scalatmp)**0.25d0
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo

  !etaS=(nu^3/(TPESDiss+TPESDissgs))^(1/4) 
   !sloc 157=TPESDiss; sloc 159=TPESEsgs  (minus)
   sloc=194; Sname(sloc)="LkolmoS"
   do k=1,nzp2
    scalatmp = abs(GSTATS(k,157))+abs(GSTATS(k,159))
    if (scalatmp.GT.1d-16) then
     GSTATS(k,sloc)=((rRe*rSc)**3.d0/scalatmp)**0.25d0
    else
     GSTATS(k,sloc)=0.d0
    endif
   enddo

  !Energy containing scale: Len = (2/3*TKE)^(3/2)/(Diss+Disssgs)
   !sloc 126 = TKE
   !sloc 129=Diss; sloc 133=Disssgs  (minus)
   sloc=195; Sname(sloc)="Len"
   GSTATS(:,sloc)=(2.d0/3.d0*GSTATS(:,126))**(3.d0/2.d0)/(abs(GSTATS(:,129))+abs(GSTATS(:,133)))

  !Buoyancy scale: Lbuoy=<w'>/N
   !sloc 10=<w'>; sloc 22=N^2
   sloc=196;Sname(sloc)="Lbuoy"
   GSTATS(:,sloc)=GSTATS(:,10)/GSTATS(:,22)**0.5d0

!**********************************************************************
!********************GROUP 13 -- intTKEBUDGET**************************
!**********************************************************************
  !intTKE
   !sloc 126=TKE 
   sloc = 197; Sname(sloc)="intTKE"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,126)+GSTATS(k,126))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intdTKE/dt
   !sloc 127=dTKEdt
   sloc=198; Sname(sloc)="intdTKEdt"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,127)+GSTATS(k,127))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intProd
   !sloc 128=Prod
   sloc=199; Sname(sloc)="intProd"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,128)+GSTATS(k,128))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intDISS
   !sloc 129=Diss
   sloc=200; Sname(sloc)="intDiss"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,129)+GSTATS(k,129))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intBfluxT
   !sloc 130=BfluxT
   sloc=201; Sname(sloc)="intBfluxT"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,130)+GSTATS(k,130))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intBfluxS
   !sloc 131=BfluxS
   sloc=202; Sname(sloc)="intBfluxS"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,131)+GSTATS(k,131))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intdTrans 
   !sloc 132= dTrans 
   sloc=203; Sname(sloc)="intdTrans"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,132)+GSTATS(k,132))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !Subgrid Diss
   !sloc 133 = Disssgs
   sloc=204; Sname(sloc)="intDisssgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,133)+GSTATS(k,133))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !Subgrid Trans
   !sloc 134 = dTsgs
   sloc=205; Sname(sloc)="intdTsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,134)+GSTATS(k,134))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPET
   !sloc 141=TPET 
   sloc=206; Sname(sloc)="intTPET"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,141)+GSTATS(k,141))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intdTPETdt
   !sloc 142=dTPETdt
   sloc=207; Sname(sloc)="intdTPETdt"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,142)+GSTATS(k,142))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

 !intTPETProd
   !sloc 143= TPETProd
   sloc=208; Sname(sloc)="intTPETProd"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,143)+GSTATS(k,143))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPETDiss
   !sloc 144=TPETDiss
   sloc=209; Sname(sloc)="intTPETDiss"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,144)+GSTATS(k,144))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPETdTrans
   !sloc 145=TPETdTrans 
   sloc=210; Sname(sloc)="intTPETdTrans"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,145)+GSTATS(k,145))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPETEsgs
   !sloc 146 = TPETEsgs
   sloc=211; Sname(sloc)="intTPETEsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,146)+GSTATS(k,146))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPETdTsgs
   !sloc 147 = TPETdTsgs
   sloc=212; Sname(sloc)="intTPETdTsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,147)+GSTATS(k,17))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPES
   !sloc 154=TPES 
   sloc=213; Sname(sloc)="intTPES"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,154)+GSTATS(k,154))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intdTPESdt
   !sloc 155=dTPESdt
   sloc=214; Sname(sloc)="intdTPESdt"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,155)+GSTATS(k,155))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

 !intTPESProd
   !sloc 156= TPESProd
   sloc=215; Sname(sloc)="intTPESProd"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,156)+GSTATS(k,156))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPESDiss
   !sloc 157=TPESDiss
   sloc=216; Sname(sloc)="intTPESDiss"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,157)+GSTATS(k,157))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPESdTrans
   !sloc 158=TPETdTrans 
   sloc=217; Sname(sloc)="intTPESdTrans"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,158)+GSTATS(k,158))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPESEsgs
   !sloc 159 = TPESEsgs
   sloc=218; Sname(sloc)="intTPESEsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,159)+GSTATS(k,159))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intTPESdTsgs
   !sloc 160 = TPESdTsgs
   sloc=219; Sname(sloc)="intTPESdTsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,160)+GSTATS(k,160))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intMKE
   !sloc 161=MKE 
   sloc=220; Sname(sloc)="intMKE"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,161)+GSTATS(k,161))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intdMKEdt
   !sloc 162=dMKEdt
   sloc=221; Sname(sloc)="intdMKEdt"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,162)+GSTATS(k,162))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

 !intMProd
   !sloc 163= MProd
   sloc=222; Sname(sloc)="intMProd"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,163)+GSTATS(k,163))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intMDiss
   !sloc 164=MDiss
   sloc=223; Sname(sloc)="intMDiss"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,164)+GSTATS(k,164))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intMdTrans
   !sloc 165=MdTrans 
   sloc=224; Sname(sloc)="intMdTrans"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,165)+GSTATS(k,165))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intMPsgs
   !sloc 166 = MPsgs
   sloc=225; Sname(sloc)="intMPsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,166)+GSTATS(k,166))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp

  !intMdTsgs
   !sloc 167 = MdTsgs
   sloc=226; Sname(sloc)="intMdTsgs"
   scalatmp = 0.d0
   do k=szint,ezint
    scalatmp=scalatmp+r_1_2*(GSTATS(k+1,167)+GSTATS(k,167))*dzc(k)
   enddo
   GSTATS(:,sloc)=scalatmp


!******************end computing statistics***************************


  if (statbin ) then
  !Create output filename
  write(filen1,'(a,i5.5,a)') trim(statDIR)//'avg',nstep,'.bin'
  open( unit=500,file=filen1,status='unknown',form='unformatted',iostat=s1 )
   write(500) time
   write(500) nzp2, nstats
   write(500) zc
   write(500) Sname
   write(500) GSTATS  
  close(500)

 write(IOUT,'(a18,f10.5)') "FULL VSHEAR STATISTICS BINARY WRITTEN AT: ",time

 endif

 if (tkstat) then
!****************************************************************************
!*******************************TK STAT WRITE********************************
!****************************************************************************
!write out mean and rms quantities in TKSTAT format
 !Create output filename
 filen1 = statdir
 call concat(filen1,'avg')
 call concati(filen1,nstep)
                                                                                                                             
 open( unit=500,file=filen1,status='unknown',form='formatted',iostat=s1 )
  !WRITE HEADER
  write(500,'(a8,f15.8)') "RTIME = ",time

  !CREATE THE MAIN HEADER
  tkheader='I J Z S1'
  do n=1,nstats
   tkheader=trim(tkheader)//' '//trim(Sname(n))
  enddo
  write(500,'(a)') trim(tkheader)

  do k=1,nzp2
   plane=1
   write(500,135) i,' ',k,' ',zc(k),' ',zc(k)/d_theta,' ',(GSTATS(k,n),n=1,nstats)
  enddo
 close(500)
                                                                                                                             
 write(IOUT,'(a18,f15.8)') "TKSTATS WRITTEN AT: ",time

 endif
 
  !write out statistics header
  filen1 = statdir
  call concat(filen1,'tkstat_header')
  if (nstep < 1) then
   open(unit=510,file=filen1,status='unknown',form='formatted',iostat=s1 )
   do n=1,nstats
    write(510,140) n,sname(n)
   enddo
   close(510)
  endif

 endif !END MASTER ONLY

 if ( allocated(Ftemp) ) deallocate(Ftemp,stat=s1)
 if (s1.NE.0) then
  write(IOUT,*) "Error De-allocating Ftemp in stat1d"
  goto 1000
 endif


 120 FORMAT( (A) )
 135 FORMAT( ( 2(I3,a1), 2(E15.8,a1), 250(2X,E15.8E3) ) )
 140 FORMAT( I5,2X,A15 )

 1000 continue
 ok=max(s1,err1)
 return
end subroutine statistics_vshear_DNS

#endif
