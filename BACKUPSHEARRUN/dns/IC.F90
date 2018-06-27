subroutine add_mean(stat)
!@t
! \textbf{subroutine add\_mean(stat)}
!@h
!   Description:
!     Adds the mean flow to a flow-field.
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
!     Options available are a horizontal or vertical shear layer, towed
!     wake, propelled wake, and self-propelled wake.
!@q

 use ntypes,       only: r8
 use grid,         only: xc,yc,zc,xe,ye,ze
 use Flow,         only: u, v, w, u1_tmp2, u2_tmp2, u3_tmp2
 use Domain,       only: sx,ex,sy,ey,sz,ez
 use IO,           only: IOUT
 use Parameters,   only: flow_type, meanP1, meanP2, meanP3, meanP4, MX1c, MX2c, MX3c, rRe
 use dd,           only: myid
 implicit none

 !Passed Variables
 integer,intent(out) :: stat
 !Local Variables
 real(r8)            :: r, ut, zz, yy  !ud
 real(r8)            :: meantmp(sz-1:ez+1),mean(sz-1:ez+1),rms(sz-1:ez+1)
 real(r8)            :: t0,amp,eta,pi
 real(r8)            :: derfc
 real(r8)            :: randm
 real(r8)            :: Um,Uu,Ul,Ud,Zmu,Zul,Zld,Dmu,Dul,Dld
 integer             :: i,j,k,err1

 select case(flow_type)
   case('Vshear')
   u = 0.0
   v = 0.0
   w = 0.0
!   call add_random_noise(u(:,:,:),18,18,258,2.0)
!   call add_random_noise(v(:,:,:),18,18,258,2.0)
!   call add_random_noise(w(:,:,:),18,18,258,2.0)
!   call crop(err1)
  do k=sz-1,ez+1
   do j=sy-1,ey+1
    do i=sx-1,ex+1 
    randm = dble(i)*dble(j)*dble(k)
    call random_number_gen_MS(randm)
    u(i,j,k) = randm
    enddo
   enddo
  enddo
  
  call crop(err1)
!  u = 0.0
!  v = 0.0
!  w = 0.0

  do k=sz-1,ez+1
   do j=sy-1,ey+1
    do i=sx-1,ex+1
    zz = zc(k)-2500.0    !velocity is defined at the edge?
    u(i,j,k) = u(i,j,k) - (20.0*0.5d0)*(dtanh(2.d0*zz/100.0))
    enddo
   enddo
  enddo

   write(*,*),"NOT SURE IF FLUCTUATIONS ARE ADDED",myid

   if(myid.eq.10) write(*,*), u(10,10,:)

  case('Hshear')
   !Horizontal Shear Layer
   do j=sy-1,ey+1
    yy = yc(j)-MX2c
    u(:,j,:) = u(:,j,:) - (meanP1*0.5d0)*dtanh(2.d0*yy/meanP2)
   enddo

  case('Channel')

   !filter the random field
!   u1_tmp2 = 0.d0
!   u2_tmp2 = 0.d0
!   u3_tmp2 = 0.d0
!   do k=1,1
!    call filter(u,u1_tmp2,2,1,1,1,'u',err1)
!    call filter(u1_tmp2,u,2,1,1,1,'u',err1)
!    call filter(v,u2_tmp2,2,1,1,1,'v',err1)
!    call filter(u2_tmp2,v,2,1,1,1,'v',err1)
!    call filter(w,u3_tmp2,2,1,1,1,'w',err1)
!    call filter(u3_tmp2,w,2,1,1,1,'w',err1)
!   enddo
  
   call avgrmsX1X2(u,meantmp,rms,'u')
   pi = 4.d0*atan(1.d0)
   !t0 = 20.d0
   !amp = 3.d0*dsqrt(pi)*dsqrt(rRe/t0)
    t0 =1.d0
   call avgrmsX1X2(u,meantmp,rms,'u')
   pi = 4.d0*atan(1.d0)
   t0 =1.d0
   amp=0.d0 
   do k=sz-1,ez+1
    zz = zc(k)-MX3c    !velocity is defined at the edge?
    eta = -zz/0.22d0
    u(:,:,k) = u(:,:,k) - meantmp(k) - amp*t0*( (1.d0+2*eta**2.d0)*derfc(eta)-2.d0/sqrt(pi)*&
              eta*dexp(-1.d0*eta**2.d0))
   enddo

   Um=0.0;Uu=meanP1;Ul=0.d0;Ud=0.d0;
   Zmu=meanP2;Zul=meanP3;Zld=-20.0;
   Dmu=0.22;Dul=0.22;Dld=0.22;
   do k=sz-1,ez+1
    zz = zc(k)-MX3c   
    u(:,:,k)=u(:,:,k) +(Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dmu*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld)))
   enddo

   call avgrmsX1X2(v,mean,rms,'v')
   do k=sz-1,ez+1
    v(:,:,k) = v(:,:,k)-mean(k)
   enddo
   call avgrmsX1X2(w,mean,rms,'w')
   do k=sz-1,ez+1
    w(:,:,k) = w(:,:,k)-mean(k)
   enddo
  
  case('River')
   !River
   !filter the random field
   u1_tmp2 = 0.d0
   u2_tmp2 = 0.d0
   u3_tmp2 = 0.d0
   do k=1,10
    call filter(u,u1_tmp2,2,1,1,1,err1)
    call filter(u1_tmp2,u,2,1,1,1,err1)
    call ghost(u,'u',err1)
    call filter(v,u2_tmp2,2,1,1,1,err1)
    call filter(u2_tmp2,v,2,1,1,1,err1)
    call ghost(v,'v',err1)
    call filter(w,u3_tmp2,2,1,1,1,err1)
    call filter(u3_tmp2,w,2,1,1,1,err1)
    call ghost(w,'w',err1)
   enddo
   do k=sz-1,ez+1
    do j=sy-1,ey+1
     do i=sx-1,ex+1
!      u(i,j,k) = 0.d0
!      v(i,j,k) = 0.d0
!      w(i,j,k) = 0.d0
     enddo
    enddo 
   enddo
  
  case('Twake')
   !Towed Wake
   do k=sz-1,ez+1
    do j=sy-1,ey+1
     zz = zc(k)-MX3c
     yy = yc(j)-MX2c
     r   = dsqrt( yy**2.d0 + zz**2.d0 )
     u(:,j,k) = u(:,j,k) +  meanP1*dexp(-r**2.d0/(2.d0*meanP2**2.d0)) 
    enddo 
   enddo
  
  case('SPwake')
   !Self-Propelled Wake
   do k=sz-1,ez+1
    do j=sy-1,ey+1
     zz = zc(k)-MX3c
     yy = yc(j)-MX2c
     r   = dsqrt( yy**2.d0 + zz**2.d0 )
     u(:,j,k) = u(:,j,k) +  meanP1*( 1.d0 - r**2.d0/(2.d0*meanP2**2.d0) )*dexp(-r**2.d0/(2.d0*meanP2**2.d0)) 
    enddo 
   enddo
  
  case('PRPwake')
   !Propelled Wake
   do k=sz-1,ez+1
    do j=sy-1,ey+1
     zz = zc(k)-MX3c
     yy = yc(j)-MX2c
     r   = dsqrt( yy**2.d0 + zz**2.d0 )
     ud =meanP1*dexp(-r**2.d0/(2.d0*meanP2**2.d0))
     ut =meanP3*dexp(-r**2.d0/(2.d0*meanP4**2.d0))
     u(:,j,k) = u(:,j,k)  + ud - ut  
    enddo 
   enddo
 case DEFAULT
  write(IOUT,'(a)') "ABORTING MEAN PROFILE "//trim(flow_type)//" NOT IMPLEMENTED"
  stat=1
  return
 end select

 write(IOUT,'(a,4(1x,f12.6))') "     ADDED MEAN PROFILE: "//trim(flow_type)//" with parameters 1-4=,",meanP1,meanP2,meanP3,meanP4
 call ghost(u,'u',err1)
 call ghost(v,'v',err1)
 call ghost(w,'w',err1)
 stat=0
 return
end subroutine add_mean

subroutine add_langmuir(stat)
 use ntypes,      only: r8
 use Domain,      only: sz,ez
 use grid,        only: zc,ze
 use parameters,  only: g
 use Langmuir_params,    only: drift_type, us_amp, vs_amp, drift_wnumber, drift_wlength, u_stokes, v_stokes, dus_dz, dvs_dz
 use IO,          only: IOUT

 !LOCAL VARIABLES
 real(r8)   :: pi

 pi = 4.d0*datan(1.d0)
 select case (drift_type)
  case('no_drift')
   u_stokes = 0.d0
   v_stokes = 0.d0
  case('monochromatic')
   do k=sz-1,ez+1
    u_stokes(k,1) = us_amp*dexp(zc(k)/(1.d0/(2.d0*drift_wnumber)))
    u_stokes(k,2) = us_amp*dexp(ze(k)/(1.d0/(2.d0*drift_wnumber)))
    dus_dz(k,1) = us_amp/(1.d0/(2.d0*drift_wnumber))*dexp(zc(k)/(1.d0/(2.d0*drift_wnumber)))
    dus_dz(k,2) = us_amp/(1.d0/(2.d0*drift_wnumber))*dexp(ze(k)/(1.d0/(2.d0*drift_wnumber)))
    v_stokes(k,1) = vs_amp*dexp(zc(k)/(1.d0/(2.d0*drift_wnumber)))
    v_stokes(k,2) = vs_amp*dexp(ze(k)/(1.d0/(2.d0*drift_wnumber)))
    dvs_dz(k,1) = vs_amp/(1.d0/(2.d0*drift_wnumber))*dexp(zc(k)/(1.d0/(2.d0*drift_wnumber)))
    dvs_dz(k,2) = vs_amp/(1.d0/(2.d0*drift_wnumber))*dexp(ze(k)/(1.d0/(2.d0*drift_wnumber)))
  enddo
  case('broadband')
   do k=sz-1,ez+1
    u_stokes(k,:) = 0.d0 
    v_stokes(k,:) = 0.d0
   enddo
  case DEFAULT
   write(IOUT,'(a)') " ABORTING DRIFT PROFILE "//TRIM(drift_type)//" NOT IMPLEMENTED"
   stat=1
  return
 end select

 write(IOUT,'(a)') " STOKE DRIFT PROFILE "//TRIM(drift_type)//" IS ADDED"
 stat=0
 return
end subroutine add_langmuir

subroutine add_density(stat)
!@t
! \textbf{subroutine add\_density(stat)}
!@h
!   Description:
!     Updates the mean density field. 
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
!     Options are linear, two-layer, and Jdeep. 
!@q

 use ntypes,     only: r8
 use flow
 use Domain,     only: sx,ex,nxp2,sz,ez,nzp2,sy,ey
 use grid,       only: xc,zc
 use boundC,     only: VB
 use parameters
 use IO,         only: IOUT
 use dd

 implicit none

 !Passed Variables
 integer,intent(out)    :: stat 

 !Local Variables
 integer                :: i,j,k,err1
 real(r8)               :: xx,zz
 real(r8)               :: mean(sz-1:ez+1),rms(sz-1:ez+1)
 real(r8)               :: Um,Uu,Ul,Ud,Zmu,Zul,Zld,Dmu,Dul,Dld
 !Linear      denP1=d(rho)/dx_3 denP2=(unused)     denP3=(unused)
 !TwoLayer    denP1=Delta_rho   denP2=delta_omega  denP3=(unused)
 !Jdeep       denP1=Jm          denP2=Jd           denP3=z0

  Rv = 461.5
  Rd = 287.1
  evv = Rd/Rv
  rho = 0.0
  qv = 0.0
  ql = 0.0
  qr = 0.0
  er = 0.0
  ar = 0.0
  cr = 0.0 
 vrr = 0.0
 select case(density_profile)

  case('Linear')
   do k=sz-1,ez+1
    do j=sy-1,ey+1
     do i=sx-1,ex+1
      zz = zc(k)-DX3c
      tz(i,j,k) = 300.0-0.0065*zz
      at(i,j,k) = 300.0-0.0065*zz 
      qv(i,j,k) = 0.0165-0.000002*zz
     qv0(i,j,k) = 0.0165-0.000002*zz
      pz(i,j,k) = 101325.0*exp(5.5547*log(46153.8 - zz) - 2.28088*log(317030.0 - zz)-30.7647)
     pzz(i,j,k) = pz(i,j,k)
      r(i,j,k)  = pz(i,j,k)*(1.0+qv0(i,j,k))/(Rd*tz(i,j,k)*(1.0+qv0(i,j,k)/evv))
     ALPHA_0(i,j,k) = tz(i,j,k)*(1.0+qv0(i,j,k)/evv)/(1.0+qv0(i,j,k))
       ALPHA(i,j,k) = ALPHA_0(i,j,k)
         rho(i,j,k) = rho(i,j,k)+(tz(i,j,k))/((pz(i,j,k)/101325.0)**(287.1/1004.0))
     enddo
    enddo
   enddo 
   if (clip) then
     rhoMin= denP1*zc(nzp2)
     rhoMax= denP1*zc(1)
    write(IOUT,'(a,2(1x,f12.6))') "     LINEAR RHO MIN/MAX USED FOR CLIPPING: ",rhoMIN,rhoMAX
   endif
 
  case('TwoLayer')
   do k=sz-1,ez+1
    zz = zc(k)-DX3c
    rho(:,:,k) = 0.d0 + 0.5d0*denP1*(1.d0-dtanh(2.d0*zz/denP2))
   enddo
   
   if (clip) then
    rhoMin=denP1
    rhoMax=0.d0
    write(IOUT,'(a,2(1x,f12.6))') "     TWOLAYERZ RHO MIN/MAX USED FOR CLIPPING: ",rhoMIN,rhoMAX
   endif

  case('TwoLayerX')
   do i=sx-1,ex+1
     xx= xc(i)
!    rho(i,:,:) = rho(i,:,:) + rho_0 + 0.5d0*denP1*(1.d0+dtanh(2.d0*(xx-denP3)/denP2))
     rho(i,:,:) = 0.d0
   enddo
       
   if (clip) then
    rhoMin=denP1
    rhoMax=0.d0
    write(IOUT,'(a,2(1x,f12.6))') "     TWOLAYERX RHO MIN/MAX USED FOR CLIPPING: ",rhoMIN,rhoMAX
   endif

  case('Jdeep')
   Um=-denP1;Uu=-denP2;Ul=-denP2;Ud=-denP2;
   Zmu=0.d0;Zul=-5.d0;Zld=-10.d0;
   Dmu=0.1d0;Dul=0.1d0;Dld=0.1d0;   
   do k=sz-1,ez+1
    zz = zc(k)-DX3c
    rho(:,:,k)=0.d0 + 1.d0/9.81d0*((Um+Ud)/2.d0*(zz)+(Um-Uu)/2.d0*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2.d0*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2.d0*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))
   enddo 

   if (clip) then
    zz=zc(nzp2)
    rhoMin=0.d0 + 1.0/9.81*((Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))
    zz=zc(1)
    rhoMax=0.d0 + 1.0/9.81*((Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))

    write(IOUT,'(a,2(1x,f12.6))') "     JDEEP RHO MIN/MAX USED FOR CLIPPING: ",rhoMIN,rhoMAX
   endif

 case('Temperature')
   Um=0.0;Uu=denP2;Ul=Uu;Ud=Uu;
   Zmu=-1.0;Zul=-30.d0;Zld=-60.d0;
   Dmu=0.1d0;Dul=25.d0;Dld=25.d0;
   do k=sz-1,ez+1
    zz = zc(k)-DX3c
!    rho(:,:,k)=(Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
!                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
!                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld)));
    if (zz.LE.-1.d0) then
     rho(:,:,k) = denP2*(zz+1.d0)
    else 
     rho(:,:,k) = 0.d0
    end if
   enddo

   if (clip) then
    zz=zc(nzp2)
    rhoMin=(Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld)));
  
    zz=zc(1)
    rhoMax=(Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld)));

    write(IOUT,'(a,2(1x,f12.6))') "     TEMPERATURE RHO MIN/MAX USED FOR CLIPPING: ",rhoMIN,rhoMAX
   endif

  case DEFAULT 
   write(IOUT,'(a)') "ABORTING DENSITY PROFILE "//trim(density_profile)//" NOT IMPLEMENTED"
   stat=1
   return
  end select

  write(IOUT,'(a,3(1x,f12.6))') "    ADDED DENSITY PROFILE: "//trim(density_profile)//" with parameters 1-3=",denP1,denP2,denP3
  call ghost(rho,'rho',err1)
  stat=max(0,err1)
 return
end subroutine add_density

subroutine add_scalar1(stat)
!@t
! \textbf{subroutine add\_scalar1(stat)}
!@h
!   Description:
!     Updates the mean scalar1 field. 
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
!     Options are linear, two-layer, and Jdeep. 
!@q

 use ntypes,     only: r8
 use flow,       only: scal1, scal1_tmp2
 use Domain,     only: sx,ex,sy,ey,nxp2,sz,ez,nzp2
 use grid,       only: xc,zc
 use boundC,     only: VB
 use parameters, only: scal1_0,g,scal1MIN,scal1MAX,scal1_profile,scal1P1,scal1P2,scal1P3,scal1X3c, clip
 use IO,         only: IOUT
 implicit none

 !Passed Variables
 integer,intent(out)    :: stat 

 !Local Variables
 integer                :: i,j,k,err1
 real(r8)               :: xx,zz
 real(r8)               :: mean(sz-1:ez+1),rms(sz-1:ez+1)
 real(r8)               :: Um,Uu,Ul,Ud,Zmu,Zul,Zld,Dmu,Dul,Dld, urv
 !Linear      scal1P1=d(scal1)/dx_3 denP2=(unused)     denP3=(unused)
 !TwoLayer    scal1P1=Delta_scal1   denP2=delta_omega  denP3=center_offset
 !Jdeep       scal1P1=Jm          denP2=Jd           denP3=z0

 select case(scal1_profile)
  case('Linear')
   do k=sz-1,ez+1
    zz = zc(k)-scal1X3c
    scal1(:,:,k) =  scal1P1*zz
   enddo 
   if (clip) then
     scal1Min= scal1P1*zc(nzp2)
     scal1Max=  scal1P1*zc(1)
    write(IOUT,'(a,2(1x,f12.6))') "     LINEAR SCAL1 MIN/MAX USED FOR CLIPPING: ",scal1MIN,scal1MAX
   endif
 
  case('TwoLayer')
   do k=sz-1,ez+1
    zz = zc(k)-scal1X3c
    scal1(:,:,k) = 0.d0 + 0.5d0*scal1P1*(1.d0-dtanh(2.d0*zz/scal1P2))
   enddo
   
   if (clip) then
    scal1Min=0.d0
    scal1Max=0.d0+scal1P1
    write(IOUT,'(a,2(1x,f12.6))') "     TWO_LAYER_Z MIN/MAX USED FOR CLIPPING: ",scal1MIN,scal1MAX
   endif

  case('TwoLayerX')
   do i=sx-1,ex+1
     xx= xc(i)
     scal1(i,:,:) = 0.d0
   enddo
       
   if (clip) then
    scal1Min=scal1P1
    scal1Max=0.d0
    write(IOUT,'(a,2(1x,f12.6))') "     TWO_LAYER_X MIN/MAX USED FOR CLIPPING: ",scal1MIN,scal1MAX
   endif

  case('Jdeep')
   Um=-scal1P1;Uu=-scal1P2;Ul=-scal1P2;Ud=-scal1P2;
   Zmu=0.d0;Zul=-5.d0;Zld=-10.d0;
   Dmu=0.1d0;Dul=0.1d0;Dld=0.1d0;   
   do k=sz-1,ez+1
    zz = zc(k)-scal1X3c
    scal1(:,:,k)=0.d0 + 1.d0/9.81d0*((Um+Ud)/2.d0*(zz)+(Um-Uu)/2.d0*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2.d0*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2.d0*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))
   enddo 

   if (clip) then
    zz=zc(nzp2)
    scal1Min=0.d0 + 1.0/9.81*((Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))
    zz=zc(1)
    scal1Max=0.d0 + 1.0/9.81*((Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))

    write(IOUT,'(a,2(1x,f12.6))') "     JDEEP SCAL1 MIN/MAX USED FOR CLIPPING: ",scal1MIN,scal1MAX
   endif
 
  case('Salinity')
   Um=0.0;Uu=scal1P2;Ul=scal1P2;Ud=scal1P2;
   Zmu=-1.d0;Zul=-30.d0;Zld=-60.d0;
   Dmu=0.1d0;Dul=25.d0;Dld=25.d0;   
   do k=sz-1,ez+1
    zz = zc(k)-scal1X3c   
!    scal1(:,:,k)=(Um+Ud)/2.d0*(zz)+(Um-Uu)/2.d0*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
!                      +(Uu-Ul)/2.d0*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
!                      +(Ul-Ud)/2.d0*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld)))
    if (zz.LE.-1.d0) then
     scal1(:,:,k) = scal1P2*(zz+1.d0)
    else 
     scal1(:,:,k) = 0.d0
    end if
   enddo

   if (clip) then
    zz=zc(nzp2)
    scal1Min=((Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))
    zz=zc(1)
    scal1Max=((Um+Ud)/2.0*(zz)+(Um-Uu)/2*Dmu*log(abs(cosh((zz-Zmu)/Dmu)/cosh(-Zmu/Dmu))) &
                      +(Uu-Ul)/2*Dul*log(abs(cosh((zz-Zul)/Dul)/cosh(-Zul/Dul))) &
                      +(Ul-Ud)/2*Dld*log(abs(cosh((zz-Zld)/Dld)/cosh(-Zld/Dld))))

    write(IOUT,'(a,2(1x,f12.6))') "    SALINITY SCAL1 MIN/MAX USED FOR CLIPPING: ",scal1MIN,scal1MAX
   endif



  case DEFAULT 
   write(IOUT,'(a)') "ABORTING  SCAL1 PROFILE: "//trim(scal1_profile)//" NOT IMPLEMENTED"
   stat=1
   return
  end select

  write(IOUT,'(a,3(1x,f12.6))') "    ADDED SCAL1 PROFILE: "//trim(scal1_profile)//" with parameters 1-3=",scal1P1,scal1P2,scal1P3
  call ghost(scal1,'scal1',err1)
  stat=max(0,err1)
 return
end subroutine add_scalar1

subroutine crop(stat)
!@t
! \textbf{subroutine crop(stat)}
!@h
!   Description:
!     Crop the flow-field for
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
!     KYLE CAN YOU ADD A COMMENT ON THIS.
!@q

 use ntypes,       only: r8
 use flow,         only: u,v,w,p,rho
 use Domain,       only: sx,ex,sy,ey,sz,ez
 use grid,         only: xc,yc,zc,xe,ye,ze
 use IO,           only: IOUT
 use Parameters,  only: flow_type,cropP1,cropP2,MX1c, MX2c, MX3c, rRe

 implicit none

 !Passed Variables
 integer,intent(out)      :: stat
 !Local Variables
 integer                  :: i,j,k
 real(r8)                 :: r, func, xx, yy, zz
 real(r8)                 :: umean, vmean, wmean
 real(r8)                 :: urms, vrms, wrms
 real(r8)                 :: t0,amp,eta

 call avgX1X2X3(u,umean,urms,'cfluc')
 call avgX1X2X3(v,vmean,vrms,'cfluc')
 call avgX1X2X3(w,wmean,wrms,'cfluc')

 !Try not to divide by zero (especially with optimizations turned on)
 if (urms.LT.1d-13) urms=1.d0
 if (vrms.LT.1d-13) vrms=1.d0
 if (wrms.LT.1d-13) wrms=1.d0

 select case(flow_type)

  case('Vshear','Channel')
!for relax, set initial amplitude for v,w higher so that after decay they're the same -Hieu
   do k=sz,ez
    zz = zc(k)-2500.0
!    u(:,:,k)   = 1.d0*(u(:,:,k)-umean)*(cropP1/urms)*dexp(-zz**2/(cropP2**2) )
     u(:,:,k)   = 1.d0*(u(:,:,k)-umean)*(cropP1/urms)*dexp(-(zz+0.d0)**2/(cropP2**2) )
!    v(:,:,k)   = 1.d0*(v(:,:,k)-vmean)*(cropP1/vrms)*dexp(-zz**2/(cropP2**2) )
    v(:,:,k)   = 1.d0*(v(:,:,k)-vmean)*(cropP1/vrms)*dexp(-(zz+0.d0)**2/(cropP2**2) )
    zz = ze(k)-2500.0
!    w(:,:,k)   = 1.d0*(w(:,:,k)-wmean)*(cropP1/wrms)*dexp(-zz**2/(cropP2**2) )
    w(:,:,k)   = 1.d0*(w(:,:,k)-wmean)*(cropP1/wrms)*dexp(-(zz+0.d0)**2/(cropP2**2) )
   enddo 

  case('River','river')
   do k=sz-1,ez+1
    do j=sy-1,ey+1
     do i=sx-1,ex+1
      xx=xe(i)-MX1c
!      u(i,j,k)=(u(i,j,k)-umean)*(cropP1/urms)*(1.d0-0.5d0*(1.0+tanh((xx-cropP2)/(0.1*cropP2))))
      u(i,j,k)=(u(i,j,k)-umean)*(cropP1/urms)*dexp(-((xx-cropP2)/(0.5*cropP2))**2.d0)
      xx=xc(i)-MX1c
!      v(i,j,k)=(v(i,j,k)-vmean)*(cropP1/vrms)*(1.d0-0.5d0*(1.0+tanh((xx-cropP2)/(0.1*cropP2))))
      v(i,j,k)=(v(i,j,k)-vmean)*(cropP1/vrms)*dexp(-((xx-cropP2)/(0.5*cropP2))**2.d0)
!      w(i,j,k)=(w(i,j,k)-wmean)*(cropP1/wrms)*(1.d0-0.5d0*(1.0+tanh((xx-cropP2)/(0.1*cropP2))))
      w(i,j,k)=(w(i,j,k)-umean)*(cropP1/wrms)*dexp(-((xx-cropP2)/(0.5*cropP2))**2.d0)
     enddo
    enddo
   enddo

  case('Hshear')
   do j=sy-1,ey+1
    yy = yc(j)-MX2c
    u(:,j,:)   = (u(:,j,:)-umean)*(cropP1/urms)*dexp(-yy**2/(2.d0*cropP2**2) )
    w(:,j,:)   = (w(:,j,:)-wmean)*(cropP1/wrms)*dexp(-yy**2/(2.d0*cropP2**2) )
    yy = ye(j)-MX2c
    v(:,j,:)   = (v(:,j,:)-vmean)*(cropP1/vrms)*dexp(-yy**2/(2.d0*cropP2**2) )
   enddo 
  
  case('Twake','SPwake','PRPwake','Wake')
   do k=sz-1,ez+1
    do j=sy-1,ey+1
     zz = zc(k)-MX3c
     yy = yc(j)-MX2c
     r   = dsqrt( yy**2.d0 + zz**2.d0 )
     func = (1.d0+(r/cropP2)**2)  * ( dexp(-(r)**2/(2.d0*cropP2**2) ) )
     u(:,j,k)   = func*(cropP1/urms)*(u(:,j,k))
  
     zz = zc(k)-MX3c
     yy = ye(j)-MX2c
     r   = dsqrt( yy**2.d0 + zz**2.d0 )
     func = (1.d0+(r/cropP2)**2)  * ( dexp(-(r)**2/(2.d0*cropP2**2) ) )
     v(:,j,k)   = func*(cropP1/vrms)*(v(:,j,k))
  
     zz = ze(k)-MX3c
     yy = yc(j)-MX2c
     r   = dsqrt( yy**2.d0 + zz**2.d0 )
     func = (1.d0+(r/cropP2)**2)  * ( dexp(-(r)**2/(2.d0*cropP2**2) ) )
     w(:,j,k)   = func*(cropP1/wrms)*(w(:,j,k))
    enddo 
   enddo

  case DEFAULT
   write(IOUT,'(a)') "ABORTING IN crop FLOW TYPE "//trim(flow_type)//" NOT IMPLEMENTED"
   stat=1
   return

 end select
  
 write(IOUT,'(a,f12.6,a11,f12.6)') "     FIELDS CROPPED FOR: "//trim(flow_type)//" with Amp= ",cropP1," and Delta= ",cropP2
 stat=0
 return
end subroutine crop

subroutine relax_ics(stat)
!@t
! \textbf{subroutine relax\_ics(stat)}
!@h
!   Description:
!     Pre-relax on the initial conditions. 
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
!     adjustAMP is meaningless if the global restart flag is true. Kyle can
!     you add a line or two about this?
!@q

 use ntypes,     only: r8, i4
 use Flow,       only: u, v, w, rho, r_tmp1, p
 use grid,       only: xc,yc,zc,xe,ye,ze,rdxc,rdyc,rdzc,rdxe,rdye,rdze
 use Domain,     only: sx,ex,sy,ey,sz,ez
 use Parameters, only: relax_time, Rsponge, delt, flow_type, cropP1, cropP2, &
                       g,nstep,time,restart, RfixMean, MX1c, MX2c, MX3c, rRe
 use IO,         only: IOUT,statDIR,relaxDIR, wpre_stats
 implicit none

 !Passed Variables
 integer(i4),intent(out)     :: stat 
 !Local Variables
 real(r8)                    :: a1,a2,a3
 integer(i4)                 :: i,j,k
 real(r8)                    :: ltime,ec,dt_rk
 real(r8)                    :: u2d(sy-1:ey+1,sz-1:ez+1), v2d(sy-1:ey+1,sz-1:ez+1),w2d(sy-1:ey+1,sz-1:ez+1)
 real(r8)                    :: u2drms(sy-1:ey+1,sz-1:ez+1), v2drms(sy-1:ey+1,sz-1:ez+1),w2drms(sy-1:ey+1,sz-1:ez+1)
 real(r8)                    :: u1d(sz-1:ez+1), v1d(sz-1:ez+1),w1d(sz-1:ez+1)
 real(r8)                    :: u1drms(sz-1:ez+1), v1drms(sz-1:ez+1),w1drms(sz-1:ez+1)
 integer(i4)                 :: ok, err1
 real(r8)                    :: gsave,timesave,rResave
 integer                     :: nstepsave
 character(len=100)          :: tempDIR
 logical                     :: Rsponge_orig
 real(r8)                    :: r,zz,yy
 integer                     :: niters, err
 real(r8)                    :: resmax
 logical                     :: wdivplanes=.false.
 logical,parameter           :: debug=.false. ! this checks that each processor is
                                              ! performing the appropriate actions

 ok=0
 err1=0

 !SAVE AND RESET OUTPUT/SPONGE/g
 tempDIR=statDIR
 statDIR=relaxDIR
 Rsponge_orig=Rsponge
 Rsponge=.false.
 nstepsave=nstep
 timesave=time
 gsave=g
 rResave=rRe
 ltime=time

 g=0.d0
 rRe=rRe*10000000.d0
 nstep=0

 write(IOUT,'(a)') "***********************************************************"
 write(IOUT,'(a)') "*********************BEGIN RELAX ICS***********************"
 write(IOUT,'(a)') "***********************************************************"
 p=0.d0
 do while (ltime .LT. relax_time )
  write(IOUT,'(a,i3,f15.8)') '  RELAX STEP: ',nstep,ltime
  
!Ensure temporary variables are empty
  call zero_temps 

  !Calculate maximum timestep
  call calcdt(delt,ok)
  write(IOUT,'(a,f15.8)') '  timestep = ', delt
  write(IOUT,'(a,f15.8)') '  rRe = ', rRe
  if (ok.NE.0) stop
  if (debug) call check_point('Relax#1',.false.)
  ltime = ltime + delt


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!    RK3
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  !First sub-step
!  a1 = 0.d0
!  a2 = 1.d0
!  a3 = 1.d0/3.d0
!  call rk_ss(a1,a2,a3,1.d0,delt,.true.,ok)
!  if (debug) call check_point('Relax#2',.false.)

  !Second sub-step
!  a1=-5.d0/9.d0
!  a2=1.d0
!  a3=15.d0/16.d0
!  call rk_ss(a1,a2,a3,1.d0,delt,.true.,ok)
!  if (debug) call check_point('Relax#3',.false.)

  !Third sub-step
!  a1=-153.d0/128.d0
!  a2=1.d0
!  a3=8.d0/15.d0
!  call rk_ss(a1,a2,a3,1.d0,delt,.true.,ok)
!  if (debug) call check_point('Relax#4',.false.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Mixed RK3-CN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!First sub-step
 a1 = 0.d0
 a2 = 1.d0
 a3 = 1.d0/3.d0
 ec = -1.d0/6.d0
 dt_rk = 1.d0/3.d0*delt
 call rk_ss(a1,a2,a3,ec,dt_rk,delt,.true.,ok)
  if (debug) call check_point('NSsolver#1',.false.)

 !Second sub-step
 a1 = -5.d0/9.d0
 a2 = 1.d0
 a3 = 15.d0/16.d0
 ec = -10.d0/3.d0
 dt_rk = 5.d0/12.d0*delt
 call rk_ss(a1,a2,a3,ec,dt_rk,delt,.true.,ok)
  if (debug) call check_point('NSsolver#2',.false.)

 !Third sub-step
 a1 = -153.d0/128.d0
 a2 = 1.d0
 a3 = 8.d0/15.d0
 ec = 15.d0/8.d0
 dt_rk = 1.d0/4.d0*delt
 call rk_ss(a1,a2,a3,ec,dt_rk,delt,.true.,ok)
  if (debug) call check_point('NSsolver#3',.false.)


  select case(flow_type)
   case('Vshear','Channel')
    call avgrmsX1X2(u,u1d,u1drms,'u')
    call avgrmsX1X2(v,v1d,v1drms,'v')
    call avgrmsX1X2(w,w1d,w1drms,'w')
   case('Twake','SPwake','PRPwake','Wake') 
    call avgX1(u,u2d,u2drms,'u')
    call avgX1(v,v2d,v2drms,'v')
    call avgX1(w,w2d,w2drms,'w')
   case DEFAULT
  end select
  if (debug) call check_point('Relax#5',.false.)
  
  if (RfixMean) then
   if (debug) call check_point('Relax#6',.false.)
   select case(flow_type)
    case('Vshear')
     do k=sz,ez
      zz = zc(k)-MX3c
      zz=zc(k)
      u(:,:,k)  = ( u(:,:,k)-u1d(k) )/u1drms(k)*dexp(-zz**2/(cropP2**2) )*cropP1
      v(:,:,k)  = ( v(:,:,k)-v1d(k) )/v1drms(k)*dexp(-zz**2/(cropP2**2) )*cropP1
      zz = ze(k)-MX3c
      w(:,:,k)  = ( w(:,:,k)-w1d(k) )/w1drms(k)*dexp(-zz**2/(cropP2**2) )*cropP1
     enddo
    case('Twake','SPwake','PRPwake','Wake') 
     do k=sz,ez
      do j=sy,ey
       zz = zc(k)-MX3c
       yy = yc(j)-MX2c
       r   = dsqrt( yy**2.d0 + zz**2.d0 )
       u(:,j,k)   = ( u(:,j,k)-u2d(j,k) )/u2drms(j,k)*dexp(-r**2/(cropP2**2) )*cropP1
       zz = zc(k)-MX3c
       yy = ye(j)-MX2c
       r   = dsqrt( yy**2.d0 + zz**2.d0 )
       v(:,j,k)   = ( v(:,j,k)-v2d(j,k) )/v2drms(j,k)*dexp(-r**2/(cropP2**2) )*cropP1
       zz = ze(k)-MX3c
       yy = yc(j)-MX2c
       r   = dsqrt( yy**2.d0 + zz**2.d0 )
       w(:,j,k)   = ( w(:,j,k)-w2d(j,k) )/w2drms(j,k)*dexp(-r**2/(cropP2**2) )*cropP1
      enddo
     enddo
    case DEFAULT
   end select
   call ghost(u,'cfluc',err)
   call ghost(v,'cfluc',err)
   call ghost(w,'cfluc',err)
   if (debug) call check_point('Relax#6a',.false.)

   !PROJECT OUT DIVERGENCE
   !Source
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
        r_tmp1(i,j,k) = ( (u(i,j,k)-u(i-1,j,k))*rdxc(i) &
                   +   (w(i,j,k)-w(i,j,k-1))*rdzc(k) &
                   +   (v(i,j,k)-v(i,j-1,k))*rdyc(j) )
     enddo
    enddo 
   enddo
    if (debug) call check_point('Relax#6b',.false.)
   call ghost(r_tmp1,'rtemp',err)
   !CALCULATE CORRECTION 
   call mg_solver(p,r_tmp1,niters,resmax)
   write(IOUT,'(a20,i3,a11,e22.15)') "    MG Iterations: ",niters,"  RESIDUAL= ",resmax
   if (debug) call check_point('relax#6c',.false.)

   !SUBTRACT OFF DIVERGENCE
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      u(i,j,k)=u(i,j,k) + ( p(i+1,j,k)-p(i,j,k) )*rdxe(i)
      v(i,j,k)=v(i,j,k) + ( p(i,j+1,k)-p(i,j,k) )*rdye(j)
      w(i,j,k)=w(i,j,k) + ( p(i,j,k+1)-p(i,j,k) )*rdze(k)
     enddo
    enddo
   enddo
   if (debug) call check_point('relax#6d',.false.)

  else !if not fixmean
   if (debug) call check_point('relax#7',.false.)

  !SUBTRACT OFF MEAN, LEAVE FLUCTUATING AMPLITUDES UNTOUCHED
   select case(flow_type)
    case('Vshear','Channel')
     do k=sz,ez
      zz=zc(k)
      u(:,:,k)   = ( u(:,:,k)-u1d(k) )
      v(:,:,k)   = ( v(:,:,k)-v1d(k) )
      zz=ze(k)   
      w(:,:,k)   = ( w(:,:,k)-w1d(k) )
     enddo
    case('Twake','SPwake','PRPwake','Wake')
     do k=sz,ez
      do j=sy,ey
       u(:,j,k)   = ( u(:,j,k)-u2d(j,k) )
       v(:,j,k)   = ( v(:,j,k)-v2d(j,k) )
       w(:,j,k)   = ( w(:,j,k)-w2d(j,k) )
      enddo
     enddo
    case DEFAULT
    call ghost(u,'cfluc',err)
    call ghost(v,'cfluc',err)
    call ghost(w,'cfluc',err)
   end select
   if (debug) call check_point('relax#7a',.false.)
  endif !end fixmean conditional 
  if (debug) call check_point('relax#8',.false.)

  !ADD ORIGINAL MEAN PROFILES
  call add_mean(stat)
  if (debug) call check_point('relax#9',.false.)

  !CALCULATE DIVERGENCE
  call divergence(u,v,w,r_tmp1,wdivplanes,ok)
  if (debug) call check_point('relax#10',.false.)

  nstep=nstep+1
  time=ltime

  ! Save pre-relax statistics every wpre_stats

  if (mod(nstep,wpre_stats).EQ.0) then
   select case(flow_type)
    case('Vshear','Channel')
    call statistics_vshear_DNS(ok)
    case('Twake','SPwake','PRPwake','Wake')
!    call statistics_wake(ok)
    case DEFAULT
   end select
  end if
 enddo !relax_time loop

 !Reset Variables For Main Solver
 nstep=nstepsave
 time=timesave 
 g=gsave
 rRe=rResave
 Rsponge=Rsponge_orig
 statDIR=tempDIR


 write(IOUT,'(a)') "***********************************************************"
 write(IOUT,'(a)') "**********************END RELAX ICS************************"
 write(IOUT,'(a)') "***********************************************************"
 return
end subroutine relax_ics  



subroutine load_IC(stat)
!@t
! \textbf{subroutine load\_grid(stat)}
!@h
!   Description:
!     Loads the grid.in files and sets up the grid.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     Ensures that the first and last cells are unstretched in each
!     direction.
!@q

 use IO,     only: IOUT
 use Grid
 use Domain
 use Parameters, only: u1ICg,u2ICg,u3ICg,rhoICg,scal1ICg
#ifdef PARALLEL
 use dd,     only: myid,comm3d,realtype,logictype,MPI_MIN, chartype
#endif
 implicit none

!Passed Variables
 integer,intent(out)         :: stat

!Local Variables
 integer                     :: i,j,k,s1,idum
 integer                     :: err1
 real(r8)                    :: rdum,nzp2L
!**********************************************************************
!*******************************IC MEAN********************************
!**********************************************************************
#ifdef PARALLEL
 if (myid.EQ.0) then !MASTER
#endif
  open(unit=305,file='meanIC.in',form='formatted',status='old',action='read',iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a,i5)') "ERROR OPENING meanIC.in, IOSTAT=",s1
   stat=1
   goto 1000
  endif
  read(305,*)
  read(305,*)
  read(305,*) nzp2L
  if (nzp2L.NE.nzp2) then
   write(IOUT,'(a30)') "ERROR: meanIC.in WRONG SIZE:"
   write(IOUT,'(a12,i4)') "nzp2 RUN : ",nzp2
   write(IOUT,'(a12,i4)') "nzp2 GRID: ",nzp2L
  else
   do k=1,nzp2
    read(305,*) idum,rdum,u1ICg(k),u2ICg(k),u3ICg(k),rhoICg(k),scal1ICg(k)
   enddo
  endif
   close(305)
#ifdef PARALLEL 
  endif !MASTER
  call MPI_BCAST(u1ICg,nzp2,realtype,0,comm3d,stat)
  call MPI_BCAST(u2ICg,nzp2,realtype,0,comm3d,stat)
  call MPI_BCAST(u3ICg,nzp2,realtype,0,comm3d,stat)
  call MPI_BCAST(rhoICg,nzp2,realtype,0,comm3d,stat)
  call MPI_BCAST(scal1ICg,nzp2,realtype,0,comm3d,stat)

#endif
 stat=0
 write(IOUT,'(a)') "READING IC COMPLETED"
 return
                                                                                                                             
 1000 continue
 stat=-1
 write(IOUT,'(a)') "READING IC FAILED"
 return
end subroutine load_IC


subroutine set_densityBG(stat)

 use ntypes,     only: r8
 use Flow
 use IO,     only: IOUT
 use grid,       only: xc,zc
 use Domain, only: sx,ex,sy,ey,sz,ez
 use Parameters
#ifdef PARALLEL
 use dd,     only: myid,comm3d,realtype,logictype,MPI_MIN, chartype
#endif
 implicit none

!Passed Variables
 integer,intent(out)         :: stat

!Local Variables
 integer                     :: i,j,k
 real(r8)                    :: xx,zz

 Rv = 461.5
 Rd = 287.1
 evv = Rd/Rv
 do k=sz-1,ez+1; do j=sy-1,ey+1; do i=sx-1,ex+1
  zz = zc(k)-DX3c
  tz(i,j,k) = 300.0-0.0065*zz
  qv0(i,j,k) = 0.0165-0.000002*zz
  pz(i,j,k) = 101325.0*exp(5.5547*log(46153.8 - zz) - 2.28088*log(317030.0 - zz)-30.7647) 
  pzz(i,j,k) = pz(i,j,k) 
  densityBG(i,j,k) = (tz(i,j,k))/((pz(i,j,k)/101325.0)**(287.1/1004.0))
  r(i,j,k)  = pz(i,j,k)*(1.0+qv0(i,j,k))/(Rd*tz(i,j,k)*(1.0+qv0(i,j,k)/evv))
  ALPHA_0(i,j,k) = tz(i,j,k)*(1.0+qv0(i,j,k)/evv)/(1.0+qv0(i,j,k))
 enddo;enddo;enddo 

 stat = 0
 return
end subroutine set_densityBG

subroutine add_random_noise(u,nx,ny,nz,m)
implicit none
integer nx,ny,nz
real    m
real    u(nx,ny,nz)
integer i,j,k
integer iseed   !!!!!!
real x   !!!!!!
 iseed = 10
 do i=1,nx
  do j=1,ny
   do k=1,nz
    CALL RANDOM_SEED(iseed)   !!!!!!
    CALL RANDOM_NUMBER(x)  !!!!!!
    u(i,j,k) = u(i,j,k) + m*(x-0.5)  !!!!!!
   enddo
  enddo
enddo
return
end

