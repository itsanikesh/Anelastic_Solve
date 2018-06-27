












subroutine init_turbulence(stat)
!@t
! \textbf{subroutine init\_turbulence(stat)}
!@h
!   Description:
!     Initializes a turbulent field for u,v,w.
!@q
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     The turbulent fields are cropped to the flow type.
!@q

 use ntypes,     only: r8, i4
 use Flow,       only: u, v, w, u1_tmp2, u2_tmp2, u3_tmp2, p, r_tmp1, r_tmp2
 use Domain,     only: sx,ex,sy,ey,sz,ez
 use ratios 
 use Parameters
 use IO,         only: IOUT
 implicit none

 !Passed Variables
 integer(i4),intent(out)     :: stat 

 !Local Variables
 integer                     :: ok,s1, s2, s3, s4, s5
 integer                     :: i,j,k
 logical,parameter           :: debug=.false.

 ok=0

 !INITIALIZE TURBULENCE FIELD
 !deallocate u,v,w to make room for uhat,vhat,what
 if ( allocated(u) ) deallocate( u,stat=s1 ) 
 if ( allocated(v) ) deallocate( v,stat=s2 )
 if ( allocated(w) ) deallocate( w,stat=s3 )
 if ( allocated(r_tmp1) ) deallocate( r_tmp1,stat=s4 )
 if ( allocated(r_tmp2) ) deallocate( r_tmp2,stat=s5 )

 if (s1.NE.0.or.s2.NE.0.or.s3.NE.0.or.s4.NE.0.or.s5.NE.0) then
  write(IOUT,'(a,i4)') "Error deallocating u,v,w in  init_turbulence, stat= ",s1
  goto 1000
 endif

 !isospec returns random fluctuations in u1_tmp2, u2_tmp2, u3_tmp2
 call isospec(ok)
  if (debug) call check_point("init_turbulence#1",.false.)

 allocate( u(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1),             & 
           v(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1),             &
           w(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1),             &
           r_tmp1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1),        &
           r_tmp2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1),stat=s1 )
 if (s1.NE.0.) then
  write(IOUT,'(a,i4)') "Error allocating u,v,w in init_turbulence, stat= ",s1
  goto 1000
 endif

 u = 0.d0
 v = 0.d0
 w = 0.d0

!  if (flow_solver.eq.'mixed_RK3_ADI_PC_coll') then ! no interpolation needed
!    u(sx:ex,sy:ey,sz:ez) = u1_tmp2(sx:ex,sy:ey,sz:ez) 
!    v(sx:ex,sy:ey,sz:ez) = u2_tmp2(sx:ex,sy:ey,sz:ez) 
!    w(sx:ex,sy:ey,sz:ez) = u3_tmp2(sx:ex,sy:ey,sz:ez) 
!  else !Interpolate Velocity to Cell Edge's
   do k=sz,ez
    do j=sy,ey
     do i=sx,ex
      u(i,j,k)=u1_tmp2(i,j,k)
      v(i,j,k)=u2_tmp2(i,j,k)
      w(i,j,k)=u3_tmp2(i,j,k)
     enddo
    enddo
   enddo
!  endif


 call ghost(u,'cfluc',ok)
 call ghost(v,'cfluc',ok)
 call ghost(w,'cfluc',ok)
  if (debug) call check_point("init_turbulence#2",.false.)

 !CROP FLUCTUATIONS TO FLOW TYPE
 call crop(ok)
  if (debug) call check_point("init_turbulence#3",.false.)

 !SUCCESS
 stat=max(ok,s1,s2,s3,s4,s5)
 return

 !FAILURE
 1000 continue
 stat=s1
 return
end subroutine init_turbulence

subroutine isospec(stat2)
!@t
! \textbf{subroutine isospec(stat2)}
!@h
!   Description:
!    isospec returns random fluctuations in u1_tmp2, u2_tmp2, u3_tmp2 
!    corresponding to isotropic turbulence with a given energy spectrum.
!@q
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!     2.0       08/2009  Update random number generator for portability 
!@h
!   Comments:
!     The algorithm follows the initial conditions section on page 51 of 
!     "Numerical experiments in homogeneous turbulence" Rogallo, R.S. 1981.
!
!     The random numbers are generated using a minimal standard random number
!     generator, this MUST be initialized with a seed. The seed can be any 
!     integer between 1 and 2147483646 and should be entered as a double,
!     1.d0 instead of 1 for example. The random numbers are repeatable for
!     different runs if the seed is held constant.

!@q

 use ntypes,         only: i4, r8 
 use domain,         only: nxp2, nyp2, nzp2, sx, ex, cex,sy, ey, sz, ez
 use Parameters,     only: k0, k1, eps0, bbspect
 use Flow,           only: u1_tmp2, u2_tmp2, u3_tmp2 
 use fft
 use IO,             only: IOUT,resultDIR
 implicit none

 !Passed Variables
 integer,intent(out)  :: stat2 

 !Local Variables
 real(r8)             :: pi
 real(r8)             :: random_normal, Rmag
 integer              :: i,j,k, stat, nshell
 complex(r8)          :: cI
 real(r8),allocatable,dimension(:) :: Energy, counter
 integer              :: Nspec
 real(r8)             :: theta(3), E, C(3), rnum1, rnum2, rnum3,rnshell
 complex(r8)          :: zeta(3)
 complex(r8)          :: UsPlane(0:nxp2/2,0:nyp2-1),VsPlane(0:nxp2/2,0:nyp2-1),WsPlane(0:nxp2/2,0:nyp2-1)
 integer              :: ok, s1
 logical,parameter    :: debug=.false.
 character(len=100)   :: tmpstring
 stat2=0
 stat=0

 ! seed for the random number generator
 seed = 5.d0
 
 pi = 4.d0*datan(1.d0) 
 cI = (0.d0,1.d0)
! call Check_FFTS
! stop

 call init_spectral ! allocate variables
  if (debug) call check_point('isospec#1',.false.)
 allocate( uhat(sx-1:cex+1,sy-1:ey+1,sz-1:ez+1),        & 
           vhat(sx-1:cex+1,sy-1:ey+1,sz-1:ez+1),        &
           what(sx-1:cex+1,sy-1:ey+1,sz-1:ez+1),stat=s1 )
 if (s1.NE.0) then
  write(IOUT,'(a,i4)') "Error allocating uhat,vhat,what in isospec, stat= ",s1
  goto 1000
 endif

Nspec=4*max(nxp2,nyp2,nzp2)
do k=0,nzp2-1
 do j=0,nyp2-1
  do i=0,nxp2/2
    Rmag     = kx(i)*kx(i) + ky(j)*ky(j) + kz(k)*kz(k)
    nshell   = (idint(2.0d0*dsqrt(Rmag))+1)/2
    Nspec    = max(Nspec,nshell)
  enddo
 enddo
enddo

! Nspec=4*max(nxp2,nyp2,nzp2)
 allocate( Energy(0:Nspec) )
 allocate( counter(0:Nspec) )

!Model Spectrum to initialize calculation with
 Energy = 0.d0
 write(tmpstring,'(a,i5)') "isospec#1.5",Nspec
  if (debug) call check_point(tmpstring,.false.)

 do nshell=0,Nspec

  rnshell=dble(nshell)

  if (bbspect .eq. 1) then 
   !Flat Spectrum E(k)=1.d0
   Energy(nshell) = 1.d0
 
  elseif (bbspect .eq. 2) then
   !E(k) = (k/k0)^4 ????DIVIDED BY???? 
   Energy(nshell) = (rnshell/k0)**4/(1.d0+12.d0/5.d0*(rnshell/k0)**2)**(17.d0/6.d0)
 
  elseif (bbspect .eq. 3) then 
  !E(k) = (k/k0)^4 exp(-2 (k/k0)^2)
   if ( (2.d0*(rnshell/k0)**2) .GT.700.d0 ) then
    Energy(nshell) = 0
   else
    Energy(nshell) = (rnshell/k0)**4 * dexp(-2.d0*(rnshell/k0)**2)
   endif

  elseif (bbspect .eq. 4) then 
  !E(k) = (k/k0)^2 exp(-2 k/k0) 
   if (2.d0*rnshell/k0.GT.700.d0) then
    Energy(nshell) = 0.d0
   else
    Energy(nshell) = (rnshell/k0)**2 * dexp(-2.d0*rnshell/k0)
   endif
  elseif (bbspect .eq. 5) then 
  !E(k) = (k/k0)^4/16 exp(-2 (k/k0)^2)
   if ( (2.d0*(rnshell/k0)**2) .GT.700.d0 ) then
    Energy(nshell) = 0
   else
    Energy(nshell) = (rnshell/k0)**4.d0/16.d0 *dexp(-2.d0*(rnshell/k0)**2)
   endif
  elseif (bbspect .eq. 6) then 
   !Kolmogorov Spectrum, for k0<k<k1 E(k)=1.5*epsilon^(2/3)*k^(-5/3) else E(k)=0 
   if (nshell.GT.k0.AND.nshell.LT.k1) then
    Energy(nshell) = 1.5d0*eps0**(2.d0/3.d0)*rnshell**(-5.d0/3.d0)
   else
    Energy(nshell) = 0.d0
   endif

  elseif (bbspect .eq. 7) then 
   !Pulse Spectrum, for k0<k<k1 E(k)=1.0 else E(k)=0 
   if (nshell.GT.k0.AND.nshell.LT.k1) then
    Energy(nshell) = 1.d0
   else
    Energy(nshell) = 0.d0
   endif

  elseif (bbspect .eq. 8) then 
  !E(k) = (k/k0)^4 exp(-2 k/k0) 
   if (2.d0*rnshell/k0.GT.700.d0) then
    Energy(nshell) = 0.d0
   else
    Energy(nshell) = (rnshell/k0)**4 * dexp(-2.d0*rnshell/k0)
   endif
  else
   write(IOUT,'(a,i5)') "ERROR IN isospec: 1<bbspect<7 | bbspec=",bbspect
   stop
  endif
 enddo
  if (debug) call check_point('isospec#2',.false.)

!determine how the bins should be such that the model spectrum will be satisfied
counter = 0.d0
do k=0,nzp2-1
 do j=0,nyp2-1
  do i=0,nxp2/2
    Rmag     = kx(i)*kx(i) + ky(j)*ky(j) + kz(k)*kz(k)
    nshell   = (idint(2.0d0*dsqrt(Rmag))+1)/2
    counter(nshell) = counter(nshell) + 1.d0
  enddo
 enddo
enddo
 open(unit=100,file=trim(resultDIR)//'TargetSpectrum.dat', status = 'unknown',form='formatted',iostat=s1)
  if (s1.NE.0) write(IOUT,'(a,i4)') "ERROR Opening File: TargetSpectrum.dat with iostat: ",s1

   write(100,'(a1,a4,2(a17))') '#','k', 'Energy(k)','counter(k)'
                                 
 do k=1,Nspec
   write(100,'(i5,2(2x,E15.8E3))') k,Energy(k),counter(k)
 enddo
 close(100)
 if (debug) call check_point('isospec#3',.false.)

! sets uhat = 0
uhat=(0.d0,0.d0)
vhat=(0.d0,0.d0)
what=(0.d0,0.d0)

! initialize Planes to zero (very important!!!) Not all values are updated so 
! you will get junk out if you do not initialize to zero.
UsPlane=0.d0
VsPlane=0.d0
WsPlane=0.d0

 do k=0,nzp2-1
   do j=0,nyp2-1
    do i=1,nxp2/2-1
     Rmag   = kx(i)**2 + ky(j)**2 + kz(k)**2
     nshell = (idint(2.0d0*dsqrt(Rmag))+1)/2
     if (counter(nshell) .EQ. 0 .or. Energy(nshell) .LT. 1.d-15 ) then
      E=0.d0
     else
      E = dsqrt( Energy(nshell)/counter(nshell) )/(dsqrt(2.d0*Rmag))
     endif
     C(1) = random_normal()
     C(2) = random_normal()
     C(3) = random_normal()
     call random_number_gen_MS( rnum1 )
     call random_number_gen_MS( rnum2 )
     call random_number_gen_MS( rnum3 )
 
     theta(1) = rnum1
     theta(2) = rnum2
     theta(3) = rnum3

     zeta(1)=E*C(1)*exp(cI*2.d0*pi*theta(1))
     zeta(2)=E*C(2)*exp(cI*2.d0*pi*theta(2))
     zeta(3)=E*C(3)*exp(cI*2.d0*pi*theta(3))

     UsPlane(i,j)=zeta(2)*kz(k)-zeta(3)*ky(j)
     VsPlane(i,j)=zeta(3)*kx(i)-zeta(1)*kz(k)
     WsPlane(i,j)=zeta(1)*ky(j)-zeta(2)*kx(i)
    enddo
   enddo
   !Where kx=0
   do j = 1,nyp2/2-1
    Rmag     = (ky(j)**2 + kz(k)**2 )
    nshell   = (idint(2.0d0*dsqrt(Rmag))+1)/2
    if (counter(nshell) .EQ. 0 .or. Energy(nshell) .LT. 1.d-15 ) then
     E=0.d0
    else
     E = dsqrt( Energy(nshell)/counter(nshell) )/(dsqrt(2.d0*Rmag))
    endif

    C(1) = random_normal()
    C(2) = random_normal()
    C(3) = random_normal()
    call random_number_gen_MS( rnum1 )
    call random_number_gen_MS( rnum2 )
    call random_number_gen_MS( rnum3 )

    theta(1) = rnum1
    theta(2) = rnum2
    theta(3) = rnum3

    zeta(1)=E*C(1)*exp(cI*2.d0*pi*theta(1))
    zeta(2)=E*C(2)*exp(cI*2.d0*pi*theta(2))
    zeta(3)=E*C(3)*exp(cI*2.d0*pi*theta(3))

    UsPlane(0,j)=zeta(2)*kz(k)-zeta(3)*ky(j)
    VsPlane(0,j)=zeta(3)*kx(0)-zeta(1)*kz(k)
    WsPlane(0,j)=zeta(1)*ky(j)-zeta(2)*kx(0)
   enddo
   !Where kx=ky=0
   Rmag= (kz(k)**2)
   nshell   = (idint(2.0d0*dsqrt(Rmag))+1)/2
   if (counter(nshell) .EQ. 0 .or. Energy(nshell) .LT. 1.d-15 ) then
    E=0.d0
   else
    E = dsqrt( Energy(nshell)/counter(nshell) )/(dsqrt(2.d0*Rmag))
   endif

   C(1) = random_normal()
   C(2) = random_normal()
   C(3) = random_normal()
   call random_number_gen_MS( rnum1 )
   call random_number_gen_MS( rnum2 )
   call random_number_gen_MS( rnum3 )

   theta(1) = rnum1
   theta(2) = rnum2
   theta(3) = rnum3

   zeta(1)=E*C(1)*exp(cI*2.d0*pi*theta(1))
   zeta(2)=E*C(2)*exp(cI*2.d0*pi*theta(2))
   zeta(3)=E*C(3)*exp(cI*2.d0*pi*theta(3))

   UsPlane(0,0)=zeta(2)*kz(k)-zeta(3)*ky(0)
   VsPlane(0,0)=zeta(3)*kx(0)-zeta(1)*kz(k)
   WsPlane(0,0)=zeta(1)*ky(0)-zeta(2)*kx(0)
 ! uhat must be defined so that values are put into the pre-defined acceptable range.
 uhat(sx-1:cex+1,sy-1:ey+1,k+1)=UsPlane(0:nxp2/2,0:nyp2-1)
 vhat(sx-1:cex+1,sy-1:ey+1,k+1)=VsPlane(0:nxp2/2,0:nyp2-1)
 what(sx-1:cex+1,sy-1:ey+1,k+1)=WsPlane(0:nxp2/2,0:nyp2-1)
 enddo !End Loop Over Planes
 if (debug) call check_point('isospec#4',.false.)

 call Spectra3d('ActualSpectrums.dat')
 if (debug) call check_point("isospec#5",.false.)

!INVERSE TRANSFORM
 call FFT_B(uhat,u1_tmp2)
 call FFT_B(vhat,u2_tmp2)
 call FFT_B(what,u3_tmp2)
 if (debug) call check_point("isospec#6",.false.)

 call ghost(u1_tmp2,'cfluc',ok)
 call ghost(u2_tmp2,'cfluc',ok)
 call ghost(u3_tmp2,'cfluc',ok)
 if (debug) call check_point("isospec#7",.false.)

 if (debug) write(IOUT,*) maxval(u2_tmp2), maxval(u3_tmp2), maxval(u1_tmp2)

! deallocate temporary variables to save space
 deallocate( uhat, vhat, what, stat=s1)
 if (s1.NE.0) then
  write(IOUT,'(a,i4)') "Error deallocating uhat,vhat,what in isospec, stat= ",s1
  goto 1000
 endif

 deallocate( Energy, counter, stat=s1)
 if (s1.NE.0) then
  write(IOUT,'(a,i4)') "Error deallocating Energy, counter in isospec, stat= ",s1
  goto 1000
 endif

 deallocate( kx, ky, kz, rkx, rky, rkz, mkx, mky, mkz, stat=s1)
 if (s1.NE.0) then
  write(IOUT,'(a,i4)') "Error deallocating wavenumbers in isospec, stat= ",s1
  goto 1000
 endif

 if (allocated(FIELD_IN) ) deallocate(FIELD_IN, stat=s1)
 if (allocated(FIELD_OUT) ) deallocate(FIELD_OUT, stat=s1)
 if (allocated(PLANE_IN) ) deallocate(PLANE_IN, stat=s1)
 if (allocated(PENCIL_IN) ) deallocate(PENCIL_IN, stat=s1)
 if (allocated(PLANE_OUT) ) deallocate(PLANE_OUT, stat=s1)
 if (allocated(PENCIL_OUT) ) deallocate(PENCIL_OUT, stat=s1)
 if (s1.NE.0) then
  write(IOUT,'(a,i4)') "Error deallocating temporary fft variables, stat= ",s1
  goto 1000
 endif

 1000 continue  !FAILURE
 stat2=max(stat,s1)
 return

end subroutine isospec

subroutine init_spectral
!@t
! \textbf{subroutine init\_spectral}
!@h
!   Description:
!     Initializes variables for use with FFTs to transfer between Fourier
!     and physical space. 
!@q
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     Also defines the wavenumbers and modified wavenumbers. This uses an 
!     equivalent length scale based on the minimum dx and the number of 
!     gridpoints in each direction to scale the kx, ky, and kz wavenumbers.
!     KYLE DO WE NEED THE MODIFIED WAVE NUMBERS??? IF SO WHY?
!@q

 use ntypes,     only: r8, i4
 use fft
 use Grid,       only: xL, yL, zL, dxc, dyc, dzc
 use domain,     only: nxp2, nyp2, nzp2
 use Parameters, only: flow_type
 use IO,         only:IOUT
 implicit none

!Passed Variables

!Local Variables
 integer  :: i,j,k
 real(r8) :: Pi
 real(r8) :: dxm, dym, dzm
 real(r8) :: equivxL, equivyL, equivzL

 Pi=4.d0*datan(1.d0)

  select case(flow_type)
   case('Vshear')
     equivxL = xL
     equivyL = yL
     equivzL = zL
   case('spat_Twake','Twake','wake','SPwake_mom','SPwake','PRPwake','Gridturb')
! Define equivalent lengths for normalizing wavenumbers. This is only valid
! for domains with a point source at the center that is damped strongly away
! from the center on a uniform block near the origin. The uniform block should
!  be at least 1.5 x 1.5. This is applicable to the wake only and a special 
! case is needed for a stretched shear layer. 

     equivxL = minval(dxc)*dble(nxp2-1)
     equivyL = minval(dyc)*dble(nyp2-1)
     equivzL = minval(dzc)*dble(nzp2-1)

   case DEFAULT
    write(IOUT,'(a,a)') "WARNING: SPECTRA NOT DEFINED FOR FLOW TYPE: ",trim(flow_type)
   end select

 allocate( kx(0:nxp2-1) )
 allocate( ky(0:nyp2-1) )
 allocate( kz(0:nzp2-1) )
 allocate( rkx(0:nxp2-1) )
 allocate( rky(0:nyp2-1) )
 allocate( rkz(0:nzp2-1) )
 allocate( mkx(0:nxp2-1) )
 allocate( mky(0:nyp2-1) )
 allocate( mkz(0:nzp2-1) )

 allocate( FIELD_IN(0:nxp2-1,0:nyp2-1,0:nzp2-1) )
 allocate( FIELD_OUT(0:nxp2/2,0:nyp2-1,0:nzp2-1))
 call dfftw_plan_dft_r2c_3d(plan3dF,nxp2,nyp2,nzp2,FIELD_IN,FIELD_OUT,FFTW_ESTIMATE)
 call dfftw_plan_dft_c2r_3d(plan3dB,nxp2,nyp2,nzp2,FIELD_OUT,FIELD_IN,FFTW_ESTIMATE)
!*********************************************************************************
!**********************Setup Wave and Modified Wave Numbers***********************
!*********************************************************************************
 do i=0,nxp2-1 !1
!  kx(i) = dble(i) 
!  rkx(i) = kx(i)/xL 
   kx(i) = 2.d0*Pi*dble(i)/equivxL
 if (i.GT.(nxp2)/2) then
!   kx(i) = dble(i-nxp2)
!   rkx(i) = kx(i)/xL 
   kx(i) = 2.d0*Pi*dble(i-nxp2)/equivxL
  endif
 enddo  !1

 do j=0,nyp2-1 !1
!  ky(j) = dble(j) 
!  rky(j) = ky(j)/yL 
  ky(j) =  2.d0*Pi*dble(j)/equivyL
 if (j.GT.(nyp2)/2) then
!   ky(j) = dble(j-nyp2)
!   rky(j) = ky(j)/yL 
   ky(j) = 2.d0*Pi*dble(j-nyp2)/equivyL
  endif
 enddo  !1

 do k=0,nzp2-1 !1
!  kz(k) = dble(k) 
!  rkz(k) = kz(k)/zL 
  kz(k) = 2.d0*Pi*dble(k)/equivzL
 if (k.GT.(nzp2)/2) then
!   kz(k) = dble(k-nzp2)
!   rkz(k) = kz(k)/zL 
  kz(k) = 2.d0*Pi*dble(k-nzp2)/equivzL
  endif
 enddo  !1

 dxM=equivxL/dble(nxp2)
 dyM=equivyL/dble(nyp2)
 dzM=equivzL/dble(nzp2)

 do i=0,nxp2-1
  mkx(i) = dsin(kx(i)*dxm)/dxm
 enddo
 do j=0,nyp2-1
  mky(j) = dsin(ky(j)*dym)/dym
 enddo
 do k=0,nzp2-1
  mkz(k) = dsin(kz(k)*dzm)/dzm
 enddo

return
end subroutine init_spectral

subroutine FFT_F(varP,varS)
!@t
! \textbf{subroutine FFT\_F(varP,varS)}
!@h
!   Description:
!    Perform a forward Fast Fourier Transform.
!@q
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     Pencil and Plane Decompostion with Reduction to Plane/Pencil Master
!     For Computation. Significant performance improvements could be
!     realized by all nodes performing transforms as opposed to just the
!     masters (parallel version only).
!@q

!**********************************************************************************
!*****FORWARD TRANSFORM  ui(x,y,z)-->ui^(kx,ky,kz) ********************************
!**********************************************************************************

 use ntypes, only: r8
 use domain, only: sx,ex,sy,ey,sz,ez,cex,nxp2,nyp2,nzp2
 use fft,    only: plan3dF, plan2dF, plan1dF, PLANE_IN, PLANE_OUT, PENCIL_IN, PENCIL_OUT, FIELD_IN, FIELD_OUT
 implicit none

!Passed Variables
 real(r8),intent(in)     :: varP(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 complex(r8),intent(out) :: varS(sx-1:cex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer     :: i,j,k
 integer     :: stat
 real(r8)    :: Zplane(1:nxp2,1:nyp2)
 complex(r8) :: ZsPlane(1:nxp2/2+1,1:nyp2)
 complex(r8) :: XYsPencil(1:nzp2)

 FIELD_IN(:,:,:) = varP(:,:,:)
 call dfftw_execute(plan3dF)
 varS(:,:,:) = FIELD_OUT(:,:,:)/dble(nxp2*nyp2*nzp2)
 return
end subroutine FFT_F


subroutine FFT_B(varS,varP)
!@t
! \textbf{subroutine FFT\_B(varP,varS)}
!@h
!   Description:
!    Perform a backward Fast Fourier Transform.
!@q
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     Input is not preserved! ui^ is used to store intermediate field ui^*.
!     Pencil and Plane Decompostion with Reduction to Plane/Pencil Master
!     For Computation. Significant performance improvements could be
!     realized by all nodes performing transforms as opposed to just the
!     masters (parallel version only).
!@q

!**********************************************************************************
!*****BACKWARD TRANSFORM  ui^(kx,ky,kz)-->ui(x,y,z) *******************************
!**********************************************************************************

 use ntypes, only: r8
 use domain, only: sx,ex,sy,ey,sz,ez,cex,nxp2,nyp2,nzp2
 use fft,    only: plan3dB, plan2dB, plan1dB, PLANE_IN, PLANE_OUT, PENCIL_IN, PENCIL_OUT, FIELD_IN, FIELD_OUT
 implicit none

!Passed Variables
 complex(r8),intent(inout) :: varS(sx-1:cex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)      :: varP(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer     :: i,j,k
 integer     :: stat
 real(r8)    :: Zplane(1:nxp2,1:nyp2)
 complex(r8) :: ZsPlane(1:nxp2/2+1,1:nyp2)
 complex(r8) :: XYsPencil(1:nzp2)


 FIELD_OUT(:,:,:)=varS(:,:,:)
 call dfftw_execute(plan3dB)
 varP(:,:,:)=FIELD_IN(:,:,:)

 return
end subroutine FFT_B

function random_normal() result(fn_val)
!@t
! \textbf{function random\_normal() result(fn\_val)}
!@h
!   Description:
!     The function random_normal() returns a normally distributed
!     pseudo-random number with zero mean and unit variance.
!@q
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     Adapted from the following Fortran 77 code
!     ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!     THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!     VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.
!     The algorithm uses the ratio of uniforms method of A.J. Kinderman
!     and J.F. Monahan augmented with quadratic bounding curves.
!@q

 use ntypes, only: r8

real(r8)            :: fn_val

!Local variables
real(r8),parameter :: s=0.449871d0, t=-0.386595d0, a=0.196d0, b=0.25472d0, r1=0.27597d0, r2=0.27846d0, half=0.5d0
real(r8)           :: u, v, x, y, q

!Generate P = (u,v) uniform in rectangle enclosing acceptance region
do
  call random_number_gen_MS(u)
  call random_number_gen_MS(v)
  v = 1.7156d0*(v - half)

!Evaluate the quadratic form
  x = u - s
  y = dabs(v) - t
  q = x**2 + y*(a*y - b*x)

  if (q < r1) exit  !Accept P if inside inner ellipse
  if (q > r2) cycle !Reject P if outside outer ellipse
  if (v**2 < -4.d0*dLOG(u)*u**2) exit !Reject P if outside acceptance region
enddo

fn_val = v/u !Return ratio of P's coordinates as the normal deviate

return
end function random_normal

subroutine Check_FFTS
!@t
! \textbf{subroutine Check\_FFTS}
!@h
!   Description:
!    Checks to make sure the forward and backward FFTs are working 
!    properly
!@q
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     Set u=sin(kx*x)
!     Set v=sin(ky*y)
!     Set w=sin(kz*z)
!     FFT-->take ki*xi-->IFFT
!     should have du/dx, dv/dy, dw/dz in physical space
!     Check with exact cos wave and write out results
!@q

 use ntypes, only: r8
 use fft
 use Grid, only: xL,yL,zL
 use domain, only: nxp2, nyp2, nzp2, sx, ex, cex,sy, ey, sz, ez
 use Flow,   only: u,v,w
 use IO,     only: IOUT,resultDIR
 use grid,   only: zc
 implicit none

!Local Variables
 integer :: i,j,k, stat
 complex(r8),parameter :: cI=(0.d0,1.d0)
 real(r8)    :: Zplane(1:nxp2,1:nyp2)
 real(r8)    :: Xplane(1:nyp2,1:nzp2)
 real(r8)    :: Yplane(1:nxp2,1:nzp2)
 real(r8)    :: pi, dx, dy, dz
 integer     :: s1

 call init_spectral

 pi = 4.d0*datan(1.d0) 
 dx=xL/dble(nxp2)
 dy=yL/dble(nyp2)
 dz=zL/dble(nzp2)

 if (.not. allocated(uhat) ) allocate( uhat(sx-1:cex+1,sy-1:ey+1,sz-1:ez+1),stat=s1 ) 
 if (.not. allocated(vhat) ) allocate( vhat(sx-1:cex+1,sy-1:ey+1,sz-1:ez+1),stat=s1 )
 if (.not. allocated(what) ) allocate( what(sx-1:cex+1,sy-1:ey+1,sz-1:ez+1),stat=s1 )

 do k=sz-1,ez+1
  do j=sy-1,ey+1
   do i=sx-1,ex+1
    u(i,j,k)= dsin(1.d0*dble(i)*dx*(2.d0*pi/xL) )
    v(i,j,k)= dsin(2.d0*dble(j)*dy*(2.d0*pi/yL) )
    w(i,j,k)= dexp(-2.d0*zc(k)**2 )! dsin(13.d0*dble(k)*dz*(2.d0*pi/zL) )
   enddo
  enddo
 enddo

 call FFT_F(u,uhat)
 call FFT_F(v,vhat)
 call FFT_F(w,what)

 do i=sx-1,cex+1
  uhat(i,:,:) = cI*kx(i-1)*uhat(i,:,:)
 enddo
 do j=sy-1,ey+1
  vhat(:,j,:) = cI*ky(j-1)*vhat(:,j,:)
 enddo
 do k=sz-1,ez+1
  what(:,:,k) = cI*kz(k-1)*what(:,:,k)
 enddo
 call Spectra3d('DexpSpec')
 call FFT_B(uhat,u)
 call FFT_B(vhat,v)
 call FFT_B(what,w)

 Zplane=0.d0
 k=sz+10
 Yplane=0.d0
 j=sy+1
 Xplane=0.d0
 i=sx+10
 Xplane(:,:)=v(i,:,:)
 Yplane(:,:)=u(:,j,:)
 Zplane(:,:)=w(:,:,k)
  open(unit=100,file=trim(resultDIR)//'testZ.out',form='formatted',status='unknown',iostat=stat)
  do i=1,nxp2
   write(100,*) i,Zplane(i,10), (1.d0*2.d0*Pi/xL)*dcos(1.d0*dble(i)*dx*(2.d0*pi/dble(xL)) )
!   write(100,*) i,Zplane(i,10), dsin(1.d0*dble(i)*dx*(2.d0*pi/xL) )
  enddo
 close(100)

 open(unit=100,file=trim(resultDIR)//'testY.out',form='formatted',status='unknown',iostat=stat)
  do k=1,nzp2
   write(100,*) k,Yplane(10,k), -4.d0*zc(k)*dexp( -2.d0*zc(k)**2 )
!   write(100,*) k,Yplane(10,k), (13.d0*2.d0*Pi/zL)*dcos(13.d0*dble(k)*dz*(2.d0*pi/zL) )
!   write(100,*) k,Yplane(10,k), dsin(3.d0*dble(k)*dz*(2.d0*pi/dble(zL)) )
  enddo
 close(100)

 open(unit=100,file=trim(resultDIR)//'testX.out',form='formatted',status='unknown',iostat=stat)
  do j=1,nyp2
   write(100,*) j,Xplane(j,10), (2.d0*2.d0*Pi/yL)*dcos(2.d0*dble(j)*dy*(2.d0*pi/yL) )
!   write(100,*) j,Xplane(j,10), dsin(2.d0*dble(j)*dy*(2.d0*pi/dble(yL)) )
  enddo
 close(100)

 write(IOUT,*) "FFT TEST COMPLETE"


return
end subroutine Check_FFTS


subroutine Spectra3d(filename)
!@t
! \textbf{subroutine Spectra3d(filename)}
!@h
!   Description:
!     Calculate the 3D spectra using uhat, vhat, what
!@q
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     KYLE CAN YOU ADD A FEW LINES ON THIS???
!@q

 use ntypes,     only: r8
 use Parameters, only: rRe,nstep,time
 use fft,        only: kx, ky, kz, uhat, vhat, what
 use Domain,     only: sx,ex,sy,ey,sz,ez,cex, nxp2, nyp2, nzp2
 use IO,         only: IOUT,resultDIR,flowDIR
 implicit none

!Passed Variables
 character(len=*)                   :: filename
character(len=250)                   :: ICfile
!Local Variables
 integer                            :: i, j, k, nshell, stat, Nspec, ierr, kw!, N
 real(r8),allocatable,dimension(:)  :: Energy11, Energy22, Energy33, Diss, Energy12, Energy31, Energy23, Energy, Etemp
 real(r8)                           :: Rmag, Umod, Vmod, Wmod,UVmod, UWmod, VWmod, KinEn, urms, vrms, wrms,RS(1:3),testrms
 real(r8)                           :: TotEn, Depsilon, acor(1:3)
 complex(r8),allocatable,dimension(:,:) :: UsPlane,VsPlane,WsPlane
 character(len=100)                 :: tmpstring
 integer :: s1

 logical,parameter                  :: debug=.false.

 allocate( UsPlane(0:nxp2/2,0:nyp2-1),VsPlane(0:nxp2/2,0:nyp2-1),WsPlane(0:nxp2/2,0:nyp2-1),    STAT=stat )

Nspec=0
do k=0,nzp2-1
 do j=0,nyp2-1
  do i=0,nxp2/2
    Rmag     = kx(i)*kx(i) + ky(j)*ky(j) + kz(k)*kz(k)
    nshell   = (idint(2.0d0*dsqrt(Rmag))+1)/2
    Nspec    = max(Nspec,nshell)
  enddo
 enddo
enddo
! N=max(nxp2,nyp2,nzp2)
! Nspec=4*N

 allocate( Etemp(0:Nspec),    STAT=stat )
 allocate( Energy(0:Nspec),   STAT=stat )
 allocate( Energy11(0:Nspec), STAT=stat )
 allocate( Energy22(0:Nspec), STAT=stat )
 allocate( Energy33(0:Nspec), STAT=stat )
 allocate( Energy12(0:Nspec), STAT=stat )
 allocate( Energy31(0:Nspec), STAT=stat )
 allocate( Energy23(0:Nspec), STAT=stat )
 allocate( Diss(0:Nspec),     STAT=stat )

 write(tmpstring,'(a,i5)') "Spectra3d#1",stat
  if (debug) call check_point(tmpstring,.false.)

 Energy   = 0.d0
 Energy11 = 0.d0
 Energy22 = 0.d0
 Energy33 = 0.d0
 Energy12 = 0.d0
 Energy31 = 0.d0
 Energy23 = 0.d0
 Diss     = 0.d0

  do k=sz,ez
  kw=k-1
 UsPlane(:,:)=uhat(:,:,k)
 VsPlane(:,:)=vhat(:,:,k)
 WsPlane(:,:)=what(:,:,k)
  do j=0,nyp2-1
   do i=1,nxp2/2-1
    Rmag   = kx(i)**2 + ky(j)**2 + kz(kw)**2
    nshell = (idint(2.0d0*dsqrt(Rmag))+1)/2
    Umod   = dble( conjg( UsPlane(i,j) ) * UsPlane(i,j) )
    Vmod   = dble( conjg( VsPlane(i,j) ) * VsPlane(i,j) )
    Wmod   = dble( conjg( WsPlane(i,j) ) * WsPlane(i,j) )
    Energy11(nshell) = Umod + Energy11(nshell)
    Energy22(nshell) = Vmod + Energy22(nshell)
    Energy33(nshell) = Wmod + Energy33(nshell)
    KinEn = Umod + Vmod + Wmod
    Energy(nshell)   = Energy(nshell)  + KinEn 
    Diss(nshell)   = Rmag*( KinEn ) + Diss(nshell)
    UVmod = dble( conjg(UsPlane(i,j))*VsPlane(i,j) + UsPlane(i,j)*conjg(VsPlane(i,j)) )*(0.5d0)
    Energy12(nshell) = UVmod + Energy12(nshell)
    UWmod = dble( conjg(UsPlane(i,j))*WsPlane(i,j) + UsPlane(i,j)*conjg(WsPlane(i,j)) )*(0.5d0) 
    Energy31(nshell) = UWmod + Energy31(nshell)
    VWmod = dble( conjg(VsPlane(i,j))*WsPlane(i,j) + VsPlane(i,j)*conjg(WsPlane(i,j)) )*(0.5d0) 
    Energy23(nshell) = VWmod + Energy23(nshell)
   enddo
  enddo
  !Where kx=0
  do j = 1,nyp2/2-1
   Rmag   =  ky(j)**2 + kz(kw)**2
   nshell = (idint(2.0d0*dsqrt(Rmag))+1)/2
   Umod   = dble( conjg( UsPlane(0,j) ) * UsPlane(0,j) )
   Vmod   = dble( conjg( VsPlane(0,j) ) * VsPlane(0,j) )
   Wmod   = dble( conjg( WsPlane(0,j) ) * WsPlane(0,j) )
   Energy11(nshell) = Umod + Energy11(nshell)
   Energy22(nshell) = Vmod + Energy22(nshell)
   Energy33(nshell) = Wmod + Energy33(nshell)
   KinEn  = Umod + Vmod + Wmod
   Energy(nshell) = Energy(nshell) + KinEn
   Diss(nshell)   = Rmag*KinEn + Diss(nshell)
   UVmod = dble( conjg(UsPlane(0,j))*VsPlane(0,j) + UsPlane(0,j)*conjg(VsPlane(0,j)) )*(0.5d0)
   Energy12(nshell) = UVmod + Energy12(nshell)
   UWmod = dble( conjg(UsPlane(0,j))*WsPlane(0,j) + UsPlane(0,j)*conjg(WsPlane(0,j)) )*(0.5d0)
   Energy31(nshell) = UWmod + Energy31(nshell)
   VWmod = dble( conjg(VsPlane(0,j))*WsPlane(0,j) + VsPlane(0,j)*conjg(WsPlane(0,j)) )*(0.5d0)
   Energy23(nshell) = VWmod + Energy23(nshell)
  enddo
  !Where kx=ky=0
  Rmag   =   kz(kw)**2
  nshell = (idint(2.0d0*dsqrt(Rmag))+1)/2
  Umod   = 0.5d0*dble( conjg( UsPlane(0,0) ) * UsPlane(0,0) )
  Vmod   = 0.5d0*dble( conjg( VsPlane(0,0) ) * VsPlane(0,0) )
  Wmod   = 0.5d0*dble( conjg( WsPlane(0,0) ) * WsPlane(0,0) )
  Energy11(nshell) = Umod + Energy11(nshell)
  Energy22(nshell) = Vmod + Energy22(nshell)
  Energy33(nshell) = Wmod + Energy33(nshell)
  KinEn  = Umod + Vmod + Wmod
  Energy(nshell) = Energy(nshell) + KinEn 
  Diss(nshell)   = Rmag*KinEn + Diss(nshell)
  UVmod = dble( conjg(UsPlane(0,0))*VsPlane(0,0) + UsPlane(0,0)*conjg(VsPlane(0,0)) )*(0.5d0)
  Energy12(nshell) = UVmod + Energy12(nshell)
  UWmod = dble( conjg(UsPlane(0,0))*WsPlane(0,0) + UsPlane(0,0)*conjg(WsPlane(0,0)) )*(0.5d0)
  Energy31(nshell) = UWmod + Energy31(nshell)
  VWmod = dble( conjg(VsPlane(0,0))*WsPlane(0,0) + VsPlane(0,0)*conjg(WsPlane(0,0)) )*(0.5d0)
  Energy23(nshell) = VWmod + Energy23(nshell)
 enddo !End Loop Over Planes
  if (debug) call check_point("Spectra3d#2",.false.)
 k=1
 UsPlane(:,:)=uhat(:,:,k)
 VsPlane(:,:)=vhat(:,:,k)
 WsPlane(:,:)=what(:,:,k)

  k=1
  kw=k-1
  do j=0,nyp2-1
   do i=1,nxp2/2-1
    Rmag   = kx(i)**2 + ky(j)**2 + kz(kw)**2
    nshell = (idint(2.0d0*dsqrt(Rmag))+1)/2
    Umod   = dble( conjg( UsPlane(i,j) ) * UsPlane(i,j) )
    Vmod   = dble( conjg( VsPlane(i,j) ) * VsPlane(i,j) )
    Wmod   = dble( conjg( WsPlane(i,j) ) * WsPlane(i,j) )
    Energy11(nshell) = Umod + Energy11(nshell)
    Energy22(nshell) = Vmod + Energy22(nshell)
    Energy33(nshell) = Wmod + Energy33(nshell)
    KinEn = Umod + Vmod + Wmod
    Energy(nshell)   = Energy(nshell)  + KinEn 
    Diss(nshell)   = Rmag*( KinEn ) + Diss(nshell)
    UVmod = dble( conjg(UsPlane(i,j))*VsPlane(i,j) + UsPlane(i,j)*conjg(VsPlane(i,j)) )*(0.5d0)
    Energy12(nshell) = UVmod + Energy12(nshell)
    UWmod = dble( conjg(UsPlane(i,j))*WsPlane(i,j) + UsPlane(i,j)*conjg(WsPlane(i,j)) )*(0.5d0) 
    Energy31(nshell) = UWmod + Energy31(nshell)
    VWmod = dble( conjg(VsPlane(i,j))*WsPlane(i,j) + VsPlane(i,j)*conjg(WsPlane(i,j)) )*(0.5d0) 
    Energy23(nshell) = VWmod + Energy23(nshell)
   enddo
  enddo
  !Where kx=0
  do j = 1,nyp2/2-1
   Rmag   =  ky(j)**2 + kz(kw)**2
   nshell = (idint(2.0d0*dsqrt(Rmag))+1)/2
   Umod   = dble( conjg( UsPlane(0,j) ) * UsPlane(0,j) )
   Vmod   = dble( conjg( VsPlane(0,j) ) * VsPlane(0,j) )
   Wmod   = dble( conjg( WsPlane(0,j) ) * WsPlane(0,j) )
   Energy11(nshell) = Umod + Energy11(nshell)
   Energy22(nshell) = Vmod + Energy22(nshell)
   Energy33(nshell) = Wmod + Energy33(nshell)
   KinEn  = Umod + Vmod + Wmod
   Energy(nshell) = Energy(nshell) + KinEn
   Diss(nshell)   = Rmag*KinEn + Diss(nshell)
   UVmod = dble( conjg(UsPlane(0,j))*VsPlane(0,j) + UsPlane(0,j)*conjg(VsPlane(0,j)) )*(0.5d0)
   Energy12(nshell) = UVmod + Energy12(nshell)
   UWmod = dble( conjg(UsPlane(0,j))*WsPlane(0,j) + UsPlane(0,j)*conjg(WsPlane(0,j)) )*(0.5d0)
   Energy31(nshell) = UWmod + Energy31(nshell)
   VWmod = dble( conjg(VsPlane(0,j))*WsPlane(0,j) + VsPlane(0,j)*conjg(WsPlane(0,j)) )*(0.5d0)
   Energy23(nshell) = VWmod + Energy23(nshell)
  enddo
  !Where kx=ky=0
  Rmag   =   kz(kw)**2
  nshell = (idint(2.0d0*dsqrt(Rmag))+1)/2
  Umod   = 0.5d0*dble( conjg( UsPlane(0,0) ) * UsPlane(0,0) )
  Vmod   = 0.5d0*dble( conjg( VsPlane(0,0) ) * VsPlane(0,0) )
  Wmod   = 0.5d0*dble( conjg( WsPlane(0,0) ) * WsPlane(0,0) )
  Energy11(nshell) = Umod + Energy11(nshell)
  Energy22(nshell) = Vmod + Energy22(nshell)
  Energy33(nshell) = Wmod + Energy33(nshell)
  KinEn  = Umod + Vmod + Wmod
  Energy(nshell) = Energy(nshell) + KinEn 
  Diss(nshell)   = Rmag*KinEn + Diss(nshell)
  UVmod = dble( conjg(UsPlane(0,0))*VsPlane(0,0) + UsPlane(0,0)*conjg(VsPlane(0,0)) )*(0.5d0)
  Energy12(nshell) = UVmod + Energy12(nshell)
  UWmod = dble( conjg(UsPlane(0,0))*WsPlane(0,0) + UsPlane(0,0)*conjg(WsPlane(0,0)) )*(0.5d0)
  Energy31(nshell) = UWmod + Energy31(nshell)
  VWmod = dble( conjg(VsPlane(0,0))*WsPlane(0,0) + VsPlane(0,0)*conjg(WsPlane(0,0)) )*(0.5d0)
  Energy23(nshell) = VWmod + Energy23(nshell)
 k=nzp2
 UsPlane(:,:)=uhat(:,:,k)
 VsPlane(:,:)=vhat(:,:,k)
 WsPlane(:,:)=what(:,:,k)
  k=nzp2
  kw=nzp2-1
  do j=0,nyp2-1
   do i=1,nxp2/2-1
    Rmag   = kx(i)**2 + ky(j)**2 + kz(kw)**2
    nshell = (idint(2.0d0*dsqrt(Rmag))+1)/2
    Umod   = dble( conjg( UsPlane(i,j) ) * UsPlane(i,j) )
    Vmod   = dble( conjg( VsPlane(i,j) ) * VsPlane(i,j) )
    Wmod   = dble( conjg( WsPlane(i,j) ) * WsPlane(i,j) )
    Energy11(nshell) = Umod + Energy11(nshell)
    Energy22(nshell) = Vmod + Energy22(nshell)
    Energy33(nshell) = Wmod + Energy33(nshell)
    KinEn = Umod + Vmod + Wmod
    Energy(nshell)   = Energy(nshell)  + KinEn 
    Diss(nshell)   = Rmag*( KinEn ) + Diss(nshell)
    UVmod = dble( conjg(UsPlane(i,j))*VsPlane(i,j) + UsPlane(i,j)*conjg(VsPlane(i,j)) )*(0.5d0)
    Energy12(nshell) = UVmod + Energy12(nshell)
    UWmod = dble( conjg(UsPlane(i,j))*WsPlane(i,j) + UsPlane(i,j)*conjg(WsPlane(i,j)) )*(0.5d0) 
    Energy31(nshell) = UWmod + Energy31(nshell)
    VWmod = dble( conjg(VsPlane(i,j))*WsPlane(i,j) + VsPlane(i,j)*conjg(WsPlane(i,j)) )*(0.5d0) 
    Energy23(nshell) = VWmod + Energy23(nshell)
   enddo
  enddo
  !Where kx=0
  do j = 1,nyp2/2-1
   Rmag   =  ky(j)**2 + kz(kw)**2
   nshell = (idint(2.0d0*dsqrt(Rmag))+1)/2
   Umod   = dble( conjg( UsPlane(0,j) ) * UsPlane(0,j) )
   Vmod   = dble( conjg( VsPlane(0,j) ) * VsPlane(0,j) )
   Wmod   = dble( conjg( WsPlane(0,j) ) * WsPlane(0,j) )
   Energy11(nshell) = Umod + Energy11(nshell)
   Energy22(nshell) = Vmod + Energy22(nshell)
   Energy33(nshell) = Wmod + Energy33(nshell)
   KinEn  = Umod + Vmod + Wmod
   Energy(nshell) = Energy(nshell) + KinEn
   Diss(nshell)   = Rmag*KinEn + Diss(nshell)
   UVmod = dble( conjg(UsPlane(0,j))*VsPlane(0,j) + UsPlane(0,j)*conjg(VsPlane(0,j)) )*(0.5d0)
   Energy12(nshell) = UVmod + Energy12(nshell)
   UWmod = dble( conjg(UsPlane(0,j))*WsPlane(0,j) + UsPlane(0,j)*conjg(WsPlane(0,j)) )*(0.5d0)
   Energy31(nshell) = UWmod + Energy31(nshell)
   VWmod = dble( conjg(VsPlane(0,j))*WsPlane(0,j) + VsPlane(0,j)*conjg(WsPlane(0,j)) )*(0.5d0)
   Energy23(nshell) = VWmod + Energy23(nshell)
  enddo
  !Where kx=ky=0
  Rmag   =   kz(kw)**2
  nshell = (idint(2.0d0*dsqrt(Rmag))+1)/2
  Umod   = 0.5d0*dble( conjg( UsPlane(0,0) ) * UsPlane(0,0) )
  Vmod   = 0.5d0*dble( conjg( VsPlane(0,0) ) * VsPlane(0,0) )
  Wmod   = 0.5d0*dble( conjg( WsPlane(0,0) ) * WsPlane(0,0) )
  Energy11(nshell) = Umod + Energy11(nshell)
  Energy22(nshell) = Vmod + Energy22(nshell)
  Energy33(nshell) = Wmod + Energy33(nshell)
  KinEn  = Umod + Vmod + Wmod
  Energy(nshell) = Energy(nshell) + KinEn 
  Diss(nshell)   = Rmag*KinEn + Diss(nshell)
  UVmod = dble( conjg(UsPlane(0,0))*VsPlane(0,0) + UsPlane(0,0)*conjg(VsPlane(0,0)) )*(0.5d0)
  Energy12(nshell) = UVmod + Energy12(nshell)
  UWmod = dble( conjg(UsPlane(0,0))*WsPlane(0,0) + UsPlane(0,0)*conjg(WsPlane(0,0)) )*(0.5d0)
  Energy31(nshell) = UWmod + Energy31(nshell)
  VWmod = dble( conjg(VsPlane(0,0))*WsPlane(0,0) + VsPlane(0,0)*conjg(WsPlane(0,0)) )*(0.5d0)
  Energy23(nshell) = VWmod + Energy23(nshell)

  if (debug) call check_point("Spectra3d#3",.false.)
 if (debug) call check_point("Spectra3d#4",.false.)
  TotEn    = 0.d0
  Depsilon = 0.d0
  RS       = 0.d0
  urms     = 0.d0
  vrms     = 0.d0
  wrms     = 0.d0

  Energy12 = 2.d0*Energy12
  Energy23 = 2.d0*Energy23
  Energy31 = 2.d0*Energy31

 do i = 1,Nspec
  urms = urms + Energy11(i)
  vrms = vrms + Energy22(i)
  wrms = wrms + Energy33(i)
  RS(1) = RS(1) + Energy12(i)
  RS(2) = RS(2) + Energy23(i)
  RS(3) = RS(3) + Energy31(i)
  Depsilon = Depsilon + 2.d0*rRe*Diss(i)
  TotEn = TotEn + Energy11(i) + Energy22(i) + Energy33(i)
 end do

 urms = dsqrt(2.d0*urms)
 vrms = dsqrt(2.d0*vrms)
 wrms = dsqrt(2.d0*wrms)

 !CHECK RMS VELOCITIES
 testrms=min(urms,vrms,wrms)

 if (testrms.LT.1.d-15) then
  write(IOUT,*) "rms velocity=0.0 in Spectra3d.... something is likely wrong check ActualSpectrum.dat"
  acor=1.d15
 else
  acor(1) = RS(1)/(urms*vrms)
  acor(2) = RS(2)/(vrms*wrms)
  acor(3) = RS(3)/(urms*wrms)
 endif
 write(ICfile,'(a,i5.5,a)') trim(flowDIR)//trim(filename),nstep,'.dat'
  open(14, file = ICfile, status = 'unknown' , form = 'formatted',iostat=s1)
   if (s1.NE.0) write(IOUT,'(a20,a,a31,i4)') "ERROR Opening File: ",trim(filename)," in Spectra3d.f90 with iostat: ",s1

!   write(14,'(a1,11(a17))') '#','TotalEnergy','Dissipation','urms','vrms','wrms','uvrms','uwrms','uwrms',&
!                                'autocor(uv)','autocor(vw)','autocor(uw)'
!   write(14,'(a1,11(2x,E15.8E2))') "#", TotEn,Depsilon,urms,vrms,wrms, RS(1),RS(2),RS(3),acor(1),acor(2),acor(3)
  write(14,'(a1,a4,2(a17))') '#','k', 'Energy(k)','time'
 do i=1,Nspec
   write(14,'(i5,2(2x,E15.8E3))') i,Energy(i),time
  enddo
  close(unit=14)

 deallocate( Energy11, Energy22, Energy33, Energy12, Energy31, Energy23, Diss, Energy, &
             Etemp, UsPlane,VsPlane,WsPlane, STAT=stat )

 write(tmpstring,'(a,i5)') "Spectra3d#5",stat
 if (debug) call check_point(tmpstring,.false.)
return

end subroutine Spectra3d

subroutine random_number_gen_MS(randm)
!@t
! \textbf{subroutine random\_number\_gen\_MS(randm)}
!@h
!   Description:
!     Generates a uniformly distributed random number between 0 and 1 
!     using a minimum standard random number generator. 
!@q
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       08/2009  Original code. [Matt de Stadler] 
!@h
!   Comments:
!     The algorithm used is taken from real version 2 on p. 1196 of
!       Park and Miller, "Random Number Generators: Good Ones are Hard to
!       Find," Communications of the ACM, October 1988. 
!     This random number generator is portable on different hardware and 
!     with different compilers. It should be used as a replacement for the
!     intrinsic function random_number. A seed must be given before the 
!     first call to this subroutine.
!@q

 use ntypes, only: r8
 use fft,    only: seed
 implicit none

 ! Constants 
 real(r8), parameter  :: a = 16807.0d0, m = 2147483647.0d0, q = 127773.0d0, r=2836.0d0

 ! Passed Variables
 real(r8),intent(out) :: randm

 ! Local Variables
 real(r8)             :: lo, hi, testv

 hi = FLOOR( seed / q )
 lo = seed - q*hi
 testv = a * lo - r * hi
 if (testv .GT. 0.d0) then
   seed = testv
 else
   seed= testv + m
 endif

 Randm= seed/ m

 return
 end subroutine
