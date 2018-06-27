

!@c
!DATA TYPES
module ntypes
 integer, parameter :: r8=8 
 integer, parameter :: i4=4 
end module ntypes 

!DOMAIN
module Domain
 use ntypes
 integer(i4)       :: nx, ny, nz, nxp2, nyp2, nzp2 !# of Grid Points
 integer(i4)       :: sx,ex,sy,ey,sz,ez,cex        !Sweep indicies
 integer(i4)       :: EU,EV,EW                     !1 if Vector Edge Node 0 otherwise set in mpi_setup
end module Domain

!GRID
module Grid
 use ntypes
 !xe_i = edge distance
 !xc_i = center distance

 !xc_i(index)= ( xe_i(k)+xe_i(k-1) )/2.d0 !BY DEFINITION
 !dxe_i(k)=xc_i(k+1)-xc_i(k)             !BY DEFINITION
 !dxc_i(k)=xe_i(k)-xe_i(k-1)             !BY DEFINITION

 !rdxe_i(k) = 1/dxe_i(k)
 !rdxc_i(k) = 1/dce_i(k)

 real(r8)          :: x0, y0, z0                   !Origin
 real(r8)          :: xL, yL, zL                   !Length

 real(r8),allocatable,dimension(:),target :: xe,xc,dxe,dxc
 real(r8),allocatable,dimension(:),target :: ye,yc,dye,dyc
 real(r8),allocatable,dimension(:),target :: ze,zc,dze,dzc
 real(r8),allocatable,dimension(:),target :: rdxe,rdxc
 real(r8),allocatable,dimension(:),target :: rdye,rdyc
 real(r8),allocatable,dimension(:),target :: rdze,rdzc
 character(len=50)                        :: Gridtype(1:3),locX0,locY0,locZ0
end module Grid

!FLOW VARIABLES
module Flow
 use ntypes
 real(r8),allocatable,dimension(:,:,:)            :: u,v,w,p,rho,scal1
 real(r8),allocatable,dimension(:,:,:)            :: u1_tmp2
 real(r8),allocatable,dimension(:,:,:)            :: u2_tmp2
 real(r8),allocatable,dimension(:,:,:)            :: u3_tmp2
 real(r8),allocatable,dimension(:,:,:)            :: r_tmp1,r_tmp2,scal1_tmp1,scal1_tmp2
 character(len=80)   :: Vmodel, Rmodel, Smodel 

end module Flow

!LES VARIABLES
module LESmod
 use ntypes
 real(r8),allocatable,dimension(:,:,:) :: S11, S22, S33, S12, S13, S23, modS, delg, Csgs, CTsgs, CSsgs,nuT,kappaT,nappaT 
 real(r8),allocatable,dimension(:,:,:) :: un_gt, vn_gt, wn_gt, lestmp, lestmp1, lestmp2, lestmp3, lestmp4
 real(r8),allocatable,dimension(:,:,:) :: un_gtgt, vn_gtgt, wn_gtgt
 real(r8) :: r_dgt_dg, nu_sgs_max, kappa_sgs_max, nappa_sgs_max, Dscal1_sgs_max, SSM_const, Pr_sgs, Sc_sgs   
 real(r8) :: nu_sgs_min, kappa_sgs_min, nappa_sgs_min   
 integer :: xfil,yfil,zfil, les_start, les_freq
 character(len=80)   :: LES_grid_size
end module LESmod

!Time rate of change variables
module transient
 use ntypes
 real(r8),allocatable,dimension(:) :: transtmp, transtmp1, transtmp2, transtmp3, transtmp4
 real(r8),allocatable,dimension(:) :: rk_old, dkdt, rr_old, dr2dt
 real(r8),allocatable,dimension(:) :: rk_old2, dkdt_c, rr_old2, dr2dt_c
 real(r8),allocatable,dimension(:) :: R11_old, dR11dt, R22_old, dR22dt, R33_old, dR33dt
 real(r8),allocatable,dimension(:) :: R11_old2, dR11dt_c, R22_old2, dR22dt_c, R33_old2, dR33dt_c
 real(r8),allocatable,dimension(:) :: R12_old, dR12dt, R13_old, dR13dt, R23_old, dR23dt
 real(r8),allocatable,dimension(:) :: R12_old2, dR12dt_c, R13_old2, dR13dt_c, R23_old2, dR23dt_c
 real(r8),allocatable,dimension(:) :: RU1_old, dRU1dt, RU2_old, dRU2dt, RU3_old, dRU3dt
 real(r8),allocatable,dimension(:) :: RU1_old2, dRU1dt_c, RU2_old2, dRU2dt_c, RU3_old2, dRU3dt_c
end module transient

!BOUNDARY CONDITIONS
module boundC
 use ntypes
 !TB/VB(direction,face,variable)
 integer(i4),dimension(1:3,1:2,1:8)  :: TB  !Type of BC
 real(r8),dimension(1:3,1:2,1:8)     :: VB  !Value of BC
 logical,dimension(1:3,1:2)          :: BND !Logical of BC (i.e. is it a xi(min/max)
end module boundC

!SPONGE
module Spng
 use ntypes
 real(r8),allocatable,dimension(:,:)     :: phiX1, phiX2, phiX3       !Spatial Damping Profile/Function
 real(r8),allocatable,dimension(:,:,:,:) :: X1inf,X2inf,X3inf         !Free Stream Velocities and Densities
 real(r8),dimension(1:2,1:8)             :: SAmpX1, SAmpX2, SAmpx3    !Amplitude of corresponding phi
 integer(i4),dimension(1:2)              :: x1spng, x2spng, x3spng    !start indice of xi_min sponge region
end module Spng

!PARAMETERS
module Parameters
 use ntypes
 real(r8)                                :: g, g_orig, gt1, gt2
 real(r8)                                :: rho_0,scal1_0 
 real(r8)                                :: Re,Pr,Sc,rRe,rPr,rSc 
 real(r8)                                :: time, delt, cfl, relax_time, Ddt,dtmax
 integer(i4)                             :: nstep, bstep, estep, szint, ezint
 logical                                 :: restart, icparam, init_stats,pzero, Rsponge,clip, IConly, RfixMean, turbICS
 logical                                 :: RK3,RK3CN 
 character(len=80)                       :: flow_type,density_profile,scal1_profile,rest_file_write, rest_file_load 
 real(r8)                                :: cropP1,cropP2,denP1,denP2,denP3,rhoMIN,rhoMAX,DX3c
 real(r8)                                :: scal1P1,scal1P2,scal1P3,scal1MIN,scal1MAX,scal1X3c
 real(r8)                                :: meanP1,meanP2,meanP3,meanP4, MX1c, MX2c, MX3c 
 real(r8)                                :: k0, k1, eps0
 integer(i4)                             :: bbspect, rest_iter
end module Parameters

!Coriolis
module Coriolis_params
 use ntypes
 logical                                 :: coriolis
 real(r8)                                :: f_Cor 
end module Coriolis_params

!Langmuir
module Langmuir_params
 use ntypes
 logical                                 :: langmuir
 character(len=80)                       :: drift_type
 real(r8),allocatable,dimension(:,:)       :: u_stokes, v_stokes, dus_dz, dvs_dz
 real(r8)                                :: us_amp, vs_amp, drift_wlength, drift_amp, drift_wnumber
end module Langmuir_params
    
!IO
module IO
 use ntypes, only: i4, r8
 integer(i4)             :: IOUT,IOUT_MASTER,IOUT_SLAVE !Unit to write output 6 is screen
 character(len=100)      :: resultDIR,tempDIR,runDIR,ext,penDIR,plnDIR,statDIR,flowDIR, gridDIR,MGDIR,relaxDIR
 logical                 :: write_grid, write_sponge, write_weights, statbin, tkstat, IObig
 integer(i4)             :: wflow, wstats, wstats_small,wplanes, wpencils, wtec, checkDIV, wpre_stats
 integer(i4),allocatable,dimension(:)   :: iLines, jLines, kLines, iPlanes, jPlanes, kPlanes
 integer(i4)             :: niLines, njLines, nkLines, niPlanes, njPlanes, nkPlanes
 real(r8),allocatable,dimension(:,:,:) :: x1_planes, x2_planes, x3_planes
 real(r8),allocatable,dimension(:,:)   :: x1_pencils, x2_pencils, x3_pencils

end module IO

module ratios
 use ntypes, only: r8
 real(r8),parameter   :: r_9_16=9.d0/16.d0
 real(r8),parameter   :: r_1_16=1.d0/16.d0
 real(r8),parameter   :: r_4_3 =4.d0/3.d0
 real(r8),parameter   :: r_1_3 =1.d0/3.d0
 real(r8),parameter   :: r_1_2 =1.d0/2.d0
 real(r8),parameter   :: r_1_4 =1.d0/4.d0

end module ratios

module fft
 use ntypes, only : r8
 include "fftw3.f"
!FFT plans
 integer(8) :: plan2dF, plan2dB, plan1dF, plan1dB, plan3dF, plan3dB

!FFT VARIABLES
 !REAL FIELDS
 real(r8),allocatable,dimension(:,:,:)    :: FIELD_IN
 real(r8),allocatable,dimension(:,:)      :: PLANE_IN

 !COMPLEX FIELDS
 complex(r8),allocatable,dimension(:,:,:) :: FIELD_OUT
 complex(r8),allocatable,dimension(:,:)   :: PLANE_OUT
 complex(r8),allocatable,dimension(:)     :: PENCIL_IN,PENCIL_OUT
 complex(r8),allocatable,dimension(:,:,:) :: uhat,vhat,what

!WAVE INDICES  
 real(r8),allocatable,dimension(:)    :: kx, ky, kz

!WAVE NUMBERS 
 real(r8),allocatable,dimension(:)    :: rkx, rky, rkz

!MODIFIED WAVE NUMBERS
 real(r8),allocatable,dimension(:)    :: mkx, mky, mkz

! Seed for Random Number generator in isospec
 real(r8)                             :: seed
end module fft

module stats
 use ntypes, only: r8

 real(r8),dimension(:),pointer          :: gc1,ge1,gc2,ge2 
 integer                                :: bnds(1:6)
 integer                                :: Srank
 character(len=10)                      :: statbase
end module stats

module ptester
 use ntypes
 logical                                 :: srcflucs, phiflucs    ! fluctations on or off
 character(len=40)                       :: testcase,testdir1D ! preset test cases
 real(r8)                                :: psolvetime ! time to solve pressure equation
 real(r8),allocatable,dimension(:,:,:)   :: phi,src,error ! field variables
 real(r8)                                :: pi!CONSTANTS
 real(r8)                                :: residual !MG OUTPUT
 integer(i4)                             :: iterations
 integer(i4)                             :: clock_start, clock_stop, clock_rate
 real(r8)                                :: exact,locE,globE !MG ERROR
 real(r8)                                :: cnst,cx,cy,cz,x,y,z !TEST PROBLEM PARAMETERS 
 real(r8),parameter                      :: wk=1.d0,rro=1.d0
 real(r8)                                :: b1, c1, a1 ! values from exact solutions
 character(len=25),parameter             :: iniFILE='ptest.ini' ! ini file
end module ptester

!PARAMETERS
module spatWake
 use ntypes
 integer (i4)                        :: num_inlet_plns
 real(r8)                            :: spat_adj_time
 real(r8),allocatable,dimension(:,:) :: uinlet, vinlet, winlet, pinlet, rhoinlet
 logical                             :: inflow_vary_in_t
end module spatWake



!@q
