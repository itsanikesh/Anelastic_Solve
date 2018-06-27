












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
 integer(i4)       :: EU,EV,EW                     !1 if Vector Edge Node 0 otherwise, set in mpi_setup
 integer(i4)       :: jmax, jmin, kmax, kmin
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
 !rdxc_i(k) = 1/dxc_i(k)

 real(r8)          :: x0, y0, z0                   !Origin
 real(r8)          :: xL, yL, zL                   !Length

 real(r8),allocatable,dimension(:),target :: xe,xc,dxe,dxc
 real(r8),allocatable,dimension(:),target :: ye,yc,dye,dyc
 real(r8),allocatable,dimension(:),target :: ze,zc,dze,dzc
 real(r8),allocatable,dimension(:),target :: rdxe,rdxc
 real(r8),allocatable,dimension(:),target :: rdye,rdyc
 real(r8),allocatable,dimension(:),target :: rdze,rdzc
 !================== NARSIMHA'S ADDITION ==================
 real(r8),allocatable,dimension(:),target :: wtx,wty,wtz
 !================== NARSIMHA'S ADDITION ==================
 character(len=50)                        :: Gridtype(1:3),locX0,locY0,locZ0
end module Grid

!FLOW VARIABLES
module Flow
 use ntypes
 real(r8),allocatable,dimension(:,:,:)    :: u,v,w,p,rho
 real(r8),allocatable,dimension(:,:,:)    :: pc,Uc,Vc,Wc
 !================== NARSIMHA'S ADDITION ==================
 real(r8),allocatable,dimension(:,:)      :: Uex,Wex
 integer(i4)			          :: indexjk(1:514,1:386)
 real(r8),allocatable,dimension(:)        :: Um,Vm,Wm,Pm,ur,vr,wr,prms
 real(r8),allocatable,dimension(:)        :: uvm,uwm,vwm,dUdz,dVdz,dWdz
 real(r8),allocatable,dimension(:)        :: pcf_1,pcf_2,pcf_3,pcf_4,pcf_5,pcf_6
 integer(i4)                              :: useghost_Tag
 !================== NARSIMHA'S ADDITION ==================
 real(r8),allocatable,dimension(:)        :: rho_bg
 real(r8),allocatable,dimension(:,:,:)    :: u1_tmp2,u2_tmp2,u3_tmp2,u1_tmp3,u2_tmp3,u3_tmp3
 real(r8),allocatable,dimension(:,:,:)    :: r_tmp1,r_tmp2
end module Flow

!BOUNDARY CONDITIONS
module boundC
 use ntypes
 !TB/VB(direction,face,variable)
 integer(i4),dimension(1:3,1:2,1:7)   :: TB  !Type of BC
 real(r8),dimension(1:3,1:2,1:7)      :: VB  !Value of BC
 logical,dimension(1:3,1:2)           :: BND !Logical of BC (i.e. is it a xi(min/max)
 character(len=20)                    :: BCloc !location of boundary, wallBC between cells, edgeBC at cells
 logical                              :: BC_vary_in_time 
 real(r8),allocatable,dimension(:,:)  :: U_x1min,V_x1min,W_x1min,P_x1min,RHO_x1min
 real(r8),allocatable,dimension(:,:)  :: U_x1max,V_x1max,W_x1max,P_x1max,RHO_x1max
 real(r8),allocatable,dimension(:,:)  :: U_x2min,V_x2min,W_x2min,P_x2min,RHO_x2min
 real(r8),allocatable,dimension(:,:)  :: U_x2max,V_x2max,W_x2max,P_x2max,RHO_x2max
 real(r8),allocatable,dimension(:,:)  :: U_x3min,V_x3min,W_x3min,P_x3min,RHO_x3min
 real(r8),allocatable,dimension(:,:)  :: U_x3max,V_x3max,W_x3max,P_x3max,RHO_x3max
end module boundC

!SPONGE
module Spng
 use ntypes
 real(r8),allocatable,dimension(:,:)     :: phiX1, phiX2, phiX3       !Spatial Damping Profile/Function
 real(r8),allocatable,dimension(:,:,:,:) :: X1inf,X2inf,X3inf         !Free Stream Velocities and Densities
 real(r8),dimension(1:2,1:5)             :: SAmpX1, SAmpX2, SAmpx3    !Amplitude of corresponding phi
 integer(i4),dimension(1:2)              :: x1spng, x2spng, x3spng    !start indice of xi_min sponge region
end module Spng

!PARAMETERS
module spatWake
 use ntypes
 integer (i4)                        :: num_inlet_plns
 real(r8)                            :: spat_adj_time
 real(r8),allocatable,dimension(:,:) :: uinlet, vinlet, winlet, pinlet, rhoinlet
 logical                             :: inflow_vary_in_t
! real(r8),allocatable,dimension(:,:,:) :: vartavg,vartavg1,varprime,varprime1,meant,rmst
 real(r8),allocatable,dimension(:,:,:) :: meant,rmst
end module spatWake

!PARAMETERS
module Parameters
 use ntypes
 real(r8)                            :: g, g_orig, gt1, gt2,rho_0 
 real(r8)                            :: Re, Fr, Pr, rRe, rFr, rPr, deltat, timeprev,deltat1
 real(r8)                            :: time, delt, cfl, relax_time, Ddt_vel,Ddt_rho,dtmax,time1,time2 !time1,time2 are timers
 integer(i4)                         :: nstep, bstep, estep
 logical                             :: restart, icparam, init_stats,pzero, rhopzero, Spatial,Userdelt
 logical                             :: Rsponge, clip, IConly, RfixMean, turbICS, Combined
 character(len=100)                  :: flow_solver, flow_type,density_profile, rest_file_write, rest_file_load, rest_file_load_grid
 real(r8)                            :: cropP1,cropP1_grid,cropP2,denP1,denP2,denP3, RhoMIN, RhoMAX, DX3c
 real(r8)                            :: meanP1,meanP2,meanP3,meanP4, MX1c, MX2c, MX3c,tke_wake,ext_turb_frac, inter_wake_grid
 real(r8)                            :: k0, k1, eps0
 integer(i4)                         :: bbspect, rest_iter,rest_iter_grid,displn
end module Parameters
    
!IO
module IO
 use ntypes, only: i4, r8
 integer(i4)             :: IOUT,IOUT_MASTER,IOUT_SLAVE !Unit to write output 6 is screen
 character(len=100)      :: resultDIR,tempDIR,runDIR,ext,penDIR,plnDIR,statDIR
 character(len=100)      :: fieldDIR,flowDIR,bigDIR,smallDIR, gridDIR,MGDIR,relaxDIR,spatICDIR
 logical                 :: write_grid, write_weights, statbin, tkstat, IObig, indexflag,writeflag
 logical                 :: relax_stats, dat_header, ics_fin, write_spat_planes
 integer(i4)             :: wflow, smallflow, wstats, wstats_small,wplanes, wpencils, wtec, checkDIV, wpre_stats,w3dviz
 integer                 :: indexcounter
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

module wakeForcing
 use ntypes, only: i4,r8
 real(r8)                                :: ft1, ft2, momP1, forcingtime, r_mom_scale
 logical                                 :: add_wake_forcing, add_tke 
 integer(i4)                             :: counterf, n_its_forcing
end module wakeForcing

module statsvars
 use ntypes, only: r8
 real(r8),allocatable,dimension(:)      :: bxL, bxM, bxR, byL, byM, byR, bzL, bzM, bzR ! coefficients for calc derivatives on stretched grids
 real(r8),dimension(:),pointer          :: gc1,ge1,gc2,ge2 
 integer                                :: bnds(1:6)
 integer                                :: Srank
 character(len=10)                      :: statbase
end module statsvars

module ptester
 use ntypes
 logical                                 :: srcflucs, phiflucs    ! fluctations on or off
 character(len=40)                       :: testcase,testdir1D    ! preset test cases
 real(r8)                                :: psolvetime            ! time to solve pressure equation
 real(r8),allocatable,dimension(:,:,:)   :: phi,src,error         ! field variables
 real(r8)                                :: pi                    !CONSTANTS
 real(r8)                                :: residual              !MG OUTPUT
 integer(i4)                             :: iterations
 integer(i4)                             :: clock_start, clock_stop, clock_rate
 real(r8)                                :: exact,locE,globE,maxE !MG ERROR
 real(r8)                                :: cnst,cx,cy,cz,x,y,z   !TEST PROBLEM PARAMETERS 
 real(r8),parameter                      :: wk=1.d0,rro=1.d0
 real(r8)                                :: a1,b1,c1,rR            ! values from exact solutions
 character(len=25),parameter             :: iniFILE='ptest.ini'   ! ini file
end module ptester
!@q

!IMMERSED BOUNDARY METHOD
module ibm
 use ntypes
 logical                                   :: ibm_tag, search_tag
 integer(i4),allocatable,dimension(:,:,:)  :: cell_type    ! 0-FLUID, 1-SOLID, 2-IMMERSED BOUNDARY CELL
 integer(i4)                               :: num_IB_cells, num_solid_cells
 integer(i4),allocatable,dimension(:,:)    :: IB_cell_indices, V_cell_indices, solid_cell_indices,BCtype_IB
 !how many different segments are there on a IB so as to have different BC for each one
 integer(i4)                               :: numIBsegments
 real(r8),allocatable,dimension(:,:)       :: BCvalue_IB, IB_value
 real(r8),allocatable,dimension(:)         :: dx_V_PP, dy_V_PP, dz_V_PP, dist_IB_PP, dist_IP_IB, x_IP, y_IP, z_IP
 real(r8)                                  :: solid_value
end module ibm
