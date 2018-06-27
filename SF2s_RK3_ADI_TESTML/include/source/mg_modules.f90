












!@c
module mgPASS
 use ntypes,   only: i4, r8
 use GRID,     only: xe,xc,ye,yc,ze,zc,dxe,dxc,dye,dyc,dze,dzc
 use DOMAIN,   only: sx,ex,sy,ey,sz,ez,nx,ny,nz
 use IO,       only: mgDIR, IOUT
 use spatWake, only: pinlet
end module mgPASS
 
module mgVARS
 use ntypes, only: r8,i4

 integer(i4)                               :: nx1Fp,nx2Fp,nx3Fp                !#Fine Points
 integer(i4)                               :: nx1Cp,nx2Cp,nx3Cp                !#Course Points
 integer(i4)                               :: nx1Levels,nx2Levels,nx3Levels    !#MG Levels
 integer(i4)                               :: ngrid                            !max(nx1Levels,nx2Levels,nx3Levels)
 integer(i4)                               :: maxcy                            !maximum number of cycles 
 real(r8)                                  :: tolmax,omega                           !maximum tolerance
 character(len=20)                         :: Smoother                         !Smoother RBPGS/LJ  
 logical                                   :: strict                           !strict exchange on 1/2 sweep  
 logical                                   :: verbose                          !Verbose Output  
 integer(i4),allocatable,dimension(:)      :: ipre_relax                       !Number of pre-relax sweeps on each level
 integer(i4),allocatable,dimension(:)      :: ipost_relax                      !Number of post-relax sweeps on each level
 character(len=100)                        :: mgDIRmg                          !Directory for MGgrids
 integer(i4)                               :: tbc(1:3,1:2)                     !type of bc at each face
 integer(i4)                               :: bdMG(1:3,1:2)                    !node boundary
 real(r8)                                  :: vbc(1:3,1:2)                     !value of bc at each face
 real(r8),allocatable,dimension(:,:,:)     :: phibc                            !array to hold bcs at each grid level
 real(r8),allocatable,dimension(:),target  :: pcf_1, pcf_2, pcf_3              !Coefficients in smoother 
 real(r8),allocatable,dimension(:),target  :: pcf_4, pcf_5, pcf_6              !          
 real(r8),allocatable,dimension(:),target  :: xek, xck, dxck, dxek             !Grids at courser levels 
 real(r8),allocatable,dimension(:),target  :: yek, yck, dyck, dyek             !          
 real(r8),allocatable,dimension(:),target  :: zek, zck, dzck, dzek             !          
 real(r8),allocatable,dimension(:)         :: work1, work2                     !WORK arrays for the solver work1=solution work2=source
 integer(i4),allocatable,dimension(:)      :: nxk,nyk,nzk                      !Sizes at all levels
 integer(i4),allocatable,dimension(:)      :: sxk,exk,syk,eyk,szk,ezk          !Start and End indices at all levels
 integer(i4),allocatable,dimension(:)      :: kp3d, kpx1, kpx2, kpx3           !3d and 1d offsets in 1d arrays
 integer(i4)                               :: nwork3d,nworkx1,nworkx2,nworkx3  !size of 1d arrays
 integer(i4)                               :: IOUTmg                           !Unit to output messages (should already be opened)
 character(len=20)                         :: mg_BCloc                         !location of boundary, wallBC between cells, edgeBC at cells
 real(r8),allocatable,dimension(:,:)       :: p_x1min,p_x2min,p_x3min          !arrays for spatially varying BCs
 real(r8),allocatable,dimension(:,:)       :: p_x1max,p_x2max,p_x3max          !

end module mgVars
!@q
