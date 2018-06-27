      subroutine mgdpfpde(sxf,exf,syf,eyf,szf,ezf,nxf,nyf,nzf,cofc1,cofc2,cofc3,cofc4,cofc5,cofc6,cofc7,r,xl,yl,zl,IOUT)
    
      use ntypes, only:i4,r8 
      include "mpif.h"
      integer  :: sxf,exf,syf,eyf,szf,ezf,nxf,nyf,nzf,IOUT
      real(r8) :: cofc1(sxf-1:exf+1,syf-1:eyf+1,szf-1:ezf+1)
      real(r8) :: cofc2(sxf-1:exf+1,syf-1:eyf+1,szf-1:ezf+1)
      real(r8) :: cofc3(sxf-1:exf+1,syf-1:eyf+1,szf-1:ezf+1)
      real(r8) :: cofc4(sxf-1:exf+1,syf-1:eyf+1,szf-1:ezf+1)
      real(r8) :: cofc5(sxf-1:exf+1,syf-1:eyf+1,szf-1:ezf+1)
      real(r8) :: cofc6(sxf-1:exf+1,syf-1:eyf+1,szf-1:ezf+1)
      real(r8) :: cofc7(sxf-1:exf+1,syf-1:eyf+1,szf-1:ezf+1)
      real(r8) :: r(sxf-1:exf+1,syf-1:eyf+1,szf-1:ezf+1),xl,yl,zl
!c------------------------------------------------------------------------
!c Determine coefficients for the pressure equation at the finest grid
!c level. These coefficients involve densities half-way between the
!c pressure and density nodes.
!c
!c cof array: 1 -> i-1
!c            2 -> i+1
!c            3 -> j-1
!c            4 -> j+1
!c            5 -> k-1
!c            6 -> k+1
!c            7 -> central
!c
!c Code      : mgd3, 3-D parallel multigrid solver
!c Author    : Bernard Bunner (bunner@engin.umich.edu), January 1998
!c Called in : mgdsolver
!c Calls     : --
!c------------------------------------------------------------------------
      real(r8)    :: dlx,todlxx,dly,todlyy,dlz,todlzz,rijk,c1,c2,c3,c4,c5,c6
      integer(i4) :: i,j,k

      dlx=xl/float(nxf-1)
      todlxx=1.0d0/(dlx*dlx)
      dly=yl/float(nyf-1)
      todlyy=1.0d0/(dly*dly)
      dlz=zl/float(nzf-1)
      todlzz=1.0d0/(dlz*dlz)
      do k=szf,ezf
        do j=syf,eyf
          do i=sxf,exf
            rijk=r(i,j,k)
            c1=todlxx
            c2=todlxx
            c3=todlyy
            c4=todlyy
            c5=todlzz*0.5*(r(i,j,k)+r(i,j,k-1))/r(i,j,k-1)
            c6=todlzz*0.5*(r(i,j,k)+r(i,j,k+1))/r(i,j,k+1)
            cofc1(i,j,k)=c1
            cofc2(i,j,k)=c2
            cofc3(i,j,k)=c3
            cofc4(i,j,k)=c4
            cofc5(i,j,k)=c5
            cofc6(i,j,k)=c6
            cofc7(i,j,k)=-(c1+c2+c3+c4+(todlzz*0.5*(r(i,j,k)+r(i,j,k-1))/r(i,j,k))+(todlzz*0.5*(r(i,j,k)+r(i,j,k+1))/r(i,j,k)))
          end do
        end do
      end do

      return
      end

