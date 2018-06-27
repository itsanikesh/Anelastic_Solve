      subroutine mgdphpde(sxm,exm,sym,eym,szm,ezm,nxm,nym,nzm,cofc1,cofc2,cofc3,cofc4,cofc5,cofc6,cofc7, &
                         sx,ex,sy,ey,sz,ez,nxf,nyf,nzf,r,bd,      &
                         xl,yl,zl,IOUT)
      use ntypes, only:i4,r8
      include "mpif.h"
      integer(i4):: sxm,exm,sym,eym,szm,ezm,nxm,nym,nzm
      integer(i4):: sx,ex,sy,ey,sz,ez,nxf,nyf,nzf,bd(26),IOUT
      real(r8) :: cofc1(sxm-1:exm+1,sym-1:eym+1,szm-1:ezm+1)
      real(r8) :: cofc2(sxm-1:exm+1,sym-1:eym+1,szm-1:ezm+1)
      real(r8) :: cofc3(sxm-1:exm+1,sym-1:eym+1,szm-1:ezm+1)
      real(r8) :: cofc4(sxm-1:exm+1,sym-1:eym+1,szm-1:ezm+1)
      real(r8) :: cofc5(sxm-1:exm+1,sym-1:eym+1,szm-1:ezm+1)
      real(r8) :: cofc6(sxm-1:exm+1,sym-1:eym+1,szm-1:ezm+1)
      real(r8) :: cofc7(sxm-1:exm+1,sym-1:eym+1,szm-1:ezm+1)
      real(r8) :: r(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1),xl,yl,zl
!c------------------------------------------------------------------------
!c For the new version of the multigrid code, determine the coefficients
!c for the pressure equation at all grid levels except the finest one.
!c The coefficients are determined directly from the density array r
!c through some manipulation of indices and are values at (i+1/2,j+1/2)
!c points. Works for periodic, Neumann, and Dirichlet boundary
!c conditions. Should work even when there is no coarsifying in one
!c direction.
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
      real(r8)     :: dlx,eidlxx,dly,eidlyy,dlz,eidlzz
      integer(i4)  :: i,j,k,im,jm,km,is,js,ks,istep,jstep,kstep
!# if cdebug
!      double precision tinitial
!      tinitial=MPI_WTIME()
!# endif
!c
!c calculate off-diagonal terms
!c
      dlx=xl/float(nxm-1)
      eidlxx=1.0d0/(dlx*dlx)
      dly=yl/float(nym-1)
      eidlyy=1.0d0/(dly*dly)
      dlz=zl/float(nzm-1)
      eidlzz=1.0d0/(dlz*dlz)
      istep=(nxf-1)/(nxm-1)
      jstep=(nyf-1)/(nym-1)
      kstep=(nzf-1)/(nzm-1)
      do k=szm,ezm
        km=2*kstep*k-3*(kstep-1)
        do j=sym,eym
          jm=2*jstep*j-3*(jstep-1)
          do i=sxm,exm
            im=2*istep*i-3*(istep-1)
            is=(im-istep)/2
            js=jm/2
            ks=km/2
            cofc1(i,j,k)=eidlxx
            is=(im+istep)/2
            cofc2(i,j,k)=eidlxx
            is=im/2
            js=(jm-jstep)/2
            cofc3(i,j,k)=eidlyy
            js=(jm-jstep)/2
            cofc4(i,j,k)=eidlyy
            js=jm/2
            ks=(km-kstep)/2
            cofc5(i,j,k)=eidlzz*0.5*(r(is,js,ks)+r(is,js,ks-1))/r(is,js,ks-1)
            ks=(km+kstep)/2
            cofc6(i,j,k)=eidlzz*0.5*(r(is,js,ks)+r(is,js,ks+1))/r(is,js,ks+1)
          end do
        end do
      end do
!c
!c enforce wall BCs
!c
      if (bd(1).eq.1) then
        do k=szm,ezm
          do i=sxm,exm
            cofc4(i,eym,k)=0.0d0
          end do
        end do
      end if
      if (bd(3).eq.1) then
        do j=sym,eym
          do i=sxm,exm
            cofc5(i,j,szm)=0.0d0
          end do
        end do
      end if
      if (bd(5).eq.1) then
        do k=szm,ezm
          do i=sxm,exm
            cofc3(i,sym,k)=0.0d0
          end do
        end do
      end if
      if (bd(7).eq.1) then
        do j=sym,eym
          do i=sxm,exm
            cofc6(i,j,ezm)=0.0d0
          end do
        end do
      end if
      if (bd(9).eq.1) then
        do k=szm,ezm
          do j=sym,eym
            cofc1(sxm,j,k)=0.0d0
          end do
        end do
      end if
      if (bd(18).eq.1) then
        do k=szm,ezm
          do j=sym,eym
            cofc2(exm,j,k)=0.0d0
          end do
        end do
      end if
!c
!c calculate diagonal term
!c
      do k=szm,ezm
        do j=sym,eym
          do i=sxm,exm
            cofc7(i,j,k)=-(cofc1(i,j,k)+cofc2(i,j,k)+cofc3(i,j,k)&
                         +cofc4(i,j,k)+(eidlzz*0.5*(r(is,js,ks)+r(is,js,ks-1))/r(is,js,ks))+(eidlzz*0.5*(r(is,js,ks)+r(is,js,ks+1))/r(is,js,ks)))
          end do
        end do
      end do
!c
!# if cdebug
!      timing(83)=timing(83)+MPI_WTIME()-tinitial
!# endif
      return
      end

