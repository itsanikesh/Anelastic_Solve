subroutine mg_cor(lC,lF,phif,phic)
!@t
! \textbf{subroutine mg\_cor(lC,lF,phif,phic)}
!@h
!   Description:
!     Add correction from coarse grid level to fine grid level, using 
!     volume weighting (also called linear,bilinear or trilinear 
!     interpolation depending on the number of dimensions). The algorithm
!     comes from "Numerical Computation in Science and Engineering" 1st Ed.
!     by C. Pozrikidis. It can be found on page 302 for the bilinear case.
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
!     Based on Bernard Bunner's (bunner@engin.umich.edu) mgd3 solver
!     available at www.mgnet.org. Assumes that the grid is uniform. This
!     should be ok for cases with mild stretching but it is definitely not
!     ok for large stretching. See further documentation for instructions
!     on how to correct for large stretching.

!     Full volume weighting for prolongation 2D example for updating point
!     F. The dotted lines represent a grid around a fine grid point (F).
!     The dotted lines represent a grid around a coarse grid point (C).

!              <------dx coarse ------->
!        <--dx fine-->
!        f . . . . . f . . + . . f
!        .                 +     .
!        .                 +     .
!  + + + + + + + + + + + + + + + + + + + + + + + + +
!  +     .                 +     .                 +
!  +     .                 +     .                 +
!  +     f           F     +     f                 +
!  +     .                 +     .                 +
!  +     .                 +     .                 +
!  +     .     C1          +     .     C2          +
!  +     .                 +     .                 +
!  +     .                 +     .                 +
!  +     f . . . . . f . . + . . f                 +
!  +                       +                       +
!  +                       +                       +
!  + + + + + + + + + + + + + + + + + + + + + + + + +

!     For the unstretched case C1 takes up 3/4 of the box in along dx and
!     3/4 of the box along dy for a total of 9/16ths of the box. C2 takes
!     up 1/4 of the box in along dx and 3/4 of the box along dy for a total
!     of 3/16.
!@q

 use ntypes, only: r8,i4
 use mgVars,    only: nxk,nyk,nzk,sxk,exk,syk,eyk,szk,ezk,strict

 implicit none

!Passed Variables
 integer(i4),intent(in)    :: lC,lF
 real(r8),intent(in)       :: phiC(sxk(lC)-1:exk(lC)+1,syk(lC)-1:&
                                      eyk(lC)+1,szk(lC)-1:ezk(lC)+1)
 real(r8),intent(out)      :: phiF(sxk(lF)-1:exk(lF)+1,syk(lF)-1:&
                                      eyk(lF)+1,szk(lF)-1:ezk(lF)+1)

!Local Varialbes
 integer(i4)               :: i,j,k,ic,jc,kc
 integer(i4)               :: sxf,exf,syf,eyf,szf,ezf,nxf,nyf,nzf
 integer(i4)               :: sxc,exc,syc,eyc,szc,ezc,nxc,nyc,nzc

 sxc=sxk(lC)
 exc=exk(lC)
 syc=syk(lC)
 eyc=eyk(lC)
 szc=szk(lC)
 ezc=ezk(lC)
 nxc=nxk(lC)
 nyc=nyk(lC)
 nzc=nzk(lC)

 sxf=sxk(lF)
 exf=exk(lF)
 syf=syk(lF)
 eyf=eyk(lF)
 szf=szk(lF)
 ezf=ezk(lF)
 nxf=nxk(lF)
 nyf=nyk(lF)
 nzf=nzk(lF)

!The correction is the weighted average of either two 
! or four points at the coarser grid level depending on whether 
! coarsening takes place in all directions or not

!First, general case where coarsening takes place in all directions;
! take the averages from the 8 surrounding points
      if ((nxc.lt.nxf).and.(nyc.lt.nyf).and.(nzc.lt.nzf)) then
        do kc=szc-1,ezc
          k=2*kc-1
          do jc=syc-1,eyc
            j=2*jc-1
            do ic=sxc-1,exc
              i=2*ic-1
              phif(i,j,k)=phif(i,j,k)+                                  &
                (27.0d0*phic(ic,jc,kc)+9.0d0*phic(ic,jc+1,kc)           &
                +9.0d0*phic(ic,jc,kc+1)+3.0d0*phic(ic,jc+1,kc+1)        &
                +9.0d0*phic(ic+1,jc,kc)+3.0d0*phic(ic+1,jc+1,kc)        &
                +3.0d0*phic(ic+1,jc,kc+1)+phic(ic+1,jc+1,kc+1))         &
                /64.0d0
              phif(i+1,j,k)=phif(i+1,j,k)+                              &
                (9.0d0*phic(ic,jc,kc)+3.0d0*phic(ic,jc+1,kc)            &
                +3.0d0*phic(ic,jc,kc+1)+phic(ic,jc+1,kc+1)              &
                +27.0d0*phic(ic+1,jc,kc)+9.0d0*phic(ic+1,jc+1,kc)       &
                +9.0d0*phic(ic+1,jc,kc+1)+3.0d0*phic(ic+1,jc+1,kc+1))   &
                /64.0d0
              phif(i,j+1,k)=phif(i,j+1,k)+                              &
                (9.0d0*phic(ic,jc,kc)+27.0d0*phic(ic,jc+1,kc)           &
                +3.0d0*phic(ic,jc,kc+1)+9.0d0*phic(ic,jc+1,kc+1)        &
                +3.0d0*phic(ic+1,jc,kc)+9.0d0*phic(ic+1,jc+1,kc)        &
                +phic(ic+1,jc,kc+1)+3.0d0*phic(ic+1,jc+1,kc+1))         &
                /64.0d0
              phif(i,j,k+1)=phif(i,j,k+1)+                              &
                (9.0d0*phic(ic,jc,kc)+3.0d0*phic(ic,jc+1,kc)            &
                +27.0d0*phic(ic,jc,kc+1)+9.0d0*phic(ic,jc+1,kc+1)       &
                +3.0d0*phic(ic+1,jc,kc)+phic(ic+1,jc+1,kc)              &
                +9.0d0*phic(ic+1,jc,kc+1)+3.0d0*phic(ic+1,jc+1,kc+1))   &
                /64.0d0
              phif(i+1,j+1,k)=phif(i+1,j+1,k)+                          &
                (3.0d0*phic(ic,jc,kc)+9.0d0*phic(ic,jc+1,kc)            &
                +phic(ic,jc,kc+1)+3.0d0*phic(ic,jc+1,kc+1)              &
                +9.0d0*phic(ic+1,jc,kc)+27.0d0*phic(ic+1,jc+1,kc)       &
                +3.0d0*phic(ic+1,jc,kc+1)+9.0d0*phic(ic+1,jc+1,kc+1))   &
                /64.0d0
              phif(i+1,j,k+1)=phif(i+1,j,k+1)+                          &
                (3.0d0*phic(ic,jc,kc)+phic(ic,jc+1,kc)                  &
                +9.0d0*phic(ic,jc,kc+1)+3.0d0*phic(ic,jc+1,kc+1)        &
                +9.0d0*phic(ic+1,jc,kc)+3.0d0*phic(ic+1,jc+1,kc)        &
                +27.0d0*phic(ic+1,jc,kc+1)+9.0d0*phic(ic+1,jc+1,kc+1))  &
                /64.0d0 
              phif(i,j+1,k+1)=phif(i,j+1,k+1)+                          &
                (3.0d0*phic(ic,jc,kc)+9.0d0*phic(ic,jc+1,kc)            &
                +9.0d0*phic(ic,jc,kc+1)+27.0d0*phic(ic,jc+1,kc+1)       &
                +phic(ic+1,jc,kc)+3.0d0*phic(ic+1,jc+1,kc)              &
                +3.0d0*phic(ic+1,jc,kc+1)+9.0d0*phic(ic+1,jc+1,kc+1))   &
                /64.0d0
              phif(i+1,j+1,k+1)=phif(i+1,j+1,k+1)+                      &
                (phic(ic,jc,kc)+3.0d0*phic(ic,jc+1,kc)                  &
                +3.0d0*phic(ic,jc,kc+1)+9.0d0*phic(ic,jc+1,kc+1)        &
                +3.0d0*phic(ic+1,jc,kc)+9.0d0*phic(ic+1,jc+1,kc)        &
                +9.0d0*phic(ic+1,jc,kc+1)+27.0d0*phic(ic+1,jc+1,kc+1))  &
                /64.0d0
            enddo
          enddo
        enddo
      else

!No coarsening in two directions; take the averages from the 2 
! surrounding points

        if ((nxf.eq.nxc).and.(nyf.eq.nyc)) then
          do kc=szc-1,ezc
            k=2*kc-1
            do j=syf-1,eyf+1
              jc=j
              do i=sxf-1,exf+1
                ic=i
                phif(i,j,k)=phif(i,j,k)+                                &
                 (3.0d0*phic(ic,jc,kc)+phic(ic,jc,kc+1))/4.0d0
                phif(i,j,k+1)=phif(i,j,k+1)+                            &
                 (phic(ic,jc,kc)+3.0d0*phic(ic,jc,kc+1))/4.0d0
              enddo
            enddo
          enddo
        else if ((nxf.eq.nxc).and.(nzf.eq.nzc)) then
          do k=szf-1,ezf+1
            kc=k
            do jc=syc-1,eyc
              j=2*jc-1
              do i=sxf-1,exf+1
                ic=i
                phif(i,j,k)=phif(i,j,k)+                                &
                 (3.0d0*phic(ic,jc,kc)+phic(ic,jc+1,kc))/4.0d0
                phif(i,j+1,k)=phif(i,j+1,k)+                            &
                 (phic(ic,jc,kc)+3.0d0*phic(ic,jc+1,kc))/4.0d0
              enddo
            enddo
          enddo
        else if ((nyf.eq.nyc).and.(nzf.eq.nzc)) then
          do k=szf-1,ezf+1
            kc=k
            do j=syf-1,eyf+1
              jc=j
              do ic=sxc-1,exc
                i=2*ic-1
                phif(i,j,k)=phif(i,j,k)+                                &
                 (3.0d0*phic(ic,jc,kc)+phic(ic+1,jc,kc))/4.0d0
                phif(i+1,j,k)=phif(i+1,j,k)+                            &
                 (phic(ic,jc,kc)+3.0d0*phic(ic+1,jc,kc))/4.0d0
              enddo
            enddo
          enddo

!No coarsening in one direction; take the averages from the 4
! surrounding points

        elseif (nxf.eq.nxc) then
          do kc=szc-1,ezc
            k=2*kc-1
            do jc=syc-1,eyc
              j=2*jc-1
              do i=sxf-1,exf+1
                ic=i
                phif(i,j,k)=phif(i,j,k)+                                &
                  (9.0d0*phic(ic,jc,kc)+3.0d0*phic(ic,jc+1,kc)          &
                  +3.0d0*phic(ic,jc,kc+1)+phic(ic,jc+1,kc+1))           &
                  /16.0d0
                phif(i,j+1,k)=phif(i,j+1,k)+                            &
                  (3.0d0*phic(ic,jc,kc)+9.0d0*phic(ic,jc+1,kc)          &
                  +phic(ic,jc,kc+1)+3.0d0*phic(ic,jc+1,kc+1))           &
                  /16.0d0
                phif(i,j,k+1)=phif(i,j,k+1)+                            &
                  (3.0d0*phic(ic,jc,kc)+phic(ic,jc+1,kc)                &
                  +9.0d0*phic(ic,jc,kc+1)+3.0d0*phic(ic,jc+1,kc+1))     &
                  /16.0d0 
                phif(i,j+1,k+1)=phif(i,j+1,k+1)+                        &
                  (phic(ic,jc,kc)+3.0d0*phic(ic,jc+1,kc)                &
                  +3.0d0*phic(ic,jc,kc+1)+9.0d0*phic(ic,jc+1,kc+1))     &
                  /16.0d0
              end do
            end do
          end do
        else if (nyf.eq.nyc) then
          do kc=szc-1,ezc
            k=2*kc-1
            do j=syf-1,eyf+1
              jc=j
              do ic=sxc-1,exc
                i=2*ic-1
                phif(i,j,k)=phif(i,j,k)+                                &
                  (9.0d0*phic(ic,jc,kc)+3.0d0*phic(ic+1,jc,kc)          &
                  +3.0d0*phic(ic,jc,kc+1)+phic(ic+1,jc,kc+1))           &
                  /16.0d0
                phif(i+1,j,k)=phif(i+1,j,k)+                            &
                  (3.0d0*phic(ic,jc,kc)+9.0d0*phic(ic+1,jc,kc)          &
                  +phic(ic,jc,kc+1)+3.0d0*phic(ic+1,jc,kc+1))           &
                  /16.0d0
                phif(i,j,k+1)=phif(i,j,k+1)+                            &
                  (3.0d0*phic(ic,jc,kc)+phic(ic+1,jc,kc)                &
                  +9.0d0*phic(ic,jc,kc+1)+3.0d0*phic(ic+1,jc,kc+1))     &
                  /16.0d0
                phif(i+1,j,k+1)=phif(i+1,j,k+1)+                        &
                  (phic(ic,jc,kc)+3.0d0*phic(ic+1,jc,kc)                &
                  +3.0d0*phic(ic,jc,kc+1)+9.0d0*phic(ic+1,jc,kc+1))     &
                  /16.0d0
              end do
            end do
          end do
        else if (nzf.eq.nzc) then
          do k=szf-1,ezf+1
            kc=k
            do jc=syc-1,eyc
              j=2*jc-1
              do ic=sxc-1,exc
                i=2*ic-1
                phif(i,j,k)=phif(i,j,k)+                                &
                  (9.0d0*phic(ic,jc,kc)+3.0d0*phic(ic+1,jc,kc)          &
                  +3.0d0*phic(ic,jc+1,kc)+phic(ic+1,jc+1,kc))           &
                  /16.0d0
                phif(i+1,j,k)=phif(i+1,j,k)+                            &
                  (3.0d0*phic(ic,jc,kc)+9.0d0*phic(ic+1,jc,kc)          &
                  +phic(ic,jc+1,kc)+3.0d0*phic(ic+1,jc+1,kc))           &
                  /16.0d0
                phif(i,j+1,k)=phif(i,j+1,k)+                            &
                  (3.0d0*phic(ic,jc,kc)+phic(ic+1,jc,kc)                &
                  +9.0d0*phic(ic,jc+1,kc)+3.0d0*phic(ic+1,jc+1,kc))     &
                  /16.0d0
                phif(i+1,j+1,k)=phif(i+1,j+1,k)+                        &
                  (phic(ic,jc,kc)+3.0d0*phic(ic+1,jc,kc)                &
                  +3.0d0*phic(ic,jc+1,kc)+9.0d0*phic(ic+1,jc+1,kc))     &
                /16.0d0
              end do
            end do
          end do
        end if
      end if

 !Impose Neumann and Dirichlet boundary conditions
 call mg_bdry(lF,phif,strict,.true.)

 return
end subroutine
