












subroutine mg_residual(phio,phin,relmax)
!@t
! \textbf{subroutine mg\_residual(phio,phin,relmax)}
!@h
!   Description:
!     Calculate the error between the new and old iterates of phi and 
!     save the new iterate into the phio array.
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
!     The residual is calculated by taking relmax = max(|phi-phio|)/max(phi)
!     over the entire domain. There is also code to output where the 
!     maximum value of the residual is occurring.
!@q

 use ntypes,    only: r8,i4
 use mgVars,    only: sxk,exk,syk,eyk,szk,ezk,ngrid
 implicit none

!Passed Variables
 real(r8),intent(out)      :: relmax
 real(r8),intent(inout)    :: phio(sxk(ngrid)-1:exk(ngrid)+1,syk(ngrid)-1:eyk(ngrid)+1,szk(ngrid)-1:ezk(ngrid)+1)
 real(r8),intent(in)       :: phin(sxk(ngrid)-1:exk(ngrid)+1,syk(ngrid)-1:eyk(ngrid)+1,szk(ngrid)-1:ezk(ngrid)+1)

!Local Variables
 integer(i4)               :: sxm,exm,sym,eym,szm,ezm
 real(r8)                  :: locval(2),totval(2)
 integer(i4)               :: i,j,k,ierr
 real(r8),parameter        :: small=1d-14
 integer(i4)               :: LocMaxRes(1:3)
 
!Indicies at grid level=ngrid
 sxm=sxk(ngrid)
 exm=exk(ngrid)
 sym=syk(ngrid)
 eym=eyk(ngrid)
 szm=szk(ngrid)
 ezm=ezk(ngrid)

!Calculate local error
 locval(1)=0.0d0
 locval(2)=0.0d0
 LocMaxRes=-1
 relmax =0.d0


 do k=szm,ezm
  do j=sym,eym
   do i=sxm,exm
    locval(1)=max(locval(1),dabs(phin(i,j,k)))
    locval(2)=max(locval(2),dabs(phin(i,j,k)-phio(i,j,k)))
!   locval(1)=locval(1) +  phin(i,j,k)**2
!   locval(2)=locval(2) +  phio(i,j,k)**2
!    if (dabs(locval(2)).GT.relmax) then
!      LocMaxRes(1) = i
!      LocMaxRes(2) = j
!      LocMaxRes(3) = k
!      relmax = locval(2)
!    endif
   enddo
  enddo
 enddo

!write (IOUT,'(a)')
!write(IOUT,'(a28,3(i4,a2),a3,3(f12.6,a2))') &
! "LOCATION of Maximum Residual: [i,j,k],[x,y,z]  [",LocMaxRes(1),',',LocMaxRes(2),',',LocMaxRes(3),']','/[',xc(LocMaxRes(1)),&
!    ',',yc(LocMaxRes(2)),',',zc(LocMaxRes(3)),']'
! write (IOUT,'(a)')

 totval=locval

 if (totval(1).gt.0.d0) then
  relmax=totval(2)/totval(1)
 else
  relmax=0.0d0
 endif

! relmax=dabs(totval(1)-totval(2))/(totval(1)+small)

!Save new values into output array
 do k=szm-1,ezm+1
  do j=sym-1,eym+1
   do i=sxm-1,exm+1
    phio(i,j,k)=phin(i,j,k)
   enddo
  enddo
 enddo

 return
end subroutine
