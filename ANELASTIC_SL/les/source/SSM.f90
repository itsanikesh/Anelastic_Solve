












subroutine SSM
 use Flow
 use LESmod
 use ntypes
 use Grid
 use Domain,  only: sx,ex,sy,ey,sz,ez
 use IO, only: IOUT

 implicit none

!Local variables
 logical,parameter             :: debug=.false.
 integer :: i,j,k,err1
 real(r8) :: nu_sgs, tmp, tmp1, tmp2, del_g, Zo
 if (debug) call check_point('SSM#0',.false.)
 
 call straincal(u,v,w,err1)
 Zo = 0.3d0

 do k=sz,ez
  do j=sy,ey
   do i=sx,ex
    !Csgs(i,j,k) = SSM_const
     Csgs(i,j,k) = SSM_const*((0.41*(zc(k)+Zo))**2.d0/((0.41*(zc(k)+Zo))**2.d0+SSM_const*delg(i,j,k)**2.d0));
   enddo
  enddo
 enddo
 call ghost(Csgs,'cfluc',err1)

 call MaxMin(Csgs,tmp1,tmp2,err1)

 write(IOUT,*)"C_sgs_max = ", tmp1," min =", tmp2

return
end subroutine SSM

subroutine SSMrho
 use Flow
 use LESmod
 use ntypes
 use Grid
 use Domain,  only: sx,ex,sy,ey,sz,ez
 use IO, only: IOUT

 implicit none

!Local variables
 logical,parameter             :: debug=.false.
 integer :: i,j,k,err1
 real(r8) :: kappa_sgs, tmp, tmp1, tmp2, del_g, Zo
 
 if (debug) call check_point('SSM#0',.false.)
 
 Zo = 0.3

 do k=sz,ez
  do j=sy,ey
   do i=sx,ex
     !CTsgs(i,j,k) =  Csgs(i,j,k)/Pr_sgs
     CTsgs(i,j,k) =  Csgs(i,j,k)/Pr_sgs*((0.41*(zc(k)+Zo))**2.d0/((0.41*(zc(k)+Zo))**2.d0+SSM_const*delg(i,j,k)**2.d0));
   enddo
  enddo
 enddo
 call ghost(CTsgs,'cfluc',err1)
 
 call MaxMin(CTsgs,tmp1,tmp2,err1)

 write(IOUT,*)"CT_sgs_max = ", tmp1," min =", tmp2

return
end subroutine SSMrho


subroutine SSMscal1
 use Flow
 use LESmod
 use ntypes
 use Grid
 use Domain,  only: sx,ex,sy,ey,sz,ez
 use IO, only: IOUT

 implicit none

!Local variables
 logical,parameter             :: debug=.false.
 integer :: i,j,k,err1
 real(r8) :: nappa_sgs, tmp, tmp1, tmp2, del_g, Zo 
 
 if (debug) call check_point('SSM#0',.false.)
 Zo = 0.3d0
 
 do k=sz,ez
  do j=sy,ey
   do i=sx,ex
     !CSsgs(i,j,k) =  Csgs(i,j,k)/Sc_sgs*(1.d0-exp(-(zc(k)/0.3)**2.d0))
     CSsgs(i,j,k) =  Csgs(i,j,k)/Pr_sgs*((0.41*(zc(k)+Zo))**2.d0/((0.41*(zc(k)+Zo))**2.d0+SSM_const*delg(i,j,k)**2.d0));
   enddo
  enddo
 enddo
 call ghost(CSsgs,'cfluc',err1)

 call MaxMin(CSsgs,tmp1,tmp2,err1)

 write(IOUT,*)"CS_sgs_max = ", tmp1," min =", tmp2

return
end subroutine SSMscal1








