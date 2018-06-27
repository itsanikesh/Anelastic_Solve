












! =======================================================
! *******************************************************
! ************ Versions with IB optimization ************
! *******************************************************
! =======================================================

subroutine thomas_MPI_forward_z_pc_3leg(a,b,c,G1,G2,G3,stat)
! downsweep
 use dd
 use ntypes
 use Domain, only: sx,ex,sy,ey,sz,ez
 use flow,   only: r_tmp1
 implicit none

!Passed Variables
 integer(i4),intent(out) :: stat  ! 0=sucess 
 real(r8),intent(in)     :: a(sz-1:ez+1),b(sz-1:ez+1),c(sz-1:ez+1)  ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
 real(r8),intent(inout)  :: G1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)  :: G2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)  :: G3(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer(i4)             :: i,j,k,of1,n1
 integer                 :: status1(MPI_STATUS_SIZE)
 real(r8)                :: ocpack(sx-1:ex+1,4), icpack(sx-1:ex+1,4)
 real(r8)                :: anew, ct(sx-1:ex+1,sz-1:ez+1), &
                              onemct(sx-1:ex+1,sz-1:ez+1)
 stat=0
 n1 = ex-sx+3
 ct=0.d0
 r_tmp1=0.d0

 of1=0
 if (coords(3).eq.sizex3-1) of1=1 ! update value at bottom of matrix

 do j = sy,ey
     do i=sx-1,ex+1
       r_tmp1(i,j,sz-1)=b(sz-1)
     enddo

 onemct = 1.d0-ct

   if (coords(3).ne.0) then !1   ! not the first block    
     call MPI_RECV(ocpack,4*n1,MPI_DOUBLE_PRECISION,nbrx3m1,12,comm3d,status1,stat)
                                                  
     ! unpack diagonal and rhs entries
     r_tmp1(:,j,sz-1)=ocpack(:,1)
         G1(:,j,sz-1)=ocpack(:,2)
         G2(:,j,sz-1)=ocpack(:,3)
         G3(:,j,sz-1)=ocpack(:,4)
   endif !1

   do k = sz-1,ez-1+of1
    do i = sx-1,ex+1
     anew       = (-a(k+1) / r_tmp1(i,j,k))*onemct(i,k+1)
     r_tmp1(i,j,k+1) =  b(k+1)*onemct(i,k+1)+ct(i,k+1) + anew*c(k)    *onemct(i,k)
         G1(i,j,k+1) =  G1(i,j,k+1)                    + anew*G1(i,j,k)
         G2(i,j,k+1) =  G2(i,j,k+1)                    + anew*G2(i,j,k)
         G3(i,j,k+1) =  G3(i,j,k+1)                    + anew*G3(i,j,k)
    enddo
   enddo
 
  if (coords(3).ne.sizex3-1) then               ! send data
   icpack(:,1)=r_tmp1(:,j,ez) 
   icpack(:,2)=    G1(:,j,ez)
   icpack(:,3)=    G2(:,j,ez)
   icpack(:,4)=    G3(:,j,ez)
   call MPI_SEND(icpack,4*n1,MPI_DOUBLE_PRECISION,nbrx3p1,12,comm3d,stat)
  endif    

 enddo

 return
end subroutine thomas_MPI_forward_z_pc_3leg

subroutine thomas_MPI_backward_z_pc_3leg(c,G1,G2,G3,stat)
! solve and then upsweep
 use dd
 use ntypes
 use Domain,   only: sx,ex,sy,ey,sz,ez
 use flow,   only: r_tmp1
 implicit none

!Passed Variables
 integer(i4),intent(out)  :: stat  ! 0=sucess 
 real(r8),intent(in)      :: c(sz-1:ez+1)  ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
 real(r8),intent(inout)   :: G1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)   :: G2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(inout)   :: G3(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer(i4)              :: j,k,n1,of1
 integer                  :: status1(MPI_STATUS_SIZE)
 real(r8)                 :: ocpack(sx-1:ex+1,4), icpack(sx-1:ex+1,4)
 real(r8)                 :: ct(sx-1:ex+1,sz-1:ez+1)
 stat = 0
 n1 = ex-sx+3
 ct = 0.d0

 of1=0
 if (coords(3).eq.0) of1=1 ! update value at bottom of matrix
                           ! not needed for dns code but useful for debugging
 do j = sy,ey

  if (coords(3).ne.sizex3-1) then !3            ! send data
    call MPI_RECV(ocpack,4*n1,MPI_DOUBLE_PRECISION,nbrx3p1,13,comm3d,status1,stat)
                                                  
    ! unpack diagonal and rhs, note that c is not updated
    r_tmp1(:,j,ez+1)=ocpack(:,1)
        G1(:,j,ez+1)=ocpack(:,2)
        G2(:,j,ez+1)=ocpack(:,3)
        G3(:,j,ez+1)=ocpack(:,4)
  endif !3

  ! solve at x3max 
  if (coords(3).eq.sizex3-1) then 
    G1(:,j,ez+1) = G1(:,j,ez+1) / r_tmp1(:,j,ez+1)
    G2(:,j,ez+1) = G2(:,j,ez+1) / r_tmp1(:,j,ez+1)
    G3(:,j,ez+1) = G3(:,j,ez+1) / r_tmp1(:,j,ez+1)
  endif

  ! sweep back up
  do k = ez,sz-of1,-1
    G1(:,j,k) = (G1(:,j,k) - c(k)*G1(:,j,k+1)*(1.d0-ct(:,k))) / r_tmp1(:,j,k)
    G2(:,j,k) = (G2(:,j,k) - c(k)*G2(:,j,k+1)*(1.d0-ct(:,k))) / r_tmp1(:,j,k)
    G3(:,j,k) = (G3(:,j,k) - c(k)*G3(:,j,k+1)*(1.d0-ct(:,k))) / r_tmp1(:,j,k)
  enddo

  if (coords(3).ne.0) then !             ! send data
    icpack(:,1)=r_tmp1(:,j,sz) 
    icpack(:,2)=    G1(:,j,sz)
    icpack(:,3)=    G2(:,j,sz)
    icpack(:,4)=    G3(:,j,sz)
    call MPI_SEND(icpack,4*n1,MPI_DOUBLE_PRECISION,nbrx3m1,13,comm3d,stat)
  endif
 enddo

 return
end subroutine thomas_MPI_backward_z_pc_3leg

subroutine thomas_MPI_forward_z_pc(a,b,c,G,stat)
! downsweep
 use dd
 use ntypes
 use Domain, only: sx,ex,sy,ey,sz,ez
 use flow,   only: r_tmp1
 implicit none

!Passed Variables
 integer(i4),intent(out) :: stat  ! 0=sucess 
 real(r8),intent(in)     :: a(sz-1:ez+1),b(sz-1:ez+1),c(sz-1:ez+1)  ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
 real(r8),intent(inout)  :: G(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer(i4)             :: i,j,k,of1,n1
 integer                 :: status1(MPI_STATUS_SIZE)
 real(r8)                :: ocpack(sx-1:ex+1,2), icpack(sx-1:ex+1,2)
 real(r8)                :: anew, ct(sx-1:ex+1,sz-1:ez+1), &
                              onemct(sx-1:ex+1,sz-1:ez+1)
 stat=0
 n1 = ex-sx+3
 ct=0.d0
 r_tmp1=0.d0

 of1=0
 if (coords(3).eq.sizex3-1) of1=1 ! update value at bottom of matrix

 do j = sy,ey
     do i=sx-1,ex+1
       r_tmp1(i,j,sz-1)=b(sz-1)
     enddo

 onemct = 1.d0-ct

   if (coords(3).ne.0) then !1   ! not the first block    
     call MPI_RECV(ocpack,2*n1,MPI_DOUBLE_PRECISION,nbrx3m1,12,comm3d,status1,stat)
                                                  
     ! unpack diagonal and rhs entries
     r_tmp1(:,j,sz-1)=ocpack(:,1)
          G(:,j,sz-1)=ocpack(:,2)
   endif !1

   do k = sz-1,ez-1+of1
   do i = sx-1,ex+1
     anew       = (-a(k+1) / r_tmp1(i,j,k))*onemct(i,k+1)
     r_tmp1(i,j,k+1) =  b(k+1)*onemct(i,k+1)+ct(i,k+1) + anew*c(k)    *onemct(i,k)
          G(i,j,k+1) =  G(i,j,k+1)                     + anew*G(i,j,k)
   enddo
   enddo
 
  if (coords(3).ne.sizex3-1) then               ! send data
   icpack(:,1)=r_tmp1(:,j,ez) 
   icpack(:,2)=     G(:,j,ez)
   call MPI_SEND(icpack,2*n1,MPI_DOUBLE_PRECISION,nbrx3p1,12,comm3d,stat)
  endif    

 enddo

 return
end subroutine thomas_MPI_forward_z_pc

subroutine thomas_MPI_backward_z_pc(c,G,stat)
! solve and then upsweep
 use dd
 use ntypes
 use Domain,   only: sx,ex,sy,ey,sz,ez
 use flow,   only: r_tmp1
 implicit none

!Passed Variables
 integer(i4),intent(out)  :: stat  ! 0=sucess 
 real(r8),intent(in)      :: c(sz-1:ez+1)  ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
 real(r8),intent(inout)   :: G(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer(i4)              :: j,k,n1,of1
 integer                  :: status1(MPI_STATUS_SIZE)
 real(r8)                 :: ocpack(sx-1:ex+1,2), icpack(sx-1:ex+1,2)
 real(r8)                 :: ct(sx-1:ex+1,sz-1:ez+1)
 stat = 0
 n1 = ex-sx+3
 ct = 0.d0

 of1=0
 if (coords(3).eq.0) of1=1 ! update value at bottom of matrix
                           ! not needed for dns code but useful for debugging
 do j = sy,ey

  if (coords(3).ne.sizex3-1) then !3            ! send data
    call MPI_RECV(ocpack,2*n1,MPI_DOUBLE_PRECISION,nbrx3p1,13,comm3d,status1,stat)
                                                  
    ! unpack diagonal and rhs, note that c is not updated
    r_tmp1(:,j,ez+1)=ocpack(:,1)
    G(:,j,ez+1)=ocpack(:,2)
  endif !3

  ! solve at x3max 
  if (coords(3).eq.sizex3-1) G(:,j,ez+1) = G(:,j,ez+1) / r_tmp1(:,j,ez+1)

  ! sweep back up
  do k = ez,sz-of1,-1
    G(:,j,k) = (G(:,j,k) - c(k)*G(:,j,k+1)*(1.d0-ct(:,k))) / r_tmp1(:,j,k)
  enddo

  if (coords(3).ne.0) then !             ! send data
    icpack(:,1)=r_tmp1(:,j,sz) 
    icpack(:,2)=     G(:,j,sz)
    call MPI_SEND(icpack,2*n1,MPI_DOUBLE_PRECISION,nbrx3m1,13,comm3d,stat)
  endif
 enddo

!****************************************************
!***************** upsweep finished *****************
!****************************************************
 return
end subroutine thomas_MPI_backward_z_pc

subroutine Thomas_periodic_parallel_2Dz_pc(a,b,c,rhs,stat)
!@t
! \textbf{subroutine Thomas(a,b,c,G,n)}
!@h
!   Description:
!     Solve a Tridiagonal system of equations using the Thomas Algorithm.
!     This was adapted from C Pozrikidis's book "Numerical Computation in 
!     Science and Engineering", II edition, 2008. See section 3.7 page 237
!@h
!   Comments:
!     Note that c is not modified within the subroutine. The definition
!     of the input variables is given below.
!
!   (A|g) = | b1 c1             a1  | g1 | j=1
!           | a2 b2 c2              | g2 | j=2
!           |    .  .  .            |    |  .
!           |      .  .  .          |    |  .
!           |        aN-1 bN-1 cN-1 |    | j=n-1
!           | cN          aN   bN   | gN | j=n
!@q
 use ntypes
 use dd
 use Domain, only: sx,ex,sy,ey,sz,ez
 use flow,   only: r_tmp1
 implicit none

!Passed Variables
 integer(i4),intent(out) :: stat  ! 0=sucess 
 real(r8),intent(in)     :: a(sz-1:ez+1),b(sz-1:ez+1),c(sz-1:ez+1)  ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
 real(r8),intent(inout)  :: rhs(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 real(r8),allocatable,dimension(:,:,:) ::  G1
 integer                 :: status1(MPI_STATUS_SIZE),Status2(MPI_STATUS_SIZE)
 real(r8)                :: ocpack(sx-1:ex+1,sy-1:ey+1,2), icpack(sx-1:ex+1,sy-1:ey+1,2)
 integer(i4)             :: k, ok1, ok2,ok3,ok4,s1,msgL
 real(r8),dimension(sx-1:ex+1,sy-1:ey+1) :: r0,r1,xN,aN,bN,cN,rhsN,rhs_2,G1_2,ct,onemct,onemct2

 allocate( G1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), stat=s1 )
 
 msgL=(ex-sx+3)*(ey-sy+3)
 ct=0.d0
 onemct2=1.d0
 onemct = 1.d0-ct

 ok1=0
 ok2=0
 ok3=0
 ok4=0

 if (coords(3).eq.sizex3-1) then
     aN =   a(ez)*onemct
     bN =   b(ez)*onemct
     cN =   c(ez)*onemct
   rhsN = rhs(:,:,ez)
 endif

 ! guess xN = 0 and solve the system Ax=B of size n-3
 ! not needed since xN=0 leads to no changes
 ! if (coords(3).eq.0)        rhs(:,:,2)    = rhs(:,:,2)    - a(:,:,2)   *xN(:,:)
 ! if (coords(3).eq.sizex3-1) rhs(:,:,n3-2) = rhs(:,:,n3-2) - c(:,:,n3-3)*xN(:,:)

! guess xN = 1 and solve the system Ax=B of size n-3
 G1 = rhs


 if (coords(3).eq.0)        G1(:,:,sz)   = G1(:,:,sz)   - a(sz)  *onemct  !*xN(:,:)!xN=1
 if (coords(3).eq.sizex3-1) G1(:,:,ez-1) = G1(:,:,ez-1) - c(ez-2)*onemct2 !*xN(:,:)!xN=1
 
 ! solve for both guesses.           note that this solves from 2:n3-2
 call thomas_MPI_forward_z_2leg_pc( a,b,c,rhs,G1,ok1)
 call thomas_MPI_backward_z_2leg_pc(    c,rhs,G1,ok1)
 call MPI_BARRIER(commx3,ok1)
 
 ! pack rhs(2) and G1(2)
  if (coords(3).eq.0) then !             ! send data
   icpack(:,:,1)=rhs(:,:,sz)
   icpack(:,:,2)= G1(:,:,sz)
   call MPI_SEND(icpack,2*msgL,MPI_DOUBLE_PRECISION,sizex3-1,12,commx3,stat)
  endif !3
  
  if (coords(3).eq.sizex3-1) then !3            ! send data
   call MPI_RECV(ocpack,2*msgL,MPI_DOUBLE_PRECISION,0,12,commx3,status1,stat)
   ! unpack 
   rhs_2(:,:)=ocpack(:,:,1)
    G1_2(:,:)=ocpack(:,:,2)

   r0(:,:) = rhsN(:,:) - cN(:,:)*rhs_2(:,:) - aN(:,:)*rhs(:,:,ez-1) !- bN(:,:)*0.d0 ! xN=0
   r1(:,:) = rhsN(:,:) - cN(:,:)* G1_2(:,:) - aN(:,:)* G1(:,:,ez-1) - bN(:,:)!*1.d0 ! xN=1

   ! compute xN
   xN(:,:) = r0(:,:)/(r0(:,:) - r1(:,:))
  endif
  
  ! broadcast xN to all nodes
  call mpi_bcast(xN,msgL,realtype,sizex3-1,commx3,ok2)

! update solution as linear combination 
 do k=sz,ez
  rhs(:,:,k) = xN(:,:) * G1(:,:,k) + (1.d0-xN(:,:)) * rhs(:,:,k)
 end do
 
 if (coords(3).eq.sizex3-1) rhs(:,:,ez) = xN(:,:)

! enforce periodicity 
   if (rankx3.EQ.0) then  
    bN(:,:) = rhs(:,:,sz)  ! bN is the send plane, aN is the received plane
    call mpi_sendrecv(bN,msgL,realtype,sizex3-1,1,aN,msgL,realtype,sizex3-1,2,commx3,Status2,ok3)
    rhs(:,:,sz-1) = aN(:,:)
   endif
   if (rankx3.EQ.sizex3-1) then
    bN(:,:) = rhs(:,:,ez)
    call mpi_sendrecv(bN,msgL,realtype,0,2,aN,msgL,realtype,0,1,commx3,Status2,ok4)
    rhs(:,:,ez+1) = aN(:,:)
   endif
 
 deallocate(G1,stat=s1)
 stat=max(ok1,ok2,ok3,ok4)
 return
end subroutine Thomas_periodic_parallel_2Dz_pc

subroutine thomas_MPI_forward_z_2leg_pc(a,b,c,G1,G2,stat)
 ! perform downsweep
 use dd
 use ntypes
 use Domain, only: sx,ex,sy,ey,sz,ez
 use flow,   only: r_tmp1
 implicit none

!Passed Variables
 integer(i4),intent(out) :: stat  ! 0=sucess 
 real(r8),intent(in)     :: a(sz-1:ez+1),b(sz-1:ez+1),c(sz-1:ez+1)  ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
 real(r8),intent(inout)  :: G1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), &
                            G2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer(i4)             :: i,j,k,of1,of2,n1
 integer                 :: status1(MPI_STATUS_SIZE)
 real(r8)                :: ocpack(sx-1:ex+1,3), icpack(sx-1:ex+1,3)
 real(r8)                :: anew, ct(sx-1:ex+1,sz-1:ez+1), &
                              onemct(sx-1:ex+1,sz-1:ez+1)
 stat=0
 n1=ex-sx+3
 ct=0.d0
 r_tmp1=0.d0

 of1=0
 of2=0
 if (coords(3).eq.0       ) of1=1 ! update value at bottom of matrix
 if (coords(3).eq.sizex3-1) of2=-1 ! update value at bottom of matrix

 do j = sy,ey
     do i=sx,ex
       r_tmp1(i,j,sz-1+of1)=b(sz-1+of1)
     enddo
! set values for enforcing IB values: a=0,b=1,c=0
 onemct = 1.d0-ct

   if (coords(3).ne.0) then !1   ! not the first block    
     call MPI_RECV(ocpack,3*n1,MPI_DOUBLE_PRECISION,nbrx3m1,12,comm3d,status1,stat)
                                                  
     ! unpack diagonal and rhs entries
     r_tmp1(:,j,sz-1)=ocpack(:,1)
         G1(:,j,sz-1)=ocpack(:,2)
         G2(:,j,sz-1)=ocpack(:,3)
   endif !1

   do k = sz-1+of1,ez-1+of2
   do i = sx,ex
     anew       = (-a(k+1) / r_tmp1(i,j,k))*onemct(i,k+1)
     r_tmp1(i,j,k+1) =  b(k+1)*onemct(i,k+1)+ct(i,k+1) + anew*c(k)    *onemct(i,k)
         G1(i,j,k+1) =  G1(i,j,k+1)                    + anew*G1(i,j,k)
         G2(i,j,k+1) =  G2(i,j,k+1)                    + anew*G2(i,j,k)
   enddo
   enddo
 
  if (coords(3).ne.sizex3-1) then               ! send data
   icpack(:,1)=r_tmp1(:,j,ez) 
   icpack(:,2)=    G1(:,j,ez)
   icpack(:,3)=    G2(:,j,ez)
   call MPI_SEND(icpack,3*n1,MPI_DOUBLE_PRECISION,nbrx3p1,12,comm3d,stat)
  endif    
 enddo

 return
end subroutine thomas_MPI_forward_z_2leg_pc

subroutine thomas_MPI_backward_z_2leg_pc(c,G1,G2,stat)
 ! solve and backsweep
 use dd
 use ntypes
 use Domain,   only: sx,ex,sy,ey,sz,ez
 use flow,   only: r_tmp1
 implicit none

!Passed Variables
 integer(i4),intent(out) :: stat  ! 0=sucess 
 real(r8),intent(in)     :: c(sz-1:ez+1)  ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
 real(r8),intent(inout)  :: G1(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), &
                            G2(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
!Local Variables
 integer(i4)             :: j,k,n1,of1
 integer                 :: status1(MPI_STATUS_SIZE)
 real(r8)                :: ocpack(sx-1:ex+1,3), icpack(sx-1:ex+1,3)
 real(r8)                :: ct(sx-1:ex+1,sz-1:ez+1)

 stat = 0
 n1=ex-sx+3
 of1=0
 ct = 0.d0
 if (coords(3).eq.sizex3-1) of1=2 ! start at ez-2 instead of ez for last proc
 
 do j = sy,ey
! set values for enforcing IB values: a=0,b=1,c=0
  if (coords(3).ne.sizex3-1) then !3            ! send data
    call MPI_RECV(ocpack,3*n1,MPI_DOUBLE_PRECISION,nbrx3p1,13,comm3d,status1,stat)
                                                  
    ! unpack diagonal and rhs, note that c is not updated
    r_tmp1(:,j,ez+1)=ocpack(:,1)
        G1(:,j,ez+1)=ocpack(:,2)
        G2(:,j,ez+1)=ocpack(:,3)
  endif !3

  ! solve at x3max 
  if (coords(3).eq.sizex3-1) G1(:,j,ez-1) = G1(:,j,ez-1) / r_tmp1(:,j,ez-1)
  if (coords(3).eq.sizex3-1) G2(:,j,ez-1) = G2(:,j,ez-1) / r_tmp1(:,j,ez-1)

  ! sweep back up
  do k = ez-of1,sz,-1
    G1(:,j,k) = (G1(:,j,k) - c(k)*G1(:,j,k+1)*(1.d0-ct(:,k))) / r_tmp1(:,j,k)
    G2(:,j,k) = (G2(:,j,k) - c(k)*G2(:,j,k+1)*(1.d0-ct(:,k))) / r_tmp1(:,j,k)
  enddo

  if (coords(3).ne.0) then !             ! send data
    icpack(:,1)=r_tmp1(:,j,sz) 
    icpack(:,2)=    G1(:,j,sz)
    icpack(:,3)=    G2(:,j,sz)
    call MPI_SEND(icpack,3*n1,MPI_DOUBLE_PRECISION,nbrx3m1,13,comm3d,stat)
  endif
 enddo

 return
end subroutine thomas_MPI_backward_z_2leg_pc


! ==================================================================
! ******************************************************************
! ************************ General versions ************************
! ******************************************************************
! ==================================================================

subroutine thomas_MPI_forward_2d_z(a,b,c,G,stat)
 use dd
 use ntypes
 use Domain,   only: sx,ex,sy,ey,sz,ez
 implicit none

!Passed Variables
 integer(i4),intent(out)  :: stat  ! 0=sucess 
 real(r8),intent(inout)      :: c(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)  ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
 real(r8),intent(inout)   :: a(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1),&
                             b(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1),&
                             G(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer(i4)              :: i,j,k,n1,of1
 integer                  :: status1(MPI_STATUS_SIZE)
 real(r8)                 :: ocpack(sx-1:ex+1,2), icpack(sx-1:ex+1,2)

 of1=0
 if (coords(3).eq.sizex3-1) of1=1 ! update value at bottom of matrix
 stat=0
 n1 = ex-sx+3

 do i = sx,ex
 do j = sy,ey

   if (coords(3).ne.0) then !1   ! not the first block    
     call MPI_RECV(ocpack,2*n1,MPI_DOUBLE_PRECISION,nbrx3m1,12,comm3d,status1,stat)
                                                  
     ! unpack diagonal and rhs entries
     b(i,j,sz-1)=ocpack(i,1)
     G(i,j,sz-1)=ocpack(i,2)
   endif !1

   do k = sz-1,ez-1+of1
     a(i,j,k+1) = -a(i,j,k+1) / b(i,j,k)
     b(i,j,k+1) =  b(i,j,k+1) + a(i,j,k+1)*c(i,j,k)
     G(i,j,k+1) =  G(i,j,k+1) + a(i,j,k+1)*G(i,j,k)
   enddo
 
  if (coords(3).ne.sizex3-1) then               ! send data
   icpack(i,1)=b(i,j,ez) 
   icpack(i,2)=G(i,j,ez)
   call MPI_SEND(icpack,2*n1,MPI_DOUBLE_PRECISION,nbrx3p1,12,comm3d,stat)
  endif
  enddo    
 enddo
!****************************************************
!**************** downsweep finished ****************
!****************************************************
 return
end subroutine thomas_MPI_forward_2d_z

subroutine thomas_MPI_backward_2d_z(b,c,G,stat)
 use dd
 use ntypes
 use Domain,   only: sx,ex,sy,ey,sz,ez
 implicit none

!Passed Variables
 integer(i4),intent(out)  :: stat  ! 0=sucess 
 real(r8),intent(inout)      :: c(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)  ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
 real(r8),intent(inout)   :: b(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1),&
                             G(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer(i4)              :: i,j,k,n1,of1
 integer                  :: status1(MPI_STATUS_SIZE)
 real(r8)                 :: ocpack(sx-1:ex+1,2), icpack(sx-1:ex+1,2)

 stat = 0
 n1 = ex-sx+3
 of1=0
 if (coords(3).eq.0) of1=1      ! update value at top of matrix

do i = sx,ex
 do j = sy,ey

  if (coords(3).ne.sizex3-1) then !3            ! send data
    call MPI_RECV(ocpack,2*n1,MPI_DOUBLE_PRECISION,nbrx3p1,13,comm3d,status1,stat)
                                                  
    ! unpack diagonal and rhs, note that c is not updated
    b(i,j,ez+1)=ocpack(i,1)
    G(i,j,ez+1)=ocpack(i,2)
  endif !3

  ! solve at x3max 
  if (coords(3).eq.sizex3-1) G(i,j,ez+1) = G(i,j,ez+1) / b(i,j,ez+1)

  ! sweep back up
  do k = ez,sz-of1,-1
    G(i,j,k) = (G(i,j,k) - c(i,j,k) * G(i,j,k+1)) / b(i,j,k)
  enddo

  if (coords(3).ne.0) then !             ! send data
    icpack(i,1)=b(i,j,sz) 
    icpack(i,2)=G(i,j,sz)
    call MPI_SEND(icpack,2*n1,MPI_DOUBLE_PRECISION,nbrx3m1,13,comm3d,stat)
  endif
  enddo
 enddo

!****************************************************
!***************** upsweep finished *****************
!****************************************************
 return
end subroutine thomas_MPI_backward_2d_z

subroutine Thomas_periodic_parallel_2Dz(a,b,c,rhs,n1,n2,n3,stat)
!@t
! \textbf{subroutine Thomas(a,b,c,G,n)}
!@h
!   Description:
!     Solve a Tridiagonal system of equations using the Thomas Algorithm.
!     This was adapted from C Pozrikidis's book "Numerical Computation in 
!     Science and Engineering", II edition, 2008. See section 3.7 page 237
!@h
!   Comments:
!     Note that c is not modified within the subroutine. The definition
!     of the input variables is given below.
!
!   (A|g) = | b1 c1             a1  | g1 | j=1
!           | a2 b2 c2              | g2 | j=2
!           |    .  .  .            |    |  .
!           |      .  .  .          |    |  .
!           |        aN-1 bN-1 cN-1 |    | j=n-1
!           | cN          aN   bN   | gN | j=n
!@q

 use dd
 use ntypes
 implicit none

!Passed Variables
 integer(i4),intent(in)    :: n1,n2,n3
 integer(i4),intent(out)   :: stat  ! 0=sucess 
 real(r8),intent(in)       :: c(n1,n2,n3)  ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
 real(r8),intent(inout)    :: a(n1,n2,n3),b(n1,n2,n3), rhs(n1,n2,n3)

!Local Variables
 integer                   :: status1(MPI_STATUS_SIZE),Status2(MPI_STATUS_SIZE)
 real(r8)                  :: ocpack(n1,n2,2), icpack(n1,n2,2)
 real(r8),allocatable,dimension(:,:,:) ::  G1
 integer(i4)               :: j, ok1, ok2,ok3,ok4,s1
 real(r8),dimension(n1,n2) :: r0,r1,xN,aN,bN,cN,rhsN,rhs_2,G1_2

 allocate( G1(n1,n2,n3), stat=s1 )

ok1=0
ok2=0
ok3=0
ok4=0

 if (coords(3).eq.sizex3-1) then
     aN =   a(:,:,n3-1)
     bN =   b(:,:,n3-1)
     cN =   c(:,:,n3-1)
   rhsN = rhs(:,:,n3-1)
 endif

 ! guess xN = 0 and solve the system Ax=B of size n-3
 ! not needed since xN=0 leads to no changes
! xN = 0.d0
! if (coords(3).eq.0)        rhs(:,:,2)    = rhs(:,:,2)    - a(:,:,2)   *xN(:,:)
! if (coords(3).eq.sizex3-1) rhs(:,:,n3-2) = rhs(:,:,n3-2) - c(:,:,n3-3)*xN(:,:)

! guess xN = 1 and solve the system Ax=B of size n-3
 G1 = rhs
 if (coords(3).eq.0)        G1(:,:,2)    = G1(:,:,2)    - a(:,:,2)   !*xN(:,:)!xN=1
 if (coords(3).eq.sizex3-1) G1(:,:,n3-2) = G1(:,:,n3-2) - c(:,:,n3-3)!*xN(:,:)!xN=1
 
 ! solve for both guesses.           note that this solves from 2:n3-2
 call thomas_MPI_forward_2d_z_2leg( a,b,c,rhs,G1,n1,n2,n3,ok1)
 call thomas_MPI_backward_2d_z_2leg(  b,c,rhs,G1,n1,n2,n3,ok1)
 call MPI_BARRIER(commx3,ok1)
 
 ! pack rhs(2) and G1(2)
  if (coords(3).eq.0) then !             ! send data
   icpack(:,:,1)=rhs(:,:,2)
   icpack(:,:,2)= G1(:,:,2)
   if (coords(3).eq.0) call MPI_SEND(icpack,2*n1*n2,MPI_DOUBLE_PRECISION,sizex3-1,12,commx3,stat)
  endif !3
  
  if (coords(3).eq.sizex3-1) then !3            ! send data
   call MPI_RECV(ocpack,2*n1*n2,MPI_DOUBLE_PRECISION,0,12,commx3,status1,stat)
   ! unpack 
   rhs_2(:,:)=ocpack(:,:,1)
    G1_2(:,:)=ocpack(:,:,2)

   r0(:,:) = rhsN(:,:) - cN(:,:)*rhs_2(:,:) - aN(:,:)*rhs(:,:,n3-2) !- bN(:,:)*0.d0 ! xN=0
   r1(:,:) = rhsN(:,:) - cN(:,:)* G1_2(:,:) - aN(:,:)* G1(:,:,n3-2) - bN(:,:)!*1.d0 ! xN=1

   ! compute xN
   xN(:,:) = r0(:,:)/(r0(:,:) - r1(:,:))
  endif
  
  ! broadcast xN to all nodes
  call mpi_bcast(xN,n1*n2,realtype,sizex3-1,commx3,ok2)

! update solution as linear combination 
 do j=2,n3-1 
  rhs(:,:,j) = xN(:,:) * G1(:,:,j) + (1.d0-xN(:,:)) * rhs(:,:,j)
 end do
 
 if (coords(3).eq.sizex3-1) rhs(:,:,n3-1) = xN(:,:)

! enforce periodicity 
   if (rankx3.EQ.0) then  
    bN(:,:) = rhs(:,:,2)  ! bN is the send plane, aN is the received plane
    call mpi_sendrecv(bN,n1*n2,realtype,sizex3-1,1,aN,n1*n2,realtype,sizex3-1,2,commx3,Status2,ok3)
    rhs(:,:,1) = aN(:,:)
   endif
   if (rankx3.EQ.sizex3-1) then
    bN(:,:) = rhs(:,:,n3-1)
    call mpi_sendrecv(bN,n1*n2,realtype,0,2,aN,n1*n2,realtype,0,1,commx3,Status2,ok4)
    rhs(:,:,n3) = aN(:,:)
   endif
 
 deallocate(G1,stat=s1)
 stat=max(ok1,ok2,ok3,ok4)
 return
end subroutine Thomas_periodic_parallel_2Dz

subroutine thomas_MPI_forward_2d_z_2leg(a,b,c,G1,G2,n1,n2,n3,stat)
 use dd
 use ntypes
 implicit none

!Passed Variables
 integer(i4),intent(in)   :: n1,n2,n3     
 integer(i4),intent(out)  :: stat  ! 0=sucess 
 real(r8),intent(in)      :: c(n1,n2,n3)  ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
 real(r8),intent(inout)   :: a(n1,n2,n3),b(n1,n2,n3), G1(n1,n2,n3), G2(n1,n2,n3)

!Local Variables
 integer(i4)              :: i,k
 integer                  :: status1(MPI_STATUS_SIZE)
 real(r8)                 :: ocpack(n1,3), icpack(n1,3)

 stat=0

 do k = 1,n2
   if (coords(3).ne.0) then !1   ! not the first block    
     call MPI_RECV(ocpack,3*n1,MPI_DOUBLE_PRECISION,nbrx3m1,12,comm3d,status1,stat)
                                                  
     ! unpack diagonal and rhs entries
      b(:,k,1)=ocpack(:,1)
     G1(:,k,1)=ocpack(:,2)
     G2(:,k,1)=ocpack(:,3)
   endif !1

  if (coords(3).eq.0) then               ! send data
   do i = 2,n3-2
     a(:,k,i+1)  = -a(:,k,i+1)  / b(:,k,i)
     b(:,k,i+1)  =  b(:,k,i+1)  + a(:,k,i+1)* c(:,k,i)
     G1(:,k,i+1) =  G1(:,k,i+1) + a(:,k,i+1)*G1(:,k,i)
     G2(:,k,i+1) =  G2(:,k,i+1) + a(:,k,i+1)*G2(:,k,i)
   enddo
  elseif (coords(3).eq.sizex3-1) then               ! send data
   do i = 1,n3-3
     a(:,k,i+1)  = -a(:,k,i+1)  / b(:,k,i)
     b(:,k,i+1)  =  b(:,k,i+1)  + a(:,k,i+1)* c(:,k,i)
     G1(:,k,i+1) =  G1(:,k,i+1) + a(:,k,i+1)*G1(:,k,i)
     G2(:,k,i+1) =  G2(:,k,i+1) + a(:,k,i+1)*G2(:,k,i)
   enddo
  else
   do i = 1,n3-2
     a(:,k,i+1)  = -a(:,k,i+1)  / b(:,k,i)
     b(:,k,i+1)  =  b(:,k,i+1)  + a(:,k,i+1)* c(:,k,i)
     G1(:,k,i+1) =  G1(:,k,i+1) + a(:,k,i+1)*G1(:,k,i)
     G2(:,k,i+1) =  G2(:,k,i+1) + a(:,k,i+1)*G2(:,k,i)
   enddo
  endif

  if (coords(3).ne.sizex3-1) then               ! send data
    icpack(:,1)=b(:,k,n3-1) 
    icpack(:,2)=G1(:,k,n3-1)
    icpack(:,3)=G2(:,k,n3-1)
    call MPI_SEND(icpack,3*n1,MPI_DOUBLE_PRECISION,nbrx3p1,12,comm3d,stat)
  endif    
 enddo
!****************************************************
!**************** downsweep finished ****************
!****************************************************
 return
end subroutine thomas_MPI_forward_2d_z_2leg

subroutine thomas_MPI_backward_2d_z_2leg(b,c,G1,G2,n1,n2,n3,stat)
 use dd
 use ntypes
 implicit none

!Passed Variables
 integer(i4),intent(in)   :: n1,n2,n3  
 integer(i4),intent(out)  :: stat  ! 0=sucess 
real(r8),intent(in)       :: c(n1,n2,n3)  ! note that a is the subdiagonal, b the diagonal and c the superdiagonal
real(r8),intent(inout)    :: b(n1,n2,n3), G1(n1,n2,n3), G2(n1,n2,n3)

!Local Variables
 integer(i4)              :: i,k 
 integer                  :: status1(MPI_STATUS_SIZE)
 real(r8)                 :: ocpack(n1,3), icpack(n1,3)

 stat = 0

 do k = 1,n2
  if (coords(3).ne.sizex3-1) then !3            ! send data
    call MPI_RECV(ocpack,3*n1,MPI_DOUBLE_PRECISION,nbrx3p1,13,comm3d,status1,stat)
                                                  
    ! unpack diagonal and rhs, note that c is not updated
     b(:,k,n3)=ocpack(:,1)
    G1(:,k,n3)=ocpack(:,2)
    G2(:,k,n3)=ocpack(:,3)
  endif !3

  ! solve at x3max 
  if (coords(3).eq.sizex3-1) then 
  ! solve at x3max 
    G1(:,k,n3-2) = G1(:,k,n3-2) / b(:,k,n3-2)
    G2(:,k,n3-2) = G2(:,k,n3-2) / b(:,k,n3-2)

    ! sweep back up
    do i = n3-3,2,-1
      G1(:,k,i) = (G1(:,k,i) - c(:,k,i) * G1(:,k,i+1)) / b(:,k,i)
      G2(:,k,i) = (G2(:,k,i) - c(:,k,i) * G2(:,k,i+1)) / b(:,k,i)
    enddo
  else
    ! sweep back up
    do i = n3-1,2,-1
      G1(:,k,i) = (G1(:,k,i) - c(:,k,i) * G1(:,k,i+1)) / b(:,k,i)
      G2(:,k,i) = (G2(:,k,i) - c(:,k,i) * G2(:,k,i+1)) / b(:,k,i)
    enddo
  endif

  if (coords(3).ne.0) then !             ! send data
    icpack(:,1)= b(:,k,2) 
    icpack(:,2)=G1(:,k,2)
    icpack(:,3)=G2(:,k,2)
    call MPI_SEND(icpack,3*n1,MPI_DOUBLE_PRECISION,nbrx3m1,13,comm3d,stat)
  endif
 enddo

!****************************************************
!***************** upsweep finished *****************
!****************************************************
 return
end subroutine thomas_MPI_backward_2d_z_2leg
