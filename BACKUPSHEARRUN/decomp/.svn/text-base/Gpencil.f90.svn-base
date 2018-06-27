subroutine Reduce_Pencil_to_Master(var,dir,index1,index2,Master,Pen,sp1,myidM,stat)
!@t
! \textbf{subroutine Reduce\_Pencil\_to\_Master(var,dir,index1,index2,Master,Pen,sp1,myidM,stat)}
!@h
!   Description:
!     Reduces a 1D pencil of data to the master node.
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
!     Returns Master to the calling program, which contains the global id
!     of the node containing the pencil, and Pen the pencil of data
!     corresponding to dir=1 index1=j index2=k, dir=2 index1=i index2=k,
!     dir=3 index1=i index2=k. MyidM should be zero since it is always the
!     master of the subcommunicator that the pencil is reduced to... 
!@q
 use ntypes, only: r8
 use dd,     only: rankx1,rankx2,rankx3,myid
 use Domain, only: sx,ex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none
 
!Passed Variables
 real(r8),intent(in)                :: var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(in)                 :: dir, sp1, index1, index2
 integer,intent(out)                :: Master
 real(r8),intent(out)               :: Pen(1:sp1)
 integer,intent(out)                :: stat
 integer,intent(in)                 :: myidM
!Local Variables
 integer                            :: Xmaster,Xnode
 Xmaster = -1
 Xnode   = -1
 Master = -1

 if (dir.EQ.1) then     !0 
 !sub-communicator is x1
  !CASE 1 Interior Pencil
  if (index1.GT.sy-1.AND.index1.LT.ey+1.AND.index2.GT.sz-1.AND.index2.LT.ez+1) then !1 
   if (rankx1.EQ.0) then 
    Xmaster = 1
    Master = myid
   else 
    Xnode   = 1
   endif 
  !CASE 2 x2-boundary x3-interior
  elseif ( ((index1.EQ.sy-1.AND.sy-1.EQ.1).OR.(index1.EQ.ey+1.AND.ey+1.EQ.nyp2)).AND. &
           ((index2.GT.sz-1).AND.(index2.LT.ez+1)) ) then !2
   if (rankx1.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 
 !CASE 3 x3-boundary x2-interior
  elseif ( ((index2.EQ.sz-1.AND.sz-1.EQ.1).OR.(index2.EQ.ez+1.AND.ez+1.EQ.nzp2)).AND. &
           ((index1.GT.sy-1).AND.(index1.LT.ey+1)) ) then !2
   if (rankx1.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 
 !CASE 4 x2-boundary x3-boundary (i.e. Corner)
  elseif ( ((index1.EQ.sy-1.AND.sy-1.EQ.1).OR.(index1.EQ.ey+1.AND.ey+1.EQ.nyp2)).AND. &
           ((index2.EQ.sz-1.AND.sz-1.EQ.1).OR.(index2.EQ.ez+1.AND.ez+1.EQ.nzp2)) ) then
   if (rankx1.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 
  elseif (index1.LT.1.OR.index1.GT.nyp2.OR.index2.LT.1.OR.index2.GT.nzp2) then !1 
    !This is an invalid plane 
   write(IOUT,'(a,i1,a,i4,a,i4,a)') "ILLEGAL INDEX REQUEST, DIRECTION:",dir,"either index1", index1,&
                  "is not between 1 and nyp2 or index2", index2, "is not between 1 and nzp2"
   goto 1000

  else !1 
    !This node does not contain this plane
   continue 
  endif !1

 elseif (dir.EQ.2) then     !0 
 !sub-communicator is x2
  !CASE 1 Interion Pencil
  if (index1.GT.sx-1.AND.index1.LT.ex+1.AND.index2.GT.sz-1.AND.index2.LT.ez+1) then !1 
   if (rankx2.EQ.0) then 
    Xmaster = 1
    Master = myid
   else 
    Xnode   = 1
   endif 
  !CASE 2 x1-boundary x3-interior
  elseif ( ((index1.EQ.sx-1.AND.sx-1.EQ.1).OR.(index1.EQ.ex+1.AND.ex+1.EQ.nxp2)).AND. &
           ((index2.GT.sz-1).AND.(index2.LT.ez+1)) ) then !2
   if (rankx2.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 
 !CASE 3 x3-boundary x1-interior
  elseif ( ((index2.EQ.sz-1.AND.sz-1.EQ.1).OR.(index2.EQ.ez+1.AND.ez+1.EQ.nzp2)).AND. &
           ((index1.GT.sx-1).AND.(index1.LT.ex+1)) ) then !2
   if (rankx2.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 
 !CASE 4 x1-boundary x3-boundary (i.e. Corner)
  elseif ( ((index1.EQ.sx-1.AND.sx-1.EQ.1).OR.(index1.EQ.ex+1.AND.ex+1.EQ.nxp2)).AND. &
           ((index2.EQ.sz-1.AND.sz-1.EQ.1).OR.(index2.EQ.ez+1.AND.ez+1.EQ.nzp2)) ) then
   if (rankx2.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 
  elseif (index1.LT.1.OR.index1.GT.nxp2.OR.index2.LT.1.OR.index2.GT.nzp2) then !1 
    !This is an invalid plane 
   write(IOUT,'(a,i1,a,i4,a,i4,a)') "ILLEGAL INDEX REQUEST, DIRECTION:",dir,"either index1", index1,&
                  "is not between 1 and nxp2 or index2", index2, "is not between 1 and nzp2"
   goto 1000

  else !1 
    !This node does not contain this plane
   continue 
  endif !1

 elseif (dir.EQ.3) then     !0 
 !sub-communicator is x3
  !CASE 1 Interion Pencil
  if (index1.GT.sx-1.AND.index1.LT.ex+1.AND.index2.GT.sy-1.AND.index2.LT.ey+1) then !1 
   if (rankx3.EQ.0) then 
    Xmaster = 1
    Master = myid
   else 
    Xnode   = 1
   endif 
  !CASE 2 x1-boundary x2-interior
  elseif ( ((index1.EQ.sx-1.AND.sx-1.EQ.1).OR.(index1.EQ.ex+1.AND.ex+1.EQ.nxp2)).AND. &
           ((index2.GT.sy-1).AND.(index2.LT.ey+1)) ) then !2
   if (rankx3.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 
 !CASE 3 x2-boundary x1-interior
  elseif ( ((index2.EQ.sy-1.AND.sy-1.EQ.1).OR.(index2.EQ.ey+1.AND.ey+1.EQ.nyp2)).AND. &
           ((index1.GT.sx-1).AND.(index1.LT.ex+1)) ) then !2
   if (rankx3.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 
 !CASE 4 x1-boundary x2-boundary (i.e. Corner)
  elseif ( ((index1.EQ.sx-1.AND.sx-1.EQ.1).OR.(index1.EQ.ex+1.AND.ex+1.EQ.nxp2)).AND. &
           ((index2.EQ.sy-1.AND.sy-1.EQ.1).OR.(index2.EQ.ey+1.AND.ey+1.EQ.nyp2)) ) then
   if (rankx3.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 
  elseif (index1.LT.1.OR.index1.GT.nxp2.OR.index2.LT.1.OR.index2.GT.nyp2) then !1 
    !This is an invalid plane 
   write(IOUT,'(a,i1,a,i4,a,i4,a)') "ILLEGAL INDEX REQUEST, DIRECTION:",dir,"either index1", index1,&
                  "is not between 1 and nxp2 or index2", index2, "is not between 1 and nyp2"
   goto 1000

  else !1 
    !This node does not contain this plane
   continue 
  endif !1

 else
  write(IOUT,'(a,i1,a)') "ILLEGAL DIRECTION:",dir,"is not 1,2,3"
  goto 1000
 endif

  !MyidM should be zero since it is always the master of the subcommunicator that the pencil is 
  !reduced to... note the subrouine returns Master to the calling program, which contains the 
  !global id of the node containing the pencil.
  if (Xmaster.EQ.1) call gather1dM(var,Pen,sp1,dir,index1,index2,0,stat)
  if (Xnode.EQ.1)   call gather1dS(var,dir,index1,index2,0,stat)


  stat=0 !Clean return 
  return

  1000 continue
  stat=1 !stat should contain the stat from the gather2d calls

  return
end subroutine Reduce_Pencil_to_Master 
