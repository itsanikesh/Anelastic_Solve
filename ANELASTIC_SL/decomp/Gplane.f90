subroutine Reduce_Plane_to_Master(var,dir,plane,Master,PX,sp1,sp2,stat)
!@t
! \textbf{subroutine Reduce\_Plane\_to\_Master(var,dir,plane,Master,PX,sp1,sp2,stat)}
!@h
!   Description:
!     Reduces a 2D plane of data to the master node.
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
!     KYLE CAN YOU TAKE A LOOK AT THIS???
!@q
 use ntypes, only: r8
 use dd,     only: rankx2x3,rankx1x2,rankx1x3,myid
 use Domain, only: sx,ex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables
 real(r8),intent(in)                :: var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(in)                 :: dir, plane, sp1, sp2
 integer,intent(out)                :: Master
 real(r8),intent(out)               :: PX(1:sp1,1:sp2)
 integer,intent(out)                :: stat
!Local Variables
 integer                            :: Xmaster,Xnode
 
 Master = -1
 if (dir.EQ.1) then     !0 
  !sub-communicator is x2x3
  Xmaster = -1
  Xnode   = -1
  if (plane.GT.sx-1.AND.plane.LT.ex+1) then !1 
   !This node contains this plane

   if (rankx2x3.EQ.0) then 
    Xmaster = 1
    Master = myid
   else 
    Xnode   = 1
   endif 

  elseif ( (plane.EQ.sx-1.AND.sx-1.EQ.1).OR.(plane.EQ.ex+1.AND.ex+1.EQ.nxp2) )  then !1 
     !This node contains this plane and its a boundary plane

   if (rankx2x3.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 

  elseif (plane.LT.1.or.plane.GT.nxp2) then !1 
    !This is an invalid plane 
   write(IOUT,'(a,i1,a,i4,a)') "ILLEGAL PLANE REQUEST, DIRECTION:",dir,"PLANE",plane,"is not between 1 and nxp2"
   goto 1000

  else !1 
    !This node does not contain this plane
   continue 
  endif !1

 elseif (dir.EQ.2) then !0 
  !sub-communicator is x2x3
  Xmaster = -1
  Xnode   = -1
  if (plane.GT.sy-1.AND.plane.LT.ey+1) then !1 
   !This node contains this plane

   if (rankx1x3.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 

  elseif ( (plane.EQ.sy-1.AND.sy-1.EQ.1).OR.(plane.EQ.ey+1.AND.ey+1.EQ.nyp2) )  then !1 
     !This node contains this plane and its a boundary plane

   if (rankx1x3.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 

  elseif (plane.LT.1.or.plane.GT.nyp2) then !1 
    !This is an invalid plane 
   write(IOUT,'(a,i1,a,i4,a)') "ILLEGAL PLANE REQUEST, DIRECTION:",dir,"PLANE",plane,"is not between 1 and nyp2"
   goto 1000

  else !1 
    !This node does not contain this plane
   continue 
  endif !1

 elseif (dir.EQ.3) then !0 
  !sub-communicator is x1x2
  Xmaster = -1
  Xnode   = -1
  if (plane.GT.sz-1.AND.plane.LT.ez+1) then !1 
   !This node contains this plane
   if (rankx1x2.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 

  elseif ( (plane.EQ.sz-1.AND.sz-1.EQ.1).OR.(plane.EQ.ez+1.AND.ez+1.EQ.nzp2) )  then !1 
     !This node contains this plane and its a boundary plane

   if (rankx1x2.EQ.0) then 
    Xmaster = 1
    Master  = myid
   else 
    Xnode   = 1
   endif 

  elseif (plane.LT.1.or.plane.GT.nzp2) then !1 
    !This is an invalid plane 
   write(IOUT,'(a,i1,a,i4,a)') "ILLEGAL PLANE REQUEST, DIRECTION:",dir,"PLANE",plane,"is not between 1 and nzp2"
   goto 1000
  else !1 
    !This node does not contain this plane
   continue 
  endif !1


  else
 
  endif
  if (Xmaster.EQ.1) call gather2dM(var,PX,sp1,sp2,dir,plane,0,stat)
  if (Xnode.EQ.1)   call gather2dS(var,dir,plane,0,stat)


!  stat=0 stat should contain the stat from the gather2d calls


  return

  1000 continue
  stat=1
  return
end subroutine Reduce_Plane_to_Master 
