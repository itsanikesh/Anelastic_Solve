subroutine gather2dM_C(varL,OutPlane,sb1,sb2,dir,plane,myidM,stat)
!@t
! \textbf{subroutine gather2dM\_C(varL,OutPlane,sb1,sb2,dir,plane,myidM,stat)}
!@h
!   Description:
!     Gather complex 2D data from slaves to a master using blocking
!     communication. KYLE CAN YOU TAKE A LOOK AT THIS???
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu) 

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: r8
 use dd,     only: myid,comm3d,commx1x2, commx2x3, commx1x3, sizex1x2, sizex2x3,& 
                   sizex1x3, coords,nxprocs, nyprocs, nzprocs,&
                    MPI_STATUS_SIZE, inttype, cmplxtype 
 use Domain, only: sx,ex,cex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)                     :: myidM,dir,plane,sb1,sb2
 complex(r8),intent(in)                 :: varL(sx-1:cex+1,sy-1:ey+1,sz-1:ez+1)
 complex(r8),intent(out)                :: OutPlane(0:sb1-1,0:sb2-1)
 integer,intent(out)                    :: stat

!Local Variables
 integer                                :: Tsize, Rcoords(3), s1, status1(MPI_STATUS_SIZE)
 integer                                :: i,j,k,n,istart, jstart, kstart, i2, j2, k2
 complex(r8),allocatable,dimension(:,:) :: Temp_Recv, Temp_Send

 
 if (dir.eq.3) then !0
  allocate( Temp_Recv(sx-1:cex+1,sy-1:ey+1), STAT=s1 )
  Tsize=size(Temp_Recv)
  allocate( Temp_Send(sx-1:cex+1,sy-1:ey+1), STAT=s1 )
  do j=sy-1,ey+1
   do i=sx-1,cex+1
    Temp_Send(i,j) = varL(i,j,plane)
   enddo
  enddo

  do n=0,sizex1x2-1
   if (n.EQ.myidM) then
    Rcoords=coords
    Temp_Recv=Temp_Send
   else
    call MPI_RECV(Rcoords,3,inttype,n,2,commx1x2,status1,stat)
    call MPI_RECV(Temp_Recv,Tsize,cmplxtype,n,1,commx1x2,status1,stat)
   endif
  !Determine Block of Data to recieve
   istart = Rcoords(1)*(nxp2-2)/(2*nxprocs)
   jstart = Rcoords(2)*(nyp2-2)/nyprocs

   !UnPack Data
   do j=sy-1,ey+1  
    do i=sx-1,cex+1
      i2=istart+i-1
      j2=jstart+j-1
      Outplane(i2,j2)=Temp_Recv(i,j)
     enddo
    enddo
   enddo

 else !0
  write(IOUT,'(a,i1,a)') "INVALID DIRECTION:", dir, "not (1,2,3)"
  goto 2000

 endif !0

 deallocate(Temp_Send,STAT=s1)
 deallocate(Temp_Recv,STAT=s1)

 return

 2000 continue
 stat=1
 return
end subroutine gather2dM_C

subroutine gather2dS_C(varL,dir,plane,myidM,stat)
!@t
! \textbf{subroutine gather2dS\_C(varL,dir,plane,myidM,stat)}
!@h
!   Description:
!     Gather complex 2D data from slaves to a master using blocking
!     communication. KYLE CAN YOU TAKE A LOOK AT THIS???
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu) 

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: r8
 use dd,     only: commx1x2,commx2x3, commx1x3,coords,&
                    MPI_STATUS_SIZE, inttype, cmplxtype 
 use Domain, only: sx,ex,cex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)                     :: myidM, dir, plane
 complex(r8),intent(in)                 :: varL(sx-1:cex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)                    :: stat

!Local Variables
 integer                                :: Tsize
 integer                                :: i,j,k 
 integer                                :: s1
 complex(r8),allocatable,dimension(:,:) :: Temp_Send

 if (dir.eq.3) then !0
   allocate( Temp_Send(sx-1:cex+1,sy-1:ey+1), STAT=s1)
   Tsize=size(Temp_Send)
   do j=sy-1,ey+1
    do i=sx-1,cex+1
     Temp_Send(i,j) = varL(i,j,plane)
    enddo
   enddo
   call MPI_SEND(coords,3,inttype,myidM,2,commx1x2,stat)
   call MPI_SEND(Temp_Send,Tsize,cmplxtype,myidM,1,commx1x2,stat)
 else !0
  write(IOUT,'(a,i1,a)') "INVALID DIRECTION:", dir, "not (1,2,3)"
  goto 2000
 endif !0

 deallocate( Temp_Send, STAT=s1 )

 2000 continue
 stat=1 !Unsucessful Return
return
end subroutine gather2dS_C
