subroutine Distribute2dM_C(varL,INPlane,sb1,sb2,dir,plane,myidM,stat)
!@t
! \textbf{subroutine Distribute2dM\_C(varL,INPlane,sb1,sb2,dir,plane,myidM,stat)}
!@h
!   Description:
!     Distribute complex 2D data from a master to a slave using blocking
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
                   MPI_STATUS_SIZE, inttype, cmplxtype, myid 
 use Domain, only: sx,ex,cex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)        :: myidM,dir,plane,sb1,sb2
 complex(r8),intent(out)   :: varL(sx-1:cex+1, sy-1:ey+1, sz-1:ez+1 )  
 complex(r8),intent(in)    :: INPlane(1:sb1,1:sb2)
 integer,intent(out)       :: stat

!Local Variables
 integer                                :: Tsize, Rcoords(3), s1, status1(MPI_STATUS_SIZE)
 integer                                :: i,j,k,n,istart, jstart, kstart, i2, j2, k2
 complex(r8),allocatable,dimension(:,:) :: Temp_Send
 integer                                :: err

 err=0
 if (dir.eq.3) then !0

  allocate( Temp_Send(sx-1:cex+1,sy-1:ey+1), STAT=s1 )
  Tsize=size(Temp_Send)

  do n=0,sizex1x2-1
   if (n.EQ.myidM) then
     Rcoords=coords
   else
    call MPI_RECV(Rcoords,3,inttype,n,2,commX1X2,status1,err)
   endif

  !Determine Block of Data to send
   istart = Rcoords(1)*(nxp2-2)/(2*nxprocs)
   jstart = Rcoords(2)*(nyp2-2)/nyprocs

   do j=sy-1,ey+1 
    do i=sx-1,cex+1
      i2=istart+i 
      j2=jstart+j
      Temp_Send(i,j)=INplane(i2,j2)
    enddo
   enddo

   if (n.EQ.myidM) then
    do j=sy-1,ey+1 
     do i=sx-1,cex+1
      varL(i,j,plane)=Temp_Send(i,j)
     enddo
    enddo
   else
   call MPI_SEND(Temp_Send,Tsize,cmplxtype,n,1,commX1X2,err) 
   endif
  enddo
 else !0
  write(IOUT,'(a,i1,a)') "INVALID DIRECTION:", dir, "not (1,2,3)"
  goto 2000
 endif !0


 deallocate(Temp_Send)
 stat=err
 return

 2000 continue
 stat=-1
 return
end subroutine Distribute2dM_C

subroutine Distribute2dS_C(varL,dir,plane,myidM,err1)
!@t
! \textbf{subroutine Distribute2dS\_C(varL,dir,plane,myidM,err1)}
!@h
!   Description:
!     Distribute complex 2D data from a slave to a master using blocking
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
                    MPI_STATUS_SIZE, inttype,cmplxtype, myid 
 use Domain, only: sx,ex,cex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)                    :: myidM, dir, plane
 complex(r8),intent(out)               :: varL( sx-1:cex+1, sy-1:ey+1, sz-1:ez+1 )  
 integer,intent(out)                   :: err1

!Local Variables
 integer                                :: Tsize, status1(MPI_STATUS_SIZE)
 integer                                :: i,j,k 
 integer                                :: s1, stat
 complex(r8),allocatable,dimension(:,:) :: Temp_Recv

 stat=0
 s1=0

 if (dir.eq.3) then !0
  allocate( Temp_Recv(sx-1:cex+1,sy-1:ey+1), STAT=s1)
  Tsize=size(Temp_Recv)
  call MPI_SEND(coords,3,inttype,myidM,2,commx1x2,stat)
  call MPI_Recv(Temp_Recv,Tsize,cmplxtype,myidM,1,commx1x2,status1,stat)
  do j=sy-1,ey+1
   do i=sx-1,cex+1
     varL(i,j,plane)=Temp_Recv(i,j)
   enddo
  enddo

 else !0
  write(IOUT,'(a,i1,a)') "INVALID DIRECTION:", dir, "not (1,2,3)"
  goto 2000
 endif !0

 deallocate( Temp_Recv, STAT=s1 )
 err1=max(stat,s1)
 2000 continue
 stat=1 !Unsucessful Return

 return
end subroutine Distribute2dS_C
