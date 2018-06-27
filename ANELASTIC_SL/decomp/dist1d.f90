subroutine Distribute1dM(varL,InPencil,sb1,dir,index1,index2,myidM,stat)
!@t
! \textbf{subroutine Distribute1dM(varL,InPencil,sb1,dir,index1,index2,myidM,stat)}
!@h
!   Description:
!     Distribute real 1D data from a master to a slave using blocking
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
 use dd,     only: myid,comm3d, commx1, commx2, commx3, sizex1, sizex2, sizex3, coords,&
                   nxprocs, nyprocs, nzprocs,MPI_STATUS_SIZE, inttype, realtype 
 use Domain, only: sx,ex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)                  :: index1, index2, myidM, sb1, dir
 real(r8),intent(out)                :: varL(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)                 :: InPencil(1:sb1)
 integer,intent(out)                 :: stat

!Local Variables
 integer                             :: Tsize, Rcoords(3), s1, status1(MPI_STATUS_SIZE)
 integer                             :: i,j,k,n,istart, jstart, kstart, i2, j2, k2
 real(r8),allocatable,dimension(:)   :: Temp_Send
 integer                             :: err1


 stat=0
 s1=0
 err1=0

 !Distribute Data
 if (dir.eq.1) then !0
  allocate( Temp_Send(sx-1:ex+1), STAT=s1 )
  Tsize=size(Temp_Send)
  do n=0,sizex1-1
   if (n.Eq.myidM) then
     Rcoords=coords
   else
    call MPI_RECV(Rcoords,3,inttype,n,2,commX1,status1,err1)
   endif
  !Determine Block of Data to send
   istart = Rcoords(1)*(nxp2-2)/nxprocs
   do i=sx-1,ex+1 
    i2=istart+i 
    Temp_Send(i)=INPENCIL(i2)
   enddo

   if (n.EQ.myidM) then
    do i=sx-1,ex+1 
      varL(i,index1,index2)=Temp_Send(i)
    enddo
   else
    call MPI_SEND(Temp_Send,Tsize,realtype,n,1,commX1,err1) 
   endif
  enddo
 elseif (dir.eq.2) then !0
  allocate( Temp_Send(sy-1:ey+1), STAT=s1 )
  Tsize=size(Temp_Send)
  do n=0,sizex2-1
   if (n.Eq.myidM) then
     Rcoords=coords
   else
    call MPI_RECV(Rcoords,3,inttype,n,2,commX2,status1,err1)
   endif
  !Determine Block of Data to send
   jstart = Rcoords(2)*(nyp2-2)/nyprocs
   do j=sy-1,ey+1 
    j2=jstart+j 
    Temp_Send(j)=INPENCIL(i2)
   enddo

   if (n.EQ.myidM) then
    do j=sy-1,ey+1 
      varL(index1,j,index2)=Temp_Send(j)
    enddo
   else
    call MPI_SEND(Temp_Send,Tsize,realtype,n,1,commX2,err1) 
   endif
  enddo
 elseif (dir.eq.3) then !0
  allocate( Temp_Send(sz-1:ez+1), STAT=s1 )
  Tsize=size(Temp_Send)
  do n=0,sizex3-1
   if (n.Eq.myidM) then
     Rcoords=coords
   else
    call MPI_RECV(Rcoords,3,inttype,n,2,commX3,status1,err1)
   endif
  !Determine Block of Data to send
   kstart = Rcoords(3)*(nzp2-2)/nzprocs
   do k=sz-1,ez+1 
    k2=kstart+k 
    Temp_Send(k)=INPENCIL(k2)
   enddo

   if (n.EQ.myidM) then
    do k=sz-1,ez+1 
      varL(index1,index2,k)=Temp_Send(k)
    enddo
   else
    call MPI_SEND(Temp_Send,Tsize,realtype,n,1,commX3,err1) 
   endif
  enddo
 else !0
  write(IOUT,'(a,i1,a)') "INVALID DIRECTION:", dir, "not (1,2,3)"
  goto 2000
 endif !0

 deallocate(Temp_Send,STAT=s1)
 stat=max(s1,err1,stat)
 return

 2000 continue
 stat=1
 return
end subroutine Distribute1dM

subroutine Distribute1dS(varL,dir,index1,index2,myidM,err1)
!@t
! \textbf{subroutine Distribute1dS(varL,dir,index1,index2,myidM,err1)}
!@h
!   Description:
!     Distribute real 1D data from a slave to a master using blocking
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
 use dd,     only: commx1, commx2, commx3, coords, MPI_STATUS_SIZE, inttype, realtype
 use Domain, only: sx,ex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)        :: myidM
 integer,intent(in)        :: dir, index1, index2
 real(r8),intent(out)      :: varL(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)       :: err1

!Local Variables
 integer                            :: Tsize
 integer                            :: i,j,k 
 integer                            :: s1, stat
 real(r8),allocatable,dimension(:)  :: Temp_Recv
 
 err1=0
 s1=0
 stat=0

 if (dir.eq.1) then !0
  allocate( Temp_Recv(sx-1:ex+1), STAT=s1)
  Tsize=size(Temp_Recv)
  call MPI_SEND(coords,3,inttype,myidM,2,commx1,stat)
  call MPI_RECV(Temp_Recv,Tsize,realtype,myidM,1,commx1,stat)
  do i=sx-1,ex+1
   varL(i,index1,index2)=Temp_Recv(i)
  enddo

 elseif (dir.eq.2) then !0
  allocate( Temp_Recv(sy-1:ey+1), STAT=s1)
  Tsize=size(Temp_Recv)
  call MPI_SEND(coords,3,inttype,myidM,2,commx2,stat)
  call MPI_RECV(Temp_Recv,Tsize,realtype,myidM,1,commx2,stat)
  do j=sy-1,ey+1
   varL(index1,j,index2)=Temp_Recv(j)  
  enddo
 elseif (dir.eq.3) then !0
  allocate( Temp_Recv(sz-1:ez+1), STAT=s1)
  Tsize=size(Temp_Recv)
  call MPI_SEND(coords,3,inttype,myidM,2,commx3,stat)
  call MPI_RECV(Temp_Recv,Tsize,realtype,myidM,1,commx3,stat)
  do k=sz-1,ez+1
   varL(index1,index2,k)=Temp_Recv(k)  
  enddo
 else !0
  write(IOUT,'(a,i1,a)') "INVALID DIRECTION:", dir, "not (1,2,3)"
  goto 2000
 endif !0

 deallocate(Temp_Recv,STAT=s1)
 err1=max(err1,stat,s1) 
 return

 2000 continue
 stat=-1
 return
end subroutine Distribute1dS
