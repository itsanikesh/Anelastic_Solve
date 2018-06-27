subroutine gather2dM(varL,OutPlane,sb1,sb2,dir,plane,myidM,stat)
!@t
! \textbf{subroutine gather2dM(varL,OutPlane,sb1,sb2,dir,plane,myidM,stat)}
!@h
!   Description:
!     Gather real 2D data from slaves to a master using blocking
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
                    MPI_STATUS_SIZE, inttype, realtype, sizex1, sizex2, sizex3
 use Domain, only: sx,ex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)        :: myidM,dir,plane,sb1,sb2
 real(r8),intent(in)       :: varL(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)      :: OutPlane(1:sb1,1:sb2)
 integer,intent(out)       :: stat

!Local Variables
 integer                             :: Tsize, Rcoords(3), s1, status1(MPI_STATUS_SIZE)
 integer                             :: i,j,k,n,istart, jstart, kstart, i2, j2, k2
 real(r8),allocatable,dimension(:,:) :: Temp_Recv, Temp_Send
 integer                             :: is,js,ks,ie,je,ke

 !Gather Data
 if (dir.eq.1) then !0
  allocate( Temp_Recv(sy-1:ey+1,sz-1:ez+1), STAT=s1 )
  Tsize=size(Temp_Recv)
  allocate( Temp_Send(sy-1:ey+1,sz-1:ez+1), STAT=s1)
  do k=sz-1,ez+1
   do j=sy-1,ey+1
    Temp_Send(j,k) = varL(plane,j,k)
   enddo
  enddo
!  call MPI_SEND(coords,3,inttype,myidM,2,commx2x3,stat)
!  call MPI_SEND(Temp_Send,Tsize,realtype,myidM,1,commx2x3,stat)
 
  do n=0,sizex2x3-1
   if (n.EQ.myidM) then
    Rcoords=coords
    Temp_Recv=Temp_Send
   else
    call MPI_RECV(Rcoords,3,inttype,n,2,commx2x3,status1,stat)
    call MPI_RECV(Temp_Recv,Tsize,realtype,n,1,commx2x3,status1,stat)
   endif
  !Determine Block of Data to recieve
   jstart = Rcoords(2)*(nyp2-2)/nyprocs
   kstart = Rcoords(3)*(nzp2-2)/nzprocs

  !Determine if there is boundary data 
   js=0
   je=0
   ks=0
   ke=0
   if ( Rcoords(2).EQ.0      )   js=1
   if ( Rcoords(3).EQ.0      )   ks=1
   if ( Rcoords(2).EQ.sizex2-1 ) je=1
   if ( Rcoords(3).EQ.sizex3-1 ) ke=1

   !UnPack Data
   do k=sz-ks,ez+ke  
    do j=sy-js,ey+je 
      j2=jstart+j
      k2=kstart+k
      Outplane(j2,k2)=Temp_Recv(j,k)
     enddo
    enddo
  enddo

 elseif (dir.eq.2) then !0
  allocate( Temp_Recv(sx-1:ex+1,sz-1:ez+1), STAT=s1 )
  Tsize=size(Temp_Recv)
  allocate( Temp_Send(sx-1:ex+1,sz-1:ez+1), STAT=s1 )
  do k=sz-1,ez+1
   do i=sx-1,ex+1
    Temp_Send(i,k) = varL(i,plane,k)
   enddo
  enddo

  do n=0,sizex1x3-1
   if (n.EQ.myidM) then
    Rcoords=coords
    Temp_Recv=Temp_Send
   else
    call MPI_RECV(Rcoords,3,inttype,n,2,commx1x3,status1,stat)
    call MPI_RECV(Temp_Recv,Tsize,realtype,n,1,commx1x3,status1,stat)
   endif
  !Determine Block of Data to recieve
   istart = Rcoords(1)*(nxp2-2)/nxprocs
   kstart = Rcoords(3)*(nzp2-2)/nzprocs

  !Determine if there is boundary data 
   is=0
   ie=0
   ks=0
   ke=0
   if ( Rcoords(1).EQ.0      )   is=1
   if ( Rcoords(3).EQ.0      )   ks=1
   if ( Rcoords(1).EQ.sizex1-1 ) ie=1
   if ( Rcoords(3).EQ.sizex3-1 ) ke=1

   !UnPack Data
   do k=sz-ks,ez+ke  
    do i=sx-is,ex+ie 
      i2=istart+i
      k2=kstart+k
      Outplane(i2,k2)=Temp_Recv(i,k)
     enddo
    enddo
   enddo

 elseif (dir.eq.3) then !0
  allocate( Temp_Recv(sx-1:ex+1,sy-1:ey+1), STAT=s1 )
  Tsize=size(Temp_Recv)
  allocate( Temp_Send(sx-1:ex+1,sy-1:ey+1), STAT=s1 )
  do j=sy-1,ey+1
   do i=sx-1,ex+1
    Temp_Send(i,j) = varL(i,j,plane)
   enddo
  enddo

  do n=0,sizex1x2-1
   if (n.EQ.myidM) then
    Rcoords=coords
    Temp_Recv=Temp_Send
   else
    call MPI_RECV(Rcoords,3,inttype,n,2,commx1x2,status1,stat)
    call MPI_RECV(Temp_Recv,Tsize,realtype,n,1,commx1x2,status1,stat)
   endif

  !Determine Block of Data to recieve
   istart = Rcoords(1)*(nxp2-2)/nxprocs
   jstart = Rcoords(2)*(nyp2-2)/nyprocs

  !Determine if there is boundary data 
   is=0
   ie=0
   js=0
   je=0
   if ( Rcoords(1).EQ.0      )   is=1
   if ( Rcoords(2).EQ.0      )   js=1
   if ( Rcoords(1).EQ.sizex1-1 ) ie=1
   if ( Rcoords(2).EQ.sizex2-1 ) je=1

   !UnPack Data
   do j=sy-js,ey+je  
    do i=sx-is,ex+ie 
      i2=istart+i
      j2=jstart+j
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
end subroutine gather2dM

subroutine gather2dS(varL,dir,plane,myidM,stat)
!@t
! \textbf{subroutine gather2dS(varL,dir,plane,myidM,stat)}
!@h
!   Description:
!     Gather real 2D data from slaves to a master using blocking
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
                    MPI_STATUS_SIZE, inttype, realtype
 use Domain, only: sx,ex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)        :: myidM, dir, plane
 real(r8),intent(in)        :: varL(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)       :: stat

!Local Variables
 integer                            :: Tsize
 integer                            :: i,j,k 
 integer                            :: s1
 real(r8),allocatable,dimension(:,:) :: Temp_Send

 if (dir.eq.1) then !0
   allocate( Temp_Send(sy-1:ey+1,sz-1:ez+1), STAT=s1)
   Tsize=size(Temp_Send)
   do k=sz-1,ez+1
    do j=sy-1,ey+1
     Temp_Send(j,k) = varL(plane,j,k)
    enddo
   enddo
   call MPI_SEND(coords,3,inttype,myidM,2,commx2x3,stat)
   call MPI_SEND(Temp_Send,Tsize,realtype,myidM,1,commx2x3,stat)

 elseif (dir.eq.2) then !0
   allocate( Temp_Send(sx-1:ex+1,sz-1:ez+1), STAT=s1)
   Tsize=size(Temp_Send)
   do k=sz-1,ez+1
    do i=sx-1,ex+1
     Temp_Send(i,k) = varL(i,plane,k)
    enddo
   enddo
   call MPI_SEND(coords,3,inttype,myidM,2,commx1x3,stat)
   call MPI_SEND(Temp_Send,Tsize,realtype,myidM,1,commx1x3,stat)

 elseif (dir.eq.3) then !0
   allocate( Temp_Send(sx-1:ex+1,sy-1:ey+1), STAT=s1)
   Tsize=size(Temp_Send)
   do j=sy-1,ey+1
    do i=sx-1,ex+1
     Temp_Send(i,j) = varL(i,j,plane)
    enddo
   enddo
   call MPI_SEND(coords,3,inttype,myidM,2,commx1x2,stat)
   call MPI_SEND(Temp_Send,Tsize,realtype,myidM,1,commx1x2,stat)
 else !0
  write(IOUT,'(a,i1,a)') "INVALID DIRECTION:", dir, "not (1,2,3)"
  goto 2000
 endif !0

 deallocate( Temp_Send, STAT=s1 )

 2000 continue
 stat=1 !Unsucessful Return
return
end subroutine gather2dS
