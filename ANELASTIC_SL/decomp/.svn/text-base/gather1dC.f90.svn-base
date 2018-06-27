subroutine gather1dM_C(varL,OutPencil,sb1,dir,index1,index2,myidM,stat)
!@t
! \textbf{subroutine gather1dM\_C(varL,OutPencil,sb1,dir,index1,index2,myidM,stat)}
!@h
!   Description:
!     Gather complex 1D data from slaves to a master using blocking
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
                   nxprocs, nyprocs, nzprocs,MPI_STATUS_SIZE, inttype, cmplxtype 
 use Domain, only: sx,ex,cex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)                     :: index1, index2, myidM, sb1, dir
 complex(r8),intent(in)                 :: varL(sx-1:cex+1,sy-1:ey+1,sz-1:ez+1)
 complex(r8),intent(out)                :: OutPencil(1:sb1)
 integer,intent(out)                    :: stat

!Local Variables
 integer                                :: Tsize, Rcoords(3), s1, status1(MPI_STATUS_SIZE)
 integer                                :: i,j,k,n,istart, jstart, kstart, i2, j2, k2
 complex(r8),allocatable,dimension(:)   :: Temp_Recv, Temp_Send

 !Gather Data
  if (dir.eq.3) then !0
  allocate( Temp_Recv(sz-1:ez+1), STAT=s1 )
  Tsize=size(Temp_Recv)
  allocate( Temp_Send(sz-1:ez+1), STAT=s1)
  do k=sz-1,ez+1
   Temp_Send(k) = varL(index1,index2,k)
  enddo
 
  do n=0,sizex3-1
   if (n.Eq.myidM) then
    Rcoords=coords
    Temp_Recv=Temp_Send
    OutPencil(1)=Temp_Recv(sz-1)
   else
    call MPI_RECV(Rcoords,3,inttype,n,2,commx3,status1,stat)
    call MPI_RECV(Temp_Recv,Tsize,cmplxtype,n,1,commx3,status1,stat)
   endif
  !Determine Block of Data to recieve
   kstart = Rcoords(3)*(nzp2-2)/nzprocs
   if ( Rcoords(3) .EQ. sizeX3-1 ) OutPencil(nzp2)=Temp_Recv(ez+1)

   !UnPack Data
   do k=sz,ez  
    k2=kstart+k
    OutPencil(k2)=Temp_Recv(k)
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
end subroutine gather1dM_C


subroutine gather1dS_C(varL,dir,index1,index2,myidM,stat)
!@t
! \textbf{subroutine gather1dS\_C(varL,dir,index1,index2,myidM,stat)}
!@h
!   Description:
!     Send complex 1D data from slaves to a master using blocking
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
 use dd,     only: commx1, commx2, commx3, coords, MPI_STATUS_SIZE, inttype, cmplxtype 
 use Domain, only: sx,ex,cex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)                     :: myidM
 integer,intent(in)                     :: dir, index1, index2
 complex(r8),intent(in)                 :: varL(sx-1:cex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)                    :: stat

!Local Variables
 integer                                :: Tsize
 integer                                :: i,j,k 
 integer                                :: s1
 complex(r8),allocatable,dimension(:)   :: Temp_Send

  if (dir.eq.3) then !0
   allocate( Temp_Send(sz-1:ez+1), STAT=s1)
   Tsize=size(Temp_Send)
   do k=sz-1,ez+1
    Temp_Send(k) = varL(index1,index2,k)
   enddo
   call MPI_SEND(coords,3,inttype,myidM,2,commx3,stat)
   call MPI_SEND(Temp_Send,Tsize,cmplxtype,myidM,1,commx3,stat)
 else !0
  write(IOUT,'(a,i1,a)') "INVALID DIRECTION:", dir, "not (1,2,3)"
  goto 2000
 endif !0

 deallocate( Temp_Send, STAT=s1 )

 2000 continue
 stat=1 !Unsucessful Return
return

end subroutine gather1dS_C
