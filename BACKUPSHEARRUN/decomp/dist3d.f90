subroutine distribute3dm(varG,varL,temp,myidM,stat)
!@t
! \textbf{subroutine subroutine distribute3dm(varG,varL,temp,myidM,stat)}
!@h
!   Description:
!     Distribute real 3D data from a master to a slave using blocking
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
 use dd,     only: commX1X2X3, coords, nxprocs, nyprocs, nzprocs,&
                    sizex1x2x3, realtype, inttype, MPI_STATUS_SIZE
 use Domain, only: nxp2,nyp2,nzp2,sx,ex,sy,ey,sz,ez
 implicit none

!Passed Variables 
 integer,intent(in)        :: myidM
 integer,intent(out)       :: stat
 real(r8),intent(in)       :: varG(1:nxp2,1:nyp2,1:nzp2)
 real(r8),intent(out)      :: varL(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8)                  :: temp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer                   :: Tsize, Rcoords(3), status1(MPI_STATUS_SIZE)
 integer                   :: i,j,k,n,istart, jstart, kstart, i2, j2, k2
 integer                   :: err

 Tsize=size(varL)

 do n=0,sizex1x2x3-1
  if (n.Eq.myidM) then
    Rcoords=coords
  else
   call MPI_RECV(Rcoords,3,inttype,n,2,commX1X2X3,status1,err)
  endif
  !Determine Block of Data to send
   istart = Rcoords(1)*(nxp2-2)/nxprocs 
   jstart = Rcoords(2)*(nyp2-2)/nyprocs
   kstart = Rcoords(3)*(nzp2-2)/nzprocs
   !Pack Data
   do k=sz,ez 
    do j=sy,ey 
     do i=sx,ex 
      i2=istart+i 
      j2=jstart+j
      k2=kstart+k
      temp(i,j,k)=varG(i2,j2,k2)
     enddo
    enddo
   enddo

   !Send Data
  if (n.EQ.myidM) then
   varL=temp
  else
   call MPI_SEND(temp,Tsize,realtype,n,1,commX1X2X3,err) 
  endif
 enddo

 stat=err
return
end subroutine distribute3dm


subroutine Distribute3dS(varL,myidM,stat)
!@t
! \textbf{subroutine Distribute3dS(varL,myidM,stat)}
!@h
!   Description:
!     Distribute real 3D data from a slave to a master using blocking
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
 use dd,     only: commX1X2X3,coords,nxprocs,nyprocs,nzprocs,myid,&
                    realtype, inttype, MPI_STATUS_SIZE
 use Domain, only: nxp2,nyp2,nzp2,sx,ex,sy,ey,sz,ez
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)        :: myidM
 integer,intent(out)       :: stat
 real(r8),intent(out)       :: varL(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)

!Local Variables
 integer                   :: Tsize, Rcoords(3), status1(MPI_STATUS_SIZE)
 integer                   :: i,j,k,n,istart, jstart, kstart, i2, j2, k2
 integer                   :: err
 Tsize=size(varL)
 do k=sz-1,ez+1
  do j=sy-1,ey+1 
   do i=sx-1,ex+1
     varL(i,j,k)=0.d0
   enddo
  enddo
 enddo
  call MPI_SEND(coords,3,inttype,myidM,2,commX1X2X3,err)
  call MPI_RECV(varL,Tsize,realtype,myidM,1,commX1X2X3,status1,err)

 stat=err
return
end subroutine Distribute3dS
