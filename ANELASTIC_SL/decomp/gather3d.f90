subroutine gather3dM(varL,varG,temp,myidM,stat)
!@t
! \textbf{subroutine gather3dM(varL,varG,temp,myidM,stat)}
!@h
!   Description:
!     Gather real 3D data from slaves to a master using blocking
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
 use dd,     only: myid, commx1x2x3, coords, nxprocs, nyprocs, nzprocs, &
                      sizex1x2x3, MPI_STATUS_SIZE, inttype, realtype, &
                      sizeX1,sizeX2,sizeX3 
 use Domain, only: sx,ex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)        :: myidM
 real(r8),intent(out)      :: varG(1:nxp2,1:nyp2,1:nzp2)
 real(r8),intent(in)       :: varL(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(out)      :: temp(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)       :: stat

!Local Variables
 integer                   :: Tsize, Rcoords(3), status1(MPI_STATUS_SIZE)
 integer                   :: i,j,k,n,istart, jstart, kstart, i2, j2, k2
 integer                   :: is,js,ks,ie,je,ke
 integer                   :: err1

 err1=0

 Tsize=size(varL)

 temp(:,:,:)=0.d0 
 varG(:,:,:)=0.d0

 !Gather Data
 do n=0,sizex1x2x3-1
  if (n.eq.myidM) then
   Rcoords(:)=coords(:)
   temp(:,:,:)=varL(:,:,:)
  else
   call MPI_RECV(Rcoords,3,inttype,n,2,commX1X2X3,status1,err1)
   call MPI_RECV(temp,Tsize,realtype,n,1,commX1X2X3,status1,err1)
  endif

  !Determine Block of Data to recieve
  istart = Rcoords(1)*(nxp2-2)/nxprocs 
  jstart = Rcoords(2)*(nyp2-2)/nyprocs
  kstart = Rcoords(3)*(nzp2-2)/nzprocs

  !Determine if there is boundary data 
  is=0
  ie=0
  js=0
  je=0
  ks=0
  ke=0

  if ( Rcoords(1).EQ.0      )   is=1
  if ( Rcoords(2).EQ.0      )   js=1
  if ( Rcoords(3).EQ.0      )   ks=1
  if ( Rcoords(1).EQ.sizex1-1 ) ie=1
  if ( Rcoords(2).EQ.sizex2-1 ) je=1
  if ( Rcoords(3).EQ.sizex3-1 ) ke=1


  !UnPack Data
  do k=sz-ks,ez+ke  
   do j=sy-js,ey+je 
    do i=sx-is,ex+ie
     i2=istart+i 
     j2=jstart+j
     k2=kstart+k
     varG(i2,j2,k2)=temp(i,j,k)
    enddo
   enddo
  enddo

 enddo

 stat=err1
return
end subroutine gather3dM

subroutine gather3dS(varL,myidM,stat)
!@t
! \textbf{subroutine gather3dS(varL,myidM,stat)}
!@h
!   Description:
!     Gather real 3D data from slaves to a master using blocking
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
 use dd,     only: commx1x2x3,coords,MPI_STATUS_SIZE, inttype, realtype
 use Domain, only: sx,ex,sy,ey,sz,ez,nxp2,nyp2,nzp2
 use IO,     only: IOUT
 implicit none

!Passed Variables 
 integer,intent(in)        :: myidM
 real(r8),intent(in)       :: varL(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)       :: stat

!Local Variables
 integer                   :: Tsize

 Tsize=size(varL)
 call MPI_SEND(coords,3,inttype,myidM,2,commX1X2X3,stat)
 call MPI_SEND(varL,Tsize,realtype,myidM,1,commX1X2X3,stat)
 
return
end subroutine gather3dS
