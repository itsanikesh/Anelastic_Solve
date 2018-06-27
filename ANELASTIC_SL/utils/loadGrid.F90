subroutine load_grid(stat)
!@t
! \textbf{subroutine load\_grid(stat)}
!@h
!   Description:
!     Loads the grid.in files and sets up the grid.
!@q
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
!@h
!   Comments:
!     Ensures that the first and last cells are unstretched in each
!     direction. Reads everything from the grid.in files.
!@q

 use IO,     only: IOUT
 use Grid
 use Domain
#ifdef PARALLEL
 use dd,     only: myid,comm3d,realtype,logictype,MPI_MIN, chartype
#endif
 implicit none

!Passed Variables
 integer,intent(out)         :: stat

!Local Variables
 integer                     :: i,j,k,s1,nxp2L,nyp2L,nzp2L, idum

!**********************************************************************
!*******************************X1 GRID********************************
!**********************************************************************
#ifdef PARALLEL
 if (myid.EQ.0) then !MASTER
#endif
  open(unit=307,file='x1_grid.in',form='formatted',status='old',action='read',iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a,i5)') "ERROR OPENING x1_grid.in, IOSTAT=",s1
   stat=1
   goto 1000
  endif
  read(307,*) 
  read(307,*) 
  read(307,*) nxp2L 
  read(307,*) xL 
  read(307,*) x0 
  read(307,*) Gridtype(1)  
  if (nxp2L.NE.nxp2) then
   write(IOUT,'(a30)') "ERROR: x1_grid.in WRONG SIZE:"
   write(IOUT,'(a12,i4)') "nxp2 RUN : ",nxp2
   write(IOUT,'(a12,i4)') "nxp2 GRID: ",nxp2L
   goto 1000
  else
   do i=1,nxp2
    read(307,*) idum,xe(i),xc(i),dxe(i),dxc(i),rdxe(i),rdxc(i)
   enddo
  endif
   close(307)
#ifdef PARALLEL 
  endif !MASTER
  call MPI_BCAST(xe,nxp2,realtype,0,comm3d,stat)
  call MPI_BCAST(xc,nxp2,realtype,0,comm3d,stat)
  call MPI_BCAST(dxe,nxp2,realtype,0,comm3d,stat)
  call MPI_BCAST(dxc,nxp2,realtype,0,comm3d,stat)
  call MPI_BCAST(rdxe,nxp2,realtype,0,comm3d,stat)
  call MPI_BCAST(rdxc,nxp2,realtype,0,comm3d,stat)
  call MPI_BCAST(xL,1,realtype,0,comm3d,stat)
  call MPI_BCAST(x0,1,realtype,0,comm3d,stat)
  call MPI_BCAST(Gridtype(1),50,chartype,0,comm3d,stat)
#endif

 !COMPUTE xc, dxe and dxc, rdxe, rdxc
 !xc(i)= ( xe(i)+xe(i-1) )/2.d0 !BY DEFINITION
 !dxe=xc(i+1)-xc(i)             !BY DEFINITION
 !dxc=xe(i)-xe(i-1)             !BY DEFINITION
 !Ensure No Stretching at First and Last Cells in X1
  dxc(nxp2) =  dxc(nxp2-1)
 rdxc(nxp2) = rdxc(nxp2-1)
  dxe(nxp2) =  dxe(nxp2-1)
 rdxe(nxp2) = rdxe(nxp2-1)
  dxc(1) =  dxc(2)
 rdxc(1) = rdxc(2)
  dxe(1) =  dxe(2)
 rdxe(1) = rdxe(2)
 xe(nxp2)=xe(nxp2-1)+dxc(nxp2)
 xc(nxp2)=xc(nxp2-1)+dxe(nxp2-1)
 xe(1)=xe(2)-dxc(2)
 xc(1)=xc(2)-dxe(1)

!**********************************************************************
!*******************************X2 GRID********************************
!**********************************************************************
#ifdef PARALLEL
 if (myid.EQ.0) then !MASTER
#endif
  open(unit=306,file='x2_grid.in',form='formatted',status='old',action='read',iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a,i5)') "ERROR OPENING x2_grid.in, IOSTAT=",s1
   stat=1
   goto 1000
  endif
  read(306,*)
  read(306,*)
  read(306,*) nyp2L
  read(306,*) yL 
  read(306,*) y0 
  read(306,*) Gridtype(2)
  if (nyp2L.NE.nyp2) then
   write(IOUT,'(a30)') "ERROR: x2_grid.in WRONG SIZE:"
   write(IOUT,'(a12,i4)') "nyp2 RUN : ",nyp2
   write(IOUT,'(a12,i4)') "nyp2 GRID: ",nyp2L
   goto 1000
  else
   do j=1,nyp2
    read(306,*) idum,ye(j),yc(j),dye(j),dyc(j),rdye(j),rdyc(j)
   enddo
  endif
   close(306)
#ifdef PARALLEL 
  endif !MASTER
  call MPI_BCAST(ye,nyp2,realtype,0,comm3d,stat)
  call MPI_BCAST(yc,nyp2,realtype,0,comm3d,stat)
  call MPI_BCAST(dye,nyp2,realtype,0,comm3d,stat)
  call MPI_BCAST(dyc,nyp2,realtype,0,comm3d,stat)
  call MPI_BCAST(rdye,nyp2,realtype,0,comm3d,stat)
  call MPI_BCAST(rdyc,nyp2,realtype,0,comm3d,stat)
  call MPI_BCAST(yL,1,realtype,0,comm3d,stat)
  call MPI_BCAST(y0,1,realtype,0,comm3d,stat)
  call MPI_BCAST(Gridtype(2),50,chartype,0,comm3d,stat)
#endif

 !Ensure No Stretching at First and Last Cells in X2
  dyc(nyp2) =  dyc(nyp2-1)
 rdyc(nyp2) = rdyc(nyp2-1)
  dye(nyp2) =  dye(nyp2-1)
 rdye(nyp2) = rdye(nyp2-1)
  dyc(1) =  dyc(2)
 rdyc(1) = rdyc(2)
  dye(1) =  dye(2)
 rdye(1) = rdye(2)
 ye(nyp2)=ye(nyp2-1)+dyc(nyp2)
 yc(nyp2)=yc(nyp2-1)+dye(nyp2-1)
 ye(1)=ye(2)-dyc(2)
 yc(1)=yc(2)-dye(1)


!**********************************************************************
!*******************************X3 GRID********************************
!**********************************************************************
#ifdef PARALLEL
 if (myid.EQ.0) then !MASTER
#endif
  open(unit=305,file='x3_grid.in',form='formatted',status='old',action='read',iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a,i5)') "ERROR OPENING x3_grid.in, IOSTAT=",s1
   stat=1
   goto 1000
  endif
  read(305,*)
  read(305,*)
  read(305,*) nzp2L
  read(305,*) zL
  read(305,*) z0
  read(305,*) Gridtype(3)
  if (nzp2L.NE.nzp2) then
   write(IOUT,'(a30)') "ERROR: x3_grid.in WRONG SIZE:"
   write(IOUT,'(a12,i4)') "nzp2 RUN : ",nzp2
   write(IOUT,'(a12,i4)') "nzp2 GRID: ",nzp2L
   goto 1000
  else
   do k=1,nzp2
    read(305,*) idum,ze(k),zc(k),dze(k),dzc(k),rdze(k),rdzc(k)
   enddo
  endif
   close(305)
#ifdef PARALLEL 
  endif !MASTER
  call MPI_BCAST(ze,nzp2,realtype,0,comm3d,stat)
  call MPI_BCAST(zc,nzp2,realtype,0,comm3d,stat)
  call MPI_BCAST(dze,nzp2,realtype,0,comm3d,stat)
  call MPI_BCAST(dzc,nzp2,realtype,0,comm3d,stat)
  call MPI_BCAST(rdze,nzp2,realtype,0,comm3d,stat)
  call MPI_BCAST(rdzc,nzp2,realtype,0,comm3d,stat)
  call MPI_BCAST(zL,1,realtype,0,comm3d,stat)
  call MPI_BCAST(z0,1,realtype,0,comm3d,stat)
  call MPI_BCAST(Gridtype(3),50,chartype,0,comm3d,stat)

#endif

 !COMPUTE zc, dze and dzc, rdze, rdzc
 !zc(i)= ( ze(i)+ze(i-1) )/2.d0 !BY DEFINITION
 !dze=zc(i+1)-zc(i)             !BY DEFINITION
 !dzc=ze(i)-ze(i-1)             !BY DEFINITION

 !Ensure No Stretching at First and Last Cells in X3
  dzc(nzp2) =  dzc(nzp2-1)
 rdzc(nzp2) = rdzc(nzp2-1)
  dze(nzp2) =  dze(nzp2-1)
 rdze(nzp2) = rdze(nzp2-1)
  dzc(1) =  dzc(2)
 rdzc(1) = rdzc(2)
  dze(1) =  dze(2)
 rdze(1) = rdze(2)
 ze(nzp2)=ze(nzp2-1)+dzc(nzp2)
 zc(nzp2)=zc(nzp2-1)+dze(nzp2-1)
 ze(1)=ze(2)-dzc(2)
 zc(1)=zc(2)-dze(1)

 stat=0
 write(IOUT,'(a)') "GRID SETUP COMPLETED"
 return
  
 1000 continue
 stat=-1
 write(IOUT,'(a)') "GRID SETUP FAILED"
 return

end subroutine load_grid
