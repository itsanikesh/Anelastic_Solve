












subroutine mg_grids(stat)
!@t
!\textbf{subroutine mg\_grids(stat)}
!@h
!   Description:
!     Define grids and grid spacings at each level
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
!     The grids are slightly offset so that prolongation and restriction  
!     will always use the at least two points in each direction. Only the
!     grid spacings are important for the algorithm, the grid locations do
!     not enter the calculation explicitly. The periodic length is currently
!     defined with respect to edge lengths
!@q
 use ntypes, only: r8, i4
 use mgVars 
 use mgPASS, only: xe,xc,ye,yc,ze,zc, dxe,dxc,dye,dyc,dze,dzc
 implicit none
 integer(i4),intent(out)        :: stat

 !Generation of Poisson Stencil Coefficients
 !Local Variables
 integer(i4)                   :: i,j,k, lvl, i2, j2, k2
 integer(i4)                   :: nxm,nym,nzm 
 integer(i4)                   :: kx1,kx2,kx3,kx1C,kx1F,kx2C,kx2F,kx3C,kx3F 
 integer(i4)                   :: ic1,jc1,kc1,iic,iif,jjc,jjf,kkc,kkf, lC, lF
 integer(i4)                   :: nx1C,nx2C,nx3C,nx1F,nx2F,nx3F
 real(r8)                      :: dxx,dyy,dzz,xLL,yLL,zLL 

 integer(i4)                   :: ierr

 !SET TO LARGE VALUES CHECK MGgrids to ensure to large values are entering stencil
 !if they are something is wrong with a boundary condtion set for the stencil
 do k=1,ngrid
  dxck(:)=99999999999.d0
  dxek(:)=99999999999.d0
  dyck(:)=99999999999.d0
  dyek(:)=99999999999.d0
  dzck(:)=99999999999.d0
  dzek(:)=99999999999.d0
 enddo

!Fill the 1d grid with fine grid data
 !Finest Grid
 lvl=ngrid
 kx1=kpx1(lvl)
 kx2=kpx2(lvl)
 kx3=kpx3(lvl)
 nxm=nxk(lvl) 
 nym=nyk(lvl) 
 nzm=nzk(lvl) 

 xLL=xe(nxm+1)-xe(1)
 yLL=ye(nym+1)-ye(1)
 zLL=ze(nzm+1)-ze(1)

 do i2=1,nxm+1
  i=i2+kx1-1
   xek(i)=xe(i2)
   xck(i)=xc(i2)
   dxek(i)=dxe(i2)
   dxck(i)=dxc(i2)
 enddo
 do j2=1,nym+1
  j=j2+kx2-1
  yek(j)=ye(j)
  yck(j)=yc(j)
  dyek(j)=dye(j)
  dyck(j)=dyc(j)
 enddo
 do k2=1,nzm+1
  k=k2+kx3-1
  zek(k)=ze(k)
  zck(k)=zc(k)
  dzek(k)=dze(k)
  dzck(k)=dzc(k)
 enddo

 !X2max
  ! Tbc(1)
 !X3min
  ! Tbc(2)
 !X2min
  ! Tbc(3)
 !X3max
  !Tbc(4)
 !X1min
  ! Tbc(5)
 !X1max
  ! Tbc(6)

 !2=Dirichlet
 !1=Neumann
 !0=Periodic

 !GENERATE GRIDS AT COURSER LEVELS
 !##################
 !#####X1 GRID######
 !##################

 do lvl=ngrid,(ngrid-nx1Levels+2),-1
  lF=lvl                !FINE LEVEL
  lC=lvl-1              !COURSE LEVEL
  nx1c=nxk(lC)          !Number of X1 points at COURSE LEVEL
  nx1f=nxk(lF)          !Number of X1 points at FINE LEVEL
  kx1C=kpx1(lC)         !COURSE OFFSET 
  kx1F=kpx1(lF)         !FINE OFFSET   

  if( Tbc(5).EQ.0 ) then  !#P1    !THE GRID IS EVENLY SPACED AND WE WANT TO ENSURE THE PERIODIC LENGTH IS MAINTAINED AT ALL LEVELS
   dxx=xLL/dble(nx1c)

   do i=1,nx1c+1
    ic1=i+kx1C-1
    xek(ic1) = -dxx + dxx*dble(i)
   enddo

   do i=2,nx1c+1
    ic1=i+kx1C-1
    xck(ic1)  = ( xek(ic1)+xek(ic1-1) )/2.d0
   enddo
   xck(kx1C) = xek(kx1C)-0.5d0*dxx

   do i=1,nx1c+1
    ic1=i+kx1C-1
    dxck(ic1) = dxx
    dxek(ic1) = dxx
   enddo

  else
   iif=kx1F
   iic=kx1C
   xek(iic)=xek(iif)

   do ic1=2,nx1C                        !LOOP OVER COURSE CELLS STARTING AT LEFT MOST CELL+1
    iif=(kx1F-1)+2*ic1-2                     !FINE POSITION w.r.t. COURSE INDEX
    iic=(kx1C-1)+ic1
    xck(iic) = xck(iif) + dxck(iif)*0.5d0
    xek(iic) = xck(iic) + dxck(iif)
   enddo

   !X1-min 
   iic=kx1C
   iif=kx1F
   if( Tbc(5).EQ.2 ) then      !DIRICHLET
    xck(iic)=xck(iif)
   else                            !NEUMANN
    xck(iic)=xck(iif)-0.5d0*dxek(iif+1)
   endif

   !X1-max 
   iic=kx1C+nx1C
   iif=kx1F+nx1F
   if ( Tbc(6).EQ.2 ) then     !DIRICHLET
    xck(iic)=xck(iif)
    xek(iic)=xek(iif)
   else                           !NEUMANN
    xck(iic)=xck(iif)+0.5d0*dxek(iif-1)
    xek(iic)=xek(iif)+0.5d0*dxck(iif-1)
   endif

   do ic1=2,nx1C                  !LOOP OVER COURSE CELLS TO CALCULATE dxek, dxck 
    iic=(kx1C-1)+ic1
    dxek(iic)=xck(iic+1)-xck(iic)
    dxck(iic)=xek(iic)-xek(iic-1)
   enddo

  !X1-min EDGE
   iic=kx1C
   dxck(iic) = 0.d0

   if ( Tbc(5).EQ.2) then       !DIRICHLET BC
    dxek(iic)=xck(iic+1)-xck(iic)
   else                             !NEUMANN
    dxek(iic)=dxek(iic+1)
   endif

  !X1-max EDGE
   iic=kx1c+nx1C
   if ( Tbc(6).EQ.2) then       !DIRICHLET 
    dxek(iic)=xck(iic)-xck(iic-1)
    dxck(iic)=xek(iic)-xek(iic-1)
   else                           !NEUMANN
    dxek(iic)=dxek(iic-1)
    dxck(iic)=dxck(iic-1)
   endif

 endif !#P1

enddo !x1-grids
 !FILL TO NGRIDS WITH COURSEST
 do lvl=(ngrid-nx1Levels+1),2,-1
  lF=lvl                !FINE LEVEL
  lC=lvl-1              !COURSE LEVEL
  nx1c=nxk(lC)          !Number of X1 points at COURSE LEVEL
  nx1f=nxk(lF)          !Number of X1 points at FINE LEVEL
  kx1C=kpx1(lC)         !COURSE OFFSET 
  kx1F=kpx1(lF)         !FINE OFFSET   
  do i=1,nx1c+1
   iif=(kx1F-1)+i
   iic=(kx1C-1)+i
   xek(iic)=xek(iif)
   xck(iic)=xck(iif)
   dxek(iic)=dxek(iif)
   dxck(iic)=dxck(iif)
  enddo
 enddo

 !##################
 !#####X2 GRID######
 !##################
 do lvl=ngrid,(ngrid-nx2Levels+2),-1
  lF=lvl                !FINE LEVEL
  lC=lvl-1              !COURSE LEVEL
  nx2c=nyk(lC)          !Number of X2 points at COURSE LEVEL
  nx2f=nyk(lF)          !Number of X2 points at FINE LEVEL
  kx2C=kpx2(lC)         !COURSE OFFSET 
  kx2F=kpx2(lF)         !FINE OFFSET   

  if( Tbc(3).EQ.0 ) then  !#P2    !THE GRID IS EVENLY SPACED AND WE WANT TO ENSURE THE PERIODIC LENGTH IS MAINTAINED AT ALL LEVELS
   dyy=yLL/dble(nx2c)

   do j=1,nx2c+1
    jc1=j+kx2C-1
    yek(jc1) = -dyy + dyy*dble(j)
   enddo

   do j=2,nx2c+1
    jc1=j+kx2C-1
    yck(jc1)  = ( yek(jc1)+yek(jc1-1) )/2.d0
   enddo
   yck(kx2C) = yek(kx2C)-0.5d0*dyy

   do j=1,nx2c+1
    jc1=j+kx2C-1
    dyck(jc1) = dyy
    dyek(jc1) = dyy
   enddo

  else

   jjf=kx2F
   jjc=kx2C
   yek(jjc)=yek(jjf)

   do jc1=2,nx2C                        !LOOP OVER COURSE CELLS STARTING AT LEFT MOST CELL+1
    jjf=(kx2F-1)+2*jc1-2                     !FINE POSITION w.r.t. COURSE INDEX
    jjc=(kx2C-1)+jc1
    yck(jjc) = yck(jjf) + dyck(jjf)*0.5d0
    yek(jjc) = yck(jjc) + dyck(jjf)
   enddo

   !X2-min 
   jjc=kx2C
   jjf=kx2F
   if( Tbc(3).EQ.2 ) then      !DIRICHLET
    yck(jjc)=yck(jjf)
   else                            !NEUMANN
    yck(jjc)=yck(jjf)-0.5d0*dyek(jjf+1)
   endif

   !X2-max 
   jjc=kx2C+nx2C
   jjf=kx2F+nx2F
   if ( Tbc(1).EQ.2 ) then     !DIRICHLET
    yck(jjc)=yck(jjf)
    yek(jjc)=yek(jjf)
   else                           !NEUMANN
    yck(jjc)=yck(jjf)+0.5d0*dyek(jjf-1)
    yek(jjc)=yek(jjf)+0.5d0*dyck(jjf-1)
   endif

   do jc1=2,nx2C                  !LOOP OVER COURSE CELLS TO CALCULATE dyek, dyck 
    jjc=(kx2C-1)+jc1
    dyek(jjc)=yck(jjc+1)-yck(jjc)
    dyck(jjc)=yek(jjc)-yek(jjc-1)
   enddo

  !X2-min EDGE
   jjc=kx2C
   dyck(jjc) = 0.d0

   if ( Tbc(3).EQ.2) then       !DIRICHLET BC
    dyek(jjc)=yck(jjc+1)-yck(jjc)
   else                             !NEUMANN
    dyek(jjc)=dyek(jjc+1)
   endif

  !X2-max EDGE
   jjc=kx2c+nx2C
   if ( Tbc(1).EQ.2) then       !DIRICHLET 
    dyek(jjc)=yck(jjc)-yck(jjc-1)
    dyck(jjc)=yek(jjc)-yek(jjc-1)
   else                           !NEUMANN
    dyek(jjc)=dyek(jjc-1)
    dyck(jjc)=dyck(jjc-1)
   endif

 endif !#P2

enddo !x2-grids

 !FILL TO NGRIDS WITH COURSEST
 do lvl=(ngrid-nx2Levels+1),2,-1
  lF=lvl                !FINE LEVEL
  lC=lvl-1              !COURSE LEVEL
  nx2c=nyk(lC)          !Number of X1 points at COURSE LEVEL
  nx2f=nyk(lF)          !Number of X1 points at FINE LEVEL
  kx2C=kpx2(lC)         !COURSE OFFSET 
  kx2F=kpx2(lF)         !FINE OFFSET   
  do j=1,nx2c+1
   jjf=(kx2F-1)+j
   jjc=(kx2C-1)+j
   yek(jjc)=yek(jjf)
   yck(jjc)=yck(jjf)
   dyek(jjc)=dyek(jjf)
   dyck(jjc)=dyck(jjf)
  enddo
 enddo

 !##################
 !#####X3 GRID######
 !##################

 do lvl=ngrid,(ngrid-nx3Levels+2),-1
  lF=lvl                !FINE LEVEL
  lC=lvl-1              !COURSE LEVEL
  nx3c=nzk(lC)          !Number of X3 points at COURSE LEVEL
  nx3f=nzk(lF)          !Number of X3 points at FINE LEVEL
  kx3C=kpx3(lC)         !COURSE OFFSET 
  kx3F=kpx3(lF)         !FINE OFFSET   


  if( Tbc(2).EQ.0 ) then  !#P3    !THE GRID IS EVENLY SPACED AND WE WANT TO ENSURE THE PERIODIC LENGTH IS MAINTAINED AT ALL LEVELS
   dzz=zLL/dble(nx3c)

   do k=1,nx3c+1
    kc1=k+kx3C-1
    zek(kc1) = -dzz + dzz*dble(k)
   enddo

   do k=2,nx3c+1
    kc1=k+kx3C-1
    zck(kc1)  = ( zek(kc1)+zek(kc1-1) )/2.d0
   enddo
   zck(kx3C) = zek(kx3C)-0.5d0*dzz

   do k=1,nx3c+1
    kc1=k+kx3C-1
    dzck(kc1) = dzz
    dzek(kc1) = dzz
   enddo

  else

   kkf=kx3F
   kkc=kx3C
   zek(kkc)=zek(kkf)

   do kc1=2,nx3C                        !LOOP OVER COURSE CELLS STARTING AT LEFT MOST CELL+1
    kkf=(kx3F-1)+2*kc1-2                     !FINE POSITION w.r.t. COURSE INDEX
    kkc=(kx3C-1)+kc1
    zck(kkc) = zck(kkf) + dzck(kkf)*0.5d0
    zek(kkc) = zck(kkc) + dzck(kkf)
   enddo

   !X3-min 
   kkc=kx3C
   kkf=kx3F
   if( Tbc(2).EQ.2 ) then      !DIRICHLET
    zck(kkc)=zck(kkf)
   else                            !NEUMANN
    zck(kkc)=zck(kkf)-0.5d0*dzek(kkf+1)
   endif

   !X3-max 
   kkc=kx3C+nx3C
   kkf=kx3F+nx3F
   if ( Tbc(4).EQ.2 ) then     !DIRICHLET
    zck(kkc)=zck(kkf)
    zek(kkc)=zek(kkf)
   else                           !NEUMANN
    zck(kkc)=zck(kkf)+0.5d0*dzek(kkf-1)
    zek(kkc)=zek(kkf)+0.5d0*dzck(kkf-1)
   endif

   do kc1=2,nx3C                  !LOOP OVER COURSE CELLS TO CALCULATE dzek, dzck 
    kkc=(kx3C-1)+kc1
    dzek(kkc)=zck(kkc+1)-zck(kkc)
    dzck(kkc)=zek(kkc)-zek(kkc-1)
   enddo

  !X3-min EDGE
   kkc=kx3C
   dzck(kkc) = 0.d0

   if ( Tbc(2).EQ.2) then       !DIRICHLET BC
    dzek(kkc)=zck(kkc+1)-zck(kkc)
   else                             !NEUMANN
    dzek(kkc)=dzek(kkc+1)
   endif

  !X3-max EDGE
   kkc=kx3c+nx3C
   if ( Tbc(4).EQ.2) then       !DIRICHLET 
    dzek(kkc)=zck(kkc)-zck(kkc-1)
    dzck(kkc)=zek(kkc)-zek(kkc-1)
   else                           !NEUMANN
    dzek(kkc)=dzek(kkc-1)
    dzck(kkc)=dzck(kkc-1)
   endif

 endif !#P3
enddo !x3-grids

 !FILL TO NGRIDS WITH COURSEST
 do lvl=(ngrid-nx3Levels+1),2,-1
  lF=lvl                !FINE LEVEL
  lC=lvl-1              !COURSE LEVEL
  nx3c=nzk(lC)          !Number of X1 points at COURSE LEVEL
  nx3f=nzk(lF)          !Number of X1 points at FINE LEVEL
  kx3C=kpx3(lC)         !COURSE OFFSET 
  kx3F=kpx3(lF)         !FINE OFFSET   
  do k=1,nx3c+1
   kkf=(kx3F-1)+k
   kkc=(kx3C-1)+k
   zek(kkc)=zek(kkf)
   zck(kkc)=zck(kkf)
   dzek(kkc)=dzek(kkf)
   dzck(kkc)=dzck(kkf)
  enddo
 enddo


do lvl=1,ngrid
 call mg_coeff(lvl,ierr)
 call mg_write_grid(lvl,ierr)              
enddo

 stat=0
 write(IOUTmg,'(a)') "MG GRID SETUP COMPLETED"

 return

 1000 continue
 stat=-1
 write(IOUTmg,'(a)') "MG GRID SETUP FAILED"
 return
end subroutine mg_grids

subroutine mg_write_grid(lvl,stat)
!@t
! \textbf{subroutine mg\_write\_grid(lvl,stat)}
!@h
!    Description:
!      Write the grid at a given level out to a file
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: i4,r8
 use mgVars
 implicit none
 
 !Passed Variables
 integer(i4),intent(in)  :: lvl
 integer(i4),intent(out) :: stat

 !Local Variables
 character(len=100) :: gridfile
 integer(i4)        :: s1
 integer(i4)        :: i,i2,j,j2,k,k2

  !##################
  !#####X1 GRID######
  !##################
  write(gridfile,'(a,i3.3,a)') trim(mgDIRmg)//'/x1_grid_',lvl,'.dat'
  open(unit=207,file=gridfile,status='unknown',form='formatted',iostat=s1)
  if (s1.NE.0) then
   close(207)
   write(IOUTmg,'(a,i4)') "ERROR OPENING FILE: "//trim(gridfile)//" IOSTAT= ",s1
   stat=1
   goto 1000
  endif 
   write(207,'(a)') "#X1-GRID"
   write(207,'(a)') "#LEVEL nx"
   write(207,'(a,2(1x,i4))') "#",lvl,nxk(lvl)+1
   write(207,'(a)') "#i xek(i) xck(i) dxek(i) dxck(i) pcf_1(i) pcf_2(i)"
   do i2=1,nxk(lvl)+1
    i=i2+kpx1(lvl)-1
    write(207,'(i4,6(1x,e15.8))') i2,xek(i),xck(i),dxek(i),dxck(i),pcf_1(i),pcf_2(i)
   enddo
  close(207)
   
  !##################
  !#####X2 GRID######
  !##################
  write(gridfile,'(a,i3.3,a)') trim(mgDIRmg)//'/x2_grid_',lvl,'.dat'
  open(unit=208,file=gridfile,status='unknown',form='formatted',iostat=s1)
  if (s1.NE.0) then
   close(208)
   write(IOUTmg,'(a,i4)') "ERROR OPENING FILE: "//trim(gridfile)//" IOSTAT= ",s1
   stat=1
   goto 1000
  endif
   write(208,'(a)') "#X2-GRID"
   write(208,'(a)') "#LEVEL ny"
   write(208,'(a,2(1x,i4))') "#",lvl,nyk(lvl)+1
   write(208,'(a)') "#j yek(j) yck(j) dyek(j) dyck(j) pcf_3(j) pcf4(j)"
   do j2=1,nyk(lvl)+1
    j=j2+kpx2(lvl)-1
    write(208,'(i4,6(1x,e15.8))') j2,yek(j),yck(j),dyek(j),dyck(j),pcf_3(j),pcf_4(j)
   enddo
  close(208)

    
  !##################
  !#####X3 GRID######
  !##################
  write(gridfile,'(a,i3.3,a)') trim(mgDIRmg)//'/x3_grid_',lvl,'.dat'
  open(unit=209,file=gridfile,status='unknown',form='formatted',iostat=s1)
  if (s1.NE.0) then
   close(209)
   write(IOUTmg,'(a,i4)') "ERROR OPENING FILE: "//trim(gridfile)//" IOSTAT= ",s1
   stat=1
   goto 1000
  endif
   write(209,'(a)') "#X3-GRID"
   write(209,'(a)') "#LEVEL nz"
   write(209,'(a,2(1x,i4))') "#",lvl,nzk(lvl)+1
   write(209,'(a)') "#k zek(k) zck(k) dzek(k) dzck(k) pcf_5(k) pcf_6(k)"
   do k2=1,nzk(lvl)+1
    k=k2+kpx3(lvl)-1
    write(209,'(i4,6(1x,e15.8))') k2,zek(k),zck(k),dzek(k),dzck(k),pcf_5(k),pcf_6(k)
   enddo
  close(209)


 stat=0
 return

 1000 continue
 return

end subroutine mg_write_grid
