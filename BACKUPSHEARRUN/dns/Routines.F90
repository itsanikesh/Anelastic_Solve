
subroutine gravity(ok)
!@t
! \textbf{subroutine gravity(ok)}
!@h
!   Description:
!     Slowly turns on gravity. KYLE IS THIS CORRECT?
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
!     Ramp function should bring g smoothly from 0 to g_z_nd_orig in time
!     gt2-gt1. Using tanh this should be
!     g(t) = g/2*( 1 + tanh( (time-gt1)/2-2.8) )
!     Running from sz-1:ez+1 automatically ghosts the output.
!@q

 use Parameters, only: g, time, gt1, gt2, g_orig
 use IO, only: IOUT
 implicit none

 !Passed Variables
 integer,intent(out) :: ok
 ok=0

 if (time.LT.gt1) then
   g = 0.d0
 elseif (time.GT.gt1.AND.time.LT.gt2) then
! tanh ramping
!   g = g_orig * dtanh( 3.d0*(time-gt1)/(gt2-gt1) )
! linear ramping
   g = g_orig * (time-gt1)/(gt2-gt1) 
! originals seem off KYLE DO WE NEED THESE?
!   g = g_orig/2.d0*( 1.d0 + dtanh( (time-gt1)/2.d0 - 1.d0 ) )
!  g = g_orig/2.d0*(1.d0+dtanh( (time-gt1)/2.d0-2.8d0 ) )
 elseif (time.GE.gt2) then
   g = g_orig
 endif

 if (g.LT.g_orig) write(IOUT,'(a,2(2x,f12.6))') "   GRAVITY: ",g

 return
end subroutine gravity

subroutine divergence(un,vn,wn,tempField,plane_write,ierr)
!@t
! \textbf{subroutine divergence(un,vn,wn,tempField,plane\_write,ierr)}
!@h
!   Description:
!     Calculate the divergence of a given field.
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
!     The plane of maximum divergence can be output if desired. This 
!     subroutine automatically ghosts u,v,w.
!@q


 use ntypes, only: r8
 use Grid,   only: rdxc,rdyc,rdzc,xc,yc,zc
 use Domain, only: sx,ex,sy,ey,sz,ez
#ifdef PARALLEL
 use dd,     only: comm3d,myid,MPI_MAX, MPI_SUM, realtype,&
                    inttype, MPI_ANY_SOURCE, sizeX1X2X3
#endif
 use IO, only: IOUT
 implicit none

!Passed Variables
 real(r8),intent(inout)        :: un(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) 
 real(r8),intent(inout)        :: vn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) 
 real(r8),intent(inout)        :: wn(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) 
 real(r8),intent(out)       :: tempField(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1) 
 logical,intent(in)         :: plane_write
 integer,intent(out)        :: ierr

!Local Variables
 real(r8) :: u1_x1,u2_x2,u3_x3,rdiv,rdiv_sum,vol_sum, avgDiv,MaxDiv
 integer  :: i,j,k
 integer  :: flag, Mflag
 real(r8) :: uixi(1:3)
 integer  :: LocMaxDiv(1:3)
 integer  :: ir1,ir2

 real(r8),parameter :: small=1d-14

 call ghost(un,'u',ir1)
 call ghost(vn,'v',ir1)
 call ghost(wn,'w',ir1)

 ir1=0
 ir2=0
 ierr=0
 tempField(:,:,:) = 0.d0
 rdiv             = 0.d0
 rdiv_sum         = 0.d0
 vol_sum          = 0.d0
 MaxDiv           = 0.d0
 avgDiv           = 0.d0
 uixi(:)          = 0.d0
 LocMaxDiv(:)     = -1

 do k=sz,ez
  do j=sy,ey
   do i=sx,ex
    u1_x1=( un(i,j,k)-un(i-1,j,k) )*rdxc(i)
    u2_x2=( vn(i,j,k)-vn(i,j-1,k) )*rdyc(j)
    u3_x3=( wn(i,j,k)-wn(i,j,k-1) )*rdzc(k)
    rdiv = u1_x1 + u2_x2 + u3_x3
    tempField(i,j,k) = rdiv
    !Check if current div is max
    if (dabs(rdiv).GT.MaxDiv) then
     LocMaxDiv(1) = i
     LocMaxDiv(2) = j
     LocMaxDiv(3) = k
     MaxDiv       = rdiv
     uixi(1)      = u1_x1
     uixi(2)      = u2_x2
     uixi(3)      = u3_x3
    endif
    rdiv_sum = rdiv_sum  + abs(rdiv)/( rdxc(i)*rdyc(j)*rdzc(k) ) 
    vol_sum =  vol_sum   + 1.d0/( rdxc(i)*rdyc(j)*rdzc(k) )
   enddo
  enddo
 enddo

 rdiv = rdiv_sum/vol_sum
#ifdef PARALLEL
 call MPI_ALLREDUCE(rdiv,avgDiv,1,realtype,MPI_SUM,comm3d,ir2)
!Average
 avgDiv = avgDiv/sizeX1X2X3
#else
avgDiv = rdiv
#endif
 rdiv=MaxDiv

#ifdef PARALLEL
 call MPI_ALLREDUCE(rdiv,maxDiv,1,realtype,MPI_MAX,comm3d,ir2)
!Maximum
 flag=0
 if ( dabs(rdiv-MaxDiv).LT.1d-14 ) then
  flag=myid
 endif
 call MPI_ALLREDUCE(flag,Mflag,1,inttype,MPI_MAX,comm3d,ir2)         !Average
 call MPI_BCAST(LocMaxDiv,3,inttype,Mflag,comm3d,ir2)                !Location
 call MPI_BCAST(uixi,3,realtype,Mflag,comm3d,ir2)                    !Components
#endif

write (IOUT,'(a,e15.8,a,e15.8)') "    DIVERGENCE:     AVG=  ", avgDiv, "   MAX=   ",maxDiv
 if (LocMaxDiv(1).GT.0.AND.LocMaxDiv(2).GT.0.AND.LocMaxDiv(3).GT.0) then
write (IOUT,'(a,3(i4,a),3(f8.4,a))') "                    LOCATION [i],[xi]:  [" &
                                 ,LocMaxDiv(1),",",LocMaxDiv(2),",",LocMaxDiv(3),"],[",xc(LocMaxDiv(1)),&
                                                                     ",",yc(LocMaxDiv(2)),",",zc(LocMaxDiv(3)),"]"
write (IOUT,'(a,3(e12.4,a))') "                    COMPONENTS [dui/dxi]:  [",uixi(1),",",uixi(2),",",uixi(3),"]"
endif

 if (plane_write) then
  call write_plane(tempField,1,LocMaxDiv(1),0,1,'DIV',.true.,ir1)
  call write_plane(tempField,2,LocMaxDiv(2),0,1,'DIV',.true.,ir1)
  call write_plane(tempField,3,LocMaxDiv(3),0,1,'DIV',.true.,ir1)
 endif
 ierr=max(ir1,ir2)
 return
end subroutine divergence


subroutine calcdt(delt,stat)
!@t
! \textbf{subroutine calcdt(delt,stat)}
!@h
!   Description:
!     Calculate the timestep based on either the CFL condition or diffusive
!     limit.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use Grid,    only: rdxc, rdyc, rdzc
 use ntypes, only: r8
 use flow,   only: u,v,w
 use Domain, only: sx,ex,sy,ey,sz,ez, nxp2, nyp2, nzp2
#ifdef PARALLEL
 use dd,     only: comm3d, myid, MPI_MAX, realtype 
#endif
 use Parameters, only: dtmax,Ddt,cfl
 use IO, only: IOUT
 implicit none

!Passed Variables
 real(r8),intent(out) :: delt
 integer,intent(out)  :: stat
!Local Variables
 real(r8)             :: adv_term, adv_term_max
 integer              :: i,j,k,err1,err2
 logical,parameter    :: debug=.false.


 err1=0
 err2=0

 adv_term_max=0.d0

 do k=sz,ez
  do j=sy,ey
   do i=sx,ex
     adv_term = rdxc(i)*dabs(0.5d0*(u(i,j,k)+u(i-1,j,k))) &
              + rdzc(k)*dabs(0.5d0*(w(i,j,k)+w(i,j,k-1))) &
              + rdyc(j)*dabs(0.5d0*(v(i,j,k)+v(i,j-1,k)))

     adv_term_max=max(adv_term,adv_term_max)

   enddo
  enddo
 enddo

#ifdef PARALLEL
 adv_term=adv_term_max
 call MPI_ALLREDUCE(adv_term,adv_term_max,1,realtype,MPI_MAX,comm3d,err1)
 if (myid.EQ.0) then
#endif
    delt=cfl/adv_term_max!dtmax        !When advection is small prevent dt-->infinity
    write(IOUT,'(a18,f15.8,a6,f15.8)') "Time limit: conv.=",delt,"diff.=",Ddt

!  delt=min(delt,Ddt) !Diffusive Limit calculated in grid_setup
#ifdef PARALLEL
 endif
 call MPI_BCAST(delt,1,realtype,0,comm3d,err2)
#endif
 stat=max(err1,err2)
return
end subroutine calcdt

subroutine check_point(outstring,kill)
!@t
! \textbf{subroutine check\_point(outstring,kill)}
!@h
!   Description:
!     Stop something if ..  Why does this exist Kyle???
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

#ifdef PARALLEL
 use dd, only:commx1x2x3, myid
#endif
  
 implicit none
 logical,intent(in)   :: kill
 character(len=*),intent(in) :: outstring

 integer ::ok

#ifdef PARALLEL
  call MPI_BARRIER(commx1x2x3,ok)
  write(6,'(a,i5)') trim(outstring),myid
  call MPI_BARRIER(commx1x2x3,ok)
#else
  write(6,'(a,i5)') trim(outstring)
#endif
  if (kill) stop

 return
end subroutine check_point 


subroutine clipVS(Var,Vmin,Vmax,stat)
!@t
! \textbf{subroutine clipVS(Var,Vmin,Vmax,stat)}
!@h
!   Description:
!     Clip a given variable for the Vertical Shear Layer.
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
!     Prevents unphysical accumulation of quantities like density??? KYLE?
!@q

 use ntypes, only: r8, i4
 use Flow,   only: r_tmp2
 use Domain, only: sx,ex,sy,ey,sz,ez
 use IO,     only: IOUT
#ifdef PARALLEL
 use dd,     only: comm3d, MPI_SUM, inttype
#endif
 implicit none

!Passed Variables
 real(r8),intent(inout)  :: var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 real(r8),intent(in)     :: Vmin, Vmax 
 integer,intent(out)     :: stat

!Local Variables
 integer(i4) :: Lclip(sz-1:ez+1), Uclip(sz-1:ez+1)
 integer     :: i,j,k, ierr
 integer(i4) :: iclip(2), iclipT(2)
 real(r8),parameter :: tol=1d-5
 real(r8)           :: tmp

 stat=0 

 iclip = 0
 iclipT =0
 Uclip = 0 
 Lclip = 0 

 do k=sz,ez
  do j=sy,ey
   do i=sx,ex

    if (Var(i,j,k).LT.Vmin) then
     Var(i,j,k) = Vmin
     iclip(1) = iclip(1) + 1
     Lclip(k) = Lclip(k)+1
    endif

    if(Var(i,j,k).GT.Vmax) then
     Var(i,j,k) = Vmax
     iclip(2) = iclip(2) + 1
     Uclip(k) = Uclip(k)+1
    endif
       
   enddo
  enddo
 enddo

#ifdef PARALLEL
 call MPI_ALLREDUCE(iclip(1),iclipT(1),1,inttype,MPI_SUM,comm3d,ierr) 
 call MPI_ALLREDUCE(iclip(2),iclipT(2),1,inttype,MPI_SUM,comm3d,ierr) 
#endif

 write(IOUT,'(a15,2(1x,i10))') "POINTS CLIPPED: ",iclipT(1),iclipT(2)

 stat=max(stat,ierr)
 return
end subroutine clipVS

subroutine psponge(var,pstep,stat)
!@t
! \textbf{subroutine psponge(var,pstep,stat)}
!@h
!   Description:
!     Set up a sponge region for a given variable.
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
!     KYLE CAN YOU ADD A LINE ABOUT THIS???
!@q

 use ntypes, only: r8, i4
 use Domain, only: sx,ex,sy,ey,sz,ez,nx,ny,nz
 use IO,     only: IOUT
 implicit none

 !Passed Variables
 integer,intent(in)      :: pstep
 real(r8),intent(inout)  :: var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(out)     :: stat

 !Local Variables
 integer                 :: j,k

 if (sy.lt.3.and.sz.lt.3) then
  !top left corner
  do k = sz-1,sz+pstep 
   do j = sy-1,sy+pstep
    var(:,k,j) = 0.d0
   enddo
  enddo
 endif

 if (sy.lt.3.and.ez.gt.nz-1) then
  !bottom left corner
  do k = ez-pstep,ez+1
   do j = sy-1,sy+pstep
    var(:,k,j) = 0.d0
   enddo
  enddo
 endif

 if (ey.gt.ny-1.and.sz.lt.3) then
 !top right corner
  do k = ez-pstep,ez+1
   do j = ey-pstep,ey+1
    var(:,k,j) = 0.d0
   enddo
  enddo
 endif

 if (ey.gt.ny-1.and.ez.gt.nz-1) then
 !bottom right corner
  do k = ez-pstep,ez+1
   do j = ey-pstep,ey+1
    var(:,k,j) = 0.d0
   enddo
  enddo
 endif

stat=0
 return
end subroutine

subroutine energy(ok)
!@t
! \textbf{subroutine energy(ok)}
!@h
!   Description:
!     Calculates the energy budget of a turbulent flow and writes to a file??? KYLE IS THIS TRUE?
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 
                                                                 
 use ntypes, only: r8,i4
 use Flow,   only: u,v,w,p,rho,u1_tmp2,u2_tmp2,u3_tmp2,r_tmp1
 use domain, only: sz,ez,nzp2, sy, ey, nyp2, sx,ex,nxp2, nx, ny, nz
 use grid,   only: zc, dye,dxe,dze
 use IO,     only: statDIR,IOUT
 use parameters, only: nstep, time, g, rho_0, Re, Pr, delt
#ifdef PARALLEL
 use dd,     only: myid,comm3d,MPI_SUM,MPI_MAX,realtype
#endif
 implicit none
                                                                                                                             
!Passed Variables
 integer,intent(out) :: ok
                                                                                                                             
!Local Variables
 character(len=250) :: filen1
 integer            :: s1,err1,i,j,k 
 real(r8)           :: PE, KinE, KEtot, PEtot, MASS, MASStot, MOM, MOMtot
 real(r8)           :: dV
 integer            :: is,ie,js,je,ks,ke
 real(r8)           :: umax,vmax,wmax,pmax,rhomax
 real(r8)           :: umaxG,vmaxG,wmaxG,pmaxG,rhomaxG
 real(r8)           :: umin,vmin,wmin,pmin,rhomin
 real(r8)           :: uminG,vminG,wminG,pminG,rhominG

 err1=0
 s1=0

 u1_tmp2 = 0.d0
 u2_tmp2 = 0.d0
 u3_tmp2 = 0.d0

 call center_velocity(u,u1_tmp2,1 )
 call center_velocity(v,u2_tmp2,2 )
 call center_velocity(w,u3_tmp2,3 )

 KinE = 0.d0
 PE   = 0.d0
 MASS = 0.d0
 MOM  = 0.d0

 is=0
 ie=0
 js=0
 je=0
 ks=0
 ke=0

 !INCLUDE BOUNDARIES
 if ( sx.LT.3      )   is=1
 if ( ex.GT.nx-1   )   ie=1
 if ( sy.LT.3      )   js=1
 if ( ey.GT.ny-1   )   je=1
 if ( sz.LT.3      )   ks=1
 if ( ez.GT.nz-1   )   ke=1

 do k=sz-ks,ez+ke
  do j=sy-js,ey+je
   do i=sx-is,ex+ie
    dV = dxe(i)*dye(j)*dze(k)
    KinE = KinE + 0.5d0*rho(i,j,k)*(u1_tmp2(i,j,k)**2+u2_tmp2(i,j,k)**2+u3_tmp2(i,j,k)**2)*dV
    PE   = PE + rho(i,j,k)*(zc(k))*dV
    MASS = rho(i,j,k)*dV 
    MOM  = rho(i,j,k)*(u1_tmp2(i,j,k)+u2_tmp2(i,j,k)+u3_tmp2(i,j,k))*dV
   enddo
  enddo
 enddo
 KEtot=0.d0
 PEtot=0.d0
 MASStot=0.d0
 MOMtot=0.d0
 umax=maxval((u))
 vmax=maxval((v))
 wmax=maxval((w))
 pmax=maxval((p))
 rhomax=maxval((rho))
 umin=maxval((-u))
 vmin=maxval((-v))
 wmin=maxval((-w))
 pmin=maxval((-p))
 rhomin=maxval((-rho))

#ifdef PARALLEL
 call MPI_ALLREDUCE(KinE,KEtot,1,realtype,MPI_SUM,comm3d,err1)
 call MPI_ALLREDUCE(PE,PEtot,1,realtype,MPI_SUM,comm3d,err1)
 call MPI_ALLREDUCE(MASS,MASStot,1,realtype,MPI_SUM,comm3d,err1)
 call MPI_ALLREDUCE(MOM,MOMtot,1,realtype,MPI_SUM,comm3d,err1)
 call MPI_ALLREDUCE(umax,umaxG,1,realtype,MPI_MAX,comm3d,err1)
 call MPI_ALLREDUCE(vmax,vmaxG,1,realtype,MPI_MAX,comm3d,err1)
 call MPI_ALLREDUCE(wmax,wmaxG,1,realtype,MPI_MAX,comm3d,err1)
 call MPI_ALLREDUCE(pmax,pmaxG,1,realtype,MPI_MAX,comm3d,err1)
 call MPI_ALLREDUCE(rhomax,rhomaxG,1,realtype,MPI_MAX,comm3d,err1)
 call MPI_ALLREDUCE(umin,uminG,1,realtype,MPI_MAX,comm3d,err1)
 call MPI_ALLREDUCE(vmin,vminG,1,realtype,MPI_MAX,comm3d,err1)
 call MPI_ALLREDUCE(wmin,wminG,1,realtype,MPI_MAX,comm3d,err1)
 call MPI_ALLREDUCE(pmin,pminG,1,realtype,MPI_MAX,comm3d,err1)
 call MPI_ALLREDUCE(rhomin,rhominG,1,realtype,MPI_MAX,comm3d,err1)
 if (myid.EQ.0) then 
  filen1=trim(statDIR)//"Energy.dat"
  open(unit=100,file=filen1,position='append',form='formatted',iostat=s1)
   write(100,'(i6,6(4x,d22.15))') nstep,time, KEtot, PEtot, KEtot+PEtot,MASStot, MOMtot
  close(100)
  filen1=trim(statDIR)//"Maxes.dat"
  open(unit=101,file=filen1,position='append',form='formatted',iostat=s1)
   write(101,'(i6,11(4x,d22.15))') nstep,time,umaxG,vmaxG,wmaxG,pmaxG,rhomaxG,uminG,vminG,wminG,pminG,rhominG 
  close(101)
 endif
 call MPI_BARRIER(comm3d,err1)
#else
  filen1=trim(statDIR)//"Energy.dat"
  open(unit=100,file=filen1,position='append',form='formatted',iostat=s1)
   write(100,'(i6,6(4x,d22.15))') nstep,time, KinE, PE, KE+PE,MASS, MOM
  close(100)
  filen1=trim(statDIR)//"Maxes.dat"
  open(unit=101,file=filen1,position='append',form='formatted',iostat=s1)
   write(101,'(i6,11(4x,d22.15))') nstep,time,umax,vmax,wmax,pmax,rhomax,umin,vmin,wmin,pmin,rhomin 
  close(101)
#endif

 ok=max(s1,err1)
 return
end subroutine energy

subroutine updateMINMAX(Var,minvar,maxvar,stat)
!@t
! \textbf{subroutine avgX1X2(Var,outm,outp,vtype)}
!@h
!   Description:
!     Averages over 1st and 2nd dimensions.
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
 use domain, only: sx,ex,sy,ey,sz,ez,nzp2
 use grid,   only: dxc, dyc,dxe,dye, dzc, dze
 use IO,     only: IOUT
#ifdef PARALLEL
 use dd,     only: realtype, MPI_MAX,comm3d
#endif
 implicit none

!Passed Variables
 real(r8),dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)  :: Var
 real(r8),intent(inout)                                          :: maxvar,minvar
 integer,intent(out) :: stat

!Local Variables
 real(r8),dimension( sz-1:ez+1 )                   :: mean, rms
 real(r8) :: maxlocal, minlocal, maxglobal, minglobal
 integer :: i,j,k,err1
 

 minlocal=0.d0
 maxlocal=0.d0
 call avgrmsX1X2(Var,mean,rms,'sf')
 do k=sz-1,ez+1
  if (k.EQ.nzp2) minlocal=mean(k)-rms(k)*sqrt(2.0)
 enddo
 
#ifdef PARALLEL
 call MPI_ALLREDUCE(minlocal,minglobal,1,realtype,MPI_MAX,comm3d,err1)
#endif
 minvar=minglobal
 maxvar=maxvar*1.d0

 do k=nzp2-2,nzp2
  do j=sy,ey
   do i=sx,ex
    if (Var(i,j,k).LT.minglobal) Var(i,j,k) = minglobal
   enddo
  enddo
 enddo
 call ghost(Var,'scal1',err1)

 stat=err1
 return
end subroutine updateMinMax
