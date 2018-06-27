program calc_dkdt
 use VSBIN
 implicit none

 integer,parameter :: nP=2  !number of stats to add

 integer           :: stepM=10,stepP=30,step=20,nstep
 character(len=50) :: statDIR='./', basename='./G'
 logical           :: list=.false.
 integer           :: Ztrans=1 !Divide x_3 coordinate by  0-->1.0  1-->delta_theta  2-->delta_omega 

 real(r8),allocatable,dimension(:)            :: tke, tkem, tkep, dkdt

 real(r8),allocatable,dimension(:,:)          :: StatsNEW
 character(len=25),allocatable,dimension(:)   :: GnameNew, SnameNew
 integer,allocatable,dimension(:)             :: groupNEW 
 real(r8)                                     :: timeM, timeP
 integer                                      :: k,n, s1

 !COMPUTE TKE AT -1
 nstep=stepM
 call Vshear_statBIN_read(nstep,statDIR,list)
 allocate( tkem(1:nzp2), stat=s1)
  if (s1.NE.0) write(6,*) "ERROR ALLOCATING"
 do k=1,nzp2
  tkem(k) = 0.5d0*( STATS(k,6)**2+STATS(k,7)**2+STATS(k,8)**2 )
 enddo
 timeM=time
 call VSBINend

 !COMPUTE TKE AT +1
 nstep=stepP
 call Vshear_statBIN_read(nstep,statDIR,list)
 allocate( tkep(1:nzp2),stat=s1)
  if (s1.NE.0) write(6,*) "ERROR ALLOCATING"
 do k=1,nzp2
  tkep(k) = 0.5d0*( STATS(k,6)**2+STATS(k,7)**2+STATS(k,8)**2 )
 enddo
 timeP=time
 call VSBINend

 !COMPUTE TKE
 nstep=step
 call Vshear_statBIN_read(nstep,statDIR,list)
 allocate( tke(1:nzp2),stat=s1)
  if (s1.NE.0) write(6,*) "ERROR ALLOCATING"
 do k=1,nzp2
  tke(k) = 0.5d0*( STATS(k,6)**2+STATS(k,7)**2+STATS(k,8)**2 )
 enddo

 allocate( dkdt(1:nzp2), stat=s1)
  if (s1.NE.0) write(6,*) "ERROR ALLOCATING"
 !COMPUTE dkdt
 do k=1,nzp2
  dkdt(k) = ( tkeP(k)-tkeM(k) ) / (timeP - timeM)
 enddo

 allocate( StatsNEW(1:nzp2,1:nstats+nP),stat=s1)
 allocate( SnameNEW(1:nstats+nP),stat=s1 )
 allocate( groupNEW(1:nstats+nP),stat=s1 )
 if (s1.NE.0) write(6,*) "ERROR ALLOCATING"

 do n=1,nstats
  StatsNEW(:,n) = STATS(:,n)
  SnameNEW(n)   = Sname(n)
  groupNEW(n)   = group(n)
 enddo

 StatsNEW(:,nstats+1) = tke
 SnameNEW(nstats+1)   ='tke'
 groupNEW(nstats+1)   = 5 
 StatsNEW(:,nstats+2) = dkdt
 SnameNEW(nstats+2)   ='dkdt'
 groupNEW(nstats+2)   = 5 


 nstats=nstats+nP
 deallocate(STATS,stat=s1)
 deallocate(Sname,stat=s1)
 deallocate(group,stat=s1)
 if (s1.NE.0) write(6,*) "ERROR DEALLOCATING"
 allocate( STATS(1:nzp2,1:nstats),stat=s1)
 allocate( Sname(1:nstats),stat=s1 )
 allocate( group(1:nstats),stat=s1 )
 if (s1.NE.0) write(6,*) "ERROR ALLOCATING 2"

 STATS=STATSNEW
 Sname=SnameNEW
 group=groupNEW

 call TKwrite(basename,nstep,Ztrans) 

 deallocate(STATSNEW,stat=s1)
 deallocate(SnameNEW,stat=s1)
 deallocate(groupNEW,stat=s1)
 deallocate(tke,stat=s1)
 if (s1.NE.0) write(6,*) "ERROR DEALLOCATING 2"

 call VSBINend

stop
end program calc_dkdt
