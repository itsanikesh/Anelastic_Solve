program read_old_tk

 implicit none

 character(len=500)                   :: infile,outfile,stemp
 character(len=1)                     :: cdum
 integer                              :: ny,nz,j,k,s1,idum,iter,offset,i,ii,imin,imax,iskip
 integer,parameter                    :: ngroups=1,nstats=17
 real(8),allocatable,dimension(:)     :: yp,zp
 real(4),allocatable,dimension(:,:,:) :: STATS
 real(8),allocatable,dimension(:,:,:) :: STAT

 real(8)                              :: time,delt,Re,Pr,g,rho_0

 character(len=25) :: Sname(1:nstats), Gname(1:ngroups)
 integer           :: group(1:nstats)

 !Set Parameters
 Re=10000.d0
 Pr=1.d0
 g=9.81d0
 rho_0=1.d0
 delt=1.d0

 imin=0
 imax=3000
 iskip=25
 ny=259
 nz=131

 allocate( yp(0:ny) )
 allocate( zp(0:nz) )
 allocate( STATS(0:ny,0:nz,1:nstats) )
 allocate( STAT(0:ny,0:nz,1:nstats-1) )


 do iter=imin,imax,iskip

 write(stemp,'(i10)') iter
  offset=1
  do i=1,10
   if (stemp(i:i).EQ.' ') offset=offset+1
  enddo
  stemp=stemp(offset:10)

  write(infile,'(a)') 'avg'//trim(stemp)
 write(6,*) "OPENING: ",trim(infile)

 open( unit=500,file=infile,status='unknown',form='formatted',iostat=s1 )
  read(500,*) time 
  do k = 0,nz
   do j = 0,ny
    read(500,*,err=1000,IOSTAT=s1) idum,idum,yp(j),zp(k),STAT(j,k,:)
   enddo
  enddo
 close(500)

 sname(1)= 'U1'
 sname(2)= 'U2' 
 sname(3)= 'U3'
! sname(4)= 'P'
 sname(4)= 'RHO'
 sname(5)= 'u1p'
 sname(6)= 'u2p'
 sname(7)= 'u3p'
! sname(9)= 'pp'
 sname(8)= 'rhop'
 sname(9)= 'OMG1'
 sname(10)= 'OMG2'
 sname(11)= 'OMG3'
 sname(12)= 'OMGMAG'
 sname(13)= 'omg1p'
 sname(14)= 'omg2p'
 sname(15)= 'omg3p'
 sname(16)= 'omgmagp'
 sname(17)= 'DISS'

 Gname(1) = 'stats' 
 group(:) = 1
 STATS(:,:,1:16)=real(STAT(:,:,:))
 STATS(:,:,17) = -1.d0/Re*(STATS(:,:,15)**2+STATS(:,:,14)**2+STATS(:,:,13)**2)
 write(outfile,'(a,i5.5)') 'WakeStatsS_',iter
  open(unit=500,file=outfile,status='unknown',form='unformatted',iostat=s1)
   write(500) iter
   write(500) time,delt,Re,Pr,g,rho_0
   write(500) ny+1,nz+1
   write(500) yp
   write(500) yp
   write(500) zp
   write(500) zp
   write(500) nstats, ngroups
   write(500) Sname
   write(500) Gname
   write(500) group
   write(500) STATS
  close(unit=500)
 enddo


 deallocate( STATS,yp,zp )
  stop

  1000 continue
  write(6,*) "ERROR",s1 
stop
end program read_old_tk


