












! serial version
subroutine write_pencil(var,dir,index1,index2,myidM,varname,verbose,stat)
!@t
! \textbf{subroutine write\_pencil(var,dir,index1,index2,myidM,varname,verbose,stat)}
!@h
!   Description:
!     Write a pencil using the serial version. 
!@q  
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       04/2009  Original code. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

 use ntypes, only: r8
 use Domain, only: sx,ex,sy,ey,sz,ez, nxp2, nyp2, nzp2
 use IO,     only: IOUT,penDIR
 use Parameters, only: Re, Pr, g, rho_0, time, nstep, delt
 use grid,    only: xe,xc,ye,yc,ze,zc
 implicit none

!Passed Variables
 real(r8),intent(in)   :: var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(in)    :: dir,index1,index2
 integer,intent(out)   :: stat
 character(len=*)      :: varname
 logical,intent(in)    :: verbose
 integer,intent(in)    :: myidM ! THIS IS ONLY ADDED SO THAT THE SYNTAX IS THE
                                ! SAME FOR THE SERIAL AND PARALLEL VERSION OF
                                ! WRITE_PLANE, myidM does nothing
!Local Variables
 integer :: i,j,k, s1
 character(len=40) :: filename
 logical             :: cfile
 integer             :: iu, iv, iw

  iu=0
  iv=0
  iw=0
  select case(varname)
   case('u1')
    iu=1
   case('u2')
    iv=1
   case('u3')
    iw=1
  end select

  !Create File Name
  if (dir.EQ.1) write(filename,'(a,i4.4,a,i4.4,a,i6.6,a)') trim(penDIR)//"/"//trim(varname)//"_j",index1,& 
                                                   "_k",index2,"_n",nstep,".pen"
  if (dir.EQ.2) write(filename,'(a,i4.4,a,i4.4,a,i6.6,a)') trim(penDIR)//"/"//trim(varname)//"_i",index1,& 
                                                   "_k",index2,"_n",nstep,".pen"
  if (dir.EQ.3) write(filename,'(a,i4.4,a,i4.4,a,i6.6,a)') trim(penDIR)//"/"//trim(varname)//"_i",index1,& 
                                                   "_j",index2,"_n",nstep,".pen"

  open(210,file=filename,form='formatted',status='unknown',iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a,a,a,i4)') "ERROR Opening File: ",filename," with iostat: ",s1
   stat=3
   goto 1000
  endif

 if (dir.EQ.1) then
  !X1
  write(210,'(a13,a5,a16,i6,e12.5)') "#X1-PROFILE OF",varname,"at nstep/time=",nstep,time
  write(210,'(a,i3,a,i3)') "#j=",index1," k=",index2
  write(210,'(i1)') iu
  do i=1,nxp2
    write(210,'(i5,4(1x,e22.12))') i,xe(i),xc(i),var(i,index1,index2)
  enddo
 elseif (dir.EQ.2) then
  !X2
  write(210,'(a13,a5,a16,i6,e12.5)') "#X2-PROFILE OF",varname,"at nstep/time=",nstep,time
  write(210,'(a3,i3,a3,i3)') "#i=",index1," k=",index2
  write(210,'(i1)') iv
  do j=1,nyp2
    write(210,'(i5,4(1x,e22.12))') j,ye(j),yc(j),var(index1,j,index2)
  enddo
 elseif (dir.EQ.3) then
  !X3
  write(210,'(a13,a5,a16,i6,e12.5)') "#X3-PROFILE OF",varname,"at nstep/time=",nstep,time
  write(210,'(a3,i3,a3,i3)') "#i=",index1," j=",index2
  write(210,'(i1)') iw
  do k=1,nzp2
   write(210,'(i5,4(1x,e22.12))') k,ze(k),zc(k),var(index1,index2,k)
  enddo
 else
  !This is an invalid direction 
   write(IOUT,'(a18,i1,a12)') "ILLEGAL DIRECTION ",dir," <> 1,2,3"
   goto 1000
 endif

 close(210)
 if (verbose) write(IOUT,'(a,f10.5)') "PENCIL: "//trim(filename)//" WRITTEN AT: ",time

 stat=s1
 return
 
 1000 continue
 inquire(unit=210,opened=cfile)
 if (cfile) then
  write(210) "ERROR ERROR"
  close(210)
 endif

 close(210)
 stat=-1
 return
end subroutine write_pencil

subroutine write_plane(var,dir,index1,myidM,prec,varname,verbose,stat)
!@t
! \textbf{subroutine write\_plane(var,dir,index1,myidM,prec,varname,verbose,stat)}
!@h
!   Description:
!     Write a plane using the serial version. 
!@q  
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
  
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       04/2009  Original code. [Kyle A. Brucker] 
   
!   Language:
!     Fortran 90

!   Comments:
!     Planes are stored in single precision to cut their storage down by a
!     factor of 2.

 use ntypes,     only: r8
 use Domain,     only: sx,ex,sy,ey,sz,ez, nxp2, nyp2, nzp2
 use IO,         only: IOUT,plnDIR
 use Parameters, only: Re, Pr, g, rho_0, time, nstep, delt
 use grid,       only: xc,xe,yc,ye,zc,ze
 implicit none

!Passed Variables
 real(r8),intent(in)   :: var(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
 integer,intent(in)    :: dir,index1,prec
 integer,intent(out)   :: stat
 character(len=*)      :: varname
 logical,intent(in)    :: verbose
 integer,intent(in)    :: myidM ! THIS IS ONLY ADDED SO THAT THE SYNTAX IS THE
                                ! SAME FOR THE SERIAL AND PARALLEL VERSION OF
                                ! WRITE_PLANE, myidM does nothing

!Local Variables
 integer :: s1
 character(len=300) :: filename
 real(4),allocatable,dimension(:,:) :: SP_plane
 logical             :: cfile
 integer             :: iu, iv, iw

 iu=0
 iv=0
 iw=0

select case(varname)
 case('u1')
 iu=1 
 case('u2')
 iv=1
 case('u3')
 iw=1
end select

  if (dir.EQ.1) write(filename,'(a,i4.4,a,i6.6,a)') trim(plnDIR)//"/"//trim(varname)//"_i",index1,"_n",nstep,".pln"
  if (dir.EQ.2) write(filename,'(a,i4.4,a,i6.6,a)') trim(plnDIR)//"/"//trim(varname)//"_j",index1,"_n",nstep,".pln"
  if (dir.EQ.3) write(filename,'(a,i4.4,a,i6.6,a)') trim(plnDIR)//"/"//trim(varname)//"_k",index1,"_n",nstep,".pln"

  open(210,file=filename,form='unformatted',status='unknown',iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a,a,a,i4)') "ERROR Opening File: ",filename," with iostat: ",s1
   stat=3
   goto 1000
  endif

 if (dir.EQ.1) then
  !X1
    write(210) nstep,time,delt,g,rho_0,Re,Pr
    write(210) dir,index1, iu, iv, iw
    write(210) xc(index1), xe(index1)
    write(210) nyp2, nzp2
    write(210) yc, ye
    write(210) zc, ze
    if (prec.EQ.0) then
     allocate( SP_plane(1:nyp2,1:nzp2),STAT=s1 )
     SP_plane=var(index1,:,:) !store plane in single precsion
     write(210)  SP_plane
     deallocate(SP_plane)
    else 
    write(210) var(index1,:,:)
    endif

 elseif (dir.EQ.2) then
  !X2
    write(210) nstep,time,delt,g,rho_0,Re,Pr
    write(210) dir,index1, iu, iv, iw
    write(210) yc(index1), ye(index1)
    write(210) nxp2, nzp2
    write(210) xc, xe
    write(210) zc, ze
    if (prec.EQ.0) then
     allocate( SP_plane(1:nxp2,1:nzp2),STAT=s1 )
     SP_plane=var(:,index1,:) !store plane in single precsion
     write(210)  SP_plane
     deallocate(SP_plane)
    else 
     write(210) var(:,index1,:)
    endif

 elseif (dir.EQ.3) then 
  !X3
    write(210) nstep,time,delt,g,rho_0,Re,Pr
    write(210) dir,index1, iu, iv, iw
    write(210) zc(index1), ze(index1)
    write(210) nxp2, nyp2
    write(210) xc, xe
    write(210) yc, ye
    if (prec.EQ.0) then
     allocate( SP_plane(1:nxp2,1:nyp2),STAT=s1 )
     SP_plane=var(:,:,index1) !store plane in single precsion
     write(210)  SP_plane
     deallocate(SP_plane)
    else 
     write(210) var(:,:,index1)
    endif
 else
  !This is an invalid direction 
   write(IOUT,'(a18,i1,a12)') "ILLEGAL DIRECTION ",dir,' <> 1,2,3'
   goto 1000
 endif

 close(210) 
 if (verbose) write(IOUT,'(a,f10.5)') "PLANE: "//trim(filename)//" WRITTEN AT: ",time

 stat=s1
 return
 
 1000 continue
  
 inquire(unit=210,opened=cfile)
 if (cfile) then
  write(210) "ERROR ERROR"
  close(210)
 endif
 stat=-1
 return
end subroutine write_plane


!*****************OLD WILL NEED TO BE UPDATED FOR STRUCTURE OF NEW PLANE FILES********************
!The matlab code to read in the plane of data written out above is
!%Open the data file float64 is double precision float32 is single
!x=fopen('filename','rb','b'); !ENDIANESS b=big? l=little? 

!%Read Block 1
!A = fread(x,1,'int'); %read arbitrary 4byte header
!ntime = fread(x,1,'int'); %ntime
!time = fread(x,1,'float64'); %time
!visc_nd = fread(x,1,'float64'); %non-dimensional viscosity
!prandtl = fread(x,1,'float64'); %Prandtl Number
!A       = fread(x,1,'int'); %read 4byte trailer

!%Read Block 2
!A       = fread(x,1,'int'); %read 4byte header
!b1      = fread(x,1,'int'); %y_min
!b2      = fread(x,1,'int'); %y_max
!b3      = fread(x,1,'int'); %z_min
!b4      = fread(x,1,'int'); %z_max
!A       = fread(x,1,'int'); %read trailer

!%Read Block 3
!A       = fread(x,1,'int'); %read header
!s1      = b2 - b1 + 1;
!s2      = b4 - b3 + 1;
!data    = fread(x,[s1 s2],'float64');
!A       = fread(x,1,'int'); %read trailer

!%Close data file
!fclose(x);

