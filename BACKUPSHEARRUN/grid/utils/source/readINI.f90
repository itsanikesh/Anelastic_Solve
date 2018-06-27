

subroutine readini(fname,block,vname,value,defval,IOUT)
!@t
! \textbf{subroutine readini(fname,block,vname,value,defval,IOUT)}
!@h
!   Description:
!     This subroutine reads a .ini file
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
  
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     0.0       xx/xxxx  Original code. [Carlos Pantano]
!     1.0       07/2008  Fortran 90.    [Kyle A. Brucker] 
   
!   Language:
!     Fortran 90

!Passed Variables
 !fname  - ini file name 
 !block  - Block
 !vname  - Variable 
 !value  - Returned value
 !defval - Default value returned if vname is not found in ini file
                                                                                                                            
 implicit none

!Passed Variables
 character(*),intent(in)  :: fname
 character(*),intent(in)  :: block
 character(*),intent(in)  :: vname
 character(*),intent(out) :: value
 character(*),intent(in)  :: defval
 integer,intent(in)       :: IOUT

!Local Variables
 character(len=512)       :: line
 character(len=256)       :: tmp
 character                :: first
 integer                  :: equal, l1, l2
 integer                  :: s1

 value = defval

!*****************
!* Open ini File *
!*****************
 open(unit=45,file=fname,status='old',action='read',iostat=s1)
  if (s1.NE.0) then
   write(IOUT,'(a23,a,i5)') "ERROR OPENING ini FILE: ",trim(fname)," IOSTAT=",s1
   goto 51
  endif
 20   read(45,'(a512)',end=50) line
      first = line(1:1)
      if ( first .eq. '[' ) then
         tmp = '['//trim(block)//']'
         call clipsL(line, l1, l2)
         if ( line(l1:l2).eq.tmp ) then
 30         read(45,'(a512)',end=50) line
            first = line(1:1)
            if ( first .eq. '[' ) then
               goto 20
            endif
            if ( first .eq. '#' ) goto 30
            equal = index(line,'=')
            call clipsL(line(1:equal-1),l1, l2)
            if ( line(l1:l2).eq.trim(vname) ) then
               call clipsL(line(equal+1:len(line)),l1,l2)
               value = line(equal+l1:equal+l2)
               goto 50
            else
               goto 30
            endif
         endif
      endif
      goto 20

 50   close(unit=45)
 51   continue

 return
end subroutine readini

subroutine scaninichar(fname, block, vname, value, defval, IOUT)
!@t
! \textbf{subroutine scaninichar(fname, block, vname, value, defval, IOUT)}
!@h
!   Description:
!     Scan a .ini file for a character value.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
  
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     0.0       xx/xxxx  Original code. [Carlos Pantano]
!     1.0       07/2008  Fortran 90.    [Kyle A. Brucker] 
   
!   Language:
!     Fortran 90

!Passed Variables
 !fname  - ini file name 
 !block  - Block
 !vname  - Variable 
 !value  - Returned CHARACTER 
 !defval - Default value returned if vname is not found in ini file
 !IOUT   - OUTPUT UNIT# 
                                                                                                                           
 use ntypes, only: i4, r8



 implicit none

!Passed Variables
 character(len=*),intent(in)    :: fname
 character(len=*),intent(in)    :: block
 character(len=*),intent(in)    :: vname
 character(len=*),intent(in)    :: defval
 character(len=*),intent(out)   :: value
 integer(i4),intent(in)         :: IOUT 

!Local Variables
 character(len=64)              :: pszvalue, pszout 
 integer(i4)                    :: l1,l2, err1





 call readini(fname, block, vname, pszvalue, defval,IOUT)

 !CHECK THAT VARIABLE IS IN INPUT FILE
 call clipsL(pszvalue,l1,l2)
 if (pszvalue(l1:l2).EQ.defval) then
   write(IOUT,'(a8,a,a26,a)') "ERROR: ", trim(vname), " NOT FOUND IN INPUT FILE: ",trim(fname)
   stop
 endif

 !TRIM STRING
 value=trim(pszvalue)

 !OUTPUT TO SCREEN
 pszout=vname//'='//pszvalue 
 write(IOUT,'(a)') trim(pszout)







 return
end subroutine scaninichar

subroutine scaniniint(fname, block, vname, value, defval, IOUT)
!@t
! \textbf{subroutine scaniniint(fname, block, vname, value, defval, IOUT)}
!@h
!   Description:
!     Scan a .ini file for a integer value.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
  
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     0.0       xx/xxxx  Original code. [Carlos Pantano]
!     1.0       07/2008  Fortran 90.    [Kyle A. Brucker] 
   
!   Language:
!     Fortran 90

!Passed Variables
 !fname  - ini file name 
 !block  - Block
 !vname  - Variable 
 !value  - Returned INTEGER 
 !defval - Default value returned if vname is not found in ini file
 !IOUT   - OUTPUT UNIT# 
                                                                                                                           
 use ntypes, only: i4



 implicit none

!Passed Variables
 character(*),intent(in)  :: fname
 character(*),intent(in)  :: block
 character(*),intent(in)  :: vname
 character(*),intent(in)  :: defval
 integer(i4),intent(out)  :: value
 integer(i4),intent(in)   :: IOUT 

!Local Variables
 character(len=64)        :: pszvalue, pszout 
 integer(i4)              :: l1,l2, err





  call readini(fname, block, vname, pszvalue, defval,IOUT)

  !CHECK THAT VARIABLE IS IN INPUT FILE
  call clipsL(pszvalue,l1,l2)
  if (pszvalue(l1:l2).EQ.defval) then
    write(IOUT,'(a8,a,a26,a)') "ERROR: ", trim(vname), " NOT FOUND IN INPUT FILE: ",trim(fname)
    stop
  endif
 
  !READ INTEGER FROM STRING
  read(pszvalue, *) value

  !OUTPUT TO SCREEN
  pszout=vname//'='//pszvalue 
  write(IOUT,'(a)') trim(pszout)






 return
end subroutine scaniniint

subroutine scaninilogical(fname, block, vname, value, defval, IOUT)
!@t
! \textbf{subroutine scaninilogical(fname, block, vname, value, defval, IOUT)}
!@h
!   Description:
!     Scan a .ini file for a logical value.
!@q 
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)
  
!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     0.0       xx/xxxx  Original code. [Carlos Pantano]
!     1.0       07/2008  Fortran 90.    [Kyle A. Brucker] 
   
!   Language:
!     Fortran 90

!Passed Variables
 !fname  - ini file name 
 !block  - Block
 !vname  - Variable 
 !value  - Returned LOGICAL 
 !defval - Default value returned if vname is not found in ini file
 !IOUT   - OUTPUT UNIT# 
                                                                                                                           
 use ntypes, only: i4, r8



 implicit none

!Passed Variables
 character(*),intent(in)  :: fname
 character(*),intent(in)  :: block
 character(*),intent(in)  :: vname
 character(*),intent(in)  :: defval
 logical,intent(out)      :: value
 integer(i4),intent(in)   :: IOUT 

!Local Variables
 character(len=64)        :: pszvalue, pszout 
 integer(i4)              :: l1,l2,err





 call readini(fname, block, vname, pszvalue, defval,IOUT)

 !CHECK THAT VARIABLE IS IN INPUT FILE
 call clipsL(pszvalue,l1,l2)
 if (pszvalue(l1:l2).EQ.defval) then
   write(IOUT,'(a8,a,a26,a)') "ERROR: ", trim(vname), " NOT FOUND IN INPUT FILE: ",trim(fname)
   stop
 endif

 !SET LOGICAL VALUE BASED ON YES OR NO 
 if (pszvalue(l1:l2).EQ."yes") then
  value=.true.
 elseif (pszvalue(l1:l2).EQ."no") then 
  value=.false. 
 else
  write(IOUT,'(a8,a,a22,a)') "ERROR: ", trim(vname), " MUST BE yes/no READ: ",pszvalue
  stop
 endif

 !OUTPUT TO SCREEN
 pszout=vname//'='//pszvalue 
 write(IOUT,'(a)') trim(pszout)





 
 return
end subroutine scaninilogical

subroutine scaninimint(fname, block, vname, value, nvar, defval, IOUT)
!@t
! \textbf{subroutine scaninimint(fname, block, vname, value, defval, IOUT)}
!@h
!   Description:
!     Scan a .ini file for a multiple integer values.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     0.0       xx/xxxx  Original code. [Carlos Pantano]
!     1.0       07/2008  Fortran 90.    [Kyle A. Brucker] 

!   Language:
!     Fortran 90
!@h
!   Comments:
!     Valid delimiters   , : ;
!@q

!Passed Variables
 !fname  - ini file name 
 !block  - Block
 !vname  - Variable 
 !value  - Returned INTEGER 
 !nvar   - Number of returned values
 !defval - Default value returned if vname is not found in ini file
 !IOUT   - OUTPUT UNIT# 
                                                                                                                           
 use ntypes, only: i4



 implicit none

!Passed Variables
 character(*),intent(in)   :: fname
 character(*),intent(in)   :: block
 character(*),intent(in)   :: vname
 character(*),intent(in)   :: defval
 integer(i4),intent(inout) :: nvar
 integer(i4),intent(out)   :: value(nvar)
 integer(i4),intent(in)    :: IOUT 

!Local Variables
 character(len=512)        :: pszvalue, pszout 
 character(len=32)         :: pszlocvalue 
 integer(i4)               :: l, l1, l2, lloc, n, stepmode, i, step, i3
 integer(i4)               :: nvarIN,err


 nvarIN=nvar
 i3 = 3





 call readini(fname, block, vname, pszvalue, defval,IOUT)

 !CHECK THAT VARIABLE IS IN INPUT FILE
 call clipsL(pszvalue,l1,l2)
 if (pszvalue(l1:l2).EQ.defval) then
   write(IOUT,'(a8,a,a26,a)') "ERROR: ", trim(vname), " NOT FOUND IN INPUT FILE: ",trim(fname)
   stop
 endif

 if (pszvalue(1:5).EQ.'     ')  then
  n=0   !IF THERE IS NOTHING RETURN ZERO
  goto 1000
 endif

 pszlocvalue = '                                '
 n = 0
 lloc = 0
 stepmode = 0
 call clipsL(pszvalue,l1,l2)
 do l = l1, l2
  if (pszvalue(l:l) .eq. ',') then
   n = n + 1
   lloc = 0
   if (nvarIN.NE.-1) read(pszlocvalue,*) value(n)
   pszlocvalue = '                                '
  else if (pszvalue(l:l) .eq. ':') then
   n = n + 1
   lloc = 0
   if (nvarIN.NE.-1) read(pszlocvalue,*) value(n)
   pszlocvalue = '                                '
   stepmode = 1
  else
   lloc = lloc + 1
   pszlocvalue(lloc:lloc) = pszvalue(l:l)
  endif
 enddo
 n=n+1
 if (nvarIN.NE.-1) read(pszlocvalue,*) value(n)

!Step mode version
 !Check vector does not have more than 3 components [i:j:k]
 if ( stepmode .eq. 1 ) then
  if (n .gt. i3 ) then
  write(IOUT,'(a8,a,a60)') "ERROR: ", trim(vname), " MUST HAVE ONLY 3&
                           & COMPONENTS (x:y:z) USE DIFFERENT DELIMITER"
  endif
  n = (value(3)-value(1))/value(2) + 1
  step = value(2)
  do i = 2,n
   value(i) = value(1) + (i-1)*step
  enddo
 endif

 if (n.GT.nvar.and.nvarIN.NE.-1) then
  write(IOUT,'(a8,a,a23,i3,a7,i3)') "ERROR: ", trim(vname), " LIST IS TOO LONG. n=",n,"nvar=",nvar
  stop
 else
   nvar = n
 endif

 if (nvarIN.NE.-1) then
  !OUTPUT TO SCREEN
   pszout=trim(vname)//'='//pszvalue 
   write(IOUT,'(a)') trim(pszout)
 endif
 1000 continue


 return
end subroutine scaninimint

subroutine scaninimreal(fname, block, vname, value, nvar, defval, IOUT)
!@t
! \textbf{subroutine scaninimreal(fname, block, vname, value, defval, IOUT)}
!@h
!   Description:
!     Scan a .ini file for multiple real values.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     0.0       xx/xxxx  Original code. [Carlos Pantano]
!     1.0       07/2008  Fortran 90.    [Kyle A. Brucker] 

!   Language:
!     Fortran 90
!@h
!   Comments:
!     Valid delimiters   , : ;
!@q

!Passed Variables
 !fname  - ini file name 
 !block  - Block
 !vname  - Variable 
 !value  - Returned REAL 
 !nvar   - Number of returned values
 !defval - Default value returned if vname is not found in ini file
 !IOUT   - OUTPUT UNIT# 
                                                                                                                           
 use ntypes, only: r8,i4
 implicit none

!Passed Variables
 character(*),intent(in)   :: fname
 character(*),intent(in)   :: block
 character(*),intent(in)   :: vname
 character(*),intent(in)   :: defval
 integer(i4),intent(inout) :: nvar
 real(r8),intent(out)      :: value(nvar)
 integer(i4),intent(in)    :: IOUT 

!Local Variables
 character(len=512)        :: pszvalue, pszout 
 character(len=32)         :: pszlocvalue 
 integer(i4)               :: l, l1, l2, lloc, n, stepmode, i, step, i3
 integer(i4)               :: nvarIN,err

 nvarIN=nvar

 !nvar=-1 FOR INQUIRING ABOUT LENGTH OF LIST DOES NOT OUTPUT TO SCREEN
 i3 = 3

 call readini(fname, block, vname, pszvalue, defval,IOUT)

 !CHECK THAT VARIABLE IS IN INPUT FILE
 call clipsL(pszvalue,l1,l2)
 if (pszvalue(l1:l2).EQ.defval) then
   write(IOUT,'(a8,a,a26,a)') "ERROR: ", trim(vname), " NOT FOUND IN INPUT FILE: ",trim(fname)
   stop
 endif

 if (pszvalue(1:5).EQ.'     ')  then
  n=0   !IF THERE IS NOTHING RETURN ZERO
  goto 1000
 endif

 pszlocvalue = '                                '
 n = 0
 lloc = 0
 stepmode = 0
 call clipsL(pszvalue,l1,l2)
 do l = l1, l2
  if (pszvalue(l:l) .eq. ',') then
   n = n + 1
   lloc = 0
   if (nvarin.NE.-1) read(pszlocvalue,*) value(n)
   pszlocvalue = '                                '
  else if (pszvalue(l:l) .eq. ':') then
   n = n + 1
   lloc = 0
   if (nvarin.NE.-1) read(pszlocvalue,*) value(n)
   pszlocvalue = '                                '
   stepmode = 1
  else
   lloc = lloc + 1
   pszlocvalue(lloc:lloc) = pszvalue(l:l)
  endif
 enddo
 n = n + 1
 if (nvarIN.NE.-1) read(pszlocvalue,*) value(n)

!Step mode version
 !Check vector does not have more than 3 components [i:j:k]
 if ( stepmode .eq. 1 ) then
  if (n .gt. i3 ) then
  write(IOUT,'(a8,a,a60)') "ERROR: ", trim(vname), " MUST HAVE ONLY 3&
                           & COMPONENTS (x:y:z) USE DIFFERENT DELIMITER"
  endif
  n = (value(3)-value(1))/value(2) + 1
  step = value(2)
  do i = 2,n
   value(i) = value(1) + (i-1)*step
  enddo
 endif

 if (n.GT.nvar.and.nvarIN.NE.-1) then
  write(IOUT,'(a8,a,a23,i3,a7,i3)') "ERROR: ", trim(vname), " LIST IS TOO LONG. n=",n,"nvar=",nvar
 else
   nvar = n
 endif

 if (nvarIN.NE.-1) then
  !OUTPUT TO SCREEN
   pszout=trim(vname)//'='//pszvalue 
   write(IOUT,'(a)') trim(pszout)
 endif

 1000 continue
  

 return
end subroutine scaninimreal

subroutine scaninireal(fname, block, vname, value, defval, IOUT)
!@t
! \textbf{subroutine scaninireal(fname, block, vname, value, defval, IOUT)}
!@h
!   Description:
!     Scan a .ini file for a real value.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     0.0       xx/xxxx  Original code. [Carlos Pantano]
!     1.0       07/2008  Fortran 90.    [Kyle A. Brucker] 

!   Language:
!     Fortran 90

!Passed Variables
 !fname  - ini file name 
 !block  - Block
 !vname  - Variable 
 !value  - Returned REAL
 !defval - Default value returned if vname is not found in ini file
 !IOUT   - OUTPUT UNIT# 
                                                                                                                           
 use ntypes, only: i4, r8
 implicit none

!Passed Variables
 character(*),intent(in)  :: fname
 character(*),intent(in)  :: block
 character(*),intent(in)  :: vname
 character(*),intent(in)  :: defval
 real(r8),intent(out)     :: value
 integer(i4),intent(in)   :: IOUT 

!Local Variables
 character(len=64)        :: pszvalue, pszout 
 integer(i4)              :: l1,l2,err


 call readini(fname, block, vname, pszvalue, defval,IOUT)

 !CHECK THAT VARIABLE IS IN INPUT FILE
 call clipsL(pszvalue,l1,l2)
 if (pszvalue(l1:l2).EQ.defval) then
   write(IOUT,'(a8,a,a26,a)') "ERROR: ", trim(vname), " NOT FOUND IN INPUT FILE: ",trim(fname)
   stop
 endif

 !READ REAL FROM STRING
 read(pszvalue, *) value

 !OUTPUT TO SCREEN
 pszout=vname//'='//pszvalue 
 write(IOUT,'(a)') trim(pszout)


 return
end subroutine scaninireal

subroutine clipsL(line, l1, l2)
!@t
! \textbf{subroutine clipsL(line, l1, l2)}
!@h
!   Description:
!    This subroutine uses the routines LEAD and TRAIL to locate the first
!    and last nonblank characters in a general length character string.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     0.0       07/1995  Cleaned Original code. [Scott Stanley]
!     1.0       06/2007  Fortran 90.            [Kyle A. Brucker] 

!   Language:
!     Fortran 90

!Passed Variables
 !line - Character string to search for nonblank characters.
 !l1 - Character number of the first nonblank leading character.
 !l2 - Character number of the first nonblank trailing character.

 implicit none
 
!Passed variables
 integer,intent(inout)       :: l1, l2
 character(len=*),intent(in) :: line

!Call LEAD and TRAIL to locate Nonblank leading and trailing characters 
 call leadL(line,l1)
 call trailL(line,l2)

 return
end subroutine clipsL



subroutine leadL(line,lenc)
!@t
! \textbf{subroutine leadL(line,lenc)}
!@h
!   Description:
!    This subroutine locates the first nonblank character in a general
!    length character string.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     0.0       07/1995  Cleaned Original code. [Scott Stanley]
!     1.0       06/2007  Fortran 90.            [Kyle A. Brucker] 

!   Language:
!     Fortran 90

!Passed Variables
 !line - String to search for first nonblank character.
 !lenc - Character number of first nonblank character.

 implicit none

!Passed variables
 integer,intent(out)         :: lenc
 character(len=*),intent(in) :: line

!Local variables
 integer                     :: lgth,i

!Locate First Nonblank Character
 lgth = len(line)
 lenc = 1

 do i = 1,lgth
  if (line(i:i).ne.' ') then
   lenc = i
   goto 20
  endif
 enddo

 20 continue
 return
end subroutine leadL


subroutine trailL(line,lenc)
!@t
! \textbf{subroutine trailL(line,lenc)}
!@h
!   Description:
!    This subroutine locates the last nonblank character in a general
!    length character string.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     0.0       07/1995  Cleaned Original code. [Scott Stanley]
!     1.0       06/2007  Fortran 90.            [Kyle A. Brucker] 

!   Language:
!     Fortran 90

!Passed Variables
 !line - String to search for the last nonblank character.
 !lenc - Character number of the last nonblank character in line.

 implicit none

!Passed variables
 integer,intent(out)         :: lenc
 character(len=*),intent(in) :: line

!Local variables
 integer                     :: lgth,i

!Locate last Nonblank Character in Line

 lgth = len(line)
 do i = lgth,1,-1
  if (line(i:i).ne.' ') then
   lenc = i
   goto 20
  endif
 enddo

 20 continue
 return
end subroutine trailL







!**********************************************************************
!*************SAMPLE PROGRAM TO TEST read_ini_file.f90 ****************
!************* *****READS IN SAMPLE dns.ini BELOW *********************
!**********************************************************************
!program test
! use ntypes, only: i4, r8
! implicit none
! integer(i4) :: IOUT
! character(len=25) :: defval
!                                                                                                                             
! integer           :: itest, Mitest(4)
! real(r8)          :: rtest, Mrtest(3)
! logical           :: ltest, cltest
! character(len=25) :: ctest
                                                                                                                             
! IOUT = 6
! defval='ERROR'
                                                                                                                             
! call scaniniint('dns.ini','Tests','Integer',itest,defval,6)
! call scaninireal('dns.ini','Tests','Real',rtest,defval,6)
! call scaninimint('dns.ini','Tests','LInteger',Mitest,4,defval,6)
! call scaninimreal('dns.ini','Tests','LReal',Mrtest,3,defval,6)
! call scaninichar('dns.ini','Tests','Character',ctest,defval,6)
! call scaninilogical('dns.ini','Tests','Logical',cltest,defval,6)

! write(6,*) itest
! write(6,*) rtest
! write(6,*) Mitest
! write(6,*) Mrtest
! write(6,*) ctest
! write(6,*) cltest
!stop
!end program test


!**********************************************************************
!*******SAMPLE dns.ini FILE TO BE READ IN BY THE ABOVE PROGRAM*********
!**********************************************************************
![Tests]
!Integer=99
!Real=0.00009d0
!Character=this is a test
!Logical=yes
!LInteger=1,2,3,4
!LReal=.1,.2,.3
