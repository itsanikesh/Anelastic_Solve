












subroutine concat(line1,line2)
!@t
! \textbf{subroutine concat(line1,line2)}
!@h
!   Description:
!    This routine concatenates two strings into the first string.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Fortran 90. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
 implicit none

!Passed Variables
 character(len=*),intent(inout) :: line1
 character(len=*),intent(in)    :: line2

!Local Variables
 integer          :: lbeg1, lend1, lbeg2, lend2

!Strip Internal Blanks and Left Pack Strings
 call clips(line1, lbeg1, lend1)
 call clips(line2, lbeg2, lend2)

!Concatenate Strings 
 line1(lend1+1:lend1+1+lend2-lbeg2) = line2(lbeg2:lend2)

 return
end subroutine concat

subroutine concati(line1,i)
!@t
! \textbf{subroutine concati(line1,i)}
!@h
!   Description:
!    This routine concatenates a string and an integer.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Fortran 90. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
!@q
 implicit none
      
!Passed Variables 
 character(len=*),intent(inout)  :: line1
 integer,intent(in)              :: i

!Local Variables
 integer           :: lbeg1, lend1, lbeg2, lend2
 character(len=18) :: line2

!write i to string
 line2 = '                '
 write(line2, '(i7)') i

!Strip Internal Blanks and Left Pack Strings
 call clips(line1, lbeg1, lend1)
 call clips(line2, lbeg2, lend2)

!Concatenate Strings
 line1(lend1+1:lend1+1+lend2-lbeg2) = line2(lbeg2:lend2)

 return
end subroutine concati

subroutine concatn(line1, line2)
!@t
! \textbf{subroutine concatn(line1,line2)}
!@h
!   Description:
!    This routine concatenates two strings into the second string.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Fortran 90. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
 implicit none
      
!Passed Variables
 character(len=*),intent(in)           :: line1
 character(len=*),intent(inout)        :: line2

!Local Variables
 integer                               :: lbeg1, lend1, lbeg2, lend2
 integer                               :: slen
 character(len=1000)                   :: tempstring
 
!Strip Internal Blanks and Left Pack Strings
 call clips(line1, lbeg1, lend1)
 call clips(line2, lbeg2, lend2)
 call slength(line1,slen)
 tempstring(lbeg2:lend2) = line2(lbeg2:lend2)

 line2 = line1
!Concatenate Strings
 line2(slen+1:slen+1+lend2-lbeg2) = tempstring(lbeg2:lend2)
 return
end subroutine concatn

subroutine concatr(line1,f)
!@t
! \textbf{subroutine concat(line1,line2)}
!@h
!   Description:
!    This routine a string and real/double precision variable.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Fortran 90. [Kyle A. Brucker] 

!   Language:
!     Fortran 90
 implicit none

!Passed Variables
 character(len=*),intent(inout)  :: line1
 real(8),intent(in)              :: f

!Local Variables
 integer           :: lbeg1, lend1, lbeg2, lend2
 character(len=18) :: line2
 write(6,*) f

!Write real to string
 line2 = '                '
 write(line2, '(f10.4)') f
 write(6,*) line2
 write(6,*) f

!Strip Internal Blanks and Left Pack Strings
 call clips(line1, lbeg1, lend1)
 call clips(line2, lbeg2, lend2)

!Concatenate Strings
 line1(lend1+1:lend1+1+lend2-lbeg2) = line2(lbeg2:lend2)

 return 
end subroutine concatr

subroutine clips(line, l1, l2)
!@t
! \textbf{subroutine clips(line, l1, l2)}
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
 call lead(line,l1)
 call trail(line,l2)

 return
end subroutine clips

subroutine lead(line,lenc)
!@t
! \textbf{subroutine lead(line,lenc)}
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
end subroutine lead

subroutine trail(line,lenc)
!@t
! \textbf{subroutine trail(line,lenc)}
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
end subroutine trail

subroutine slength(line, l1)
!@t
! \textbf{subroutine slength(line1,l1)}
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
!     1.0       07/2008  Fortran 90. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

!Passed Variables
 !line - Character string to search for nonblank characters.
 !l1 - length of string

 implicit none

!Passed variables
 integer,intent(out)         :: l1
 character(len=*),intent(in) :: line

!Local variables
 integer                     :: l2, l3
!Call LEAD and TRAIL to locate Nonblank leading and trailing characters 
 call lead(line,l2)
 call trail(line,l3)
 l1 = l3-l2+1
 return
end subroutine slength

subroutine fullstring(line1,line2)
!subroutine fullstring(line1,line2, l1)
!@t
! \textbf{subroutine fullstring(line1,line2, l1)}
!@h
!   Description:
!    This subroutine uses the routines LEAD, TRAIL and SLENGTH to locate
!    the first and last nonblank characters in a general length character
!    string, and then copies only the characters between the first and 
!    last into a full string (line2).
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Fortran 90. [Kyle A. Brucker] 

!   Language:
!     Fortran 90

!Passed Variables
 !line1 - Input Character string to search for nonblank characters.
 !line2 - Output Character string
 !l1    - length of OUTPUT string
 implicit none

!Passed Variables
 character(len=*),intent(in)  :: line1
 character(len=*),intent(out) :: line2
! integer,intent(in)           :: l1

!Local variables
 integer          :: j, j2, b1, b2
                                                                                                                             
 call lead(line1,b1)
 call trail(line1,b2)

 do j=b1,b2
  j2 = j - b1+1
  write(line2(j2:j2),'(a1)') line1(j:j)
 enddo

 return
end subroutine fullstring
