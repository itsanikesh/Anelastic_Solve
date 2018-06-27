subroutine write_tec_binary(is1,is2,is3,var_to_write,DIsDouble)
!Used to write to tecplot binary file for writting.
 implicit none
                                                                                                                             
!Passed Variables
 integer,intent(in)                          :: is1, is2, is3
 integer,intent(in)                          :: DIsDouble
 real(8),intent(in),dimension( is1,is2,is3 ) :: var_to_write

!Local Variables
 integer                                     :: TecDat,IRES

  IRES   = TecDat(is1*is2*is3,var_to_write,DIsDouble)

 return
end subroutine write_tec_binary

subroutine open_tec_binary(stime,is1,is2,is3,line1,line2,line3,line4,Debug,VIsDouble)
 implicit none

!Passed Variables
 integer,intent(in)            :: is1, is2, is3 
 character(len=*),intent(in)   :: line1
 character(len=*),intent(in)   :: line2
 character(len=*),intent(in)   :: line3
 character(len=*),intent(in)   :: line4
 integer,intent(in)            :: Debug, VIsDouble
 real(8),intent(in)            :: stime

!Local Variables
 integer                       :: slen1, slen2, slen3, slen4

 call slength(line1,slen1)
 call slength(line2,slen2)
 call slength(line3,slen3)
 call slength(line4,slen4)
 call Otec_binary(stime,is1,is2,is3,line1,slen1,line2,slen2,line3,slen3,line4,slen4,Debug,VIsDouble)

 return
end subroutine


subroutine Otec_binary(stime,is1,is2,is3,line1,slen1,line2,slen2,line3,slen3,line4,slen4,Debug,VIsDouble)
!Used to open tecplot binary file for writting.
!set is3=1 for 2d plane
 implicit none

!Passed Variables
 integer,intent(in)                :: is1, is2, is3, slen1, slen2, slen3, slen4
 character(len=slen1),intent(in)   :: line1
 character(len=slen2),intent(in)   :: line2
 character(len=slen3),intent(in)   :: line3
 character(len=slen4),intent(in)   :: line4
 integer,intent(in)                :: Debug, VIsDouble
 real(8),intent(in)                :: stime

!Local Variables
 integer(4)                        :: tecini,teczne,IRES
 character,parameter               :: NULLCHR = CHAR(0)

 IRES = TecIni(line1//NULLCHR,line2//NULLCHR,line3//NULLCHR,line4//NULLCHR,Debug,VIsDouble)

 IRES = TecZne("Zone1"//NULLCHR, is1, is2, is3, "BLOCK"//NULLCHR, NULLCHR )
 return
end subroutine Otec_binary


subroutine close_tec_binary
!Used to open tecplot binary file for writting.

 implicit none

!Local Variables
 integer(4)                        :: TecEnd,IRES

  IRES   = TecEnd()

 return
end subroutine close_tec_binary

subroutine write_tec360_binary(is1,is2,var_to_write,DIsDouble)
!Used to write to tecplot binary file for writting.
 implicit none
                                                                                                                             
!Passed Variables
 integer,intent(in)                      :: is1, is2
 integer,intent(in)                      :: DIsDouble
 real(8),intent(in),dimension( is1,is2 ) :: var_to_write

!Local Variables
 integer(4)                              :: TecDat110,IRES

!  IRES   = TecDat110(is1*is2,var_to_write,DIsDouble)

 return
end subroutine write_tec360_binary


subroutine open_tec360_binary(stime,is1,is2,line1,slen1,line2,slen2,line3,slen3,line4,slen4,Debug,VIsDouble)
!**IMPORTANT**
!open_tec360_binary cannot be compiled with -check, only -check bounds
!the need to pass a null pointer to a c library causes the -check pointer part of -check
!to fail since an unassigned pointer is referenced


!Used to open tecplot binary file for writting.

 implicit none

!Passed Variables
 integer,intent(in)                :: is1, is2, slen1, slen2, slen3, slen4
 character(len=slen1),intent(in)   :: line1
 character(len=slen2),intent(in)   :: line2
 character(len=slen3),intent(in)   :: line3
 character(len=slen4),intent(in)   :: line4
 integer,intent(in)                :: Debug, VIsDouble
 real(8),intent(in)                :: stime

!Local Variables
 integer(4)                        :: tecini110,teczne110,IRES
 character,parameter               :: NULLCHR = CHAR(0)

!Null pointer assignment
 INTEGER(4),pointer,dimension(:)   :: Pt
 Pt => null()

! IRES=TecIni110(line1//NULLCHR,line2//NULLCHR,line3//NULLCHR,line4//NULLCHR,Debug,VIsDouble)
                                                                                                                             
! IRES = TECZNE110('Zone 1'//NULLCHR,&
!                    0,&     !ZONETYPE 0
!                    is1,&   !imax
!                    is2,&   !jmax
!                    1,&     !kmax
!                    0,&     !icellmax reserved for future use set to zero
!                    0,&     !jcellmax reserved for future use set to zero
!                    0,&     !kcellmax reserved for future use set to zero
!                    stime,& !Solution Time
!                    1,&     !StrandID
!                    0,&     !ParentZn
!                    1,&     !ISBLOCK
!                    0,&     !NumFaceConnections
!                    0,&     !FaceNeighborMode
!                    Pt,&    !PassiveVarList
!                    Pt,&    !ValueLocation
!                    Pt,&    !ShareVarFromZone
!                    0)      !ShareConnectivityFromZone)
 return
end subroutine open_tec360_binary

subroutine close_tec360_binary
!Used to open tecplot binary file for writting.

 implicit none

!Local Variables
 integer(4)                        :: TecEnd110,IRES

!  IRES   = TecEnd110()

 return
end subroutine close_tec360_binary

subroutine slength(line, l1)
!This subroutine uses the routines LEAD and TRAIL to
!locate the first and last nonblank characters in a
!general length character string.

!Passed Variables
 !line - Character string to search for nonblank characters.
 !l1 - length of string

!Modification History
!------------------------------------------------------------------------
! 6/2007   - Original Version  (Kyle A. Brucker)
!------------------------------------------------------------------------

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


subroutine trail(line,lenc)
!This subroutine locates the last nonblank character
!in a general length character string.

!Passed Variables
 !line - String to search for the last nonblank character.
 !lenc - Character number of the last nonblank character in line.

!Modification History
!------------------------------------------------------------------------
! 07/03/95 - Cleaned original version (Scott Stanley)
! 6/2007   - Rewritten in Fortran 90  (Kyle A. Brucker)
!------------------------------------------------------------------------

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


subroutine lead(line,lenc)
!This subroutine locates the first nonblank character
!in a general length character string.

!Passed Variables
 !line - String to search for first nonblank character.
 !lenc - Character number of first nonblank character.

!Modification History
!------------------------------------------------------------------------
! 07/03/95 - Cleaned original version (Scott Stanley)
! 6/2007   - Rewritten in Fortran90   (Kyle A. Brucker)
!------------------------------------------------------------------------

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

