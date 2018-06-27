module BTEC
 integer,parameter :: r8=4
 integer,parameter :: DIsDouble=0
 integer,parameter :: VIsDouble=0
 integer,parameter :: tec_debug=0

 private :: slength, lead,trail 

 contains

 subroutine write_tec_binary(is1,is2,is3,var)
  !Write to open tecplot binary file.  Must be called after open_tec_binary

  !Passed Variables
  !is1 - nx1 {integer}
  !is2 - nx2 {integer}
  !is3 - nx3 {integer}

  !var - field variable to write {real(nx1,nx2,nx3}

  !Modification History
  !------------------------------------------------------------------------
  ! 6/2007   - Original Version  (Kyle A. Brucker)
  !------------------------------------------------------------------------

  implicit none
                                                                                                                             
  !Passed Variables
  integer,intent(in)                          :: is1, is2, is3
  real(r8),intent(in),dimension( is1,is2,is3 ) :: var

  !Local Variables
  integer                                     :: TecDat,IRES

   IRES   = TecDat(is1*is2*is3,var,DIsDouble)

  return
 end subroutine write_tec_binary

 subroutine open_tec_binary(stime,is1,is2,is3,line1,line2,line3,line4,Debug,VIsDouble)
  !Open a tecplot binary file for writing
  !set is3=1 for 2d plane

  !Passed Variables
  !stime- time {real(8)} 
  !is1 - nx1 {integer}
  !is2 - nx2 {integer}
  !is3 - nx3 {integer}
  !line1- title {string}
  !line2- list of variables including x1,x2,(x3) {string}
  !line3- output file name {string}
  !line4- temp directory for scratch files {string}
  !Debug- Debug Lever 0,1,2 {integer}                <---0 should normally be used 
  !VIsDouble- 0=SP 1=DP (tecplot output) {integer}   <---SP should normally be used

  !Modification History
  !------------------------------------------------------------------------
  ! 6/2007   - Original Version  (Kyle A. Brucker)
  !------------------------------------------------------------------------

  implicit none

  !Passed Variables
  integer,intent(in)            :: is1, is2, is3 
  character(len=*),intent(in)   :: line1
  character(len=*),intent(in)   :: line2
  character(len=*),intent(in)   :: line3
  character(len=*),intent(in)   :: line4
  integer,intent(in)            :: Debug, VIsDouble
  real(8)                       :: stime
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
  !Wrapper routine to pass strings of the correct length to the tecplot library

  implicit none
  !Passed Variables
  !stime- time {real(8)} 
  !is1 - nx1 {integer}
  !is2 - nx2 {integer}
  !is3 - nx3 {integer}
  !line1- title {string}
  !slen1- length of line1 {integer}
  !line2- list of variables including x1,x2,(x3) {string}
  !slen2- length of line2 {integer}
  !line3- output file name {string}
  !slen3- length of line3 {integer}
  !line4- temp directory for scratch files {string}
  !slen4- length of line4 {integer}
  !Debug- Debug Lever 0,1,2 {integer}                <---0 should normally be used 
  !VIsDouble- 0=SP 1=DP (tecplot output) {integer}   <---SP should normally be used

  !Modification History
  !------------------------------------------------------------------------
  ! 6/2007   - Original Version  (Kyle A. Brucker)
  !------------------------------------------------------------------------

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
  !Used to close tecplot binary file.

  !Modification History
  !------------------------------------------------------------------------
  ! 6/2007   - Original Version  (Kyle A. Brucker)
  !------------------------------------------------------------------------

  implicit none

  !Local Variables
  integer(4)                        :: TecEnd,IRES

  IRES   = TecEnd()

  return
 end subroutine close_tec_binary
 
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

end module BTEC
