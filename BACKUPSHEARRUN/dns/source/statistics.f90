












subroutine statistics(stat)
!@t
! \textbf{subroutine statistics(stat)}
!@h
!   Description:
!     Determine whether the simulation is of a wake or shear layer and
!     call the appropriate statistics package.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use IO,         only: IOUT
 use Parameters, only: flow_type
 use Flow,       only: Vmodel, Rmodel
 implicit none

 !Passed Variables
  integer,intent(out) :: stat
 !Local Variables
  integer             :: ok

 ok=0

  select case(flow_type)
   case('Vshear','Channel')
    if (Vmodel.EQ.'DNS') then
     call statistics_vshear_DNS(ok)
    elseif (Vmodel.EQ.'SSM'.or.Vmodel.EQ.'DSM'.or.Vmodel.EQ.'DMM') then
     call statistics_vshear_EDDY(ok)
    else
     write(IOUT,'(a30,a)') "STATISTICS NOT AVAILABLE FOR MODEL TYPE: ",trim(flow_type)
     ok=1
    endif
   case('Twake','wake','SPwake','PRPwake')
!    call statistics_wake(ok)
   case DEFAULT
    write(IOUT,'(a30,a)') "STATISTICS NOT AVAILABLE FOR FLOW TYPE: ",trim(flow_type)
    ok=1
   end select

 stat=ok
return
end subroutine statistics

subroutine statistics_small(stat)
!@t
! \textbf{subroutine statistics\_small(stat)}
!@h
!   Description:
!     Determine whether the simulation is of a wake or shear layer and
!     call the appropriate small statistics package.
!@q
!   Current Code Owner:
!     Matt de Stadler (mdestadl@ucsd.edu)

!   Modification History
!     Version   Date     Comment 
!     -------   ----     ------- 
!     1.0       07/2008  Original code. [Kyle A. Brucker] 

 use IO,         only:IOUT
 use Parameters, only: flow_type
 implicit none

 !Passed Variables
  integer,intent(out) :: stat
 !Local Variables
  integer             :: ok

 ok=0

  select case(flow_type)
   case('Vshear','Channel')
    call statistics_vshear_small(ok)
   case('Twake','Wake','SPwake','PRPwake')
!   call statistics_wake_small(ok)
   case DEFAULT
    write(IOUT,'(a30,a)') "STATISTICS NOT AVAILABLE FOR FLOW TYPE: ",trim(flow_type)
    ok=1
   end select

 stat=ok
return
end subroutine statistics_small

subroutine statistics_vshear(ok)
 implicit none
 integer,intent(out) :: ok
 ok=0
 return
end subroutine

subroutine statistics_vshear_small(ok)
 implicit none
 integer,intent(out) :: ok
 ok=0
 return
end subroutine
subroutine statistics_wake(ok)
 implicit none
 integer,intent(out) :: ok
 ok=0
 return
end subroutine

subroutine statistics_wake_small(ok)
 implicit none
 integer,intent(out) :: ok
 ok=0
 return
end subroutine

