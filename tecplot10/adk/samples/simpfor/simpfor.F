C
C... Create a subroutine that will be called when tecplot
C... Starts up.  
C
      Subroutine InitTecAddOn()
      include 'FGLUE.INC'
      INTEGER*4 IsOk
      external toolcallback
	!DEC$ ATTRIBUTES DLLEXPORT::InitTecAddOn

      Call TecUtilLockOn()

      CALL TECUTILADDONREGISTERINFO(
     & 'FORTRAN TEST'//char(0),
     & 'V1.0-09/27/97'//char(0),
     & 'Amtec Engineering'//char(0))
C
C... TECUTILMENUADDOPTION adds another menu option to 
C... the "Tools" main menu in the tecplot interface.
C
      IsOk = TECUTILMENUADDOPTION('TOOLS'//char(0),
     &                            'SIMPFOR'//char(0),
     &                            'F'//char(0),
     &                            toolcallback)
      Call TecUtilLockOff()
      return
      end


C
C...Subroutine to call when the tool menu option
C..."FORTRAN OPTION" (registered above) is selected by the user.
C...This subroutine shows how to access data in tecplot
C...and change the values.
C
      Subroutine toolcallback ()
      include 'FGLUE.INC'
      INTEGER*4     I
      INTEGER*4     J
      INTEGER*4     IsOk
      character*256 DataSetTitle
      INTEGER*4     Length
      INTEGER*4     IMAX,JMAX,KMAX,IDTYPE
      INTEGER       DATAALTERED
      POINTER       (VSetPtr, VSet)
      REAL*8        X
      POINTER       (REALPTR, REALARRAY)
      POINTER       (NullPntr, Null)
      POINTER       (VarNamesPointer, VarNamesUnused)
      REAL*4        REALARRAY(10,10)
	CHARACTER*256 Message
c
c.... Creating a Null pointer
c
      NullPntr = 0

C... Check to see if a dataset is attached to the current
C... Frame.  If IsOk is equal to TRUE then one is attached.
C... Otherwise, create one and add a zone.
C
      Call TECUTILLOCKON()

      DATAALTERED = 0

      if (TECUTILDATASETISAVAILABLE().Eq.FALSE) Then

        call TecUtilStringListAlloc(VarNamesPointer)

        IsOk = TecUtilStringListAppendString(VarNamesPointer,
     &                                       "X"//char(0))
        if(IsOk .eq. 1)
     &    IsOk = TecUtilStringListAppendString(VarNamesPointer,
     &                                         "Y"//char(0))
        if(IsOk .eq. 1)
     &    IsOk = TecUtilDataSetCreate("My Dataset"//char(0),
     &                                VarNamesPointer,
     &                                0)

	  call TecUtilStringListDealloc(VarNamesPointer)

	  if (IsOk .eq. 1) then
     &    IsOk = TecUtilDataSetAddZone("My zone"//char(0),
     &                                 10,
     &                                 10,
     &                                 1,
     &                                 ZONETYPE_ORDERED,
     &                                 Null)

	  if (IsOk .eq. 1)
     &    IsOk = TecUtilFrameSetMode(Frame_TwoD)
	endif

      if (TECUTILDATASETISAVAILABLE().Eq.TRUE) Then
        IsOk = TECUTILDATASETGETINFO(DataSetTitle,
     &                               Length,
     &                               NumZones,
     &                               NumVars)
C
          write(Message, *) 'DataSetTitle,NumZones,NumVars = ',
     &                DataSetTitle(1:Length),
     &                NumZones,NumVars, char(0)
	    I = TecUtilDialogMessageBox(Message, MessageBox_Information)
C
C... Get some information about zone 1.
C
        call TECUTILZONEGETINFO(
     &            1, IMAX, JMAX, KMAX,
     &            Null, Null, Null, Null,
     &            Null, Null, Null,
     &            Null, Null, Null)

C
C... Slow way to get a field value and change it.
C... Get value from zone 1, variable 1, offset 3.
C
        DATAALTERED = 1

        X = TECUTILDATAVALUEGETBYZONEVAR(1,1,3)
        write (Message,*) 'X at offset 3 is ',X, char(0)
	  I = TecUtilDialogMessageBox(Message, MessageBox_Information)

        IsOk = TECUTILDATAVALUESETBYZONEVAR(1,1,3,6.7D0)


        X = TECUTILDATAVALUEGETBYZONEVAR(1,1,3)
        write (Message,*) 'X at offset 3 is ',X, char(0)
	  I = TecUtilDialogMessageBox(Message, MessageBox_Information)

C
C... Faster way to do the same thing.  This time
C... alter all X-values in zone 1.
C

        CALL TECUTILDATAVALUEGETRAWPTR(1,
     &                                 1,
     &                                 REALPTR,
     &                                 IDTYPE)

        write (Message,*) 'IDTYPE = ',IDTYPE, char(0)
	  I = TecUtilDialogMessageBox(Message, MessageBox_Information)

        Do 10 I = 1,IMax
        Do 10 J = 1,JMax
   10     REALARRAY(I,J) = REALARRAY(I,J)+1

      Else
        CALL TECUTILDIALOGERRMSG(
     &  'No dataset in current frame'//char(0))
      Endif
C
C... Inform tecplot that variable 1 (i.e. X) has changed.
C... Tecplot needs to know this so doing things like
C... view/fit will know to recalculate mins and maxes
C... etc. if one of the spacial variables has been
C... altered.  
C
      if (DATAALTERED.NE.0) Then
        call TecUtilSetAlloc(TRUE, VSetPtr)
        if (VSetPtr .ne. 0) Then
          IsOk = TecUtilSetAddMember(VSetPtr, 1, TRUE)
          call TecUtilStateChanged(STATECHANGE_VARSALTERED,
     &                             VSetPtr)
          call TecUtilSetDealloc(VSetPtr)
        Endif
	  I = TecUtilViewFit()
      Endif
C
C... Unlock tecplot
C
      Call TecUtilLockOff()
      return
      end

