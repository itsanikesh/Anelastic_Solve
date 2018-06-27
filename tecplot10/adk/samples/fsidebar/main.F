C    
C     This function is called when
C     the $!ADDONCOMMAND macro command is
C     processed.
C     
      INTEGER*4 Function MacroCommandCallback(MacroCommandString,
     &                                        ErrMsg)
      INCLUDE 'ADDGLBL.INC'
      CHARACTER*(*) MacroCommandString
      CHARACTER*(*) ErrMsg
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      INTEGER*4 IsOk

      IsOk = 1

C     
C     MacroCommandString is the add-on macro command string needing processing.
C    
C     ErrMsg is an error message string which must be assigned somthing if
C     there is an error (i.e. IsOk is set to 0).
C     
  
      Call TecUtilLockStart(AddOnID)
  
C   
C     TO DO : Process the addon command
C    
C     Example:
C    
C     $!ADDONCOMMAND ADDONID='Sidebar FORTRAN Example' COMMAND='MYCOMMAND'
C    
  
      if (MacroCommandString.eq.'MYCOMMAND') Then
C       IsOk = ProcessMyCommand()
        Write (*,*) 'ProcessMyCommand function missing'
      Endif

      if (IsOk.Eq.0) Then
C     
C     Some kind of error, so tell the user about it.  MAKE SURE
C     and terminate the string with char(0).
C      
        ErrMsg = 'Some error message'//char(0)
      Endif

      Call TecUtilLockFinish(AddOnID)
      MacroCommandCallback = IsOk
      Return
      End

C
C This function will be called by Tecplot
C each time a state change occurs.
C
      SUBROUTINE StateChangeCallback(StateChange, ArbParamPtr)
      INCLUDE 'ADDGLBL.INC'
      INTEGER*4 StateChange
      POINTER(ArbParamPtr, ArbParam)
      INTEGER*4 ArbParam
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
C    
C     This function will be called by Tecplot
C     each time a state change occurs.
C    
C    
C     NOTE:
C    
C     By default, when loading a layout or running a macro,
C     you will not receive any state change callbacks until
C     the macro or layout is complete. At that time you will
C     receive the StateChange_CompleteReset message.
C    
C     If you wish to receive state changes AS a macro is run
C     or a layout file is loaded, you must call
C     TecUtilStateChangeSetMode(...). See the ADK reference
C     manual for more information on this function.
C    
C     
      IF     (StateChange.EQ.StateChange_VarsAltered)            THEN
      ElseIf (StateChange.EQ.StateChange_VarsAdded)              THEN
      ElseIf (StateChange.EQ.StateChange_ZonesDeleted)           THEN
      ElseIf (StateChange.EQ.StateChange_ZonesAdded)             THEN
      ElseIf (StateChange.EQ.StateChange_NodeMapsAltered)        THEN
      ElseIf (StateChange.EQ.StateChange_MouseModeUpdate)        THEN
      ElseIf (StateChange.EQ.StateChange_FrameDeleted)           THEN
      ElseIf (StateChange.EQ.StateChange_NewTopFrame)            THEN
      ElseIf (StateChange.EQ.StateChange_Style)                  THEN
      ElseIf (StateChange.EQ.StateChange_Text)                   THEN
      ElseIf (StateChange.EQ.StateChange_Geom)                   THEN
      ElseIf (StateChange.EQ.StateChange_DataSetReset)           THEN
      ElseIf (StateChange.EQ.StateChange_NewLayout)              THEN
      ElseIf (StateChange.EQ.StateChange_CompleteReset)          THEN
      ElseIf (StateChange.EQ.StateChange_LineMapAssignment)      THEN
      ElseIf (StateChange.EQ.StateChange_ContourLevels)          THEN
      ElseIf (StateChange.EQ.StateChange_ModalDialogLaunch)      THEN
      ElseIf (StateChange.EQ.StateChange_ModalDialogDismiss)     THEN
      ElseIf (StateChange.EQ.StateChange_QuitTecplot)            THEN
      ElseIf (StateChange.EQ.StateChange_ZoneName)               THEN
      ElseIf (StateChange.EQ.StateChange_VarName)                THEN
      ElseIf (StateChange.EQ.StateChange_LineMapName)            THEN
      ElseIf (StateChange.EQ.StateChange_LineMapAddDeleteOrR)    THEN
      ElseIf (StateChange.EQ.StateChange_View)                   THEN
      ElseIf (StateChange.EQ.StateChange_ColorMap)               THEN
      ElseIf (StateChange.EQ.StateChange_ContourVar)             THEN
      ElseIf (StateChange.EQ.StateChange_Streamtrace)            THEN
      ElseIf (StateChange.EQ.StateChange_NewAxisVariables)       THEN
      ElseIf (StateChange.EQ.StateChange_PickListCleared)        THEN
      ElseIf (StateChange.EQ.StateChange_PickListGroupSelect)    THEN
      ElseIf (StateChange.EQ.StateChange_PickListSingleSelec)    THEN
      ElseIf (StateChange.EQ.StateChange_PickListStyle)          THEN
      ElseIf (StateChange.EQ.StateChange_DataSetFileName)        THEN
      ElseIf (StateChange.EQ.StateChange_DrawingInterrupted)     THEN
      Else
C
C...... New statechange.  
C
      Endif
C
      Return
      End




      SUBROUTINE MenuCallback()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INTEGER*4 IGUI
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
      Call TecGUISidebarActivate(Sidebar1Manager)
      Call TecUtilLockFinish(AddOnID)
      return
      end
 


      Subroutine InitTecAddOn()
C     The following line tells Compaq Visual Fortran to export this function
!DEC$ ATTRIBUTES DLLEXPORT::InitTecAddOn
      INCLUDE  'ADDGLBL.INC'
      INTEGER*4  I
      EXTERNAL   MenuCallback
      EXTERNAL   MacroCommandCallback 
      INTEGER*4  MacroCommandCallback
      EXTERNAL   StateChangeCallback 
      INCLUDE  'FGLUE.INC'
      INCLUDE  'GUIDEFS.INC'


C   
C    The function TecUtilAddOnRegister() is the
C    only function that is REQUIRED to be called from
C    the initialization function.
C   
C    The information you give Tecplot by calling
C    this function will show up in the Help/About Addons
C    dialog box.
C    
C   
C    Note that if your add-on requires a specific version of Tecplot,
C    you would check for that here using TecUtilGetTecplotVersion()
C    
 
      Call TecUtilLockOn()
C
      Call TecUtilAddOnRegister(
     &         100,
     &         'Sidebar FORTRAN Example'//char(0),
     &         'V1.0-01/01/2000'//char(0),
     &         'Example'//char(0),
     &         AddOnID)

      Call InitTGB()

      if(TecUtilMacroIsBatchModeActive().EQ.0) then
        Call BuildSidebar1()
      endif


      I = TecUtilMacroAddCommandCallback(
     &          'Sidebar FORTRAN Example'//char(0),
     &          MacroCommandCallback) 
      I = TecUtilStateChangeAddCallback(StateChangeCallback) 
C... Edit Menu Name....
      I = TecUtilMenuAddOption(
     &         'Tools'//char(0),
     &         'Sidebar FORTRAN Example'//char(0),
     &         ''//char(0),
     &         MenuCallback)

      Call TecUtilLockOff()
      return
      end
