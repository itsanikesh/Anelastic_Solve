C    
C     This function is called when
C     the $!ADDONCOMMAND macro command is
C     processed.
C     
      INTEGER*4 Function MacroCommandCallback(MacroCommandString,
     &                                        ErrMsg)
      CHARACTER*(*) MacroCommandString
      CHARACTER*(*) ErrMsg
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'GUIDEFS.INC'
      EXTERNAL  CheckZone
      INTEGER*4 CheckZone
      EXTERNAL  CreateCellCenteredZone
      INTEGER*4 CreateCellCenteredZone
      INTEGER*4 IsOk
      INTEGER*4 ZoneToProcess

      IsOk = 1

C     
C     MacroCommandString is the add-on macro command string needing processing.
C    
C     ErrMsg is an error message string which must be assigned somthing if
C     there is an error (i.e. IsOk is set to 0).
C     
      Call TecUtilLockOn()
C   
C
C...This addon only does one thing so the "command" simply
C   contains the zone to process...
C    
C     $!ADDONCOMMAND ADDONID='Create Cell Center' COMMAND='n'
C    
C
C...Get the zone number
C  
      READ(MacroCommandString,1) ZoneToProcess
    1 Format(I)

      IsOk = CheckZone(ZoneToProcess)
      if (IsOk.Eq.TRUE) Then
        IsOk = CreateCellCenteredZone(ZoneToProcess)
      Endif

      if (IsOk.Eq.FALSE) Then
C     
C     Some kind of error, so tell the user about it.  MAKE SURE
C     and terminate the string with char(0).
C      
        ErrMsg = 'No DataSet, Invalid Zone, or Zone not IJK'//char(0)
      Endif

      Call TecUtilLockOff()
      MacroCommandCallback = IsOk
      Return
      End

C
C This function will be called by Tecplot
C each time a state change occurs.
C
      SUBROUTINE StateChangeCallback(StateChange, ArbParamPtr)
      INTEGER*4 StateChange
      POINTER(ArbParamPtr, ArbParam)
      INTEGER*4 ArbParam
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUI.INC'
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
      ElseIf (StateChange.EQ.StateChange_XYMapAssignment)        THEN
      ElseIf (StateChange.EQ.StateChange_ContourLevels)          THEN
      ElseIf (StateChange.EQ.StateChange_ModalDialogLaunch)      THEN
      ElseIf (StateChange.EQ.StateChange_ModalDialogDismiss)     THEN
      ElseIf (StateChange.EQ.StateChange_QuitTecplot)            THEN
      ElseIf (StateChange.EQ.StateChange_ZoneName)               THEN
      ElseIf (StateChange.EQ.StateChange_VarName)                THEN
      ElseIf (StateChange.EQ.StateChange_XYMapName)              THEN
      ElseIf (StateChange.EQ.StateChange_XYMapAddDeleteOrReo)    THEN
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
      ElseIf (StateChange.EQ.StateChange_DrawingInterruped)      THEN
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
      INCLUDE 'GUI.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockOn()
      Call BuildDialog1(MAINDIALOGID)
      Call TecGUIDialogLaunch(Dialog1Manager)
      Call TecUtilLockOff()
      return
      end
 


      Subroutine InitTecAddOn()
      INTEGER*4  I
      EXTERNAL   MenuCallback
      EXTERNAL   MacroCommandCallback 
      EXTERNAL   StateChangeCallback 
      INCLUDE  'ADDGLBL.INC'
      INCLUDE  'FGLUE.INC'
      INCLUDE  'GUI.INC'
      INCLUDE  'GUIDEFS.INC'


C   
C    The function TecUtilAddOnRegisterInfo() is the
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
      Call InitTGB()
C
      CALL TECUTILADDONREGISTERINFO(
     &         'Create Cell Center'//char(0),
     &         'V1.0-01/01/2000'//char(0),
     &         'Amtec Engineering'//char(0))
 
C... Edit Menu Name....
      I = TECUTILMENUADDOPTION(
     &         'Tools'//char(0),
     &         'Create Cell Center'//char(0),
     &         'M'//char(0),
     &         MenuCallback)



      I = TecUtilMacroAddCommandCallback(
     &          'Create Cell Center'//char(0),
     &          MacroCommandCallback) 
      I = TecUtilStateChangeAddCallback(StateChangeCallback) 
      Call TecUtilLockOff()
      return
      end
