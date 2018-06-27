$$IF(AW_MACROCOMMANDS)
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
$$IF(AW_USETGB)
      INCLUDE 'GUIDEFS.INC'
$$ENDIF
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
C     $!ADDONCOMMAND ADDONID='$$AW_ADDONNAME$$' COMMAND='MYCOMMAND'
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
$$ENDIF

$$IF(AW_STATECHANGE)
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
$$IF(AW_USETGB)
      INCLUDE 'GUIDEFS.INC'
$$ENDIF
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

$$ENDIF



$$IF(AW_GENERAL)
$$IF(AW_MAKEMENUOPTION)
      SUBROUTINE MenuCallback()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INTEGER*4 IGUI
$$IF(AW_USETGB)
      INCLUDE 'GUIDEFS.INC'
$$ENDIF
      Call TecUtilLockStart(AddOnID)
$$IF(AW_USETGB)
$$IF(!AW_ISMODAL)
$$IF(!AW_ISMODELESS)
$$IF(AW_HASSIDEBAR)
      Call TecGUISidebarActivate(Sidebar1Manager)
$$ENDIF
$$ENDIF
$$ENDIF
$$IF(AW_ISMODAL)
      Call BuildDialog1(MAINDIALOGID)
      Call TecGUIDialogLaunch(Dialog1Manager)
$$ENDIF
$$IF(AW_ISMODELESS)
      Call BuildDialog1(MAINDIALOGID)
      Call TecGUIDialogLaunch(Dialog1Manager)
$$ENDIF
$$ENDIF
$$IF(!AW_USETGB)
      IGUI = TecUtilDialogMessageBox("Menu function called"//char(0),
     &                                MessageBox_Information) 
$$ENDIF
      Call TecUtilLockFinish(AddOnID)
      return
      end
$$ENDIF
$$ENDIF
 


      Subroutine InitTecAddOn()
C     The following line tells Compaq Visual Fortran to export this function
!DEC$ ATTRIBUTES DLLEXPORT::InitTecAddOn
      INCLUDE  'ADDGLBL.INC'
      INTEGER*4  I
$$IF(AW_GENERAL)
$$IF(AW_MAKEMENUOPTION)
      EXTERNAL   MenuCallback
$$ENDIF
$$ENDIF
$$IF(AW_LOADER)
$$IF(AW_INCLUDEOVERRIDE)
      EXTERNAL   LoaderCallback 
      EXTERNAL   LoaderSelectedCallback 
      EXTERNAL   InstructionOverrideCallback
$$ENDIF
$$IF(!AW_INCLUDEOVERRIDE)
      EXTERNAL   LoaderCallback 
      EXTERNAL   LoaderSelectedCallback 
$$ENDIF
$$ENDIF
$$IF(AW_MACROCOMMANDS)
      EXTERNAL   MacroCommandCallback 
      INTEGER*4  MacroCommandCallback
$$ENDIF
$$IF(AW_STATECHANGE)
      EXTERNAL   StateChangeCallback 
$$ENDIF
$$IF(AW_CONVERTER)
      EXTERNAL   ConverterCallback 
$$ENDIF
$$IF(AW_CURVEFIT)
      POINTER(NullFuncPtr, NullFunc)
      INTEGER*4  NullFunc
      EXTERNAL   XYDataPointsCallback
      EXTERNAL   CurveInfoStringCallback
$$IF(AW_USEPROBECB)
      EXTERNAL   ProbeValueCallback
$$ENDIF
$$IF(AW_USETGB)
      EXTERNAL   CurveSettingsCallback
      EXTERNAL   AbbreviatedSettingsStringCallback
$$ENDIF
$$ENDIF
      INCLUDE  'FGLUE.INC'
$$IF(AW_USETGB)
      INCLUDE  'GUIDEFS.INC'
$$ENDIF

$$IF(AW_CURVEFIT)
      NullFuncPtr = 0
$$ENDIF

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
     &         $$AW_TECPLOTVERSION$$,
     &         '$$AW_ADDONNAME$$'//char(0),
     &         'V1.0-01/01/2000'//char(0),
     &         '$$AW_COMPANY$$'//char(0),
     &         AddOnID)

$$IF(AW_USETGB)
      Call InitTGB()

$$IF(AW_HASSIDEBAR)
      if(TecUtilMacroIsBatchModeActive().EQ.0) then
        Call BuildSidebar1()
      endif
$$ENDIF
$$ENDIF

$$IF(AW_LOADER)
$$IF(AW_INCLUDEOVERRIDE)
      I = TecUtilImportAddLoader(
     &          LoaderCallback,
     &          '$$AW_ADDONNAME$$'//char(0),
     &          LoaderSelectedCallback,
     &          InstructionOverrideCallback) 
$$ENDIF
$$IF(!AW_INCLUDEOVERRIDE)
      I = TecUtilImportAddLoader(
     &          LoaderCallback,
     &          '$$AW_ADDONNAME$$'//char(0),
     &          LoaderSelectedCallback,
     &          0) 
$$ENDIF
$$ENDIF

$$IF(AW_MACROCOMMANDS)
      I = TecUtilMacroAddCommandCallback(
     &          '$$AW_ADDONNAME$$'//char(0),
     &          MacroCommandCallback) 
$$ENDIF
$$IF(AW_STATECHANGE)
      I = TecUtilStateChangeAddCallback(StateChangeCallback) 
$$ENDIF
$$IF(AW_GENERAL)
$$IF(AW_MAKEMENUOPTION)
C... Edit Menu Name....
      I = TecUtilMenuAddOption(
     &         'Tools'//char(0),
     &         '$$AW_MENUTEXT$$'//char(0),
     &         ''//char(0),
     &         MenuCallback)
$$ENDIF
$$ENDIF

$$IF(AW_CONVERTER)
      I = TecUtilImportAddConverter(
     &          ConverterCallback,
     &          '$$AW_ADDONNAME$$'//char(0),
     &          '*.*'//char(0)) 
$$ENDIF
$$IF(AW_CURVEFIT)
C
C     Register this add-on as an extended curve fit. But first, check
C     that Tecplot is version 9 or later, as earlier versions did not
C     support extended curve fits.
C
      if(TecUtilGetTecplotVersion().LT.890000) then
        call TecUtilDialogErrMsg("Extended curve-fit add-on requires"//
     &       " version 9 or greater"//char(0))
      else
$$IF(AW_USETGB)
$$IF(AW_USEPROBECB)
        I = TecUtilCurveRegisterExtCrvFit(
     &          '$$AW_ADDONNAME$$'//char(0),
     &          XYDataPointsCallback,
     &          ProbeValueCallback,
     &          CurveInfoStringCallback,
     &          CurveSettingsCallback,
     &          AbbreviatedSettingsStringCallback)
$$ENDIF
$$IF(!AW_USEPROBECB)
        I = TecUtilCurveRegisterExtCrvFit(
     &          '$$AW_ADDONNAME$$'//char(0),
     &          XYDataPointsCallback,
     &          NullFunc,
     &          CurveInfoStringCallback,
     &          CurveSettingsCallback,
     &          AbbreviatedSettingsStringCallback)
$$ENDIF
$$ENDIF
$$IF(!AW_USETGB)
$$IF(AW_USEPROBECB)
        I = TecUtilCurveRegisterExtCrvFit(
     &          '$$AW_ADDONNAME$$'//char(0),
     &          XYDataPointsCallback,
     &          ProbeValueCallback,
     &          CurveInfoStringCallback,
     &          NullFunc,
     &          NullFunc)
$$ENDIF
$$IF(!AW_USEPROBECB)
        I = TecUtilCurveRegisterExtCrvFit(
     &          '$$AW_ADDONNAME$$'//char(0),
     &          XYDataPointsCallback,
     &          NullFunc,
     &          CurveInfoStringCallback,
     &          NullFunc,
     &          NullFunc)
$$ENDIF
$$ENDIF
      endif

$$ENDIF
      Call TecUtilLockOff()
      return
      end
