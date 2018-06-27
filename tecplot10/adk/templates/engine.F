$$IF(AW_LOADER)
      INTEGER*4 Function LoaderCallback(InstructionPtr)
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      POINTER       (InstructionPtr,InstructionsUnused)
      INTEGER*4     InstructionsUnused
      INTEGER*4     IsOk
      INTEGER*4     Count
      INTEGER*4     I
      INTEGER*4     SLen
      CHARACTER*256 INSTRING

      IsOk = 1
C  
C    Tecplot will call this function any time it needs to load 
C    data using this custom loader. 
C   
C      IMPORTANT: After the data has been successfully loaded,
C      be sure to call TecUtilImportSetLoaderInstr(). This will allow
C      the user to save a layout file which references this 
C      custom loader.
C   

      Call TecUtilLockStart(AddOnID)
  
      Count = TecUtilStringListGetCount(InstructionPtr)

      do i=1,Count
        Call TecUtilStringListGetString(InstructionPtr,i,INSTRING,SLen)
C
C       The instruction is now in the variable INSTRING and has a length 
C       of SLen
C
      enddo

      Call TecUtilLockFinish(AddOnID)
      LoaderCallback = IsOk
      Return
      End






      Subroutine LoaderSelectedCallback()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
C  
C   This function is called when
C   the user selects this dataset
C   loader from the list of importers
C   in the File/Import dialog.
C  
C   TODO:
C  
C   1. Launch a dialog which collects settings
C      for how the file should be loaded.
C      (filename, skip, etc.)
C  
C   2. In the OK callback of the dialog, close the dialog
C      and call a function to load the data using the
C      indicated settings. Note: you may want to use
C      the same function to load the data from the
C      LoaderCallback() function above.
C  
C   3. IMPORTANT: After the data has been successfully loaded,
C      be sure to call TecUtilImportSetLoaderInstr(). This will allow
C      the user to save a layout file which references this 
C      custom loader.
C  

      Call TecUtilLockStart(AddOnID)
      Call BuildDialog1(MAINDIALOGID)
      Call TecGUIDialogLaunch(Dialog1Manager)
      Call TecUtilLockFinish(AddOnID)
      Return
      End





$$IF(AW_INCLUDEOVERRIDE)
      INTEGER*4 Function InstructionOverrideCallback(InstructionPtr)
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      INTEGER*4 IsOk
      POINTER (InstructionPtr,DummyInstructions)
      INTEGER*4 DummyInstructions
      IsOk = 1
C   
C     ToDo:
C     Add code to view the current instructions and display
C     an interface to the user to change them.
C    
      InstructionOverrideCallback = IsOk
      Return
      End
$$ENDIF
$$ENDIF




$$IF(AW_CONVERTER)
      INTEGER*4 Function ConverterCallback(DataFName,
     &                                     TempBinFName,
     &                                     MessageString)
      INCLUDE 'ADDGLBL.INC'
      CHARACTER*(*) DataFName
      CHARACTER*(*) TempBinFName
      CHARACTER*(*) MessageString
      INCLUDE 'FGLUE.INC'
      INTEGER*4 IsOk,IErr

      IsOk = 1

      call TecUtilLockStart(AddOnID)
C   
C     Add code here to open DataFName,
C     read in the data and write out a binary
C     Tecplot datafile to TempBinFName using
C     calls to TecUtilTecxxx functions. On error
C     populate MessageString with an error message
C     (no more than 2000 characters) and set IsOk to 0
C    

C   
C    Remove next call when finished...
C    
       IErr = TecUtilDialogMessageBox(
     & '$$AW_ADDONNAME$$ data converter currently '//
     & 'under construction....'//char(0),
     & MessageBox_Information)
  
      Call TecUtilLockFinish(AddOnID)
      ConverterCallback = IsOk
      Return
      End
$$ENDIF



$$IF(AW_CURVEFIT)
$$IF(AW_USETGB)

      Subroutine CurveSettingsCallback(SelectedMapsPtr,
     &                                 MapExtCrvSettingsPtr)
      INCLUDE 'ADDGLBL.INC'
      Pointer(SelectedMapsPtr,SelectedXYUnused)
      INTEGER*4 SelectedXYUnused
      Pointer(MapExtCrvSettingsPtr,MapExtCrvSettingsUnused)
      INTEGER*4 MapExtCrvSettingsUnused
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'ENGINE.INC'
C
      Call TecUtilLockStart(AddOnID)
C
C    This function is called when, for a map having an extended 
C    curve fit of this type, the user selects Curve Settings
C    in the Curves-Fits attributes page of the Plot Attributes 
C    dialog. 
C    
C    Save off the pointers to XYMapSettings and SelectedXYMaps
C    for use by functions in guicb.F
C
      XYMapSetPtr      = SelectedMapsPtr
      XYMapSettingsPtr = MapExtCrvSettingsPtr
C    
C    Build and launch the dialog.
C
      Call BuildDialog1(MAINDIALOGID)
      Call TecGUIDialogLaunch(Dialog1Manager)
      Call TecUtilLockFinish(AddOnID)
      Return
      End


      Subroutine AbbreviatedSettingsStringCallback(
     &                                 XYMapNum,
     &                                 CurveSettings,
     &                                 AbbreviatedSettings)
      INCLUDE 'ADDGLBL.INC'
      Integer*4 XYMapNum
      CHARACTER*(*) CurveSettings
      CHARACTER*(*) AbbreviatedSettings
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
C
      Call TecUtilLockStart(AddOnID)
C
C    Create a string based on your settngs. The first
C    time Tecplot calls this function, CurveSettings 
C    will be NULL, so new settings must be constructed
C    from default values. AbbrevieatedSettings is the 
C    string that will be presented to the Plot Attributes 
C    dialog. 
C    
      Call TecUtilLockFinish(AddOnID)
      Return
      End


$$ENDIF

      INTEGER*4 Function  XYDataPointsCallback(RawIndVPtr,
     &                                         RawDepVPtr,
     &                                         IndVCoordScale,
     &                                         DepVCoordScale,
     &                                         NumRawPts,
     &                                         NumCurvePts,
     &                                         XYMapNum,
     &                                         CurveSettings,
     &                                         IndCurveValues,
     &                                         DepCurveValues)
      INCLUDE 'ADDGLBL.INC'
      Pointer       (RawIndVPtr,RawIndVUnused)
      INTEGER*4     RawIndVUnused
      Pointer       (RawDepVPtr,RawDepVUnused)
      INTEGER*4     RawDepVUnused
      INTEGER*4     IndVCoordScale
      INTEGER*4     DepVCoordScale
      INTEGER*4     NumRawPts
      INTEGER*4     NumCurvePts
      INTEGER*4     XYMapNum
      CHARACTER*(*) CurveSettings
      Real*8        IndCurveValues(NumCurvePts)
      Real*8        DepCurveValues(NumCurvePts)

 
      Real*8 Average, IndVarMin, IndVarMax, Delta
      Real*8 SimpleAverage
      INTEGER*4 IsOK,IPoint

      IsOK = 1

      Call TecUtilLockStart(AddOnID)
C
C     BEGINNING OF DEFAULT CURVE FIT
C
C     The rest of the function, between here and the END OF DEFAULT
C     CURVE FIT comment, is the information for the default curve
C     fit (straight line at the average of the raw dependent variable).
C     Replace with your own code for loading the curve data arrays.
C

C
C     Get the min and max values of the independent variable and
C     use these values to calculate the size of each step along
C     the independent axis for the curve fit.

      call TecUtilDataValueGetMinMaxByRef(RawIndVPtr,
     &                                    IndVarMin,
     &                                    IndVarMax)

      Delta = (IndVarMax - IndVarMin) / float(NumCurvePts - 1)
C
C     Find the average value of the raw dependent variable for the
C     default curve fit (straight line at average).

      Average = SimpleAverage(RawDepVPtr, NumRawPts)
C
C     Step through all the points along the curve and set the 
C     DepcurveValues to the Average at each IndCurveValue
   
      Do 100 IPoint=1,NumCurvePts
        IndCurveValues(IPoint) = (IPoint-1)*Delta + IndVarMin
        DepCurveValues(IPoint) = Average
  100 CONTINUE

C     END OF DEFAULT CURVE FIT

      XYDataPointsCallback = IsOK
 
      Call TecUtilLockFinish(AddOnID)

      Return
      End

$$IF(AW_USEPROBECB)

      INTEGER*4 Function  ProbeValueCallback(RawIndVPtr,
     &                                       RawDepVPtr,
     &                                       IndVCoordScale,
     &                                       DepVCoordScale,
     &                                       NumRawPts,
     &                                       NumCurvePts,
     &                                       XYMapNum,
     &                                       CurveSettings,
     &                                       ProbeIndValue,
     &                                       ProbeDepValue)
      INCLUDE 'ADDGLBL.INC'
      Pointer       (RawIndVPtr,RawIndVUnused)
      INTEGER*4     RawIndVUnused
      Pointer       (RawDepVPtr,RawDepVUnused)
      INTEGER*4     RawDepVUnused
      INTEGER*4     IndVCoordScale
      INTEGER*4     DepVCoordScale
      INTEGER*4     NumRawPts
      INTEGER*4     NumCurvePts
      INTEGER*4     XYMapNum
      CHARACTER*(*) CurveSettings
      Real*8        ProbeIndValue
      Real*8        ProbeDepValue


      Real*8    SimpleAverage
      INTEGER*4 IsOK

      IsOK = 1

      Call TecUtilLockStart(AddOnID)

      ProbeValueCallback = IsOK
C   
C     If this function is not registered with Tecplot, a linear
C     interpolation will be performed, based on the values returned
C     be XYDataPointsCallback, in order to display a probe value.
C    

C    
C     BEGINNING OF DEFAULT CURVE FIT
C    
C     The rest of the function, between here and the END OF DEFAULT
C     CURVE FIT comment is the default curve fit (straight line at
C     the average of the raw dependent variable). Replace with your
C     own code for computing a curve fit value for the dependent 
C     variable.
C     

C   
C     Find the average value of the raw dependent variable for the
C     default curve fit (straight line at average).
C    
      ProbeDepValue = SimpleAverage(RawDepVPtr, NumRawPts)

C     END OF DEFAULT CURVE FIT

      Call TecUtilLockFinish(AddOnID)

      Return
      End
$$ENDIF

      INTEGER*4 Function  CurveInfoStringCallback(RawIndVPtr,
     &                                            RawDepVPtr,
     &                                            IndVCoordScale,
     &                                            DepVCoordScale,
     &                                            NumRawPts,
     &                                            XYMapNum,
     &                                            CurveSettings,
     &                                            CurveInfoString)
      INCLUDE 'ADDGLBL.INC'
      Pointer        (RawIndVPtr,RawIndVUnused)
      INTEGER*4      RawIndVUnused
      Pointer        (RawDepVPtr,RawDepVUnused)
      INTEGER*4      RawDepVUnused
      INTEGER*4      IndVCoordScale
      INTEGER*4      DepVCoordScale
      INTEGER*4      NumRawPts
      INTEGER*4      NumCurvePts
      INTEGER*4      XYMapNum
      CHARACTER*(*)  CurveSettings
      CHARACTER*2096 CurveInfoString

 
      Real*8    Average
      Real*8    SimpleAverage
      INTEGER*4 IsOK

      IsOK = 1

      Call TecUtilLockStart(AddOnID)
C
C     If this function is not registered with Tecplot, no curve
C     information will be displayed in the XY-Curve Info dialog.      
C

C
C     BEGINNING OF DEFAULT CURVE FIT
C
C     The rest of the function, between here and the END OF DEFAULT
C     CURVE FIT comment, is the information for the default curve
C     fit (straight line at the average of the raw dependent variable).
C     Replace with your own code for loading CurveInfoString.
C

      Average = SimpleAverage(RawDepVPtr, NumRawPts)
      WRITE(CurveInfoString,1000) Average
 1000 FORMAT(12HAverage is: ,1PE13.6)

C     END OF DEFAULT CURVE FIT

      CurveInfoStringCallback = IsOK

      Call TecUtilLockFinish(AddOnID)

      Return
      End



      REAL*8 Function  SimpleAverage(RawDepVPtr,
     &                               NumRawPts)
C
C     Compute the simple average that is used by the default 
C     curve fit.
C
      Pointer(RawDepVPtr,RawDepV)
      Real*8 Sum
      Real*8 TecUtilDataValueGetByRef

      Sum = 0.0
      DO 100 ii=1,NumRawPts
        Sum = Sum + TecUtilDataValueGetByRef(RawDepVPtr,ii)
  100 CONTINUE

      SimpleAverage = Sum/float(NumRawPts)

      Return
      End

$$ENDIF
