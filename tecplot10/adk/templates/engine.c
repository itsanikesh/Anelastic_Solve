$$IF(MFCDLL)
#include "stdafx.h"
$$ENDIF
#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
$$IF(AW_USETGB)
$$IF(!MFCDLL)
#include "GUIDEFS.h"
$$ENDIF
$$ENDIF
$$IF(!AW_GENERAL)
#include "ENGINE.h"
$$ENDIF

$$IF(AW_LOADER)
Boolean_t STDCALL LoaderCallback(StringList_pa Instructions) /* IN */
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF

  Boolean_t IsOk = TRUE;
  LgIndex_t Count;
  LgIndex_t i;
  char      *Ins = NULL;
  /*
   *  Tecplot will call this function any time it needs to load 
   *  data using this custom loader. 
   *
   *    IMPORTANT: After the data has been successfully loaded,
   *    be sure to call TecUtilImportSetLoaderInstr(). This will allow
   *    the user to save a layout file which references this 
   *    custom loader.
   */

  TecUtilLockStart(AddOnID);
  
  Count = TecUtilStringListGetCount(Instructions);
  for (i=1;i<=Count;i++)
    {
      Ins = TecUtilStringListGetString(Instructions,i);
      /* The instruction is now in the string variable 'Ins' */
      TecUtilStringDealloc(&Ins); /* Deallocate when finished with it */
    }

  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

void STDCALL LoaderSelectedCallback(void)
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF
  /*
   * This function is called when
   * the user selects this dataset
   * loader from the list of importers
   * in the File/Import dialog.
   *
   * TODO:
   *
   * 1. Launch a dialog which collects settings
   *    for how the file should be loaded.
   *    (filename, skip, etc.)
   *
   * 2. In the OK callback of the dialog, close the dialog
   *    and call a function to load the data using the
   *    indicated settings. Note: you may want to use
   *    the same function to load the data from the
   *    LoaderCallback() function above.
   *
   * 3. IMPORTANT: After the data has been successfully loaded,
   *    be sure to call TecUtilImportSetLoaderInstr(). This will allow
   *    the user to save a layout file which references this 
   *    custom loader.
   */
  TecUtilLockStart(AddOnID);
$$IF(!MFCDLL)
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
$$ENDIF
$$IF(MFCDLL)

  // TODO: Launch a modal dialog here which promps the user for loader parameters
  TecUtilDialogMessageBox("Loader dialog under construction...",MessageBox_Information);
$$ENDIF
  TecUtilLockFinish(AddOnID);
}

$$IF(AW_INCLUDEOVERRIDE)
Boolean_t STDCALL InstructionOverrideCallback(StringList_pa Instructions)
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF

  Boolean_t IsOk = TRUE;
  /*
   * TODO:
   * Add code to view the current instructions and display
   * an interface which allows the user the user to change them.
   */
  return (IsOk);
}
$$ENDIF
$$ENDIF

$$IF(AW_CONVERTER)

$$IF(AW_CONVERTERPLUS)

$$IF(AW_USETGB)
GlobalConvPlus_s GlobalConvPlus;
$$ENDIF


/*
 * This function will alter the data.
 */
Boolean_t Action_Alter(char *Multiplier)
{
  Boolean_t IsOk = TRUE;
  char      AlterString[200];
  Set_pa    EnabledZones;

  REQUIRE(VALID_REF(Multiplier));

  
  /*
   * Operate on all the zones.  If we were smarter with our instruction
   * string we could construct a set of zones to operate on that is only
   * the zones that are being read in.
   */
  sprintf(AlterString, "{x}={x}*%s", Multiplier);
  TecUtilZoneGetEnabled(&EnabledZones);

  /*
   * Make sure to tell Tecplot to suspend the marking of
   * the data during the data alter event.  If the data is
   * altered outside of the TecUtilDataSetSuspendMarking()
   * calls, the data will be marked as dirty, and a new data
   * file will need to be saved when saving a layout.
   */
  TecUtilDataSetSuspendMarking(TRUE);

  IsOk = TecUtilDataAlter(AlterString,
                          EnabledZones,
                          1,0,1, /* All I-Values */
                          1,0,1, /* All J-Values */
                          1,0,1, /* All K-Values */
                          FieldDataType_Double);
  /*
   * Now that the data alter event is done, we need to let
   * Tecplot mark the data during subsequent events.
   */  
  TecUtilDataSetSuspendMarking(FALSE);

  TecUtilSetDealloc(&EnabledZones);  
  
  if ( IsOk )
    {
      /*
       * This is a rudamentary instruction string.
       * We are simply writing out the value that we are
       * to multiply by.  Unlike macro commands, we need to
       * inform Tecplot of this string every time the alteration
       * is successful, even if we are not recording.  This is
       * so the command can be written out in a layout if need be.
       * If a macro is being recorded, Tecplot knows to record the
       * instruction for you.
       */
      TecUtilDataSetAddJournalCommand(ADDON_NAME,
                                      Multiplier,
                                      NULL);      
    }  
  ENSURE(VALID_BOOLEAN(IsOk));
  return IsOk;
}


/*
 * This function is called when processing a macro command or a layout.
 */
Boolean_t STDCALL ProcessConvertMacroCommand(char  *CommandString,
                                             char **ErrMsg)
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF
  Boolean_t IsOk = TRUE;
  TecUtilLockStart(AddOnID);
  /*
   * Remember, the command string that we registered is simply a string
   * representation of the value the user typed into the text field.  We
   * can simply pass this string to out Action_Alter() function.
   */
  IsOk = Action_Alter(CommandString);
  if ( !IsOk )
    {
      *ErrMsg = TecUtilStringAlloc(80, "Error Message");
      strcpy(*ErrMsg, "An error occured while altering the data.");
    }
  TecUtilLockFinish(AddOnID);
  ENSURE(VALID_BOOLEAN(IsOk));
  return IsOk;
}


/*
 * The post convert callback is what presents the dialog that
 * asks for user input.  If there is no dialog, this function
 * simply perfoms the post load action.
 *
 * In this case we prompt the user for a value by which to multiply
 * the 'X' variable by.
 */
void STDCALL PostConvertCallback(const char   *PreviousInstructions,
                                 const char   *PreviousRawData,
                                 const Set_pa PreviousZones)
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF
$$IF(AW_USETGB)
  TecUtilLockStart(AddOnID);
  /* 
   * Save off Previous data for use in the 
   * functions in guicb.c
   */
  GlobalConvPlus.PreviousInstructions = PreviousInstructions;
  GlobalConvPlus.PreviousRawData      = PreviousRawData;
  GlobalConvPlus.PreviousZones        = PreviousZones;  

  /* Build and Launch the dialog */
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
$$ENDIF
$$IF(!AW_USETGB)
  Boolean_t IsOk = TRUE;
  char      *Multiplier;

  TecUtilLockStart(AddOnID);

  /*
   * Prompt the user for a value by which to multiply the 'X' variable by.
   */
  if ( TecUtilDialogGetSimpleText("Enter the value to multiply 'X' by.",
                                  "1",
                                  &Multiplier) )
    {
      IsOk = Action_Alter(Multiplier);
      TecUtilStringDealloc(&Multiplier);
    }

  if ( !IsOk )
    {
      TecUtilDialogErrMsg("An error occured while altering the data.");
    }

  /*
   * This is required to be called after the user input and the calculations
   * have been made.  It must not be called from the macro process function.
   * This tells Tecplot that we are done performing our post load actions.
   */  
  TecUtilDataSetPostReadFinished(IsOk);

  TecUtilLockFinish(AddOnID);  
$$ENDIF
}
$$ENDIF


Boolean_t STDCALL ConverterCallback(char *DataFName,      /* IN */
                                    char *TempBinFName,   /* IN */
                                    char **MessageString) /* OUT (if and only if returning FALSE) */
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF

  Boolean_t IsOk = TRUE;

  TecUtilLockStart(AddOnID);
  /*
   * Add code here to open DataFName,
   * read in the data and write out a binary
   * Tecplot datafile to TempBinFName using
   * calls to TecUtilTecxxx functions. If there is
   * a problem, call TecUtilStringAlloc() on
   * MessageString, supply a message describing the
   * problem, and set IsOk to FALSE.
   */

  /*
   * Remove next call when finished...
   */
  TecUtilDialogMessageBox("$$AW_ADDONNAME$$ data converter currently under construction....",
                          MessageBox_Information);
  
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}
$$ENDIF


$$IF(AW_CURVEFIT)
$$IF(AW_USETGB)

GlobalCurve_s GlobalCurve;

void STDCALL CurveSettingsCallback(Set_pa        XYMapSet,      
                                   StringList_pa XYMapSettings) 
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF

  TecUtilLockStart(AddOnID);
  /* 
   * Save off XYMapSettings and SelectedXYMaps for use
   * in the functions in guicb.c
   */
  GlobalCurve.XYMapSet      = XYMapSet;
  GlobalCurve.XYMapSettings = XYMapSettings;

  /* Build and Launch the dialog */
$$IF(!MFCDLL)
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager); 
$$ENDIF
$$IF(MFCDLL)
  // TODO: Launch a dialog here
  // It is strongly recommend that curve-fit add-ons use Modal,
  // rather than modeless dialogs for curve fit options

  TecUtilDialogMessageBox("Curve fit settings modal dialog currently under construction...",
                           MessageBox_Information);
$$ENDIF

  TecUtilLockFinish(AddOnID);
}


void STDCALL AbbreviatedSettingsStringCallback(EntIndex_t XYMapNum,             
                                               char      *CurveSettings,        
                                               char     **AbbreviatedSettings)  
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF

  TecUtilLockStart(AddOnID);
  /*
   * Create a string based on your settings.  The first
   * time Tecplot calls this function, CurveSettings
   * will be NULL, so NewSettings must be constructed from
   * default values.  AbbreviatedSettings is the string
   * that will be presented to the Plot Attributes dialog.
   */
  TecUtilLockFinish(AddOnID);
}
$$ENDIF

/*
 * Function to compute the average of the raw dependent
 * variable for the default fit (straight line at average). 
 * REMOVE THIS FUNCTION FOR OTHER FITS.
 */
double SimpleAverage(FieldData_pa RawDepV,    
                     LgIndex_t    NumRawPts)  
{
  int    ii;
  int    Count   = 0;
  double Sum     = 0;

  for (ii = 0; ii < NumRawPts; ii++)
    {
      Sum += TecUtilDataValueGetByRef(RawDepV, ii+1);
      Count++;
    }

  return (Sum/Count);
}


Boolean_t STDCALL XYDataPointsCallback(FieldData_pa RawIndV,        
                                       FieldData_pa RawDepV,        
                                       CoordScale_e IndVCoordScale, 
                                       CoordScale_e DepVCoordScale, 
                                       LgIndex_t    NumRawPts,      
                                       LgIndex_t    NumCurvePts,    
                                       EntIndex_t   XYMapNum,       
                                       char        *CurveSettings,  
                                       double      *IndCurveValues, 
                                       double      *DepCurveValues) 
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF

  Boolean_t IsOk  = TRUE;

  /* Variables for default curve fit */
  int       ii;
  double    Average;
  double    Delta = 0.0;
  double    IndVarMin,
            IndVarMax;

  TecUtilLockStart(AddOnID);
  /*
   * This function must be registered with Tecplot.  IndCurveValues
   * and DepCurveValues will be allocated for you and will be of 
   * size NumCurvePts.
   */

  /*
   * BEGINNING OF DEFAULT CURVE FIT
   *
   * The rest of the function, between here and the END OF DEFAULT
   * CURVE FIT comment is the default curve fit (straight line at
   * the average of the raw dependent variable). Replace with your
   * own code for computing and populating IndCurveValues and 
   * DepCurveValues.
   */

  /*
   * Get the min and max values of the independent variable and
   * use these values to calculate the size of each step along
   * the independent axis for the curve fit.
   */
  TecUtilDataValueGetMinMaxByRef(RawIndV,
                                 &IndVarMin,
                                 &IndVarMax);

  Delta = (IndVarMax-IndVarMin)/(NumCurvePts-1);

  /*
   * Find the average value of the raw dependent variable for the
   * default curve fit (straight line at average).
   */
  Average = SimpleAverage(RawDepV, NumRawPts);

  /*
   * Step through all the points along the curve and set the
   * DepCurveValues to the Average at each IntCurveValue.
   */
  for (ii = 0; ii < NumCurvePts; ii++)
    {
       IndCurveValues[ii] = ii*Delta + IndVarMin;
       DepCurveValues[ii] = Average;
    }

  /* END OF DEFAULT CURVE FIT */

  TecUtilLockFinish(AddOnID);
  return IsOk;
}



$$IF(AW_USEPROBECB)
Boolean_t STDCALL ProbeValueCallback(FieldData_pa RawIndV,
                                     FieldData_pa RawDepV,
                                     CoordScale_e IndVCoordScale,
                                     CoordScale_e DepVCoordScale,
                                     LgIndex_t    NumRawPts,
                                     LgIndex_t    NumCurvePts,
                                     EntIndex_t   XYMapNum,
                                     char        *CurveSettings,
                                     double       ProbeIndValue,
                                     double      *ProbeDepValue)
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF

  Boolean_t     IsOk = TRUE;
  TecUtilLockStart(AddOnID);
  /*
   * If this function is not registered with Tecplot, a linear
   * interpolation will be performed, based on the values returned
   * be XYDataPointsCallback, in order to display a probe value.
   */

  /*
   * BEGINNING OF DEFAULT CURVE FIT
   *
   * The rest of the function, between here and the END OF DEFAULT
   * CURVE FIT comment is the default curve fit (straight line at
   * the average of the raw dependent variable). Replace with your
   * own code for computing a curve fit value for the dependent 
   * variable.
   */

  /*
   * Find the average value of the raw dependent variable for the
   * default curve fit (straight line at average).
   */
  *ProbeDepValue = SimpleAverage(RawDepV, NumRawPts);

  /* END OF DEFAULT CURVE FIT */

  TecUtilLockFinish(AddOnID);
  return IsOk;
}
$$ENDIF



Boolean_t STDCALL CurveInfoStringCallback(FieldData_pa RawIndV,
                                          FieldData_pa RawDepV,
                                          CoordScale_e IndVCoordScale,
                                          CoordScale_e DepVCoordScale,
                                          LgIndex_t    NumRawPts,
                                          EntIndex_t   XYMapNum,
                                          char        *CurveSettings,
                                          char       **CurveInfoString)
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF

  Boolean_t     IsOk = TRUE;
  TecUtilLockStart(AddOnID);
  /*
   * If this function is not registered with Tecplot, no curve
   * information will be displayed in the XY-Curve Info dialog.
   */

  /*
   * BEGINNING OF DEFAULT CURVE FIT
   *
   * The rest of the function, between here and the END OF DEFAULT
   * CURVE FIT comment is the information for the default curve 
   * fit (straight line at the average of the raw dependent variable).
   * Replace with your own code for displaying curve fit info.
   */

  *CurveInfoString = TecUtilStringAlloc(30, "CurveInfoString");


  sprintf(*CurveInfoString, "Average is: %G\n",
                          SimpleAverage(RawDepV, NumRawPts));

  /* END OF DEFAULT CURVE FIT */

  TecUtilLockFinish(AddOnID);
  return IsOk;
}

$$ENDIF

