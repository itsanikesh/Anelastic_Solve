#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "GUIDEFS.h"
#include "ENGINE.h"
#include "SUTIL.h"


/**
 */
/* DOCSTART:gr/simpavg_GlobalCurve_def.txt */
GlobalCurve_s GlobalCurve;
/* DOCEND */

/**
 */
/* DOCSTART:gr/simpavg_CurveSettingsCallback.txt */
void STDCALL CurveSettingsCallback(Set_pa        XYMapSet,      
                                   StringList_pa XYMapSettings) 
{

  TecUtilLockStart(AddOnID);
  /* 
   * Save off XYMapSettings and SelectedXYMaps for use
   * in the functions in guicb.c
   */
  GlobalCurve.XYMapSet      = XYMapSet;
  GlobalCurve.XYMapSettings = XYMapSettings;

  /* Build and Launch the dialog */
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager); 

  TecUtilLockFinish(AddOnID);
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/simpavg_AbbreviatedSettingsStringCallback.txt */
void STDCALL AbbreviatedSettingsStringCallback(EntIndex_t XYMapNum,             
                                               char      *CurveSettings,        
                                               char     **AbbreviatedSettings)  
{
  CurveParams_s CurveParams;
  char          *S;

  TecUtilLockStart(AddOnID);
  GetValuesFromCurveSettings(XYMapNum,
                             CurveSettings,
                             &CurveParams);

  S = TecUtilStringAlloc(80, "Abbreviated Settings");

  if (CurveParams.UseIndVarRange)
    {
      sprintf(S,
              "IndVar Range: Min = %G; Max = %G",
              CurveParams.IndVarMin,
              CurveParams.IndVarMax);
      *AbbreviatedSettings = S;
    }
  else
    {
      strcpy(S, "No IndVarRange");
      *AbbreviatedSettings = S;
    }  
  TecUtilLockFinish(AddOnID);
}
/* DOCEND */

/* DOCSTART:gr/simpavg_SimpleAverage.txt */
/**
 * Function to compute the average of the raw dependent variable for the
 * default fit (straight line at average).
 *
 * REMOVE THIS FUNCTION FOR OTHER FITS.
 */
double SimpleAverage(FieldData_pa RawDepV,
                     FieldData_pa RawIndV,
                     LgIndex_t    NumRawPts,
                     double       IndVarMin,
                     double       IndVarMax)  
{
  int    ii;
  int    Count   = 0;
  double Sum     = 0;

  for (ii = 0; ii < NumRawPts; ii++)
    {
      double IndV = TecUtilDataValueGetByRef(RawIndV, ii+1);

      /* 
       * Only compute the average on values that fall in the
       * specified range of the independent variable. 
       */
      if ( IndV >= IndVarMin && IndV <= IndVarMax)
        { 
          Sum += TecUtilDataValueGetByRef(RawDepV, ii+1);
          Count++;
        }
    }

  return (Sum/Count);
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/simpavg_XYDataPointsCallback.txt */
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
  Boolean_t IsOk  = TRUE;

  int       ii;
  double    Average;
  double    Delta = 0.0;
  double    IndVarMin,
  IndVarMax;
  CurveParams_s CurveParams;

  TecUtilLockStart(AddOnID);

  /* Get the min and max values of the independent variable. */
  TecUtilDataValueGetMinMaxByRef(RawIndV,
                                 &IndVarMin,
                                 &IndVarMax);

  /* Get the curve parameters */
  GetValuesFromCurveSettings(XYMapNum,
                             CurveSettings,
                             &CurveParams);

  if (CurveParams.UseIndVarRange)
    {
      /* 
       * Adjust the independent variable range to fall either within
       * the range of data or the range specified by the 
       * CurveParams structure. 
       */
      IndVarMin = MAX(IndVarMin, CurveParams.IndVarMin);
      IndVarMax = MIN(IndVarMax, CurveParams.IndVarMax);
    }

  Delta = (IndVarMax-IndVarMin)/(NumCurvePts-1);

  /* 
   * Find the average value of the raw dependent variable for the
   * default curve fir (straight line at average).
   */
  Average = SimpleAverage(RawDepV,
                          RawIndV,
                          NumRawPts,
                          IndVarMin,
                          IndVarMax);

  /*
   * Step through all the points along the curve and set the
   * DepCurveValues to the Average at each IntCurveValue.
   */

  for (ii = 0; ii < NumCurvePts; ii++)
    {
      IndCurveValues[ii] = ii*Delta + IndVarMin;
      DepCurveValues[ii] = Average;
    }

  TecUtilLockFinish(AddOnID);
  return IsOk;
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/simpavg_CurveInfoStringCallback.txt */
Boolean_t STDCALL CurveInfoStringCallback(FieldData_pa RawIndV,
                                          FieldData_pa RawDepV,
                                          CoordScale_e IndVCoordScale,
                                          CoordScale_e DepVCoordScale,
                                          LgIndex_t    NumRawPts,
                                          EntIndex_t   XYMapNum,
                                          char        *CurveSettings,
                                          char       **CurveInfoString)
{
  Boolean_t     IsOk = TRUE;
  CurveParams_s CurveParams;
  double IndVarMin,IndVarMax;
  double Average;

  TecUtilLockStart(AddOnID);

  /*
   * If this function is not registered with Tecplot, no curve
   * information will be displayed in the XY-Curve Info dialog.
   */
  *CurveInfoString = TecUtilStringAlloc(30, "CurveInfoString");

  /* Get the curve parameters. */
  GetValuesFromCurveSettings(XYMapNum,CurveSettings,&CurveParams);

  if (CurveParams.UseIndVarRange)
    {
      /*
       * Adjust the Independent variable range to fall either within
       * the range of the data or the range specified by the
       * CurveParams structure.
       */
      IndVarMin = CurveParams.IndVarMin;  /* initialize these values */
      IndVarMax = CurveParams.IndVarMax;
      IndVarMin = MAX(IndVarMin, CurveParams.IndVarMin);
      IndVarMax = MIN(IndVarMax, CurveParams.IndVarMax);
    }

  Average = SimpleAverage(RawDepV,
                          RawIndV,
                          NumRawPts,
                          IndVarMin,
                          IndVarMax);

  sprintf(*CurveInfoString, "Average is: %G\n", Average);

  TecUtilLockFinish(AddOnID);
  return IsOk;
}
/* DOCEND */

/* DOCSTART:gr/simpavg_CreateCurveSettingsString.txt */
/**
 * Creates a CurveSettings string based on the values
 * in the CurveParams structure that is passed in.
 */
char *CreateCurveSettingsString(CurveParams_s CurveParams)
{
  char S[1000];
  char *CurveSettings;

  if (CurveParams.UseIndVarRange)
    strcpy(S,"UseIndVarRange = TRUE\n");
  else
    strcpy(S,"UseIndVarRange = FALSE\n");

  sprintf(&S[strlen(S)], "IndVarMin = %G\n", CurveParams.IndVarMin);
  sprintf(&S[strlen(S)], "IndVarMax = %G\n", CurveParams.IndVarMax);

  S[strlen(S)] = '\0';
  CurveSettings = TecUtilStringAlloc(strlen(S), "CurveSettings");
  strcpy(CurveSettings, S);
  return CurveSettings;
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/simpavg_InitializeCurveParams.txt */
void InitializeCurveParams(CurveParams_s *CurveParams)
{
  CurveParams->UseIndVarRange = FALSE;
  CurveParams->IndVarMin      = -LARGEDOUBLE;
  CurveParams->IndVarMax      = LARGEDOUBLE;
}
/* DOCEND */

/* DOCSTART:gr/simpavg_GetValuesFromCurveSettings.txt */
/**
 * This function makes use of functions found in the
 * adkutil.c module to parse the CurveSettings string.
 */
void GetValuesFromCurveSettings(EntIndex_t    XYMapNum,
                                char          *CurveSettings,
                                CurveParams_s *CurveParams)
{
  Boolean_t  IsOk = TRUE;
# define     MAXCHARS 50
  char       Command[MAXCHARS+1];
  char       ValueString[MAXCHARS+1];
  char      *CPtr;
  char      *ErrMsg = NULL;

  if (CurveSettings != NULL && strlen(CurveSettings) > 0)
    {
      CPtr = CurveSettings;
      while (IsOk && *CPtr)
        {
          if (GetArgPair(&CPtr,
                         Command,
                         ValueString,
                         MAXCHARS,
                         &ErrMsg))
            {
              if (Str_ustrcmp(Command, "USEINDVARRANGE") == 0)
                {
                  Boolean_t UseRange;
                  IsOk = Macro_GetBooleanArg(Command,
                                             ValueString,
                                             &UseRange,
                                             &ErrMsg);
                  if (IsOk)
                    CurveParams->UseIndVarRange = UseRange;
                }
              else if (Str_ustrcmp(Command, "INDVARMIN") == 0)
                {
                  double Min;
                  IsOk = Macro_GetDoubleArg(Command,
                                            ValueString,
                                            -LARGEDOUBLE,
                                            LARGEDOUBLE,
                                            &Min,
                                            &ErrMsg);
                  if (IsOk)
                    CurveParams->IndVarMin = Min;
                }
              else if (Str_ustrcmp(Command, "INDVARMAX") == 0)
                {
                  double Max;
                  IsOk = Macro_GetDoubleArg(Command,
                                            ValueString,
                                            -LARGEDOUBLE,
                                            LARGEDOUBLE,
                                            &Max,
                                            &ErrMsg);
                  if (IsOk)
                    CurveParams->IndVarMax = Max;
                }
              else
                {
                  ErrMsg = TecUtilStringAlloc((strlen(Command)+100),
                                              "error message");
                  sprintf(ErrMsg, "Unknown argument: %s.", Command);
                  IsOk = FALSE;
                }
            }
          else /* GetArgPair Failed. */
            IsOk = FALSE;
        }
    }
  else /* CurveSettings is an invalid string. */
    IsOk = FALSE;

  /* Repair the string.  Display the Error Message if needed. */
  if (!IsOk)
    {
      char *NewCurveSettings = NULL;
      InitializeCurveParams(CurveParams);
      NewCurveSettings = CreateCurveSettingsString(*CurveParams);

      if (NewCurveSettings != NULL)
        {
          TecUtilCurveSetExtendedSettings(XYMapNum,NewCurveSettings);
          TecUtilStringDealloc(&NewCurveSettings);
        }
      if (ErrMsg != NULL)
        {
          TecUtilDialogErrMsg(ErrMsg);
          TecUtilStringDealloc(&ErrMsg);
        }
    }
}
/* DOCEND */
