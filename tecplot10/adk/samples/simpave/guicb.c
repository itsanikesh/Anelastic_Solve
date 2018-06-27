#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include "ENGINE.h"
#include "SUTIL.h"

extern GlobalCurve_s GlobalCurve;

/**
 */
/* DOCSTART:gr/simpavg_UpdateMainDialogSensitivities.txt */
static void UpdateMainDialogSensitivities(void)
{
  Boolean_t Sensitive = TecGUIToggleGet(UseIndVarRan_TOG_D1);
  TecGUISetSensitivity(IndVarMin_TF_D1, Sensitive);
  TecGUISetSensitivity(IndVarMax_TF_D1, Sensitive);
  TecGUISetSensitivity(Min_LBL_D1,      Sensitive);
  TecGUISetSensitivity(Max_LBL_D1,      Sensitive);
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/simpavg_InitializeGUICurveParams.txt */
static void InitializeGUICurveParams(CurveParams_s *CurveParamsPtr)
{
  char          *CurveSettings = NULL;
  CurveParams_s OrigCurveParams;
  Boolean_t    UseIndVarRangeIsSame = TRUE;
  Boolean_t     IndVarMinIsSame = TRUE;
  Boolean_t     IndVarMaxIsSame = TRUE;
  int           ii;
  int           NumMembers;

  /* Get the CurveParams associated with the first mapping. */
  CurveSettings = TecUtilStringListGetString(GlobalCurve.XYMapSettings, 1);
  GetValuesFromCurveSettings(
                             (EntIndex_t)TecUtilSetGetNextMember(GlobalCurve.XYMapSet,TECUTILSETNOTMEMBER),
                             CurveSettings,
                             &OrigCurveParams);
  if (CurveSettings != NULL)
    TecUtilStringDealloc(&CurveSettings);

  NumMembers = TecUtilStringListGetCount(GlobalCurve.XYMapSettings);

  /*
   * Compare the value of the first mapping with all the other mappings.
   * This loop will not be done if there is only one mapping selected.
   */
  for (ii = 2; ii <= NumMembers; ii++)
    {
      CurveParams_s TmpParams;
      CurveSettings = TecUtilStringListGetString(GlobalCurve.XYMapSettings, ii);
      GetValuesFromCurveSettings(
                                 (EntIndex_t)TecUtilSetGetNextMember(GlobalCurve.XYMapSet, ii),
                                 CurveSettings,
                                 &TmpParams);
      if (UseIndVarRangeIsSame)
        UseIndVarRangeIsSame = (TmpParams.UseIndVarRange == 
                                OrigCurveParams.UseIndVarRange);

      if (IndVarMinIsSame)
        IndVarMinIsSame = (TmpParams.IndVarMin == OrigCurveParams.IndVarMin);

      if (IndVarMaxIsSame)
        IndVarMaxIsSame = (TmpParams.IndVarMax == OrigCurveParams.IndVarMax);

      if (CurveSettings != NULL)
        TecUtilStringDealloc(&CurveSettings);
    }

  /*
   * Initialize the CurveParamsPtr to the default values.
   * If all mappings have the same value for a particular parameter,
   * use that value instead. 
   */
  InitializeCurveParams(CurveParamsPtr);

  if (UseIndVarRangeIsSame)
    CurveParamsPtr->UseIndVarRange = OrigCurveParams.UseIndVarRange;
  if (IndVarMinIsSame)
    CurveParamsPtr->IndVarMin = OrigCurveParams.IndVarMin;
  if (IndVarMaxIsSame)
    CurveParamsPtr->IndVarMax = OrigCurveParams.IndVarMax;                                              
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/simpavg_UpdateMainDialog.txt */
static void UpdateMainDialog(void)
{
  CurveParams_s CurveParams;
  InitializeGUICurveParams(&CurveParams);
  TecGUIToggleSet(UseIndVarRan_TOG_D1,CurveParams.UseIndVarRange);
  TecGUITextFieldSetDouble(IndVarMin_TF_D1,CurveParams.IndVarMin,"%G");
  TecGUITextFieldSetDouble(IndVarMax_TF_D1,CurveParams.IndVarMax,"%G");
  UpdateMainDialogSensitivities();
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/simpavg_Dialog1Init_CB.txt */
static void Dialog1Init_CB(void)
{
  /* TODO: Add any initialization here. */
  TecUtilLockStart(AddOnID);
  UpdateMainDialog();
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/simpavg_AssignCurveParams.txt */
static void AssignCurveParams(CurveParams_s *CurveParams)
{
  CurveParams->UseIndVarRange = TecGUIToggleGet(UseIndVarRan_TOG_D1);
  /*
   * Note this function returns a boolean alerting user whether or not
   * input value is legitimate.  Some error checking may be added here. 
   */
  TecGUITextFieldGetDouble(IndVarMin_TF_D1,&CurveParams->IndVarMin);
  TecGUITextFieldGetDouble(IndVarMax_TF_D1,&CurveParams->IndVarMax);
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/simpavg_Dialog1OkButton_CB.txt */
static void Dialog1OkButton_CB(void)
{
  /* Only unlock tecplot here because a modal dialog was launched. */

  /* When curve settings change, Tecplot must be informed of the change. */

  char *CurveSettings = NULL;
  CurveParams_s CurveParams;

  /* Assign the new curve parameters from the dialog settings. */
  AssignCurveParams(&CurveParams);

  /* Create the Curve Settings string from the new curve parameters. */
  CurveSettings = CreateCurveSettingsString(CurveParams);
  if (CurveSettings != NULL)
    {
      EntIndex_t  Map;
      TecUtilSetForEachMember(Map, GlobalCurve.XYMapSet)
        {
          TecUtilCurveSetExtendedSettings(Map, CurveSettings);
        }
      TecUtilStringDealloc(&CurveSettings);
    }

  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}
/* DOCEND */

/**
 */
static void Dialog1CancelButton_CB(void)
{
  /* Only unlock tecplot here because a modal dialog was launched. */
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}

/**
 */
static int  IndVarMin_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (IndVarMin_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return IsOk;
}

/**
 */
static int  IndVarMax_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (IndVarMax_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return IsOk;
}

/**
 */
/* DOCSTART:gr/simpavg_UseIndVarRan_TOG_D1_CB.txt */
static void UseIndVarRan_TOG_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  /*
   * Make sure to update the sensitivities when
   * the toggle button is pressed.
   */
  UpdateMainDialogSensitivities();
  TecUtilLockFinish(AddOnID);
}
/* DOCEND */


#include "guibld.c"
