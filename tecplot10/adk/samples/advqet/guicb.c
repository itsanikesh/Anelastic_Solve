/*
*****************************************************************
*****************************************************************
*******                                                  ********
****** (C) Copyright 1989-1998  by AMTEC ENGINEERING INC.********
*******       All Rights Reserved.                       ********
*******                                                  ********
*****************************************************************
*****************************************************************
*/

#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUI.h"
#include "GUIDEFS.h"
#include "ADVQET.h"


extern AddOn_pa AddOnID;
static int  SelectedZoneOrMap = 0;
static char *TextBoxMarginString = NULL;
static char *TextLineSpacingString = NULL;


static void PerformRecordingCheck(void)
{
  static Boolean_t HaveWarned = FALSE;

  if (!HaveWarned && TecUtilMacroIsRecordingActive())
    {
      HaveWarned = TRUE;
      TecUtilDialogMessageBox("The Advanced Quick Edit Tool does not "
                              "currently support macro recording.\n"
                              "\n"
                              "Any changes made to text or geometries "
                              "with the tool will not be represented "
                              "in your macro file.",
                              MessageBox_Warning);
    }
}


/*
 * Main Advanced Quick Edit dialog callback functions.
 */
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilHelp("tecplot/addon_advanced_quick_edit.htm", FALSE, 0);
  TecUtilLockFinish(AddOnID);
}


static void Dialog1CloseButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}


static void Dialog1Init_CB(void)
{
  TecUtilLockStart(AddOnID);

  /* update the main dialog with current settings and set up the */
  /* control sensitivities based on what is currently selected   */
  UpdateMainDialog();
  MainDialogSetSensitivities();

  TecUtilLockFinish(AddOnID);
}


static void GeomFPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);
  PerformRecordingCheck();
  PickedGeomChangeCoordSys(CoordSys_Frame);
  TecUtilLockFinish(AddOnID);
}


static void GeomGPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedGeomChangeCoordSys(CoordSys_Grid);
  TecUtilLockFinish(AddOnID);
}


static void TextFFPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeCoordSysAndSizeUnits(CoordSys_Frame, Units_Frame);
  TecUtilLockFinish(AddOnID);
}


static void TextFPPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeCoordSysAndSizeUnits(CoordSys_Frame, Units_Point);
  TecUtilLockFinish(AddOnID);
}


static void TextGGPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeCoordSysAndSizeUnits(CoordSys_Grid, Units_Grid);
  TecUtilLockFinish(AddOnID);
}


static void TextGFPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeCoordSysAndSizeUnits(CoordSys_Grid, Units_Frame);
  TecUtilLockFinish(AddOnID);
}


static void TextBoxMargi_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);
  PerformRecordingCheck();
  BuildDialog3(Dialog1Manager);
  TecGUIDialogLaunch(Dialog3Manager);
  TecUtilLockFinish(AddOnID);
}


static void TextLineSpac_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);
  PerformRecordingCheck();
  BuildDialog4(Dialog1Manager);
  TecGUIDialogLaunch(Dialog4Manager);
  TecUtilLockFinish(AddOnID);
}


static void TextHLPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeAnchor(TextAnchor_HeadLeft);
  TecUtilLockFinish(AddOnID);
}


static void TextHCPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeAnchor(TextAnchor_HeadCenter);
  TecUtilLockFinish(AddOnID);
}


static void TextHRPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeAnchor(TextAnchor_HeadRight);
  TecUtilLockFinish(AddOnID);
}


static void TextMLPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeAnchor(TextAnchor_MidLeft);
  TecUtilLockFinish(AddOnID);
}


static void TextMCPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeAnchor(TextAnchor_MidCenter);
  TecUtilLockFinish(AddOnID);
}


static void TextMRPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeAnchor(TextAnchor_MidRight);
  TecUtilLockFinish(AddOnID);
}


static void TextBLPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeAnchor(TextAnchor_Left);
  TecUtilLockFinish(AddOnID);
}


static void TextBCPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeAnchor(TextAnchor_Center);
  TecUtilLockFinish(AddOnID);
}


static void TextBRPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedTextChangeAnchor(TextAnchor_Right);
  TecUtilLockFinish(AddOnID);
}


static void ScopeLPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedObjectChangeScope(Scope_Local);
  TecUtilLockFinish(AddOnID);
}


static void ScopeGPB_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);                 
  PerformRecordingCheck();
  PickedObjectChangeScope(Scope_Global);
  TecUtilLockFinish(AddOnID);
}


static void Select_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);
  BuildDialog2(Dialog1Manager);
  TecGUIDialogLaunch(Dialog2Manager);
  TecUtilLockFinish(AddOnID);
}


/*
 * Attachment Selection dialog callback functions.
 */
static void Dialog2HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilHelp("tecplot/addon_advanced_quick_edit.htm", FALSE, 0);
  TecUtilLockFinish(AddOnID);
}


static void Dialog2CancelButton_CB(void)
{
  TecGUIDialogDrop(Dialog2Manager);

  /* NOTE: modal dialogs lock Tecplot at dialog initialization */
  TecUtilLockFinish(AddOnID);
}


static void Dialog2OkButton_CB(void)
{
  if (SelectedZoneOrMap >= 1)
    {
      PickedListChangeZoneOrMapAttachment(SelectedZoneOrMap);
    }

  TecGUIDialogDrop(Dialog2Manager);

  /* NOTE: modal dialogs lock Tecplot at dialog initialization */
  TecUtilLockFinish(AddOnID);
}


static void Dialog2Init_CB(void)
{
  SelectedZoneOrMap = 0;
  UpdateAttachmentDialog();

  /* NOTE: modal dialogs unlock Tecplot at the OK or Cancel button press */
  TecUtilLockStart(AddOnID);
}


static void ZoneOrMapLi_SLST_D2_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  SelectedZoneOrMap = *I;
  TecUtilLockFinish(AddOnID);
}


/*
 * Enter Text Box Margin dialog callback functions.
 */
static void Dialog3HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilHelp("tecplot/addon_advanced_quick_edit.htm", FALSE, 0);
  TecUtilLockFinish(AddOnID);
}


static void Dialog3CancelButton_CB(void)
{
  if (TextBoxMarginString != NULL)
    {
      free(TextBoxMarginString);
      TextBoxMarginString = NULL;
    }

  TecGUIDialogDrop(Dialog3Manager);

  /* NOTE: modal dialogs lock Tecplot at dialog initialization */
  TecUtilLockFinish(AddOnID);
}


static void Dialog3OkButton_CB(void)
{
  if (TextBoxMarginString != NULL)
    {
      PickedTextChangeMargin(TextBoxMarginString);
      free(TextBoxMarginString);
      TextBoxMarginString = NULL;
    }

  TecGUIDialogDrop(Dialog3Manager);

  /* NOTE: modal dialogs lock Tecplot at dialog initialization */
  TecUtilLockFinish(AddOnID);
}


static void Dialog3Init_CB(void)
{
  /* NOTE: modal dialogs lock Tecplot at dialog initialization */
  TecUtilLockStart(AddOnID);

  TextBoxMarginString = NULL;
  TecGUITextFieldSetString(TextBoxMargin_TF_D3, "");
}


static int  TextBoxMargin_TF_D3_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  if (TextBoxMarginString != NULL)
    free(TextBoxMarginString);
  TextBoxMarginString = StringDup(S);
  TecUtilLockFinish(AddOnID);
  return IsOk;
}


/*
 * Enter Text Line Spacing dialog callback functions.
 */
static void Dialog4HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilHelp("tecplot/addon_advanced_quick_edit.htm", FALSE, 0);
  TecUtilLockFinish(AddOnID);
}


static void Dialog4CancelButton_CB(void)
{
  if (TextLineSpacingString != NULL)
    {
      free(TextLineSpacingString);
      TextLineSpacingString = NULL;
    }

  TecGUIDialogDrop(Dialog4Manager);

  /* NOTE: modal dialogs lock Tecplot at dialog initialization */
  TecUtilLockFinish(AddOnID);
}


static void Dialog4OkButton_CB(void)
{
  if (TextLineSpacingString != NULL)
    {
      PickedTextChangeLineSpacing(TextLineSpacingString);
      free(TextLineSpacingString);
      TextLineSpacingString = NULL;
    }

  TecGUIDialogDrop(Dialog4Manager);

  /* NOTE: modal dialogs lock Tecplot at dialog initialization */
  TecUtilLockFinish(AddOnID);
}


static void Dialog4Init_CB(void)
{
  /* NOTE: modal dialogs lock Tecplot at dialog initialization */
  TecUtilLockStart(AddOnID);

  TextLineSpacingString = NULL;
  TecGUITextFieldSetString(TextLineSpaci_TF_D4, "");
}


static int  TextLineSpaci_TF_D4_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  if (TextLineSpacingString != NULL)
    free(TextLineSpacingString);
  TextLineSpacingString = StringDup(S);
  TecUtilLockFinish(AddOnID);
  return IsOk;
}


#include "guibld.c"
