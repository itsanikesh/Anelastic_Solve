#include "TECADDON.h"
extern AddOn_pa AddOnID;

#include "ADDGLBL.h"
#include "GUIDEFS.h"

/* DOCSTART:gr/loadtxt_LoadTxtGlobalState_extern.txt */
extern LoadTxtGlobalState_s GS;
/* DOCEND */

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
static void Dialog1CancelButton_CB(void)
{
  /* Only unlock tecplot here because a modal dialog was launched. */
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

/**
 */
/* DOCSTART:gr/loadtxt_Dialog1OkButton_CB.txt */
static void Dialog1OkButton_CB(void)
{
  char *FileName;

  FileName = TecGUITextFieldGetString(FileName_TF_D1);

  /* check that the filename is valid */
  if ((FileName != NULL && strlen(FileName) > 0) &&
      DoLoadDelimitedText(FileName,GS.Skip))
    {
      /* Save the filename for next time */
      strcpy(GS.FileName,FileName);
      TecGUIDialogDrop(Dialog1Manager);
      TecUtilLockFinish(AddOnID);
    }

  if (FileName != NULL)
    {
      TecUtilLockStart(AddOnID);
      TecUtilStringDealloc(&FileName);
      TecUtilLockFinish(AddOnID);
    }
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/loadtxt_Dialog1Init_CB.txt */
static void Dialog1Init_CB(void)
{
  char StrSkip[32];
  TecUtilLockStart(AddOnID);

  sprintf(StrSkip,"%d",GS.Skip);

  TecGUITextFieldSetString(Skip_TF_D1,StrSkip);
  TecGUITextFieldSetString(FileName_TF_D1,GS.FileName); 
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/loadtxt_Skip_TF_D1_CB.txt */
static int  Skip_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  int Value;

  TecUtilLockStart(AddOnID);

  /* 
   * We could be more elaborate here and check for non-numeric
   * digits, but for now just check that the string converts to
   * an integer >= 1
   */
  Value = atoi(S);
  if (Value < 1)
    {
      TecUtilDialogErrMsg("Skip parameter must be greater than "
                          "or equal to 1.");
      IsOk = 0;
    }

  if (IsOk)
    GS.Skip = Value; /* Remember for next time */

  TecUtilLockFinish(AddOnID);

  return IsOk;
}
/* DOCEND */

/**
 */
static int  FileName_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (FileName_TF_D1) Value Changed, New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
/* DOCSTART:gr/loadtxt_Browse_BTN_D1_CB.txt */
static void Browse_BTN_D1_CB(void)
{
  char *SelectedFileName  = NULL;
  char *Type              = "Delimited Text";
  char *Filter            = "*.txt";

  TecUtilLockStart(AddOnID);

  if (TecUtilDialogGetFileName(SelectFileOption_ReadSingleFile,
                               &SelectedFileName,Type,GS.FileName,Filter))
    {
      strcpy(GS.FileName,SelectedFileName);
      TecUtilStringDealloc(&SelectedFileName);
      TecGUITextFieldSetString(FileName_TF_D1,GS.FileName);
    }

  TecUtilLockFinish(AddOnID);
}
/* DOCEND */


#include "guibld.c"

