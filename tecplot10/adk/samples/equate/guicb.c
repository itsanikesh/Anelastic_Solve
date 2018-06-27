#include "TECADDON.h"
extern AddOn_pa AddOnID;

#include "ADDGLBL.h"
#if !defined (MSWIN)
# include <unistd.h>
#endif
#include "GUIDEFS.h"

/* DOCSTART:gr/equate_DEFAULT_MULNUM.txt */
/* This is a string because it is put in a dialog text field */
#define DEFAULT_MULNUM "2"
/* DOCEND */

extern void Compute(double MulNum);

/**
 */
/* DOCSTART:gr/equate_Dialog1HelpButton_CB.txt */
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilHelp("equate.html",FALSE,0);
  TecUtilLockFinish(AddOnID);
}
/* DOCEND */

/**
 */
static void Dialog1CloseButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

/**
 */
/* DOCSTART:gr/equate_Dialog1Init_CB.txt */
static void Dialog1Init_CB(void)
{
  TecUtilLockStart(AddOnID);

  /*<<< Add init code (if necessary) here>>>*/
  TecGUITextFieldSetString(MulNum_TF_D1,DEFAULT_MULNUM);
  TecUtilLockFinish(AddOnID);
}
/* DOCEND */

/**
 */
static int MulNum_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TecUtilLockFinish(AddOnID);
  return IsOk;
}

/**
 */
/* DOCSTART:gr/equate_Compute_BTN_D1_CB_Pre.txt */
static void Compute_BTN_D1_CB(void)
{
  char *strMulNum = NULL;

  TecUtilLockStart(AddOnID);
  strMulNum = TecGUITextFieldGetString(MulNum_TF_D1);

  if (TecUtilDataSetIsAvailable())
    {
      Compute(atof(strMulNum));
/* DOCEND */
/* DOCSTART:gr/equate_Compute_BTN_D1_CB_Recording.txt */
      if (TecUtilMacroIsRecordingActive())
        TecUtilMacroRecordAddOnCommand("equate", strMulNum);
/* DOCEND */
/* DOCSTART:gr/equate_Compute_BTN_D1_CB_Post.txt */
    }
  else
    TecUtilDialogErrMsg("No data set available.");

  TecUtilStringDealloc(&strMulNum);

  TecUtilLockFinish(AddOnID);
}
/* DOCEND */


#include "guibld.c"
