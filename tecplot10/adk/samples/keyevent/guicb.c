#include "TECADDON.h"
#include "ADDGLBL.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"





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
static void Dialog1CloseButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}


static Boolean_t KeyCallbacksInstalled = FALSE;

static void TextKeyEventCallback(const LgIndex_t *I)
{
  char S[50];
  sprintf(S,"TextKey: %d\n",*I);
  TecGUITextAppendString(KeyResult_T_D1,S);
}

static void TextFieldKeyEventCallback(const LgIndex_t *I)
{
  char S[50];
  sprintf(S,"TextFieldKey: %d\n",*I);
  TecGUITextAppendString(KeyResult_T_D1,S);
}



/**
 */
static void Dialog1Init_CB(void)
{
  TecUtilLockStart(AddOnID);
  if (!KeyCallbacksInstalled)
    {
       TecGUITextAddKeyEventCallback(TSrc_T_D1,TextKeyEventCallback);
       TecGUITextAddKeyEventCallback(TFSrc_TF_D1,TextFieldKeyEventCallback);
    }
  TecUtilLockFinish(AddOnID);
}


/**
 */
static LgIndex_t  KeyResult_T_D1_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Multi-Line Text field (KeyResult_T_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static LgIndex_t  TFSrc_TF_D1_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (TFSrc_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}


/**
 */
static LgIndex_t  TSrc_T_D1_CB(const char *S)
{
  LgIndex_t IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Multi-Line Text field (TSrc_T_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}




#include "guibld.c"
