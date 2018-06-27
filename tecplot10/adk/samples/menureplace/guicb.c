#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"


static void Dialog2HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}




static void Dialog2CancelButton_CB(void)
{
  TecGUIDialogDrop(Dialog2Manager);
/*Modal Dialogs must call TecUtilLockStart prior to coming*/
/*up and then call TecUtilLockFinish when the Ok or Cancel*/
/*button is pressed.  Only TecUtilLockFinish is supplied here.*/
  TecUtilLockFinish(AddOnID);
}




static void Dialog2OkButton_CB(void)
{
  TecGUIDialogDrop(Dialog2Manager);
/*Modal Dialogs must call TecUtilLockStart prior to coming*/
/*up and then call TecUtilLockFinish when the Ok or Cancel*/
/*button is pressed.  Only TecUtilLockFinish is supplied here.*/
  TecUtilLockFinish(AddOnID);
}




static void Dialog2Init_CB(void)
{
/*Modal Dialogs must call TecUtilLockStart prior to coming*/
/*up and then call TecUtilLockFinish when the Ok or Cancel*/
/*button is pressed.*/
  TecUtilLockStart(AddOnID);
/*<<< Add init code (if necessary) here>>>*/
}




static void GoAhead_BTN_D2_CB(void)
{
  TecUtilLockStart(AddOnID);
  TRACE("Go Ahead Button Pushed\n");
  TecUtilLockFinish(AddOnID);
}




static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}

static void Dialog1CloseButton_CB(void)
{
  TecGUIDialogDrop(Dialog1Manager);
}


static void Dialog1Init_CB(void)
{
  /* TODO: Add any initialization here. */

}




#include "guibld.c"

