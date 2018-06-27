#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUI.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"
#include "UTIL.h"



extern AddOn_pa AddOnID;
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilHelp("tecplot/addon_circle_stream.htm",FALSE,0);
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
   TecGUIRadioBoxSetToggle(RadioButto_RADIO_D1,CurStreamDirection);
   TecUtilLockFinish(AddOnID);
}




static void AddStreamtra_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);
  GoStreamCircle();
  TecUtilLockFinish(AddOnID);
}


static void RadioButto_RADIO_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  CurStreamDirection = *I;
  TecUtilLockFinish(AddOnID);
}



#include "guibld.c"
