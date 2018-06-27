#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUI.h"
#if !defined(MSWIN)
#include <unistd.h>
#endif /* MSWIN */
#include "GUIDEFS.h"
#include "CRSFEZ.h"
#include "ADKUTIL.h"





extern AddOn_pa AddOnID;
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  
  /* TecUtilHelp("crsfez.html", FALSE, 0); */
  TecUtilDialogErrMsg("No help is available for this add-on");
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
  DialogSetSensitivities();
  TecUtilLockFinish(AddOnID);
}




static void Create_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);  
  if ( Data_AnyVariableIsCellCentered() )
    {
      TecUtilDialogErrMsg("One or more variables in this data set are cell centered.\n"
                          "This add-on does not currently support cell centered data");
    }
  else
    {
      if ( Data_AnyVariableIsShared() )
        {
          TecUtilDialogMessageBox("Warning: This add-on may not work correctly because one or more "
                                      "variables are shared.",MessageBox_Warning);
        }
      CreateSubFEZone();
    }
  TecUtilLockFinish(AddOnID);
}





#include "guibld.c"
