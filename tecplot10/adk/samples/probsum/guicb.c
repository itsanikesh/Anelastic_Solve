#include "TECADDON.h"
#include "ADDGLBL.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"
#include "UTIL.h"






static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
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
/*<<< Add init code (if necessary) here>>>*/
  TecUtilLockFinish(AddOnID);
}




static void StartProbe_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);
  if (TecUtilFrameGetPlotType() == PlotType_XYLine)
    {
      TecUtilProbeInstallCallback(XYSumProbeCallback,
                                  "Click to Sum XY-Map Values");
    }
  else
    TecUtilDialogErrMsg("Err: Only allowed to use this with XY-Plots");
  TecUtilLockFinish(AddOnID);
}





#include "guibld.c"
