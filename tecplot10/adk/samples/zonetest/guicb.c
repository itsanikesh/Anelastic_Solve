#include "TECADDON.h"
#include "ADDGLBL.h"
#ifndef MSWIN
#include <unistd.h>
#endif
#include "GUIDEFS.h"
#include "ZONEUTIL.h"



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
  TecUtilLockFinish(AddOnID);
}




static void ShrinkZone_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);
  ShrinkZone(TRUE);
  TecUtilLockFinish(AddOnID);
}




static void ExpandZone_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);
  ExpandZone(TRUE);
  TecUtilLockFinish(AddOnID);
}




static void RefineMesh_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);
  if (TecUtilDataSetIsAvailable() && TecUtilFrameGetPlotType() != PlotType_Sketch)
    TecUtilProbeInstallCallback(CellRefineProbeCallback,
                                "Click to refine a cell.");
  else
    TecUtilDialogErrMsg("To refine the mesh Tecplot must have a\n"
                        "data set and the frame must be in XY,\n"
                        "2D, or 3D.");
  TecUtilLockFinish(AddOnID);
}





#include "guibld.c"
