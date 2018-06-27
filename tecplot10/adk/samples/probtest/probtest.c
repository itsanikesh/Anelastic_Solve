#include "TECADDON.h"
#include "UTIL.h"
 
 
static void probtestMenuCB()
{
  TecUtilLockOn();
  if (TecUtilDataSetIsAvailable() && TecUtilFrameGetPlotType() != PlotType_Sketch)
    TecUtilProbeInstallCallback(ProbeTestCallback,
                                "Click to run the probe test.");
  else
    TecUtilDialogErrMsg("To execute this add-on Tecplot must have\n"
                        "a data set and the frame must be in XY,\n"
                        "2D, or 3D.");
  TecUtilLockOff();
}
 
 
void InitTecAddOn(void)
{
  TecUtilLockOn();
  TecUtilAddOnRegisterInfo("Probe Test",
                           "V1.0-01/01/99",
                           "Amtec Engineering");

  TecUtilStateChangeAddCallback(ProbeTestStateChangeCallback);
 
  TecUtilMenuAddOption("Tools",
                       "Probe Test",
                       'P',
                        probtestMenuCB);
  TecUtilLockOff();
}
