#define GUIMAIN
#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include "ZONEUTIL.h"
 
AddOn_pa AddOnID;
 
static void STDCALL zonetestMenuCB(void)
{
  TecUtilLockStart(AddOnID);
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}
 
 
EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{
  TecUtilLockOn();
  AddOnID = TecUtilAddOnRegister(100,
                                 ADDON_NAME,
                                 "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                                 "Tecplot, Inc.");

  TecUtilMacroAddCommandCallback("ZONETEST",
                                 ZoneTestMacroCallback);
  InitTGB();
 
  TecUtilMenuAddOption("Tools",
                       "Zone Test",
                       'Z',
                       zonetestMenuCB);
  TecUtilLockOff();
}
