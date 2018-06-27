#include "TECADDON.h"
AddOn_pa AddOnID;

#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include "ADDONVER.h"
#include "TIMETEST.h"

/* THE global state */
TimerTestState_s TimerTestState;

static void STDCALL MenuCallback(void)
{
  TecUtilLockStart(AddOnID);
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

/*
 * When Tecplot first loads an add-on, it makes a 
 * call to initialize the add-on. This function
 * must be named InitTecAddOn, as shown below.
 *
 */
EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{
  TecUtilLockOn();

  /*
   * The function TecUtilAddOnRegisterInfo() is the
   * only function that is REQUIRED to be called from
   * the initialization function.
   *
   * The information you give Tecplot by calling
   * this function will show up in the Help/About Addons
   * dialog box.
   */

  /*
   * Note that if your add-on requires a specific version of Tecplot,
   * you would check for that here using TecUtilGetTecplotVersion()
   */

  AddOnID = TecUtilAddOnRegister(100,ADDON_NAME,
                                 ADDON_VERSION" ("TecVersionId") "ADDON_DATE,
                                 "Tecplot, Inc.");

  if (TecUtilGetTecplotVersion() < MinTecplotVersionAllowed)
    {
      char buffer[256];
      sprintf(buffer,"Add-on \"%s\" requires Tecplot version %s or greater",ADDON_NAME,TecVersionId);
      TecUtilDialogErrMsg(buffer);
    }
  else
    {
      TecUtilMenuAddOption("Tools", /* Currently the only place you can add a menu item */
                           "Timer Test",
                           '\0',
                           MenuCallback); 
    }

  TecUtilLockOff();
}
