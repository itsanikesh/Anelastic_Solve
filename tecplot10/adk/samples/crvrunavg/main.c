#include "TECADDON.h"
AddOn_pa AddOnID;

#include "ADDGLBL.h"
#include "ENGINE.h"

#define MIN_TECPLOT_VERSION_ALLOWED 890000
#define MIN_TECPLOT_VERSION_STRING  "8.9-0-0"





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


  AddOnID = TecUtilAddOnRegister(100,ADDON_NAME,
                           "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                           "Amtec Engineering, Inc.");




  /*
   * Register this add-on as an extended curve fit. But first, check
   * that Tecplot is version 9 or later, as earlier versions did not
   * support extended curve fits.
   */
  if (TecUtilGetTecplotVersion() < MIN_TECPLOT_VERSION_ALLOWED)
    {
      char buffer[256];
      sprintf(buffer, "Add-On \"%.50s\" requires Tecplot version %s or greater",
              ADDON_NAME, MIN_TECPLOT_VERSION_STRING);
      TecUtilDialogErrMsg(buffer);
    }
  else
    {       
      TecUtilCurveRegisterExtCrvFit(ADDON_NAME,
                                XYDataPointsCallback,
                                ProbeValueCallback,
                                CurveInfoStringCallback,
                                NULL,  /* CurveSettingsCallback */
                                NULL); /* AbbreviatedSettingsStringCallback */
    }

  TecUtilLockOff();
}

