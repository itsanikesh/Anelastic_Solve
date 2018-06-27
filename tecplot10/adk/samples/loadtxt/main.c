#include "TECADDON.h"
AddOn_pa AddOnID;

#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include "ENGINE.h"

/* DOCSTART:gr/loadtxt_LoadTxtGlobalState_def.txt */
LoadTxtGlobalState_s GS;
/* DOCEND */

/***********************************************
 * InitTecAddOn
 ***********************************************/

/*
 * When Tecplot first loads an add-on, it makes a 
 * call to initialize the add-on. This function
 * must be named InitTecAddOn, as shown below.
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

  AddOnID = TecUtilAddOnRegister(
                100,ADDON_NAME,
                "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                "Joe Coder");



  TecUtilImportAddLoader(LoaderCallback,ADDON_NAME,
                         LoaderSelectedCallback,NULL);

/* DOCSTART:gr/loadtxt_LoadTxtGlobalState_init.txt */
  /*
   * Initialize the file name to be empty and the default skip
   * parameter to hav a value of 1.
   */
  GS.FileName[0]  = '\0';
  GS.Skip         = 1;
/* DOCEND */

  TecUtilLockOff();
}

