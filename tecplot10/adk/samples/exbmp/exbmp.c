#include "TECADDON.h"
AddOn_pa AddOnID;

#include "TASSERT.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include "ADDONVER.h"
#include "EXBMP.h"


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
 */

EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{
  TecUtilLockOn();

  REQUIRE(sizeof(BYTE)  == 1);
  REQUIRE(sizeof(WORD)  == 2);
  REQUIRE(sizeof(DWORD) == 4);

  AddOnID = TecUtilAddOnRegister(100,ADDON_NAME,ADDON_VERSION" ("TecVersionId") "ADDON_DATE,
    "Amtec Engineering, Inc.");

  if (TecUtilGetTecplotVersion() < MinTecplotVersionAllowed)
  {
    char buffer[256];
    sprintf(buffer,"Add-on \"%s\" requires Tecplot version %s or greater",ADDON_NAME,TecVersionId);
    TecUtilDialogErrMsg(buffer);
  }
  else
  {
    InitTGB(); 

    TecUtilMenuAddOption("Tools", /* Currently the only place you can add a menu item */
                         "Export BMP Image",
                         '\0',
                         MenuCallback);
  }

  TecUtilLockOff();
}



      

   
 
