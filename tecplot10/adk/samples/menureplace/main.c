#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include <string.h>


AddOn_pa AddOnID;


static void STDCALL ExitMenuCallback(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilQuit();
  TecUtilLockFinish(AddOnID);
}

static void STDCALL MyDialogMenuCallback(void)
{
  TecUtilLockStart(AddOnID);
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

static void STDCALL MySecondDialogMenuCallback(void)
{
  TecUtilLockStart(AddOnID);
  BuildDialog2(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog2Manager);
  TecUtilLockFinish(AddOnID);
}

static void SpecialCallback(const char *String)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox(String, MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}

static void STDCALL MySpecial1Callback(void)
{
  SpecialCallback("1");
}
static void STDCALL MySpecial2Callback(void)
{
  SpecialCallback("2");
}
static void STDCALL MySpecial3Callback(void)
{
  SpecialCallback("3");
}
static void STDCALL MySpecial4Callback(void)
{
  SpecialCallback("4");
}
static void STDCALL MySpecial5Callback(void)
{
  SpecialCallback("5");
}
static void STDCALL MySpecial6Callback(void)
{
  SpecialCallback("6");
}
static void STDCALL MySpecial7Callback(void)
{
  SpecialCallback("7");
}
static void STDCALL MySpecial8Callback(void)
{
  SpecialCallback("8");
}

static void CreateNewMenuSystem(void); /*forward reference*/

/**
 * Controls the scheduling of the CreateNewMenuSystemOnIdle function.
 */
static Boolean_t CreateNewMenuSystemOnIdleIsScheduled = FALSE;

/**
 * If scheduled this function is called when Tecplot is idle to update the
 * sidebar.
 */
static void STDCALL CreateNewMenuSystemOnIdle(ArbParam_t ClientData)
{
  TecUtilLockOn();

  CreateNewMenuSystem();

  CreateNewMenuSystemOnIdleIsScheduled = FALSE;
  TecUtilLockOff();
}

/**
 */
static void ScheduleCreateNewMenuSystemOnIdle(void)
{
  /* only schedule the on-idle callback if it isn't already */
  if (!CreateNewMenuSystemOnIdleIsScheduled)
    {
      CreateNewMenuSystemOnIdleIsScheduled = TRUE;
      TecUtilOnIdleQueueAddCallback(CreateNewMenuSystemOnIdle, 0);
    }
}

static void STDCALL RecreateMenuSystem(void)
{
  TecUtilLockStart(AddOnID);
  /*
   * Note that we schudule the creation of the new menu system when Tecplot is
   * idle instead of just creating the new menu system right here. We do this
   * because our CreateNewMenuSystem function deletes all the menus and this
   * callback is a response to a menu selection event. We doin't want to delete
   * the menu out from under us so we have to get out of this menu call chain
   * before attempting to delete the menu system. This is done by registering
   * an on idle callback to update the menu sytem.
   */
  ScheduleCreateNewMenuSystemOnIdle();
  TecUtilLockFinish(AddOnID);
}

static void CreateNewMenuSystem(void)
{
  /*
   * Use TecUtilMenuClearAll with caution.  TecUtilMenuClearAll clears the entire
   * Tecplot menu system.  All menus, submenus and items are cleared, including
   * those from add-ons.  Make sure this is the first add-on loaded (first in tecplot.add)
   * or other add-ons may become inaccessable.  Once cleared there is no way
   * to put back the standard menu system (or any lost add-on menu options).
   */
  TecUtilMenuClearAll();

  /*
   * You can use '&' to specify a mnemonic on a new menu path.  You use the third
   * parameter of TecUtilMenuAddOption to specify the mnemonic for the option itself.
   */
  TecUtilMenuAddOption("&File",
                       "Exit",
                       'x',
                       ExitMenuCallback);
  TecUtilMenuAddOption("&Tools",
                       "My Dialog",
                       'D',
                       MyDialogMenuCallback);
  TecUtilMenuAddOption("Tools", /* Since tools will already exist we can skip the mnemonic */
                       "My Second Dialog",
                       'S',
                       MySecondDialogMenuCallback);
  TecUtilMenuAddSeparator("T&ools"); /* Specifying a mnemonic for an existing path is ignored */

  /*
   * Now add the same option above in some submenus.  Submenus are specified
   * with '\n' in the menu path.
   */
  TecUtilMenuAddOption("Tools\n&More Tools",
                       "Another Route to My Dialog",
                       '\0', /* no mnemonic */
                       MyDialogMenuCallback);
  TecUtilMenuAddSeparator("Tools\n&More Tools");
  TecUtilMenuAddOption("Tools\n&More Tools\nSpecial", /* No mnemonic for new submenu Special */
                       "Another Route to My Second Dialog",
                       'S',
                       MySecondDialogMenuCallback);

  /* Now add some menu options with various accelerator keys */
  TecUtilMenuAddSeparator("Tools\n&More Tools\nSpecial");
  TecUtilMenuAddOption("Tools\n&More Tools\nSpecial",
                       "Special 1\tAlt+1",
                       '1',
                       MySpecial1Callback);
  TecUtilMenuAddOption("Tools\n&More Tools\nSpecial",
                       "Special 2\tShift+2",
                       '2',
                       MySpecial2Callback);
  TecUtilMenuAddOption("Tools\n&More Tools\nSpecial",
                       "Special 3\tCtrl+3",
                       '3',
                       MySpecial3Callback);
  TecUtilMenuAddOption("Tools\n&More Tools\nSpecial",
                       "Special 4\tAlt+Ctrl+4",
                       '4',
                       MySpecial4Callback);
  TecUtilMenuAddOption("Tools\n&More Tools\nSpecial",
                       "Special 5\tAlt+Shift+5",
                       '5',
                       MySpecial5Callback);
  TecUtilMenuAddOption("Tools\n&More Tools\nSpecial",
                       "Special 6\tCtrl+Shift+6",
                       '6',
                       MySpecial6Callback);
  TecUtilMenuAddOption("Tools\n&More Tools\nSpecial",
                       "Special 7\tAlt+Ctrl+Shift+7",
                       '7',
                       MySpecial7Callback);
  TecUtilMenuAddOption("Tools\n&More Tools\nSpecial",
                       "Special 8\tAlt+1",
                       '8',
                       MySpecial8Callback);

  /*
   * Finally test recreating the entire menu system.  This is 
   * dangerous if any other add-ons are loaded and using the
   * menu system (which most other add-on do).
   */
  TecUtilMenuAddSeparator("Tools\n&More Tools\nSpecial");
  TecUtilMenuAddOption("Tools\n&More Tools\nSpecial",
                       "Recreate Menu System",
                       '\0',
                       RecreateMenuSystem);
}


/*
 * When Tecplot first loads an add-on, it makes a 
 * call to initialize the add-on. This function
 * must be named InitTecAddOn, as shown below.
 */
EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{


  /*
   * NOTE:  TecUtilLockOn MUST be used for InitTecAddOn instead
   *        of TecUtilLockStart because AddonID has yet to be
   *        established.  TecUtilLockOn is in effect an "annonymous"
   *        locking of tecplot (old style).
   */

  TecUtilLockOn();

  /*
   * The function TecUtilAddOnRegister() is the
   * only function that is REQUIRED to be called from
   * the initialization function.
   *
   * The information you give Tecplot by calling
   * this function will show up in the Help/About Add-ons
   * dialog box.
   */

  /*
   * Note that if your add-on requires a specific version of Tecplot,
   * you would check for that here using TecUtilGetTecplotVersion()
   */

  AddOnID = TecUtilAddOnRegister(100,
                                 ADDON_NAME,
                                 "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                                 "Tecplot Inc.");

  /*
   * Initialize the Tecplot GUI Builder libraries.
   */
  InitTGB();

  /*
   */
  CreateNewMenuSystem();


  /*
   * See note on TecUtilLockOn at start of this function.
   */
  TecUtilLockOff();
}

