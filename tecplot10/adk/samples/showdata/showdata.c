#include "TECADDON.h"
AddOn_pa AddOnID;

#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include "ADDONVER.h"

extern void RefreshDialog(void);

/**
 * Controls the scheduling of the RefreshDialogOnIdle function.
 */
static Boolean_t RefreshDialogOnIdleIsScheduled = FALSE;

/**
 * If scheduled this function is called when Tecplot is idle to update the
 * sidebar.
 */
static void STDCALL RefreshDialogOnIdle(ArbParam_t ClientData)
{
  TecUtilLockStart(AddOnID);

  RefreshDialog();
  RefreshDialogOnIdleIsScheduled = FALSE;

  TecUtilLockFinish(AddOnID);
}

/**
 * Responsible for scheduling the updating of the sidebar interface.
 */
static void ScheduleRefreshDialogOnIdle(void)
{
  /* only schedule the on-idle callback if it isn't already */
  if (!RefreshDialogOnIdleIsScheduled)
    {
      RefreshDialogOnIdleIsScheduled = TRUE;
      TecUtilOnIdleQueueAddCallback(RefreshDialogOnIdle, 0);
    }
}

/*
 * This function will be called by Tecplot
 * each time a state change occurs.
 */
void STDCALL StateChangeCallback(StateChange_e StateChange,
                                 ArbParam_t    CallData)
{
  switch (StateChange)
    {
      /*
       *
       * Your add-on can process the following state changes.
       * See the ADK Users Manual for more complete
       * information on each state change listed here.
       *
       */
      case StateChange_VarsAltered:
      case StateChange_VarsAdded:       /* Call data is the set of added variables (Set_pa type) */
      case StateChange_ZonesDeleted:    /* Call data is the set of deleted zones (Set_pa type) */
      case StateChange_ZonesAdded:      /* Call data is the set of added zones (Set_pa type) */
      case StateChange_NodeMapsAltered: /* Call data is the set of node maps altered (Set_pa type) */
      case StateChange_MouseModeUpdate:
      case StateChange_FrameDeleted:
      case StateChange_NewTopFrame:
      case StateChange_Style:
      case StateChange_DataSetReset:
      case StateChange_NewLayout:
      case StateChange_CompleteReset:
      case StateChange_XYMapAssignment:
      case StateChange_ContourLevels:
      case StateChange_ModalDialogLaunch:
      case StateChange_ModalDialogDismiss: 
      case StateChange_QuitTecplot:
      case StateChange_ZoneName:
      case StateChange_VarName:
      case StateChange_XYMapName:
      case StateChange_XYMapAddDeleteOrReorder:
      case StateChange_View:
      case StateChange_ColorMap:
      case StateChange_ContourVar:
      case StateChange_Streamtrace:
      case StateChange_NewAxisVariables:
      case StateChange_PickListCleared:
      case StateChange_PickListGroupSelect:
      case StateChange_PickListSingleSelect:
      case StateChange_PickListStyle:
      case StateChange_DataSetFileName:
      break;
      default:
      break;
    } /* end switch */

  if (TecGUIDialogIsUp(Dialog1Manager))
    ScheduleRefreshDialogOnIdle();
}



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

  AddOnID = TecUtilAddOnRegister(100,"Show Data",
                           "1.0 (10/24/1999)",
                           "Amtec Engineering, Inc.");

  if (TecUtilGetTecplotVersion() < MinTecplotVersionAllowed)
  {
    char buffer[256];

    sprintf(buffer,
            "Add-on \"%s\" requires Tecplot version %s or greater",
            ADDON_NAME,
            TecVersionId);

    TecUtilDialogErrMsg(buffer);
  }

  else
  {

    TecUtilStateChangeAddCallback(StateChangeCallback);
    TecUtilMenuAddOption("Tools", /* Currently the only place you can add a menu item */
                       "Show Field Data",
                       '\0',
                       MenuCallback);
  }

  TecUtilLockOff();
}



      

   
 
