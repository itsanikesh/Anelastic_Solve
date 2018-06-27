#include "TECADDON.h"
AddOn_pa AddOnID;

#include "ADDGLBL.h"
#include "GUI.h"
#include "GUIDEFS.h"

static Boolean_t IsDialogSensitive = FALSE;

extern void Refresh(Boolean_t Sensitive);

/**
 * Controls the scheduling of the UpdateDialogOnIdle function.
 */
static Boolean_t UpdateDialogOnIdleIsScheduled = FALSE;

/**
 * If scheduled this function is called when Tecplot is idle to update the
 * sidebar.
 */
static void STDCALL UpdateDialogOnIdle(ArbParam_t ClientData)
{
  TecUtilLockStart(AddOnID);

  Refresh(IsDialogSensitive);
  UpdateDialogOnIdleIsScheduled = FALSE;

  TecUtilLockFinish(AddOnID);
}

/**
 * Responsible for scheduling the updating of the sidebar interface.
 */
static void ScheduleUpdateDialogOnIdle(void)
{
  /* only schedule the on-idle callback if it isn't already */
  if (!UpdateDialogOnIdleIsScheduled)
    {
      UpdateDialogOnIdleIsScheduled = TRUE;
      TecUtilOnIdleQueueAddCallback(UpdateDialogOnIdle, 0);
    }
}

void STDCALL StateChangeCallback(StateChange_e StateChange, /* IN */
                                 ArbParam_t    CallData)    /* IN */ 
{

  switch (StateChange)
    {
   /*
    * This function will be called by Tecplot
    * each time a state change occurs.
    *
    *
    * NOTE:
    *
    * By default, when loading a layout or running a macro,
    * you will not receive any state change callbacks until
    * the macro or layout is complete. At that time you will
    * receive the StateChange_CompleteReset message.
    *
    * If you wish to receive state changes AS a macro is run
    * or a layout file is loaded, you must call
    * TecUtilStateChangeSetMode(...). See the ADK reference
    * manual for more information on this function.
    *
    */

    /* State changes which use 'CallData' */

    /* Note: before using the 'CallData' prameter, be sure to
       cast it to the appropriate type ((Set_pa), for example.) */

      case StateChange_VarsAdded:       /* 'CallData' is the set of added variables (Set_pa type) */
      case StateChange_ZonesDeleted:    /* 'CallData' is the set of deleted zones (Set_pa type) */
      case StateChange_ZonesAdded:      /* 'CallData' is the set of added zones (Set_pa type) */

      case StateChange_FrameDeleted:        /* A frame was delete */
      case StateChange_NewTopFrame:         /* A new frame has become the current frame */   
      case StateChange_DataSetReset:        /* A new dataset has been loaded */
      case StateChange_NewLayout:           /* The current layout has been cleared and reset */
      case StateChange_CompleteReset:       /* Anything could have happened */
      case StateChange_ContourLevels:       /* The contour levels have been altered */
      case StateChange_ZoneName:            /* The name of a zone has been altered */
      case StateChange_VarName:             /* The name of a variable has been altered */
      case StateChange_ColorMap:            /* The color mapping has been altered */
      case StateChange_ContourVar:          /* The contour variable has been reassigned */
      case StateChange_DataSetFileName:     /* The current data set has been saved to a file */
      case StateChange_DataSetTitle:        /* The current data set title has been changed */
      case StateChange_DataSetLockOn:       /* The dataset attached to the active frome in Tecplot has been locked */
      case StateChange_DataSetLockOff:      /* The dataset attached to the active frome in Tecplot has been unlocked */
      case StateChange_DrawingInterrupted:  /* The user has interrupted the drawing */
        {
          IsDialogSensitive = TRUE;
          ScheduleUpdateDialogOnIdle();
        } break;

    /* Version 9 Note: If you are using modeless dialogs, you should
       trap the following state changes and take appropriate
       action when print preview is launched and dismissed.

       Usually you will either disable or close your dialog
       when print preview is launched. */

      case StateChange_PrintPreviewLaunch:  /* Modeless dialogs should close or disable themselves */
      case StateChange_ModalDialogLaunch:
        {
          IsDialogSensitive = FALSE;
          ScheduleUpdateDialogOnIdle();

        } break;

      case StateChange_PrintPreviewDismiss: /* Modeless dialogs can re-launch or enable themselves */
      case StateChange_ModalDialogDismiss:
        {
          IsDialogSensitive = FALSE;
          ScheduleUpdateDialogOnIdle();
        } break;


      case StateChange_SuspendInterface:    /* Replaces StateChange_DrawGraphicsOn */
      case StateChange_UnsuspendInterface:  /* Replaces StateChange_DrawGraphicsOff */

        {
          /* TODO: Insert code to handle selected state changes here */
        } break;

      default: break;
    } /* end switch */
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
   * this function will show up in the Help/About Add-ons
   * dialog box.
   */

  /*
   * Note that if your add-on requires a specific version of Tecplot,
   * you would check for that here using TecUtilGetTecplotVersion()
   */

  AddOnID = TecUtilAddOnRegister(100,ADDON_NAME,
                           "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                           "Amtec Engineering, Inc.");

  /*
   * Initialize the Tecplot GUI Builder libraries.
   */
  InitTGB();


  TecUtilStateChangeAddCallback(StateChangeCallback);
  TecUtilMenuAddOption("Tools",
                       "Set Style",
                       '\0',
                       MenuCallback);


  TecUtilLockOff();
}

