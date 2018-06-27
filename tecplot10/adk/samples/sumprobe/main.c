#include "TECADDON.h"
AddOn_pa AddOnID;

#include "ADDGLBL.h"
#include "GUIDEFS.h"

/* DOCSTART:gr/sumprobe_def.txt */
EntIndex_t Variable = 1;
double     Total    = 0.0;
/* DOCEND */


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


    case StateChange_VarsAltered:     /* 'CallData' is the set of altered variables (Set_pa type) */
    case StateChange_VarsAdded:       /* 'CallData' is the set of added variables (Set_pa type) */
    case StateChange_ZonesDeleted:    /* 'CallData' is the set of deleted zones (Set_pa type) */
    case StateChange_ZonesAdded:      /* 'CallData' is the set of added zones (Set_pa type) */
    case StateChange_NodeMapsAltered: /* 'CallData' is the set of node maps altered (Set_pa type) */
    case StateChange_MouseModeUpdate: /* 'CallData' is the new mouse mode (MouseButtonMode_e type) */
    case StateChange_FrameDeleted:
    case StateChange_NewTopFrame:
    case StateChange_Style:           /* 'CallData' is string representing the "P1" parameter */
    case StateChange_Text:
    case StateChange_Geom:
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
      {
        /* TODO: Insert code to handle selected state changes here */
      }
      break;
    default:
      break;
  } /* end switch */
}

/**
 */
/* DOCSTART:gr/sumprobe_MyProbeCallback.txt */
static void STDCALL MyProbeCallback(Boolean_t IsNearestPoint)
{
  TecUtilLockStart(AddOnID);

  if (IsNearestPoint)
    {
      double ProbeValue = TecUtilProbeFieldGetValue(Variable);
      char Msg[100]; 

      Total = Total + ProbeValue;
      sprintf(Msg, "The total is: %f", Total);
      CHECK(strlen(Msg) < sizeof(Msg));

      TecGUILabelSetText(Thetotalis00_LBL_D1, Msg);
    }
  else
    TecUtilDialogErrMsg("You must hold down the Ctrl key when probing");

  TecUtilLockFinish(AddOnID);
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/sumprobe_MenuCallback.txt */
static void STDCALL MenuCallback(void)
{
  TecUtilLockStart(AddOnID);

  if (TecUtilDataSetIsAvailable())
    {
      if (TecUtilFrameGetPlotType() == PlotType_Cartesian2D)
        {
          TecUtilDialogGetVariables("Pick Variable to Sum",
                                    NULL,
                                    NULL,
                                    NULL,
                                    &Variable,
                                    NULL,
                                    NULL);

          BuildDialog1(MAINDIALOGID);
          TecGUIDialogLaunch(Dialog1Manager);

          TecUtilProbeInstallCallback(MyProbeCallback,
                                      "Suming Probed Values");
        }  
      else 
        TecUtilDialogErrMsg("Plot type must be 2D cartesian.");
    }
  else
    TecUtilDialogErrMsg("Frame does not contain a dataset "
                        "with which to probe.");

  TecUtilLockFinish(AddOnID);
}
/* DOCEND */

/**
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

  AddOnID = TecUtilAddOnRegister(100,ADDON_NAME,
                                 "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                                 "Amtec Engineering, inc.");


  TecUtilStateChangeAddCallback(StateChangeCallback);
  TecUtilMenuAddOption("Tools", /* Currently the only place you can add a menu item is the 'Tools' menu */
                       "Sum Probed Values",
                       '\0',
                       MenuCallback);

  TecUtilLockOff();
}
