$$IF(MFCDLL)
#include "stdafx.h"
$$ENDIF
#include "TECADDON.h"
#include "ADDGLBL.h"
$$IF(AW_USETGB)
$$IF(!MFCDLL)
#include "GUIDEFS.h"
$$ENDIF
$$ENDIF
$$IF(AW_LOADER)
#include "ENGINE.h"
$$ENDIF
$$IF(AW_CONVERTER)
#include "ENGINE.h"
$$ENDIF
$$IF(AW_CURVEFIT)
#include "ENGINE.h"
$$ENDIF
$$IF(AW_MACROCOMMANDS)
#include <string.h>
$$ENDIF

$$IF(AW_CURVEFIT)
#define MIN_TECPLOT_VERSION_ALLOWED 890000
#define MIN_TECPLOT_VERSION_STRING  "8.9-0-0"
$$ENDIF
$$IF(AW_CONVERTERPLUS)
#define MIN_TECPLOT_VERSION_ALLOWED 900300
#define MIN_TECPLOT_VERSION_STRING  "9.0-3-0"
$$ENDIF

AddOn_pa AddOnID;


$$IF(AW_MACROCOMMANDS)
/**
 * This function is called when the
 * $!ADDONCOMMAND macro command is
 * processed.
 */
static Boolean_t STDCALL MacroCommandCallback(char *MacroCommandString,  /* IN */
                                              char **ErrMsg)             /* OUT (only if returning FALSE) */
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF

  Boolean_t IsOk = TRUE;

  /* 
   * MacroCommandString is the add-on macro command string needing processing.
   *
   * *ErrMsg is an error message string which must be allocated and set by this
   * function if and only if the return value is FALSE.
   */
  
  TecUtilLockStart(AddOnID);
  
  /*
   * TODO: Process the macro command.
   *
   * Example:
   *
   * $!ADDONCOMMAND ADDONID='$$AW_ADDONNAME$$' COMMAND='MYCOMMAND'
   */
  
  if (!strcmp(MacroCommandString,"MYCOMMAND")) /* For example */
    {
      /* IsOk = ProcessMacroCommand_MYCOMMAND(); */
    }

  if (!IsOk)
    {
      /*
       * Some kind of error, so inform the user about it.
       */
  
      *ErrMsg = TecUtilStringAlloc(1000,"String for Error Message");
      strcpy(*ErrMsg,"Error processing macro command");
    }
  else
    {
      /* Ignore the *ErrMsg parameter */
    }

  TecUtilLockFinish(AddOnID);
  return (IsOk);
}
$$ENDIF

$$IF(AW_STATECHANGE)
/**
 */
static void STDCALL StateChangeCallback(StateChange_e StateChange)
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF

  switch (StateChange)
    {
   /*
    * This function will be called by Tecplot
    * each time a state change occurs.
    *
    *
    * NOTE:
    *
    * Some State changes also have some supplemental "state"
    * information that can be retrieved if you desire.
    * Comments in the case statements below identify these
    * state changes.  To retrieve the supplemental information
    * use the functions TecUtilStateChangeGetXXXXX. You may
    * only call these functions during the scope of this
    * callback.  Once control returns from this call the
    * supplemental information will become unaccessible.
    *
    */

           /*   State Change                Supplemental information */

      case StateChange_VarsAltered:     /* set of altered variables */
      case StateChange_VarsAdded:       /* set of added variables */
      case StateChange_ZonesDeleted:    /* set of deleted zones */
      case StateChange_ZonesAdded:      /* set of added zones */
      case StateChange_NodeMapsAltered: /* set of node maps altered */
      case StateChange_MouseModeUpdate: /* the new mouse mode */
      case StateChange_Style:           /* Style Parameters P1,P2,P3,P4,P5,P6 */
      case StateChange_View:            /* View action (View_e) */
      case StateChange_Streamtrace:     /* Streamtrace action (Streamtrace_e) */
      case StateChange_AuxDataAltered:  /* Auxiliary Location (AuxDataLocation_e)*/
      case StateChange_AuxDataAdded:    /* Auxiliary Location (AuxDataLocation_e)*/
      case StateChange_AuxDataDeleted:  /* Auxiliary Location (AuxDataLocation_e)*/

    /* State changes which do not have any supplemental "state" information. */
      case StateChange_FrameDeleted:        /* A frame was delete */
      case StateChange_NewTopFrame:         /* A new frame has become the current frame */   
      case StateChange_Text:                /* One or more text elements has changed */
      case StateChange_Geom:                /* One or more geometry elements has changed */
      case StateChange_DataSetReset:        /* A new dataset has been loaded */
      case StateChange_NewLayout:           /* The current layout has been cleared and reset */
      case StateChange_CompleteReset:       /* Anything could have happened */
      case StateChange_LineMapAssignment:   /* A line mapping definition has been altered (includes zone and axis information) */
      case StateChange_ContourLevels:       /* The contour levels have been altered */
      case StateChange_ModalDialogLaunch:   /* A modal dialog has been launched */
      case StateChange_ModalDialogDismiss:  /* A modal dialog has been dismissed */
      case StateChange_QuitTecplot:         /* Tecplot is about to exit */
      case StateChange_ZoneName:            /* The name of a zone has been altered */
      case StateChange_VarName:             /* The name of a variable has been altered */
      case StateChange_LineMapName:           /* The name of an X-Y mapping has been altered */
      case StateChange_LineMapAddDeleteOrReorder: /* The set of existing X-Y mappings has been altered */
      case StateChange_ColorMap:            /* The color mapping has been altered */
      case StateChange_ContourVar:          /* The contour variable has been reassigned */
      case StateChange_NewAxisVariables:    /* The axis variables have been reassigned */
      case StateChange_PickListCleared:     /* All picked objects are unpicked */
      case StateChange_PickListGroupSelect: /* A group of objects has been added to the pick list */
      case StateChange_PickListSingleSelect:/* A single object has been added to or removed from the pick list */
      case StateChange_PickListStyle:       /* An action has been performed on all of the objects in the pick list */
      case StateChange_DataSetFileName:     /* The current data set has been saved to a file */
      case StateChange_DataSetTitle:        /* The current data set title has been changed */
      case StateChange_DataSetLockOn:       /* The dataset attached to the active frome in Tecplot has been locked */
      case StateChange_DataSetLockOff:      /* The dataset attached to the active frome in Tecplot has been unlocked */
      case StateChange_DrawingInterrupted:  /* The user has interrupted the drawing */


    /* Version 9 and later Note: If you are using modeless dialogs, you should
       trap the following state changes and take appropriate
       action when print preview is launched and dismissed.

       Usually you will either disable or close your dialog
       when print preview is launched. */

      case StateChange_PrintPreviewLaunch:  /* Modeless dialogs should close or disable themselves */
      case StateChange_PrintPreviewDismiss: /* Modeless dialogs can re-launch or enable themselves */


      case StateChange_SuspendInterface:    /* Replaces StateChange_DrawGraphicsOn */
      case StateChange_UnsuspendInterface:  /* Replaces StateChange_DrawGraphicsOff */
        {
          /* TODO: Add code to handle state changes.... */
        } break;
      default: break;
    } /* end switch */
}
$$ENDIF

$$IF(AW_GENERAL)
$$IF(AW_MAKEMENUOPTION)
/**
 */
static void STDCALL MenuCallback(void)
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF
  TecUtilLockStart(AddOnID);
$$IF(AW_USETGB)
$$IF(!MFCDLL)
$$IF(!AW_ISMODAL)
$$IF(!AW_ISMODELESS)
$$IF(AW_HASSIDEBAR)
  TecGUISidebarActivate(Sidebar1Manager);
$$ENDIF
$$ENDIF
$$ENDIF
$$IF(AW_ISMODAL)
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
$$ENDIF
$$IF(AW_ISMODELESS)
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
$$ENDIF
$$ENDIF
$$IF(MFCDLL)
  /* TODO: Add code to launch an MFC dialog here */
  TecUtilDialogMessageBox("MFC Dialog under construction...",MessageBox_Information);
$$ENDIF
$$ENDIF
$$IF(!AW_USETGB)
  TecUtilDialogMessageBox("Menu function called",MessageBox_Information);
$$ENDIF
  TecUtilLockFinish(AddOnID);
}
$$ENDIF
$$ENDIF

/**
 * When Tecplot first loads an add-on, it makes a 
 * call to initialize the add-on. This function
 * must be named InitTecAddOn, as shown below.
 */
EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{
$$IF(MFCDLL)
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
$$ENDIF


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

$$IF(!AW_CURVEFIT)
$$IF(!AW_CONVERTERPLUS)
  /*
   * Note that if your add-on requires a specific version of Tecplot,
   * you would check for that here using TecUtilGetTecplotVersion()
   */
$$ENDIF
$$ENDIF

  AddOnID = TecUtilAddOnRegister($$AW_TECPLOTVERSION$$,
                                 ADDON_NAME,
                                 "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                                 "$$AW_COMPANY$$");

$$IF(AW_USETGB)
$$IF(!MFCDLL)
  /*
   * Initialize the Tecplot GUI Builder libraries.
   */
  InitTGB();

$$IF(AW_HASSIDEBAR)
  if (!TecUtilMacroIsBatchModeActive())
    BuildSidebar1(); /* ... build but don't activate the sidebar */
$$ENDIF
$$ENDIF
$$ENDIF
$$IF(AW_LOADER)
$$IF(AW_INCLUDEOVERRIDE)
  /* Register this add-on as a loader */
  TecUtilImportAddLoader(LoaderCallback,
                         ADDON_NAME,
                         LoaderSelectedCallback,
                         InstructionOverrideCallback);
$$ENDIF
$$IF(!AW_INCLUDEOVERRIDE)
  TecUtilImportAddLoader(LoaderCallback,
                         ADDON_NAME,
                         LoaderSelectedCallback,
                         NULL);
$$ENDIF
$$ENDIF

$$IF(AW_MACROCOMMANDS)
  TecUtilMacroAddCommandCallback(ADDON_NAME,
                                 MacroCommandCallback);
$$ENDIF
$$IF(AW_STATECHANGE)
  {
    ArgList_pa ArgList;
    ArgList = TecUtilArgListAlloc();
    TecUtilArgListAppendFunction(ArgList, SV_CALLBACKFUNCTION, (const void *)StateChangeCallback);
    TecUtilArgListAppendInt(ArgList,      SV_STATECHANGEMODE,  StateChangeMode_v100);
    TecUtilStateChangeAddCallbackX(ArgList);
    TecUtilArgListDealloc(&ArgList);
  }
$$ENDIF
$$IF(AW_GENERAL)
$$IF(AW_MAKEMENUOPTION)
  TecUtilMenuAddOption("Tools",
                       "$$AW_MENUTEXT$$",
                       '\0',
                       MenuCallback);
$$ENDIF
$$ENDIF
$$IF(AW_CONVERTER)
$$IF(!AW_CONVERTERPLUS)
  /* Register this add-on as a converter */
  TecUtilImportAddConverter(ConverterCallback,
                            ADDON_NAME,
                            "*.*");
$$ENDIF
$$ENDIF

$$IF(AW_CONVERTERPLUS)
  /*
   * Register this add-on as a converter plus. But first, check
   * that Tecplot is version 9.0-3 or later, as earlier versions do not
   * support converter plus add-ons.
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
      /* Register this add-on as a converter */
      TecUtilImportAddConverter(ConverterCallback,
                                ADDON_NAME,
                                "*.*");
      /*
       * This handles launching of the dialog to get user input.
       */
      TecUtilConvAddPostReadCallback(ADDON_NAME,
                                     PostConvertCallback);
      /*
       * When macro command or layout is processed, this function is called.
       */
      TecUtilMacroAddCommandCallback(ADDON_NAME, 
                                     ProcessConvertMacroCommand);
    }
$$ENDIF

$$IF(AW_CURVEFIT)
  /*
   * Register this add-on as an extended curve fit. But first, check
   * that Tecplot is version 9 or later, as earlier versions do not
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
$$IF(AW_USETGB)
$$IF(AW_USEPROBECB)
      TecUtilCurveRegisterExtCrvFit(ADDON_NAME,
                                XYDataPointsCallback,
                                ProbeValueCallback,
                                CurveInfoStringCallback,
                                CurveSettingsCallback,
                                AbbreviatedSettingsStringCallback);
$$ENDIF
$$IF(!AW_USEPROBECB)
      TecUtilCurveRegisterExtCrvFit(ADDON_NAME,
                                XYDataPointsCallback,
                                NULL,  /* ProbeValueCallback */
                                CurveInfoStringCallback,
                                CurveSettingsCallback,
                                AbbreviatedSettingsStringCallback);
$$ENDIF
$$ENDIF
$$IF(!AW_USETGB)
$$IF(AW_USEPROBECB)
      TecUtilCurveRegisterExtCrvFit(ADDON_NAME,
                                XYDataPointsCallback,
                                ProbeValueCallback,
                                CurveInfoStringCallback,
                                NULL,  /* CurveSettingsCallback */
                                NULL); /* AbbreviatedSettingsStringCallback */
$$ENDIF
$$IF(!AW_USEPROBECB)
      TecUtilCurveRegisterExtCrvFit(ADDON_NAME,
                                XYDataPointsCallback,
                                NULL,  /* ProbeValueCallback */
                                CurveInfoStringCallback,
                                NULL,  /* CurveSettingsCallback */
                                NULL); /* AbbreviatedSettingsStringCallback */
$$ENDIF
$$ENDIF
    }
$$ENDIF

  /*
   * See note on TecUtilLockOn at start of this function.
   */
  TecUtilLockOff();
}

