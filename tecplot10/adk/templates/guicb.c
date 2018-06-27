$$IF(MFCDLL)
#include "stdafx.h"
$$ENDIF
#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
$$IF(AW_CURVEFIT)
#include "ENGINE.h"

extern GlobalCurve_s GlobalCurve;
$$ENDIF
$$IF(AW_CONVERTERPLUS)
#include "ENGINE.h"

extern GlobalConvPlus_s GlobalConvPlus;
$$ENDIF

$$IF(AW_HASSIDEBAR)
static void Sidebar1Activate_CB(void)
{
  /* <<< This function is called when sidebar 1 is activated >>> */
}

static void Sidebar1Deactivate_CB(void)
{
  /* <<< This function is called when sidebar 1 is deactivated >>> */
}

$$ENDIF


$$IF(AW_ISMODAL)
/**
 */
static void Dialog1Init_CB(void)
{
  /* TODO: Add any initialization here. */
  TecUtilLockStart(AddOnID);
}

/**
 */
static void Dialog1OkButton_CB(void)
{
  /* Only unlock tecplot here because a modal dialog was launched. */
$$IF(AW_CONVERTERPLUS)
  Boolean_t IsOk = TRUE;
 
  /*
   * Here you would collect data from the dialog and pass that information
   * to the Action_Alter function.  Since we have not created the dialog
   * for you, we hard code the multiplier to "2".
   */
  IsOk = Action_Alter("2");      
  if ( !IsOk )
    {
      TecUtilDialogErrMsg("An error occured while altering the data.");
    }
  /*
   * This is required to be called after the user input and the calculations
   * have been made.  It must not be called from the macro process function.
   * This tells Tecplot that we are done performing our post load actions.
   */  
  TecUtilDataSetPostReadFinished(IsOk);  
$$ENDIF
$$IF(AW_CURVEFIT)
  /*
   * When curve settings change, Tecplot must be informed of the change.
   * It is most convenient to inform Tecplot when the 'OK' button
   * is pressed on your dialog.
   */
    {
      /* TODO: Construct the CurveSettings string from the new settings */
      char *CurveSettings = NULL;
      if (CurveSettings != NULL)
        {
          SetIndex_t Map;
          TecUtilSetForEachMember(Map, GlobalCurve.XYMapSet)
            {
              TecUtilCurveSetExtendedSettings((EntIndex_t)Map, CurveSettings);
            }
          TecUtilStringDealloc(&CurveSettings);
        }
    }
$$ENDIF

  TecGUIDialogDrop(Dialog1Manager);

  TecUtilLockFinish(AddOnID);
}

/**
 */
static void Dialog1CancelButton_CB(void)
{
  /* Only unlock tecplot here because a modal dialog was launched. */
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}
$$ENDIF
$$IF(AW_ISMODELESS)
/**
 */
static void Dialog1Init_CB(void)
{
  /* TODO: Add any initialization here. */
}

/**
 */
static void Dialog1CloseButton_CB(void)
{
  TecGUIDialogDrop(Dialog1Manager);
}

/**
 */
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}
$$ENDIF


$$IF(AW_CPPLANGUAGE)
#include "guibld.cpp"
$$ENDIF
$$IF(AW_CLANGUAGE)
#include "guibld.c"
$$ENDIF

