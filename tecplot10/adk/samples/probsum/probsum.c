#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include "ADDONVER.h"

/*
*
*
* Note: Developer Studio does not like to mix .c and .cpp files
* in the same project, so each file in the project is c++ file.
* For each platform independent .c file, we use a dummy .cpp file which
* just #includes it.
*
* Alternatively, if you're not planning to use MFC or C++, you can make your add-on
* project a "c" dll. Just create a new "win32 dll" project and add all of the "c" files.
* Be sure to link your new project with 'libtec.lib' and 'wingui.lib' (if
* you're using the Tecplot GUI Builder).
*
*
*/
 
AddOn_pa AddOnID;

  



static void STDCALL PROBSUMMenuCB(void)
{

  TecUtilLockOn();
  if (TecUtilFrameGetPlotType() == PlotType_XYLine)
    {
      BuildDialog1(MAINDIALOGID);
      TecGUIDialogLaunch(Dialog1Manager);
    }
  else
    TecUtilDialogErrMsg("Err: You must be in XY frame mode to use this");
  TecUtilLockOff();
}


void STDCALL XYSumStateChangeCallback(StateChange_e StateChange,
                                      ArbParam_t    CallData)
{
  TecUtilLockStart(AddOnID);
  if (TecGUIDialogIsUp(Dialog1Manager) &&
      ((StateChange == StateChange_QuitTecplot) || 
       (TecUtilFrameGetPlotType() != PlotType_XYLine)))
    {
      TecGUIDialogDrop(Dialog1Manager); 
    }
  TecUtilLockFinish(AddOnID);
}


EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{
  MANAGESTATE /* Required for Windows MFC calls - Ignored in MOTIF */
  TecUtilLockOn();

  /*
  * Note: TecVersionId is #define'd
  * to a string in VERSION.h
  */

  AddOnID = TecUtilAddOnRegister(100,ADDON_NAME,
                           "V"ADDON_VERSION" ("TecVersionId") "ADDON_DATE,
                           "Amtec Engineering");
  InitTGB();

  TecUtilMenuAddOption("Tools",
                       "Sum XY Maps",
                       'S',
                        PROBSUMMenuCB);

  TecUtilStateChangeAddCallback(XYSumStateChangeCallback);
  
  TecUtilLockOff();
} 
