/*
*****************************************************************
*****************************************************************
*******                                                  ********
****** (C) Copyright 1989-1998  by AMTEC ENGINEERING INC.********
*******       All Rights Reserved.                       ********
*******                                                  ********
*****************************************************************
*****************************************************************
*/

#define GUIMAIN
#include "TECADDON.h"
#include "ADDONVER.h"
#include "ADDGLBL.h"
#include "GUI.h"
#include "GUIDEFS.h"

AddOn_pa AddOnID;
static void STDCALL cstreamMenuCB(void)
{
  TecUtilLockStart(AddOnID);
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}
 

EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{
  TecUtilLockOn();
  AddOnID = TecUtilAddOnRegister(100,
                                 ADDON_NAME,
                                 ADDON_VERSION" ("TecVersionId") "ADDON_DATE,
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
 
    TecUtilMenuAddOption("Tools",
                       "Circle Stream",
                       'C',
                        cstreamMenuCB);
  }
  TecUtilLockOff();
}
