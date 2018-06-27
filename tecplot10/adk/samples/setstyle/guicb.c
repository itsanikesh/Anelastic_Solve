#include "TECADDON.h"

#include "ADDGLBL.h"
#include "GUI.h"
#include "GUIDEFS.h"

/*
  Dialog control variable names:

int Dialog1Manager = BADDIALOGID;
int ShowMesh_TOG_D1 = BADDIALOGID;
int ShowContour_TOG_D1 = BADDIALOGID;
int ResetContour_BTN_D1 = BADDIALOGID;
int NumLevels_TF_D1 = BADDIALOGID;
int Levels_LBL_D1 = BADDIALOGID;
int MeshColor_OPT_D1 = BADDIALOGID;
int MeshColor_LBL_D1 = BADDIALOGID;
*/

static void MeshColor_OPT_D1_CB(const int *I)
{
  ColorIndex_t MeshColor;
  TecUtilLockStart(AddOnID);
  
  switch (*I)
    {
      case 1: default: MeshColor = Red_C; break;
      case 2: MeshColor = Yellow_C; break;
      case 3: MeshColor = Green_C; break;
    }

  /* $!FIELD MESH { COLOR = MeshColor } */

  TecUtilStyleSetLowLevel((Widget)NULL,
                          0.0,
                          (ArbParam_t) (ColorIndex_t) MeshColor,
                          0,
                          AssignOp_Equals,
                          SV_FIELD,
                          SV_MESH,
                          SV_COLOR,
                          NULL,
                          NULL,
                          NULL,
                          TRUE);


  TecUtilLockFinish(AddOnID);
}

void Refresh(Boolean_t Sensitive)
{
  Boolean_t DataIsAvailable;
  Boolean_t ShowContour;
  Boolean_t ShowMesh;
  int       MeshColor;

  TecUtilLockStart(AddOnID);

  if (TecGUIDialogIsUp(Dialog1Manager))
    {
      DataIsAvailable = TecUtilDataSetIsAvailable();
      ShowContour = (Boolean_t) TecGUIToggleGet(ShowContour_TOG_D1);
      ShowMesh    = (Boolean_t) TecGUIToggleGet(ShowMesh_TOG_D1);

      TecGUISetSensitivity(ShowMesh_TOG_D1,(Boolean_t)(Sensitive && DataIsAvailable));
      TecGUISetSensitivity(ShowContour_TOG_D1,(Boolean_t)(Sensitive && DataIsAvailable));
      TecGUISetSensitivity(ResetContour_BTN_D1,(Boolean_t)(Sensitive && DataIsAvailable && ShowContour));
      TecGUISetSensitivity(NumLevels_TF_D1,(Boolean_t)(Sensitive && DataIsAvailable && ShowContour));
      TecGUISetSensitivity(Levels_LBL_D1,(Boolean_t)(Sensitive && DataIsAvailable && ShowContour));
      TecGUISetSensitivity(MeshColor_OPT_D1,(Boolean_t)(Sensitive && DataIsAvailable && ShowMesh));
      TecGUISetSensitivity(MeshColor_LBL_D1,(Boolean_t)(Sensitive && DataIsAvailable && ShowMesh)); 

      if ( Sensitive && DataIsAvailable && ShowMesh )
        {
          /* Reset the mesh color */
          MeshColor = TecGUIOptionMenuGet(MeshColor_OPT_D1);
          MeshColor_OPT_D1_CB(&MeshColor); /* Fake the callback event */
        }
    }

  TecUtilLockFinish(AddOnID);
}
  
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}

static void Dialog1CloseButton_CB(void)
{
  TecGUIDialogDrop(Dialog1Manager);
}


static void Dialog1Init_CB(void)
{
  /* TODO: Add any initialization here. */
  TecGUIToggleSet(ShowMesh_TOG_D1,TRUE);
  TecGUIOptionMenuSet(MeshColor_OPT_D1,1);
  Refresh(TRUE);
}

static void ShowMesh_TOG_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  Refresh(TRUE);
  TecUtilFieldSetLayer(SV_SHOWMESH,(Boolean_t)*I);
  TecUtilLockFinish(AddOnID);
}




static void ShowContour_TOG_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  Refresh(TRUE);
  TecUtilFieldSetLayer(SV_SHOWCONTOUR,(Boolean_t)*I);
  TecUtilLockFinish(AddOnID);
}




static void ResetContour_BTN_D1_CB(void)
{
  int        NumLevels;
  char      *Levels;

  TecUtilLockStart(AddOnID);
  Levels = TecGUITextFieldGetString(NumLevels_TF_D1);
  NumLevels = atoi(Levels);
  TecUtilStringDealloc(&Levels);

  if ( NumLevels >= 1 )
    {
      EntIndex_t ContourVar = TecUtilVarGetNumByAssignment('C');
      if ( ContourVar >= 1 )
        TecUtilContourLevelReset(NumLevels);
      else
        TecUtilDialogErrMsg("Please set the contour variable");
    }
  else
    {
      TecUtilDialogErrMsg("Number of coutour levels must be at least 1");
    }

  TecUtilLockFinish(AddOnID);
}




static int  NumLevels_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}



char *MeshColor_OPT_D1_List = "Red,Yellow,Green";









#include "guibld.c"
