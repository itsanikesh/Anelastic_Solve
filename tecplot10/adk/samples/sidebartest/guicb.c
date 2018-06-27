#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUI.h"
#include "GUIDEFS.h"






static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}




static void Dialog1CloseButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}




static void Dialog1Init_CB(void)
{
  TecUtilLockStart(AddOnID);

  if ( TecGUISidebarIsActive(Sidebar1Manager) )
    TecGUIRadioBoxSetToggle(Sidebar_RADIO_D1,2);

  else if ( TecGUISidebarIsActive(Sidebar2Manager) )
    TecGUIRadioBoxSetToggle(Sidebar_RADIO_D1,3);

  else if ( TecGUISidebarIsActive(Sidebar2Manager) )
    TecGUIRadioBoxSetToggle(Sidebar_RADIO_D1,4);
  
  else if ( TecGUISidebarIsActive(TECGUITECPLOTSIDEBAR) )
    TecGUIRadioBoxSetToggle(Sidebar_RADIO_D1,1);
  else
    TecGUIRadioBoxSetToggle(Sidebar_RADIO_D1,5);

  TecUtilLockFinish(AddOnID);
}


void InitSidebar(void)
{
  Set_pa EnabledZones = NULL;
  if (Dialog1Manager != BADDIALOGID  &&
      TecUtilDataSetIsAvailable()    &&
      TecUtilZoneGetEnabled(&EnabledZones))
    {
      EntIndex_t FirstEnabledZone = (EntIndex_t)TecUtilSetGetNextMember(EnabledZones, TECUTILSETNOTMEMBER);
      if (FirstEnabledZone != TECUTILSETNOTMEMBER)
        {
          LgIndex_t CurrentSidebar = TecGUIRadioBoxGetToggle(Sidebar_RADIO_D1);
          switch (CurrentSidebar)
            {
              case 1:
              case 5:
              default:
                {
                  /* Do nothing */
                } break;

              case 2: /* Mesh */
                {
                  TecGUIOptionMenuSet(LinePattern_OPT_S1,
                                      (LgIndex_t)TecUtilFieldStyleGetArbValue(FirstEnabledZone,
                                                                              SV_MESH,
                                                                              SV_LINEPATTERN,
                                                                              NULL)+1);

                  TecGUIOptionMenuSet(Type_OPT_S1,
                                      (LgIndex_t)TecUtilFieldStyleGetArbValue(FirstEnabledZone,
                                                                              SV_MESH,
                                                                              SV_LINEPATTERN,
                                                                              NULL)+1);

                  TecGUITextFieldSetDouble(PatternLength_TF_S1,
                                           TecUtilFieldStyleGetDoubleValue(FirstEnabledZone,
                                                                           SV_MESH,
                                                                           SV_PATTERNLENGTH,
                                                                           NULL),
                                           "%g");

                  TecGUITextFieldSetDouble(LineThickness_TF_S1,
                                           TecUtilFieldStyleGetDoubleValue(FirstEnabledZone,
                                                                           SV_MESH,
                                                                           SV_LINETHICKNESS,
                                                                           NULL),
                                           "%g");

                  TecGUIToggleSet(Show_TOG_S1,
                                  TecUtilFieldLayerIsActive(SV_SHOWMESH));

                } break;

              case 3: /* Contour */
                {
                  TecGUIToggleSet(Show_TOG_S2,
                                  TecUtilFieldLayerIsActive(SV_SHOWCONTOUR));

                  TecGUIOptionMenuSet(Type_OPT_S2,
                                      (LgIndex_t)TecUtilFieldStyleGetArbValue(FirstEnabledZone,
                                                                              SV_CONTOUR,
                                                                              SV_CONTOURTYPE,
                                                                              NULL)+1);

                } break;

              case 4: /* Shade */
                {
                  TecGUIToggleSet(Show_TOG_S3,
                                  TecUtilFieldLayerIsActive(SV_SHOWSHADE));
                } break;

            }
        }
    }

  TecUtilSetDealloc(&EnabledZones);
}
/**
 */
static void Sidebar3Activate_CB(void)
{
  TRACE0("Activate callback called for Shade sidebar.\n");
  InitSidebar();
  TecGUIRadioBoxSetToggle(Sidebar_RADIO_D1,4);

}


/**
 */
static void Sidebar3Deactivate_CB(void)
{
/*   <<< This function is called when sidebar "Shade" is deactivated >>> */
  TRACE0("Deactivate callback called for Shade sidebar.\n");


}


/**
 */
static void Sidebar2Activate_CB(void)
{
/*  <<< This function is called when sidebar "Contour" is activated >>> */
  TRACE0("Activate callback called for Contour sidebar.\n");  
  InitSidebar();
  TecGUIRadioBoxSetToggle(Sidebar_RADIO_D1,3);
}


/**
 */
static void Sidebar2Deactivate_CB(void)
{
/*   <<< This function is called when sidebar "Contour" is deactivated >>> */
  TRACE0("Dectivate callback called for Contour sidebar.\n");
}

static void Sidebar1Activate_CB(void)
{
/*  <<< This function is called when sidebar "Mesh" is activated >>> */
  TRACE0("Activate callback called for Mesh sidebar.\n");
  InitSidebar();
  TecGUIRadioBoxSetToggle(Sidebar_RADIO_D1,2);
}


/**
 */
static void Sidebar1Deactivate_CB(void)
{
/*   <<< This function is called when sidebar "Mesh" is deactivated >>> */
  TRACE0("Deactivate callback called for Mesh sidebar.\n");
  
}


static void Sidebar_RADIO_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  switch (*I)
    {
      case 1:
        {
          TecGUISidebarActivate(TECGUITECPLOTSIDEBAR);

        } break;

      case 2:
        {
          TecGUISidebarActivate(Sidebar1Manager);
        } break;

      case 3:
        {
          TecGUISidebarActivate(Sidebar2Manager);
        } break;

      case 4:
        {
          TecGUISidebarActivate(Sidebar3Manager);
        } break;

      case 5:
        {
          TecGUISidebarDeactivateAll();
        } break;

      default:
        {
          /* NOP */
        } break;

    }

  TecUtilLockFinish(AddOnID);
}

static void Show_TOG_S3_CB(const int *I)
{
  TecUtilLockStart(AddOnID);

  TecUtilFieldLayerSetIsActive(SV_SHOWSHADE, 
    (Boolean_t)*I);

  TecUtilLockFinish(AddOnID);
}




static void Black_BB_S1_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilZoneSetMesh(SV_COLOR,
    NULL,
    0.0,
    (ArbParam_t)Black_C);

  TecUtilLockFinish(AddOnID);
}




static void Blue_BB_S1_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilZoneSetMesh(SV_COLOR,
    NULL,
    0.0,
    (ArbParam_t)Blue_C);
  TecUtilLockFinish(AddOnID);
}




static void Gray_BB_S1_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilZoneSetMesh(SV_COLOR,
    NULL,
    0.0,
    (ArbParam_t)Custom1_C);
  TecUtilLockFinish(AddOnID);
}




static void Green_BB_S1_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilZoneSetMesh(SV_COLOR,
    NULL,
    0.0,
    (ArbParam_t)Green_C);
  TecUtilLockFinish(AddOnID);
}




static void Purple_BB_S1_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilZoneSetMesh(SV_COLOR,
    NULL,
    0.0,
    (ArbParam_t)Purple_C);
  TecUtilLockFinish(AddOnID);
}




static void Red_BB_S1_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilZoneSetMesh(SV_COLOR,
    NULL,
    0.0,
    (ArbParam_t)Red_C);
  TecUtilLockFinish(AddOnID);
}




static void White_BB_S1_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilZoneSetMesh(SV_COLOR,
    NULL,
    0.0,
    (ArbParam_t)White_C);
  TecUtilLockFinish(AddOnID);
}




static void Yellow_BB_S1_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilZoneSetMesh(SV_COLOR,
    NULL,
    0.0,
    (ArbParam_t)Yellow_C);
  TecUtilLockFinish(AddOnID);
}




static void Show_TOG_S1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  TecUtilFieldLayerSetIsActive(SV_SHOWMESH, 
    (Boolean_t)*I);
  TecUtilLockFinish(AddOnID);
}



char *Type_OPT_S1_List = "WIREFRAME,OVERLAY,HIDDENLINE";




static void Type_OPT_S1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  TecUtilZoneSetMesh(SV_MESHTYPE,
    NULL,
    0.0,
    (ArbParam_t)*I);
  TecUtilRedraw(TRUE);
  TecUtilLockFinish(AddOnID);
}



char *LinePattern_OPT_S1_List = 
  "SOLID,DASHED,DASHDOT,DOTTED,LONGDASH,DASHDOTDOT";




static void LinePattern_OPT_S1_CB(const int *I)
{

  TecUtilLockStart(AddOnID);
  TecUtilZoneSetMesh(SV_LINEPATTERN,
    NULL,
    0.0,
    (ArbParam_t)*I-1);
  TecUtilRedraw(TRUE);
  TecUtilLockFinish(AddOnID);
}




static int  PatternLength_TF_S1_CB(const char *S)
{
  int IsOk = 1;
  double d = atof(S);

  TecUtilLockStart(AddOnID);
  TecUtilZoneSetMesh(SV_PATTERNLENGTH,
    NULL,
    d,
    (ArbParam_t)0);
  TecUtilRedraw(TRUE);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}




static int  LineThickness_TF_S1_CB(const char *S)
{
  int IsOk = 1;
  double d = atof(S);

  TecUtilLockStart(AddOnID);
  TecUtilZoneSetMesh(SV_LINETHICKNESS,
    NULL,
    d,
    (ArbParam_t)0);
  TecUtilRedraw(TRUE);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}




static void Show_TOG_S2_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  TecUtilFieldLayerSetIsActive(SV_SHOWCONTOUR, 
    (Boolean_t)*I);
  TecUtilRedraw(TRUE);
  TecUtilLockFinish(AddOnID);
}



char *Type_OPT_S2_List = "LINES,FLOOD,OVERLAY,AVERAGECELL,CORNERCELL";




static void Type_OPT_S2_CB(const int *I)
{

  TecUtilLockStart(AddOnID);
  TecUtilZoneSetContour(SV_CONTOURTYPE,
    NULL,
    0.0,
    (ArbParam_t)*I-1);
  TecUtilLockFinish(AddOnID);
}

#include "guibld.c"
