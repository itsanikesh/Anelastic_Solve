#include "TECADDON.h"


#include "ADDGLBL.h"
#include "GUIDEFS.h"

extern AddOn_pa AddOnID;

int *DialogControls[] =
  {
    &Zone_OPT_D1,
    &Variable_OPT_D1,
    &Zone_LBL_D1,
    &Variable_LBL_D1,
    &Numberofpoin_LBL_D1,
    &NumPoints_LBL_D1,
    &LocationLabe_LBL_D1,
    &Location_LBL_D1,
    &FieldDataTyp_LBL_D1,
    &FDType_LBL_D1,
    &I_OPT_D1,
    &I_LBL_D1,
    &J_OPT_D1,
    &J_LBL_D1,
    &K_OPT_D1,
    &K_LBL_D1,
    &Value_LBL_D1,
    &Value_TF_D1,
    &Set_BTN_D1
  };

static void UpdateDataInfo(Boolean_t SetValues)
{
  int Index;
  FieldData_pa FD;
  double Value;
  char strNum[100];

  int ZoneSel = TecGUIOptionMenuGet(Zone_OPT_D1);
  int VarSel  = TecGUIOptionMenuGet(Variable_OPT_D1);
  int ISel = TecGUIOptionMenuGet(I_OPT_D1);
  int JSel = TecGUIOptionMenuGet(J_OPT_D1);
  int KSel = TecGUIOptionMenuGet(K_OPT_D1);

  int IIndexBase;
  int JIndexBase;
  int KIndexBase;
  
  KIndexBase = (KSel - 1) * ISel * JSel;
  JIndexBase = (JSel - 1) * ISel;
  IIndexBase = (ISel - 1) * 1;

  Index = KIndexBase + JIndexBase + IIndexBase + 1;
  FD = TecUtilDataValueGetRef((EntIndex_t)ZoneSel,(EntIndex_t)VarSel);

  if ( SetValues )
    {
      Set_pa Set = TecUtilSetAlloc(FALSE);
      char *strValue = TecGUITextFieldGetString(Value_TF_D1);
      TecUtilDataValueSetByRef(FD,Index,atof(strValue));
      TecUtilSetAddMember(Set,TecGUIOptionMenuGet(Variable_OPT_D1),FALSE);
      TecUtilStateChanged(StateChange_VarsAltered,(ArbParam_t)Set);
      TecUtilSetDealloc(&Set);
      TecUtilStringDealloc(&strValue);
    }
  else
    {
      Value = TecUtilDataValueGetByRef(FD,Index);
      sprintf(strNum,"%g",Value);
      TecGUITextFieldSetString(Value_TF_D1,strNum);
    }

}

static void UpdateInfo(void)
{
  EntIndex_t ii;
  LgIndex_t IMax,JMax,KMax;
  char strNum[100];
  char strFD[100];

  EntIndex_t CurZone = TecGUIOptionMenuGet(Zone_OPT_D1);
  EntIndex_t CurVar  = TecGUIOptionMenuGet(Variable_OPT_D1);
  EntIndex_t CurI = TecGUIOptionMenuGet(I_OPT_D1);
  EntIndex_t CurJ = TecGUIOptionMenuGet(J_OPT_D1);
  EntIndex_t CurK = TecGUIOptionMenuGet(K_OPT_D1);

  TecUtilZoneGetInfo(CurZone,
                     &IMax,
                     &JMax,
                     &KMax,
                     NULL, /* XVAR */
                     NULL, /* YVAR */
                     NULL, /* ZVAR */
                     NULL, /* NMAP */
                     NULL, /* UVAR */
                     NULL, /* VVAR */
                     NULL, /* WVAR */
                     NULL, /* BVAR */
                     NULL, /* CVAR */
                     NULL); /* SVAR */

  sprintf(strNum,"%d",IMax * JMax * KMax);
  TecGUILabelSetText(NumPoints_LBL_D1,strNum);
  TecGUILabelSetText(Location_LBL_D1,
                     (TecUtilDataValueGetLocation(CurZone, CurVar) ==
                      ValueLocation_CellCentered)
                         ? "Cell Centered"
                         : "Nodal");

  TecGUIOptionMenuDeleteAllItems(I_OPT_D1);
  TecGUIOptionMenuDeleteAllItems(J_OPT_D1);
  TecGUIOptionMenuDeleteAllItems(K_OPT_D1);

  for (ii=1;ii<=IMax;ii++)
    {
      sprintf(strNum,"%d",ii);
      TecGUIOptionMenuAppendItem(I_OPT_D1,strNum);
    }

  for (ii=1;ii<=JMax;ii++)
    {
      sprintf(strNum,"%d",ii);
      TecGUIOptionMenuAppendItem(J_OPT_D1,strNum);
    }

  for (ii=1;ii<=KMax;ii++)
    {
      sprintf(strNum,"%d",ii);
      TecGUIOptionMenuAppendItem(K_OPT_D1,strNum);
    }

  TecGUIOptionMenuSet(I_OPT_D1,CurI <= IMax ? CurI : 1);
  TecGUIOptionMenuSet(J_OPT_D1,CurJ <= JMax ? CurJ : 1);
  TecGUIOptionMenuSet(K_OPT_D1,CurK <= KMax ? CurK : 1);

  if ( KMax == 1 )
    TecGUISetSensitivity(K_OPT_D1,FALSE);

  if ( JMax == 1 )
    TecGUISetSensitivity(J_OPT_D1,FALSE);

  switch (TecUtilDataValueGetRefType(TecUtilDataValueGetRef(CurZone,CurVar)))
    {
      case FieldDataType_Float:
        {
          sprintf(strFD,"Float");
        } break;

      case FieldDataType_Double:
        {
          sprintf(strFD,"Double");
        } break;

      case FieldDataType_LongInt:
        {
          sprintf(strFD,"Long Integer");
        } break;

      case FieldDataType_ShortInt:
        {
          sprintf(strFD,"Short Integer");
        } break;

      case FieldDataType_Byte:
        {
          sprintf(strFD,"Byte");
        } break;

      case FieldDataType_Bit:
        {
          sprintf(strFD,"Bit");
        } break;

      case FieldDataType_IJKFunction:
        {
          sprintf(strFD,"IJK Function");
        } break;

      default:
        {
          CHECK(FALSE);
        } break;

    }

  TecGUILabelSetText(FDType_LBL_D1,strFD);

}


void RefreshDialog(void)
{
  EntIndex_t ii;
  EntIndex_t NumZones;
  EntIndex_t NumVars;
  EntIndex_t CurZone;
  EntIndex_t CurVar;

  TecUtilLockStart(AddOnID);

  CurZone = TecGUIOptionMenuGet(Zone_OPT_D1);
  CurVar  = TecGUIOptionMenuGet(Variable_OPT_D1);

  TecGUIOptionMenuDeleteAllItems(Zone_OPT_D1);
  TecGUIOptionMenuDeleteAllItems(Variable_OPT_D1);

  if (!(TecUtilDataSetIsAvailable() &&
        TecUtilDataSetGetInfo(NULL,&NumZones,&NumVars)))
    {
      NumZones = 0;
      NumVars  = 0;
    }

  if (NumZones >= 1 && NumVars >= 1)
    {
      char *ZoneName  = NULL;
      char *VarName   = NULL;
      char  strZoneName[1000];

      for (ii=0;ii<sizeof(DialogControls) / sizeof(void *);ii++)
        TecGUISetSensitivity(*(DialogControls[ii]),TRUE);


      /* Fill the Zone list box */
      for (ii=1;ii<=NumZones;ii++)
        {
          TecUtilZoneGetName(ii,&ZoneName);
          sprintf(strZoneName,"%s %s",ZoneName,
                  TecUtilZoneIsFiniteElement(ii) ? "[FE]" : "");
          TecGUIOptionMenuAppendItem(Zone_OPT_D1,strZoneName);
          TecUtilStringDealloc(&ZoneName);
        }
      TecGUIOptionMenuSet(Zone_OPT_D1, CurZone <= NumZones ? CurZone : 1);

      /* Fill the var name list box */
      for (ii=1;ii<=NumVars;ii++)
        {
          TecUtilVarGetName(ii,&VarName);
          TecGUIOptionMenuAppendItem(Variable_OPT_D1,VarName);
          TecUtilStringDealloc(&VarName);
        }
      TecGUIOptionMenuSet(Variable_OPT_D1, CurVar <= NumVars ? CurVar : 1);

      UpdateInfo();
      UpdateDataInfo(FALSE);
    }
  else
    {
      TecGUIOptionMenuAppendItem(Zone_OPT_D1,"No Dataset");
      TecGUIOptionMenuAppendItem(Variable_OPT_D1,"No Dataset");
      TecGUIOptionMenuSet(Zone_OPT_D1,1);
      TecGUIOptionMenuSet(Variable_OPT_D1,1);

      TecGUIOptionMenuDeleteAllItems(I_OPT_D1);
      TecGUIOptionMenuDeleteAllItems(J_OPT_D1);
      TecGUIOptionMenuDeleteAllItems(K_OPT_D1);
      TecGUIOptionMenuAppendItem(I_OPT_D1,"1");
      TecGUIOptionMenuAppendItem(J_OPT_D1,"1");
      TecGUIOptionMenuAppendItem(K_OPT_D1,"1");
      TecGUIOptionMenuSet(I_OPT_D1,1);
      TecGUIOptionMenuSet(J_OPT_D1,1);
      TecGUIOptionMenuSet(K_OPT_D1,1);

      for (ii=0;ii<sizeof(DialogControls) / sizeof(void *);ii++)
        TecGUISetSensitivity(*(DialogControls[ii]),FALSE);
    }

  TecUtilLockFinish(AddOnID);
}

static void Dialog1HelpButton_CB(void)
{
  TecUtilLockOn();
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockOff();
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
  RefreshDialog();
  TecUtilLockFinish(AddOnID);
}



char *Zone_OPT_D1_List = "Option 1,Option 2,Option 3";




static void Zone_OPT_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  UpdateInfo();
  UpdateDataInfo(FALSE);
  TecUtilLockFinish(AddOnID);
}



char *Variable_OPT_D1_List = "Option 1,Option 2,Option 3";




static void Variable_OPT_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  UpdateInfo();
  UpdateDataInfo(FALSE);
  TecUtilLockFinish(AddOnID);
}



char *I_OPT_D1_List = "Option 1,Option 2,Option 3";




static void I_OPT_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  UpdateDataInfo(FALSE);
  TecUtilLockFinish(AddOnID);
}



char *J_OPT_D1_List = "Option 1,Option 2,Option 3";




static void J_OPT_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  UpdateDataInfo(FALSE);
  TecUtilLockFinish(AddOnID);
}



char *K_OPT_D1_List = "Option 1,Option 2,Option 3";




static void K_OPT_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  UpdateDataInfo(FALSE);
  TecUtilLockFinish(AddOnID);
}



static void Set_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);
  UpdateDataInfo(TRUE);
  TecUtilLockFinish(AddOnID);
}



static int  Value_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  /* Do nothing */
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}
#include "guibld.c"
