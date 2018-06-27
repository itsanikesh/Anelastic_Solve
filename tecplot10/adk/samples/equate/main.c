#include "TECADDON.h"
AddOn_pa AddOnID;


#include "GUIDEFS.h"
#include "ADDONVER.h"
 
/**
 */
static void STDCALL MenuCB(void)
{
  /* Since this is an MFC app under Windows. This is nothing under UNIX. */
  MANAGESTATE

  TecUtilLockStart(AddOnID);
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

/**
 */
/* DOCSTART:gr/equate_Compute.txt */
void Compute(double MulNum)
{
  LgIndex_t IMax;
  LgIndex_t JMax;
  LgIndex_t KMax;
  LgIndex_t i;
  LgIndex_t MaxIndex;
  FieldData_pa FD;
  double Value;
  Set_pa set;

  TecUtilLockStart(AddOnID);

  if (TecUtilZoneIsEnabled(1)                   &&
      TecUtilVarIsEnabled(1)                    &&
      TecUtilZoneGetType(1) == ZoneType_Ordered &&
      TecUtilDataValueGetLocation(1,1) == ValueLocation_Nodal)
    {
      /* Get the number of data points */
      TecUtilZoneGetInfo(1,     /* Zone */
                         &IMax,
                         &JMax,
                         &KMax,
                         NULL,  /* XVar */
                         NULL,  /* YVar */
                         NULL,  /* ZVar */
                         NULL,  /* NMap */
                         NULL,  /* UVar */
                         NULL,  /* VVar */
                         NULL,  /* WVar */
                         NULL,  /* BVar */
                         NULL,  /* CVar */
                         NULL); /* SVar */

      MaxIndex = IMax * JMax * KMax;

      FD = TecUtilDataValueGetRef(1,1);

      for (i = 1; i <= MaxIndex; i++)
        {
          /* Get the value */
          Value = TecUtilDataValueGetByRef(FD,i);

          /* Change it */
          Value *= MulNum;

          /* And set it back */
          TecUtilDataValueSetByRef(FD,i,Value);
        }

      /* Inform Tecplot that we've changed the data */
      set = TecUtilSetAlloc(FALSE);
      TecUtilSetAddMember(set,1,FALSE); /* Zone 1 */
      TecUtilStateChanged(StateChange_VarsAltered,
                          (ArbParam_t)set);
      TecUtilSetDealloc(&set);
    }
  else
    TecUtilDialogErrMsg("This sample add-on only performs an equation on "
                        "variable 1 of zone 1 and only if the zone is "
                        "ordered and the variable is node located.");

  TecUtilLockFinish(AddOnID);
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/equate_ProcessEquateCommand.txt */
Boolean_t STDCALL ProcessEquateCommand(char  *CommandString,
                                       char **ErrMsg)
{
  Boolean_t IsOk;

  TecUtilLockStart(AddOnID);

  IsOk = TecUtilDataSetIsAvailable();
  if (IsOk)
    {
      Compute(atof(CommandString));    
    }
  else
    {
      *ErrMsg = TecUtilStringAlloc(2000, "Error message");
      strcpy(*ErrMsg, "No data set available.");
    }

  TecUtilLockFinish(AddOnID);

  return IsOk;
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/equate_InitTecAddOn.txt */
EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{
  TecUtilLockOn();

  AddOnID = TecUtilAddOnRegister(
                100,
                ADDON_NAME,
                "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                "Joe Coder");

  if (TecUtilGetTecplotVersion() < MinTecplotVersionAllowed)
    {
      char buffer[256];
      sprintf(buffer, "Add-on \"%s\" requires Tecplot "
                      "version %s or greater.",
              ADDON_NAME, TecVersionId);
      TecUtilDialogErrMsg(buffer);
    }
  else
    {
      InitTGB();

      TecUtilMenuAddOption("Tools",
                           "Equate",
                           'E',
                           MenuCB);
      TecUtilMacroAddCommandCallback("equate", ProcessEquateCommand);
    }

  TecUtilLockOff();
}
/* DOCEND */

