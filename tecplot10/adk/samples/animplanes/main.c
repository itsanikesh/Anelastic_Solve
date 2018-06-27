#include "TECADDON.h"
AddOn_pa AddOnID;

#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include "SUTIL.h"



#define MINTECPLOTVERSIONALLOWED 800000

/**
 */
static void STDCALL MenuCallback(void)
{
  TecUtilLockStart(AddOnID);
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

/**
 * Fill the Zone multi-selection list with
 * all the zones in the current dataset
 */
/* DOCSTART:gr/animiplanes_FillZoneList.txt */
void FillZoneList(void)
{
  if (TecUtilDataSetIsAvailable())
    {
      EntIndex_t NumZones, i;

      TecUtilDataSetGetInfo(NULL, &NumZones, NULL);
      TecGUIListDeleteAllItems(ZoneList_MLST_D1);
      for (i = 1; i <= NumZones; i++)
        {
          char *ZoneName;
          TecUtilZoneGetName(i, &ZoneName);
          TecGUIListAppendItem(ZoneList_MLST_D1, ZoneName);
          TecUtilStringDealloc(&ZoneName);
        }
    }
  else
    TecGUIListDeleteAllItems(ZoneList_MLST_D1);
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/animiplanes_ZoneIsIJKOrdered.txt */
static Boolean_t ZoneIsIJKOrdered(EntIndex_t ZoneNum)
{
  Boolean_t IsOk;
  LgIndex_t IMax,JMax,KMax;

  TecUtilZoneGetInfo(ZoneNum,
                     &IMax,
                     &JMax,
                     &KMax,
                     NULL, /* XVar */
                     NULL, /* YVar */
                     NULL, /* ZVar */
                     NULL, /* NMap */
                     NULL, /* UVar */
                     NULL, /* VVar */
                     NULL, /* WVar */
                     NULL, /* BVar */
                     NULL, /* CVar */
                     NULL); /* SVar */
  IsOk = (IMax > 1 && JMax > 1 && KMax > 1);
  return IsOk;
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/animiplanes_GetIMaxFromCurZone.txt */
static LgIndex_t GetIMaxFromCurZone(EntIndex_t ZoneNum)
{
  LgIndex_t IMax;
  TecUtilZoneGetInfo(ZoneNum,
                     &IMax,
                     NULL, /* JMax */
                     NULL, /* KMax */
                     NULL, /* XVar */
                     NULL, /* YVar */
                     NULL, /* ZVar */
                     NULL, /* NMap */
                     NULL, /* UVar */
                     NULL, /* VVar */
                     NULL, /* WVar */
                     NULL, /* BVar */
                     NULL, /* CVar */
                     NULL); /* SVar */
  return IMax;
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/animiplanes_AnimatePlanes.txt */
void AnimatePlanes(Set_pa ZoneSet,
                   int    Skip)
{
  LgIndex_t   MaxIndex = 0;
  EntIndex_t  CurZone;
  SetIndex_t  NumberOfZonesInSet;
  SetIndex_t  Index;
  Set_pa      IJKZoneSet = TecUtilSetAlloc(TRUE);
  char       *strMacroCommand;


  /* Get the number of zones in ZoneSet */
  NumberOfZonesInSet = TecUtilSetGetMemberCount(ZoneSet);

  if (TecUtilMacroIsRecordingActive() &&
      (NumberOfZonesInSet >= 1))
    {
      strMacroCommand = TecUtilStringAlloc(2000, "Macro Command");
      strcpy(strMacroCommand, "ZONESET=");
    }


  /* 
   * Create a subset of ZoneSet that includes only 
   * IJK Ordered Zones.  Do this by looping through
   * all the zones in ZoneSet, check to see if the zone
   * is IJK Ordered.  Then add the zone to IJKZoneSet
   */
  for (Index = 1; Index <= NumberOfZonesInSet; Index++)
    {
      /* Get the current zone */
      CurZone = (EntIndex_t)TecUtilSetGetMember(ZoneSet, Index);

      /* Make sure the current zone is enabled */
      if (TecUtilZoneIsEnabled(CurZone))
        {
          /* Only add the zone if it is IJK ordered */
          if (ZoneIsIJKOrdered(CurZone))
            {
              TecUtilSetAddMember(IJKZoneSet, CurZone, TRUE);
              /* Find the greatest IMax of all the valid IJK ordered zones */
              MaxIndex = MAX(MaxIndex, GetIMaxFromCurZone(CurZone));
            }

          if (TecUtilMacroIsRecordingActive())
            {
              sprintf(&strMacroCommand[strlen(strMacroCommand)], "%d", CurZone);
              if (Index != NumberOfZonesInSet)
                strcat(strMacroCommand, ",");
            }
        }
    }


  /* Only proceed if there is at least one IJK ordered zone */
  if (TecUtilSetGetMemberCount(IJKZoneSet) >= 1)
    {
      Boolean_t IsOk = TRUE;

      /* Setup the zones for animation of I-Planes */

      /* Change the cell type to planes */
      TecUtilZoneSetIJKMode(SV_CELLTYPE,
                            NULL,
                            IJKZoneSet,
                            (ArbParam_t)IJKCellType_Planes);

      /* Display only the I-Planes */
      TecUtilZoneSetIJKMode(SV_PLANES,
                            NULL,
                            IJKZoneSet,
                            (ArbParam_t)Planes_I);

      /* Make sure that the Skip is greater than or equal to one. */
      if (Skip < 1)
        Skip = 1;

      /* Do the actual animation */
      TecUtilDoubleBuffer(DoubleBufferAction_On);
      for (Index = 1; IsOk && Index <=MaxIndex; Index += Skip)
        {
          /* 
           * Set the range of the I-Planes so that the
           * minimum I-Plane to display is the same as
           * the maximum displayed.  Then increment
           * by Skip.  This will make the I-Planes "move"
           */
          TecUtilZoneSetIJKMode(SV_IRANGE,
                                SV_MIN,
                                IJKZoneSet,
                                (ArbParam_t)Index);
          TecUtilZoneSetIJKMode(SV_IRANGE,
                                SV_MAX,
                                IJKZoneSet,
                                (ArbParam_t)Index);
          IsOk = TecUtilRedraw(TRUE);
          TecUtilDoubleBuffer(DoubleBufferAction_Swap);
        }
      TecUtilDoubleBuffer(DoubleBufferAction_Off);

      if (IsOk && TecUtilMacroIsRecordingActive())
        {
          /* At this point we have all the IJK ordered zones.
           * So all we need to add is the skip value.  Add a semi-colon
           * to the end to signify the end of the IJKZoneSet information.
           */
          strcat(strMacroCommand, "; ");
          sprintf(&strMacroCommand[strlen(strMacroCommand)], "SKIP=%d", Skip);
          strMacroCommand[strlen(strMacroCommand)] = '\0';

          /* Record the command */
          TecUtilMacroRecordAddOnCommand("animiplanes", strMacroCommand);
          TecUtilStringDealloc(&strMacroCommand);
        }
    }
  TecUtilSetDealloc(&IJKZoneSet);
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/animiplanes_AnimIPlanesStateChangeMonitor_Pre.txt */
void STDCALL AnimIPlanesStateChangeMonitor(StateChange_e StateChange,
                                           ArbParam_t    CallData)
{
  TecUtilLockStart(AddOnID);
  switch (StateChange)
    {
/* DOCEND */
/* DOCSTART:gr/animiplanes_AnimIPlanesStateChangeMonitor_Cases.txt */
      case StateChange_NewTopFrame  :
      case StateChange_ZonesAdded   :
      case StateChange_ZonesDeleted :
      case StateChange_FrameDeleted :
      case StateChange_ZoneName     :
      case StateChange_DataSetReset :
/* DOCEND */
/* DOCSTART:gr/animiplanes_AnimIPlanesStateChangeMonitor_Post.txt */
        {
          /* 
           * State changes may come in here while the dialog
           * is down.  We only want to fill the zone list
           * while the dialog is up.
           */
          if (TecGUIDialogIsUp(Dialog1Manager))
            FillZoneList();

        } break;
      default: break;
    }
  TecUtilLockFinish(AddOnID);
}
/* DOCEND */

/**
 */
Boolean_t STDCALL ProcessAnimIPlanesMacroCommand(char  *CommandString,
                                                 char **ErrMsg)
{
  Boolean_t  IsOk = TRUE;
  char      *Arg;
  Set_pa     ZoneSet = NULL;
  int        Skip = 1; /* default skip value */

  TecUtilLockStart(AddOnID);
  Arg = CommandString;

  Arg = strtok(CommandString, " ,=;");
  while (IsOk && Arg != NULL)
    {
      switch (toupper(Arg[0]))
        {
          case 'Z' :
            {
              /* Get everything from the '=' to the ';' into Arg */
              Arg = strtok(NULL, "=;");
              /* 
               * The function below is defined in sutil.c 
               * This function will parse a comma separated list
               * and place the members in a set
               */
              IsOk = Str_GetSet(Arg, &ZoneSet, FALSE);
              if (IsOk &&
                  TecUtilSetGetMemberCount(ZoneSet) < 1)
                {
                  IsOk = FALSE;
                  *ErrMsg = TecUtilStringAlloc(2000, "Error message");
                  strcpy(*ErrMsg, "Error -- No zones have been specified");
                }
              else if (!IsOk)
                {
                  *ErrMsg = TecUtilStringAlloc(2000, "Error message");
                  strcpy(*ErrMsg, "Error creating zone set.");
                }                           
            } break;
          case 'S' :
            {
              Arg = strtok(NULL, " ,=;");
              Skip = atoi(Arg);
            } break;
        }
      Arg = strtok(NULL, " =,;");
    }


  if (IsOk)
    AnimatePlanes(ZoneSet, Skip);

  if (ZoneSet != NULL)
    TecUtilSetDealloc(&ZoneSet);

  TecUtilLockFinish(AddOnID);
  return IsOk;
}

/**
 * When Tecplot first loads an add-on, it makes a call to initialize the
 * add-on. This function must be named InitTecAddOn, as shown below.
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
                                 "Amtec Engineering, Inc.");

  if (TecUtilGetTecplotVersion() < MINTECPLOTVERSIONALLOWED)
    {
      char buffer[256];
      sprintf(buffer, 
              "Add-On \"%s\" requires Tecplot version %s or greater.", 
              ADDON_NAME,
              TecVersionId);
      TecUtilDialogErrMsg(buffer);
    }
  else
    {
      InitTGB();      
      TecUtilMenuAddOption("Tools",
                           "Animate I Planes...",
                           'P',
                           MenuCallback);
      TecUtilStateChangeAddCallback(AnimIPlanesStateChangeMonitor);
      TecUtilStateChangeSetMode(AnimIPlanesStateChangeMonitor, StateChangeMode_v80);
      TecUtilMacroAddCommandCallback("animiplanes", ProcessAnimIPlanesMacroCommand);
    }

  TecUtilLockOff();
}
