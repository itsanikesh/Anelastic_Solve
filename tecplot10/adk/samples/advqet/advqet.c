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

#include <assert.h>
#include <errno.h>
#include "ADDONVER.h"
#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUI.h"
#include "GUIDEFS.h"
 

#define MAX_BUFFER_LEN 255

/* keep track of the frame plot type each time the dialog is updated */
AddOn_pa AddOnID;
static PlotType_e LastPlotType = PlotType_Sketch;


/*
 * Return a duplicate of the string.
 */
char *StringDup(const char *String)
{
  char *Result = NULL;

  assert(String != NULL);

  Result = (char *)malloc(strlen(String) + 1);
  strcpy(Result, String);

  assert(Result != NULL && strcmp(Result, String) == 0);
  return Result;
}


/*
 * Remove any leading white space from the string and return
 * a reference to it. NOTE: The input string is modified.
 */
char *StringFlushLeft(char *String)
{
  char *Result = String;
  char *Start = String;

  assert(String != NULL);

  /* move the substring beginning at the first non-whitespace */
  /* character to the head of the string                      */
  while (isspace(*Start))
    Start++;
  if (Start != String)
    memmove(String, Start, strlen(Start) + 1);

  assert(Result != NULL && Result == String);
  return Result;
}


/*
 * Remove any trailing white space from the string and return
 * a reference to it. NOTE: The input string is modified.
 */
char *StringFlushRight(char *String)
{
  char *Result = String;
  char *End = NULL;
  
  assert(String != NULL);

  for (End = &String[strlen(String)]; End != String && isspace(End[-1]); End--)
    End[-1] = '\0';

  assert(Result != NULL && Result == String);
  return Result;
}


/*
 * Remove any leading or trailing white space from the string and return
 * a reference to it. NOTE: The input string is modified.
 */
char *StringFlush(char *String)
{
  char *Result = StringFlushRight(StringFlushLeft(String));

  assert(String != NULL);

  assert(Result != NULL && Result == String);
  return Result;
}


static void GeomControlsSetSensitivity(Boolean_t Sensitive)
{
  assert(Sensitive == TRUE || Sensitive == FALSE);

  TecUtilLockStart(AddOnID);
  TecGUISetSensitivity(Geometry_LBL_D1, Sensitive);
  TecGUISetSensitivity(GeomFPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(GeomGPB_BTN_D1, Sensitive);
  TecUtilLockFinish(AddOnID);
}


static void TextControlsSetSensitivity(Boolean_t Sensitive)
{
  assert(Sensitive == TRUE || Sensitive == FALSE);

  TecUtilLockStart(AddOnID);
  TecGUISetSensitivity(Text_LBL_D1, Sensitive);
  TecGUISetSensitivity(TextFFPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(TextFPPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(TextGGPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(TextGFPB_BTN_D1, Sensitive);

  TecGUISetSensitivity(TextBoxMargi_BTN_D1, Sensitive);
  TecGUISetSensitivity(TextLineSpac_BTN_D1, Sensitive);

  TecGUISetSensitivity(Left_LBL_D1, Sensitive);
  TecGUISetSensitivity(Cntr_LBL_D1, Sensitive);
  TecGUISetSensitivity(Right_LBL_D1, Sensitive);
  TecGUISetSensitivity(Head_LBL_D1, Sensitive);
  TecGUISetSensitivity(Mid_LBL_D1, Sensitive);
  TecGUISetSensitivity(Base_LBL_D1, Sensitive);
  TecGUISetSensitivity(TextHLPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(TextHCPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(TextHRPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(TextMLPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(TextMCPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(TextMRPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(TextBLPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(TextBCPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(TextBRPB_BTN_D1, Sensitive);
  TecUtilLockFinish(AddOnID);
}


static void GeneralControlsSetSensitivity(Boolean_t Sensitive)
{
  Boolean_t AttachmentSensitive;

  assert(Sensitive == TRUE || Sensitive == FALSE);

  TecUtilLockStart(AddOnID);
  TecGUISetSensitivity(Scope_LBL_D1, Sensitive);
  TecGUISetSensitivity(ScopeLPB_BTN_D1, Sensitive);
  TecGUISetSensitivity(ScopeGPB_BTN_D1, Sensitive);

  AttachmentSensitive = TecUtilDataSetIsAvailable() && Sensitive;

  TecGUISetSensitivity(Attachment_LBL_D1, AttachmentSensitive);
  TecGUISetSensitivity(Select_BTN_D1, AttachmentSensitive);
  TecUtilLockFinish(AddOnID);
}


void MainDialogSetSensitivities(void)
{
  int           Count = 0;
  int           Index = 0;
  PickObjects_e ObjectType = PickObject_Invalid;

  TecUtilLockStart(AddOnID);

  /* first make all controls insensitive */
  GeomControlsSetSensitivity(FALSE);
  TextControlsSetSensitivity(FALSE);
  GeneralControlsSetSensitivity(FALSE);

  /* turn on each set of controls as appropriate */
  for (Index = 0, Count = TecUtilPickListGetCount(); Index < Count; Index++)
    {
      ObjectType = TecUtilPickListGetType(Index + 1);
      if (ObjectType == PickObject_Geom)
        {
          GeomControlsSetSensitivity(TRUE);
          GeneralControlsSetSensitivity(TRUE);
        }
      else if (ObjectType == PickObject_Text)
        {
          TextControlsSetSensitivity(TRUE);
          GeneralControlsSetSensitivity(TRUE);
        }
    }

  TecUtilLockFinish(AddOnID);
}


void PickedObjectChangeScope(Scope_e Scope)
{
  int       Count = 0;
  int       Index = 0;
  Boolean_t TextChanged = FALSE;
  Boolean_t GeomChanged = FALSE;

  assert(Scope == Scope_Global ||
         Scope == Scope_Local);

  TecUtilLockStart(AddOnID);

  for (Index = 0, Count = TecUtilPickListGetCount(); Index < Count; Index++)
    {
      if (TecUtilPickListGetType(Index + 1) == PickObject_Geom)
        {
          GeomChanged = TRUE;
          TecUtilGeomSetScope(TecUtilPickListGetGeom(Index + 1), Scope);
        }
      else if (TecUtilPickListGetType(Index + 1) == PickObject_Text)
        {
          TextChanged = TRUE;
          TecUtilTextSetScope(TecUtilPickListGetText(Index + 1), Scope);
        }
    }

  if (GeomChanged)
    TecUtilStateChanged(StateChange_Geom, 0);
  if (TextChanged)
    TecUtilStateChanged(StateChange_Text, 0);

  TecUtilRedraw(TRUE);

  TecUtilLockFinish(AddOnID);
}


void PickedGeomChangeCoordSys(CoordSys_e CoordSys)
{
  int        Count = 0;
  int        Index = 0;
  Geom_ID    GID = TECUTILBADID;
  Boolean_t  GeomChanged = FALSE;

  assert(CoordSys == CoordSys_Grid ||
         CoordSys == CoordSys_Frame);

  TecUtilLockStart(AddOnID);

  for (Index = 0, Count = TecUtilPickListGetCount(); Index < Count; Index++)
    if (TecUtilPickListGetType(Index + 1) == PickObject_Geom)
      {
        GeomChanged = TRUE;
        GID = TecUtilPickListGetGeom(Index + 1);
        TecUtilGeomSetPositionCoordSys(GID, CoordSys);
      }

  if (GeomChanged)
    TecUtilStateChanged(StateChange_Geom, 0);

  TecUtilRedraw(TRUE);

  TecUtilLockFinish(AddOnID);
}


void PickedTextChangeCoordSysAndSizeUnits(CoordSys_e CoordSys,
                                          Units_e    Units)
{
  int        Count = 0;
  int        Index = 0;
  Text_ID    TID = TECUTILBADID;
  Boolean_t  TextChanged = FALSE;

  assert(CoordSys == CoordSys_Grid ||
         CoordSys == CoordSys_Frame);
  assert(Units == Units_Grid ||
         Units == Units_Frame ||
         Units == Units_Point);

  TecUtilLockStart(AddOnID);

  for (Index = 0, Count = TecUtilPickListGetCount(); Index < Count; Index++)
    if (TecUtilPickListGetType(Index + 1) == PickObject_Text)
      {
        TextChanged = TRUE;
        TID = TecUtilPickListGetText(Index + 1);
        TecUtilTextSetCoordSysAndUnits(TID,CoordSys,Units);
      }

  if (TextChanged)
    TecUtilStateChanged(StateChange_Text, 0);

  TecUtilRedraw(TRUE);

  TecUtilLockFinish(AddOnID);
}


void PickedTextChangeAnchor(TextAnchor_e TextAnchor)
{
  int       Count = 0;
  int       Index = 0;
  Boolean_t TextChanged = FALSE;

  assert(TextAnchor == TextAnchor_Left ||
         TextAnchor == TextAnchor_Center ||
         TextAnchor == TextAnchor_Right ||
         TextAnchor == TextAnchor_MidLeft ||
         TextAnchor == TextAnchor_MidCenter ||
         TextAnchor == TextAnchor_MidRight ||
         TextAnchor == TextAnchor_HeadLeft ||
         TextAnchor == TextAnchor_HeadCenter ||
         TextAnchor == TextAnchor_HeadRight);

  TecUtilLockStart(AddOnID);

  for (Index = 0, Count = TecUtilPickListGetCount(); Index < Count; Index++)
    if (TecUtilPickListGetType(Index + 1) == PickObject_Text)
      {
        TextChanged = TRUE;
        TecUtilTextSetAnchor(TecUtilPickListGetText(Index + 1), TextAnchor);
      }

  if (TextChanged)
    TecUtilStateChanged(StateChange_Text, 0);

  TecUtilRedraw(TRUE);

  TecUtilLockFinish(AddOnID);
}


void PickedTextChangeMargin(const char *MarginString)
{
  int       Count = 0;
  int       Index = 0;
  double    Margin = 0.0;
  char      *TrimmedMarginString = NULL;
  char      *EndPtr = NULL;
  Boolean_t TextChanged = FALSE;

  assert(MarginString != NULL);

  TecUtilLockStart(AddOnID);

  errno = 0;
  TrimmedMarginString = StringFlush(StringDup(MarginString));
  if (strlen(TrimmedMarginString) != 0)
    {
      Margin = strtod(TrimmedMarginString, &EndPtr);
      if (*EndPtr == '\0' && errno == 0 && Margin >= 0.0)
        {
            for (Index = 0, Count = TecUtilPickListGetCount();
                 Index < Count;
                 Index++)
              {
                  if (TecUtilPickListGetType(Index + 1) == PickObject_Text)
                    {
                      TextChanged = TRUE;
                      TecUtilTextBoxSetMargin(TecUtilPickListGetText(Index + 1),
                                              Margin);
                    }
              }

          if (TextChanged)
            TecUtilStateChanged(StateChange_Text, 0);

          TecUtilRedraw(TRUE);
        }
    }
  free(TrimmedMarginString);

  TecUtilLockFinish(AddOnID);
}


void PickedTextChangeLineSpacing(const char *LineSpacingString)
{
  int       Count = 0;
  int       Index = 0;
  double    LineSpace = 0.0;
  char      *TrimmedLineSpacingString = NULL;
  char      *EndPtr = NULL;
  Boolean_t TextChanged = FALSE;

  assert(LineSpacingString != NULL);

  TecUtilLockStart(AddOnID);

  errno = 0;
  TrimmedLineSpacingString = StringFlush(StringDup(LineSpacingString));
  if (strlen(TrimmedLineSpacingString) != 0)
    {
      LineSpace = strtod(TrimmedLineSpacingString, &EndPtr);
      if (*EndPtr == '\0' && errno == 0 && LineSpace > 0.0)
        {
          for (Index = 0, Count = TecUtilPickListGetCount();
               Index < Count;
               Index++)
            {
              if (TecUtilPickListGetType(Index + 1) == PickObject_Text)
                {
                  TextChanged = TRUE;
                  TecUtilTextSetLineSpacing(TecUtilPickListGetText(Index + 1),
                                            LineSpace);
                }
            }

          if (TextChanged)
            TecUtilStateChanged(StateChange_Text, 0);

          TecUtilRedraw(TRUE);
        }
    }
  free(TrimmedLineSpacingString);

  TecUtilLockFinish(AddOnID);
}


void PickedListChangeZoneOrMapAttachment(int Selection)
{
  Boolean_t     AttachToZone = FALSE;
  int           Count = 0;
  int           Index = 0;
  EntIndex_t    Zone = 0;
  char          *ItemString = NULL;
  PickObjects_e PickedObjectType = PickObject_Invalid;
  Boolean_t     TextChanged = FALSE;
  Boolean_t     GeomChanged = FALSE;

  assert(Selection >= 1);

  TecUtilLockStart(AddOnID);

  AttachToZone = (Selection >= 2);
  for (Index = 0, Count = TecUtilPickListGetCount(); Index < Count; Index++)
    {
      PickedObjectType = TecUtilPickListGetType(Index + 1);
      if (PickedObjectType == PickObject_Geom)
        {
          Geom_ID GID = TecUtilPickListGetGeom(Index + 1);
          GeomChanged = TRUE;
          TecUtilGeomSetAttached(GID, AttachToZone);
          if (AttachToZone)
            {
              ItemString = TecGUIListGetString(ZoneOrMapLi_SLST_D2, Selection);
              Zone = (EntIndex_t)strtol(ItemString, NULL, 10);
              TecUtilStringDealloc(&ItemString);
              TecUtilGeomSetZoneOrMap(GID, Zone);
            }
        }
      else if (PickedObjectType == PickObject_Text)
        {
          Text_ID TID = TecUtilPickListGetText(Index + 1);
          TextChanged = TRUE;
          TecUtilTextSetAttached(TID, AttachToZone);
          if (AttachToZone)
            {
              ItemString = TecGUIListGetString(ZoneOrMapLi_SLST_D2, Selection);
              Zone = (EntIndex_t)strtol(ItemString, NULL, 10);
              TecUtilStringDealloc(&ItemString);
              TecUtilTextSetZoneOrMap(TID, Zone);
            }
        }
    }

  if (GeomChanged)
    TecUtilStateChanged(StateChange_Geom, 0);
  if (TextChanged)
    TecUtilStateChanged(StateChange_Text, 0);

  TecUtilRedraw(TRUE);

  TecUtilLockFinish(AddOnID);
}


void UpdateMainDialog(void)
{
  PlotType_e PlotType = PlotType_Invalid;

  TecUtilLockStart(AddOnID);

  PlotType = TecUtilFrameGetPlotType();
  LastPlotType = PlotType; /* keep track of frame plot type for this update */

  if (PlotType != PlotType_XYLine)
    TecGUILabelSetText(Attachment_LBL_D1, "Zone Attachment:");
  else
    TecGUILabelSetText(Attachment_LBL_D1, "XY Map Attachment:");

  TecUtilLockFinish(AddOnID);
}


void UpdateAttachmentDialog(void)
{
  char        *DataSetTitle = NULL;
  char        *ZoneOrMapName = NULL;
  EntIndex_t  ZoneOrMap = 0;
  EntIndex_t  ZoneOrMapCount = 0;
  EntIndex_t  VarCount = 0;
  Set_pa      EnabledZones = NULL;
  char        ItemString[MAX_BUFFER_LEN+1];
  PlotType_e  PlotType = PlotType_Invalid;

  TecUtilLockStart(AddOnID);

  TecGUIListDeleteAllItems(ZoneOrMapLi_SLST_D2);
  TecGUIListAppendItem(ZoneOrMapLi_SLST_D2, "<Unattach Object>");

  PlotType = TecUtilFrameGetPlotType();
  if (PlotType != PlotType_XYLine && TecUtilDataSetIsAvailable())
    TecUtilZoneGetEnabled(&EnabledZones);

  if (PlotType == PlotType_XYLine || EnabledZones != NULL)
    {
      if (PlotType != PlotType_XYLine)
        {
          TecGUILabelSetText(SelectAttach_LBL_D2, "Select Zone Attachment:");
          TecUtilDataSetGetInfo(&DataSetTitle, &ZoneOrMapCount, &VarCount);
        }
      else
        {
          TecGUILabelSetText(SelectAttach_LBL_D2, "Select XY Map Attachment:");
          ZoneOrMapCount = TecUtilXYMapGetCount();
        }

      if (DataSetTitle != NULL)
        TecUtilStringDealloc(&DataSetTitle);
      for (ZoneOrMap = 1; ZoneOrMap <= ZoneOrMapCount; ZoneOrMap++)
        {
          if (PlotType == PlotType_XYLine ||
              TecUtilSetIsMember(EnabledZones, ZoneOrMap))
            {
              sprintf(ItemString, "%d", ZoneOrMap);
              if (PlotType != PlotType_XYLine)
                TecUtilZoneGetName(ZoneOrMap, &ZoneOrMapName);
              else
                TecUtilXYMapGetName(ZoneOrMap, &ZoneOrMapName);

              if (ZoneOrMapName != NULL)
                {
                  strcat(ItemString, ":");
                  strcat(ItemString, ZoneOrMapName);
                  assert(strlen(ItemString) <= MAX_BUFFER_LEN);

                  TecGUIListAppendItem(ZoneOrMapLi_SLST_D2, ItemString);
                  TecUtilStringDealloc(&ZoneOrMapName);
                }
            }
        }
      if (PlotType != PlotType_XYLine)
        TecUtilSetDealloc(&EnabledZones);
    }

  TecUtilLockFinish(AddOnID);
}


static void STDCALL StateChangeListener(StateChange_e StateChange,
                                        ArbParam_t    CallData)
{
  TecUtilLockStart(AddOnID);

  /* only listen to events if the dialog is up */
  if (TecGUIDialogIsUp(Dialog1Manager))
    {
      if (StateChange == StateChange_PickListCleared      ||
          StateChange == StateChange_PickListGroupSelect  ||
          StateChange == StateChange_PickListSingleSelect ||
          StateChange == StateChange_PickListStyle        ||
          StateChange == StateChange_Text                 ||
          StateChange == StateChange_Geom)
        {
          /* update the main dialog with current settings and set up the */
          /* control sensitivities based on what is currently selected   */
          UpdateMainDialog();
          MainDialogSetSensitivities();
        }
      else if (StateChange == StateChange_ZonesDeleted            ||
               StateChange == StateChange_ZonesAdded              ||
               StateChange == StateChange_ZoneName                ||
               StateChange == StateChange_XYMapAddDeleteOrReorder ||
               StateChange == StateChange_XYMapName               ||
               StateChange == StateChange_FrameDeleted            ||
               StateChange == StateChange_NewTopFrame             ||
               StateChange == StateChange_DataSetReset            ||
               StateChange == StateChange_NewLayout               ||
               StateChange == StateChange_CompleteReset           ||
               (StateChange == StateChange_Style &&
                TecUtilFrameGetPlotType() != LastPlotType))
        {
          /* update the main and attachment dialogs with current settings and */
          /* set control sensitivities based on what is currently selected    */
          UpdateMainDialog();
          if (TecGUIDialogIsUp(Dialog2Manager))
              UpdateAttachmentDialog();
          MainDialogSetSensitivities();
        }
    }

  TecUtilLockFinish(AddOnID);
}


static void STDCALL advqetMenuCB(void)
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
                                 "Amtec Engineering Inc.");
  

  if (TecUtilGetTecplotVersion() < MinTecplotVersionAllowed)
  {
    char buffer[1000];
    sprintf(buffer,"Add-on \"%s\" requires Tecplot version %s or greater",ADDON_NAME,TecVersionId);
    TecUtilDialogErrMsg(buffer);
  }

  else
  {
    InitTGB();
  
    TecUtilMenuAddOption("Tools",
      ADDON_NAME,
      'E',
      advqetMenuCB);
  
    /* install a listener of picked objects */
    TecUtilStateChangeAddCallback(StateChangeListener);
  }
  
  
  TecUtilLockOff();
}
