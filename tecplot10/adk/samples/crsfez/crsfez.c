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
#include <assert.h>
#include "TECADDON.h"
#include "ADDONVER.h"
#include "ADDGLBL.h"
#include "GUI.h"
#include "GUIDEFS.h"
#include "LIST.h"
#include "CRSFEZ.h"

AddOn_pa AddOnID;
typedef struct
  {
    double XMin;
    double XMax;
    double YMin;
    double YMax;
  } DataViewRegion_s;

typedef struct
  {
    EntIndex_t   Zone;
    LgIndex_t    ElementCount;
    LgIndex_t    CornersPerElement;
    FieldData_pa X;
    FieldData_pa Y;
    NodeMap_pa   NodeMap;
    List_pa      DesiredElements;
  } FEZoneInfo_s;


static DataViewRegion_s *GetDataViewRegion(void)
{
  DataViewRegion_s *Result = NULL;

  Result = (DataViewRegion_s *)malloc(sizeof(DataViewRegion_s));
  assert(Result != NULL);

  TecUtilAxisGetRange('X', 1, &Result->XMin, &Result->XMax);
  TecUtilAxisGetRange('Y', 1, &Result->YMin, &Result->YMax);

  assert(Result != NULL);
  return Result;
}


static void ReleaseDataViewRegion(DataViewRegion_s **DataViewRegion)
{
  assert(DataViewRegion != NULL && *DataViewRegion != NULL);

  free(*DataViewRegion);
  *DataViewRegion = NULL;
}


static void ClearFEZoneInfoList(List_pa FEZoneInfoList)
{
  LgIndex_t Index = 0;
  LgIndex_t Count = 0;

  assert(ListIsValid(FEZoneInfoList));

  /* release each member and set it's count to zero */
  for (Index = 0, Count = ListGetCount(FEZoneInfoList);
       Index < Count;
       Index++)
    {
      FEZoneInfo_s *FEZoneInfo = ListGetItemRef(FEZoneInfoList, Index);
      ListDestroy(&FEZoneInfo->DesiredElements);
      free(FEZoneInfo);
    }
  ListSetCount(FEZoneInfoList, 0);
}


static void ReleaseFEZoneInfoList(List_pa *FEZoneInfoList)
{
  assert(FEZoneInfoList != NULL && ListIsValid(*FEZoneInfoList));

  /* release each member and the list itself */
  ClearFEZoneInfoList(*FEZoneInfoList);
  ListDestroy(FEZoneInfoList);
}


static List_pa GetFEZoneInfoList(void)
{
  Set_pa     ActiveZones = NULL;
  Boolean_t  FoundFirstFEZone = FALSE;
  ZoneType_e FirstFEZoneType = ZoneType_Invalid;
  SetIndex_t ZIndex;
  List_pa    FEZoneInfoList = ListCreate();

  assert(TecUtilDataSetIsAvailable());

  TecUtilZoneGetActive(&ActiveZones);
  assert(ActiveZones != NULL);

  /* cycle through active zones with a TecUtil convenience macro */
  TecUtilSetForEachMember(ZIndex, ActiveZones)
    {
      EntIndex_t Zone = (EntIndex_t)ZIndex;
      if (TecUtilZoneIsFiniteElement(Zone))
        {
          ZoneType_e FEZoneType = TecUtilZoneGetType(Zone);
          if (!FoundFirstFEZone)
            {
              FoundFirstFEZone = TRUE;
              FirstFEZoneType  = FEZoneType;
            }
          if (FEZoneType == FirstFEZoneType)
            {
              FEZoneInfo_s *FEZoneInfo = malloc(sizeof(FEZoneInfo_s));
              assert(FEZoneInfo != NULL);
              FEZoneInfo->Zone = Zone;
              FEZoneInfo->DesiredElements = ListCreate();
              TecUtilZoneGetInfo(Zone, NULL,
                                 &FEZoneInfo->ElementCount,
                                 &FEZoneInfo->CornersPerElement,
                                 &FEZoneInfo->X, &FEZoneInfo->Y,
                                 NULL, &FEZoneInfo->NodeMap, NULL,
                                 NULL, NULL, NULL, NULL, NULL);
              ListAppendItemRef(FEZoneInfoList, FEZoneInfo);
            }
          else
            {
              /* empty (but not destroy) the zone info list and report error */
              ClearFEZoneInfoList(FEZoneInfoList);
              TecUtilDialogErrMsg("Mixed zone types are not allowed.");
              break;
            }
        }
    }

  if (!FoundFirstFEZone)
    {
      TecUtilDialogErrMsg("No Active Finite Element Zones\n"
                          "are in the current data view.");
    }

  TecUtilSetDealloc(&ActiveZones);

  assert(ListIsValid(FEZoneInfoList));
  return FEZoneInfoList;
}


static Boolean_t PointWithinDataViewRegion(DataViewRegion_s *DataViewRegion,
                                           LgIndex_t        PointIndex,
                                           FieldData_pa     X,
                                           FieldData_pa     Y)
{
  Boolean_t Result = FALSE;
  double    XOrdinate = 0.0;
  double    YOrdinate = 0.0;

  assert(DataViewRegion != NULL);
  assert(PointIndex >= 1);
  assert(X != NULL);
  assert(Y != NULL);

  XOrdinate = TecUtilDataValueGetByRef(X, PointIndex);
  YOrdinate = TecUtilDataValueGetByRef(Y, PointIndex);

  Result = ((DataViewRegion->XMin <= XOrdinate &&
                                     XOrdinate <= DataViewRegion->XMax) &&
            (DataViewRegion->YMin <= YOrdinate &&
                                     YOrdinate <= DataViewRegion->YMax));

  assert(Result == TRUE || Result == FALSE);
  return Result;
}


static FieldData_pa *GetVarCache(EntIndex_t Zone)
{
  char         *DataSetTitle = NULL;
  EntIndex_t   Var = 0;
  EntIndex_t   Index = 0;
  EntIndex_t   NumZones = 0;
  EntIndex_t   NumVars = 0;
  FieldData_pa *Result = NULL;
  Set_pa       EnabledVars = NULL;

  /* create variable cache to improve assignment performance */
  TecUtilDataSetGetInfo(&DataSetTitle, &NumZones, &NumVars);
  TecUtilStringDealloc(&DataSetTitle);
  Result = (FieldData_pa *)malloc(NumVars * sizeof(FieldData_pa));
  assert(Result != NULL);
  TecUtilVarGetEnabled(&EnabledVars);

  Index = 0;
  TecUtilSetForEachMember(Var, EnabledVars)
    {
      Result[Index] = TecUtilDataValueGetRef(Zone, Var);
      Index++;
    }

  TecUtilSetDealloc(&EnabledVars);

  assert(Result != NULL);
  return Result;
}


static void ExtractSubFEZone(DataViewRegion_s *DataViewRegion,
                             List_pa           FEZoneInfoList)
{
  EntIndex_t    Zone = 0;
  LgIndex_t     Index = 0;
  LgIndex_t     Count = 0;
  LgIndex_t     ElementCount = 0;
  LgIndex_t     Element = 0;
  LgIndex_t     Corner = 0;
  LgIndex_t     PointIndex = 0;
  FEZoneInfo_s  *FEZoneInfo = NULL;
  Boolean_t     PointWithin = FALSE;
  LgIndex_t     TotalElementCount = 0;
  char          *DataSetTitle = NULL;
  EntIndex_t    Var = 0;
  EntIndex_t    NumZones = 0;
  EntIndex_t    NumVars = 0;
  EntIndex_t    CornersPerElement = 0;
  ZoneType_e    ZoneType = ZoneType_Invalid;
  Boolean_t     IsOk = FALSE;

  assert(DataViewRegion != NULL);
  assert(ListIsValid(FEZoneInfoList) && ListGetCount(FEZoneInfoList) >= 1);

  for (Index = 0, Count = ListGetCount(FEZoneInfoList); Index < Count; Index++)
    {
      FEZoneInfo = ListGetItemRef(FEZoneInfoList, Index);
      assert(FEZoneInfo != NULL);
      if (Index == 0)
        {
          /* all zones are of the same type so get info from the first one */
          CornersPerElement = FEZoneInfo->CornersPerElement;
          ZoneType = TecUtilZoneGetType(FEZoneInfo->Zone);
        }

      for (Element = 1; Element <= FEZoneInfo->ElementCount; Element++)
        {
          /* cycle through all element nodes and determine */
          /* if they all are within the data view region   */
          for (Corner = 1, PointWithin = TRUE;
               Corner <= CornersPerElement && PointWithin;
               Corner++)
            {
              PointIndex = TecUtilDataNodeGetByRef(FEZoneInfo->NodeMap,
                                                   Element, Corner);
              PointWithin = PointWithinDataViewRegion(DataViewRegion,
                                                      PointIndex,
                                                      FEZoneInfo->X,
                                                      FEZoneInfo->Y);
            }

          /* if all points are within the region the element is also */
          if (PointWithin)
            {
              TotalElementCount++;
              ListAppendItemRef(FEZoneInfo->DesiredElements, (void *)Element);
            }
        }
    }

  if (TotalElementCount >= 1)
    {
      IsOk = TecUtilDataSetAddZone("SubFEZone",
                                   TotalElementCount*CornersPerElement,
                                   TotalElementCount, CornersPerElement,
                                   ZoneType, NULL);
      if (IsOk)
        {
          Set_pa       ThisZone = NULL;
          NodeMap_pa   TargetNodeMap = NULL;
          FieldData_pa *TargetVarCache = NULL;
          LgIndex_t    TargetElement = 0;
          LgIndex_t    TargetPoint = 0;
          FieldData_pa *SourceVarCache;
          LgIndex_t    *SourceToTargetPointCache;
          LgIndex_t    SourceElement = 0;
          double       Value = 0.0;
          char         MessageString[200];

          /* get the zone number which is the the last item */
          TecUtilDataSetGetInfo(&DataSetTitle, &NumZones, &NumVars);
          TecUtilStringDealloc(&DataSetTitle);
          Zone = NumZones;

          /* add this zone to the set of active zones */
          ThisZone = TecUtilSetAlloc(TRUE);
          TecUtilSetAddMember(ThisZone, Zone, TRUE);
          TecUtilZoneSetActive(ThisZone, AssignOp_PlusEquals);
          TecUtilSetDealloc(&ThisZone);

          /* get the target node map and variable cache */
          TargetNodeMap  = TecUtilDataNodeGetRef(Zone);
          TargetVarCache = GetVarCache(Zone);

          SourceVarCache           = NULL;
          SourceToTargetPointCache = NULL;
          Zone = TECUTILSETNOTMEMBER;
          TargetPoint = 1;
          TargetElement = 1;
          for (Index = 0, Count = ListGetCount(FEZoneInfoList);
               Index < Count;
               Index++)
            {
              FEZoneInfo = ListGetItemRef(FEZoneInfoList, Index);
              assert(FEZoneInfo != NULL);

              ElementCount = ListGetCount(FEZoneInfo->DesiredElements);
              CornersPerElement = FEZoneInfo->CornersPerElement;
              if (FEZoneInfo->Zone != Zone && ElementCount >= 1)
                {
                  LgIndex_t NIndex;
                  LgIndex_t NumSourceNodes = (FEZoneInfo->ElementCount *
                                              FEZoneInfo->CornersPerElement);

                  /* this is a new zone so switch the enabled variable cache */
                  Zone = FEZoneInfo->Zone;
                  if (SourceVarCache != NULL)
                    free(SourceVarCache);
                  SourceVarCache = GetVarCache(Zone);

                  /* create a node mapping cache from source to target zone */
                  if (SourceToTargetPointCache != NULL)
                    free(SourceToTargetPointCache);
                  SourceToTargetPointCache = (LgIndex_t *)malloc(NumSourceNodes * sizeof(LgIndex_t));
                  for (NIndex = 0; NIndex < NumSourceNodes; NIndex++)
                    SourceToTargetPointCache[NIndex] = -1; /* ...no set */
                }

              for (Element = 0; Element < ElementCount; Element++)
                {
                  SourceElement = (LgIndex_t)ListGetItemRef(
                                      FEZoneInfo->DesiredElements, Element);
                  for (Corner = 1; Corner <= CornersPerElement; Corner++)
                    {
                      LgIndex_t SourcePoint = TecUtilDataNodeGetByRef(FEZoneInfo->NodeMap,
                                                                      SourceElement,
                                                                      Corner);
                      LgIndex_t CachedTargetPoint = SourceToTargetPointCache[SourcePoint];
                      if (CachedTargetPoint == -1)
                        {
                          CachedTargetPoint = SourceToTargetPointCache[SourcePoint] = TargetPoint;
                          TargetPoint++;
                        }
                      TecUtilDataNodeSetByRef(TargetNodeMap, TargetElement, 
                                              Corner, CachedTargetPoint);
                      for (Var = 0; Var < NumVars; Var++)
                        {
                          Value = TecUtilDataValueGetByRef(SourceVarCache[Var],
                                                           SourcePoint);
                          TecUtilDataValueSetByRef(TargetVarCache[Var],
                                                   CachedTargetPoint, Value);
                        }
                    }
                  TargetElement++;
                }
            }
          if (TargetVarCache != NULL)
            free(TargetVarCache);
          if (SourceVarCache != NULL)
            free(SourceVarCache);
          if (SourceToTargetPointCache)
            free(SourceToTargetPointCache);
          sprintf(MessageString,"FE Sub-Zone successfully created.\n"
                                "Zone %d has %d Nodes and %d elements",
                                NumZones, TotalElementCount*CornersPerElement,
                                TotalElementCount);
          TecUtilDialogMessageBox(MessageString,MessageBox_Information);
        }
    }
}


void STDCALL CreateSubFEZone(void)
{
  DataViewRegion_s *DataViewRegion = NULL;
  List_pa          FEZoneInfoList = NULL;

  TecUtilLockStart(AddOnID);
  assert(TecUtilFrameGetPlotType() == PlotType_Cartesian2D);

  DataViewRegion = GetDataViewRegion();
  FEZoneInfoList = GetFEZoneInfoList();
  if (ListGetCount(FEZoneInfoList) >= 1)
    {
      ExtractSubFEZone(DataViewRegion, FEZoneInfoList);
      TecUtilStateChanged(StateChange_ZonesAdded, (ArbParam_t)NULL);
    }
  ReleaseFEZoneInfoList(&FEZoneInfoList);
  ReleaseDataViewRegion(&DataViewRegion);
  TecUtilLockFinish(AddOnID);
}


void DialogSetSensitivities(void)
{
  Boolean_t Sensitive = (TecUtilFrameGetPlotType() == PlotType_Cartesian2D);
  TecGUISetSensitivity(Create_BTN_D1, Sensitive);
}


static void STDCALL StateChangeListener(StateChange_e StateChange,
                                ArbParam_t    CallData)
{
  if (TecGUIDialogIsUp(Dialog1Manager))
    {
      TecUtilLockStart(AddOnID);
      DialogSetSensitivities();
      TecUtilLockFinish(AddOnID);
    }
}


static void STDCALL crsfezMenuCB(void)
{
  TecUtilLockStart(AddOnID);
  BuildDialog1(MAINDIALOGID);
  TecGUIDialogLaunch(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}
 

EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{
  TecUtilLockOn();
  AddOnID = TecUtilAddOnRegister(100,ADDON_NAME,
                                 ADDON_VERSION" ("TecVersionId") "ADDON_DATE,
                                 "Amtec Engineering, Inc.");


  if (TecUtilGetTecplotVersion() < MinTecplotVersionAllowed)
    {
      char buffer[256];
      sprintf(buffer,"Add-on \"%s\" requires Tecplot version "
                     "%s or greater", ADDON_NAME, TecVersionId);
      TecUtilDialogErrMsg(buffer);
    }
  else
    {
      InitTGB();
 
      TecUtilMenuAddOption("Tools", ADDON_NAME, 'C', crsfezMenuCB);

      /* listen for frame mode changes */
      TecUtilStateChangeAddCallback(StateChangeListener);
    }

  TecUtilLockOff();
}
