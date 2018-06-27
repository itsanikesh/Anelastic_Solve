#include "TECADDON.h"
#include "ADDGLBL.h"
#ifndef MSWIN
#include <unistd.h>
#endif
#include "GUIDEFS.h"
#include "ZONEUTIL.h"

/**
 */
static void SendZoneResizeNotification(EntIndex_t Zone)
{
  Set_pa VarSet;
  Set_pa ZoneSet;

  ZoneSet = TecUtilSetAlloc(FALSE);
  TecUtilVarGetEnabled(&VarSet);
  if (ZoneSet != NULL && VarSet != NULL)
    {
      TecUtilSetAddMember(ZoneSet, Zone, FALSE);
      TecUtilStateChanged(StateChange_NodeMapsAltered, (ArbParam_t)ZoneSet);
      TecUtilStateChanged(StateChange_VarsAltered, (ArbParam_t)VarSet);
    }
}

/*
 * ShinkZone is called when the "Shrink Zone" button is pushed 
 * or when a macro command of the form:
 * $!ADDONCOMMAND ADDONID = "ZONETEST" COMMAND = "SHRINKZONE"
 * is processed.  This simply "shrinks" the first zone if it
 * is enabled.
 */

void ShrinkZone(Boolean_t DoRedraw)
{
  LgIndex_t  IMax;
  LgIndex_t  JMax;
  LgIndex_t  KMax;

  TecUtilLockStart(AddOnID);

  if (TecUtilDataSetIsAvailable() &&
      TecUtilZoneIsEnabled(1))
    {
      TecUtilZoneGetInfo(1,
                         &IMax,
                         &JMax,
                         &KMax,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (NodeMap_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL);
     
      if (TecUtilZoneIsFiniteElement(1))
        {
          if (JMax > 2)
            TecUtilZoneRealloc(1,
                               IMax,
                               JMax-1,
                               KMax);
        }
      else
        {
          if ((IMax > 1) && (JMax > 1))
            {
              TecUtilZoneRealloc(1,
                                 IMax-1,
                                 JMax-1,
                                 KMax);
            }
        }
      SendZoneResizeNotification(1);
      if (DoRedraw)
        TecUtilRedraw(TRUE);

      if (TecUtilMacroIsRecordingActive())
        TecUtilMacroRecordAddOnCommand("ZONETEST","SHRINKZONE");
    }

  TecUtilLockFinish(AddOnID);
}


#define ZoneModified 1

/*
 * ExpandZone is called when the "Expand Zone" button is pushed 
 * or when a macro command of the form:
 * $!ADDONCOMMAND ADDONID = "ZONETEST" COMMAND = "EXPANDZONE"
 * is processed.  This simply "expands" the first zone if it
 * is enabled.
 */


void ExpandZone(Boolean_t DoRedraw)
{
  LgIndex_t  IMax;
  LgIndex_t  JMax;
  LgIndex_t  KMax;

  TecUtilLockStart(AddOnID);

  if (TecUtilDataSetIsAvailable() &&
      TecUtilZoneIsEnabled(ZoneModified))
    {
      TecUtilZoneGetInfo(ZoneModified,
                         &IMax,
                         &JMax,
                         &KMax,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (NodeMap_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL,
                         (FieldData_pa *)NULL);
     
      if (TecUtilZoneIsFiniteElement(ZoneModified))
        {
          TecUtilZoneRealloc(ZoneModified,
                             IMax+1,
                             JMax,
                             KMax);
        }
      else
        {
          LgIndex_t I,J,K;
          EntIndex_t V;
          EntIndex_t NumVars;
          LgIndex_t  FinalIMax;
          LgIndex_t  FinalJMax;
          LgIndex_t  FinalKMax;
          FieldData_pa XD;
          FieldData_pa YD;
          FieldData_pa ZD;

          
          TecUtilDataSetGetInfo((char **)NULL,
                               (EntIndex_t *)NULL,
                               &NumVars);

          TecUtilZoneRealloc(ZoneModified,
                             IMax+2,
                             JMax+2,
                             KMax);

          TecUtilZoneGetInfo(ZoneModified,
                             &FinalIMax,
                             &FinalJMax,
                             &FinalKMax,
                             &XD,
                             &YD,
                             &ZD,
                             (NodeMap_pa *)NULL,
                             (FieldData_pa *)NULL,
                             (FieldData_pa *)NULL,
                             (FieldData_pa *)NULL,
                             (FieldData_pa *)NULL,
                             (FieldData_pa *)NULL,
                             (FieldData_pa *)NULL);

          for (V = 1; V <= NumVars; V++)
            {
              FieldData_pa FD;
              FD = TecUtilDataValueGetRef(ZoneModified,V);
              if (FD == XD)
                {
                  for (I = 0; I < FinalIMax; I++)
                  for (J = 0; J < FinalJMax; J++)
                  for (K = 0; K < FinalKMax; K++)
                    {
                      LgIndex_t Offset;
                      if ((I >= IMax) ||
                          (J >= JMax) ||
                          (K >= KMax))
                        {
                          Offset = 1 + I + FinalIMax*(J + FinalJMax*(K));
                          TecUtilDataValueSetByRef(FD,Offset,(double)I);
                        }
                    }
                }
              else if (FD == YD)
                {
                  for (I = 0; I < FinalIMax; I++)
                  for (J = 0; J < FinalJMax; J++)
                  for (K = 0; K < FinalKMax; K++)
                    {
                      LgIndex_t Offset;
                      if ((I >= IMax) ||
                          (J >= JMax) ||
                          (K >= KMax))
                        {
                          Offset = 1 + I + FinalIMax*(J + FinalJMax*(K));
                          TecUtilDataValueSetByRef(FD,Offset,(double)J);
                        }
                    }
                }
              else if (FD == ZD)
                {
                  for (I = 0; I < FinalIMax; I++)
                  for (J = 0; J < FinalJMax; J++)
                  for (K = 0; K < FinalKMax; K++)
                    {
                      LgIndex_t Offset;
                      if ((I >= IMax) ||
                          (J >= JMax) ||
                          (K >= KMax))
                        {
                          Offset = 1 + I + FinalIMax*(J + FinalJMax*(K));
                          TecUtilDataValueSetByRef(FD,Offset,(double)K);
                        }
                    }
                }
              else
                {
                  for (I = 0; I < FinalIMax; I++)
                  for (J = 0; J < FinalJMax; J++)
                  for (K = 0; K < FinalKMax; K++)
                    {
                      LgIndex_t Offset;
                      if ((I >= IMax) ||
                          (J >= JMax) ||
                          (K >= KMax))
                        {
                          Offset = 1 + I + FinalIMax*(J + FinalJMax*(K));
                          TecUtilDataValueSetByRef(FD,Offset,0.0);
                        }
                    }
                }
            }
        }
      SendZoneResizeNotification(ZoneModified);

      if (DoRedraw)
        TecUtilRedraw(TRUE);

      if (TecUtilMacroIsRecordingActive())
        TecUtilMacroRecordAddOnCommand("ZONETEST","EXPANDZONE");
    }

  TecUtilLockFinish(AddOnID);
}

/*
 * Add a point to a FE zone.  Sub-divide the cell that the point
 * sits in.  Assume the following:
 *
 *   - SourceZone is the zone into which the new point is added.
 *   - CellIndex is the cell into which the new point is added.
 *   - VValues has been filled in with the variable values to use for 
 *     the new point to be added.
 *   - All tests for a valid dataset, zone and zone type have been done.
 *   - Tecplot has been locked.  
 */

static void AddPointToFEZone(EntIndex_t SourceZone,
                             ZoneType_e ZoneType,
                             LgIndex_t  CellIndex,
                             double    *VValues)
{
  LgIndex_t   IMax;
  LgIndex_t   JMax;
  LgIndex_t   KMax;
  LgIndex_t   FinalIMax;
  LgIndex_t   FinalJMax;
  LgIndex_t   FinalKMax;
  NodeMap_pa  NM;
  SetIndex_t  V;
  Set_pa      EnabledVarList;
  LgIndex_t   N1,N2,N3,N4;
  Boolean_t   IsTriangle = ZoneType == ZoneType_FETriangle;

  TecUtilLockStart(AddOnID);

  /*
   * Get the zone dimensions.
   */
  TecUtilZoneGetInfo(SourceZone,
                     &IMax,
                     &JMax,
                     &KMax,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL,
                     (NodeMap_pa *)NULL,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL);

  /*
   * Increase the size of the zone.
   */
  if (IsTriangle)
    TecUtilZoneRealloc(SourceZone,
                       IMax+1,
                       JMax+2,
                       KMax);
  else
    TecUtilZoneRealloc(SourceZone,
                       IMax+1,
                       JMax+3,
                       KMax);


  /*
   * Have to GetZoneInfo again because all of the data pointers have now changed.
   */

  TecUtilZoneGetInfo(SourceZone,
                     &FinalIMax,
                     &FinalJMax,
                     &FinalKMax,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL,
                     &NM,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL,
                     (FieldData_pa *)NULL);

  /*
   * Get the set of enabled variables.  Only allowed to
   * stuff values into new point for enabled variables.
   */
  TecUtilVarGetEnabled(&EnabledVarList);

  /*
   * Set the field values....TecUtilSetForEachMember is a handy
   * macro that skips through all members of a set.
   */
  TecUtilSetForEachMember(V,EnabledVarList)
    {
      FieldData_pa FD;
      FD = TecUtilDataValueGetRef(SourceZone,(EntIndex_t)V);
      TecUtilDataValueSetByRef(FD,FinalIMax,VValues[V-1]);
    }

  TecUtilSetDealloc(&EnabledVarList);

  /*
   * set the connectivity.  N1,N2 and N3 are the nodes from the
   * original cell.
   */
  N1 = TecUtilDataNodeGetByRef(NM,CellIndex,1);
  N2 = TecUtilDataNodeGetByRef(NM,CellIndex,2);
  N3 = TecUtilDataNodeGetByRef(NM,CellIndex,3);

  if (!IsTriangle)
    N4 = TecUtilDataNodeGetByRef(NM,CellIndex,4);


  if (IsTriangle)
    {
      /*
       * The old cell is transformed to be N1,N2 and FinalIMax,
       * New Cell number 1 is N1,N3 and FinalIMax
       * New Cell number 2 is N2,N3 and FinalIMax
       */

      TecUtilDataNodeSetByRef(NM,CellIndex,3,FinalIMax);

      TecUtilDataNodeSetByRef(NM,FinalJMax-1,1,N1);
      TecUtilDataNodeSetByRef(NM,FinalJMax-1,2,N3);
      TecUtilDataNodeSetByRef(NM,FinalJMax-1,3,FinalIMax);

      TecUtilDataNodeSetByRef(NM,FinalJMax,1,N2);
      TecUtilDataNodeSetByRef(NM,FinalJMax,2,N3);
      TecUtilDataNodeSetByRef(NM,FinalJMax,3,FinalIMax);
    }
  else
    {
      /*
       * The old cell is transformed to be N1,N2 and FinalIMax twice.
       * New Cell number 1 is N2,N3 and FinalIMax twice,
       * New Cell number 2 is N3,N4 and FinalIMax twice,
       * New Cell number 3 is N4,N1 and FinalIMax twice,
       */

      TecUtilDataNodeSetByRef(NM,CellIndex,3,FinalIMax);
      TecUtilDataNodeSetByRef(NM,CellIndex,4,FinalIMax);

      TecUtilDataNodeSetByRef(NM,FinalJMax-2,1,N2);
      TecUtilDataNodeSetByRef(NM,FinalJMax-2,2,N3);
      TecUtilDataNodeSetByRef(NM,FinalJMax-2,3,FinalIMax);
      TecUtilDataNodeSetByRef(NM,FinalJMax-2,4,FinalIMax);

      TecUtilDataNodeSetByRef(NM,FinalJMax-1,1,N3);
      TecUtilDataNodeSetByRef(NM,FinalJMax-1,2,N4);
      TecUtilDataNodeSetByRef(NM,FinalJMax-1,3,FinalIMax);
      TecUtilDataNodeSetByRef(NM,FinalJMax-1,4,FinalIMax);

      TecUtilDataNodeSetByRef(NM,FinalJMax,1,N4);
      TecUtilDataNodeSetByRef(NM,FinalJMax,2,N1);
      TecUtilDataNodeSetByRef(NM,FinalJMax,3,FinalIMax);
      TecUtilDataNodeSetByRef(NM,FinalJMax,4,FinalIMax);
    }
  SendZoneResizeNotification(SourceZone);

  TecUtilLockFinish(AddOnID);
}


/*
 * A probe event in tecplot has occurred and sent here because
 * we earlier replaced the tecplot probe call back with this function.
 * Determine the location of the point probed and call AddPointToFECell.  If
 * recording is turned on then write out a macro command.
 */ 

void STDCALL CellRefineProbeCallback(Boolean_t IsNearestPoint)
{
  TecUtilLockStart(AddOnID);

  if ((TecUtilFrameGetPlotType() == PlotType_Cartesian2D) && !IsNearestPoint)
    {
      EntIndex_t SourceZone = TecUtilProbeFieldGetZone();
      ZoneType_e ZoneType   = TecUtilZoneGetType(SourceZone);

      if ((ZoneType == ZoneType_FETriangle) || 
          (ZoneType == ZoneType_FEQuad))
        {
          EntIndex_t NumVars;
          SetIndex_t V;
          double    *VValues;
          LgIndex_t  CellIndex;
          Set_pa     EnabledVarList;

          TecUtilDataSetGetInfo((char **)NULL,
                                (EntIndex_t *)NULL,
                                &NumVars);

          VValues = (double *)malloc(NumVars*sizeof(double));

          TecUtilVarGetEnabled(&EnabledVarList);
          /*
           * Get the field values....
           */
          TecUtilSetForEachMember(V,EnabledVarList)
            {
              VValues[V-1] = TecUtilProbeFieldGetValue((EntIndex_t)V);
            }

           TecUtilSetDealloc(&EnabledVarList);
           CellIndex = TecUtilProbeFieldGetCell();

           AddPointToFEZone(SourceZone,
                            ZoneType,
                            CellIndex,
                            VValues);

           if (TecUtilMacroIsRecordingActive())
             {
               EntIndex_t XVar = TecUtilVarGetNumByAssignment('X');
               EntIndex_t YVar = TecUtilVarGetNumByAssignment('Y');
               char Command[80];
               sprintf(Command,"REFINECELL %G %G",
                       VValues[XVar-1],
                       VValues[YVar-1]);
               TecUtilMacroRecordAddOnCommand("ZONETEST",Command);
             }

           free(VValues);
           VValues = NULL;

           TecUtilRedraw(TRUE);
        }
    }

  TecUtilLockFinish(AddOnID);
}



/*
 * Process a macro command of the form:
 *
 *  $!ADDONCOMMAND
 *    ADDONID = "ZONETEST"
 *    COMMAND = "REFINECELL xpos ypos"
 *
 * NOTE: This currently does not return any type of
 *       error if the cell refinement is not successful.
 *       This could be added later.
 */

static void RefineCellViaMacroCommand(double X,
                                      double Y)
{
  TecUtilLockStart(AddOnID);

  if (TecUtilFrameGetPlotType() == PlotType_Cartesian2D)
    {
      /*
       * Use TecUtilProbeAtPosition to simulate a probe event.
       */
    
      LgIndex_t   ICell;
      LgIndex_t   JCell;
      LgIndex_t   KCell;
      IJKPlanes_e Plane = Planes_Unused;
      EntIndex_t  SourceZone;
      EntIndex_t  NumVars;
      double     *VValues;

      TecUtilDataSetGetInfo((char **)NULL,
                            (EntIndex_t *)NULL,
                            &NumVars);

      VValues = (double *)malloc(NumVars*sizeof(double));

      if (TecUtilProbeAtPosition(X,Y,0.0,
                                 &ICell,
                                 &JCell,
                                 &KCell,
                                 &Plane,
                                 &SourceZone,
                                 FALSE,
                                 VValues,
                                 (Set_pa )NULL,
                                 FALSE,
                                 FALSE,
                                 FALSE))
        {
          ZoneType_e ZoneType = TecUtilZoneGetType(SourceZone);

          if ((ZoneType == ZoneType_FETriangle) || 
              (ZoneType == ZoneType_FEQuad))
            {
               AddPointToFEZone(SourceZone,
                                ZoneType,
                                JCell,
                                VValues);
            }
        }
       free(VValues);
       VValues = NULL;
    }

  TecUtilLockFinish(AddOnID);
}





static Boolean_t MatchCommand(char **CommandString,
                              char *CommandToMatch)
{
  char *SPtr = *CommandString;
  char *MPtr = CommandToMatch;
  while (*MPtr)
    {
      if (*MPtr != CAPITAL(*SPtr))
        return (FALSE);

      MPtr++;
      SPtr++;
    }
  if ((*SPtr == '\0') ||
      (*SPtr == ' '))
    {
      *CommandString = SPtr;
      return (TRUE);
    }
  return (FALSE);
}


/*
 * Augment tecplot's macro language.  This function is called
 * whenever tecplot encounters a macro command of the form:
 *
 *   $!ADDONCOMMAND
 *     ADDONID = "ZONETEST"
 *     COMMAND = <string>
 *
 *  where <string> contains a made up language for this addon
 *  and can be one of the following:
 *
 *  "SHRINKZONE"
 *  "EXPANDZONE"
 *  "REFINECELL <xpos> <ypos>"
 *
 *  where <xpos> and <ypos> are floating point numbers.
 *
 *
 */
Boolean_t STDCALL ZoneTestMacroCallback(char  *Command,
                                        char **ErrMsg)
{
  Boolean_t IsOk = TRUE;
  char    *CPtr = Command;

  TecUtilLockStart(AddOnID);

  if (Command == NULL)
    {
      *ErrMsg = TecUtilStringAlloc(80,"err message string");
      sprintf(*ErrMsg,"Command is missing");
      IsOk = FALSE;
    }

  if (IsOk)
    {
      if (MatchCommand(&CPtr,"SHRINKZONE"))
        {
          ShrinkZone(FALSE);
        }
      else if (MatchCommand(&CPtr,"EXPANDZONE"))
        {
          ExpandZone(FALSE);
        }
      else if (MatchCommand(&CPtr,"REFINECELL"))
        {
          double X,Y;
          if (sscanf(CPtr,"%le %le",&X,&Y) == 2)
            {
              RefineCellViaMacroCommand(X,Y);
            }
          else
            {
              *ErrMsg = TecUtilStringAlloc(80,"err message string");
              sprintf(*ErrMsg,"Invalid X,Y position in REFINECELL command");
              IsOk = FALSE;
            }
        }
      else
        {
          *ErrMsg = TecUtilStringAlloc(60,"err message string");
          sprintf(*ErrMsg,"Invalid command string for ZONETEST");
          IsOk = FALSE;
        }
    }

  TecUtilLockFinish(AddOnID);

  return IsOk;
}
