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

#include "TECADDON.h"
#include "UTIL.h"

extern AddOn_pa AddOnID;
int CurStreamDirection = 1;

void GoStreamCircle(void)
{
  /*int           CurPick;*/
  int           NumPicks;
  Boolean_t     CircleUsed = FALSE;
  StreamDir_e   StreamDir = StreamDir_Forward;
  if (!TecUtilDataSetIsAvailable())
    {
      TecUtilDialogErrMsg("Must have an attached data set!");
      return;
    }
  if (TecUtilFrameGetPlotType() != PlotType_Cartesian2D)
    {
      TecUtilDialogErrMsg("This only works in 2D!");
      return;
    }
  if ((TecUtilVarGetNumByAssignment('U') == TECUTILSETNOTMEMBER) ||
      (TecUtilVarGetNumByAssignment('V') == TECUTILSETNOTMEMBER))
    {
      TecUtilDialogErrMsg("Vector variables are not defined!");
      return;
    }

  switch (CurStreamDirection)
    {
      case 1: StreamDir = StreamDir_Forward; break;
      case 2: StreamDir = StreamDir_Reverse; break;
      case 3: StreamDir = StreamDir_Both; break;
    }

  NumPicks = TecUtilPickListGetCount();

  if (NumPicks > 0)
    {
      int P;
      for (P = 1; P <= NumPicks; P++)
        {
          if (TecUtilPickListGetType(P) == PickObject_Geom)
            {
              Geom_ID G;
              G = TecUtilPickListGetGeom(P);
              if ((TecUtilGeomGetType(G) == GeomForm_Circle) &&
                  (TecUtilGeomGetPositionCoordSys(G) == CoordSys_Grid))
                {
                  double R,X,Y,Z;
                  int    I,NumCirclePts;
                  TecUtilGeomGetXYZAnchorPos(G,&X,&Y,&Z);
                  R = TecUtilGeomCircleGetRadius(G);
                  NumCirclePts = TecUtilGeomEllipseGetNumPoints(G);
                  CircleUsed = TRUE;
                  for (I = 0; I < NumCirclePts; I++)
                    {
                      double Ang = I*2*PI/(NumCirclePts);
                      double XP,YP;
                      XP = X + R*cos(Ang);
                      YP = Y + R*sin(Ang);
                      TecUtilStreamtraceAdd(1,
                                            Streamtrace_TwoDLine,
                                            StreamDir,
                                            XP,
                                            YP,
                                            0.0,
                                            0.0,
                                            0.0,
                                            0.0);
                    }
                }
            }
        }
    }
   if (!CircleUsed)
     TecUtilDialogErrMsg("No circles in grid mode are selected");
}


