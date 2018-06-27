#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUICB.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"


static LgIndex_t CurrentTabPageID = BADGUIID;
static LgIndex_t MaxNumZonesOrMaps = 0;

/**
 */
static void SetMaxNumZonesOrMaps(LgIndex_t NewMaximum)
{
  LgIndex_t NewZoneOrMap;
  LgIndex_t OrigZoneOrMap;

  REQUIRE(NewMaximum >= 0);

  /* attempt to preserve Mx syntax if it exists */
  if (NewMaximum >= 1 &&
      TecGUITextFieldGetLgIndex(ZoneOrMap_TFS_T2_1, &OrigZoneOrMap))
    {
      if (OrigZoneOrMap < 0)
        {
          /* ...Mx-# syntax */
          if (NewMaximum+OrigZoneOrMap >= 1)
            NewZoneOrMap = OrigZoneOrMap;
          else
            NewZoneOrMap = 1;
        }
      else if (OrigZoneOrMap == 0)
        {
          /* ...Mx syntax */
          NewZoneOrMap = OrigZoneOrMap;
        }
      else /* if (OrigZoneOrMap > 0) */
        {
          /* ...just a plain number */
          CHECK(OrigZoneOrMap > 0);
          if (NewMaximum >= OrigZoneOrMap)
            NewZoneOrMap = OrigZoneOrMap;
          else
            NewZoneOrMap = NewMaximum;
        }

      if (NewZoneOrMap != OrigZoneOrMap)
        TecGUITextFieldSetLgIndex(ZoneOrMap_TFS_T2_1, NewZoneOrMap, (OrigZoneOrMap <= 0));
    }
  else
    TecGUITextFieldSetLgIndex(ZoneOrMap_TFS_T2_1, 1, FALSE);

  /* update the new maximum */
  MaxNumZonesOrMaps = NewMaximum;
}

/**
 */
static void UpdateSpecificTabPage(void)
{
  EntIndex_t NumZones = 0;
  EntIndex_t NumMaps  = 0;
  PlotType_e PlotType;

  if (TecUtilDataSetIsAvailable())
    {
      char *DataSetTitle = NULL;
      TecUtilDataSetGetInfo(&DataSetTitle,&NumZones,NULL);
      if ( DataSetTitle )
        TecUtilStringDealloc(&DataSetTitle);
      NumMaps = TecUtilLineMapGetCount();
    }

  PlotType = TecUtilFrameGetPlotType();
  if (NumMaps > 0 &&
      (PlotType == PlotType_XYLine ||
       PlotType == PlotType_PolarLine))
    SetMaxNumZonesOrMaps(NumMaps);
  else if ( NumZones > 0 &&
            (PlotType == PlotType_Cartesian3D ||
             PlotType == PlotType_Cartesian2D) )
    SetMaxNumZonesOrMaps(NumZones);
  else
    SetMaxNumZonesOrMaps(0);

  /*
   * Update control sensitivies.
   */
  TecGUISetSensitivity(GetValue_BTN_T1_1, TRUE);
}

/**
 */
static const char *IJKLines_eToString(IJKLines_e I)
{
  switch (I)
    {
      case Lines_I: return "Lines_I"; break;
      case Lines_J: return "Lines_J"; break;
      case Lines_K: return "Lines_K"; break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *ErrorBar_eToString(ErrorBar_e E)
{
  switch (E)
    {
      case ErrorBar_Up: return "ErrorBar_Up"; break;
      case ErrorBar_Down: return "ErrorBar_Down"; break;
      case ErrorBar_Left: return "ErrorBar_Left"; break;
      case ErrorBar_Right: return "ErrorBar_Right"; break;
      case ErrorBar_Horz: return "ErrorBar_Horz"; break;
      case ErrorBar_Vert: return "ErrorBar_Vert"; break;
      case ErrorBar_Cross: return "ErrorBar_Cross"; break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *SkipMode_eToString(SkipMode_e S)
{
  switch (S)
    {
      case Skip_ByIndex: return "Skip_ByIndex"; break;
      case Skip_ByFrameUnits: return "Skip_ByFrameUnits"; break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *CurveType_eToString(CurveType_e C)
{
  switch (C)
    {
      case CurveType_LineSeg:     return "CurveType_LineSeg"; break;
      case CurveType_CurvFit:     return "CurveType_CurvFit"; break;
      case CurveType_EToRFit:     return "CurveType_EToRFit"; break;
      case CurveType_PowerFit:    return "CurveType_PowerFit"; break;
      case CurveType_Spline:      return "CurveType_Spline"; break;
      case CurveType_ParaSpline:  return "CurveType_ParaSpline"; break;
      case CurveType_Extended:    return "CurveType_Extended"; break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *FunctionDependency_eToString(FunctionDependency_e F)
{
  switch (F)
    {
      case FunctionDependency_XIndependent: return "FunctionDependency_XIndependent"; break;
      case FunctionDependency_YIndependent: return "FunctionDependency_YIndependent"; break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *LineMapSort_eToString(LineMapSort_e L)
{
  switch (L)
    {
      case LineMapSort_None          : return "LineMapSort_None";           break;
      case LineMapSort_IndependentVar: return "LineMapSort_IndependentVar"; break;
      case LineMapSort_DependentVar  : return "LineMapSort_DependentVar";   break;
      case LineMapSort_SpecificVar   : return "LineMapSort_SpecificVar";    break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *LegendShow_eToString(LegendShow_e L)
{
  switch (L)
    {
      case LegendShow_Yes  : return "LegendShow_Yes";  break;
      case LegendShow_No   : return "LegendShow_No";   break;
      case LegendShow_Auto : return "LegendShow_Auto"; break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *LightingEffect_eToString(LightingEffect_e L)
{
  switch (L)
    {
      case LightingEffect_Paneled: return "LightingEffect_Paneled"; break;
      case LightingEffect_Gouraud: return "LightingEffect_Gouraud"; break;
      case LightingEffect_None: return "LightingEffect_None"; break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *PointsToPlot_eToString(PointsToPlot_e P)
{
  switch (P)
    {
      case PointsToPlot_SurfaceNodes       : return "PointsToPlot_SurfaceNodes"; break;
      case PointsToPlot_AllNodes           : return "PointsToPlot_AllNodes"; break;
      case PointsToPlot_SurfaceCellCenters : return "PointsToPlot_SurfaceCellCenters"; break;
      case PointsToPlot_AllCellCenters     : return "PointsToPlot_AllCellCenters"; break;
      case PointsToPlot_AllConnected       : return "PointsToPlot_AllConnected"; break;
      default: CHECK(FALSE); return ""; break;
    }
}

#if 0 /* Not currently used */
/**
 */
static const char *Font_eToString(SurfacesToPlot_e S)
{
  switch (S)
    {
      case Font_Helvetica       : return "Helvetica"; break;
      case Font_HelveticaBold   : return "HelveticaBold"; break;
      case Font_Greek           : return "Greek"; break;
      case Font_Math            : return "Math"; break;
      case Font_UserDefined     : return "UserDefined"; break;
      case Font_Times           : return "Times"; break;
      case Font_TimesItalic     : return "TimesItalic"; break;
      case Font_TimesBold       : return "TimesBold"; break;
      case Font_TimesItalicBold : return "TimesItalicBold"; break;
      case Font_Courier         : return "Courier"; break;
      case Font_CourierBold     : return "CourierBold"; break;
      default: CHECK(FALSE); return ""; break;
    }
}
#endif

/**
 */
static const char *SurfacesToPlot_eToString(SurfacesToPlot_e S)
{
  switch (S)
    {
      case SurfacesToPlot_BoundaryFaces: return "SurfacesToPlot_BoundaryFaces"; break;
      case SurfacesToPlot_ExposedCellFaces: return "SurfacesToPlot_ExposedCellFaces"; break;
      case SurfacesToPlot_IPlanes: return "SurfacesToPlot_IPlanes"; break;
      case SurfacesToPlot_JPlanes: return "SurfacesToPlot_JPlanes"; break;
      case SurfacesToPlot_KPlanes: return "SurfacesToPlot_KPlanes"; break;
      case SurfacesToPlot_IJPlanes: return "SurfacesToPlot_IJPlanes"; break;
      case SurfacesToPlot_JKPlanes: return "SurfacesToPlot_JKPlanes"; break;
      case SurfacesToPlot_IKPlanes: return "SurfacesToPlot_JKPlanes"; break;
      case SurfacesToPlot_IJKPlanes: return "SurfacesToPlot_IJKPlanes"; break;
      case SurfacesToPlot_All: return "SurfacesToPlot_All"; break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *BoundPlotType_eToString(BoundPlotType_e B)
{
  switch (B)
    {
      case Boundary_None: return "Boundary_None"; break;
      case Boundary_Min: return "Boundary_Min"; break;
      case Boundary_Max:  return "Boundary_Max"; break;
      case Boundary_Both: return "Boundary_Both"; break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *FillMode_eToString(FillMode_e FillMode)
{
  REQUIRE(VALID_ENUM(FillMode, FillMode_e));
  switch (FillMode)
    {
      case FillMode_None               : return "None";                 break;
      case FillMode_UseSpecificColor   : return "Use Specific Color";   break;
      case FillMode_UseLineColor       : return "Use Line Color";       break;
      case FillMode_UseBackgroundColor : return "Use Background Color"; break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static char SymbolCharBuffer[2+1];
static const char *SymbolChar_tToString(char *AllocatedSymbolChar)
{
  REQUIRE(VALID_REF(AllocatedSymbolChar));
  REQUIRE(0 <= strlen(AllocatedSymbolChar) && strlen(AllocatedSymbolChar) < sizeof(SymbolCharBuffer));

  strcpy(SymbolCharBuffer, AllocatedSymbolChar);
  TecUtilStringDealloc(&AllocatedSymbolChar);
  return SymbolCharBuffer;
}

/**
 */
static const char *ArrowheadStyle_eToString(ArrowheadStyle_e A)
{
  switch (A)
    {
      case Arrowhead_Plain: return "Arrowhead_Plain"; break;
      case Arrowhead_Filled: return "Arrowhead_Filled"; break;
      case Arrowhead_Hollow: return "Arrowhead_Hollow"; break;
      default: CHECK(FALSE); return ""; break;
    }

}

/**
 */
static const char *VectorPlotType_eToString(VectorPlotType_e V)
{
  switch (V)
    {
      case Vector_TailAtPoint: return "Vector_TailAtPoint"; break;
      case Vector_HeadAtPoint: return "Vector_HeadAtPoint"; break;
      case Vector_MidAtPoint: return "Vector_MidAtPoint"; break;
      case Vector_HeadOnly: return "Vector_HeadOnly";break;
      default: CHECK(FALSE); return ""; break;
    }


}

/**
 */
static const char *ContourPlotType_eToString(ContourPlotType_e C)
{
  switch (C)
    {
      case Contour_Lines: return "Contour_Lines"; break;
      case Contour_Flood: return "Contour_Flood"; break;
      case Contour_Overlay: return "Contour_Overlay";break;
      case Contour_AverageCell: return "Contour_AverageCell";break;
      case Contour_CornerCell: return "Contour_CornerCell"; break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *ContourColoring_eToString(ContourColoring_e C)
{
  switch (C)
    {
      case ContourColoring_RGB    : return "RGB"; break;
      case ContourColoring_Group1 : return "C1";  break;
      case ContourColoring_Group2 : return "C2";  break;
      case ContourColoring_Group3 : return "C3";  break;
      case ContourColoring_Group4 : return "C4";  break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *ContourGroupToString(int C)
{
  switch (C)
    {
      case 1 : return "C1";  break;
      case 2 : return "C2";  break;
      case 3 : return "C3";  break;
      case 4 : return "C4";  break;
      default: CHECK(FALSE); return ""; break;
    }
}

/**
 */
static const char *LinePattern_eToString(LinePattern_e L)
{
  switch (L)
    {
      case LinePattern_Solid: return "LinePattern_Solid"; break;
      case LinePattern_Dashed: return "LinePattern_Dashed"; break;
      case LinePattern_DashDot: return "LinePattern_DashDot";break;
      case LinePattern_Dotted: return "LinePattern_Dotted";break;
      case LinePattern_LongDash: return "LinePattern_LongDash";break;
      case LinePattern_DashDotDot: return "LinePattern_DashDotDot";break;
      default: CHECK(FALSE); return "";break;
    }
}

/**
 */
static const char *MeshPlotType_eToString(MeshPlotType_e M)
{
    switch (M)
      {
        case Mesh_Wireframe:  return   "Mesh_WireFrame"; break;
        case Mesh_Overlay:    return   "Mesh_Overlay"; break;
        case Mesh_HiddenLine: return   "Mesh_HiddenLine"; break;
        default: CHECK(FALSE); return ""; break;
      }
}

/**
 */
static const char *Boolean_tToString(Boolean_t B)
{
  return B ? "TRUE" : "FALSE";
}

/**
 */
static const char *ColorIndexToString(ColorIndex_t BColor)
{
  switch (BColor)
    {
      case Black_C       :   return "Black";     break;
      case Red_C         :   return "Red";       break;
      case Green_C       :   return "Green";     break;
      case Blue_C        :   return "Blue";      break;
      case Cyan_C        :   return "Cyan";      break;
      case Yellow_C      :   return "Yellow";    break;
      case Purple_C      :   return "Purple";    break;
      case White_C       :   return "White";     break;
      case Custom1_C     :   return "Custom 1";  break;
      case Custom2_C     :   return "Custom 2";  break;
      case Custom3_C     :   return "Custom 3";  break;
      case Custom4_C     :   return "Custom 4";  break;
      case Custom5_C     :   return "Custom 5";  break;
      case Custom6_C     :   return "Custom 6";  break;
      case Custom7_C     :   return "Custom 7";  break;
      case Custom8_C     :   return "Custom 8";  break;
      case Custom9_C     :   return "Custom 9";  break;
      case Custom10_C    :   return "Custom 10"; break;
      case Custom11_C    :   return "Custom 11"; break;
      case Custom12_C    :   return "Custom 12"; break;
      case Custom13_C    :   return "Custom 13"; break;
      case Custom14_C    :   return "Custom 14"; break;
      case Custom15_C    :   return "Custom 15"; break;
      case Custom16_C    :   return "Custom 16"; break;
      case Custom17_C    :   return "Custom 17"; break;
      case Custom18_C    :   return "Custom 18"; break;
      case Custom19_C    :   return "Custom 19"; break;
      case Custom20_C    :   return "Custom 20"; break;
      case Custom21_C    :   return "Custom 21"; break;
      case Custom22_C    :   return "Custom 22"; break;
      case Custom23_C    :   return "Custom 23"; break;
      case Custom24_C    :   return "Custom 24"; break;
      case Custom25_C    :   return "Custom 25"; break;
      case Custom26_C    :   return "Custom 26"; break;
      case Custom27_C    :   return "Custom 27"; break;
      case Custom28_C    :   return "Custom 28"; break;
      case Custom29_C    :   return "Custom 29"; break;
      case Custom30_C    :   return "Custom 30"; break;
      case Custom31_C    :   return "Custom 31"; break;
      case Custom32_C    :   return "Custom 32"; break;
      case Custom33_C    :   return "Custom 33"; break;
      case Custom34_C    :   return "Custom 34"; break;
      case Custom35_C    :   return "Custom 35"; break;
      case Custom36_C    :   return "Custom 36"; break;
      case Custom37_C    :   return "Custom 37"; break;
      case Custom38_C    :   return "Custom 38"; break;
      case Custom39_C    :   return "Custom 39"; break;
      case Custom40_C    :   return "Custom 40"; break;
      case Custom41_C    :   return "Custom 41"; break;
      case Custom42_C    :   return "Custom 42"; break;
      case Custom43_C    :   return "Custom 43"; break;
      case Custom44_C    :   return "Custom 44"; break;
      case Custom45_C    :   return "Custom 45"; break;
      case Custom46_C    :   return "Custom 46"; break;
      case Custom47_C    :   return "Custom 47"; break;
      case Custom48_C    :   return "Custom 48"; break;
      case Custom49_C    :   return "Custom 49"; break;
      case Custom50_C    :   return "Custom 50"; break;
      case Custom51_C    :   return "Custom 51"; break;
      case Custom52_C    :   return "Custom 52"; break;
      case Custom53_C    :   return "Custom 53"; break;
      case Custom54_C    :   return "Custom 54"; break;
      case Custom55_C    :   return "Custom 55"; break;
      case Custom56_C    :   return "Custom 56"; break;
      case MultiColor_C  :   return "Multi C1";  break;
      case MultiColor2_C :   return "Multi C2";  break;
      case MultiColor3_C :   return "Multi C3";  break;
      case MultiColor4_C :   return "Multi C4";  break;
      case RGBColor_C    :   return "RGB";       break;
      default: CHECK(FALSE); return "No Color";
    }
}

/**
 */
static const char *GeomShape_eToString(GeomShape_e G)
{
  switch (G)
  {
    case GeomShape_Square     : return "GeomShape_Square";  break;
    case GeomShape_Del        : return "GeomShape_Del";     break;
    case GeomShape_Grad       : return "GeomShape_Grad";    break;
    case GeomShape_RTri       : return "GeomShape_RTri";    break;
    case GeomShape_LTri       : return "GeomShape_LTri";    break;
    case GeomShape_Diamond    : return "GeomShape_Diamond"; break;
    case GeomShape_Circle     : return "GeomShape_Circle";  break;
    case GeomShape_Cube       : return "GeomShape_Cube";    break;
    case GeomShape_Sphere     : return "GeomShape_Shpere";  break;
    case GeomShape_Octahedron : return "GeomShape_Tetra";   break;
    case GeomShape_Point      : return "GeomShape_Point";   break;
    default: CHECK(FALSE); return ""; break;
  }
}

/**
 */
static LgIndex_t GetZoneOrMap(void)
{
  LgIndex_t Result;

  if (!TecGUITextFieldGetLgIndex(ZoneOrMap_TFS_T2_1, &Result))
    Result = 1;
  if (Result <= 0)
    Result += MaxNumZonesOrMaps;

  ENSURE(1 <= Result && Result <= MaxNumZonesOrMaps);
  return Result;
}

/**
 */
static void UpdateZoneMapTabPage(void)
{
  static char Attr[10000];
  static char buffer[5000];
  char *MapExtendedName = NULL;
  char *MapExtendedSettings = NULL;
  char *DataSetTitle  = NULL;
  EntIndex_t NumZones = 0;
  EntIndex_t NumMaps  = 0;
  PlotType_e PlotType;

  strcpy(Attr,"");
  strcpy(buffer,"");

  if ( TecUtilDataSetIsAvailable() )
    {
      TecUtilDataSetGetInfo(&DataSetTitle,&NumZones,NULL);
      NumMaps = TecUtilLineMapGetCount();
    }
  else
    {
      strcpy(Attr,"No data set available.\n");
    }

  PlotType = TecUtilFrameGetPlotType();
  if (NumMaps > 0 &&
      (PlotType == PlotType_XYLine ||
       PlotType == PlotType_PolarLine))
    {
      LgIndex_t Map;
      Boolean_t IsXY = (PlotType == PlotType_XYLine);
      char *Name;
      SetMaxNumZonesOrMaps(NumMaps);
      Map = GetZoneOrMap();
      Name = (char*)TecUtilLineMapStyleGetArbValue(Map,SV_NAME,NULL,NULL);
      MapExtendedName = (char*)TecUtilLineMapStyleGetArbValue(Map,SV_CURVES,SV_EXTENDEDNAME,NULL);
      MapExtendedSettings = (char*)TecUtilLineMapStyleGetArbValue(Map,SV_CURVES,SV_EXTENDEDSETTINGS,NULL);

      sprintf(buffer,
        "NAME = %s\n\n"

        "*** Assign ***\n"
        "ZONE = %d\n",
        (Name != NULL ? Name : "UNSPECIFIED"),
        (int) TecUtilLineMapStyleGetArbValue(Map,SV_ASSIGN,SV_ZONE,NULL));

      if ( IsXY )
        {
          sprintf(&buffer[strlen(buffer)],
                  "XAXISVAR = %d\n"
                  "YAXISVAR = %d\n",
                  (int) TecUtilLineMapStyleGetArbValue(Map,SV_ASSIGN,SV_XAXISVAR,NULL),
                  (int) TecUtilLineMapStyleGetArbValue(Map,SV_ASSIGN,SV_YAXISVAR,NULL));
          /* Axis assignment is not used for Polar Plots */
          sprintf(&buffer[strlen(buffer)],
                  "XAXIS = %d\n"
                  "YAXIS = %d\n",
                  (int) TecUtilLineMapStyleGetArbValue(Map,SV_ASSIGN,SV_XAXIS,NULL),
                  (int) TecUtilLineMapStyleGetArbValue(Map,SV_ASSIGN,SV_YAXIS,NULL));
        }
      else
        {
          sprintf(&buffer[strlen(buffer)],
                  "THETAAXISVAR = %d\n"
                  "RAXISVAR     = %d\n",
                  (int) TecUtilLineMapStyleGetArbValue(Map,SV_ASSIGN,SV_THETAAXISVAR,NULL),
                  (int) TecUtilLineMapStyleGetArbValue(Map,SV_ASSIGN,SV_RAXISVAR,NULL));
        }
      sprintf(&buffer[strlen(buffer)],
              "FUNCTIONDEPENDENCY = %s\n"
              "SORT = %s\n"
              "SORTVAR = %d\n"
              "SHOWINLEGEND = %s\n\n",
              FunctionDependency_eToString((FunctionDependency_e)
                TecUtilLineMapStyleGetArbValue(Map,SV_ASSIGN,SV_FUNCTIONDEPENDENCY,NULL)),
              LineMapSort_eToString((LineMapSort_e)TecUtilLineMapStyleGetArbValue(Map,SV_ASSIGN,SV_SORT,NULL)),
              (int)TecUtilLineMapStyleGetArbValue(Map,SV_ASSIGN,SV_SORTVAR,NULL),
              LegendShow_eToString((LegendShow_e)TecUtilLineMapStyleGetArbValue(Map,SV_ASSIGN,SV_SHOWINLEGEND,NULL)));


      sprintf(&buffer[strlen(buffer)],
              "*** Lines ***\n"
              "SHOW = %s\n"
              "COLOR = %s\n"
              "LINEPATTERN = %s\n"
              "PATTERNLENGTH = %G\n"
              "LINETHICKNESS = %G\n\n",
              Boolean_tToString((Boolean_t)TecUtilLineMapStyleGetArbValue(Map,SV_LINES,SV_SHOW,NULL)),
              ColorIndexToString((ColorIndex_t)TecUtilLineMapStyleGetArbValue(Map,SV_LINES,SV_COLOR,NULL)),
              LinePattern_eToString((LinePattern_e)TecUtilLineMapStyleGetArbValue(Map,SV_LINES,SV_LINEPATTERN,NULL)),
              TecUtilLineMapStyleGetDoubleValue(Map,SV_LINES,SV_PATTERNLENGTH,NULL),
              TecUtilLineMapStyleGetDoubleValue(Map,SV_LINES,SV_LINETHICKNESS,NULL));

      sprintf(&buffer[strlen(buffer)],
              "*** Curves ***\n"
              "CURVETYPE = %s\n"
              "NUMPTS = %d\n"
              "POLYORDER = %d\n"
              "WEIGHTVAR = %s\n"
              "USEINDVARRANGE = %s\n"
              "CLAMPSPLINE = %s\n"
              "EXTENDEDNAME = %s\n"
              "EXTENDEDSETTINGS = %s\n"
              "SPLINEDERIVATIVEATSTART = %G\n"
              "SPLINEDERIVATIVEATEND   = %G\n"
              "INDVARMIN = %G\n"
              "INDVARMAX = %G\n\n",
              CurveType_eToString((CurveType_e)TecUtilLineMapStyleGetArbValue(Map,SV_CURVES,SV_CURVETYPE,NULL)),
              (int)TecUtilLineMapStyleGetArbValue(Map,SV_CURVES,SV_NUMPTS,NULL),
              (int)TecUtilLineMapStyleGetArbValue(Map,SV_CURVES,SV_POLYORDER,NULL),
              Boolean_tToString((Boolean_t)TecUtilLineMapStyleGetArbValue(Map,SV_CURVES,SV_WEIGHTVAR,NULL)),
              Boolean_tToString((Boolean_t)TecUtilLineMapStyleGetArbValue(Map,SV_CURVES,SV_USEINDVARRANGE,NULL)),
              Boolean_tToString((Boolean_t)TecUtilLineMapStyleGetArbValue(Map,SV_CURVES,SV_CLAMPSPLINE,NULL)),
              MapExtendedName ? MapExtendedName : "[NULL]",
              MapExtendedSettings ? MapExtendedSettings : "[NULL]",
              TecUtilLineMapStyleGetDoubleValue(Map,SV_CURVES,SV_SPLINEDERIVATIVEATSTART,NULL),
              TecUtilLineMapStyleGetDoubleValue(Map,SV_CURVES,SV_SPLINEDERIVATIVEATEND,NULL),
              TecUtilLineMapStyleGetDoubleValue(Map,SV_CURVES,SV_INDVARMIN,NULL),
              TecUtilLineMapStyleGetDoubleValue(Map,SV_CURVES,SV_INDVARMAX,NULL));

      sprintf(&buffer[strlen(buffer)],
              "*** Symbols ***\n"
              "SHOW = %s\n"
              "COLOR = %s\n"
              "FILLMODE = %s\n"
              "FILLCOLOR = %s\n"
              "SKIPMODE = %s\n"
              "ISASCII = %s\n"
              "ASCIICHAR = %s\n"
              "GEOMSHAPE = %s\n"
              "SIZE = %G\n"
              "LINETHICKNESS = %G\n"
              "SKIPPING = %G\n\n",
              Boolean_tToString((Boolean_t)TecUtilLineMapStyleGetArbValue(Map,SV_SYMBOLS,SV_SHOW,NULL)),
              ColorIndexToString((ColorIndex_t)TecUtilLineMapStyleGetArbValue(Map,SV_SYMBOLS,SV_COLOR,NULL)),
              FillMode_eToString((FillMode_e)TecUtilLineMapStyleGetArbValue(Map,SV_SYMBOLS,SV_FILLMODE,NULL)),
              ColorIndexToString((ColorIndex_t)TecUtilLineMapStyleGetArbValue(Map,SV_SYMBOLS,SV_FILLCOLOR,NULL)),
              SkipMode_eToString((SkipMode_e)TecUtilLineMapStyleGetArbValue(Map,SV_SYMBOLS,SV_SKIPMODE,NULL)),
              Boolean_tToString((Boolean_t)TecUtilLineMapStyleGetArbValue(Map,SV_SYMBOLS,SV_SYMBOLSHAPE,SV_ISASCII)),
              /* ASCIISHAPE actually goes down one more level, but TecUtilFieldStyleGetArbValue doesn't have enough parameters */
              SymbolChar_tToString((char *)TecUtilLineMapStyleGetArbValue(Map,SV_SYMBOLS,SV_SYMBOLSHAPE,SV_ASCIISHAPE)),
              GeomShape_eToString((GeomShape_e)TecUtilLineMapStyleGetArbValue(Map,SV_SYMBOLS,SV_SYMBOLSHAPE,SV_GEOMSHAPE)),
              TecUtilLineMapStyleGetDoubleValue(Map,SV_SYMBOLS,SV_SIZE,NULL),
              TecUtilLineMapStyleGetDoubleValue(Map,SV_SYMBOLS,SV_LINETHICKNESS,NULL),
              TecUtilLineMapStyleGetDoubleValue(Map,SV_SYMBOLS,SV_SKIPPING,NULL));


      if ( IsXY )
        {
          sprintf(&buffer[strlen(buffer)],
                  "*** Error Bar ***\n"
                  "SHOW = %s\n"
                  "VAR = %d\n"
                  "BARTYPE = %s\n"
                  "COLOR = %s\n"
                  "LINETHICKNESS = %G\n"
                  "SKIPPING = %G\n"
                  "SKIPMODE = %s\n"
                  "SIZE = %G\n\n",
                  Boolean_tToString((Boolean_t)TecUtilLineMapStyleGetArbValue(Map,SV_ERRORBARS,SV_SHOW,NULL)),
                  (int)TecUtilLineMapStyleGetArbValue(Map,SV_ERRORBARS,SV_VAR,NULL),
                  ErrorBar_eToString((ErrorBar_e)TecUtilLineMapStyleGetArbValue(Map,SV_ERRORBARS,SV_BARTYPE,NULL)),
                  ColorIndexToString((ColorIndex_t)TecUtilLineMapStyleGetArbValue(Map,SV_ERRORBARS,SV_COLOR,NULL)),
                  TecUtilLineMapStyleGetDoubleValue(Map,SV_ERRORBARS,SV_LINETHICKNESS,NULL),
                  TecUtilLineMapStyleGetDoubleValue(Map,SV_ERRORBARS,SV_SKIPPING,NULL),
                  SkipMode_eToString((SkipMode_e)TecUtilLineMapStyleGetArbValue(Map,SV_ERRORBARS,SV_SKIPMODE,NULL)),
                  TecUtilLineMapStyleGetDoubleValue(Map,SV_ERRORBARS,SV_SIZE,NULL));

          sprintf(&buffer[strlen(buffer)],
                  "*** Bar Chart ***\n"
                  "SHOW = %s\n"
                  "COLOR = %s\n"
                  "FILLMODE = %s\n"
                  "FILLBCOLOR = %s\n"
                  "SIZE = %G\n"
                  "LINETHICKNESS = %G\n\n",
                  Boolean_tToString((Boolean_t)TecUtilLineMapStyleGetArbValue(Map,SV_BARCHARTS,SV_SHOW,NULL)),
                  ColorIndexToString((ColorIndex_t)TecUtilLineMapStyleGetArbValue(Map,SV_BARCHARTS,SV_COLOR,NULL)),
                  FillMode_eToString((FillMode_e)TecUtilLineMapStyleGetArbValue(Map,SV_BARCHARTS,SV_FILLMODE,NULL)),
                  ColorIndexToString((ColorIndex_t)TecUtilLineMapStyleGetArbValue(Map,SV_BARCHARTS,SV_FILLCOLOR,NULL)),
                  TecUtilLineMapStyleGetDoubleValue(Map,SV_BARCHARTS,SV_SIZE,NULL),
                  TecUtilLineMapStyleGetDoubleValue(Map,SV_BARCHARTS,SV_LINETHICKNESS,NULL));


        }

      sprintf(&buffer[strlen(buffer)],
              "*** Indicies ***\n"
              "IJKLINES = %s\n"
              "IRANGE = (%d,%d,%d)\n"
              "JRANGE = (%d,%d,%d)\n"
              "KRANGE = (%d,%d,%d)\n\n",
              IJKLines_eToString((IJKLines_e)TecUtilLineMapStyleGetArbValue(Map,SV_INDICIES,SV_IJKLINES,NULL)),
              (int)TecUtilLineMapStyleGetArbValue(Map,SV_INDICIES,SV_IRANGE,SV_MIN),
              (int)TecUtilLineMapStyleGetArbValue(Map,SV_INDICIES,SV_IRANGE,SV_MAX),
              (int)TecUtilLineMapStyleGetArbValue(Map,SV_INDICIES,SV_IRANGE,SV_SKIP),
              (int)TecUtilLineMapStyleGetArbValue(Map,SV_INDICIES,SV_JRANGE,SV_MIN),
              (int)TecUtilLineMapStyleGetArbValue(Map,SV_INDICIES,SV_JRANGE,SV_MAX),
              (int)TecUtilLineMapStyleGetArbValue(Map,SV_INDICIES,SV_JRANGE,SV_SKIP),
              (int)TecUtilLineMapStyleGetArbValue(Map,SV_INDICIES,SV_KRANGE,SV_MIN),
              (int)TecUtilLineMapStyleGetArbValue(Map,SV_INDICIES,SV_KRANGE,SV_MAX),
              (int)TecUtilLineMapStyleGetArbValue(Map,SV_INDICIES,SV_KRANGE,SV_SKIP));

      if (Name != NULL)
        TecUtilStringDealloc(&Name);
      strcpy(Attr,buffer);
    }
  else if ( NumZones > 0 &&
            (PlotType == PlotType_Cartesian3D ||
             PlotType == PlotType_Cartesian2D) )
    {
      LgIndex_t Zone;
      char *ZoneName;

      SetMaxNumZonesOrMaps(NumZones);
      Zone = GetZoneOrMap();
      TecUtilZoneGetName(Zone,&ZoneName);

      sprintf(buffer,
       "Zone: %s\n"
       "GROUP = %d\n\n"

       "*** Mesh Attributes ***\n"
       "SHOW = %s\n"
       "MESHTYPE = %s\n"
       "COLOR = %s\n"
       "LINEPATTERN = %s\n"
       "PATTERNLENGTH = %G\n"
       "LINETHICKNESS = %G\n\n"

       "*** Contour Attributes ***\n"
       "SHOW = %s\n"
       "CONTOURTYPE = %s\n"
       "FLOOD BY = %s\n"
       "LINES BY = %s\n"
       "LINE COLOR = %s\n"
       "LINEPATTERN = %s\n"
       "PATTERNLENGTH = %G\n"
       "LINETHICKNESS = %G\n"
       "USELIGHTINGEFFECT = %s\n\n"

       "*** Vector Attributes ***\n"
       "SHOW = %s\n"
       "VECTORTYPE = %s\n"
       "ARROWHEADSTYLE = %s\n"
       "COLOR = %s\n"
       "ISTANGENT = %s\n"
       "LINEPATTERN = %s\n"
       "PATTERNLENGTH = %G\n"
       "LINETHICKNESS = %G\n\n"

       "*** Scatter Attributes ***\n"
       "SHOW = %s\n"
       "SYMBOLSHAPE = %s\n"
       "ISASCII = %s\n"
       "ASCIICHAR = %s\n"
       "COLOR = %s\n"
       "FILLMODE = %s\n"
       "FILLCOLOR = %s\n"
       "FRAMESIZE = %G\n"
       "SIZEBYVARIABLE = %s\n"
       "LINETHICKNESS = %G\n\n"

       "*** Shade Attributes ***\n"
       "SHOW = %s\n"
       "COLOR = %s\n"
       "USELIGHTINGEFFECT = %s\n\n"

       "*** Boundary Attributes ***\n"
       "SHOW = %s\n"
       "IBOUNDARY = %s\n"
       "JBOUNDARY = %s\n"
       "KBOUNDARY = %s\n"
       "COLOR = %s\n"
       "LINETHICKNESS = %G\n\n"

       "*** Points ***\n"
       "POINTSTOPLOT = %s\n"
       "IJKSKIP = (%d,%d,%d)\n\n"

       "*** Surfaces ***\n"
       "SURFACESTOPLOT = %s\n"
       "IRANGE = (%d,%d,%d)\n"
       "JRANGE = (%d,%d,%d)\n"
       "KRANGE = (%d,%d,%d)\n\n"

       "*** Volume Mode Attributes ***\n"
       "SHOWISOSURFACES = %s\n"
       "SHOWSLICES = %s\n"
       "SHOWSTREAMTRACES = %s\n\n"

       "*** Surface Effects ***\n"
       "USETRANSLUCENCY = %s\n"
       "SURFACETRANSLUCENCY = %d\n"
       "LIGHTINGEFFECT = %s\n"


       ,ZoneName,
       (int)TecUtilFieldStyleGetArbValue(Zone,SV_GROUP,NULL,NULL),

       /* MESH */
       Boolean_tToString((Boolean_t) TecUtilFieldStyleGetArbValue(Zone,SV_MESH,SV_SHOW,NULL)),
       MeshPlotType_eToString((MeshPlotType_e)TecUtilFieldStyleGetArbValue(Zone,SV_MESH,SV_MESHTYPE,NULL)),
       ColorIndexToString((ColorIndex_t)TecUtilFieldStyleGetArbValue(Zone,SV_MESH,SV_COLOR,NULL)),
       LinePattern_eToString((LinePattern_e)TecUtilFieldStyleGetArbValue(Zone,SV_MESH,SV_LINEPATTERN,NULL)),
       TecUtilFieldStyleGetDoubleValue(Zone,SV_MESH,SV_PATTERNLENGTH,NULL),
       TecUtilFieldStyleGetDoubleValue(Zone,SV_MESH,SV_LINETHICKNESS,NULL),

       /* CONTOUR */
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_CONTOUR,SV_SHOW,NULL)),
       ContourPlotType_eToString((ContourPlotType_e)TecUtilFieldStyleGetArbValue(Zone,SV_CONTOUR,SV_CONTOURTYPE,NULL)),
       ContourColoring_eToString((ContourColoring_e)TecUtilFieldStyleGetArbValue(Zone,SV_CONTOUR,SV_FLOODCOLORING,NULL)) ,
       ContourGroupToString((int)TecUtilFieldStyleGetArbValue(Zone,SV_CONTOUR,SV_LINECONTOURGROUP,NULL)),
       ColorIndexToString((ColorIndex_t)TecUtilFieldStyleGetArbValue(Zone,SV_CONTOUR,SV_COLOR,NULL)),
       LinePattern_eToString((LinePattern_e)TecUtilFieldStyleGetArbValue(Zone,SV_CONTOUR,SV_LINEPATTERN,NULL)),
       TecUtilFieldStyleGetDoubleValue(Zone,SV_CONTOUR,SV_PATTERNLENGTH,NULL),
       TecUtilFieldStyleGetDoubleValue(Zone,SV_CONTOUR,SV_LINETHICKNESS,NULL),
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_CONTOUR,SV_USELIGHTINGEFFECT,NULL)),

       /* Vector */
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_VECTOR,SV_SHOW,NULL)),
       VectorPlotType_eToString((VectorPlotType_e)TecUtilFieldStyleGetArbValue(Zone,SV_VECTOR,SV_VECTORTYPE,NULL)),
       ArrowheadStyle_eToString((ArrowheadStyle_e)TecUtilFieldStyleGetArbValue(Zone,SV_VECTOR,SV_ARROWHEADSTYLE,NULL)),
       ColorIndexToString((ColorIndex_t)TecUtilFieldStyleGetArbValue(Zone,SV_VECTOR,SV_COLOR,NULL)),
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_VECTOR,SV_ISTANGENT,NULL)),
       LinePattern_eToString((LinePattern_e)TecUtilFieldStyleGetArbValue(Zone,SV_VECTOR,SV_LINEPATTERN,NULL)),
       TecUtilFieldStyleGetDoubleValue(Zone,SV_VECTOR,SV_PATTERNLENGTH,NULL),
       TecUtilFieldStyleGetDoubleValue(Zone,SV_VECTOR,SV_LINETHICKNESS,NULL),

       /* Scatter */
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_SCATTER,SV_SHOW,NULL)),
       GeomShape_eToString((GeomShape_e)TecUtilFieldStyleGetArbValue(Zone,SV_SCATTER,SV_SYMBOLSHAPE,SV_GEOMSHAPE)),
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_SCATTER,SV_SYMBOLSHAPE,SV_ISASCII)),
       /* ASCIISHAPE actually goes down one more level, but TecUtilFieldStyleGetArbValue doesn't have enough parameters */
       SymbolChar_tToString((char *)TecUtilFieldStyleGetArbValue(Zone,SV_SCATTER,SV_SYMBOLSHAPE,SV_ASCIISHAPE)),
       ColorIndexToString((ColorIndex_t)TecUtilFieldStyleGetArbValue(Zone,SV_SCATTER,SV_COLOR,NULL)),
       FillMode_eToString((FillMode_e)TecUtilFieldStyleGetArbValue(Zone,SV_SCATTER,SV_FILLMODE,NULL)),
       ColorIndexToString((ColorIndex_t)TecUtilFieldStyleGetArbValue(Zone,SV_SCATTER,SV_FILLCOLOR,NULL)),
       TecUtilFieldStyleGetDoubleValue(Zone,SV_SCATTER,SV_FRAMESIZE,NULL),
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_SCATTER,SV_SIZEBYVARIABLE,NULL)),
       TecUtilFieldStyleGetDoubleValue(Zone,SV_SCATTER,SV_LINETHICKNESS,NULL),

       /* Shade */
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_SHADE,SV_SHOW,NULL)),
       ColorIndexToString((ColorIndex_t)TecUtilFieldStyleGetArbValue(Zone,SV_SHADE,SV_COLOR,NULL)),
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_SHADE,SV_USELIGHTINGEFFECT,NULL)),

       /* Boundary */
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_BOUNDARY,SV_SHOW,NULL)),
       BoundPlotType_eToString((BoundPlotType_e)TecUtilFieldStyleGetArbValue(Zone,SV_BOUNDARY,SV_IBOUNDARY,NULL)),
       BoundPlotType_eToString((BoundPlotType_e)TecUtilFieldStyleGetArbValue(Zone,SV_BOUNDARY,SV_JBOUNDARY,NULL)),
       BoundPlotType_eToString((BoundPlotType_e)TecUtilFieldStyleGetArbValue(Zone,SV_BOUNDARY,SV_KBOUNDARY,NULL)),
       ColorIndexToString((ColorIndex_t)TecUtilFieldStyleGetArbValue(Zone,SV_BOUNDARY,SV_COLOR,NULL)),
       TecUtilFieldStyleGetDoubleValue(Zone,SV_BOUNDARY,SV_LINETHICKNESS,NULL),

       /* Points */
       PointsToPlot_eToString((PointsToPlot_e)TecUtilFieldStyleGetArbValue(Zone,SV_POINTS,SV_POINTSTOPLOT,NULL)),
       (int) TecUtilFieldStyleGetArbValue(Zone,SV_POINTS,SV_IJKSKIP,SV_I),
       (int) TecUtilFieldStyleGetArbValue(Zone,SV_POINTS,SV_IJKSKIP,SV_J),
       (int) TecUtilFieldStyleGetArbValue(Zone,SV_POINTS,SV_IJKSKIP,SV_K),

       /* Surfaces */
       SurfacesToPlot_eToString((SurfacesToPlot_e)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACES,SV_SURFACESTOPLOT,NULL)),
       (int)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACES,SV_IRANGE,SV_MIN),
       (int)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACES,SV_IRANGE,SV_MAX),
       (int)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACES,SV_IRANGE,SV_SKIP),
       (int)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACES,SV_JRANGE,SV_MIN),
       (int)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACES,SV_JRANGE,SV_MAX),
       (int)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACES,SV_JRANGE,SV_SKIP),
       (int)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACES,SV_KRANGE,SV_MIN),
       (int)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACES,SV_KRANGE,SV_MAX),
       (int)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACES,SV_KRANGE,SV_SKIP),

       /* Volume */
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_VOLUMEMODE,SV_VOLUMEOBJECTSTOPLOT,SV_SHOWISOSURFACES)),
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_VOLUMEMODE,SV_VOLUMEOBJECTSTOPLOT,SV_SHOWSLICES)),
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_VOLUMEMODE,SV_VOLUMEOBJECTSTOPLOT,SV_SHOWSTREAMTRACES)),

       /* Effects */
       Boolean_tToString((Boolean_t)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACEEFFECTS,SV_USETRANSLUCENCY,NULL)),
       (int)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACEEFFECTS,SV_SURFACETRANSLUCENCY,NULL),
       LightingEffect_eToString((LightingEffect_e)TecUtilFieldStyleGetArbValue(Zone,SV_SURFACEEFFECTS,SV_LIGHTINGEFFECT,NULL))
       );

      strcat(Attr,buffer);

      if ( ZoneName )
        TecUtilStringDealloc(&ZoneName);
    }
  else
    {
      SetMaxNumZonesOrMaps(0);
      strcat(Attr,"The current plot type has no\n"
                  "zone or line map information.\n");
    }

  TecGUITextSetString(StyleResult_T_T2_1,Attr);

  if ( MapExtendedName )
    TecUtilStringDealloc(&MapExtendedName);

  if ( MapExtendedSettings )
    TecUtilStringDealloc(&MapExtendedSettings);

  if ( DataSetTitle )
    TecUtilStringDealloc(&DataSetTitle);

  /*
   * Update control sensitivies.
   */
  TecGUISetSensitivity(ZoneOrMap_TFS_T2_1, (MaxNumZonesOrMaps != 0));
}


static void SetupArgList(ArgList_pa  ArgList,
                         LgIndex_t   Offset1,
                         LgIndex_t   Offset2,
                         const char *P1,
                         const char *P2,
                         const char *P3,
                         const char *P4,
                         const char *P5,
                         const char *P6)
{
  if (P1) TecUtilArgListAppendString(ArgList, SV_P1, P1);
  if (P2) TecUtilArgListAppendString(ArgList, SV_P2, P2);
  if (P3) TecUtilArgListAppendString(ArgList, SV_P3, P3);
  if (P4) TecUtilArgListAppendString(ArgList, SV_P4, P4);
  if (P5) TecUtilArgListAppendString(ArgList, SV_P5, P5);
  if (P6) TecUtilArgListAppendString(ArgList, SV_P6, P6);
  TecUtilArgListAppendInt(ArgList,    SV_OFFSET1, Offset1);
  TecUtilArgListAppendInt(ArgList,    SV_OFFSET2, Offset2);
}

static void TackOnAttr(char *Attr,
                       LgIndex_t   Offset1,
                       LgIndex_t   Offset2,
                       const char *P1,
                       const char *P2,
                       const char *P3,
                       const char *P4,
                       const char *P5,
                       const char *P6,
                       const char *Value)
{
  sprintf(&Attr[strlen(Attr)],"%3ld ",(long)Offset1);
  sprintf(&Attr[strlen(Attr)],"%3ld ",(long)Offset2);
  if (P1) sprintf(&Attr[strlen(Attr)],"%s ",P1);
  if (P2) sprintf(&Attr[strlen(Attr)],"%s ",P2);
  if (P3) sprintf(&Attr[strlen(Attr)],"%s ",P3);
  if (P4) sprintf(&Attr[strlen(Attr)],"%s ",P4);
  if (P5) sprintf(&Attr[strlen(Attr)],"%s ",P5);
  if (P6) sprintf(&Attr[strlen(Attr)],"%s ",P6);
  sprintf(&Attr[strlen(Attr)],"= %s\n",Value);
}

static void TackOnAttrError(char                 *Attr,
                            GetValueReturnCode_e  GVRC,
                            LgIndex_t             Offset1,
                            LgIndex_t             Offset2,
                            const char           *P1,
                            const char           *P2,
                            const char           *P3,
                            const char           *P4,
                            const char           *P5,
                            const char           *P6)
{
  const char *Reason;

  TecUtilLockStart(AddOnID);

  if (GVRC == GetValue_ResultTypeError)
    Reason = "Result type error";
  else if (GVRC == GetValue_SyntaxError)
    Reason = "Syntax error";
  else
    Reason = "Uknown";

  strcpy(&Attr[strlen(Attr)], "Unable to obtain style value for: ");
  TackOnAttr(Attr,
             Offset1,Offset2,
             P1,P2,P3,P4,P5,P6,Reason);

  /* also show an error message */
    {
      char AttrErrString[512];
      strcpy(AttrErrString,"Unable to obtain style value for\n");
      TackOnAttr(AttrErrString,
                 Offset1,Offset2,
                 P1,P2,P3,P4,P5,P6,Reason);

      TecUtilDialogErrMsg(AttrErrString);
    }

  TecUtilLockFinish(AddOnID);
}



static void TackOnBoolAttr(char     *Attr,
                           LgIndex_t Offset1,
                           LgIndex_t Offset2,
                           const char *P1,
                           const char *P2,
                           const char *P3,
                           const char *P4,
                           const char *P5,
                           const char *P6)
{
  GetValueReturnCode_e Result;
  ArbParam_t  IValue;
  ArgList_pa  ArgList;

  TecUtilLockStart(AddOnID);

  ArgList = TecUtilArgListAlloc();

  SetupArgList(ArgList,Offset1,Offset2,P1,P2,P3,P4,P5,P6);
  TecUtilArgListAppendArbParamPtr(ArgList, SV_IVALUE,  &IValue);

  Result = TecUtilStyleGetLowLevelX(ArgList);

  if (Result == GetValue_Ok)
    {
      TackOnAttr(Attr,
                 Offset1,Offset2,
                 P1,P2,P3,P4,P5,P6,(Boolean_t)IValue ? "TRUE" : "FALSE");
      /*
       * Now as a check make sure that we can set the value and retrieve the
       * exact same value back again. This will test the interface shift and
       * other potential problems.
       */
      TecUtilArgListClear(ArgList);
      SetupArgList(ArgList,Offset1,Offset2,P1,P2,P3,P4,P5,P6);
      TecUtilArgListAppendArbParam(ArgList, SV_IVALUE,  IValue);
      if (TecUtilStyleSetLowLevelX(ArgList) == SetValue_DuplicateValue)
        {
          ArbParam_t SecondIValue;
          TecUtilArgListClear(ArgList);
          SetupArgList(ArgList,Offset1,Offset2,P1,P2,P3,P4,P5,P6);
          TecUtilArgListAppendArbParamPtr(ArgList, SV_IVALUE, &SecondIValue);
          if (TecUtilStyleGetLowLevelX(ArgList) != GetValue_Ok || SecondIValue != IValue)
            TecUtilDialogErrMsg("Second fetch did not return the same value as expected.");
        }
      else
        TecUtilDialogErrMsg("Did not get back duplicate value as expected.");
    }
  else
    TackOnAttrError(Attr, Result, Offset1,Offset2,P1,P2,P3,P4,P5,P6);

  TecUtilArgListDealloc(&ArgList);

  TecUtilLockFinish(AddOnID);
}

static void TackOnStrgAttr(char     *Attr,
                           LgIndex_t Offset1,
                           LgIndex_t Offset2,
                           const char *P1,
                           const char *P2,
                           const char *P3,
                           const char *P4,
                           const char *P5,
                           const char *P6)
{
  GetValueReturnCode_e Result;
  ArbParam_t  IValue;
  ArgList_pa  ArgList;

  TecUtilLockStart(AddOnID);

  ArgList = TecUtilArgListAlloc();

  SetupArgList(ArgList,Offset1,Offset2,P1,P2,P3,P4,P5,P6);
  TecUtilArgListAppendArbParamPtr(ArgList, SV_IVALUE,  &IValue);

  Result = TecUtilStyleGetLowLevelX(ArgList);

  if (Result == GetValue_Ok)
    {
      char *StrValue = (char *)IValue;
      TackOnAttr(Attr,
                 Offset1,Offset2,
                 P1,P2,P3,P4,P5,P6,StrValue ? StrValue : "NULL");
      /*
       * Now as a check make sure that we can set the value and retrieve the
       * exact same value back again. This will test the interface shift and
       * other potential problems.
       */
      TecUtilArgListClear(ArgList);
      SetupArgList(ArgList,Offset1,Offset2,P1,P2,P3,P4,P5,P6);
      TecUtilArgListAppendArbParam(ArgList, SV_IVALUE,  IValue);
      if (TecUtilStyleSetLowLevelX(ArgList) == SetValue_DuplicateValue)
        {
          ArbParam_t SecondIValue;
          char *SecondStrValue;
          GetValueReturnCode_e GVRC;
          TecUtilArgListClear(ArgList);
          SetupArgList(ArgList,Offset1,Offset2,P1,P2,P3,P4,P5,P6);
          TecUtilArgListAppendArbParamPtr(ArgList, SV_IVALUE, &SecondIValue);
          GVRC = TecUtilStyleGetLowLevelX(ArgList);
          SecondStrValue = (char *)SecondIValue;
          if (SecondStrValue != NULL)
            {
              if (GVRC != GetValue_Ok ||
                  !(SecondStrValue == NULL && StrValue == NULL ||
                    strcmp(SecondStrValue, StrValue) == 0))
                TecUtilDialogErrMsg("Second fetch did not return the same value as expected.");
              TecUtilStringDealloc(&SecondStrValue);
            }
        }
      else
        TecUtilDialogErrMsg("Did not get back duplicate value as expected.");

      if (StrValue != NULL)
        TecUtilStringDealloc(&StrValue);
    }
  else
    TackOnAttrError(Attr, Result, Offset1,Offset2,P1,P2,P3,P4,P5,P6);

  TecUtilArgListDealloc(&ArgList);

  TecUtilLockFinish(AddOnID);
}


static void TackOnIntgAttr(char     *Attr,
                           LgIndex_t Offset1,
                           LgIndex_t Offset2,
                           const char *P1,
                           const char *P2,
                           const char *P3,
                           const char *P4,
                           const char *P5,
                           const char *P6)
{
  GetValueReturnCode_e Result;
  ArbParam_t  IValue;
  ArgList_pa  ArgList;

  TecUtilLockStart(AddOnID);

  ArgList = TecUtilArgListAlloc();

  SetupArgList(ArgList,Offset1,Offset2,P1,P2,P3,P4,P5,P6);
  TecUtilArgListAppendArbParamPtr(ArgList, SV_IVALUE,  &IValue);

  Result = TecUtilStyleGetLowLevelX(ArgList);

  char IValueString[80];

  if (Result == GetValue_Ok)
    {
      sprintf(IValueString,"%ld",(long)IValue);
      TackOnAttr(Attr,
                 Offset1,Offset2,
                 P1,P2,P3,P4,P5,P6,IValueString);

      /*
       * With variable assignments if the variable was not set then the result
       * is TECUTILBADVARNUMBER which we can't send to Tecplot as a valid
       * assignment so we need to filter those cases out.
       */
      if (IMPLICATION(((strcmp(P1,SV_GLOBALCONTOUR)     == 0 && strcmp(P2,SV_VAR)  == 0) ||
                       (strcmp(P1,SV_GLOBALTWODVECTOR)  == 0 && strcmp(P2,SV_UVAR) == 0) ||
                       (strcmp(P1,SV_GLOBALTWODVECTOR)  == 0 && strcmp(P2,SV_VVAR) == 0) ||
                       (strcmp(P1,SV_GLOBALTHREEDVECTOR)== 0 && strcmp(P2,SV_UVAR) == 0) ||
                       (strcmp(P1,SV_GLOBALTHREEDVECTOR)== 0 && strcmp(P2,SV_VVAR) == 0) ||
                       (strcmp(P1,SV_GLOBALTHREEDVECTOR)== 0 && strcmp(P2,SV_WVAR) == 0) ||
                       (strcmp(P1,SV_GLOBALSCATTER)     == 0 && strcmp(P2,SV_VAR)  == 0) ||
                       (strcmp(P1,SV_GLOBALSCATTER)     == 0 && strcmp(P2,SV_DATALABELS) == 0 && strcmp(P3,SV_NODELABELVAR) == 0) ||
                       (strcmp(P1,SV_GLOBALSCATTER)     == 0 && strcmp(P2,SV_DATALABELS) == 0 && strcmp(P3,SV_CELLLABELVAR) == 0)),
                      IValue != TECUTILBADVARNUMBER))
        {
          /*
           * Now as a check make sure that we can set the value and retrieve the
           * exact same value back again. This will test the interface shift and
           * other potential problems.
           */
          TecUtilArgListClear(ArgList);
          SetupArgList(ArgList,Offset1,Offset2,P1,P2,P3,P4,P5,P6);
          TecUtilArgListAppendArbParam(ArgList, SV_IVALUE,  IValue);
          if (TecUtilStyleSetLowLevelX(ArgList) == SetValue_DuplicateValue)
            {
              ArbParam_t SecondIValue;
              TecUtilArgListClear(ArgList);
              SetupArgList(ArgList,Offset1,Offset2,P1,P2,P3,P4,P5,P6);
              TecUtilArgListAppendArbParamPtr(ArgList, SV_IVALUE, &SecondIValue);
              if (TecUtilStyleGetLowLevelX(ArgList) != GetValue_Ok || SecondIValue != IValue)
                TecUtilDialogErrMsg("Second fetch did not return the same value as expected.");
            }
          else
            TecUtilDialogErrMsg("Did not get back duplicate value as expected.");
        }
    }
  else
    TackOnAttrError(Attr, Result, Offset1,Offset2,P1,P2,P3,P4,P5,P6);

  TecUtilArgListDealloc(&ArgList);

  TecUtilLockFinish(AddOnID);
}

static void TackOnDbleAttr(char     *Attr,
                           LgIndex_t Offset1,
                           LgIndex_t Offset2,
                           const char *P1,
                           const char *P2,
                           const char *P3,
                           const char *P4,
                           const char *P5,
                           const char *P6)
{
  GetValueReturnCode_e Result;
  double      DValue;
  ArgList_pa  ArgList;

  TecUtilLockStart(AddOnID);

  ArgList = TecUtilArgListAlloc();

  SetupArgList(ArgList,Offset1,Offset2,P1,P2,P3,P4,P5,P6);
  TecUtilArgListAppendDoublePtr(ArgList, SV_DVALUE,  &DValue);

  Result = TecUtilStyleGetLowLevelX(ArgList);

  if (Result == GetValue_Ok)
    {
      char DValueString[80];
      sprintf(DValueString,"%lG",DValue);
      TackOnAttr(Attr,
                 Offset1,Offset2,
                 P1,P2,P3,P4,P5,P6,DValueString);
      /*
       * Now as a check make sure that we can set the value and retrieve the
       * exact same value back again. This will test the interface shift and
       * other potential problems.
       */
      TecUtilArgListClear(ArgList);
      SetupArgList(ArgList,Offset1,Offset2,P1,P2,P3,P4,P5,P6);
      TecUtilArgListAppendDouble(ArgList, SV_DVALUE,  DValue);
      if (TecUtilStyleSetLowLevelX(ArgList) == SetValue_DuplicateValue)
        {
          double SecondDValue;
          TecUtilArgListClear(ArgList);
          SetupArgList(ArgList,Offset1,Offset2,P1,P2,P3,P4,P5,P6);
          TecUtilArgListAppendDoublePtr(ArgList, SV_DVALUE, &SecondDValue);
          if (TecUtilStyleGetLowLevelX(ArgList) != GetValue_Ok || SecondDValue != DValue)
            TecUtilDialogErrMsg("Second fetch did not return the same value as expected.");
        }
      else
        TecUtilDialogErrMsg("Did not get back duplicate value as expected.");
    }
  else
    TackOnAttrError(Attr, Result, Offset1,Offset2,P1,P2,P3,P4,P5,P6);

  TecUtilArgListDealloc(&ArgList);

  TecUtilLockFinish(AddOnID);
}



/**
 */
static void UpdateGlobalStyleTabPage(void)
{
  static char Attr[100000];
  PlotType_e PlotType;
  LgIndex_t Offset1;
  LgIndex_t Offset2;

  strcpy(Attr,"");

  PlotType = TecUtilFrameGetPlotType();

  if ( TecUtilDataSetIsAvailable() )
    {
      for (Offset1 = 1; Offset1 <=4; Offset1++)
        {
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_SHOW,NULL,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_GENERATEAUTOLABELS,NULL,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_ALIGNAUTOLABELS,NULL,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_LABELWITHVALUE,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_AUTOLEVELSKIP,NULL,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_AUTOLABELSPACING,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_TEXTSHAPE,SV_FONT,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_TEXTSHAPE,SV_HEIGHT,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_TEXTSHAPE,SV_SIZEUNITS,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_COLOR,NULL,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_ISFILLED,NULL,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_FILLCOLOR,NULL,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_MARGIN,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_NUMFORMAT,SV_FORMATTING,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_NUMFORMAT,SV_CUSTOMLABEL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_NUMFORMAT,SV_PRECISION,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_NUMFORMAT,SV_SHOWDECIMALSONWHOLENUMBERS,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_NUMFORMAT,SV_REMOVELEADINGZEROS,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_NUMFORMAT,SV_SHOWNEGATIVESIGN,NULL,NULL);
          TackOnStrgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_NUMFORMAT,SV_POSITIVEPREFIX,NULL,NULL);
          TackOnStrgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_NUMFORMAT,SV_POSITIVESUFFIX,NULL,NULL);
          TackOnStrgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_NUMFORMAT,SV_NEGATIVEPREFIX,NULL,NULL);
          TackOnStrgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_NUMFORMAT,SV_NEGATIVESUFFIX,NULL,NULL);
          TackOnStrgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_NUMFORMAT,SV_ZEROPREFIX,NULL,NULL);
          TackOnStrgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LABELS,SV_NUMFORMAT,SV_ZEROSUFFIX,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_SHOW,NULL,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_SHOWHEADER,NULL,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_ROWSPACING,NULL,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_XYPOS,SV_X,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_XYPOS,SV_Y,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_ISVERTICAL,NULL,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_OVERLAYBARGRID,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_HEADERTEXTSHAPE,SV_FONT,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_HEADERTEXTSHAPE,SV_HEIGHT,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_HEADERTEXTSHAPE,SV_SIZEUNITS,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_NUMBERTEXTSHAPE,SV_FONT,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_NUMBERTEXTSHAPE,SV_HEIGHT,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_NUMBERTEXTSHAPE,SV_SIZEUNITS,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_TEXTCOLOR,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_BOX,SV_BOXTYPE,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_BOX,SV_MARGIN,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_BOX,SV_LINETHICKNESS,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_BOX,SV_COLOR,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_BOX,SV_FILLCOLOR,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_INCLUDECUTOFFLEVELS,NULL,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_AUTORESIZE,NULL,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_AUTOSIZEMAXLIMIT,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_LABELLOCATION,NULL,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_LABELINCREMENT,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_LEGEND,SV_ANCHOR,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_DEFNUMLEVELS,NULL,NULL,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORCUTOFF,SV_RANGEMIN,NULL,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORCUTOFF,SV_RANGEMAX,NULL,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORCUTOFF,SV_INCLUDEMIN,NULL,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORCUTOFF,SV_INCLUDEMAX,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_CONTOURLINESTYLE,SV_CONTOURLINEMODE,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_CONTOURLINESTYLE,SV_LINESKIP,NULL,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_CONTOURLINESTYLE,SV_PATTERNLENGTH,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_COLORMAPDISTRIBUTION,NULL,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_REVERSECOLORMAP,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_COLORMAPCYCLES,NULL,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_CONTINUOUSCOLOR,SV_CMIN,NULL,NULL);
          TackOnDbleAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_CONTINUOUSCOLOR,SV_CMAX,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_ZEBRA,SV_INCLUDE,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_ZEBRA,SV_ISTRANSPARENT,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_ZEBRA,SV_COLOR,NULL,NULL);
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_COLORMAPOVERRIDEACTIVE,NULL,NULL,NULL);
          for (Offset2 = 1; Offset2 <= MaxColorMapOverrides; Offset2++)
            {
              TackOnBoolAttr(Attr,Offset1,Offset2,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_COLORMAPOVERRIDE,SV_INCLUDE,NULL,NULL);
              TackOnIntgAttr(Attr,Offset1,Offset2,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_COLORMAPOVERRIDE,SV_COLOR,NULL,NULL);
              TackOnIntgAttr(Attr,Offset1,Offset2,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_COLORMAPOVERRIDE,SV_STARTLEVEL,NULL,NULL);
              TackOnIntgAttr(Attr,Offset1,Offset2,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_COLORMAPOVERRIDE,SV_ENDLEVEL,NULL,NULL);
            }
          TackOnBoolAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_COLORMAPFILTER,SV_USEFASTAPPROXCONTINUOUSFLOOD,NULL,NULL,NULL);
          TackOnIntgAttr(Attr,Offset1,0,SV_GLOBALCONTOUR,SV_VAR,NULL,NULL,NULL,NULL);
        }
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_RELATIVELENGTH,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_UNIFORMLENGTH,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_USERELATIVE,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_RELATIVELENGTHINGRIDUNITS,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_HEADSIZEASFRACTION,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_HEADSIZEINFRAMEUNITS,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_SIZEHEADBYFRACTION,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_ARROWHEADANGLE,NULL,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_UVAR,NULL,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_VVAR,NULL,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_WVAR,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_SHOW,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_COLOR,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_XYPOS,SV_X,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_XYPOS,SV_Y,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDE,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_LINETHICKNESS,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_ANGLE,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_SHOW,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_TEXTCOLOR,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_TEXTSHAPE,SV_FONT,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_TEXTSHAPE,SV_HEIGHT,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_TEXTSHAPE,SV_SIZEUNITS,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_FORMATTING,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_CUSTOMLABEL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_PRECISION,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_SHOWDECIMALSONWHOLENUMBERS,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_REMOVELEADINGZEROS,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_SHOWNEGATIVESIGN,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_POSITIVEPREFIX,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_POSITIVESUFFIX,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_NEGATIVEPREFIX,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_NEGATIVESUFFIX,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_ZEROPREFIX,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_ZEROSUFFIX,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTHREEDVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_OFFSET,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_RELATIVELENGTH,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_UNIFORMLENGTH,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_USERELATIVE,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_RELATIVELENGTHINGRIDUNITS,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_HEADSIZEASFRACTION,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_HEADSIZEINFRAMEUNITS,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_SIZEHEADBYFRACTION,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_ARROWHEADANGLE,NULL,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_UVAR,NULL,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_VVAR,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_SHOW,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_COLOR,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_XYPOS,SV_X,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_XYPOS,SV_Y,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDE,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_LINETHICKNESS,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_ANGLE,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_SHOW,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_TEXTCOLOR,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_TEXTSHAPE,SV_FONT,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_TEXTSHAPE,SV_HEIGHT,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_TEXTSHAPE,SV_SIZEUNITS,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_FORMATTING,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_CUSTOMLABEL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_PRECISION,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_SHOWDECIMALSONWHOLENUMBERS,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_REMOVELEADINGZEROS,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_SHOWNEGATIVESIGN,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_POSITIVEPREFIX,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_POSITIVESUFFIX,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_NEGATIVEPREFIX,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_NEGATIVESUFFIX,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_ZEROPREFIX,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_NUMFORMAT,SV_ZEROSUFFIX,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALTWODVECTOR,SV_REFVECTOR,SV_MAGNITUDELABEL,SV_OFFSET,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_VAR,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSCATTER,SV_RELATIVESIZE,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_RELATIVESIZEINGRIDUNITS,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_SHOW,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_SHOWTEXT,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_TEXTSHAPE,SV_FONT,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_TEXTSHAPE,SV_HEIGHT,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_TEXTSHAPE,SV_SIZEUNITS,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_TEXTCOLOR,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_BOX,SV_BOXTYPE,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_BOX,SV_MARGIN,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_BOX,SV_LINETHICKNESS,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_BOX,SV_COLOR,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_BOX,SV_FILLCOLOR,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_ROWSPACING,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_XYPOS,SV_X,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_XYPOS,SV_Y,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_LEGEND,SV_ANCHOR,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_SHOW,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_COLOR,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_LINETHICKNESS,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_ISFILLED,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_FILLCOLOR,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_XYPOS,SV_X,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_XYPOS,SV_Y,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_MAGNITUDE,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_SYMBOLSHAPE,SV_GEOMSHAPE,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_SYMBOLSHAPE,SV_ISASCII,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_SYMBOLSHAPE,SV_ASCIISHAPE,SV_USEBASEFONT,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_SYMBOLSHAPE,SV_ASCIISHAPE,SV_FONTOVERRIDE,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_REFSCATSYMBOL,SV_SYMBOLSHAPE,SV_ASCIISHAPE,SV_ASCIICHAR,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_SHOWNODELABELS,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_SHOWCELLLABELS,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_COLORBYZONEMAP,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_COLOR,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_TEXTSHAPE,SV_FONT,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_TEXTSHAPE,SV_HEIGHT,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_TEXTSHAPE,SV_SIZEUNITS,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NUMFORMAT,SV_FORMATTING,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NUMFORMAT,SV_CUSTOMLABEL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NUMFORMAT,SV_PRECISION,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NUMFORMAT,SV_SHOWDECIMALSONWHOLENUMBERS,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NUMFORMAT,SV_REMOVELEADINGZEROS,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NUMFORMAT,SV_SHOWNEGATIVESIGN,NULL,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NUMFORMAT,SV_POSITIVEPREFIX,NULL,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NUMFORMAT,SV_POSITIVESUFFIX,NULL,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NUMFORMAT,SV_NEGATIVEPREFIX,NULL,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NUMFORMAT,SV_NEGATIVESUFFIX,NULL,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NUMFORMAT,SV_ZEROPREFIX,NULL,NULL);
      TackOnStrgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NUMFORMAT,SV_ZEROSUFFIX,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_INCLUDEBOX,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NODELABELTYPE,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_NODELABELVAR,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_CELLLABELTYPE,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_CELLLABELVAR,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_DATALABELS,SV_INDEXSKIP,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_BASEFONT,NULL,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSCATTER,SV_SPHERESCATTERRENDERQUALITY,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_SHOW,NULL,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_DEFINITIONCONTOURGROUP,NULL,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_ISOSURFACESELECTION,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_ISOVALUE1,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_ISOVALUE2,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_ISOVALUE3,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_MESH,SV_SHOW,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_MESH,SV_MESHTYPE,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_MESH,SV_COLOR,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_MESH,SV_LINETHICKNESS,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_CONTOUR,SV_SHOW,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_CONTOUR,SV_CONTOURTYPE,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_CONTOUR,SV_FLOODCOLORING,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_CONTOUR,SV_LINECONTOURGROUP,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_CONTOUR,SV_COLOR,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_CONTOUR,SV_LINETHICKNESS,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_CONTOUR,SV_USELIGHTINGEFFECT,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_SHADE,SV_SHOW,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_SHADE,SV_USELIGHTINGEFFECT,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_SHADE,SV_COLOR,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_SURFACEEFFECTS,SV_LIGHTINGEFFECT,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_SURFACEEFFECTS,SV_SURFACETRANSLUCENCY,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_SURFACEEFFECTS,SV_USETRANSLUCENCY,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALISOSURFACE,SV_MARCHINGCUBEALGORITHM,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSLICE,SV_SHOW,NULL,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_SLICESURFACE,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSLICE,SV_POSITION1,SV_X,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSLICE,SV_POSITION1,SV_Y,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSLICE,SV_POSITION1,SV_Z,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_POSITION1,SV_I,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_POSITION1,SV_J,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_POSITION1,SV_K,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSLICE,SV_SHOWPOSITION2,NULL,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSLICE,SV_POSITION2,SV_X,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSLICE,SV_POSITION2,SV_Y,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSLICE,SV_POSITION2,SV_Z,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_POSITION2,SV_I,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_POSITION2,SV_J,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_POSITION2,SV_K,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSLICE,SV_SHOWINTERMEDIATESLICES,NULL,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_NUMINTERMEDIATESLICES,NULL,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSLICE,SV_MESH,SV_SHOW,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_MESH,SV_MESHTYPE,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_MESH,SV_COLOR,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSLICE,SV_MESH,SV_LINETHICKNESS,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSLICE,SV_CONTOUR,SV_SHOW,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_CONTOUR,SV_CONTOURTYPE,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_CONTOUR,SV_FLOODCOLORING,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_CONTOUR,SV_LINECONTOURGROUP,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_CONTOUR,SV_COLOR,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSLICE,SV_CONTOUR,SV_LINETHICKNESS,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSLICE,SV_CONTOUR,SV_USELIGHTINGEFFECT,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSLICE,SV_VECTOR,SV_SHOW,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_VECTOR,SV_VECTORTYPE,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_VECTOR,SV_ARROWHEADSTYLE,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_VECTOR,SV_COLOR,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSLICE,SV_VECTOR,SV_ISTANGENT,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSLICE,SV_VECTOR,SV_LINETHICKNESS,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSLICE,SV_SHADE,SV_SHOW,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSLICE,SV_SHADE,SV_USELIGHTINGEFFECT,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_SHADE,SV_COLOR,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSLICE,SV_BOUNDARY,SV_SHOW,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_BOUNDARY,SV_COLOR,NULL,NULL,NULL);
      TackOnDbleAttr(Attr,0,0,SV_GLOBALSLICE,SV_BOUNDARY,SV_LINETHICKNESS,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_SURFACEEFFECTS,SV_LIGHTINGEFFECT,NULL,NULL,NULL);
      TackOnIntgAttr(Attr,0,0,SV_GLOBALSLICE,SV_SURFACEEFFECTS,SV_SURFACETRANSLUCENCY,NULL,NULL,NULL);
      TackOnBoolAttr(Attr,0,0,SV_GLOBALSLICE,SV_SURFACEEFFECTS,SV_USETRANSLUCENCY,NULL,NULL,NULL);
    }
  else
    {
      strcpy(Attr,"No data set available.\n");
    }

  TecGUITextSetString(GlobalStyle_T_T3_1,Attr);
}

/**
 */
void UpdateCurrentTabPage(void)
{
  if (TecGUIDialogIsUp(Dialog1Manager))
    {
      if (CurrentTabPageID == Tab1_1Manager)
        UpdateSpecificTabPage();
      else if (CurrentTabPageID == Tab2_1Manager)
        UpdateZoneMapTabPage();
      else if (CurrentTabPageID == Tab3_1Manager)
        UpdateGlobalStyleTabPage();
    }
}

/**
 */
static int  ZoneOrMap_TFS_T2_1_ValueChanged_CB(const char *S)
{
  int IsOk = 1;
  PlotType_e PlotType;
  Boolean_t  IsLinePlot;
  LgIndex_t  ZoneOrMap;

  TecUtilLockStart(AddOnID);

  PlotType = TecUtilFrameGetPlotType();
  IsLinePlot = (PlotType == PlotType_XYLine || PlotType == PlotType_PolarLine);
  if (!(TecGUITextFieldValidateLgIndex(ZoneOrMap_TFS_T2_1,
                                       (IsLinePlot ? "Map" : "Zone"),
                                       1, MaxNumZonesOrMaps, TRUE) &&
        TecGUITextFieldGetLgIndex(ZoneOrMap_TFS_T2_1, &ZoneOrMap)))
    {
      TecGUITextFieldSetLgIndex(ZoneOrMap_TFS_T2_1, 1, FALSE);
    }
  else if (ZoneOrMap <= 0) /* ...MX syntax was used */
    {
      TecGUITextFieldSetLgIndex(ZoneOrMap_TFS_T2_1, ZoneOrMap, TRUE);
    }

  UpdateCurrentTabPage();

  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static void ZoneOrMap_TFS_T2_1_ButtonUp_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecGUISpinTextFieldIncLgIndex(ZoneOrMap_TFS_T2_1,
                                1,                  /* Increment */
                                1,                  /* MinDomain */
                                MaxNumZonesOrMaps); /* MaxDomain */
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void ZoneOrMap_TFS_T2_1_ButtonDown_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecGUISpinTextFieldIncLgIndex(ZoneOrMap_TFS_T2_1,
                                -1,                 /* Increment */
                                1,                  /* MinDomain */
                                MaxNumZonesOrMaps); /* MaxDomain */
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void GetValue_BTN_T1_1_CB(void)
{
  GetValueReturnCode_e Result;
  double      DValue;
  ArbParam_t  IValue;
  ArgList_pa  ArgList;
  char       *S1;
  char       *S2;
  char       *S3;
  char       *S4;
  char       *S5;
  char       *S6;
  LgIndex_t   Offset1;
  LgIndex_t   Offset2;

  TecUtilLockStart(AddOnID);

  ArgList = TecUtilArgListAlloc();
  S1 = TecGUITextFieldGetString(S1Value_TF_T1_1);
  S2 = TecGUITextFieldGetString(S2Value_TF_T1_1);
  S3 = TecGUITextFieldGetString(S3Value_TF_T1_1);
  S4 = TecGUITextFieldGetString(S4Value_TF_T1_1);
  S5 = TecGUITextFieldGetString(S5Value_TF_T1_1);
  S6 = TecGUITextFieldGetString(S6Value_TF_T1_1);
  TecGUITextFieldGetLgIndex(Offset1Value_TF_T1_1, &Offset1);
  TecGUITextFieldGetLgIndex(Offset2Value_TF_T1_1, &Offset2);

  /* first try to fetch the requested item as a double */
  TecUtilArgListAppendString(ArgList,    SV_P1,      S1);
  TecUtilArgListAppendString(ArgList,    SV_P2,      S2);
  TecUtilArgListAppendString(ArgList,    SV_P3,      S3);
  TecUtilArgListAppendString(ArgList,    SV_P4,      S4);
  TecUtilArgListAppendString(ArgList,    SV_P5,      S5);
  TecUtilArgListAppendString(ArgList,    SV_P6,      S6);
  TecUtilArgListAppendInt(ArgList,       SV_OFFSET1, Offset1);
  TecUtilArgListAppendInt(ArgList,       SV_OFFSET2, Offset2);
  TecUtilArgListAppendDoublePtr(ArgList, SV_DVALUE,  &DValue);

  Result = TecUtilStyleGetLowLevelX(ArgList);
  if (Result == GetValue_Ok)
    {
      TecGUITextFieldSetDouble(GetValueResul_TF_T1_1, DValue, "%lg");
      TecGUILabelSetText(Units_LBL_T1_1, "double");
    }
  else if (Result == GetValue_ResultTypeError)
    {
      /* the syntax required an ArbParam_t return */
      TecUtilArgListClear(ArgList);
      TecUtilArgListAppendString(ArgList,      SV_P1,      S1);
      TecUtilArgListAppendString(ArgList,      SV_P2,      S2);
      TecUtilArgListAppendString(ArgList,      SV_P3,      S3);
      TecUtilArgListAppendString(ArgList,      SV_P4,      S4);
      TecUtilArgListAppendString(ArgList,      SV_P5,      S5);
      TecUtilArgListAppendString(ArgList,      SV_P6,      S6);
      TecUtilArgListAppendInt(ArgList,         SV_OFFSET1, Offset1);
      TecUtilArgListAppendInt(ArgList,         SV_OFFSET2, Offset2);
      TecUtilArgListAppendArbParamPtr(ArgList, SV_IVALUE,  &IValue);

      /* make the call again */
      Result = TecUtilStyleGetLowLevelX(ArgList);

      TecGUITextFieldSetLgIndex(GetValueResul_TF_T1_1,
                                (LgIndex_t)IValue, FALSE);
      TecGUILabelSetText(Units_LBL_T1_1, "ArbParam");
      /*
       * NOTE: some ArbParam return values are allocated strings. In a real
       * addon the addon would know if it asked for a string and would have
       * to deallocated it when finished. For this sample we won't bother and
       * will just accept the memory leak.
       */
    }
  else
    TecUtilDialogErrMsg("TecUtilStyleGetLowLevelX syntax error");

  /* cleanup */
  if (S1 != NULL) TecUtilStringDealloc(&S1);
  if (S2 != NULL) TecUtilStringDealloc(&S2);
  if (S3 != NULL) TecUtilStringDealloc(&S3);
  if (S4 != NULL) TecUtilStringDealloc(&S4);
  if (S5 != NULL) TecUtilStringDealloc(&S5);
  if (S6 != NULL) TecUtilStringDealloc(&S6);
  TecUtilArgListDealloc(&ArgList);

  TecUtilLockFinish(AddOnID);
}

/**
 */
static int  Offset1Value_TF_T1_1_CB(const char *S)
{
  int IsOk = 1;
  LgIndex_t Offset1;
  TecUtilLockStart(AddOnID);
  if (!(TecGUITextFieldValidateLgIndex(Offset1Value_TF_T1_1, "Offset1",
                                       -MAXINDEX, MAXINDEX, FALSE) &&
        TecGUITextFieldGetLgIndex(Offset1Value_TF_T1_1, &Offset1)))
    {
      TecGUITextFieldSetLgIndex(Offset1Value_TF_T1_1, 1, FALSE);
    }
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static int  Offset2Value_TF_T1_1_CB(const char *S)
{
  int IsOk = 1;
  LgIndex_t Offset2;
  TecUtilLockStart(AddOnID);
  if (!(TecGUITextFieldValidateLgIndex(Offset2Value_TF_T1_1, "Offset2",
                                       -MAXINDEX, MAXINDEX, FALSE) &&
        TecGUITextFieldGetLgIndex(Offset2Value_TF_T1_1, &Offset2)))
    {
      TecGUITextFieldSetLgIndex(Offset2Value_TF_T1_1, 1, FALSE);
    }
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static int  S1Value_TF_T1_1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static int  S2Value_TF_T1_1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static int  S3Value_TF_T1_1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static int  S4Value_TF_T1_1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static int  S5Value_TF_T1_1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static int  S6Value_TF_T1_1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static int  GetValueResul_TF_T1_1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void Dialog1CloseButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void Dialog1Init_CB(void)
{
  TecUtilLockStart(AddOnID);
  if (CurrentTabPageID == BADGUIID)
    {
      /*
       * Dialog has never been displayed; TGB will set it to the first page so
       * we need to reflect that fact with our local variable that keeps track
       * of the top tab page.
       */
      CurrentTabPageID = Tab1_1Manager;

      /* default values for page 1 */
      TecGUITextFieldSetLgIndex(Offset1Value_TF_T1_1, 1, FALSE);
      TecGUITextFieldSetLgIndex(Offset2Value_TF_T1_1, 1, FALSE);

      /* default values for page 2 */
      TecGUITextFieldSetLgIndex(ZoneOrMap_TFS_T2_1, 1, FALSE);
    }
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void TAB1_TBA_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  CurrentTabPageID = *I;
  UpdateCurrentTabPage();
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void TAB1_TBD_D1_CB(const int *I)
{
  /* ...nothing to do when we leave a tab page */
}

#include "guibld.cpp"
