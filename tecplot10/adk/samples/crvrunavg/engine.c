#include "TECADDON.h"


#include "ADDGLBL.h"
#include "ENGINE.h"



static double CalcAvgAtRawDataPoint(double        IndV,
                                    FieldData_pa  RawIndV,
                                    FieldData_pa  RawDepV,
                                    LgIndex_t     NumRawPts)
{
  int    ii;
  int    Count = 0;
  double Sum   = 0;
  
  for (ii = 0; ii < NumRawPts; ii++)
    {
      /* Only sum up the values to the left of IndV */
      if (TecUtilDataValueGetByRef(RawIndV, ii+1) <= IndV)
        {
          Sum += TecUtilDataValueGetByRef(RawDepV, ii+1);
          Count++;
        }
    }
  return (Sum/Count);
}



/*
 * This function returns the zero base indicies of the data points
 * on either side of the current data point.  It is assumed that
 * the data is sorted in ascending order.
 */
static Boolean_t GetIndicesSurroundingIndV(double       IndV,
                                           FieldData_pa RawIndV,
                                           LgIndex_t    NumRawPts,
                                           LgIndex_t   *Idx1,
                                           LgIndex_t   *Idx2)
{  
  Boolean_t Found = FALSE;
  int ii;

  for (ii = 1; !Found && ii < NumRawPts; ii++)
    {      
      if (TecUtilDataValueGetByRef(RawIndV, ii+1) >= IndV)
        {
          *Idx1 = ii-1;
          *Idx2 = ii;
          Found = ( IndV >= TecUtilDataValueGetByRef(RawIndV, *Idx1+1) &&
                    IndV <= TecUtilDataValueGetByRef(RawIndV, *Idx2+1) );            
        }
    }      
  return Found;
}


static Boolean_t CalcDepValueAtCurvePoint(double        IndVCurveValue,
                                          double       *DepVCurveValue,
                                          FieldData_pa  RawIndV,
                                          FieldData_pa  RawDepV,
                                          LgIndex_t     NumRawPts,
                                          double       *Averages)
{
  Boolean_t IsOk = TRUE;
  LgIndex_t Idx1;
  LgIndex_t Idx2;
  double    RawIndValue1;
  double    RawIndValue2;
  double    IndValue1Weight;
  double    IndValue2Weight;

  *DepVCurveValue = 0;

  /*
   * Get the indices into the raw data for the data points
   * surrounding the given IndVCurveValue.
   */
  IsOk = GetIndicesSurroundingIndV(IndVCurveValue,
                                   RawIndV,
                                   NumRawPts,
                                   &Idx1,
                                   &Idx2);
  if (IsOk)
    {
      /*
       * Use the indices returned above to perform a linear
       * interpolation of the dependent value between the two 
       * data points that surround the independent value.
       */
      RawIndValue1 = TecUtilDataValueGetByRef(RawIndV, Idx1+1);
      RawIndValue2 = TecUtilDataValueGetByRef(RawIndV, Idx2+1);

      IndValue2Weight = (IndVCurveValue - RawIndValue1)/(RawIndValue2 - RawIndValue1);
      IndValue1Weight = (1 - IndValue2Weight);

      *DepVCurveValue = (Averages[Idx1]*IndValue1Weight) + (Averages[Idx2]*IndValue2Weight);
    }
  return IsOk;
}



static void CreateArrayOfAverages(FieldData_pa  RawIndV,
                                  FieldData_pa  RawDepV,
                                  LgIndex_t     NumRawPts,
                                  double       *Averages)
{
  double IndV;
  int    ii;
  
  for (ii = 0; ii < NumRawPts; ii++)
    {
      IndV = TecUtilDataValueGetByRef(RawIndV, ii+1);
      Averages[ii] = CalcAvgAtRawDataPoint(IndV,
                                           RawIndV,
                                           RawDepV,
                                           NumRawPts);
    }
}


Boolean_t STDCALL XYDataPointsCallback(FieldData_pa RawIndV,
                                       FieldData_pa RawDepV,
                                       CoordScale_e IndVCoordScale,
                                       CoordScale_e DepVCoordScale,
                                       LgIndex_t    NumRawPts,
                                       LgIndex_t    NumCurvePts,
                                       EntIndex_t   XYMapNum,
                                       char        *CurveSettings,
                                       double      *IndCurveValues,
                                       double      *DepCurveValues)
{
  Boolean_t IsOk  = TRUE;
  int       ii;
  double    Delta = 0.0;
  double    IndV;
  double    DepV;
  double    IndVarMin, 
            IndVarMax;
  double   *Averages = NULL;
  
  TecUtilLockStart(AddOnID);

  /*
   * Allocate an array which will store the 
   * running average at each data point.
   */
  Averages = malloc(NumRawPts*sizeof(double));
  if (Averages == NULL)
    IsOk = FALSE;

  if (IsOk)
    {
      /*
       * Fill the Averages array with the running average values.
       */
      CreateArrayOfAverages(RawIndV,
                            RawDepV,
                            NumRawPts,
                            Averages);

      /*
       * Get the min and max values of the independent variable
       * and use these values to calculate size of each step along
       * the independent axis for the curve fit.
       */
      TecUtilDataValueGetMinMaxByRef(RawIndV, 
                                     &IndVarMin,
                                     &IndVarMax);
      
      Delta = (IndVarMax-IndVarMin)/(NumCurvePts-1);

      /* 
       * Step through all the points along the curve and calculate
       * the running average at each step.  Then copy the values into
       * the IndCurveValues and DepCurveValues arrays.
       */
      for (ii = 0; IsOk && ii < NumCurvePts; ii++)
        {          
          IndV = ii*Delta + IndVarMin;
          if (IndV > IndVarMax)
            IndV = IndVarMax;
          IsOk = CalcDepValueAtCurvePoint(IndV,
                                          &DepV,
                                          RawIndV,
                                          RawDepV,
                                          NumRawPts,
                                          Averages);
          IndCurveValues[ii] = IndV;
          DepCurveValues[ii] = DepV;          
        }      
    }
  if (Averages != NULL)
    free(Averages);
  
  TecUtilLockFinish(AddOnID);
  return IsOk;
}


/*
 * The ProbeValueCallback is optional.  If this function
 * is not specified, Tecplot will do a linear interpolation
 * based on the points returned by XYDataPointsCallback
 */
Boolean_t STDCALL ProbeValueCallback(FieldData_pa RawIndV,
                                     FieldData_pa RawDepV,
                                     CoordScale_e IndVCoordScale,
                                     CoordScale_e DepVCoordScale,
                                     LgIndex_t    NumRawPts,
                                     LgIndex_t    NumCurvePts,
                                     EntIndex_t   XYMapNum,
                                     char        *CurveSettings,
                                     double       ProbeIndValue,
                                     double      *ProbeDepValue)
{
  Boolean_t IsOk = TRUE;
  double    *Averages = NULL;
  TecUtilLockStart(AddOnID);

  Averages = malloc(NumRawPts*sizeof(double));
  if (Averages == NULL)
    IsOk = FALSE;

  if(IsOk)
    {
      CreateArrayOfAverages(RawIndV,
                            RawDepV,
                            NumRawPts,
                            Averages);

      IsOk = CalcDepValueAtCurvePoint(ProbeIndValue,
                                      ProbeDepValue, /* return value */
                                      RawIndV,
                                      RawDepV,
                                      NumRawPts,
                                      Averages);
    }
  if (Averages != NULL)
    free(Averages);

  TecUtilLockFinish(AddOnID);
  return IsOk;
}


/*
 * The CurveInfoStringCallback function is optional.  If
 * no function is registered with Tecplot, no information
 * will be displayed in the XY-Curve Info dialog.
 */
Boolean_t STDCALL CurveInfoStringCallback(FieldData_pa RawIndV,
                                          FieldData_pa RawDepV,
                                          CoordScale_e IndVCoordScale,
                                          CoordScale_e DepVCoordScale,
                                          LgIndex_t    NumRawPts,
                                          EntIndex_t   XYMapNum,
                                          char        *CurveSettings,
                                          char       **CurveInfoString)
{
  double     IndVarMin, 
             IndVarMax;  
  double     FirstDepVal,
             LastDepVal;
  double     Slope;
  TecUtilLockStart(AddOnID);

  TecUtilDataValueGetMinMaxByRef(RawIndV, 
                                 &IndVarMin,
                                 &IndVarMax);
  FirstDepVal = CalcAvgAtRawDataPoint(IndVarMin,
                                      RawIndV,
                                      RawDepV,
                                      NumRawPts);
  LastDepVal = CalcAvgAtRawDataPoint(IndVarMax,
                                     RawIndV,
                                     RawDepV,
                                     NumRawPts);
  
  Slope = (LastDepVal-FirstDepVal)/(IndVarMax-IndVarMin);

  *CurveInfoString = TecUtilStringAlloc(1000, "CurveInfoString");
  sprintf(*CurveInfoString, "First: %G\nLast: %G\nAverage Slope: %G",FirstDepVal,LastDepVal,Slope);

  TecUtilLockFinish(AddOnID);
  return TRUE;
}
