#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "ENGINE.h"

/* DOCSTART:gr/polyint_PolyInt.txt */
/**
 * Interpolate y=f(x) using the method given in Stineman article from
 * Creative Computing (July 1980). At least 3 points required for
 * interpolation, if fewer then use linear interpolation...
 *
 * Data is treated as a 1 based array, while lx,ly,lxn,lyn are treated as 0
 * base.
 *
 * @param npts
 *     number of original data points
 * @param lx
 *     location of x data points
 * @param ly
 *     location of y data points
 * @param nptn
 *     number of points on the fitted curve
 * @param lxn
 *     location of fitted x points
 * @param lyn
 *     location of fitted y points
 * @param data
 *     working array 
 */
void PolyInt(int     npts,
             int     lx,
             int     ly,
             int     nptn,
             int     lxn,
             int     lyn,
             double *data)
{
  int    j,j1,i,ix,jx,kx,ixx,jxx;
  double xv,yv,dydx,dydx1,s,y0,dyj,dyj1;

  j  = 1;
  j1 = j+1;

  /* Isolate the data(lx+j) and the data(lx+j+1) that bracket xv... */
  for (i=1; i<=nptn; i++)
    {
      xv = data[lxn+i];
      while (xv > data[lx+j1])
        {
          j++;
          j1 = j+1;
        }

      if (npts == 1)
        yv = data[ly+j];

      if (npts == 2)
        yv = data[ly+2]-(data[lx+j1]-xv)*(data[ly+j1]-data[ly+j])/
            (data[lx+j1]-data[lx+j]);

      if (npts >= 3)
        {
          /*
           * Calculate the slope at the jth point (from fitting a circle thru
           * 3 points and getting slope of circle).
           */         
          ix = 1;
          jx = 2;
          kx = 3;
          if (j != 1)
            {
              ix = j-1;
              jx = j;
              kx = j+1;
            }

          dydx = (((data[ly+jx]-data[ly+ix])*
                   (pow(data[lx+kx]-data[lx+jx],2)+
                    pow(data[ly+kx]-data[ly+jx],2))+
                   (data[ly+kx]-data[ly+jx])*
                   (pow(data[lx+jx]-data[lx+ix],2)+
                    pow(data[ly+jx]-data[ly+ix],2)))/
                  ((data[lx+jx]-data[lx+ix])*
                   (pow(data[lx+kx]-data[lx+jx],2)+
                    pow(data[ly+kx]-data[ly+jx],2))+
                   (data[lx+kx]-data[lx+jx])*
                   (pow(data[lx+jx]-data[lx+ix],2)+
                    pow(data[ly+jx]-data[ly+ix],2))));
          if (j == 1)
            {
              ixx = ix;
              jxx = jx;
              s = (data[ly+jxx]-data[ly+ixx])/(data[lx+jxx]-data[lx+ixx]);

              if (s != 0.0)
                {
                  if (!((s >= 0.0 && s > dydx) || (s <= 0.0 && s < dydx)))
                    dydx = s+(fabs(s)*(s-dydx))/(fabs(s)+fabs(s-dydx));
                  else
                    dydx = 2.0*s-dydx;
                }
            }

          /* Calculate the slope at j+1 point. */
          ix = nptn-2;
          jx = nptn-1;
          kx = nptn;

          if (j1 != nptn)
            {
              ix = j1-1;
              jx = j1;
              kx = j1+1;
            }
          dydx1 = (((data[ly+jx]-data[ly+ix])*
                    (pow(data[lx+kx]-data[lx+jx],2.)+
                     pow(data[ly+kx]-data[ly+jx],2.))+
                    (data[ly+kx]-data[ly+jx])*
                    (pow(data[lx+jx]-data[lx+ix],2.)+
                     pow(data[ly+jx]-data[ly+ix],2.)))/
                   ((data[lx+jx]-data[lx+ix])*
                    (pow(data[lx+kx]-data[lx+jx],2.)+
                     pow(data[ly+kx]-data[ly+jx],2.))+
                    (data[lx+kx]-data[lx+jx])*
                    (pow(data[lx+jx]-data[lx+ix],2.)+
                     pow(data[ly+jx]-data[ly+ix],2.))));

          if (j1 == nptn)
            {
              ixx = jx;
              jxx = kx;
              s = (data[ly+jxx]-data[ly+ixx])/
                  (data[lx+jxx]-data[lx+ixx]);
              if (s != 0.0)
                {
                  if (!((s >= 0.0 && s > dydx1) ||
                        (s <= 0.0 && s < dydx1)))
                    dydx1 = s+(fabs(s)*(s-dydx1))/(fabs(s)+fabs(s-dydx1));
                  else
                    dydx1 = 2.0*s-dydx1;
                }
            }

          /* 
           * Calculate s=slope between j and j+1 points
           * y0   = y-value if linear interp used
           * dyj  = delta-y at the j-th point
           * dyj1 = delta-y at the j+1 point
           */
          s    = (data[ly+j1]-data[ly+j])/(data[lx+j1]-data[lx+j]);
          y0   = data[ly+j]+s*(xv-data[lx+j]);
          dyj  = data[ly+j]+dydx*(xv-data[lx+j])-y0;
          dyj1 = data[ly+j1]+dydx1*(xv-data[lx+j1])-y0;

          /* Calculate y... */

          if (dyj*dyj1 == 0.0)
            yv = y0;
          if (dyj*dyj1 > 0.0)
            yv = y0+(dyj*dyj1)/(dyj+dyj1);
          if (dyj*dyj1 < 0.0)
            yv = y0+((dyj*dyj1*(xv-data[lx+j]+xv-data[lx+j1]))/
                     ((dyj-dyj1)*(data[lx+j1]-data[lx+j])));
        }

      data[lyn+i] = yv;
    }
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/polyint_PrepareWorkingArray.txt */
static void PrepareWorkingArray(FieldData_pa  RawIndV,
                                FieldData_pa  RawDepV,
                                LgIndex_t     NumRawPts,
                                LgIndex_t     NumCurvePts,
                                int          *lx,
                                int          *ly,
                                int          *lxn,
                                int          *lyn,
                                double       *Data)
{
  double FirstValidPoint;
  double LastValidPoint;
  double StepSize;
  int    ii;

  /* 
   * The followint are indices to start points of
   * the data blocks in the 1 based arrray, Data
   *   lx  - Start of the raw Independent data.
   *   ly  - Start of the raw   Dependent data.
   *   lxn - Start of the calculated independent data.
   *   lyn - Start of the calculated   dependent data.
   *
   * The PolyInt function treats lx,ly,lxn,lyn as 0 base
   * indices, but treats Data as a 1 base array.
   */
  *lx = 0;
  *ly = NumRawPts;
  *lxn = 2*NumRawPts;
  *lyn = 2*NumRawPts+NumCurvePts;

  /* Fill the first blocks of the Data array with the Raw Data Values. */
  for (ii = 1; ii <= NumRawPts; ii++)
    {
      Data[*lx+ii] = TecUtilDataValueGetByRef(RawIndV, ii);
      Data[*ly+ii] = TecUtilDataValueGetByRef(RawDepV, ii);
    }

  /*
   * Calculate the size of steps to take while stepping
   * along the independent variable range. 
   */
  TecUtilDataValueGetMinMaxByRef(RawIndV,
                                 &FirstValidPoint,
                                 &LastValidPoint);
  StepSize = (LastValidPoint-FirstValidPoint)/(NumCurvePts-1);

  /* 
   * Fill the third block of the Data array with the
   * calculated independent values.
   */
  for (ii = 1; ii <= NumCurvePts; ii++)
    {
      double IndV = FirstValidPoint + (ii-1)*StepSize;
      if (IndV > LastValidPoint)
        IndV = LastValidPoint;
      Data[*lxn+ii] = IndV;
    }
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/polyint_ExtractCurveValuesFromWorkingArray.txt */
static void ExtractCurveValuesFromWorkingArray(LgIndex_t  NumCurvePts,
                                               int        lxn,
                                               int        lyn,
                                               double    *Data,
                                               double    *IndCurveValues,
                                               double    *DepCurveValues)
{
  int ii;
  for (ii = 1; ii <= NumCurvePts; ii++)
    {
      IndCurveValues[ii-1] = Data[lxn+ii];
      DepCurveValues[ii-1] = Data[lyn+ii];
    }
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/polyint_XYDataPointsCallback.txt */
Boolean_t STDCALL XYDataPointsCallback(FieldData_pa  RawIndV,
                                       FieldData_pa  RawDepV,
                                       CoordScale_e  IndVCoordScale,
                                       CoordScale_e  DepVCoordScale,
                                       LgIndex_t     NumRawPts,
                                       LgIndex_t     NumCurvePts,
                                       EntIndex_t    XYMapNum,
                                       char         *CurveSettings,
                                       double       *IndCurveValues,
                                       double       *DepCurveValues)
{
  Boolean_t IsOk = TRUE;
  int       ii;
  double    *Data = NULL;
  int       TotalNumDataPts;

  TecUtilLockStart(AddOnID);

  /*
   * Data will contain all the data points and is 1 base:
   *   RawIndpts
   *   RawDepPts
   *   IndCurveValues
   *   DepCurveValues
   * Therefore, the array must be large enough to 
   * contain all these points: 2*(NumRawPts+NumCurvePts).
   */
  TotalNumDataPts = 2*(NumRawPts+NumCurvePts);
  Data = malloc((TotalNumDataPts+1)*sizeof(double));
  if (Data != NULL)
    {
      /* Initialize Data to contain all zero. */
      for (ii = 0; ii < TotalNumDataPts+1; ii++)
        Data[ii] = 0;
    }
  else
    IsOk = FALSE;

  if (IsOk)
    {
      int lx;
      int ly;
      int lxn;
      int lyn;

      /* Setup the working array, Data. */
      PrepareWorkingArray(RawIndV,
                          RawDepV,
                          NumRawPts,
                          NumCurvePts,
                          &lx,
                          &ly,
                          &lxn,
                          &lyn,
                          Data);
      /* Perform the curve fit. */
      PolyInt(NumRawPts,
              lx,
              ly,
              NumCurvePts,
              lxn,
              lyn,
              Data);

      /* Extract the values from Data that were placed there by the curve fit. */
      ExtractCurveValuesFromWorkingArray(NumCurvePts,
                                         lxn,
                                         lyn,
                                         Data,
                                         IndCurveValues,
                                         DepCurveValues);
      free(Data);
    }
  TecUtilLockFinish(AddOnID);
  return IsOk;
}
/* DOCEND */
                                   
/**
 * Function to compute the average of the raw dependent variable for the
 * default fit (straight line at average).
 *
 * REMOVE THIS FUNCTION FOR OTHER FITS.
 */
double SimpleAverage(FieldData_pa RawDepV,    
                     LgIndex_t    NumRawPts)  
{
  int    ii;
  int    Count   = 0;
  double Sum     = 0;

  for (ii = 0; ii < NumRawPts; ii++)
    {
      Sum += TecUtilDataValueGetByRef(RawDepV, ii+1);
      Count++;
    }

  return (Sum/Count);
}

/**
 */
/* DOCSTART:gr/polyint_InsertProbeValueInWorkingArray.txt */
static Boolean_t InsertProbeValueInWorkingArray(double     ProbeIndValue,
                                                LgIndex_t  NumCurvePts,
                                                int        lxn,
                                                int       *ProbeValueIndex,
                                                double    *Data)
{
  Boolean_t Found = FALSE;
  int       ii;

  for (ii = 1; ii < NumCurvePts; ii++)
    {
      /* If the probed value is between the data points record its location. */
      if (ProbeIndValue >= Data[lxn+ii] &&
          ProbeIndValue <= Data[lxn+ii+1])
        {
          *ProbeValueIndex = ii;
          Data[lxn+ii] = ProbeIndValue;
          Found = TRUE;
          break;
        }
    }
  return Found;
}
/* DOCEND */

/* DOCSTART:gr/polyint_ProbeValueCallback.txt */
/**
 */
#define NUMPTSFORPROBING 3000

/**
 * This functions follows a similar process as the XYDataPointsCallback,
 * except it manually inserts ProbeIndValue in the list of the
 * independent curve points.  It stores the index in the Data array for
 * that value and uses that relative location to find the calculated
 * ProbeDepValue.
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
  int       ii;
  double    FirstValidPoint;
  double    LastValidPoint;
  double    *Data = NULL;
  int       TotalNumDataPts;

  TecUtilLockStart(AddOnID);

  /* Make sure the probe is within the bounds of the data. */
  TecUtilDataValueGetMinMaxByRef(RawIndV,
                                 &FirstValidPoint,
                                 &LastValidPoint);
  IsOk = (ProbeIndValue >= FirstValidPoint &&
          ProbeIndValue <=  LastValidPoint);

  if (IsOk)
    {
      /*
       * If the Curve has too few points, crank the number of points
       * on the curve up, so we get a good approximation of the curve.
       */
      NumCurvePts = MAX(NUMPTSFORPROBING, NumCurvePts);

      TotalNumDataPts = 2*(NumRawPts+NumCurvePts);
      Data = malloc((TotalNumDataPts+1)*sizeof(double));
      if (Data != NULL)
        {
          /* Initialize Data to contain all zero. */
          for (ii = 0; ii < TotalNumDataPts+1; ii++)
            Data[ii] = 0;
        }
      else
        IsOk = FALSE;
    }

  if (IsOk)
    {
      int lx,ly,lxn,lyn;
      int ProbeValueIndex = -1;

      PrepareWorkingArray(RawIndV,
                          RawDepV,
                          NumRawPts,
                          NumCurvePts,
                          &lx,
                          &ly,
                          &lxn,                       
                          &lyn,
                          Data);
      IsOk = InsertProbeValueInWorkingArray(ProbeIndValue,
                                            NumCurvePts,
                                            lxn,
                                            &ProbeValueIndex,
                                            Data);
      if (IsOk && ProbeValueIndex != -1)
        {
          /* Perform the curve fit. */
          PolyInt(NumRawPts,
                  lx,
                  ly,
                  NumCurvePts,
                  lxn,
                  lyn,
                  Data);
          /* The dependent value is in the same relative location. */
          /* as the probed independent value. */
          *ProbeDepValue = Data[lyn+ProbeValueIndex];
        }
    }
  if (Data != NULL)
    free(Data);
  TecUtilLockFinish(AddOnID);
  return IsOk;
}
/* DOCEND */
 
/**
 */
/* DOCSTART:gr/polyint_CurveInfoStringCallback.txt */
Boolean_t STDCALL CurveInfoStringCallback(FieldData_pa RawIndV,
                                          FieldData_pa RawDepV,
                                          CoordScale_e IndVCoordScale,
                                          CoordScale_e DepVCoordScale,
                                          LgIndex_t    NumRawPts,
                                          EntIndex_t   XYMapNum,
                                          char        *CurveSettings,
                                          char       **CurveInfoString)
{
  Boolean_t IsOk = TRUE;

  *CurveInfoString = TecUtilStringAlloc(1000, "CurveInfoString");
  strcpy(*CurveInfoString, "Information about the curve goes here.\n");
  strcat(*CurveInfoString, "Such as curve coefficients.");
  return IsOk; 
}
/* DOCEND */
