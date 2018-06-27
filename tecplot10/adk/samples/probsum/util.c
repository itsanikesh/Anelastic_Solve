#include "TECADDON.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "UTIL.h"






void STDCALL XYSumProbeCallback(Boolean_t IsNearestPoint)
{
  TecUtilLockStart(AddOnID);
  if (!IsNearestPoint)
    {
      if (TecUtilFrameGetPlotType() == PlotType_XYLine)
        {
          char SumString[50];
          double XYSum = 0;
          EntIndex_t Map;
          for (Map = 1;  Map <= TecUtilXYMapGetCount(); Map++)
            {
              double CurValue;
              if (TecUtilProbeXYGetDepValue(Map,&CurValue))
                XYSum += CurValue;
            }
          sprintf(SumString,"SUM = %12.2f",XYSum);
          TecGUILabelSetText(SumTotal_LBL_D1,SumString);
        }
      else
        TecGUILabelSetText(SumTotal_LBL_D1," ");
    }
  else
    TecGUILabelSetText(SumTotal_LBL_D1," ");
  TecUtilLockFinish(AddOnID);
}
