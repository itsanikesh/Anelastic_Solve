#include "TECADDON.h"
#include <unistd.h>
#include "UTIL.h"




void ProbeTestStateChangeCallback(StateChange_e StateChange,
                                  ArbParam_t    CallData)
{
  TecUtilLockOn();
   printf("State Changed, %d\n",StateChange);
  TecUtilLockOff();
}

void ProbeTestCallback(Boolean_t IsNearestPoint)
{
  PlotType_e PlotType;

  TecUtilLockOn();

  PlotType = TecUtilFrameGetPlotType();

  if (PlotType == PlotType_XYLine)
    {
      printf("XY Probe, Independent value is: %G\n",TecUtilProbeXYGetIndValue());
      if (IsNearestPoint)
        {
          double DepValue;
          TecUtilProbeXYGetDepValue(1,&DepValue); 
          printf("Nearest Point:  Dependent Value = %G\n",DepValue);
          printf("                Source XYMap    = %d\n",TecUtilProbeXYGetSourceMap());
          printf("                Point Index     = %d\n",TecUtilProbeGetPointIndex());
        }
      else
        {
          EntIndex_t M;
          for (M = 1; M <= TecUtilXYMapGetCount(); M++)
            {
              double DepValue;
              printf("XYMap = %02d, Dependent Value = ",M);
              if (TecUtilProbeXYGetDepValue(M,&DepValue))
                printf("%G\n",DepValue);
              else
                printf("Unknown\n");
            }
        }
    }
  else
    {
      EntIndex_t V;
      EntIndex_t NumVars;

      TecUtilDataSetGetInfo((char **)NULL,
                            (EntIndex_t *)NULL,
                            &NumVars);

      if (IsNearestPoint)
        printf("Nearest point probe:\n");
      else
        printf("Interpolated  probe:\n");

      for (V = 1; V <= NumVars; V++)
        {
          char *VName;
          if (TecUtilVarGetName(V,&VName))
            {
              printf("%20s : ",VName);
              TecUtilStringDealloc(&VName);
            }
          else
            printf("<Unknown>            : ");
          printf("%G\n",TecUtilProbeFieldGetValue(V));
        }
    }
  TecUtilLockOff();
}
