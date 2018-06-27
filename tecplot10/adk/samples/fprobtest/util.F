      Subroutine ProbeTestCallback(IsNearestPoint)
      INTEGER*4 IsNearestPoint

      INTEGER*4     PlotType
      INTEGER*4     M
      INTEGER*4     V
      INTEGER*4     NumVars
      INTEGER*4     NumZones
      INTEGER*4     IErr
      CHARACTER*20  VName
      CHARACTER*256 DataSetTitle
      REAL*8        DepValue

      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'

      Call TecUtilLockStart(AddOnID)

      PlotType = TecUtilFrameGetPlotType()

      if (PlotType.Eq.PlotType_XYLine .Or. 
     &    PlotType.Eq.PlotType_PolarLine) then
C
C ... LINE PLOT.
C
        write(*,*) 'LinePlot Probe, Independent value is:',
     &             TecUtilProbeLinePlotGetIndValue()
        if (IsNearestPoint.Eq.TRUE) Then
            if (TecUtilProbeLinePlotGetDepValue(1,DepValue).eq.TRUE) 
     &      Then
              write(*,*) 'Nearest Point:  Dep Value    = ',DepValue
              write(*,*) '                Source LineMap = ',
C
C                        ... NOTE: TecUtilProbeLinePlotGetSourceMa 
C                                  (truncated to 31 characters).
C
     &                   TecUtilProbeLinePlotGetSourceMa()
              write(*,*) '                Point Index  = ',
     &                   TecUtilProbeGetPointIndex()
            Endif
        else
          Do 10 M = 1,TecUtilLineMapGetCount(),1
            if (TecUtilProbeLinePlotGetDepValue(M,DepValue).Eq.TRUE) 
     &      Then
              Write(*,*) 'LineMap = ',M,' Dep Value = ',DepValue
            Else
              Write(*,*) 'LineMap = ',M,' Dep Value = Unknown'
            Endif
   10     Continue
        Endif
      else
C
C ... FIELD PLOT
C
        IErr = TecUtilDataSetGetInfo(DataSetTitle,
     &                               255,
     &                               NumZones,
     &                               NumVars)

        If (IsNearestPoint.Eq.TRUE) Then
          Write(*,*) 'Nearest point probe:'
        else
          Write(*,*) 'Interpolated probe:'
        endif
  
        Do 20 V = 1,NumVars,1
          DepValue = TecUtilProbeFieldGetValue(V)
          if (TecUtilVarGetName(V,VName,20)) then
            write(*,*) VName,' = ',DepValue
          else
            write(*,*) '<Unknown> = ',DepValue
          endif
   20   Continue
      Endif

      Call TecUtilLockFinish(AddOnId)
      return
      end
