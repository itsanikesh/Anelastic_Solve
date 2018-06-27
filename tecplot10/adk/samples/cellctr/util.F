      INTEGER*4 FUNCTION CreateCellCenteredZone(SrcZone)
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'

      INTEGER*4 SrcZone
      INTEGER*4 SrcIMax
      INTEGER*4 SrcJMax
      INTEGER*4 SrcKMax
      INTEGER*4 NumZones
      INTEGER*4 NumVars
      INTEGER*4 IsOk
      POINTER   (SrcD,  SrcDV)
      POINTER   (DestD, DestDV)
      POINTER   (NullPntr, Null)
      REAL*8    TMin,TMax
      REAL*8    NewV
      INTEGER*4 FieldDataType
      INTEGER*4 SBase
      INTEGER*4 DBase
      INTEGER*4 NumIJPts
      CHARACTER*80 DataSetTitle

      NullPntr = 0

      Call TecUtilZoneGetInfo(SrcZone,
     &                        SrcIMax,
     &                        SrcJMax,
     &                        SrcKMax,
     &                        Null,
     &                        Null,
     &                        Null,
     &                        Null,
     &                        Null,
     &                        Null,
     &                        Null,
     &                        Null,
     &                        Null,
     &                        Null)

      TMin = 0.0
      TMax = 1.0
      IsOk = TecUtilCreateRectangularZone(SrcIMax-1,
     &                                    SrcJMax-1,
     &                                    SrcKMax-1,
     &                                    TMin,
     &                                    TMin,
     &                                    TMin,
     &                                    TMax,
     &                                    TMax,
     &                                    TMax,
     &                                    FieldDataType_Invalid)


      if (IsOk.Eq.TRUE) Then 
        IsOk = TecUtilDataSetGetInfo(DataSetTitle,
     &                               80,
     &                               NumZones,
     &                               NumVars)

        NumIJPts = SrcJMax*SrcIMax
        DestIMax = SrcIMax-1
        DestJMax = SrcJMax-1

        Do 10 N = 1,NumVars,1
          Call TecUtilDataValueGetRef(SrcZone,N,SrcD)
          Call TecUtilDataValueGetRef(NumZones,N,DestD)
          Do 10 K = 1,SrcKMax-1,1
          Do 10 J = 1,SrcJMax-1,1
          Do 10 I = 1,SrcIMax-1,1
            SBase = (K-1)*SrcJMax*SrcIMax + (J-1)*SrcIMax + I
            DBase = (K-1)*DestJMax*DestIMax + (J-1)*DestIMax + I
            IMaxTJMax = SrcIMax*SrcJMax
            NewV = 
     &      (TecUtilDataValueGetByRef(SrcD,SBase) +
     &       TecUtilDataValueGetByRef(SrcD,SBase+1) +
     &       TecUtilDataValueGetByRef(SrcD,SBase+1+SrcIMax) +
     &       TecUtilDataValueGetByRef(SrcD,SBase+SrcIMax) +
     &       TecUtilDataValueGetByRef(SrcD,IMaxTJMax+SBase) +
     &       TecUtilDataValueGetByRef(SrcD,IMaxTJMax+SBase+1) +
     &       TecUtilDataValueGetByRef(SrcD,IMaxTJMax+SBase+1+SrcIMax) +
     &       TecUtilDataValueGetByRef(SrcD,IMaxTJMax+SBase+SrcIMax))/8.0
            Call TecUtilDataValueSetByRef(DestD,DBase,NewV)
   10   Continue
      Endif
C
      CreateCellCenteredZone = IsOk
      Return
      End
