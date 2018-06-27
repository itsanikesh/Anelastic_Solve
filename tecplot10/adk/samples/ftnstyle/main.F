C
C
C
C  Sample FORTRAN addon demonstrating how to deal with various
C  advanced programming problems in fortran application.  
C
C  This addon simply runs off the menu callback which in turn calls
C  the following functions:
C
C     Call SetupDataset()
C     Call SetLowLevelStyleUsingOldFunction()
C     Call SetLowLevelStyleUsingNewFunction()
C     Call SendStateChange()
C     Call GetStyleUsingConvenienceFunctions()
C     Call GetStyleUsingLowLevelStyle()
C     Call SetStyleString()
C
C





      Subroutine SetupDataset()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'

      INTEGER*4 IErr

      IErr = TecUtilNewLayout()
 
      IErr = TecUtilCreateRectangularZone(10,10,10,
     &                                    0.0D0,0.0D0,0.0D0,
     &                                    1.0D0,1.0D0,1.0D0,
     &                                    FieldDataType_Float)
      IErr = TecUtilCreateRectangularZone(10,10,10,
     &                                    1.0D0,1.0D0,1.0D0,
     &                                    2.0D0,2.0D0,2.0D0,
     &                                    FieldDataType_Float)

      Return
      End

C
C Set low level style using old method 
C

      Subroutine SetLowLevelStyleUsingOldFunction()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'

      pointer   (SetOrOffsetPtr,DummySetOrOffset)
C      INTEGER*4 DummySetOrOffset
      pointer   (IValuePtr,DummyIValue)
      INTEGER*4 DummyIValue
      pointer   (LocalNullPtr,DummyLocalNull)
      INTEGER*4 DummyLocalNull
      pointer   (ZoneListPtr,DummyZoneList)
      INTEGER*4 DummyZoneList
      INTEGER*4 IShowErr
      INTEGER*4 ISVRC,IErr

      IShowErr     = 0
      LocalNullPtr = 0

C
C ... Use SetLowLevel to set the relop
C
      IValuePtr      = RelOp_GreaterThanOrEqual
      SetOrOffsetPtr = 1
      ISVRC = TecUtilStyleSetLowLevel(LocalNullPtr,
     &                                0.0D0,
     &                                IValuePtr,
     &                                SetOrOffsetPtr,
     &                                AssignOp_Equals,
     &                                'BLANKING'//char(0),
     &                                'VALUE'//char(0),
     &                                'CONSTRAINT'//char(0),
     &                                'RELOP'//char(0),
     &                                char(0),
     &                                char(0),
     &                                FALSE)

C
C .... Set Surfaces to plot to be I-Planes for zone 2.
C
      Call TecUtilSetAlloc(IShowErr,ZoneListPtr)
      IErr = TecUtilSetAddMember(ZoneListPtr,2,IShowErr)
C
C ... Must use a pointer to ship IValue because call stack expects
C ... argument the size of a pointer.
C
      IValuePtr = SurfacesToPlot_IPlanes
      ISVRC = TecUtilStyleSetLowLevel(LocalNullPtr,
     &                                0.0D0,
     &                                IValuePtr,
     &                                ZoneListPtr,
     &                                AssignOp_Equals,
     &                                'FIELD'//char(0),
     &                                'SURFACES'//char(0),
     &                                'SURFACESTOPLOT'//char(0),
     &                                char(0),
     &                                char(0),
     &                                char(0),
     &                                FALSE)
      Call TecUtilSetDealloc(ZoneListPtr)

      return
      End





C
C ... Set low level style using arglist method.
C
      Subroutine SetLowLevelStyleUsingNewFunction()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'

      pointer   (ArgListPtr,DummyArgList)
      INTEGER*4 DummyArgList
      pointer   (SetOrOffsetPtr,DummySetOrOffset)
      INTEGER*4 DummySetOrOffset
      pointer   (IValuePtr,DummyIValue)
      INTEGER*4 DummyIValue
      INTEGER*4 IErr,IRet

C
C ... This will set the value blanking condition
C ... cutoff value to be V3 >= 0.6
C
C ... Using the "X" function we have the following 
C     arglist entries are available:
C 
C  Name              Type
C -------------------------------
C  'P1'              string
C  'P2'              string
C  'P3'              string
C  'P4'              string
C  'P5'              string
C  'P6'              string
C  'OBJECTSET'       Set_pa
C  'OFFSET1'         LgIndex_t
C  'OFFSET2'         LgIndex_t
C  'ASSIGNMODIFIER'  AssignOp_e
C  'DVALUE'          double
C  'IVALUE'          ArbParam_t

C
C ... Set the value blanking variable to 3
C
      Call TecUtilArgListAlloc(ArgListPtr)
      IErr = TecUtilArgListAppendString(ArgListPtr, 
     &                                  'P1'//char(0),
     &                                  'BLANKING'//char(0))

      IErr = TecUtilArgListAppendString(ArgListPtr,
     &                                  'P2'//char(0),
     &                                  'VALUE'//char(0))

      IErr = TecUtilArgListAppendString(ArgListPtr,
     &                                  'P3'//char(0),
     &                                  'CONSTRAINT'//char(0))

      IErr = TecUtilArgListAppendString(ArgListPtr,
     &                                  'P4'//char(0),
     &                                  'VARA'//char(0))

      IErr = TecUtilArgListAppendInt(   ArgListPtr,
     &                                  'OFFSET1'//char(0),
     &                                  1)

      IValuePtr = 3
      IErr = TecUtilArgListAppendArbParam( ArgListPtr,
     &                                    'IVALUE'//char(0),
     &                                    IValuePtr)
      write(*,*) 'setting constraint vara'
      IRet = TecUtilStyleSetLowLevelX(ArgListPtr)

      Call TecUtilArgListDealloc(ArgListPtr)



C
C ... Set cutoff value.
C
      Call TecUtilArgListAlloc(ArgListPtr)
      IErr = TecUtilArgListAppendString(ArgListPtr, 
     &                                  'P1'//char(0),
     &                                  'BLANKING'//char(0))

      IErr = TecUtilArgListAppendString(ArgListPtr,
     &                                  'P2'//char(0),
     &                                  'VALUE'//char(0))

      IErr = TecUtilArgListAppendString(ArgListPtr,
     &                                  'P3'//char(0),
     &                                  'CONSTRAINT'//char(0))

      IErr = TecUtilArgListAppendString(ArgListPtr,
     &                                  'P4'//char(0),
     &                                  'VALUECUTOFF'//char(0))

      IErr = TecUtilArgListAppendInt(   ArgListPtr,
     &                                  'OFFSET1'//char(0),
     &                                  1)

      IErr = TecUtilArgListAppendDouble( ArgListPtr,
     &                                  'DVALUE'//char(0),
     &                                  0.6D0)
      write(*,*) 'setting constraint value cut-off'
      IRet = TecUtilStyleSetLowLevelX(ArgListPtr)

      Call TecUtilArgListDealloc(ArgListPtr)

      return
      End






C
C ... Send a vars altered state change telling tecplot the
C ... variable AND the zone where the alteration occurred.
C
      Subroutine SendStateChange()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'

      INTEGER*4 DummyArgList
      pointer (ArgListPtr,DummyArgList)
      pointer (ZoneListPtr,DummyZoneList)
      INTEGER*4 DummyZoneList
      pointer (VarListPtr,DummyVarList)
      INTEGER*4 DummyVarList
      INTEGER*4 IShowErr,IErr

      IShowErr = 0

      Call TecUtilArgListAlloc(ArgListPtr)
      Call TecUtilSetAlloc(IShowErr,ZoneListPtr)
      Call TecUtilSetAlloc(IShowErr,VarListPtr)

      IErr = TecUtilSetAddMember(VarListPtr,2,IShowErr)
      IErr = TecUtilSetAddMember(ZoneListPtr,2,IShowErr)

      IErr = TecUtilArgListAppendInt(ArgListPtr,
     &                               'STATECHANGE'//char(0),
     &                               StateChange_VarsAltered)
      IErr = TecUtilArgListAppendSet(ArgListPtr,
     &                               'ZONELIST'//char(0),
     &                               ZoneListPtr)
      IErr = TecUtilArgListAppendSet(ArgListPtr,
     &                               'VARLIST'//char(0),
     &                               VarListPtr)

      Call TecUtilStateChangedX(ArgListPtr)

      Call TecUtilArgListDealloc(ArgListPtr)
      Call TecUtilSetDealloc(ZoneListPtr)
      Call TecUtilSetDealloc(VarListPtr)
      Return
      End







C
C ... Use a convenience function to obtain a style setting.
C
      Subroutine GetStyleUsingConvenienceFunctions()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'

      INTEGER*4 DummyFieldStyle
      pointer (ResultPtr,DummyFieldStyle)
C
C ... Get the Range 
C
      call TecUtilFieldStyleGetArbValue(2,
     &                                  'SURFACES'//char(0),
     &                                  'SURFACESTOPLOT'//char(0),
     &                                  char(0),
     &                                  ResultPtr)
      write(*,*) 'SurfacesToPlot = ',ResultPtr

      if (ResultPtr.Eq.SURFACESTOPLOT_IPLANES) Then

      call TecUtilFieldStyleGetArbValue(2,
     &                                  'SURFACES'//char(0),
     &                                  'IRANGE'//char(0),
     &                                  'MIN'//char(0),
     &                                  ResultPtr)
      write(*,*) 'IRange Min = ',ResultPtr

      call TecUtilFieldStyleGetArbValue(2,
     &                                  'SURFACES'//char(0),
     &                                  'IRANGE'//char(0),
     &                                  'MAX'//char(0),
     &                                  ResultPtr)
      write(*,*) 'IRange Max = ',ResultPtr
      Endif

      Return
      End


C
C ... Obtain an arbituary low level style setting.
C
      Subroutine GetStyleUsingLowLevelStyle()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'

      INTEGER*4 DummyArgList
      pointer   (ArgListPtr,DummyArgList)
      pointer   (IValuePtr,DummyIValue)
      INTEGER*4 DummyIValue
      INTEGER*4 ContourGroup,IErr,IRet

C
C ... Get the numver formatting for contour group 1
C ... GlobalContour Labels {NumFormat {Formatting = [NumberFormat_e]}}
C
      Call TecUtilArgListAlloc(ArgListPtr)
      IErr = TecUtilArgListAppendString(ArgListPtr, 
     &                                  'P1'//char(0),
     &                                  'GLOBALCONTOUR'//char(0))

      IErr = TecUtilArgListAppendString(ArgListPtr,
     &                                  'P2'//char(0),
     &                                  'LABELS'//char(0))

      IErr = TecUtilArgListAppendString(ArgListPtr,
     &                                  'P3'//char(0),
     &                                  'NUMFORMAT'//char(0))

      IErr = TecUtilArgListAppendString(ArgListPtr,
     &                                  'P4'//char(0),
     &                                  'FORMATTING'//char(0))

      ContourGroup = 1
      IErr = TecUtilArgListAppendInt(   ArgListPtr,
     &                                  'OFFSET1'//char(0),
     &                                  ContourGroup)

      IErr = TecUtilArgListAppendArbParamPtr(ArgListPtr,
     &                                       'IVALUE'//char(0),
     &                                       IValuePtr)

      write (*,*) 'Getting style'
      IRet = TecUtilStyleGetLowLevelX(ArgListPtr)

      write (*,*) 'Number format is ',IValuePtr

      Call TecUtilArgListDealloc(ArgListPtr)

      Return
      End



C
C ... Apply a style setting that is a string.
C
      subroutine SetStyleString()
C
      INCLUDE 'ADDGLBL.INC'      
      INCLUDE 'FGLUE.INC'      

      pointer   (ArgListPtr,DummyArgList)
      INTEGER*4 DummyArgList
      INTEGER*4 IRet,IErr

      Call TecUtilArgListAlloc(ArgListPtr)

      IErr = TecUtilArgListAppendString(ArgListPtr, 
     &                                  'P1'//char(0),
     &                                  'FRAMENAME'//char(0))

      IErr = TecUtilArgListAppendString(ArgListPtr,
     &                                  'STRVALUE'//char(0),
     &                                  'Hi Mom'//char(0))

      write(*,*) 'setting frame name'
      IRet = TecUtilStyleSetLowLevelX(ArgListPtr)

      Call TecUtilArgListDealloc(ArgListPtr)


      return
      End


C
C ... Obtain a string style setting.
C
C ... Only string we can currently "get" is an XY line map name.
C ... First switch to xy line plot and then get the mapping name of
C ... The first map.
C
      subroutine GetStyleString()
      INCLUDE 'ADDGLBL.INC'      
      INCLUDE 'FGLUE.INC'      

      pointer   (IValuePtr,IValuePointee)
      INTEGER*1 IValuePointee
      INTEGER*4 DummyIValue
      pointer   (ArgListPtr,DummyArgList)
      INTEGER*4 DummyArgList
      INTEGER*4 IRet,IErr,ILen
      INTEGER*4 MapNum
      CHARACTER*256 XYMapName

      Call TecUtilArgListAlloc(ArgListPtr)

      IErr = TecUtilFrameSetPlotType(PlotType_XYLine)

      IErr = TecUtilArgListAppendString(ArgListPtr, 
     &                                  'P1'//char(0),
     &                                  'LINEMAP'//char(0))

      IErr = TecUtilArgListAppendString(ArgListPtr, 
     &                                  'P2'//char(0),
     &                                  'NAME'//char(0))

      MapNum = 1
      IErr = TecUtilArgListAppendInt(ArgListPtr,
     &                               'OFFSET1'//char(0),
     &                               MapNum)

      IErr = TecUtilArgListAppendArbParamPtr(ArgListPtr,
     &                                       'IVALUE'//char(0),
     &                                       IValuePtr)

      write(*,*) 'getting xymap name'
      IRet = TecUtilStyleGetLowLevelX(ArgListPtr)

C
C ... XY map name is now in IValuePtr,  Need to convert to FORTRAN
C ... string.
C
      CALL FExtGetCharFromCString(IValuePointee,XYMapName,ILen)

      write(*,*) 'XYMap name is ',XYMapName
      write(*,*) 'XYMap name Length is',ILen

      call TecUtilArrayDealloc(IValuePtr)

      Call TecUtilArgListDealloc(ArgListPtr)


      return
      End



      SUBROUTINE MenuCallback()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'

      Call TecUtilLockStart(AddOnID)

      Call SetupDataset()
      Call SetLowLevelStyleUsingOldFunction()
      Call SetLowLevelStyleUsingNewFunction()
      Call SendStateChange()
      Call GetStyleUsingConvenienceFunctions()
      Call GetStyleUsingLowLevelStyle()
      Call SetStyleString()
      Call GetStyleString()
      Call TecUtilLockFinish(AddOnID)
      return
      end






      Subroutine InitTecAddOn()
!DEC$ ATTRIBUTES DLLEXPORT::InitTecAddOn
      INCLUDE  'ADDGLBL.INC'
      INCLUDE  'FGLUE.INC'
      INTEGER*4  I,N
      EXTERNAL   MenuCallback

      Call TecUtilLockOn()
 
      Call TECUTILADDONREGISTER(
     &         100,
     &         'Fortran Style Test'//char(0),
     &         'V1.0-03/04/2004'//char(0),
     &         'Tecplot Inc.'//char(0),
     &         AddOnID)
 
      I = TECUTILMENUADDOPTION(
     &         'Tools'//char(0),
     &         'Fortran Style Test'//char(0),
     &         ''//char(0),
     &         MenuCallback)


      Call TecUtilLockOff()
      return
      end
