      subroutine calcxy(x,y,npts,amp,offset)
      include 'FGLUE.INC'
      dimension x(npts)
      dimension y(npts)

      POINTER (VSetPtr, Vset)

      pi = acos(-1.0)
c
c...this is recognized to be inefficient as x(i) really
c...only needs to be calculated once for a given offset...
c
      do i = 1, npts
        x(i) = 2.0 * pi * (i-1)/(npts-1)
        y(i) = amp * sin(x(i)-offset)
      enddo
C
C... Make sure and tell tecplot which variables have been changed.
C
      call TecUtilSetAlloc(TRUE, VSetPtr)
      if (VSetPtr .ne. 0) Then
        IsOk = TecUtilSetAddMember(VSetPtr, 1, TRUE)
        IsOk = TecUtilSetAddMember(VSetPtr, 2, TRUE)
        if (IsOk .eq. TRUE) Then
          call TecUtilStateChanged(STATECHANGE_VARSALTERED, VSetPtr)
        Endif
        call TecUtilSetDealloc(VSetPtr)
      Endif

      return
      end






      subroutine drawwave()
      include 'SINEWAVE.INC'
      include 'FGLUE.INC'
      include 'ADDGLBL.INC'
      include 'GUIDEFS.INC'
      POINTER   (NullPntr, Null)
      POINTER   (XPTR, XARRAY)
      REAL*4    XARRAY(1)
      POINTER   (YPTR, YARRAY)
      REAL*4    YARRAY(1)
      integer*4 imax,jmax,kmax
      integer*4 XDATATYPE
      integer*4 YDATATYPE
c
c.... Creating a Null pointer
c
      NullPntr = 0
c
c.... Make sure the data is still around (note assume tecplot is locked)
c
      if (TecUtilDataSetIsAvailable().eq.TRUE) Then
        call TecUtilZoneGetInfo(1,imax,jmax,kmax,
     &                          Null, Null, Null, Null,
     &                          Null, Null, Null,
     &                          Null, Null, Null)
C
C... A bunch of checking could be done here to make sure the
C... current frame has the correct dataset but we will skip 
C... that stuff for this example....
C
C
C... Get the address of the X and Y variables in zone 1 of the dataset.
C
        Call TecUtilDataValueGetRawPtr(1,1,XPTR,XDATATYPE)
        Call TecUtilDataValueGetRawPtr(1,2,YPTR,YDATATYPE)
C
C... Calculate a new wave and draw it.  
C
        Call calcxy(xarray,yarray,imax,amp,xoffset)
        I = TecUtilMacroExecuteCommand('$!DoubleBuffer On'//char(0))
        I = TecUtilRedraw(TRUE)
        I = TecUtilMacroExecuteCommand('$!DoubleBuffer Swap'//char(0))
        I = TecUtilMacroExecuteCommand('$!DoubleBuffer Off'//char(0))
      Endif
      return
      end



      subroutine ResetDataSet()
      include 'SINEWAVE.INC'
      include 'FGLUE.INC'
      include 'ADDGLBL.INC'
      include 'GUIDEFS.INC'
      integer*4 IVarDataType(2)
      pointer (VarListPtr, VarList)
C
C.... clear out tecplot and rebuild the default dataset.
C.... An array of integers must be set up to identify the variable types...
C
      IVarDataType(1) = FIELDDATATYPE_FLOAT
      IVarDataType(2) = FIELDDATATYPE_FLOAT
C
C... Clear out tecplot.  TecUtilNewLayout is currently not available to
C... FORTRAN so you have to issue a macro command....
C
      I = TecUtilMacroExecuteCommand('$!NewLayout'//char(0))
C
C... Create the dataset....
C
      call TecUtilStringListAlloc(VarListPtr)
      I = TecUtilStringListAppendString(VarListPtr, "V1")
      I = TecUtilStringListAppendString(VarListPtr, "V2")
      I = TecUtilDataSetCreate('Some title'//char(0),VarListPtr,TRUE)
      call TecUtilStringListDealloc(VarListPtr)
C
C... Stuff in the first zone.  It is dimensioned nptsx1x1 and is and ORDERED zone
C
      I = TecUtilDataSetAddZone('Sinewave Zone'//char(0),
     &                          npts,
     &                          1,
     &                          1,
     &                          ZONETYPE_ORDERED,
     &                          IVarDataType)
C
C...set the frame to be xy, make the view reasonable, and draw the first image
C...loading a stylesheet would work just as well here....
C
      I = TecUtilFrameSetMode(FRAME_XY)
      I = TecUtilMacroExecuteCommand(
     &      '$!XYAXIS XDETAIL 1 { RANGEMIN = 0 RANGEMAX = 7 }'
     &      //char(0))
      I = TecUtilMacroExecuteCommand(
     &      '$!XYAXIS YDETAIL 1 { RANGEMIN = -10 RANGEMAX = 10 }'
     &      //char(0))
C
C... turn the annoying status line off ...
C
      I = TecUtilMacroExecuteCommand(
     &      '$!INTERFACE SHOWSTATUSLINE = NO'//char(0))
C
C... draw an initial image
C
        call drawwave()
      return
      end
