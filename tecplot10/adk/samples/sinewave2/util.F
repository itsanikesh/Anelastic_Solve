      subroutine calcxy(x,y,npts,amp,offset)
      include 'FGLUE.INC'
      dimension x(npts)
      dimension y(npts)

      POINTER (VSetPtr, Vset)

      pi = acos(-1.0)

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
      INCLUDE 'ADDGLBL.INC'
      include 'GUIDEFS.INC'
      include 'FGLUE.INC'
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
c.... First make sure the data is still around
c
      call TecUtilLockOn()

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
        CALL TECUTILDATAVALUEGETRAWPTR(1,1,XPTR,XDATATYPE)
        CALL TECUTILDATAVALUEGETRAWPTR(1,2,YPTR,YDATATYPE)
C
C... Calculate a new wave and draw it.  
C
        Call calcxy(xarray,yarray,imax,amp,xoffset)
        I = TecUtilRedraw(TRUE)
        I = TecUtilMacroExecuteCommand('$!DoubleBuffer Swap'//char(0))
      Endif

      call TecUtilLockOff()
      return
      end



      integer*4 function PollForData(UnusedClientDataPtr)
      include 'SINEWAVE.INC'
      include 'FGLUE.INC'
      pointer (UnusedClientDataPtr,UnusedClientData)
      integer*4 UnusedClientData
      pi = acos(-1.0)
C
C...Simulate acquisition of next phase shift.  Just shift the phase by 0.1
C
      xoffset = xoffset + 0.1
      if (xoffset.gt.2*pi) xoffset = 0.0

C... Don't need to check to see if tecplot is locked here because 
C... the way the modal dialog was launched guarentees that we have 
C... Tecplot locked already
C
      call drawwave()
C
C... Setup the return value to indicate if the timer should continue.
C
      if (DonePolling .eq. 1) then
        PollForData = 0
      else
        PollForData = 1
      endif

      return
      end



      subroutine ResetDataSet()
      include 'SINEWAVE.INC'
      include 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      include 'GUIDEFS.INC'
      integer*4 PollForData
      external  PollForData
      integer*4 IVarDataType(2)
      pointer (VarListPtr, VarList)
      pointer (ClientDataPtr,ClientDataUnused)
      integer*4 ClientDataUnused
C
C.... clear out tecplot and rebuild the default dataset.
C... An array of integers must be set up to identify the variable types...
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
C... Set the phase shift to zero and start up the data acquisition.
C
        xoffset = 0.0
        Call TecUtilSidebarAutoSensitivity(0)
        I = TecUtilMacroExecuteCommand('$!DoubleBuffer On'//char(0))
        DonePolling = 0
C
C ... Client data is really not used.
C
        ClientDataPtr = 0
        I = TecUtilTimerAddCallback(50, ClientDataPtr, PollForData)
      return
      end
