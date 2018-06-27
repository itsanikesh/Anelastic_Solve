

      SUBROUTINE Dialog1HelpButton_CB()
      !DEC$ attributes C::Dialog1HelpButton_CB
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      Call TecUtilLockOn()
      I = TecUtilDialogMessageBox(
     & 'On-line Help not available for this dialog.'//char(0),
     & MessageBox_Information)
      Call TecUtilLockOff()
      RETURN
      END



      SUBROUTINE Dialog1CancelButton_CB()
      !DEC$ attributes C::Dialog1CancelButton_CB
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      CALL TecGUIDialogDrop(Dialog1Manager)
C... Modal Dialogs must call TecUtilLockOn prior to coming
C... up and then call TecUtilLockOff when the Ok or Cancel
C... button is pressed.  Only TecUtilLockOff is supplied here.
      Call TecUtilLockOff()
      RETURN
      END


      INTEGER*4 Function CheckZone(SourceZone)
      INTEGER*4 SourceZone
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      POINTER   (NullPntr, Null)
      INTEGER*4 IsOk
      CHARACTER*80 DataSetTitle
      INTEGER*4 ZoneType
      INTEGER*4 NumZones,NumVars
      INTEGER*4 IMax,JMax,KMax

      NullPntr = 0

      IsOk = TRUE
C
C... There must be a dataset...
C
      if (TecUtilDataSetIsAvailable().eq.FALSE) Then
        IsOk = FALSE
      Endif
C
C... The zone must be enabled...
C
      if (IsOk.Eq.TRUE) Then
        IsOk = TecUtilDataSetGetInfo(DataSetTitle,80,
     &                               NumZones,
     &                               NumVars)
        if (IsOk.Eq.TRUE) Then
          If (TecUtilZoneIsEnabled(SourceZone).Eq.FALSE) Then
            IsOk = FALSE
          Endif
        Endif
      Endif
C
C...The zone must be Ordered
C
      if (IsOk.Eq.TRUE) Then
        ZoneType = TecUtilZoneGetType(SourceZone)
        if (ZoneType.NE.ZONETYPE_ORDERED) Then
          IsOk = FALSE
        Endif
      Endif

C
C...The zone must be IJK-Ordered
C
      if (IsOk.Eq.TRUE) Then
        Call TecUtilZoneGetInfo(SourceZone,
     &                          IMax,
     &                          JMax,
     &                          KMax,
     &                          Null,
     &                          Null,
     &                          Null,
     &                          Null,
     &                          Null,
     &                          Null,
     &                          Null,
     &                          Null,
     &                          Null,
     &                          Null)
        if (IMax.Eq.1.Or.JMax.Eq.1.Or.KMax.Eq.1) Then
          IsOk = FALSE
        Endif
      Endif

      CheckZone = IsOk
      Return
      End



      SUBROUTINE Dialog1OkButton_CB()
      !DEC$ attributes C::Dialog1OkButton_CB
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'CELLCTR.INC'

      INTEGER*4 ZoneIsOk
      EXTERNAL  CheckZone
      INTEGER*4 CheckZone
      EXTERNAL  CreateCellCenteredZone
      INTEGER*4 CreateCellCenteredZone
C
C... Modal Dialogs must call TecUtilLockOn prior to coming
C... up and then call TecUtilLockOff when the Ok or Cancel
C... button is pressed.  Only TecUtilLockOff is supplied here.
C
C...If the zone is IJK-Ordered then call CreateCellCenter
C
      ZoneIsOk = TRUE
      if (SelectedZone.eq.-1) Then
        Call TecUtilDialogErrMsg('No Zone IsSelected'//char(0))
        ZoneIsOk = FALSE
      Endif

      if (ZoneIsOk.Eq.TRUE) Then
        ZoneIsOk = CheckZone(SelectedZone)
        if (ZoneIsOk.Eq.FALSE) Then
          Call TecUtilDialogErrMsg(
     &  'Zone is not available or not an IJK-Ordered Zone'//char(0))
        Endif
      Endif

      if (ZoneIsOk.Eq.TRUE) Then
        CALL TecGUIDialogDrop(Dialog1Manager)
        IsOk = CreateCellCenteredZone(SelectedZone)
        if (IsOk.Eq.FALSE) Then
          Call TecUtilDialogErrMsg(
     &    'Cell Center Zone not created.'//char(0))
        Endif
        Call TecUtilLockOff()
      Endif
      RETURN
      END



      SUBROUTINE Dialog1Init_CB()
      !DEC$ attributes C::Dialog1Init_CB
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'CELLCTR.INC'

      INTEGER*4    NumZones
      INTEGER*4    NumVars
      INTEGER*4    Z
      CHARACTER*80 DataSetTitle
      CHARACTER*50 TString
      CHARACTER*32 ZoneName
C
C... Modal Dialogs must call TecUtilLockOn prior to coming
C... up and then call TecUtilLockOff when the Ok or Cancel
C... button is pressed.
      Call TecUtilLockOn()

      SelectedZone = -1

      if (TecUtilDataSetIsAvailable().eq.TRUE) Then

        IsOk = TecUtilDataSetGetInfo(DataSetTitle,80,
     &                               NumZones,
     &                               NumVars)

        Call TecGUIListDeleteAllItems(SLIST_SLST_D1)

        Do 10 Z = 1,NumZones,1
          IsOk = TecUtilZoneGetName(Z,ZoneName,32)
          Write(TString,11) Z,ZoneName
   11     Format(I3,'. ',20A)
          Call TecGUIListAppendItem(SLIST_SLST_D1,TString//char(0))
   10   Continue
      Else
        Call TecUtilDialogErrMsg('No Dataset Available'//char(0))
      Endif

      RETURN
      END



      SUBROUTINE SLIST_SLST_D1_CB(I)
      !DEC$ attributes C::SLIST_SLST_D1_CB
      !DEC$ attributes REFERENCE::I
      INTEGER I

      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'CELLCTR.INC'
      Call TecUtilLockOn()
      SelectedZone = I
      Call TecUtilLockOff()
      RETURN
      END

      include "guibld.F"
