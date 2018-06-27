C   ***************************************************
C   *                                                 *
C   *  NOTE!  This file is automatically built by     *
C   *         the Tecplot GUI Builder.  It is highly  *
C   *         recommended that you never edit this    *
C   *         file directly!                          *
C   *                                                 *
C   ***************************************************


      SUBROUTINE BuildDialog1(ParentDialog)
      INTEGER    ParentDialog
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'GUICB.INC'
      INCLUDE 'GUIDEFS.INC'

      POINTER (NullPntr, Null)

      NullPntr = 0

      if (Dialog1Manager.ne.BADDIALOGID) Return

      Dialog1Manager = TecGUIDialogCreateModal(ParentDialog,
     &                                         4383,
     &                                         1458,
     &'Sinewave Example'//char(0),
     &                                         Dialog1Init_CB,
     &                                         Dialog1OkButton_CB,
     &                                         Null,
     &                                         Dialog1CancelButton_CB,
     &                                         Dialog1HelpButton_CB)
      PhaseScale_SC_D1 = TecGUIScaleAdd(Dialog1Manager,
     &                                        224,
     &                                        427,
     &                                             3947,
     &                                             263,
     &               0,
     &               100,
     &               0,
     &               PhaseScale_SC_D1_CB,
     &               PhaseScale_SCD_D1_CB)
      PhaseShift_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
     &                                         1600,
     &                                         728,
     &               'Phase Shift'//char(0))
      Reset_BTN_D1 = TecGUIButtonAdd(Dialog1Manager,
     &                                     1734,
     &                                     138,
     &                                          807,
     &                                          150,
     &               'Reset'//char(0),
     &               Reset_BTN_D1_CB)
      AmpScale_SC_D1 = TecGUIScaleAdd(Dialog1Manager,
     &                                      239,
     &                                      929,
     &                                           3947,
     &                                           263,
     &               0,
     &               100,
     &               0,
     &               AmpScale_SC_D1_CB,
     &               AmpScale_SCD_D1_CB)
      AmplitudeShi_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
     &                                           1420,
     &                                           1218,
     &               'Amplitude Shift'//char(0))
      RETURN
      END

      Subroutine InitTGB()
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      Dialog1Manager = BADDIALOGID
      Return
      End
