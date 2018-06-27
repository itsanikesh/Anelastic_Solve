

      SUBROUTINE Dialog1HelpButton_CB()
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
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
C
C... Since dialog1 is modal it is already locked.  Just unlock after dismissing
C
      Call TecGUIDialogDrop(Dialog1Manager)
      Call TecUtilLockOff()
      RETURN
      END



      SUBROUTINE Dialog1OkButton_CB()
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
C
C... Since dialog1 is modal it is already locked.  Just unlock after dismissing
C
      Call TecGUIDialogDrop(Dialog1Manager)
      Call TecUtilLockOff()
      RETURN
      END



      SUBROUTINE Dialog1Init_CB()
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
C
C... Don't really need to lock and unlock since this is a callback for a
C... modal dialog but do so just for good measure.
C
      Call TecUtilLockOn()
      Call TecGUIScaleSetLimits(PhaseScale_SC_D1,0,50,1)
      Call TecGUIScaleSetLimits(AmpScale_SC_D1,0,100,1)
      Call TecUtilLockOff()
      RETURN
      END


      SUBROUTINE PhaseScale_SC_D1_CB(I)
      INTEGER I
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'SINEWAVE.INC'
      INCLUDE 'FGLUE.INC'
C
C... Don't really need to lock and unlock since this is a callback for a
C... modal dialog but do so just for good measure.
C
      Call TecUtilLockOn()
C.... This is called when the user let's go of the phase scale control
      xoffset = (I*1.0)/10.0
      call drawwave()
      Call TecUtilLockOff()
      RETURN
      END

      SUBROUTINE PhaseScale_SCD_D1_CB(I)
      INTEGER I
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'SINEWAVE.INC'
C
C... Don't really need to lock and unlock since this is a callback for a
C... modal dialog but do so just for good measure.
C
      Call TecUtilLockOn()
      xoffset = (I*1.0)/10.0
      call drawwave()
      Call TecUtilLockOff()
      RETURN
      END


      SUBROUTINE AmpScale_SC_D1_CB(I)
      INTEGER I
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'SINEWAVE.INC'
      INCLUDE 'FGLUE.INC'
      Call TecUtilLockOn()
C.... This is called when the user let's go of the amplitude scale control
C.... or when a left arrow or right arrow key is pressed.
      amp = (I*1.0)/10.0
      call drawwave()
      Call TecUtilLockOff()
      RETURN
      END



      SUBROUTINE AmpScale_SCD_D1_CB(I)
      INTEGER I
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'SINEWAVE.INC'
      INCLUDE 'FGLUE.INC'
C
C... Don't really need to lock and unlock since this is a callback for a
C... modal dialog but do so just for good measure.
C
      Call TecUtilLockOn()
      amp = (I*1.0)/10.0
      call drawwave()
      Call TecUtilLockOff()
      RETURN
      END


      SUBROUTINE Reset_BTN_D1_CB()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'FGLUE.INC'
C
C... Don't really need to lock and unlock since this is a callback for a
C... modal dialog but do so just for good measure.
C
      Call TecUtilLockOn()
      call ResetDataSet()
      Call TecUtilLockOff()
      RETURN
      END







#include "guibld.F"
