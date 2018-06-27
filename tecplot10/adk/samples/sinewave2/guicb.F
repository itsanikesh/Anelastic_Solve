      subroutine shutdown()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUI.INC'
      IGUI = TecGUIDialogDrop(Dialog1Manager)
      DonePolling = 1
      I = TecUtilMacroExecuteCommand('$!DoubleBuffer Off'//char(0))
      call TecUtilSidebarAutoSensitivity(1)
      return 
      end




      SUBROUTINE Dialog1CancelButton_CB()
      INCLUDE 'FGLUE.INC'
C
C... Don't call TecUtilLockOn here because Dialog1 is a modal dialog and 
C... was locked on launch.
C
      call shutdown()
      Call TecUtilLockOff()
      RETURN
      END


      SUBROUTINE Dialog1HelpButton_CB()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'FGLUE.INC'
      Call TecUtilLockOn()
      I = TecUtilDialogMessageBox(
     & 'On-line Help not available for this dialog.'//char(0),
     & MessageBox_Information)
      Call TecUtilLockOff()
      RETURN
      END


      SUBROUTINE Dialog1OkButton_CB()
      INCLUDE 'FGLUE.INC'
C
C... Don't call TecUtilLockOn here because Dialog1 is a modal dialog and 
C... was locked on launch.
C
      call shutdown()
      Call TecUtilLockOff()
      RETURN
      END



      SUBROUTINE Dialog1Init_CB()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'FGLUE.INC'
      Call TecUtilLockOn()
      Call TecGUIScaleSetLimits(AmpScale_SC_D1,-100,100,1)
      Call TecUtilLockOff()
      RETURN
      END


      SUBROUTINE AmpScale_SC_D1_CB(I)
      INTEGER I
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'FGLUE.INC'
      Call TecUtilLockOn()
C.... This is called when the user let's go of the amplitude scale control
C.... currently do nothing.
      Call TecUtilLockOff()
      RETURN
      END



      SUBROUTINE AmpScale_SCD_D1_CB(I)
      INTEGER I
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'SINEWAVE.INC'
      amp = (I*1.0)/10.0
      RETURN
      END


      SUBROUTINE Reset_BTN_D1_CB()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'FGLUE.INC'
      Call TecUtilLockOn()
      DonePolling = 1
      call ResetDataSet()
      Call TecUtilLockOff()
      RETURN
      END







#include "guibld.F"
