$$IF(AW_ISMODAL)
      SUBROUTINE Dialog1Init_CB()
$$IF(AW_ISWINDOWS)
      !DEC$ attributes C::Dialog1Init_CB
$$ENDIF
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
C... Only lock here because this is a modal dialog being launched 
      Call TecUtilLockStart(AddOnID)
C... <<< Add init code (if necessary) here>>>
      RETURN
      END


      SUBROUTINE Dialog1OkButton_CB()
$$IF(AW_ISWINDOWS)
      !DEC$ attributes C::Dialog1OkButton_CB
$$ENDIF
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
C
C... Only unlock here because of modal dialog.
C
      Call TecGUIDialogDrop(Dialog1Manager)
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END


      SUBROUTINE Dialog1CancelButton_CB()
$$IF(AW_ISWINDOWS)
      !DEC$ attributes C::Dialog1CancelButton_CB
$$ENDIF
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
C
C... Only unlock here because of modal dialog.
C
      Call TecGUIDialogDrop(Dialog1Manager)
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END


      SUBROUTINE Dialog1HelpButton_CB()
$$IF(AW_ISWINDOWS)
      !DEC$ attributes C::Dialog1HelpButton_CB
$$ENDIF
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      INTEGER*4 I

      Call TecUtilLockStart(AddOnID)
      I = TecUtilDialogMessageBox(
     & 'On-line Help not available for this dialog.'//char(0),
     & MessageBox_Information)
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END
$$ENDIF
$$IF(AW_ISMODELESS)
      SUBROUTINE Dialog1Init_CB()
$$IF(AW_ISWINDOWS)
      !DEC$ attributes C::Dialog1Init_CB
$$ENDIF
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
C... <<< Add init code (if necessary) here>>>
      RETURN
      END


      SUBROUTINE Dialog1CloseButton_CB()
$$IF(AW_ISWINDOWS)
      !DEC$ attributes C::Dialog1CloseButton_CB
$$ENDIF
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'

      Call TecGUIDialogDrop(Dialog1Manager)

      RETURN
      END


      SUBROUTINE Dialog1HelpButton_CB()
$$IF(AW_ISWINDOWS)
      !DEC$ attributes C::Dialog1HelpButton_CB
$$ENDIF
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      INTEGER*4 I

      Call TecUtilLockStart(AddOnID)
      I = TecUtilDialogMessageBox(
     & 'On-line Help not available for this dialog.'//char(0),
     & MessageBox_Information)
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END
$$ENDIF


      SUBROUTINE Sidebar1Activate_CB()
      !DEC$ attributes C::Sidebar1Activate_CB
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
C...  <<< This function is called when sidebar 1 is activated >>>
      RETURN
      END



      SUBROUTINE Sidebar1Deactivate_CB()
      !DEC$ attributes C::Sidebar1Deactivate_CB
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
C...   <<< This function is called when sidebar sidebar 2 is deactivated >>>
      RETURN
      END



      INCLUDE 'guibld.F'

