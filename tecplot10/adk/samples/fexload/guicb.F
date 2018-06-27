

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



      SUBROUTINE Dialog1OkButton_CB()
      !DEC$ attributes C::Dialog1OkButton_CB
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'FLOAD.INC'
      INTEGER*4 IsOk
      pointer (NullPtr,DummyNullPtrValue)
      pointer (InstructionPtr,Instruction)

      NullPtr = 0

      CALL TecGUIDialogDrop(Dialog1Manager)

      Call TecUtilStringListAlloc(InstructionPtr)
      write (*,*) 'NullPtr        = ',NullPtr
      write (*,*) 'InstructionPtr = ',InstructionPtr

      if (InstructionPtr.NE.NullPtr) Then
        IsOk = TecUtilStringListAppendString(InstructionPtr,
     &                                       "-FILENAME"//char(0))
        IsOk = TecUtilStringListAppendString(InstructionPtr,
     &                  FLoadFName(1:FLoadFNameLen)//char(0))
        IsOk = TecUtilStringListAppendString(InstructionPtr,
     &                                       "-SKIP"//char(0))
        IsOk = TecUtilStringListAppendString(InstructionPtr,
     &                                       FLoadSkipString//char(0))
        IsOk = LoaderCallback(InstructionPtr)
        Call TecUtilStringListDealloc(InstructionPtr)
      Endif

C... Modal Dialogs must call TecUtilLockOn prior to coming
C... up and then call TecUtilLockOff when the Ok or Cancel
C... button is pressed.  Only TecUtilLockOff is supplied here.
      Call TecUtilLockOff()

      if (IsOk.NE.TRUE) Then
C
C...Problem with the load.  Might as well relaunch the loader dialog
C...for another try.  Note that the init callback on the launch will
C...relock tecplot.
C
        Call LoaderSelectedCallback()
      Endif

      RETURN
      END


      subroutine updateloaderdialog()
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'FLOAD.INC'
      Call TecGUITextFieldSetString(FName_TF_D1,
     &                          FLoadFName(1:FLoadFNameLen)//char(0))
      Call TecGUITextFieldSetString(Skip_TF_D1,
     &                             FLoadSkipString)
      return
      end



      SUBROUTINE Dialog1Init_CB()
      !DEC$ attributes C::Dialog1Init_CB
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
C... Modal Dialogs must call TecUtilLockOn prior to coming
C... up and then call TecUtilLockOff when the Ok or Cancel
C... button is pressed.
      Call TecUtilLockOn()
      Call updateloaderdialog()
C... <<< Add init code (if necessary) here>>>
      RETURN
      END



      INTEGER FUNCTION FName_TF_D1_CB(S)
      !DEC$ attributes C::FName_TF_D1_CB
      LOGICAL*1 S(1)
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'FLOAD.INC'
      FName_TF_D1_CB = 1
      Call TecUtilLockOn()
      CALL FExtGetCharFromCString(S,FLoadFName,FLoadFNameLen)
      Call TecUtilLockOff()
      RETURN
      END



      INTEGER FUNCTION Skip_TF_D1_CB(S)
      !DEC$ attributes C::Skip_TF_D1_CB
      LOGICAL*1 S(1)
      CHARACTER*256 S2
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'FLOAD.INC'
      Skip_TF_D1_CB = 1
      Call TecUtilLockOn()
      CALL FExtGetCharFromCString(S,FLoadSkipString,ILen)
      Call TecUtilLockOff()
      RETURN
      END



      SUBROUTINE Browse_BTN_D1_CB()
      !DEC$ attributes C::Browse_BTN_D1_CB
      INCLUDE 'FGLUE.INC'
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'GUIDEFS.INC'
      INCLUDE 'GUI.INC'
      INCLUDE 'FLOAD.INC'
      Integer*4 IsOk
      Call TecUtilLockOn()

      IsOk = tecutildialoggetfilename(SelectFileOption_ReadSingleFile,
     &       FLoadFName,
     &       FLoadFNameLen,
     &       'example fortran loader file'//char(0),
     &       FLoadFName(1:FLoadFNameLen)//char(0),
     &       '*.dat'//char(0))

      if (IsOk.Eq.TRUE) Then
        Call UpdateLoaderDialog()
      Endif

      Call TecUtilLockOff()
      RETURN
      END





      include "guibld.F"
