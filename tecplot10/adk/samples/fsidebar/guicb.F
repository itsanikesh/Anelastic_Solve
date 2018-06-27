

      SUBROUTINE Sidebar1Activate_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::Sidebar1Activate_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
C...  <<< This function is called when sidebar "fsidebar" is activated >>>
      RETURN
      END



      SUBROUTINE Sidebar1Deactivate_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::Sidebar1Deactivate_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
C...   <<< This function is called when sidebar "fsidebar" is deactivated >>>
C 
      RETURN
      END



      SUBROUTINE ZoomIn_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::ZoomIn_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
      I = TecUtilViewSetMagnification(2.0d0)
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE BTN14_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::BTN14_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
C     WRITE(*,*) 'Task14  Button Pushed'
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE BTN13_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::BTN13_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
C     WRITE(*,*) 'Task13  Button Pushed'
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE BTN12_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::BTN12_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
C     WRITE(*,*) 'Task12  Button Pushed'
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE BTN11_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::BTN11_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
C     WRITE(*,*) 'Task11  Button Pushed'
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE BTN10_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::BTN10_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
C     WRITE(*,*) 'Task10  Button Pushed'
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE BTN9_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::BTN9_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
C     WRITE(*,*) 'Task09  Button Pushed'
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE BTN8_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::BTN8_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
C     WRITE(*,*) 'Task08  Button Pushed'
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE BTN7_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::BTN7_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
C     WRITE(*,*) 'Task07  Button Pushed'
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE BTN6_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::BTN6_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
C     WRITE(*,*) 'Task06  Button Pushed'
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE BTN5_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::BTN5_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
C     WRITE(*,*) 'Task05  Button Pushed'
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE Scatter_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::Scatter_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
      if (TecUtilDataSetIsAvailable().EQ.TRUE) then
        I = TecUtilFieldLayerSetIsActive('SHOWSCATTER'//char(0),
     &                                   TRUE)
      else
        call TecUtilDialogErrMsg(
     &         'No data: press the "Data" button first.'//char(0))
      endif
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE Contour_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::Contour_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
      if (TecUtilDataSetIsAvailable().EQ.TRUE) then
        I = TecUtilContourSetVariable(3)
        I = TecUtilFieldLayerSetIsActive('SHOWCONTOUR'//char(0),
     &                                   TRUE)
      else
        call TecUtilDialogErrMsg(
     &         'No data: press the "Data" button first.'//char(0))
      endif
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE Alter_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::Alter_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      POINTER(NullPtr, Null)
      NullPtr = 0
      Call TecUtilLockStart(AddOnID)
      if (TecUtilDataSetIsAvailable().EQ.TRUE) then
        I = TecUtilDataAlter('x=(y-0.5)*(z-0.5)'//char(0),
     &                       NullPtr,
     &                       1, 0, 1,
     &                       1, 0, 1,
     &                       1, 0, 1,
     &                       FieldDataType_Float)
        I = TecUtilDataAlter('y=(x-0.5)*(z-0.5)'//char(0),
     &                       NullPtr,
     &                       1, 0, 1,
     &                       1, 0, 1,
     &                       1, 0, 1,
     &                       FieldDataType_Float)
        I = TecUtilDataAlter('z=(x-0.5)*(y-0.5)'//char(0),
     &                       NullPtr,
     &                       1, 0, 1,
     &                       1, 0, 1,
     &                       1, 0, 1,
     &                       FieldDataType_Float)
        I = TecUtilReset3DAxes()
        I = TecUtilViewDataFit()
      else
        call TecUtilDialogErrMsg(
     &         'No data: press the "Data" button first.'//char(0))
      endif
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE Data_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::Data_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
      I = TecUtilNewLayout()
      I = TecUtilCreateRectangularZone(10, 10, 10,
     &                                 0.0d0, 0.0d0, 0.0d0,
     &                                 1.0d0, 1.0d0, 1.0d0,
     &                                 FieldDataType_Float)
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END



      SUBROUTINE ZoomOut_BTN_S1_CB()
      INCLUDE 'ADDGLBL.INC'
      !DEC$ attributes C::ZoomOut_BTN_S1_CB
      INTEGER*4 I,ILEN

      INCLUDE 'FGLUE.INC'
      INCLUDE 'GUIDEFS.INC'
      Call TecUtilLockStart(AddOnID)
      I = TecUtilViewSetMagnification(0.5d0)
      Call TecUtilLockFinish(AddOnID)
      RETURN
      END





      include "guibld.F"
