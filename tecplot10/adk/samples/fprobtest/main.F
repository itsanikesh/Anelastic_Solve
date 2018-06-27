C
C.... This function is called if the "Tools/Probe Test" option is
C     selected from the main menu.

      SUBROUTINE MenuCallback()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'FGLUE.INC'
      External  ProbeTestCallback

      Call TecUtilLockStart(AddOnID)

C ... Install the probe callback if a dataset is available and we
C ... are not in a sketch frame.

      if (TecUtilDataSetIsAvailable().Eq.TRUE  .And.
     &    TecUtilFrameGetPlotType().NE.PlotType_Sketch) Then
        IErr = TecUtilProbeInstallCallback(ProbeTestCallback,
     &        'Click to run the probe test.'//char(0))
      else
        Call TecUtilDialogErrMsg(
     &     'To execute this add-on Tecplot must have'//char(10)//
     &     'a data set and the frame must be in XY,'//char(10)//
     &     '2D, or 3D.')
      endif

      Call TecUtilLockFinish(AddOnID)
      return
      end
 


      Subroutine InitTecAddOn()
      INTEGER*4  I
      EXTERNAL   MenuCallback
      INCLUDE  'ADDGLBL.INC'
      INCLUDE  'FGLUE.INC'


C   
C    The function TecUtilAddOnRegister() is the
C    only function that is REQUIRED to be called from
C    the initialization function.
C   
C    The information you give Tecplot by calling
C    this function will show up in the Help/About Addons
C    dialog box.
C    
      Call TecUtilLockOn()
C
      Call TECUTILADDONREGISTER(
     &         100,
     &         'FORTRAN Probe Test'//char(0),
     &         'V1.0-01/06/2004'//char(0),
     &         'Tecplot Inc.'//char(0),
     &         AddOnID)
 
C... Edit Menu Name....
      I = TECUTILMENUADDOPTION(
     &         'Tools'//char(0),
     &         'Probe Test'//char(0),
     &         'P'//char(0),
     &         MenuCallback)

      Call TecUtilLockOff()
      return
      end
