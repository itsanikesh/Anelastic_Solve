



 


      Subroutine InitTecAddOn()
      !DEC$ attributes DLLEXPORT::InitTecAddOn
      INTEGER*4  I
      EXTERNAL   LoaderCallback 
      EXTERNAL   LoaderSelectedCallback 
      INCLUDE  'ADDGLBL.INC'
      INCLUDE  'FGLUE.INC'
      INCLUDE  'GUI.INC'
      INCLUDE  'GUIDEFS.INC'
      INCLUDE  'FLOAD.INC'

C   
C    The function TecUtilAddOnRegisterInfo() is the
C    only function that is REQUIRED to be called from
C    the initialization function.
C   
C    The information you give Tecplot by calling
C    this function will show up in the Help/About Addons
C    dialog box.
C    
C   
C    Note that if your add-on requires a specific version of Tecplot,
C    you would check for that here using TecUtilGetTecplotVersion()
C    
 
      Call TecUtilLockOn()
      Call InitTGB()
C
C...  The next four lines initialize some globals (in FLOAD.INC)
C
      DialogInitialized = 0
      FLoadFName        = 'test.dat'
      FLoadFNameLen     = 8
      FLoadSkipString   = '1'
C
      CALL TECUTILADDONREGISTERINFO('Fortran Example Loader'//char(0),
     &                              'V1.0-01/01/2000'//char(0),
     &                              'Amtec Engineering Inc.'//char(0))
 


      I = TecUtilImportAddLoader(LoaderCallback,
     &                           'Fortran Example Loader'//char(0),
     &                           LoaderSelectedCallback,
     &                           0) 

      Call TecUtilLockOff()
      return
      end
