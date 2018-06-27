C
      subroutine sinewaveMenuCB()
      INCLUDE 'FGLUE.INC'
      INCLUDE  'ADDGLBL.INC'
      include  'GUI.INC'
      include  'GUIDEFS.INC'
      Call TecUtilLockOn()
      Call BuildDialog1(MAINDIALOGID)
      Call TecGUIDialogLaunch(Dialog1Manager)
C
C... Since Dialog1 is a modal dialog, do not unlock until close or
C... cancel button is pressed.  See guicb.F
C
      return
      end



      subroutine InitializeSineWave()
      INCLUDE  'SINEWAVE.INC'
      INCLUDE  'ADDGLBL.INC'
      include  'GUIDEFS.INC'
C
C... Set some defaults for the sinewave addon
C... The LockString must be a unique string.  This is used when
C... making a request to lock the internal tecplot data exclusively
C... for the sinewave addon.
C
      amp                  = 0
      xoffset              = 0
      return
      end




 
      Subroutine InitTecAddOn()
      INTEGER*4  I
      EXTERNAL   sinewaveMenuCB
      INCLUDE  'ADDGLBL.INC'
      INCLUDE  'GUI.INC'
      INCLUDE  'GUIDEFS.INC'
      INCLUDE  'FGLUE.INC'

      Call TecUtilLockOn()

      Call InitializeSineWave()

      Call InitTGB()
C
      CALL TECUTILADDONREGISTERINFO('My Add-on'//char(0),
     &                              'V1.0-01/01/99'//char(0),
     &                              'My Company Name'//char(0))
 
C... Edit Menu Name....
      I = TECUTILMENUADDOPTION('Tools'//char(0),
     &                         'sinewave'//char(0),
     &                         'M'//char(0),
     &                         sinewaveMenuCB)
      Call TecUtilLockOff()
      return
      end
