C
      subroutine sinewaveMenuCB()
      INCLUDE 'ADDGLBL.INC'
      include  'GUI.INC'
      include  'GUIDEFS.INC'
      INCLUDE 'FGLUE.INC'
C
C... Special handling here.  Normally you just lock on launch of a modal dialog.
C... Since a timer is involved with this dialog we want to make sure nothing else
C... has tecplot locked prior to launching this dialog.  Once this dialog is launched
C... we can assume that no other process is able to lock tecplot.
C
      if (TecUtilLockIsOn().eq.FALSE) Then
        Call TecUtilLockOn()
        Call BuildDialog1(MAINDIALOGID)
        IGUI = TecGUIDialogLaunch(Dialog1Manager)
C
C... Dont call TecUtilLockOff ... Dialog1 is a Modal dialog.  Unlock when
C... the dialog is dropped.  See guicb.F
C
      Endif
      return
      end



      subroutine InitializeSineWave()
      INCLUDE 'ADDGLBL.INC'
      INCLUDE 'SINEWAVE.INC'
      include 'GUIDEFS.INC'
C
C... Set some defaults for the sinewave addon
C... The LockString must be a unique string.  This is used when
C... making a request to lock the internal tecplot data exclusively
C... for the sinewave addon.
C
      amp         = 0
      xoffset     = 0
      DonePolling = 0
      return
      end




 
      Subroutine InitTecAddOn()
      INTEGER*4  I
      EXTERNAL   sinewaveMenuCB
      INCLUDE 'ADDGLBL.INC'
      INCLUDE  'FGLUE.INC'
      INCLUDE  'GUI.INC'
      INCLUDE  'GUIDEFS.INC'

      Call TecUtilLockOn()
      Call InitializeSineWave()

      Call InitTGB()
C
      CALL TECUTILADDONREGISTERINFO('My Add-on'//char(0),
     &                                    'V1.0-01/01/99'//char(0),
     &                                    'My Company Name'//char(0))
 
C... Edit Menu Name....
      I = TECUTILMENUADDOPTION('Tools'//char(0),
     &                        'sinewave2'//char(0),
     &                        'M'//char(0),
     &                         sinewaveMenuCB)
      Call TecUtilLockOff()
 
      return
      end
