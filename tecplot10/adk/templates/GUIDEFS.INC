C 
C 
C  NOTE: This file automatically generated by the Tecplot GUI Builder.
C        Do not edit this file!
C 
C  
      INTEGER*4 TECGUITECPLOTSIDEBAR,BADDIALOGID,MAINDIALOGID
      Parameter(TECGUITECPLOTSIDEBAR=-3,BADDIALOGID=-2,MAINDIALOGID=-1)
$$IF(AW_ISMODAL)
      INTEGER*4 Dialog1Manager
      COMMON /Dialog1Manager_C/ Dialog1Manager
$$ENDIF
$$IF(AW_ISMODELESS)
      INTEGER*4 Dialog1Manager
      COMMON /Dialog1Manager_C/ Dialog1Manager
$$ENDIF
$$IF(AW_HASSIDEBAR)
      INTEGER*4 Sidebar1Manager
      COMMON /Sidebar1Manager_C/ Sidebar1Manager
$$ENDIF
