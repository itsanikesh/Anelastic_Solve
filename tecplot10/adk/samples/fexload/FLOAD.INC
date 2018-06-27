      COMMON /FLoad/ DialogInitialized,
     &               FLoadFNameLen,
     &               FLoadSkip
      INTEGER*4 DialogInitialized
      INTEGER*4 FLoadFNameLen

      COMMON /FLoadC/FLoadFName,
     &               FLoadSkipString
      CHARACTER*256 FLoadFName
      CHARACTER*12  FLoadSkipString
