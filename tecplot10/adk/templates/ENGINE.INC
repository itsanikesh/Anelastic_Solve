$$IF(AW_CURVEFIT)
$$IF(AW_USETGB)

      Pointer(XYMapSetPtr,      Garbage1)
      INTEGER*4 Garbage1
      Pointer(XYMapSettingsPtr, Garbage2)
      INTEGER*4 Garbage2
      COMMON/GlobalCurve/ XYMapSetPtr,XYMapSettingsPtr

$$ENDIF
$$ENDIF


