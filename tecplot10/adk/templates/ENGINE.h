#ifndef  ENGINE_H_
#define ENGINE_H_ /* Only include once */

$$IF(AW_LOADER)
Boolean_t STDCALL LoaderCallback(StringList_pa Instructions); /* in 'engine.c' */
void STDCALL LoaderSelectedCallback(void);
$$IF(AW_INCLUDEOVERRIDE)
Boolean_t STDCALL InstructionOverrideCallback(StringList_pa Instructions);
$$ENDIF
$$ENDIF

$$IF(AW_CONVERTER)

$$IF(AW_CONVERTERPLUS)
$$IF(AW_USETGB)
typedef struct
  {
    const char   *PreviousInstructions;
    const char   *PreviousRawData;
    Set_pa  PreviousZones;
  } GlobalConvPlus_s;
$$ENDIF

Boolean_t Action_Alter(char *Multiplier);
Boolean_t STDCALL ProcessConvertMacroCommand(char  *CommandString,
                                             char **ErrMsg); /* in 'engine.c' */
void STDCALL PostConvertCallback(const char   *PreviousInstructions,
                                 const char   *PreviousRawData,
                                 const Set_pa PreviousZones); /* in 'engine.c' */
$$ENDIF
Boolean_t STDCALL ConverterCallback( char *DataFName,
                                     char *TempBinFName,
                                     char **MessageString); /* in 'engine.c' */

$$ENDIF

$$IF(AW_CURVEFIT)

$$IF(AW_USETGB)
typedef struct
  {
    StringList_pa XYMapSettings;
    Set_pa        XYMapSet;    
  } GlobalCurve_s;

void STDCALL CurveSettingsCallback(Set_pa        XYMapSet,
                                   StringList_pa XYMapSettings);
void STDCALL AbbreviatedSettingsStringCallback(EntIndex_t  XYMapNum,
                                               char       *CurveSettings,
                                               char      **AbbreviatedSettings);
$$ENDIF
Boolean_t STDCALL XYDataPointsCallback(FieldData_pa RawIndV,
                                       FieldData_pa RawDepV,
                                       CoordScale_e IndVCoordScale,
                                       CoordScale_e DepVCoordScale,
                                       LgIndex_t    NumRawPts,
                                       LgIndex_t    NumCurvePts,
                                       EntIndex_t   XYMapNum,
                                       char        *CurveSettings,
                                       double      *IndCurveValues,
                                       double      *DepCurveValues);
Boolean_t STDCALL CurveInfoStringCallback(FieldData_pa  RawIndV,
                                          FieldData_pa  RawDepV,
                                          CoordScale_e  IndVCoordScale,
                                          CoordScale_e  DepVCoordScale,
                                          LgIndex_t     NumRawPts,
                                          EntIndex_t    XYMapNum,
                                          char         *CurveSettings,
                                          char        **CurveInfoString);
Boolean_t STDCALL ProbeValueCallback(FieldData_pa RawIndV,
                                     FieldData_pa RawDepV,
                                     CoordScale_e IndVCoordScale,
                                     CoordScale_e DepVCoordScale,
                                     LgIndex_t    NumRawPts,
                                     LgIndex_t    NumCurvePts,
                                     EntIndex_t   XYMapNum,
                                     char        *CurveSettings,
                                     double       ProbeIndValue,
                                     double      *ProbeDepValue);
$$ENDIF

#endif /* ENGINE_H_ */
