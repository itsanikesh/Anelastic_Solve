#ifndef  ENGINE_H_
#define ENGINE_H_ /* Only include once */


/* DOCSTART:gr/simpavg_GlobalCurve_decl.txt */
typedef struct
  {
    StringList_pa XYMapSettings;
    Set_pa        XYMapSet;    
  } GlobalCurve_s;
/* DOCEND */

char *CreateCurveSettingsString(CurveParams_s CurveParams);

void GetValuesFromCurveSettings(EntIndex_t    XYMapNum,
                                char          *CurveSettings,
                                CurveParams_s *CurveParams);

void InitializeCurveParams(CurveParams_s *CurveParams);

void STDCALL CurveSettingsCallback(Set_pa        XYMapSet,
                                   StringList_pa XYMapSettings);
void STDCALL AbbreviatedSettingsStringCallback(EntIndex_t  XYMapNum,
                                               char       *CurveSettings,
                                               char      **AbbreviatedSettings);
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

#endif /* ENGINE_H_ */
