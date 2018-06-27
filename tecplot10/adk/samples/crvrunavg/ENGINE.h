#ifndef  ENGINE_H_
#define ENGINE_H_ /* Only include once */




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
