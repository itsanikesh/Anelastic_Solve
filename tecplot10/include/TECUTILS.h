/*
******************************************************************
******************************************************************
*******                                                   ********
******             (C) 1988-2004 Tecplot, Inc.            *******
*******                                                   ********
******************************************************************
******************************************************************
*/
/**
 * @file
 */
#if defined EXTERN
#undef EXTERN
#endif
#if defined TECUTILSMODULE
#define EXTERN
#else
#define EXTERN extern
#endif

/**
 *   Sets the position and size of the current frame.
 *   
 * @param X
 *   X-Coordinate for the upper left-hand corner of the frame in inches
 *   relative to the upper left-hand corner of the paper
 * @param Y
 *   Y-Coordinate for the upper left-hand corner of the frame in inches
 *   relative to the upper left-hand corner of the paper
 * @param Width
 *   Width of the frame in inches.
 * @param Height
 *   Height of the frame in inches.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameSetPosAndSize(
 *   &                   X,
 *   &                   Y,
 *   &                   Width,
 *   &                   Height)
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *   
 *   Set the frame's position and size:
 *   
 * @code
 *   TecUtilFrameSetPosAndSize(1.0,1.0,3.0,3.0);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilFrameSetPosAndSize(double X,
                                                                   double Y,
                                                                   double Width,
                                                                   double Height);




/**
 * Low level function used to set most frame and base attribute values in
 * Tecplot. Use this function only if you cannot find an appropriate
 * convenience function that will do the job. The parameters to
 * TecUtilStyleSetLowLevelX() mimic the Macro Frame SetValue Commands and Macro
 * General SetValue Commands described in the Tecplot Reference Manual.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_P1
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   These parameters define the attribute to assign to. Attributes in Tecplot
 *   are defined hierarchically. These parameters follow the same order as you
 *   would use when constructing a macro command. These parameters are actually
 *   strings but you can use the supplied SV_XXXXX constants from the SV.h
 *   include file. Using the SV_ constants will help prevent misspellings and
 *   other errors.
 *   
 * Name:
 *   SV_P2
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   NULL
 * Required:
 *   No
 *   
 * Name:
 *   SV_P3
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   NULL
 * Required:
 *   No
 *   
 * Name:
 *   SV_P4
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   NULL
 * Required:
 *   No
 *   
 * Name:
 *   SV_P5
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   NULL
 * Required:
 *   No
 *   
 * Name:
 *   SV_P6
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   NULL
 * Required:
 *   No
 *   
 * Name:
 *   SV_OBJECTSET
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   When assigning to SV_FIELD attributes (that is, mesh color, scatter symbol
 *   size, and so forth) or to SV_LINEMAP attributes (that is, bar chart color,
 *   error bar type, and so forth) the set defines the zones on which to
 *   opererate.
 *   
 * Name:
 *   SV_OFFSET1
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Those options that don't require SV_OBJECTSE set often have need of one or
 *   more of these offset parameters.
 *   
 * Name:
 *   SV_OFFSET2
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *   
 * Name:
 *   SV_ASSIGNMODIFIER
 * Type:
 *   AssignOp_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   AssignOp_Equals
 * Required:
 *   No
 * Notes:
 *   The possible values are:
 *   
 * Name:
 *   SV_DVALUE
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   If the attribute to be assigned requires a floating value then DValue is
 *   used as the value to assign.
 *   
 * Name:
 *   SV_IVALUE
 * Type:
 *   ArbParam_t
 * Arg Function:
 *   TecUtilArgListAppendArbParam()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   If the attribute to be assigned requires an integer, an enumerated value
 *   or IValue is used as the value to assign.
 *   
 * Name:
 *   SV_STRVALUE
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   Null
 * Required:
 *   No
 * Notes:
 *   If the attribute to be assigned requires a string than StrValue is used as
 *   the value to assign.
 * </ArgListTable>
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStyleSetLowLevelX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   Set the contour variable of the second contour group to variable number 4:
 *   
 * @code
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   if (ArgList != NULL)
 *     {
 *        SetValueReturnCode_e SVRC;
 *        TecUtilArgListClear(ArgList);
 *        TecUtilArgListAppendString(ArgList, SV_P1, 
 *                                SV_GLOBALCONTOUR);
 *        TecUtilArgListAppendString(ArgList, SV_P2,      SV_VAR)
 *        TecUtilArgListAppendInt(ArgList,    SV_OFFSET1, 2);
 *        // contour group
 *        TecUtilArgListAppendArbParam(ArgList,    SV_IVALUE,  4);
 *        // variable
 *        SVRC = TecUtilStyleSetLowLevelX(ArgList);
 *        // A more conventient way to do this is to use the
 *        // TecUtilContourSetVariableX() function.
 *        TecUtilArgListClear(ArgList);
 *        TecUtilArgListAppendInt(ArgList, SV_CONTOURGROUP, 2);
 *        TecUtilArgListAppendInt(ArgList, SV_VAR, 4);
 *        SVRC = TecUtilContourSetVariableX(ArgList);
 *        TecUtilArgListDealloc(&ArgList);
 *       }
 *   TecUtilLockFinish(AddOnID);
 *   
 * @endcode
 *
 *
 * FORTRAN EXAMPLE:
 *
 * This will set the value blanking condition
 * cutoff value to be V3 >= 0.6
 *
 * Using the "X" function we have the following 
 * arglist entries are available:
 *  
 * @verbatim
     Name              Type
    -------------------------------
     'P1'              string
     'P2'              string
     'P3'              string
     'P4'              string
     'P5'              string
     'P6'              string
     'OBJECTSET'       Set_pa
     'OFFSET1'         LgIndex_t
     'OFFSET2'         LgIndex_t
     'ASSIGNMODIFIER'  AssignOp_e
     'DVALUE'          double
     'IVALUE'          ArbParam_t
   @endverbatim
 *
 * @code
 * C
 * C ... Set the value blanking variable to 3
 * C
 *       Call TecUtilArgListAlloc(ArgListPtr)
 *       IErr = TecUtilArgListAppendString(ArgListPtr, 
 *      &                                  'P1'//char(0),
 *      &                                  'BLANKING'//char(0))
 * 
 *       IErr = TecUtilArgListAppendString(ArgListPtr,
 *      &                                  'P2'//char(0),
 *      &                                  'VALUE'//char(0))
 * 
 *       IErr = TecUtilArgListAppendString(ArgListPtr,
 *      &                                  'P3'//char(0),
 *      &                                  'CONSTRAINT'//char(0))
 * 
 *       IErr = TecUtilArgListAppendString(ArgListPtr,
 *      &                                  'P4'//char(0),
 *      &                                  'VARA'//char(0))
 * 
 *       IErr = TecUtilArgListAppendInt(   ArgListPtr,
 *      &                                  'OFFSET1'//char(0),
 *      &                                  1)
 * 
 *       IValuePtr = 3
 *       IErr = TecUtilArgListAppendArbParam( ArgListPtr,
 *      &                                    'IVALUE'//char(0),
 *      &                                    IValuePtr)
 *       write(*,*) 'setting constraint vara'
 *       IRet = TecUtilStyleSetLowLevelX(ArgListPtr)
 * 
 *       Call TecUtilArgListDealloc(ArgListPtr)
 * 
 * C
 * C ... Set cutoff value.
 * C
 *       Call TecUtilArgListAlloc(ArgListPtr)
 *       IErr = TecUtilArgListAppendString(ArgListPtr, 
 *      &                                  'P1'//char(0),
 *      &                                  'BLANKING'//char(0))
 * 
 *       IErr = TecUtilArgListAppendString(ArgListPtr,
 *      &                                  'P2'//char(0),
 *      &                                  'VALUE'//char(0))
 * 
 *       IErr = TecUtilArgListAppendString(ArgListPtr,
 *      &                                  'P3'//char(0),
 *      &                                  'CONSTRAINT'//char(0))
 * 
 *       IErr = TecUtilArgListAppendString(ArgListPtr,
 *      &                                  'P4'//char(0),
 *      &                                  'VALUECUTOFF'//char(0))
 * 
 *       IErr = TecUtilArgListAppendInt(   ArgListPtr,
 *      &                                  'OFFSET1'//char(0),
 *      &                                  1)
 * 
 *       IErr = TecUtilArgListAppendDouble( ArgListPtr,
 *      &                                  'DVALUE'//char(0),
 *      &                                  0.6D0)
 *       write(*,*) 'setting constraint value cut-off'
 *       IRet = TecUtilStyleSetLowLevelX(ArgListPtr)
 * 
 *       Call TecUtilArgListDealloc(ArgListPtr)
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilStyleSetLowLevelX(ArgList_pa ArgList);








/**
 *   Low level function used to set most frame and base attribute values in
 *   Tecplot. Use this function only if you cannot find an appropriate
 *   convenience function that will do the job. As of Tecplot v. 10, this
 *   function has ben superseded by TecUtilStyleSetLowLevelX(). The parameters
 *   to TecUtilStyleSetLowLevel() mimic the Macro Frame SetValue Commands and
 *   Macro General SetValue Commands described in the Tecplot Reference Manual.
 *   
 * @param TextFieldWidget
 *   Under Windows, this parameter is not allowed and must be set to NULL.Under
 *   Motif, if the value is coming from a text field and you supply the text
 *   field's name to TecUtilStyleSetLowLevel(), the following will happen: It
 *   will parse the value in the text field for you.Tecplot will repair the
 *   text field if the input value is invalid
 *   
 * @param DValue
 *   If the attribute to be assigned requires a floating point value and
 *   TextFieldWidget is set to NULL then DValue is used as the value to assign
 *   
 * @param IValue
 *   If the attribute to be assigned requires an integer, an enumerated value,
 *   or is a handle to a string, and, TextFieldWidget is set to NULL then
 *   IValue is used as the value to assign
 *   
 * @param SetOrOffset
 *   Some attributes require further definition on what to assign the incoming
 *   value to. When assigning to SV_FIELD attributes (that is, mesh color,
 *   scatter symbol size, and so forth) or to SV_LINEMAP attributes (that is,
 *   bar chart color, error bar type, and so forth). SetOrOffset is a Set_pa
 *   type.For field attributes it represents the set of zones to operate on and
 *   for Line-map attributes it represents the set of Line-maps to operate on.
 *   In a few other cases, SetOrOffset represents the offset into a list of
 *   items. For example, when assigning attributes for the third X-axis in an
 *   XY-plot, SetOrOffset is set to 3
 *   
 * @param AssignModifier
 *   The possible values are:AssignOp_Equals Assign the value directly to the
 *   attribute.AssignOp_PlusEquals Add the value to the current attribute
 *   value.  AssignOp_MinusEquals Subtract the value from the current attribute
 *   value.  AssignOp_TimesEquals Multiply the value with the current attribute
 *   value.  AssignOp_DivideEquals Divide the current attribute value by the
 *   supplied value.
 *   
 * @param P1
 *   The first parameter used to define the attribute to assign to. Attributes
 *   in Tecplot are defined hierarchically. These parameters follow the same
 *   order as you would use when constructing a macro command. These parameters
 *   are actually strings but you can use the supplied SV_XXXXX constants from
 *   the SV.h include file. Using the SV_ constants will help prevent
 *   misspellings and other errors
 *   
 * @param P2
 *   The second parameter used to define the attribute to assign to. See P1.
 *   
 * @param P3
 *   The third parameter used to define the attribute to assign to. See P1.
 *   
 * @param P4
 *   The fourth parameter used to define the attribute to assign to. See P1.
 *   
 * @param P5
 *   The fifth parameter used to define the attribute to assign to. See P1.
 *   
 * @param P6
 *   The sixth parameter used to define the attribute to assign to. See P1.
 *   
 * @param OkToRecord
 *   Not used anymore. Always set to FALSE.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *
   @verbatim
     SetValue_NoAttachedDatasetError 
     SetValue_NoAttachedFrameError 
     SetValue_NotAllowedInConfigError
     SetValue_ValueRangeError 
     SetValue_ValueSyntaxError 
     SetValue_AssignOpError
     SetValue_InvalidVarOrZone 
     SetValue_InternalMemoryError 
     SetValue_ContextError1
     SetValue_ContextError2
   @endverbatim
 *
 *  Usually, a return value of SetValue_Ok or SetValue_DuplicateValue can be
 *  interpreted as being successful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStyleSetLowLevel(
 *   &                   TextFieldWidgetPtr,
 *   &                   DValue,
 *   &                   IValuePtr,
 *   &                   SetOrOffsetPtr,
 *   &                   AssignModifier,
 *   &                   P1,
 *   &                   P2,
 *   &                   P3,
 *   &                   P4,
 *   &                   P5,
 *   &                   P6,
 *   &                   OkToRecord)
 *    POINTER         (TextFieldWidgetPtr, TextFieldWidget)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 *    POINTER         (SetOrOffsetPtr, SetOrOffset)
 *    INTEGER*4       AssignModifier
 *    CHARACTER*(*)   P1
 *    CHARACTER*(*)   P2
 *    CHARACTER*(*)   P3
 *    CHARACTER*(*)   P4
 *    CHARACTER*(*)   P5
 *    CHARACTER*(*)   P6
 *    INTEGER*4       OkToRecord
 * </FortranSyntax>
 *   
 * Call TecUtilStyleSetLowLevel() to assign the color red to the text on the
 * second Y-axis in an XY-plot.
 *
 * From the reference manual (or by recording a macro) we see that the macro
 * command to accomplish this feat is:
 * @code
 *   $!XYAXIS YDETAIL 2 { TICKLABEL { COLOR = BLUE } }
 * @endcode
 *
 * The corresponding call to TecUtilStyleSetLowLevel() is then:
 * @code
 *   S = TecUtilStyleSetLowLevel((Widget)NULL,
 *                               0.0,                // Not Used
 *                               (ArbParam_t)Blue_C,
 *                               (ArbParam_t)2,      // second Y-Axis
 *                               AssignOp_Equals,
 *                               SV_XYAXIS,
 *                               SV_YDETAIL,
 *                               SV_TICKLABEL,
 *                               SV_COLOR,
 *                               (char *)NULL,
 *                               (char *)NULL,
 *                               TRUE);
 * @endcode
 *   
 * From the Tecplot Reference Manual (or by recording a macro) we see that
 * the macro command to accomplish this feat is:
 * @code
 *   $!FIELD [1-3,7,9] POINTS { IJKSKIP { I = 2 } }
 * @endcode
 *
 * The corresponding call to TecUtilStyleSetLowLevel() is then:
 * @code
 *   Set_pa               ZoneSet;
 *   SetValueReturnCode_e SVRC;
 *   
 *   ZoneSet = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(ZoneSet,1,FALSE);
 *   TecUtilSetAddMember(ZoneSet,2,FALSE);
 *   TecUtilSetAddMember(ZoneSet,3,FALSE);
 *   TecUtilSetAddMember(ZoneSet,7,FALSE);
 *   TecUtilSetAddMember(ZoneSet,9,FALSE);
 * @endcode
 *   
 * For the following
 * @code
 *   $!Field [zoneset] Points {IJKSkip {I = (LgIndex_t)}}
 * @endcode
 *
 * use the the corresponding TecUtilStyleSetLowLevel() call:
 * @code
 *   SVRC = TecUtilStyleSetLowLevel((Widget)NULL,
 *                                  0.0,              // Not used
 *                                  (ArbParam_t)2,    // Vector Skip
 *                                  (ArbParam_t)ZoneSet,
 *                                  AssignOp_Equals,
 *                                  SV_FIELD,
 *                                  SV_POINTS,
 *                                  SV_IJKSKIP,
 *                                  SV_I,
 *                                  (char *)NULL,
 *                                  (char *)NULL,
 *                                  FALSE);
 * @endcode
 *   
 * The above task could also be accomplished by using the convenience function
 * TecUtilZoneSetVectorIJKSkip() as follows:
 * @code
 *   S = TecUtilZoneSetVectorIJKSkip(SV_I, ZoneSet, 2);
 * @endcode
 *   
 * Call TecUtilStyleSetLowLevel() to assign the I-Skip to 2 for vectors and
 * scatter symbols in zones 1-3, 7, and 9.
 *
 * From the Tecplot Reference Manual (or by recording a macro) we see that the
 * macro command to accomplish this feat is:
 * @code
 *   $!FIELD [1-3,7,9] POINTS { IJKSKIP { I = 2 } }
 * @endcode
 *
 * The corresponding call to TecUtilStyleSetLowLevel() is then:
 * @code
 *   Set_pa               ZoneSet;
 *   SetValueReturnCode_e SVRC;
 *   
 *   ZoneSet = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(ZoneSet,1,FALSE);
 *   TecUtilSetAddMember(ZoneSet,2,FALSE);
 *   TecUtilSetAddMember(ZoneSet,3,FALSE);
 *   TecUtilSetAddMember(ZoneSet,7,FALSE);
 *   TecUtilSetAddMember(ZoneSet,9,FALSE);
 * @endcode
 *   
 * For the following
 * @code
 *   $!Field [zoneset] Points {IJKSkip {I = (LgIndex_t)}}
 * @endcode
 *
 * use the the corresponding TecUtilStyleSetLowLevel() call:
 *   
 * @code
 *   SVRC = TecUtilStyleSetLowLevel((Widget)NULL,
 *                                  0.0,              // Not used
 *                                  (ArbParam_t)2,    // Vector Skip
 *                                  (ArbParam_t)ZoneSet,
 *                                  AssignOp_Equals,
 *                                  SV_FIELD,
 *                                  SV_POINTS,
 *                                  SV_IJKSKIP,
 *                                  SV_I,
 *                                  (char *)NULL,
 *                                  (char *)NULL,
 *                                  FALSE);
 * @endcode
 *   
 * The above task could also be accomplished by using the convenience 
 * function TecUtilZoneSetVectorIJKSkip() as follows:
 * @code
 *   S = TecUtilZoneSetVectorIJKSkip(SV_I, ZoneSet, 2);
 * @endcode
 *
 *
 * FORTRAN EXAMPLE:  Set Surfaces to plot to be I-Planes for zone 2.
 *
 * @code
 *       Call TecUtilSetAlloc(IShowErr,ZoneListPtr)
 *       IErr = TecUtilSetAddMember(ZoneListPtr,2,IShowErr)
 *       C
 *       C ... Must use a pointer to ship IValue because call stack expects
 *       C ... argument the size of a pointer.
 *       C
 *       IValuePtr = SurfacesToPlot_IPlanes
 *       ISVRC = TecUtilStyleSetLowLevel(LocalNullPtr,
 *      &                                0.0D0,
 *      &                                IValuePtr,
 *      &                                ZoneListPtr,
 *      &                                AssignOp_Equals,
 *      &                                'FIELD'//char(0),
 *      &                                'SURFACES'//char(0),
 *      &                                'SURFACESTOPLOT'//char(0),
 *      &                                char(0),
 *      &                                char(0),
 *      &                                char(0),
 *      &                                FALSE)
 *       Call TecUtilSetDealloc(ZoneListPtr)
 * @endcode
 *
 *
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilStyleSetLowLevel(Widget       TextFieldWidget,
                                                                 double       DValue,
                                                                 ArbParam_t   IValue,
                                                                 ArbParam_t   SetOrOffset,
                                                                 AssignOp_e   AssignModifier,
                                                                 const char  *P1,
                                                                 const char  *P2,
                                                                 const char  *P3,
                                                                 const char  *P4,
                                                                 const char  *P5,
                                                                 const char  *P6,
                                                                 Boolean_t    OkToRecord);

/**
 * @deprecated
 *   Please use TecUtilFrameSetPlotType() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilFrameSetMode(FrameMode_e NewFrameMode);




/**
 *   Set the frame mode.
 *   
 * @param NewPlotType
 *   Plot type to switch to. The possible values are: PlotType_Cartesian3D, PlotType_Cartesian3D,
 *   PlotType_XYLine or PlotType_Sketch
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameSetPlotType(NewPlotType)
 *    INTEGER*4 NewPlotType
 * </FortranSyntax>
 *   
 *   Set the current frame's mode to be 3D:
 *   
 * @code
 *   TecUtilFrameSetPlotType(PlotType_Cartesian3D);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilFrameSetPlotType(PlotType_e NewPlotType);
/**
 * Set the name for the current frame.
 *   
 * @param Name
 *   Name to assign to the current frame.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameSetName(Name)
 *    CHARACTER*(*) Name
 * </FortranSyntax>
 *   
 *   Rename the current frame to be "XY-plot #1":
 *   
 * @code
 *   TecUtilFrameSetName("XY-plot #1");
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilFrameSetName(const char *Name);

/**
 *   Assign which zones are active.
 *   
 * @param ZoneSet
 *   Set of zones used to change the set of active zones. The way in which the active zones are changed
 *   is based on the AssignModifier. Must not be NULL.
 *   
 * @param AssignModifier
 *   The possible values are: AssignOp_Equals, AssignOp_PlusEquals, AssignOp_MinusEquals
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneSetActive(
 *   &                   ZoneSetPtr,
 *   &                   AssignModifier)
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    INTEGER*4       AssignModifier
 * </FortranSyntax>
 *   
 *   Activate zone 3:
 *   
 * @code
 *   Set_pa zone_set = TecUtilSetAlloc();
 *   TecUtilSetAddMember(zone_set, 3,TRUE);
 *   TecUtilZoneSetActive(zone_set, AssignOp_PlusEquals);
 *   TecUtilSetDealloc(&zone_set);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilZoneSetActive(Set_pa     ZoneSet,
                                                              AssignOp_e AssignModifier);

/**
 * @deprecated
 *   Please use TecUtilLineMapSetActive() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilXYMapSetActive(Set_pa     XYMapSet,
                                                               AssignOp_e AssignModifier);



/**
 *   Assigns which Line-maps are active.
 *   
 * @param LineMapSet
 *   Set of Line-maps used to change the current set of active Line-maps. How the active Line-maps are
 *   changed is based on the AssignModifier
 *   
 * @param AssignModifier
 *   The possible values are: AssignOp_Equals, AssignOp_PlusEquals, AssignOp_MinusEquals,
 *   AssignOp_TimesEquals, AssignOp_DivideEquals
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapSetActive(
 *   &                   LineMapSetPtr,
 *   &                   AssignModifier)
 *    POINTER         (LineMapSetPtr, LineMapSet)
 *    INTEGER*4       AssignModifier
 * </FortranSyntax>
 *   
 *   Add Line-map 3 to the set of Line-maps:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,3,FALSE);
 *   TecUtilLineMapSetActive(set,AssigOp_PlusEquals);
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilLineMapSetActive(Set_pa     LineMapSet,
                                                                 AssignOp_e AssignModifier);

/**
 * @deprecated
 *   Please use TecUtilFieldLayerSetIsActive() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilFieldSetLayer(const char *LayerShowFlag,
                                                              Boolean_t   TurnOnFieldLayer);


/**
 *   Instructs Tecplot to turn the specified layer on or off.
 *   
 * @param LayerShowFlag
 *   The show flag for the field layer of interest. Possible values are:
   @verbatim
     SV_SHOWMESH 
     SV_SHOWCONTOUR
     SV_SHOWVECTOR 
     SV_SHOWSCATTER 
     SV_SHOWSHADE 
     SV_SHOWBOUNDARY
   @endverbatim
 *   
 * @param TurnOnFieldLayer
 *   If TRUE, Tecplot will turn on the layer, otherwise it will turn the layer off
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldLayerSetIsActive(
 *   &                   LayerShowFlag,
 *   &                   TurnOnFieldLayer)
 *    CHARACTER*(*)   LayerShowFlag
 *    INTEGER*4       TurnOnFieldLayer
 * </FortranSyntax>
 *   
 *   Turn on the field's contour layer:
 *   
 * @code
 *   SetValueReturnCode_e SVRC;
 *   SVRC = TecUtilFieldLayerSetIsActive(SV_SHOWCONTOUR, TRUE);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilFieldLayerSetIsActive(const char *LayerShowFlag,
                                                                      Boolean_t   TurnOnFieldLayer);

/**
 * @deprecated
 *   Please use TecUtilLinePlotLayerSetIsActive() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilXYSetLayer(const char *LayerShowFlag,
                                                           Boolean_t   TurnOnXYLayer);
/**
 *   Instructs Tecplot to turn on or off the specified line plot layer.
 *   
 * @param LayerShowFlag
 *   Show flag for the line plot layer of interest
 *   
 * @param TurnOnLinePlotLayer
 *   If TRUE Tecplot will turn on the layer, otherwise it will turn the layer off.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLinePlotLayerSetIsActive(
 *   &                   LayerShowFlag,
 *   &                   TurnOnLinePlotLayer)
 *    CHARACTER*(*)   LayerShowFlag
 *    INTEGER*4       TurnOnLinePlotLayer
 * </FortranSyntax>
 *   
 *   Turn on the line plot's symbol layer:
 *   
 * @code
 *   SetValueReturnCode_e SVRC;
 *   SVRC = TecUtilLinePlotLayerSetIsActive(SV_SHOWSYMBOLS, TRUE);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilLinePlotLayerSetIsActive(const char *LayerShowFlag,
                                                                         Boolean_t   TurnOnLinePlotLayer);




/**
 *   Assign which variable to use for contouring a specific contour group.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_CONTOURGROUP
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Contour group to which the contour variable assignment applies.  This value must be between 1 and 4
 *   
 * Name:
 *   SV_VAR
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Variable number to assign as the contour variable for the specified group
 * </ArgListTable>
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilContourSetVariableX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   Set the contour variable of the second contour group to variable number 4:
 *   
 * @code
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   if (ArgList != NULL)
 *   {
 *      SetValueReturnCode_e SVRC;
 *      TecUtilArgListClear(ArgList);
 *      TecUtilArgListAppendInt(ArgList, SV_CONTOURGROUP, 2);
 *      TecUtilArgListAppendInt(ArgList, SV_VAR, 4);
 *      SVRC = TecUtilContourSetVariableX(ArgList);
 *      TecUtilArgListDealloc(&ArgList);
 *   }
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilContourSetVariableX(ArgList_pa ArgList);



/**
 *   Assign which variable to use for contouring.   This function only operates 
 *   on contour group 1.  To operate on any contour group you must use
 *   TecUtilContourSetVariableX().
 *   
 * @param NewVariable
 *   Number of the variable to use.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilContourSetVariable(NewVariable)
 *    INTEGER*4 NewVariable
 * </FortranSyntax>
 *   
 *   To set the contour variable to be variable 3, use:
 *   
 * @code
 *   TecUtilContourSetVariable(3);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilContourSetVariable(EntIndex_t NewVariable);

/**
 *   Assign values to attributes for mesh plots.
 *   
 * @param Attribute
 *   Specify the attribute to change from the possible values found below:
 *
 *
   @verbatim
        Attribute              Assign To    Value Notes
        ---------------------------------------------------------
        SV_SHOW                IValue       TRUE,FALSE
        SV_MESHTYPE            IValue       MeshType_e
        SV_COLOR               IValue       ColorIndex_t
        SV_LINEPATTERN         IValue       LinePattern_e
        SV_PATTERNLENGTH       DValue       Valid pattern length
        SV_LINETHICKNESS       DValue       Valid line thickness
   @endverbatim
 *   
 * @param ZoneSet
 *   Set of zones to operate on. Pass NULL to operate on all zones.
 *   
 * @param DValue
 *   If the attribute requires a floating point value then put that value in
 *   DValue, otherwise DValue is not used.
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a
 *   string then assigned it to the IValue parameter. Always typecast the
 *   IValue parameter to ArbParam_t
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneSetMesh(
 *   &                   Attribute,
 *   &                   ZoneSetPtr,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Set the mesh color for all zones to be red:
 *   
 * @code
 *   TecUtilZoneSetMesh(SV_Color,NULL,0.0,(ArbParam_t)Blue_C);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilZoneSetMesh(const char *Attribute,
                                                            Set_pa      ZoneSet,
                                                            double      DValue,
                                                            ArbParam_t  IValue);

/**
 *   Assign values to attributes for contour plots.
 *   
 * @param Attribute
 *   Specify the attribute to change from the possible values found below:
 *
   @verbatim
        Attribute                  I or D Value     Notes
        -----------------------------------------------------------------
        SV_SHOW                         IValue      TRUE, FALSE
        SV_CONTOURTYPE                  IValue      ContourType_e
        SV_COLOR                        IValue      ColorIndex_t
        SV_LINEPATTERN                  IValue      LinePattern_e
        SV_PATTERNLENGTH                DValue      Valid length
        SV_LINETHICKNESS                DValue      Valid thickness
   @endverbatim
 *   
 * @param ZoneSet
 *   Set of zones to operate on. Pass NULL to operate on all zones.
 *   
 * @param DValue
 *   If the attribute requires a floating point value, put that value in
 *   DValue, otherwise DValue is not used.
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a
 *   string then assigned it to the IValue parameter. Always typecast the
 *   IValue parameter to ArbParam_t
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneSetContour(
 *   &                   Attribute,
 *   &                   ZoneSetPtr,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Change the contour plot type to flood for the first zone:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,1,FALSE);
 *   TecUtilZoneSetContour(SV_CONTOURTYPE,set,0.0,
 *                         (ArbParam_t)Contour_Flood);
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilZoneSetContour(const char *Attribute,
                                                               Set_pa      ZoneSet,
                                                               double      DValue,
                                                               ArbParam_t  IValue);

/**
 *   Assign values to attributes for vector plots.
 *   
 * @param Attribute
 *   Specify the attribute to change from the possible values found below:
 *   
   @verbatim
        Attribute              Assign To    Value Notes
        ---------------------------------------------------------
        SV_SHOW                IValue       TRUE,FALSE
        SV_VECTORTYPE          IValue       VectorType_e
        SV_COLOR               IValue       ColorIndex_t
        SV_LINEPATTERN         DValue       LinePattern_e
        SV_PATTERNLENGTH       DValue       Valid pattern length
        SV_LINETHICKNESS       DValue       Valid line thickness
        SV_ARROWHEADSTYLE      IValue       ArrowheadStyle_e
        SV_ISTANGENT           IValue       TRUE,FALSE
   @endverbatim
 *
 * @param ZoneSet
 *   Set of zones to operate on. Pass NULL to operate on all zones
 *   
 * @param DValue
 *   If the attribute requires a floating point value then put that value in
 *   DValue, otherwise DValue is not used.
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a
 *   string then assign it to the IValue parameter. Always typecast the IValue
 *   parameter to ArbParam_t.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneSetVector(
 *   &                   Attribute,
 *   &                   ZoneSetPtr,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Set the arrowhead style for vector plots to be "plain" for all zones:
 *   
 * @code
 *   TecUtilZoneSetVector(SV_ARROWHEADSTYLE,NULL,0.0,
 *                        (ArbParam_t)Arrowhead_Plain);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilZoneSetVector(const char *Attribute,
                                                              Set_pa      ZoneSet,
                                                              double      DValue,
                                                              ArbParam_t  IValue);

/**
 *   Set the vector I-, J-, or K-skipping.
 *   
 * @param Attribute
 *   Specify the attribute (in this case, I, J, or K-skip) to change. The
 *   possible values are: SV_I, SV_J, or SV_K
 *   
 * @param ZoneSet
 *   Set of zones to operate on. Pass NULL to operate on all zones
 *   
 * @param Skip
 *   The vector skip value to assign
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneSetVectorIJKSkip(
 *   &                   Attribute,
 *   &                   ZoneSetPtr,
 *   &                   Skip)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    INTEGER*4       Skip
 * </FortranSyntax>
 *   
 *   Set the vector I-skip to two for all zones:
 *   
 * @code
 *   TecUtilZoneSetVectorIJKSkip(SV_I,NULL,(ArbParam_t)2);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilZoneSetVectorIJKSkip(const char *Attribute,
                                                                     Set_pa      ZoneSet,
                                                                     LgIndex_t   Skip);
/**
 *   Assign top level values to attributes for scatter plots.
 *   
 * @param Attribute
 *   Specify the attribute to change from the possible values found below:
 *   
   @verbatim
        Attribute              Assign To    Value Notes
        ---------------------------------------------------------
        SV_SHOW                IValue       TRUE,FALSE
        SV_COLOR               IValue       ColorIndex_t
        SV_ISFILLED            IValue       TRUE,FALSE
        SV_FILLCOLOR           IValue       ColorIndex_t
        SV_SIZEBYVARIABLE      IValue       TRUE,FALSE
        SV_FRAMESIZE           DValue       0.0-100.0
        SV_LINETHICKNESS       DValue       0.001-100.0
   @endverbatim
 * @param ZoneSet
 *   Set of zones to operate on. Pass NULL to operate on all zones
 *   
 * @param DValue
 *   If the attribute requires a double value then assigned it to the DValue
 *   parameter.
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a
 *   string then assigned it to the IValue parameter. Always typecast the
 *   IValue parameter to ArbParam_t
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneSetScatter(
 *   &                   Attribute,
 *   &                   ZoneSetPtr,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Set scatter line thickness for all zones to 0.1:
 *   
 * @code
 *   TecUtilZoneSetScatter(SV_LINETHICKNESS,NULL,0.1,(ArbParam_t)0);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilZoneSetScatter(const char *Attribute,
                                                               Set_pa      ZoneSet,
                                                               double      DValue,
                                                               ArbParam_t  IValue);

/**
 *   Set the scatter I-, J-, or K-skipping.
 *   
 * @param Attribute
 *   Specify the attribute (in this case, I-, J-, or K-skip) to change. The
 *   possible values are SV_I, SV_J, or SV_K.
 *   
 * @param ZoneSet
 *   Set of zones to operate on. Pass NULL to operate on all zones.
 *   
 * @param Skip
 *   The scatter skip value to assign.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneSetScatterIJKSkip(
 *   &                   Attribute,
 *   &                   ZoneSetPtr,
 *   &                   Skip)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    INTEGER*4       Skip
 * </FortranSyntax>
 *   
 *   Set the scatter I-skip to two for all zones:
 *   
 * @code
 *   TecUtilZoneSetScatterIJKSkip(SV_I,NULL,(ArbParam_t)2);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilZoneSetScatterIJKSkip(const char *Attribute,
                                                                      Set_pa      ZoneSet,
                                                                      LgIndex_t   Skip);

/**
 *   Assign values for the symbol shape in scatter plots.
 *   
 * @param Attribute
 *   Specify the attribute to change from the possible values found below:
 *
 *
   @verbatim
        Attribute              Assign To    Value Notes
        ---------------------------------------------------------
        SV_ISASCII             IValue       TRUE,FALSE
        SV_GEOMSHAPE           IValue       GeomShape_e
        SV_ASCIICHAR           IValue       Character string.  Must
                                            be a single character (like "A")
                                            or a font modifier plust a single
                                            character (like "~A").
   @endverbatim
 *
 *   
 * @param ZoneSet
 *   Set of zones to operate on. Pass NULL to operate on all zones
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a
 *   string then assigned it to the IValue parameter. Always typecast the
 *   IValue parameter to ArbParam_t
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneSetScatterSymbolShap(
 *   &                   Attribute,
 *   &                   ZoneSetPtr,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 * @par Note:
 *   IMPORTANT!  Note that the FORTRAN Name for this function is truncated
 *   to 31 characters!
 *   
 *   Set the scatter symbol shape to squares for all zones:
 *   
 * @code
 *   TecUtilZoneSetScatterSymbolShape(SV_ISASCII,NULL,
 *                                    (ArbParam_t)FALSE);
 *   TecUtilZoneSetScatterSymbolShape(SV_GEOMSHAPE,NULL,
 *                                    (ArbParam_t)GeomShape_Square);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilZoneSetScatterSymbolShape(const char *Attribute,
                                                                          Set_pa      ZoneSet,
                                                                          ArbParam_t  IValue);

/**
 * Assign values to attributes for shade plots.
 *   
 * @param Attribute
 *   Specify the attribute to change from the possible values found below:
 *   
 *   
   @verbatim
        Attribute              Assign To    Value Notes
        ---------------------------------------------------------
        SV_SHOW                IValue       TRUE,FALSE
        SV_COLOR               IValue       ColorIndex_t
        SV_USELIGHTINGEFFECT   IValue       TRUE,FALSE
   @endverbatim
 * @param ZoneSet
 *   Set of zones to operate on. Pass NULL to operate on all zones.
 *   
 * @param DValue
 *   If the attribute requires a floating point value then put that value in
 *   DValue, otherwise DValue is not used. This is reserved for future use.
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a
 *   string then assign it to the IValue parameter. Always typecast the
 *   IValueparameter to ArbParam_t.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneSetShade(
 *   &                   Attribute,
 *   &                   ZoneSetPtr,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Set the shade color to red:
 * @code
 *   TecUtilZoneSetShade(SV_COLOR,NULL,0.0,(ArbParam_t)Red_C);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilZoneSetShade(const char *Attribute,
                                                             Set_pa      ZoneSet,
                                                             double      DValue,
                                                             ArbParam_t  IValue);

/**
 *   Assign values to attributes for boundary plots.
 *   
 * @param Attribute
 *   Specify the attribute to change from the possible values found below:
 *
 *
   @verbatim
        Attribute                  I or D Value     Notes
        -----------------------------------------------------------------
        SV_SHOW                    IValue          TRUE, FALSE
        SV_IBOUNDARY               IValue          BoundaryType_e
        SV_JBOUNDARY               IValue          BoundaryType_e
        SV_KBOUNDARY               IValue          BoundaryType_e
        SV_COLOR                   IValue          Valid color index.
        SV_LINETHICKNESS           DValue          Valid line thickness
   @endverbatim
 *   
 * @param ZoneSet
 *   Set of zones to operate on. Pass NULL to operate on all zones
 *   
 * @param DValue
 *   If the attribute requires a floating point value then put that value in
 *   DValue, otherwise DValue is not used
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a
 *   string then assigned it to the IValue parameter. Always typecast the
 *   IValue parameter to ArbParam_t
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneSetBoundary(
 *   &                   Attribute,
 *   &                   ZoneSetPtr,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Set the line thickness for the first zone to 0.1:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,1,FALSE);
 *   TecUtilZoneSetBoundary( SV_LINETHICKNESS,set,0.1,(ArbParam_t)NULL);
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilZoneSetBoundary(const char *Attribute,
                                                                Set_pa      ZoneSet,
                                                                double      DValue,
                                                                ArbParam_t  IValue);

/**
 *   Assign the Volume Mode for field plots.
 *   
 * @param Attribute
 *   Specify the attribute to change.  See SubAttribute for possible value
 *   combinations.
 *   
 * @param SubAttribute
 *   Specify the sub-attribute to change.   The table below lists possible 
 *   Attribute/SubAttribute combinations.
 *
   @verbatim
   Attribute               SubAttribute  IValue
   ------------------------------------------------------------------------------------------------
   SV_SURFACESTOPLOT       NULL          SurfacesToPlot_BoundaryFaces,SurfacesToPlot_ExposedCellFaces, 
                                         SurfacesToPlot_IPlanes,SurfacesToPlot_JPlanes,
                                         SurfacesToPlot_KPlanes,SurfacesToPlot_IJPlanes,
                                         SurfacesToPlot_JKPlanes,SurfacesToPlot_IKPlanes,
                                         SurfacesToPlot_IJKPlanes,SurfacesToPlot_All

   SV_VOLUMEOBJECTSTOPLOT  NULL          VolumeObjectsToPlot_All,VolumeObjectsToPlot_StreamtracesOnly,
                                         VolumeObjectsToPlot_IsoSurfacesOnly,VolumeObjectsToPlot_None

   SV_POINTSTOPLOT         NULL          PointsToPlot_SurfacesOnly, PointsToPlot_All.

   SV_IRANGE               SV_MIN        Any integer.  0 means IMax
   SV_IRANGE               SV_MAX        Any integer.  0 means IMax
   SV_IRANGE               SV_SKIP       Any integer.  0 means IMax-1
   SV_JRANGE               SV_MIN        Any integer.  0 means JMax
   SV_JRANGE               SV_MAX        Any integer.  0 means JMax
   SV_JRANGE               SV_SKIP       Any integer.  0 means JMax-1
   SV_KRANGE               SV_MIN        Any integer.  0 means KMax
   SV_KRANGE               SV_MAX        Any integer.  0 means KMax
   SV_KRANGE               SV_SKIP       Any integer.  0 means KMax-1
   @endverbatim
 *
 *   
 * @param ZoneSet
 *   Set of zones to operate on. Pass NULL to operate on all zones.
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a
 *   string then assign it to the IValue parameter. Always typecast the IValue
 *   parameter to ArbParam_t.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneSetVolumeMode(
 *   &                   Attribute,
 *   &                   SubAttribute,
 *   &                   ZoneSetPtr,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    CHARACTER*(*)   SubAttribute
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Draw I-planes only for the first zone:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,1,FALSE);
 *   TecUtilZoneSetVolumeMode(SV_SURFACESTOPLOT,NULL,set,
 *                         (ArbParam_t)SurfacesToPlot_IPlanes);
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilZoneSetVolumeMode(const char *Attribute,
                                                                  const char *SubAttribute,
                                                                  Set_pa      ZoneSet,
                                                                  ArbParam_t  IValue);


/**
 * @deprecated
 *   Please use TecUtilZoneSetVolumeMode() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilZoneSetIJKMode(const char *Attribute,
                                                               const char *SubAttribute,
                                                               Set_pa      ZoneSet,
                                                               ArbParam_t  IValue);


/**
 * @deprecated
 *   Please use TecUtilLineMapSetName() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilXYMapSetName(Set_pa      XYMapSet,
                                                             const char *NewName);
/**
 *   Set the name of an Line-map.
 *   
 * @param LineMapSet
 *   Set of maps to operate on. Pass NULL to operate on all maps
 *   
 * @param NewName
 *   New name of the map(s). Must not be NULL
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapSetName(
 *   &                   LineMapSetPtr,
 *   &                   NewName)
 *    POINTER         (LineMapSetPtr, LineMapSet)
 *    CHARACTER*(*)   NewName
 * </FortranSyntax>
 *   
 *   Set the name of the first Line-map:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,1,FALSE);
 *   TecUtilLineMapSetName(set,"MyName");
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilLineMapSetName(Set_pa      LineMapSet,
                                                               const char *NewName);

/**
 * @deprecated
 *   Please use TecUtilLineMapSetAssignment() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilXYMapSetAssignment(const char *Attribute,
                                                                   Set_pa      XYMapSet,
                                                                   double      DValue,
                                                                   ArbParam_t  IValue);
/**
 *   Define Line-map assignments.
 *   
 * @param Attribute
 *   Specify the attribute to change. The possible values are:
 *
 *
   @verbatim
        Attribute              Assign To    Value Notes
        ---------------------------------------------------------
        SV_ZONE                IValue       Valid Zone Number
        SV_XAXISVAR            IValue       Valid Variable Number
        SV_YAXISVAR            IValue       Valid Variable Number
        SV_XAXIS               IValue       Axis number (1-5)
        SV_YAXIS               IValue       Axis number (1-5)
        SV_FUNCTIONDEPENDENCY  IValue       FunctionDependency_e
        SV_SORT                IValue       LineMapSort_e
        SV_SORTVAR             IValue       Valid Variable number.
   @endverbatim
 *   
 * @param LineMapSet
 *   Set of Line-maps to operate on. Use NULL to specify all Line-maps
 *   
 * @param DValue
 *   If the attribute requires a floating point value, put that value in
 *   DValue, otherwise DValue is not used. (Reserved for future use)
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a
 *   string then assigned it to the IValue parameter. Always typecast the
 *   IValue parameter to ArbParam_t
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapSetAssignment(
 *   &                   Attribute,
 *   &                   LineMapSetPtr,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (LineMapSetPtr, LineMapSet)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Assign variable 1 to be on the X-axis and variable 4 to be on the Y-axis
 *   for Line-mapping number 7:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,7,FALSE);
 *   TecUtilLineMapSetAssignment(SV_XAXISVAR,set,0.0,(ArbParam_t)1);
 *   TecUtilLineMapSetAssignment(SV_YAXISVAR,set,0.0,(ArbParam_t)4);
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilLineMapSetAssignment(const char *Attribute,
                                                                     Set_pa      LineMapSet,
                                                                     double      DValue,
                                                                     ArbParam_t  IValue);

/**
 * @deprecated
 *   Please use TecUtilLineMapSetLine() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilXYMapSetLine(const char *Attribute,
                                                             Set_pa      XYMapSet,
                                                             double      DValue,
                                                             ArbParam_t  IValue);
/**
 * Assign values to attributes for lines in Line-plots.
 *   
 * @param Attribute
 *   Specify the attribute to change. The possible values are in the table
 *   below:
 *
   @verbatim
        Attribute              Assign To    Value Notes
        ---------------------------------------------------------
        SV_SHOW                IValue       TRUE,FALSE
        SV_COLOR               IValue       ColorIndex_t
        SV_LINEPATTERN         IValue       LinePattern_e
        SV_PATTERNLENGTH       DValue       Valid pattern length
        SV_LINETHICKNESS       DValue       Valid line thickness
   @endverbatim
 *   
 * @param LineMapSet
 *   Set of Line-maps to operate on.
 *   
 * @param DValue
 *   If the attribute requires a floating point value then put that value in
 *   DValue, otherwise DValue is not used.
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a
 *   string then assign it to the IValue parameter. Always typecast the IValue
 *   parameter to ArbParam_t.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapSetLine(
 *   &                   Attribute,
 *   &                   LineMapSetPtr,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (LineMapSetPtr, LineMapSet)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Set line patterns in Line-mappings 3-5 to dashed:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,3,FALSE);
 *   TecUtilSetAddMember(set,4,FALSE);
 *   TecUtilSetAddMember(set,5,FALSE);
 *   TecUtilLineMapSetLine(SV_SHOW,set,0.0,(ArbParam_t)TRUE);
 *   TecUtilLineMapSetLine(SV_LINEPATTERN,
 *                         set, 0.0
 *                         (ArbParam_t)LinePattern_Dashed);
 *           TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilLineMapSetLine(const char *Attribute,
                                                               Set_pa      LineMapSet,
                                                               double      DValue,
                                                               ArbParam_t  IValue);

/**
 * @deprecated
 *   Please use TecUtilLineMapSetCurve() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilXYMapSetCurve(const char *Attribute,
                                                              Set_pa      XYMapSet,
                                                              double      DValue,
                                                              ArbParam_t  IValue);
/**
 *   Assign values to attributes for curves in Line-plots.
 *   
 * @param Attribute
 *   Specify the attribute to change. The possible values are:
 *   
   @verbatim
        Attribute              Assign To    Value Notes
        ---------------------------------------------------------
        SV_NUMPTS              IValue       
        SV_POLYORDER           IValue       1-10
        SV_WEIGHTVAR           IValue       Valid variable number
        SV_INDVARMIN           DValue
        SV_INDVARMAX           DValue       
        SV_USEINDVARRANGE      IValue       TRUE,FALSE
        SV_CLAMPSPLINE         IValue       TRUE,FALSE
        SV_SPLINESLOPE1        DValue       
        SV_SPLINESLOPE2        DValue
        SV_EXTENDEDNAME        IValue       String
        SV_EXTENDEDSETTINGS    IValue       String
        SV_CURVETYPE           IValue       CurveType_e
   @endverbatim
 * @param LineMapSet
 *   Set of Line-maps to operate on. Pass NULL to operate on all Line-maps
 *   
 * @param DValue
 *   If the attribute requires a floating point value, then put that value in DValue, otherwise DValue
 *   is not used
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a string then assigned it
 *   to the IValue parameter. Always typecast the IValue parameter to ArbParam_t
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapSetCurve(
 *   &                   Attribute,
 *   &                   LineMapSetPtr,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (LineMapSetPtr, LineMapSet)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Set Line-mappings 3-5 to draw a polynomial curve fit of order 5:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,3,FALSE);
 *   TecUtilSetAddMember(set,4,FALSE);
 *   TecUtilSetAddMember(set,5,FALSE);
 *   TecUtilLineMapSetCurve(SV_POLYORDER,set,0.0,(ArbParam_t)5);
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilLineMapSetCurve(const char *Attribute,
                                                                Set_pa      LineMapSet,
                                                                double      DValue,
                                                                ArbParam_t  IValue);

/**
 * @deprecated
 *   Please use TecUtilLineMapSetSymbol() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilXYMapSetSymbol(const char *Attribute,
                                                               Set_pa      XYMapSet,
                                                               double      DValue,
                                                               ArbParam_t  IValue);
/**
 * Assign values to attributes for symbols in Line-plots. All attributes except
 * for symbol shape are set here. Use TecUtilLineMapSetSymbolShape() to set the
 * symbol shape.
 *   
 * @param Attribute
 *   Specify the attribute to change. The possible values are in the table below:
 *
 *
   @verbatim
        Attribute              Assign To    Value Notes
        -------------------------------------------------------------------
        SV_SHOW                IValue       TRUE,FALSE
        SV_COLOR               IValue       ColorIndex_t
        SV_ISFILLED            IValue       TRUE,FALSE
        SV_FILLCOLOR           IValue       ColorIndex_t
        SV_SIZE                DValue       Frame Units
        SV_LINETHICKNESS       DValue       Valid line thickness
        SV_SKIPMODE            IValue       Skip_ByIndex,Skip_ByFrameUnits
        SV_SKIPPING            DValue       If SkipMode is Skip_ByIndex
                                            then set to index skip.  If 
                                            SkipMode is Skip_ByFrameUnits then
                                            supply a distance in frame units.
   @endverbatim
 *   
 * @param LineMapSet
 *   Set of Line-maps to operate on.
 *   
 * @param DValue
 *   If the attribute requires a floating point value then put that value in DValue, otherwise DValue
 *   is not used.
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a string then assign it to
 *   the IValue parameter. Always typecast the IValue parameter to ArbParam_t.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapSetSymbol(
 *   &                   Attribute,
 *   &                   LineMapSetPtr,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (LineMapSetPtr, LineMapSet)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Set the skip mode of all Line-maps to frame units (distance):
 *   
 * @code
 *   TecUtilLineMapSetSymbol(SV_SKIPMODE,NULL,0.0,
 *                         (ArbParam_t)Skip_ByFrameUnits);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilLineMapSetSymbol(const char *Attribute,
                                                                 Set_pa      LineMapSet,
                                                                 double      DValue,
                                                                 ArbParam_t  IValue);

/**
 * @deprecated
 *   Please use TecUtilLineMapSetSymbolShape() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilXYMapSetSymbolShape(const char *Attribute,
                                                                    Set_pa      XYMapSet,
                                                                    ArbParam_t  IValue);
/**
 *   Assign values for the symbol shape in Line-plots.
 *   
 * @param Attribute
 *   Specify the attribute to change. The possible values are in the table below:
 *
 *
   @verbatim
        Attribute              Assign To    Value Notes
        ----------------------------------------------------------------
        SV_ISASCII             IValue       TRUE,FALSE
        SV_ASCIICHAR           IValue       String containing a single
                                            character or a lead-in font
                                            modifier plus one character.
        SV_GEOMSHAPE           IValue       GeomShape_e
   @endverbatim
 *   
 * @param LineMapSet
 *   Set of Line-maps to operate on.
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a string then assign it to
 *   the IValue parameter. Always typecast the IValue parameter to ArbParam_t.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapSetSymbolShape(
 *   &                   Attribute,
 *   &                   LineMapSetPtr,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (LineMapSetPtr, LineMapSet)
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Change the symbol shape for symbols drawn with Line-map 3 to use circles:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set, 3,FALSE);
 *   TecUtilLineMapSetSymbolShape(SV_ISASCII,
 *                                set,0.0,
 *                               (ArbParam_t)FALSE);
 *   TecUtilLineMapSetSymbolShape(SV_GEOMSHAPE,set,0.0,
 *                               (ArbParam_t)GeomShape_Circle);
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilLineMapSetSymbolShape(const char *Attribute,
                                                                      Set_pa      LineMapSet,
                                                                      ArbParam_t  IValue);

/**
 * @deprecated
 *   Please use TecUtilLineMapSetBarChart() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilXYMapSetBarChart(const char *Attribute,
                                                                 Set_pa      XYMapSet,
                                                                 double      DValue,
                                                                 ArbParam_t  IValue);
/**
 *   Assign values to attributes for bar charts in Line-plots.
 *   
 * @param Attribute
 *   Specify the attribute to change. The possible values are:
 *   
   @verbatim
        Attribute              Assign To    Value Notes
        ---------------------------------------------------------
        SV_SHOW                IValue       TRUE,FALSE
        SV_COLOR               IValue       ColorIndex_t
        SV_ISFILLED            IValue       TRUE,FALSE
        SV_FILLCOLOR           IValue       ColorIndex_t
        SV_SIZE                DValue       Frame Units
        SV_LINETHICKNESS       DValue       Frame Units
   @endverbatim
 *
 * @param LineMapSet
 *   Set of Line-maps to operate on. Pass NULL to specify all Line-maps
 *   
 * @param DValue
 *   If the attribute requires a floating point value, put that value in DValue, otherwise DValue is
 *   not used.
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a string then assigned it
 *   to the IValue parameter. Always typecast the IValue parameter to ArbParam_t
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapSetBarChart(
 *   &                   Attribute,
 *   &                   LineMapSetPtr,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (LineMapSetPtr,LineMapSet)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr,IValue)
 * </FortranSyntax>
 *   
 *   Set the fill color of the bar chart to red for Line-mapping number 7:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,7,FALSE);
 *   TecUtilLineMapSetBarChart(SV_FILLCOLOR,set,0.0,(ArbParam_t)Red_C);
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilLineMapSetBarChart(const char *Attribute,
                                                                   Set_pa      LineMapSet,
                                                                   double      DValue,
                                                                   ArbParam_t  IValue);

/**
 * @deprecated
 *   Please use TecUtilLineMapSetErrorBar() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilXYMapSetErrorBar(const char *Attribute,
                                                                 Set_pa      XYMapSet,
                                                                 double      DValue,
                                                                 ArbParam_t  IValue);
/**
 *   Assign values to attributes for error bars in Line-plots.
 *   
 * @param Attribute
 *   Specify the attribute to change. The possible values are in the table below:
 *
 *
   @verbatim
        Attribute           Assign To    Value Notes
        ---------------------------------------------------------
        SV_SHOW             IValue       TRUE,FALSE
        SV_VAR              IValue       Valid Variable Number
        SV_BARTYPE          IValue       ErrorBar_e
        SV_COLOR            IValue       ColorIndex_t
        SV_LINETHICKNESS    DValue       Frame Units
        SV_SIZE             DValue       Frame Units
        SV_SKIPMODE         IValue       SkipMode_e
   @endverbatim
 *   
 * @param LineMapSet
 *   Set of Line-maps to operate on.
 *   
 * @param DValue
 *   If the attribute requires a floating point value then put that value in DValue, otherwise DValue
 *   is not used.
 *   
 * @param IValue
 *   If the attribute requires an integer, enumerated value, or a handle to a string then assign it to
 *   the IValue parameter. Always typecast the IValue parameter to ArbParam_t.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapSetErrorBar(
 *   &                   Attribute,
 *   &                   LineMapSetPtr,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    POINTER         (LineMapSetPtr, LineMapSet)
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Make Error Bars red for all Line-mappings:
 *   
 * @code
 *   TecUtilLineMapSetErrorBar(SV_COLOR,NULL,0.0,(ArbParam_t)Red_C);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilLineMapSetErrorBar(const char *Attribute,
                                                                   Set_pa      LineMapSet,
                                                                   double      DValue,
                                                                   ArbParam_t  IValue);

/**
 * @deprecated
 *   Please use TecUtilLineMapSetIndices() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilXYMapSetIndices(const char *Attribute,
                                                                const char *SubAttribute,
                                                                Set_pa      XYMapSet,
                                                                ArbParam_t  IValue);
/**
 *   Assign values to attributes for index ranges in Line-plots.
 *   
 * @param Attribute
 *   Specify the attribute to change.  Set SubAttribute.
 *   
 * @param SubAttribute
 *   Specify the sub-attribute to change.  The following table shows
 *   the possible values for Attribute and SubAttribute:
 *
 *
   @verbatim
        Attribute    SubAttribute  Value Notes
        --------------------------------------------------
        SV_IJKLINES  NULL          IJKLines_e
        SV_IRANGE    SV_MIN        Min range, 0 = IMax
        SV_IRANGE    SV_MAX        Max range, 0 = IMax
        SV_IRANGE    SV_SKIP       Index skip, 0 = IMax-1
        SV_JRANGE    SV_MIN        Min range, 0 = JMax
        SV_JRANGE    SV_MAX        Max range, 0 = JMax
        SV_JRANGE    SV_SKIP       Index skip, 0 = JMax-1
        SV_KRANGE    SV_MIN        Min range, 0 = KMax
        SV_KRANGE    SV_MAX        Max range, 0 = KMax
        SV_KRANGE    SV_SKIP       Index skip, 0 = KMax-1
   @endverbatim
 *   
 * @param LineMapSet
 *   Set of Line-maps to operate on. Pass NULL to affect all Line-maps.
 *   
 * @param IValue
 *   Pass the value via the IValue prameter. Always typecast the IValue parameter to ArbParam_t.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapSetIndices(
 *   &                   Attribute,
 *   &                   SubAttribute,
 *   &                   LineMapSetPtr,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    CHARACTER*(*)   SubAttribute
 *    POINTER         (LineMapSetPtr, LineMapSet)
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Set the I-range to 0-10 for all Line-mappings:
 *   
 * @code
 *   TecUtilLineMapSetIndices(SV_IRANGE,SV_MIN,NULL,(ArbParam_t)0);
 *   TecUtilLineMapSetIndices(SV_IRANGE,SV_MAX,NULL,(ArbParam_t)10);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilLineMapSetIndices(const char *Attribute,
                                                                  const char *SubAttribute,
                                                                  Set_pa      LineMapSet,
                                                                  ArbParam_t  IValue);

/**
 * Sets the extended curve fit settings for the set of Line-maps selected in
 * the Plot Attributes dialog.
 *   
 * @param LineMapNum
 *   LineMapNum is the map number that is currently being operated on.
 *   
 * @param Settings
 *   Settings is the current settings for your extended curve fit. This string
 *   will be maintained by Tecplot and saved in layouts.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCurveSetExtendedSettings(
 *   &                   LineMapNum,
 *   &                   Settings)
 *    INTEGER*4       LineMapNum
 *    CHARACTER*(*)   Settings
 * </FortranSyntax>
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilCurveSetExtendedSettings(EntIndex_t  LineMapNum,
                                                                         const char *Settings);

/**
 * Setup the attributes for printing. Use TecUtilPrint() to do the actual
 * printing. See the $!PRINTSETUP macro command in the Tecplot Reference Manual
 * for more details on available options.
 *   
 * @param Attribute
 *   First parameter used in the $!PRINTSETUP macro command. See the Tecplot
 *   Reference Manual for the possible combinations of Attribute and
 *   SubAttribute.   The following table shows all possible values for
 *   attribute and the corresponding type:
 *
   @verbatim
        Attribute                  I or D Value     Type
        -----------------------------------------------------------------
        SV_PRINTFNAME                IValue         (char *)
        SV_PRECISION                 IValue         (SmInteger_t)
        SV_SENDPRINTTOFILE           IValue         (Boolean_t)
        SV_NUMHARDCOPYCOPIES         IValue         (SmInteger_t)
        SV_MAKEROUGHDRAFT            IValue         (Boolean_t)
        SV_LARGEPAPEROK              IValue         (Boolean_t)
        SV_DRIVER                    IValue         (PrinterDriver_e)
        SV_PALETTE                   IValue         (Palette_e)
        SV_PENSPEED                  IValue         (SmInteger_t)
        SV_PLOTTERUNITSPERINCH       DValue         (double)
        SV_WINPRINTERSMAPCOLORTOMONO IValue         (Boolean_t)
        SV_SPOOLER                   ------         Base on SubAttribute
        SV_JOBCONTROL                ------         Base on SubAttribute
        SV_NUMLIGHTSOURCESHADES      IValue         (SmInteger_t)
        SV_FORCEEXTRA3DSORTING       IValue         (Boolean_t)
        SV_PRINTRENDERTYPE           IValeu         (PrintRenderType_e)
        SV_IMAGERESOLUTION           IValue         (LgIndex_t)
   @endverbatim
 *
 *
 * @param SubAttribute
 *   Second Parameter in the $!PRINTSETUP macro command. See $!PRINTSETUP in
 *   the Tecplot Reference Manual for more information. Only used with an
 *   Attribute of SV_PLOTTERPENMAP, SV_MONOFLOODMAP, SV_MONOLINEANDTEXTMAP,
 *   SV_SPOOLER, or SV_JOBCONTROL. If SubAttribute is not used, pass NULL.  The
 *   following table shows values for SubAttribute:
 *
 *   If Attribute is SV_SPOOLER, the sent value is ALWAYS a string (thus
 *   assigned to IValue) and SubAttribute can be one of:
  
   @verbatim
      SV_HPGL2MONOSPOOLCMD
      SV_HPGL2COLORSPOOLCMD
      SV_HPGLSPOOLCMD
      SV_PSMONOSPOOLCMD
      SV_PSCOLORSPOOLCMD
      SV_LGSPOOLCMD
   @endverbatim
 *  
 *    If Attribute is SV_JOBCONTROL, the sent value is ALWAYS a string (thus
 *    assigned to IValue) and SubAttribute can be one of:
 * 
   @verbatim
      SV_HPGLMOPUPSTR
      SV_HPGL2MOPUPSTR
      SV_POSTMOPUPSTR
      SV_LGMOPUPSTR
      SV_HPGLSETUPSTR
      SV_HPGL2SETUPSTR
      SV_POSTSETUPSTR
      SV_LGSETUPSTR
   @endverbatim
 *   
 * @param DValue
 *   If the Attribute/SubAttribute requires a double or float value, pass it
 *   here. Otherwise, use 0.0.
 *   
 * @param IValue
 *   If the Attribute/SubAttribute requires a value other than a double or
 *   float value (that is, an integer, enumerated type, boolean value, or a
 *   string), pass it here. Otherwise, use zero. Always typecast the IValue
 *   parameter to (ArbParam_t).The possible values for an Attribute of
 *   SV_DRIVER are PD_HPGL, PD_HPGL2, PD_PS, PD_LASERG, PD_EPS, and
 *   PD_WINDOWS.The possible values for an Attribute of SV_PALETTE are
 *   Palette_Monochrome, Palette_PenPlotter, and Palette_Color.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPrintSetup(
 *   &                   Attribute,
 *   &                   SubAttribute,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    CHARACTER*(*)   SubAttribute
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *
 * Change the type of print output to be postscript:
 *
 * @code
 *   SetValueReturnCode_e SVRC;
 *
 *   SVRC = TecUtilPrintSetup(SV_DRIVER,
 *                            NULL,
 *                            0.0,  // Not used
 *                            (ArbParam_t)PD_PS);
 * @endcode
 *
 *
 * Change the name of the output file for print commands to be "myprint.ps"
 *
 * @code
 *   SetValueReturnCode_e SVRC;
 *
 *   SVRC = TecUtilPrintSetup(SV_PRINTFNAME,
 *                            NULL,
 *                            0.0,  // Not used
 *                            (ArbParam_t)"myprint.ps");
 * @endcode
 *
 * Set the print spooler command for color output to be "lpr -hpcolor \@"
 *
 * @code
 *   SetValueReturnCode_e SVRC;
 *
 *   SVRC = TecUtilPrintSetup(SV_SPOOLER,
 *                            SV_PSCOLORSPOOLCMD,
 *                            0.0,  // Not used
 *                            (ArbParam_t)"lpr -hpcolor @");
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilPrintSetup(const char *Attribute,
                                                           const char *SubAttribute,
                                                           double      DValue,
                                                           ArbParam_t  IValue);









/**
 * Set up all attributes related to exporting a plot. Use TecUtilExport() to do
 * the actual exporting.  See the $!EXPORTSETUP macro command in Tecplot
 * Reference Manual for more details on available options.
 *   
 * @param Attribute
 *   SubParameter of the $!EXPORTSETUP macro. See $!EXPORTSETUP in the Tecplot
 *   Reference Manual for more information. The posible values are listed in
 *   the Attribute column below, as well as whether the DValue or IValue
 *   parameter is used and what kind of information is expected in the DValue
 *   or IValue parameter:
 
   @verbatim
           Attribute                        Assign To:    Value Notes
           --------------------------------------------------------
           SV_EXPORTFNAME                   IValue        string
           SV_EXPORTFORMAT                  IValue        ExportFormat_e
           SV_MAKEROUGHDRAFT                IValue        Boolean_t
           SV_PALETTE                       IValue        Palette_e
           SV_SUNRASTERFORMAT               IValue        SunRaster_e
           SV_EXPORTREGION                  IValue        ExportRegion_e
           SV_EPSPREVIEWIMAGE               IValue        See SubAttribute
           SV_GRAYSCALEDEPTH                IValue        SmInteger_t
           SV_IMAGEWIDTH                    IValue        ScreenDim_t
           SV_CONVERTTO256COLORS            IValue        Boolean_t
           SV_ANIMATIONSPEED                DValue        double
           SV_USEMULTIPLECOLORTABLES        IValue        Boolean_t
           SV_EXTRASORTFOR3DPLOTS           IValue        Boolean_t
           SV_USESUPERSAMPLEANTIALIASING    IValue        Boolean_t
           SV_SUPERSAMPLEFACTOR             IValue        SmInteger_t
           SV_TIFFBYTEORDER                 IValue        TIFFByteOrder_e
           SV_JPEGENCODING                  IValue        JPEGEncoding_e
           SV_QUALITY                       DValue        double
           SV_FLASHIMAGETYPE                IValue        FlashImageType_e
           SV_FLASHCOMPRESSIONTYPE          IValue        FlashCompressionType_e
           SV_RESIZEFILTER                  IValue        ImageResizeFilter_e
   @endverbatim


 *   
 * @param SubAttribute
 *   SubParameter of the $!EXPORTSETUP macro command. See $!EXPORTSETUP in the
 *   Tecplot Reference Manual for more information. Only used with an Attribute
 *   of SV_EPSPREVIEWIMAGE. If SubAttribute is not used, pass NULL. Available
 *   options are listed in the SubAttribute column below, as well as whether
 *   the DValue or IValue parameter is used and what kind of information is
 *   expected in the DValue or IValue parameter: For an Attribute of
 *   SV_EPSPREVIEWIMAGE the possible values are:
 *
   @verbatim
           Attribute                Assign to:       Value Notes
           --------------------------------------------------------
           SV_IMAGETYPE             IValue           EPSPreviewImage_e
           SV_IMAGEWIDTH            IValue           Must be > 0
           SV_IMAGEHEIGHT           IValue           Must be > 0
           SV_GRAYSCALEDEPTH        IValue           Must be > 0
   @endverbatim
 *   
 * @param DValue
 *   If the Attribute/SubAttribute requires a double or float value pass it
 *   here. Otherwise, use 0.0.
 *   
 * @param IValue
 *   If the Attribute/SubAttribute requires a value other than double or float
 *   value (that is, an integer, enumerated type, boolean value, or a string),
 *   pass it here. Otherwise, use 0.0. Always typecast the IValue parameter to
 *   (ArbParam_t).
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExportSetup(
 *   &                   Attribute,
 *   &                   SubAttribute,
 *   &                   DValue,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    CHARACTER*(*)   SubAttribute
 *    REAL*8          DValue
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Export an EPS file with a TIFF preview image to the file "file1.eps." Then, export a PostScript
 *   Image file with a 1.25 scale factor to the file "file1.ps."
 *   
 * @code
 *   TecUtilExportSetup(SV_EXPORTFNAME, NULL,
 *                      0.0, (ArbParam_t)"file1.eps");
 *   TecUtilExportSetup(SV_EXPORTFORMAT, NULL,
 *                      0.0, (ArbParam_t)ExportFormat_EPS);
 *   TecUtilExportSetup(SV_EPSPREVIEWIMAGE, SV_IMAGETYPE,
 *                      0.0, (ArbParam_t)Image_TIFF);
 *   TecUtilExport();
 *   
 *   TecUtilExportSetup(SV_EXPORTFNAME, NULL,
 *                      0.0, (ArbParam_t)"file1.ps");
 *   TecUtilExportSetup(SV_EXPORTFORMAT, NULL,
 *                      0.0, (ArbParam_t)ExportFormat_PSImage);
 *   TecUtilExportSetup(SV_EXPORTSCALEFACTOR, NULL,
 *                      1.25, (ArbParam_t)0);
 *   TecUtilExport();
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilExportSetup(const char *Attribute,
                                                            const char *SubAttribute,
                                                            double      DValue,
                                                            ArbParam_t  IValue);
/**
 * @deprecated
 *   Please use TecUtilLinkingSetValue() instead.
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilFrameSetLinking(const char  *Attribute,
                                                                ArbParam_t   IValue);
/**
 * Convenience function for setting a frame linking attribute. This function in
 * turn calls TecUtilStyleSetLowLevel().
 *   
 * @param Attribute
 *   Valid values: SV_BETWEENFRAMES and SV_WITHINFRAME
 *   
 * @param SubAttribute
 *   Attribute to set. For Attribute SV_BETWEENFRAMES the subattribute must be one of
 *   SV_LINKCONTOURLEVELS, SV_LINKFRAMESIZEANDPOSITION, SV_LINKXAXISRANGE, SV_LINKYAXISRANGE,
 *   SV_LINK3DVIEW, SV_LINKGROUP. For Attribute SV_WITHINFRAME the subattribute must be one of
 *   SV_LINKAXISSTYLE, SV_LINKGRIDLINESTYLE, SV_LINKLAYERLINECOLOR, SV_LINKLAYERLINEPATTERN
 *   
 * @param IValue
 *   If Attribute is SV_LINKGROUP then this is the group number otherwise this
 *   is set to TRUE or FALSE
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLinkingSetValue(
 *   &                   Attribute,
 *   &                   SubAttribute,
 *   &                   IValuePtr)
 *    CHARACTER*(*)   Attribute
 *    CHARACTER*(*)   SubAttribute
 *    POINTER         (IValuePtr, IValue)
 * </FortranSyntax>
 *   
 *   Turn on linking for contour levels.
 *   
 * @code
 *   TecUtilLinkingSetValue(SV_BETWEENFRAMES,
 *             SV_LINKCONTOURLEVELS,TRUE);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilLinkingSetValue(const char  *Attribute,
                                                                const char  *SubAttribute,
                                                                ArbParam_t   IValue);

/**
 * Refresh the current tecplot colormap.  This must be called after setting
 * basic color RGB values to apply the changes to the installed colormap.
 *
 * @since
 *   10.0-3-127
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilColorMapRefresh()
 * </FortranSyntax>
 *
 *
 * Change the RGB values for basic color "Custom9" to be 85,50,99:
 * @code
 * {
 *   SetValueReturnCode_e SVRC;
 *   ArgList_pa           ArgList;
 *
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendString(ArgList,   SV_P1,      SV_BASICCOLOR);
 *   TecUtilArgListAppendString(ArgList,   SV_P2,      SV_CUSTOM9);
 *   TecUtilArgListAppendString(ArgList,   SV_P3,      SV_R);
 *   TecUtilArgListAppendArbParam(ArgList, SV_IVALUE,  85);
 *   SVRC = TecUtilStyleSetLowLevelX(ArgList);
 *
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendString(ArgList,   SV_P1,      SV_BASICCOLOR);
 *   TecUtilArgListAppendString(ArgList,   SV_P2,      SV_CUSTOM9);
 *   TecUtilArgListAppendString(ArgList,   SV_P3,      SV_G);
 *   TecUtilArgListAppendArbParam(ArgList, SV_IVALUE,  50);
 *   SVRC = TecUtilStyleSetLowLevelX(ArgList);
 *
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendString(ArgList,   SV_P1,      SV_BASICCOLOR);
 *   TecUtilArgListAppendString(ArgList,   SV_P2,      SV_CUSTOM9);
 *   TecUtilArgListAppendString(ArgList,   SV_P3,      SV_B);
 *   TecUtilArgListAppendArbParam(ArgList, SV_IVALUE,  99);
 *   SVRC = TecUtilStyleSetLowLevelX(ArgList);
 *
 *   TecUtilColorMapRefresh();
 *
 *   TecUtilArgListDealloc(&ArgList);
 * }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilColorMapRefresh(void);

/**
 * Convenience function used to set the color map in Tecplot to one of the base
 * color map types.
 *   
 * @param BaseColorMap
 *   Color map type to be set as the base for the global contour color map. The
 *   possible values are: ColorMap_SmRainbow, ColorMap_LgRainbow,
 *   ColorMap_Modern, ColorMap_GrayScale, ColorMap_Wild, ColorMap_UserDef,
 *   ColorMap_TwoColor or ColorMap_RawUserDef.
 *   
 * @return
 *   The setvalue return code (of type \ref SetValueReturnCode_e).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilColorMapSetBase(BaseColorMap)
 *    INTEGER*4 BaseColorMap
 * </FortranSyntax>
 *   
 *   To set the current Tecplot color map to be the large rainbow color map:
 *   
 * @code
 *   TecUtilColorMapSetBase(ColorMap_LgRainbow);
 * @endcode
 */
LINKTOADDON SetValueReturnCode_e STDCALL TecUtilColorMapSetBase(ContourColorMap_e BaseColorMap);

/**
 * @deprecated
 *   No substitute.
 */
LINKTOADDON void STDCALL TecUtilDialogSetLaunchPosition(Widget            W,
                                                        int               DialogWidth,
                                                        int               DialogHeight,
                                                        AnchorAlignment_e DialogAnchor,
                                                        LgIndex_t         IOffset,
                                                        LgIndex_t         JOffset);
