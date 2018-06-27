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
#if defined TECUTILMMODULE
#define EXTERN
#else
#define EXTERN extern
#endif

/**
 *   Allocate an argument list. Argument lists are used with all of the "X" (extended) functions in
 *   Tecplot.
 *   
 * @return
 *   Returns an ArgList object if successful, otherwise NULL.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilArgListAlloc(ResultPtr)
 *    POINTER (ResultPtr, Result)
 * </FortranSyntax>
 */
LINKTOADDON ArgList_pa STDCALL TecUtilArgListAlloc(void);
/**
 *   Clear all arguments from an argument list.
 *   
 * @return
 *   Returns the allocated argument list.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilArgListClear(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilArgListClear(ArgList_pa ArgList);
/**
 *   Append an integer parameter to an argument list.
 *   
 * @param ArgList
 *   The argument list.
 *   
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *   
 * @param Value
 *   Value associated with the parameter name.
 *   
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendInt(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   Value)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    INTEGER*4       Value
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendInt(ArgList_pa   ArgList,
                                                       const char  *Name,
                                                       LgIndex_t    Value);
/**
 *   Append an ArbParam_t type parameter to an argument list
 *   
 * @param ArgList
 *   The argument list.
 *   
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *   
 * @param Value
 *   Value associated with the parameter name.
 *   
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendArbParam(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   ValuePtr)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendArbParam(ArgList_pa  ArgList,
                                                            const char *Name,
                                                            ArbParam_t  Value);
/**
 *   Appends an ArbParam_t pointer parameter to an argument list.
 *   
 * @param ArgList
 *   The argument list
 *   
 * @param Name
 *   Parameter Name. Use the SV_ constants listed with the extended function.
 *   
 * @param Value
 *   Pointer value associated with the parameter name
 *   
 * @return
 *   Returns TRUE is append is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendArbParamPtr(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   ValuePtr)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendArbParamPtr(ArgList_pa  ArgList,
                                                               const char *Name,
                                                               ArbParam_t *Value);
/**
 *   Append a double parameter to an argument list.
 *   
 * @param ArgList
 *   The argument list.
 *   
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *   
 * @param Value
 *   Value associated with the parameter name.
 *   
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendDouble(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   Value)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    REAL*8          Value
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendDouble(ArgList_pa  ArgList,
                                                          const char *Name,
                                                          double      Value);
/**
 * Appends a double pointer parameter to an argument list.
 *   
 * @param ArgList
 *   The argument list
 *   
 * @param Name
 *   Parameter Name. Use the SV_ constants listed with the extended function
 *   
 * @param Value
 *   Pointer value associated with the parameter name
 *   
 * @return
 *   Returns TRUE is append is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendDoublePtr(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   Value)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    REAL*8          Value
 * </FortranSyntax>
 *   
 * See TecUtilStyleGetLowLevelX() for a complete example.
 *   
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendDoublePtr(ArgList_pa  ArgList,
                                                             const char *Name,
                                                             double     *Value);
/**
 *   Append a string to an argument list.
 *   
 * @param ArgList
 *   The argument list.
 *   
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *   
 * @param Value
 *   String value associated with the parameter name.
 *   
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendString(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   Value)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    CHARACTER*(*)   Value
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendString(ArgList_pa  ArgList,
                                                          const char *Name,
                                                          const char *Value);
/**
 *   Appends a named array to the argument list.
 *   
 * @param ArgList
 *   The argument list.
 *   
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *   
 * @param Value
 *   Value associated with the parameter name.
 *   
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendArray(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   Value)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendArray(ArgList_pa  ArgList,
                                                         const char *Name,
                                                         const void *Value);
/**
 *   Append a named set to an argument list.
 *   
 * @param ArgList
 *   The argument list.
 *   
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *   
 * @param Value
 *   Set associated with the parameter name.
 *   
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendSet(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   ValuePtr)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendSet(ArgList_pa  ArgList,
                                                       const char *Name,
                                                       Set_pa      Value);
/**
 *   Append a named string list to an argument list.
 *   
 * @param ArgList
 *   The argument list.
 *   
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *   
 * @param StringList
 *   String List associated with the parameter name.
 *   
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendStringList(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   StringListPtr)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    POINTER         (StringListPtr, StringList)
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendStringList(ArgList_pa     ArgList,
                                                              const char    *Name,
                                                              StringList_pa  StringList);
/**
 *   Append a named function pointer to an argument list.
 *   
 * @param ArgList
 *   The argument list.
 *   
 * @param Name
 *   Parameter name. Use the SV_constants listed with the extended function.
 *   
 * @param Value
 *   Function pointer associated with the parameter name.
 *   
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilArgListAppendFunction(
 *   &                   ArgListPtr,
 *   &                   Name,
 *   &                   Value)
 *    POINTER         (ArgListPtr, ArgList)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t  STDCALL TecUtilArgListAppendFunction(ArgList_pa  ArgList,
                                                            const char *Name,
                                                            const void *Value);
/**
 *   Deallocate an argument list.
 *   
 * @param ArgList
 *   The argument list.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilArgListDealloc(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilArgListDealloc(ArgList_pa *ArgList);

/**
 *   Redistribute the control points for the currently active color map so they are evenly spaced
 *   across the color spectrum. This is equivalent to clicking Redistribute Control Points in the
 *   Workspace/Color Map dialog. Note that this does not change the RGB values assigned at each
 *   control point.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilColorMapRedistControlPts()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilColorMapRedistControlPts(void);
/**
 *   Preset either the user-defined color map or the raw user-defined color map to be a copy of one of
 *   the standard color maps. Tecplot must currently be using either the user-defined color map or the
 *   raw user-defined color map in order to use the function.
 *   
 * @param ColorMap
 *   Color map type to use as the source. The possible values are: ColorMap_SmRainbow,
 *   ColorMap_LgRainbow, ColorMap_Modern, ColorMap_GrayScale or ColorMap_TwoColor
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilColorMapCopyStandard(ColorMap)
 *    INTEGER*4 ColorMap
 * </FortranSyntax>
 *   
 *   Reset the user-defined color map to be a copy of the modern color map.
 *   
 * @code
 *   TecUtilColorMapSetBase(ColorMap_UserDef);
 *   TecUtilColorMapCopyStandard(ColorMap_Modern);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilColorMapCopyStandard(ContourColorMap_e ColorMap);
/**
 *   Redistribute the control points and reset the RGB values for the currently active color map. This
 *   is equivalent to clicking Reset on the Workspace/ColorMap dialog.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilColorMapResetToFactory()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilColorMapResetToFactory(void);

/**
 *   Redraw the current frame.
 *   
 * @param DoFullDrawing
 *   Set to FALSE to only draw a trace of the data in the current frame. Set to TRUE to do a full
 *   redraw.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilRedraw(DoFullDrawing)
 *    INTEGER*4 DoFullDrawing
 * </FortranSyntax>
 *   
 *   Do a complete redraw of the current frame:
 *   
 * @code
 *   TecUtilRedraw(TRUE);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilRedraw(Boolean_t DoFullDrawing);/* <help> "Redraw the current frame." */
/**
 *   Redraw all frames.
 *   
 * @param DoFullDrawing
 *   Set to FALSE to only draw a trace of the data in all frames. Set to TRUE to do a full redraw.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilRedrawAll(DoFullDrawing)
 *    INTEGER*4 DoFullDrawing
 * </FortranSyntax>
 *   
 *   Do a complete redraw of all frames:
 *   
 * @code
 *   TecUtilRedrawAll(TRUE);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilRedrawAll(Boolean_t DoFullDrawing);
/**
 *   Implements a double buffer action as described below by the DoubleBufferAction parameter
 *   definitions.
 *   
 * @param DoubleBufferAction
 *   Action to take. The possible values are:  \n
 *
 *   - DoubleBufferAction_Off (turns off double buffering. Use this command
 *     once at the end of a sequence of using the double buffer).  
 *   - DoubleBufferAction_On (turns on double buffering.  Use this command once
 *     at the beginning of a sequence of using the double buffer. While double
 *     buffering is turned on, all drawing is sent to the back buffer).
 *   - DoubleBufferAction_Swap (swaps the back buffer to the front.  In
 *     other words, copy the image in the back buffer to the front).
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDoubleBuffer(DoubleBufferAction)
 *    INTEGER*4 DoubleBufferAction
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilDoubleBuffer(DoubleBufferAction_e DoubleBufferAction);
/**
 *   Turn on or off all graphics drawing.
 *   
 * @param DoDrawing
 *   TRUE to turn graphics drawing on, FALSE to turn off
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDrawGraphics(DoDrawing)
 *    INTEGER*4 DoDrawing
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilDrawGraphics(Boolean_t DoDrawing);
/**
 * Terminate the execution of the Tecplot program. Note that program control
 * does return to your application.Calling TecUtilQuit() merely sends a
 * suggestion to Tecplot to quit. Other add-ons may block the actual
 * termination of Tecplot. See TecUtilQuitAddQueryCallback(). Even if Tecplot
 * does decide to quit, it will still occur at a later time. The only code that
 * follows a call to TecUtilQuit() should be one or more calls to
 * TecUtilLockFinish().
 *   
 * @return
 *   Currently TRUE is always returned.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQuit()
 * </FortranSyntax>
 *   
 *   Exit Tecplot:
 *   
 * @code
 *   TecUtilLockStart(AddOnID);
 *   TecUtilQuit();
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilQuit(void);
/**
 *   Creates a new frame.
 *   
 * @param UseSuppliedFrameSize
 *   If set to TRUE, then XYPos, Width, and Height are used to position and size the new frame.
 *   Otherwise, the frame is created with the position and size of the factory default frame
 *   
 * @param XPos
 *   X-Position (in inches) of the frame relative to the left edge of the paper. The frame is anchored
 *   in its upper left corner.
 *   
 * @param YPos
 *   Y-Position (in inches) of the frame relative to the top edge of the paper.
 *   
 * @param Width
 *   Width of the frame in inches.
 *   
 * @param Height
 *   Height of the frame in inches. Note that the Y-direction is positive in the down direction on the
 *   paper
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameCreateNew(
 *   &                   UseSuppliedFrameSize,
 *   &                   XPos,
 *   &                   YPos,
 *   &                   Width,
 *   &                   Height)
 *    INTEGER*4       UseSuppliedFrameSize
 *    REAL*8          XPos
 *    REAL*8          YPos
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *   
 *   Create two new frames:
 *   
 * @code
 *   // let Tecplot position the frame
 *   Boolean_t IsOk1 = TecUtilFrameCreateNew(FALSE,0,0,0,0);
 *   
 *   // use an absolute position 
 *   Boolean_t IsOk2 = TecUtilFrameCreateNew(TRUE, 1,1,7,7);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameCreateNew(Boolean_t UseSuppliedFrameSize,
                                                    double    XPos,
                                                    double    YPos,
                                                    double    Width,
                                                    double    Height);

/**
 *   Pop a frame (specified by name) to the top.
 *   
 * @param Name
 *   Name of the frame to pop. Use TecUtilFrameGetName() to get the name of the
 *   current frame.
 *   
 * @return
 *   TRUE if Name is a valid frame name (and thus the frame is popped).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFramePopByName(Name)
 *    CHARACTER*(*) Name
 * </FortranSyntax>
 *   
 *   Pop the frame with the name "Frame 001", and make that frame the current frame:
 *   
 * @code
 *   TecUtilFramePopByName("Frame 001");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePopByName(const char *Name);
/**
 *   Push a frame (specified by name) to the bottom.
 *   
 * @param Name
 *   Name of the frame to push. Use TecUtilFrameGetName() to get the name of the
 *   current frame
 *   
 * @return
 *   TRUE if Name is a valid frame name (and thus the frame is pushed), otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFramePushByName(Name)
 *    CHARACTER*(*) Name
 * </FortranSyntax>
 *   
 *   Push the frame with the name "Frame 001" to the bottom, and make the frame under that frame the
 *   current frame:
 *   
 * @code
 *   TecUtilFramePushByName("Frame 001");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePushByName(const char *Name);
/**
 *   Pops a frame identified by a unique ID.
 *   
 * @param UniqueID
 *   Unique ID of the frame.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFramePopByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *   
 *   Pop the current frame using a unique id.
 *   
 * @code
 *   {
 *     UniqueID_t id;
 *     TecUtilLockStart(AddOnID);
 *     id = TecUtilFrameGetUniqueID();
 *     TecUtilFramePopByUniqueID(id);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePopByUniqueID(UniqueID_t UniqueID);
/**
 *   Pushes a frame identified by a unique ID.
 *   
 * @param UniqueID
 *   Unique ID of the frame.
 *   
 * @return
 *   TRUE if successful, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFramePushByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *   
 *  Push the current frame using a unique id
 *   
 * @code
 *   {
 *     UniqueID_t id;
 *     TecUtilLockStart(AddOnID);
 *     id = TecUtilFrameGetUniqueID();
 *     TecUtilFramePushByUniqueID(id);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePushByUniqueID(UniqueID_t UniqueID);


/**
 *   Push the top (active) frame to the bottom.
 *   
 * @return
 *   TRUE if successful, FALSE if not
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFramePushTop()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePushTop(void);
/**
 * Push a frame to the bottom of the frame stack. In other words it is given
 * the frame number 1 and therefore drawn first.
 *
 * @par Note:
 *   This function requires Tecplot Version 7.5-0-6 or newer.
 *   
 * @param FrameNum
 *   Frame to be pushed. Frames are numbered 1 to numframes with frame 1 drawn
 *   first and the highest numbered frame drawn last when Redraw All is
 *   executed. See TecUtilFrameGetCount() to get the number of frames currently
 *   defined.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFramePush(FrameNum)
 *    INTEGER*4 FrameNum
 * </FortranSyntax>
 *   
 * Assuming that there are at least two frames, push the frame underlying the
 * top frame to the bottom of the frame stack.
 *   
 * @code
 *   TecUtilFramePush(TecUtilFrameGetCount() - 1);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePush(int FrameNum);
/**
 *   Pop a frame to the top (that is, make it the active frame).
 *   
 * @param FrameNum
 *   Frame to be popped. Frames are numbered 1 to numframes with frame 1 drawn
 *   first and the highest numbered frame drawn last when RedrawAll is
 *   executed. See TecUtilFrameGetCount() to get the number of frames currently
 *   defined.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFramePop(FrameNum)
 *    INTEGER*4 FrameNum
 * </FortranSyntax>
 *   
 *   Pop the frame underneath the current frame assuming that there are at least two frames.
 *   
 * @code
 *   TecUtilFramePop(TecUtilFrameGetCount() - 1);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePop(int FrameNum);
/**
 *   Pop the frame on top at a specified position on the paper.
 *   
 * @param X
 *   X-Coordinates to specify a position on the paper of the frame to be popped. X is in inches from
 *   the left edge of the paper.
 *   
 * @param Y
 *   Y-Coordinates to specify a position on the paper of the frame to be popped. Y is in inches from
 *   the top edge of the paper.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFramePopAtPosition(
 *   &                   X,
 *   &                   Y)
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *   
 *   Pop the frame currently visible at one inch from the left, two inches from the right of the paper,
 *   and make that frame the current frame:
 *   
 * @code
 *   TecUtilFramePopAtPosition(1.0, 2.0);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilFramePopAtPosition(double X,
                                                        double Y);
/**
 *   Delete the top (active) frame. If there is only one frame when this is called, a new empty frame
 *   is automatically created after this command is executed. (Thus, you can never have a workspace
 *   without at least one frame.)
 *   
 * @return
 *   TRUE is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameDeleteTop()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameDeleteTop(void);
/**
 *   Resize all frames so that they fit inside the hardclip limits of the paper.
 *   
 * @return
 *   TRUE is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameFitAllToPaper()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameFitAllToPaper(void);

/**
 *   This function gets the current background color of the frame.
 *   
 * @return
 *   Current background color of frame. The possible values are: Black_C, Blue_C, Red_C, Green_C,
 *   Cyan_C, Purple_C, Yellow_C, White_C. CustomXX_C (where XX can be any number between 1 and 64.)
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameGetBackgroundColor()
 * </FortranSyntax>
 */
LINKTOADDON ColorIndex_t STDCALL TecUtilFrameGetBackgroundColor(void);

/**
 *   Get the name of the current frame.
 *   
 * @param Name
 *   Address of character string. Space for the new name will be allocated for you. You must later free
 *   the string by using TecUtilStringDealloc().
 *   
 * @return
 *   Returns TRUE if the frame was successfully retrieved.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameGetName(
 *   &                   Name,
 *   &                   NameLength)
 *    CHARACTER*(*)   Name
 *    INTEGER*4       NameLength
 * </FortranSyntax>
 *   
 *   Get the name of the current frame:
 *   
 * @code
 *   char *name = NULL;
 *   if ( TecUtilFrameGetName(&name) )
 *     {
 *       // do something 
 *       TecUtilStringDealloc(&name);
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameGetName(char **Name);
/**
 *   Set the frame style defaults used by Tecplot when creating new frames. During normal operation,
 *   Tecplot bases the style of a new frame on the factory defaults plus any changes assigned in the
 *   Tecplot configuration file. Layout files and stylesheet files, however, rely on Tecplot basing
 *   new frames only on the factory defaults. This command is typically not used by the casual user.
 *   
 * @param StyleBase
 *   Style base used by Tecplot for all future frames. This may be set to StyleBase_Factory or
 *   StyleBase_Config
 *   
 * @return
 *   TRUE if successful, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStyleSetBase(StyleBase)
 *    INTEGER*4 StyleBase
 * </FortranSyntax>
 *   
 *   Instruct Tecplot to create all future frames based upon the factory defaults, ignoring any
 *   modifications specified in the user's tecplot.cfg file.
 *   
 * @code
 *   TecUtilStyleSetBase(StyleBase_Factory);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStyleSetBase(StyleBase_e StyleBase);


/**
 * @deprecated 
 *   Please use TecUtilDataSetDeleteZone() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneDelete(Set_pa ZoneList);


/**
 * Deletes the specified set of zones.
 *   
 * @param ZoneList
 *   Set of zones to delete.
 *   
 * @return
 *   Returns TRUE if variable were deleted, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetDeleteZone(ZoneListPtr)
 *    POINTER (ZoneListPtr, ZoneList)
 * </FortranSyntax>
 *
 * Delete zones 1,3, and 9
 * @code
 *
 *   Set_pa ZoneList;
 *
 *   Set_pa ZoneList = TecUtilSetAlloc(FALSE);
 *   if (ZoneList)
 *     {
 *       TecUtilSetAddMember(ZoneList,1,FALSE);
 *       TecUtilSetAddMember(ZoneList,3,FALSE);
 *       TecUtilSetAddMember(ZoneList,9,FALSE);
 *       if (TecUtilDataSetDeleteZone(ZoneList))
 *         {
 *           ... zones deleted successfully.   Take approp. action.
 *         }
 *       TecUtilSetDealloc(&ZoneList);
 *     }
 * @endcode
 *
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetDeleteZone(Set_pa ZoneList);






/**
 *   Deletes the specified set of variables.
 *   
 * @param VarList
 *   Set of variables to delete.
 *   
 * @return
 *   Returns TRUE if variables were deleted, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetDeleteVar(VarListPtr)
 *    POINTER (VarListPtr, VarList)
 * </FortranSyntax>
 *
 * Delete variables 1,3, and 9
 * @code
 *
 *   Set_pa VarList;
 *
 *   Set_pa VarList = TecUtilSetAlloc(FALSE);
 *   if (VarList)
 *     {
 *       TecUtilSetAddMember(VarList,1,FALSE);
 *       TecUtilSetAddMember(VarList,3,FALSE);
 *       TecUtilSetAddMember(VarList,9,FALSE);
 *       if (TecUtilDataSetDeleteVar(VarList))
 *         {
 *           ... variables deleted successfully.   Take approp. action.
 *         }
 *       TecUtilSetDealloc(&VarList);
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetDeleteVar(Set_pa VarList);






/**
 * Read one or more data files into Tecplot to form a new data set in the
 * current frame.
 *   
 * @param ReadDataOption
 *   Determine how to handle the situation where a data set already exists in
 *   the current frame. The possible values are: ReadDataOption_NewData (Remove
 *   the data set from the current frame before loading the new data set),
 *   ReadDataOption_AppendData (Append the new data to the current data set),
 *   and ReadDataOption_ReplaceData (Replace the data set in the current frame
 *   and in all frames which share the data set with the new data).
 *   
 * @param ResetStyle
 *   TRUE if you want to reset the style of the current frame, FALSE if you
 *   want to keep the same style. Only used if ReadDataOption is
 *   ReadDataOption_NewData or ReadDataOption_ReplaceData.FileNamesOr
 *   
 * @param FileNamesOrInstructions
 *   A string list containing the file names to load or the instructions to
 *   send to the data set reader (converter or loader)
 *   
 * @param DataSetReader
 *   Name of the data set reader (converter or loader). To let Tecplot load the
 *   data, use "TECPLOT."
 *   
 * @param InitialPlotType
 *   Initial PlotType for the data. Only used if ResetStyle is TRUE. To have
 *   Tecplot determine the most appropriate frame mode for the data, use
 *   Frame_Empty. The possible values are: PlotType_Automatic:
 *   PlotType_Cartesian3D, PlotType_Cartesian2D, PlotType_XYLine,
 *   PlotType_PolarLine, and PlotType_Sketch. PlotType_Automatic instructs
 *   Tecplot to choose the best frame mode.
 *   
 * @param IncludeText
 *   Set to TRUE to load any text in the data files.
 *   
 * @param IncludeGeom
 *   Set to TRUE to load any geometries in the data files.
 *   
 * @param IncludeCustomLabels
 *   Set to TRUE to load any custom labels in the data files
 *   
 * @param IncludeData
 *   Set to TRUE to load the data from the data files. Set to FALSE to only
 *   load text, geometries and/or custom labels, depending on IncludeText,
 *   IncludeGeom, and IncludeCustomLabels
 *   
 * @param CollapseZonesAndVars
 *   Set to TRUE to renumber zones and variables if any are disabled. For more
 *   information on collapsing zones and variables, see Section 5.1.1.7, "Zone
 *   and Variable List Collapsing," in the Tecplot User's Manual
 *   
 * @param ZonesToRead
 *   Set of zones to load from the data files. Use NULL to load all zones
 *   
 * @param VarLoadMode
 *   Choose to load variables by name or by their position in the data file.
 *   See Section 5.1.1.6, "Variable Loading by Position," in the Tecplot User's
 *   Manual for more information about loading variables by name or by
 *   position. If ReadDataOption is ReadDataOption_AppendData, this must be
 *   same as the mode of the data set in the current frame. Use
 *   TecUtilDataSetGetVarLoadMode() to get this information. The possible
 *   values are: VarLoadMode_ByName and VarLoadMode_ByPosition.
 *   
 * @param VarPositionList
 *   Set of variables to load from the data files. Use NULL to load all
 *   variables. Ignored if VarLoadMode is VarLoadMode_ByName
 *   
 * @param VarNameList
 *   Set of variable names to load from the data files. Use NULL to load only
 *   variable names common to all data files. Must be NULL if VarLoadMode is
 *   VarLoadMode_ByPosition. When appending to the existing data set you must
 *   supply a new VarNameList where the new VarNameList is a superset of the
 *   existing one. A VarNameList that is a superset is one that contains all
 *   the variable names currently in use Tecplot. They must be in the same
 *   position. You can add new names either at the end of the list, or as
 *   aliases in the already established positions. Use a newline character to
 *   separate aliased names.If you do not create a VarNameList that is a
 *   superset then it is indeterminant which variable is in which position for
 *   the original data
 *   
 * @param ISkip
 *   Set to 1 to load every data point in the I-direction; 2 to load every
 *   other data point, and so forth.
 *   
 * @param JSkip
 *   Same as ISkip but for J-direction.
 *   
 * @param KSkip
 *   Same as ISkip but for K-direction.
 *   
 * @return
 *   TRUE if the input parameters are valid and the data was successfully
 *   loaded, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReadDataSet(
 *   &                   ReadDataOption,
 *   &                   ResetStyle,
 *   &                   FileNamesOrInstructionsPtr,
 *   &                   DataSetReader,
 *   &                   InitialPlotType,
 *   &                   IncludeText,
 *   &                   IncludeGeom,
 *   &                   IncludeCustomLabels,
 *   &                   IncludeData,
 *   &                   CollapseZonesAndVars,
 *   &                   ZonesToReadPtr,
 *   &                   VarLoadMode,
 *   &                   VarPositionListPtr,
 *   &                   VarNameListPtr,
 *   &                   ISkip,
 *   &                   JSkip,
 *   &                   KSkip)
 *    INTEGER*4       ReadDataOption
 *    INTEGER*4       ResetStyle
 *    POINTER         (FileNamesOrInstructionsPtr, FileNamesOrInstructions)
 *    CHARACTER*(*)   DataSetReader
 *    INTEGER*4       InitialPlotType
 *    INTEGER*4       IncludeText
 *    INTEGER*4       IncludeGeom
 *    INTEGER*4       IncludeCustomLabels
 *    INTEGER*4       IncludeData
 *    INTEGER*4       CollapseZonesAndVars
 *    POINTER         (ZonesToReadPtr, ZonesToRead)
 *    INTEGER*4       VarLoadMode
 *    POINTER         (VarPositionListPtr, VarPositionList)
 *    POINTER         (VarNameListPtr, VarNameList)
 *    INTEGER*4       ISkip
 *    INTEGER*4       JSkip
 *    INTEGER*4       KSkip
 * </FortranSyntax>
 *   
 *   Good: Var names currently in Tecplot = "A" "B" "C"
 *   VarNameList is appended to read = "A" "B\nR" "C" "D"
 *
 *   Bad: Var names currently in Tecplot = "A" "B" "C"
 *   VarNameList is appended to read "A" "C" "B" "D"
 *
 *   In the good example the integrity of the original data is always maintained, that is, 
 *   the new VarNameList works for the original data as well as the new data.  Variable 2 can 
 *   be either "B" or "R".
 *
 *   In the bad example, the new VarNameList states that variable 2 must contain "C" 
 *   but "C" has already been assigned to variable 3 with the original data.
 *   Keep in mind that once variables are in Tecplot all style assignements are made based on 
 *   variable position and not by name. Thus the variable positioning is important.
 *   
 * @code
 *   StringList_pa FileNames, VarNames;
 *   FileNames = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(FileNames, "file1.plt");
 *   TecUtilStringListAppendString(FileNames, "file2.plt");
 *   VarNames  = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(VarNames,  "X");
 *   TecUtilStringListAppendString(VarNames,  "Y");
 *   TecUtilStringListAppendString(VarNames,  "P\nPress");
 *   IsOk = TecUtilReadDataSet(ReadDataOption_NewData,
 *                             TRUE,
 *                             FileNames,
 *                             "TECPLOT",
 *                             PlotType_Automatic,
 *                             TRUE, TRUE, TRUE, TRUE,
 *                             FALSE,
 *                             (Set_pa)NULL,
 *                             VarLoadMode_ByName,
 *                             (Set_pa)NULL,
 *                             VarNames,
 *                             1, 1, 1))
 *   TecUtilStringListDealloc(&FileNames);
 *   TecUtilStringListDealloc(&VarNames);
 * @endcode
 *   
 *   Read a data set consisting of file1.plt and file2.plt. Load the variables named "X", "Y", and
 *   either "P" or "Press."
 *   
 * @code
 *   StringList_pa FileNames, VarNames;
 *   FileNames = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(FileNames, "file1.plt");
 *   TecUtilStringListAppendString(FileNames, "file2.plt");
 *   VarNames  = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(VarNames,  "X");
 *   TecUtilStringListAppendString(VarNames,  "Y");
 *   TecUtilStringListAppendString(VarNames,  "P\nPress");
 *   IsOk = TecUtilReadDataSet(ReadDataOption_NewData,
 *                             TRUE,
 *                             FileNames,
 *                             "TECPLOT",
 *                             PlotType_Automatic,
 *                             TRUE, TRUE, TRUE, TRUE,
 *                             FALSE,
 *                             (Set_pa)NULL,
 *                             VarLoadMode_ByName,
 *                             (Set_pa)NULL,
 *                             VarNames,
 *                             1, 1, 1))
 *   TecUtilStringListDealloc(&FileNames);
 *   TecUtilStringListDealloc(&VarNames);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilReadDataSet(ReadDataOption_e  ReadDataOption,
                                                 Boolean_t         ResetStyle,
                                                 StringList_pa     FileNamesOrInstructions,
                                                 const char       *DataSetReader,
                                                 PlotType_e        InitialPlotType,        
                                                 Boolean_t         IncludeText,
                                                 Boolean_t         IncludeGeom,
                                                 Boolean_t         IncludeCustomLabels,
                                                 Boolean_t         IncludeData,
                                                 Boolean_t         CollapseZonesAndVars,
                                                 Set_pa            ZonesToRead,    
                                                 VarLoadMode_e     VarLoadMode,
                                                 Set_pa            VarPositionList,     
                                                 StringList_pa     VarNameList,     
                                                 LgIndex_t         ISkip,
                                                 LgIndex_t         JSkip,
                                                 LgIndex_t         KSkip);
/**
 *   Read one or more data files into Tecplot to form a new data set in the current frame.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_READDATAOPTION
 * Type:
 *   ReadDataOption_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ReadDataOption_NewData
 * Required:
 *   No
 * Notes:
 *   Determine how to handle the situation where a data set already exists in the current frame. The possible values are:
 *   
 * Name:
 *   SV_RESETSTYLE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   TRUE if you want to reset the style of the current frame, FALSE if you want to keep the same style.
 *   
 * Name:
 *   SV_FILENAMESORINSTRUCTIONS
 * Type:
 *   StringList_pa
 * Arg Function:
 *   TecUtilArgListAppendStringList()
 * Required:
 *   Yes
 * Notes:
 *   A string list containing the file names to load or the instructions to send to the data set reader (converter or loader).
 *   
 * Name:
 *   SV_DATASETREADER
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   "TECPLOT"
 * Required:
 *   No
 * Notes:
 *   Name of the data set reader (converter or loader). To let Tecplot load the data, use NULL.
 *   
 * Name:
 *   SV_INITIALPLOTFIRSTZONEONLY
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Informs Tecplot that after the data is loaded it only needs to activate the first 
 *   enabled zone for the initial plot. This option is particularly usefull if you have many 
 *   zones and want to get the data into Tecplot and the first zone drawn as fast as 
 *   possible. The inactive zones can always be activated when needed.
 *   
 * Name:
 *   SV_INITIALPLOTTYPE
 * Type:
 *   PlotType_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   PlotType_Automatic
 * Required:
 *   No
 * Notes:
 *   Initial frame plot type for the data. Only used if SV_RESETSTYLE is TRUE. To have 
 *   Tecplot determine the most appropriate frame plot type for the data, use 
 *   PlotType_Automatic (the default).
 *   
 * Name:
 *   SV_INCLUDETEXT
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to load any text, geometries, or custom labels in the data files.
 *   
 * Name:
 *   SV_INCLUDEGEOM
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 *   
 * Name:
 *   SV_INCLUDECUSTOMLABL
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 *   
 * Name:
 *   SV_INCLUDEDATA
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to load the data from the data files. Set to FALSE to only load text, 
 *   geometries and/or customer labels, depending on SV_INCLUDETEXT, SV_INCLUDEGEOM, 
 *   and SV_INCLUDECUSTOMLABELS
 *   
 * Name:
 *   SV_COLLAPSEZONESANDVARS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to renumber zones and variables if any are disabled. For more information 
 *   on collapsing zones and variables, see Section 5.1.1.7, "Zone and Variable List 
 *   Collapsing," in the Tecplot User's Manual.
 *   
 * Name:
 *   SV_ZONELIST
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Set of zones to load from the data files. Use NULL to load all zones.
 *   
 * Name:
 *   SV_VARLOADMODE
 * Type:
 *   VarLoadMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   VarLoadMode_ByPosition
 * Required:
 *   No
 * Notes:
 *   Choose to load variables by name or by their position in the data file.
 *   
 * Name:
 *   SV_VARPOSITIONLIST
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Set of variables to load from the data files. Use NULL to load all
 *   variables. Must be NULL if SV_VARLOADMODE is VarLoadMode_ByName.
 *   
 * Name:
 *   SV_VARNAMELIST
 * Type:
 *   StringList_pa
 * Arg Function:
 *   TecUtilArgListAppendStringList()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Set of variable names to laod from the data files.
 *   
 * Name:
 *   SV_ISKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   The default value of 1 loads every data point in the I-, J-, or K-directions. A value 
 *   of 2 for each loads every other data point and so forth. These values only apply 
 *   to ordered dat
 *   
 * Name:
 *   SV_JSKIP
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
 *   SV_KSKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * </ArgListTable>
 *   
 * @return
 *   TRUE if the input parameters are valid and the data was successfully loaded, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetReadX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   
 *   Read in a dataset.  Note that some arglist entries are shown 
 *   here using their default values as an example only as they
 *   normally would not have to be provided.
 * @code
 *   char tstring[500];
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   
 *   strcpy (tstring,".");
 *   
 *   #ifdef MSWIN
 *    strcat (tstring, "\\");
 *   #else
 *    strcat (tstring, "/");
 *   #endif
 *    strcat (tstring, "d5.plk");
 *   
 *   TecUtilArgListAppendInt(ArgList,       SV_READATAOPTION,  ReadDataOption_NewData);
 *   TecUtilArgListAppendInt(ArgList,       SV_RESETSTYLE,     TRUE);
 *   TecUtilArgListAppendStringList(ArgList,SV_FILENAMESORINSTRUCTIONS, tstring);
 *   TecUtilArgListAppendInt(ArgList,       SV_INITIALPLOTFIRSTZONEONLY,FALSE);
 *   TecUtilArgListAppendInt(ArgList,       SV_INITAILPLOTTYPE,PlotType_Automatic);
 *   TecUtilArgListAppendInt(ArgList,       SV_INCUDETEXT,     TRUE);
 *   TecUtilArgListAppendInt(ArgList,       SV_INCLUDEGEOM,    TRUE);
 *   TecUtilArgListAppendInt(ArgList,       SV_INCLUDECUSTOMLABLES,  TRUE);
 *   TecUtilArgListAppendInt(ArgList,       SV_INCLUDEDATA,    TRUE);
 *   TecUtilArgListAppendInt(ArgList,       SV_COLLAPSEZONESANDVARS,    FALSE);
 *   TecUtilArgListAppendInt(ArgList,       SV_ZONELIST,       NULL);
 *   TecUtilArgListAppendInt(ArgList,       SV_VARLOADMODE,    VarLoadMode_ByPosition);
 *   TecUtilArgListAppendArray(ArgList,     SV_VARPOSITIONLIST,NULL);
 *   TecUtilArgListAppendStringList(ArgList,SV_VARNAMELIST,    NULL);
 *   TecUtilArgListAppendInt(ArgList,       SV_ISKIP, 1);
 *   TecUtilArgListAppendInt(ArgList,       SV_JSKIP, 1);
 *   TecUtilArgListAppendInt(ArgList,       SV_KSKIP, 1);
 *   
 *   TecUtilDataSetReadX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetReadX(ArgList_pa ArgList);

/**
 *   Write the specified components of the current frame's data set to a file.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   Name of the data file to write.
 *   
 * Name:
 *   SV_INCLUDETEXT
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to write any text, geometries, or custom labels to the data
 *   files.
 *   
 * Name:
 *   SV_INCLUDEGEOM
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 *   
 * Name:
 *   SV_INCLUDEDATA
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to write the data to the data files. Set to FALSE to only write text and geometries, depending on SV_INCLUDETEXT and SV_INCLUDEGEOM
 *   
 * Name:
 *   SV_INCLUDEDATASHARELINKAGE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to conserve space and write the variable and connectivity linkage 
 *   to the data file wherever possible. Set to FALSE to write all the data to the 
 *   data file and loose the variable and connectivity sharing linkage for future 
 *   dataset reads of the file.
 *   
 * Name:
 *   SV_INCLUDEAUTOGENFACENEIGHBORS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to save the face neighbor connectivity to the data file. This may produce very large data files.
 *   
 * Name:
 *   SV_USEPOINTFORMAT
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to write the data file in point format and FALSE for block format.
 *   
 * Name:
 *   SV_BINARY
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to write the data in binary format and FALSE for ASCII.
 *   
 * Name:
 *   SV_ASSOCIATELAYOUTWITHDATAFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to assoicate this data file with the current layout. Set to FALSE 
 *   to write the datafile without modifying Tecplot's current data file to 
 *   layout association.
 *   
 * Name:
 *   SV_ZONELIST
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Set of zones to write to the data file. Use NULL to write all zones.
 *   
 * Name:
 *   SV_VARLIST
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Set of variables to write to the data file. Use NULL to write all variables.
 * </ArgListTable>
 *   
 * @return
 *   TRUE if the input parameters are valid and the data was successfully written, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetWriteX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetWriteX(ArgList_pa ArgList);


/**
 *   Write the data set attached to the current frame to a file.
 *   
 * @param FName
 *   File name. Must not be NULL
 *   
 * @param IncludeText
 *   Set to TRUE to include text.
 *   
 * @param IncludeGeom
 *   Set to TRUE to include geometries
 *   
 * @param IncludeCustomLabels
 *   Set to TRUE to include custom labels
 *   
 * @param IncludeData
 *   Set to TRUE to include data
 *   
 * @param ZonesToWrite
 *   Set of zones to write. Pass NULL to write all zones
 *   
 * @param VarsToWrite
 *   Set of vars to write. Pass NULL to write all variables
 *   
 * @param WriteBinary
 *   Set to TRUE to write a binary file, FALSE to write an ASCII file
 *   
 * @param UsePointFormat
 *   Valid only if WriteBinary is FALSE, ignored otherwise
 *   
 * @param AsciiPrecision
 *   Valid only if WriteBinary is FALSE, ignored otherwise
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWriteDataSet(
 *   &                   FName,
 *   &                   IncludeText,
 *   &                   IncludeGeom,
 *   &                   IncludeCustomLabels,
 *   &                   IncludeData,
 *   &                   ZonesToWritePtr,
 *   &                   VarsToWritePtr,
 *   &                   WriteBinary,
 *   &                   UsePointFormat,
 *   &                   AsciiPrecision)
 *    CHARACTER*(*)   FName
 *    INTEGER*4       IncludeText
 *    INTEGER*4       IncludeGeom
 *    INTEGER*4       IncludeCustomLabels
 *    INTEGER*4       IncludeData
 *    POINTER         (ZonesToWritePtr, ZonesToWrite)
 *    POINTER         (VarsToWritePtr, VarsToWrite)
 *    INTEGER*4       WriteBinary
 *    INTEGER*4       UsePointFormat
 *    INTEGER*4       AsciiPrecision
 * </FortranSyntax>
 *   
 *   Write out only zone 3 to a file called zone3.plt:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,3,FALSE);
 *   TecUtilWriteDataSet("zone3.plt",FALSE,FALSE,FALSE,TRUE,set,NULL,
 *                       TRUE,FALSE,0);
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilWriteDataSet(const char       *FName,      
                                                   Boolean_t         IncludeText,
                                                   Boolean_t         IncludeGeom,
                                                   Boolean_t         IncludeCustomLabels,
                                                   Boolean_t         IncludeData,
                                                   Set_pa            ZonesToWrite,    
                                                   Set_pa            VarsToWrite,
                                                   Boolean_t         WriteBinary,
                                                   Boolean_t         UsePointFormat,
                                                   SmInteger_t       AsciiPrecision);
/**
 *   Create a rectangular zone. If no data set exists when this command is executed, a data set is
 *   created with variables X, Y (and Z, if KMAX>1). If a data set exists prior to this command, the
 *   non-coordinate variables for the zone created are initialized to zero.
 *   
 * @param IMax
 *   I-Dimension of the zone to create.
 *   
 * @param JMax
 *   J-Dimension of the zone to create.
 *   
 * @param KMax
 *   K-Dimension of the zone to create.
 *   
 * @param XMin
 *   X min (occurs at I = 1) for the recangular zone.
 *   
 * @param YMin
 *   Y min (occurs at J = 1) for the recangular zone.
 *   
 * @param ZMin
 *   Z min (occurs at K = 1) for the recangular zone.
 *   
 * @param XMax
 *   X max (occurs at I = I-Max) for the recangular zone.
 *   
 * @param YMax
 *   Y max (occurs at J = J-Max) for the recangular zone.
 *   
 * @param ZMax
 *   Z max (occurs at K = K-Max) for the recangular zone.
 *   
 * @param FieldDataType
 *   Data type for the variables in the new zone. The possible choices are: FieldDataType_Float,
 *   FieldDataType_Double, FieldDataType_Int32, FieldDataType_Int16, FieldDataType_Bit,
 *   FieldDataType_Byte, or FieldDataType_Invalid.  If set to FieldDataType_Invalid, Tecplot
 *   will choose the type for you.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateRectangularZone(
 *   &                   IMax,
 *   &                   JMax,
 *   &                   KMax,
 *   &                   XMin,
 *   &                   YMin,
 *   &                   ZMin,
 *   &                   XMax,
 *   &                   YMax,
 *   &                   ZMax,
 *   &                   FieldDataType)
 *    INTEGER*4       IMax
 *    INTEGER*4       JMax
 *    INTEGER*4       KMax
 *    REAL*8          XMin
 *    REAL*8          YMin
 *    REAL*8          ZMin
 *    REAL*8          XMax
 *    REAL*8          YMax
 *    REAL*8          ZMax
 *    INTEGER*4       FieldDataType
 * </FortranSyntax>
 *   
 *   Create a 16 by 8 by 2 zone that is specified from zero to one in all directions:
 *   
 * @code
 *   Boolean_t ISOK;
 *   ISOK = TecUtilCreateRectangularZone(16,  // imax 
 *                                     8,     // jmax 
 *                                     2,     // kmax 
 *                                     0,0,0, // x,y,z min 
 *                                     1,1,1, // x,y,z max 
 *                                     FieldDataType_Float);
 * @endcode
 *
 *   FORTRAN EXAMPLE:  Create 10x10x10 zone:
 *
 * @code
 *   IErr = TecUtilCreateRectangularZone(10,10,10,
 *  &                                    0.0D0,0.0D0,0.0D0,
 *  &                                    1.0D0,1.0D0,1.0D0,
 *  &
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateRectangularZone(LgIndex_t       IMax,
                                                            LgIndex_t       JMax,
                                                            LgIndex_t       KMax,
                                                            double          XMin,
                                                            double          YMin,
                                                            double          ZMin,
                                                            double          XMax,
                                                            double          YMax,
                                                            double          ZMax,
                                                            FieldDataType_e FieldDataType);
/**
 *   Create a circular (or cylindrical) IJ- or IJK-ordered zone. If no data set exists when this
 *   command is executed, a data set is created with variables X, Y (and Z, if KMAX>1). If a data set
 *   exists prior to this command, the non-coordinate variables for the zone created are initialized
 *   to zero.
 *   
 * @param IMax
 *   I-Dimension of the zone to create. I is in the radial direction.
 *   
 * @param JMax
 *   J-Dimension of the zone to create. J is in the circumferential direction.
 *   
 * @param KMax
 *   K-Dimension of the zone to create. K is in the Z-direction. Set K to 1 to create a circle
 *   
 * @param XOrigin
 *   X-Origin for the circle or cylinder.
 *   
 * @param YOrigin
 *   Y-Origin for the circle or cylinder.
 *   
 * @param Radius
 *   Radius of the circle or cylinder
 *   
 * @param ZMin
 *   Z-Min value used when creating a cylinder
 *   
 * @param ZMax
 *   Z-Max value used when creating a cylinder
 *   
 * @param FieldDataType
 *   Data type for the variables in the new zone. The possible choices are: FieldDataType_Float,
 *   FieldDataType_Double, FieldDataType_Int32, FieldDataType_Int16, FieldDataType_Bit,
 *   FieldDataType_Byte, or FieldDataType_Invalid.  If set to FieldDataType_Invalid, Tecplot
 *   will choose the type for you.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateCircularZone(
 *   &                   IMax,
 *   &                   JMax,
 *   &                   KMax,
 *   &                   XOrigin,
 *   &                   YOrigin,
 *   &                   Radius,
 *   &                   ZMin,
 *   &                   ZMax,
 *   &                   FieldDataType)
 *    INTEGER*4       IMax
 *    INTEGER*4       JMax
 *    INTEGER*4       KMax
 *    REAL*8          XOrigin
 *    REAL*8          YOrigin
 *    REAL*8          Radius
 *    REAL*8          ZMin
 *    REAL*8          ZMax
 *    INTEGER*4       FieldDataType
 * </FortranSyntax>
 *   
 *   Create a circular 10 by 20 IJ-ordered zone centered at (5, 5) with a radius of two:
 *   
 * @code
 *   Boolean_t IsOk;
 *   
 *   IsOk = TecUtilCreateCircularZone(10,20,1,
 *                                    5.0,5.0,2.0,
 *                                    0.0,0.0,
 *                                    FieldDataType_Float);
 *   
 *   IsOk = TecUtilCreateCircularZone(5,6,8,
 *                                    4.0,4.0,3.0,
 *                                    0.0,7.0,
 *                                    FieldDataType_Float);
 * @endcode
 *   
 *   Create a cylindrical 5 by 6 by 8 IJK-ordered zone with the bottom centered at (4, 4, 0) and the
 *   top centered at (4, 4, 7) and a radius of three:
 *   
 * @code
 *   
 *   IsOk = TecUtilCreateCircularZone(5,6,8,
 *                                    4.0,4.0,3.0,
 *                                    0.0,7.0,
 *                                    FieldDataType_Float);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateCircularZone(LgIndex_t       IMax,
                                                         LgIndex_t       JMax,
                                                         LgIndex_t       KMax,
                                                         double          XOrigin,
                                                         double          YOrigin,
                                                         double          Radius,
                                                         double          ZMin,
                                                         double          ZMax,
                                                         FieldDataType_e FieldDataType);
/**
 * @deprecated 
 *   Please use TecUtilCreateSimpleZone() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSimpleXYZone(LgIndex_t        NumPoints,
                                                        const double    *XValues,
                                                        const double    *YValues,
                                                        FieldDataType_e  FieldDataType);
/**
 *   Create a new zone by specifying only a list of XY pairs of data. If other zones exist prior to
 *   using this function and there are more than two variables, then the additional variables are also
 *   created and set to zero.
 *   
 * @param NumPoints
 *   Number of XY pairs of data.
 *   
 * @param V1Values
 *   Array of X (or theta) values for the zone.
 *   
 * @param V2Values
 *   Array of Y (or R) values for the zone.
 *   
 * @param FieldDataType
 *   Data type for the variables in the new zone. The possible choices are: FieldDataType_Float,
 *   FieldDataType_Double, FieldDataType_Int32, FieldDataType_Int16, FieldDataType_Bit,
 *   FieldDataType_Byte, or FieldDataType_Invalid.  If set to FieldDataType_Invalid, Tecplot
 *   will choose the type for you.
 *   
 * @return
 *   TRUE if the zone could be created, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateSimpleZone(
 *   &                   NumPoints,
 *   &                   V1Values,
 *   &                   V2Values,
 *   &                   FieldDataType)
 *    INTEGER*4       NumPoints
 *    REAL*8          V1Values
 *    REAL*8          V2Values
 *    INTEGER*4       FieldDataType
 * </FortranSyntax>
 *   
 *   Create a zone containing three points: (0.0, 1.0), (1.0, 1.1), and (2.0, 1.3):
 *   
 * @code
 *   double xarray[3] = {0.0, 1.0, 2.0};
 *   double yarray[3] = {1.0, 1.1, 1.3);
 *   Boolean_t IsOk;
 *   IsOk = TecUtilCreateSimpleZone(3, xarray, yarray,
 *                                  FieldDataType_Float);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSimpleZone(LgIndex_t        NumPoints,
                                                      const double    *V1Values,
                                                      const double    *V2Values,
                                                      FieldDataType_e  FieldDataType);

/**
 * Get the base path of a file name. This function allocates space for and
 * creates a new string of the base path.
 *   
 * @param FName
 *   File name. A NULL file name is allowed in which case the startup directory
 *   is returned
 *   
 * @return
 *   The base path of the file name including the trailing '/' or '\.' You must
 *   call TecUtilStringDealloc() on the returned string.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGetBasePath(
 *   &           FName,
 *   &           Result,
 *   &           ResultLength)
 *    CHARACTER*(*)   FName
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *   
 *   Get the start up directory and the base path of the current layout file name:
 *   
 * @code
 *   char *FName = TecUtilGetCurLayoutFName();
 *   if (FName)
 *     {
 *       char *StartUpDir = TecUtilGetBasePath(NULL);
 *       char *BasePath = TecUtilGetBasePath(FName);
 *       //  BasePath is something like "C:\TEC75\DEMO\LAY\"  
 *       if (BasePath)
 *         {
 *           .
 *           .
 *             TecUtilStringDealloc(&BasePath);
 *         }
 *       TecUtilStringDealloc(&FName);
 *       if (StartUpDir)
 *         TecUtilStringDealloc(&StartUpDir);
 *     }
 * @endcode
 */
LINKTOADDON char * STDCALL TecUtilGetBasePath(const char *FName);


/**
 *   Write out an html file and related image files. The resulting html file, when viewed will show the
 *   current tecplot session.
 *   
 * @param FName
 *   Name of the publish file to create
 *   
 * @param IncludeLayoutPackage
 *   If TRUE, write out a layout package file as well and include a link to the file in the HTML
 *   document
 *   
 * @param ImageSelection
 *   Choose the image types to reference in the HTML document. Choose from one of
 *   ImageSelection_OnePerFrame or ImageSelection_WorkspaceOnly
 *   
 * @return
 *   Returns TRUE if publish file could be written, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPublish(
 *   &                   FName,
 *   &                   IncludeLayoutPackage,
 *   &                   ImageSelection)
 *    CHARACTER*(*)   FName
 *    INTEGER*4       IncludeLayoutPackage
 *    INTEGER*4       ImageSelection
 * </FortranSyntax>
 *   
 *   Write out a publish file to mydoc.htm. Include the package file and include a separate image per
 *   frame.
 *   
 * @code
 *      TecUtilPublish("mydoc.htm",
 *                     TRUE,
 *                     ImageSelection_OnePerFrame);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPublish(const char      *FName,
                                             Boolean_t        IncludeLayoutPackage,
                                             ImageSelection_e ImageSelection);
/**
 *   Delete all frames and data sets (clear the current layout). A blank default frame will be created
 *   for you.
 *   
 * @return
 *   Currently, TRUE is always returned.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilNewLayout()
 * </FortranSyntax>
 *   
 *   Clear the current layout:
 *   
 * @code
 *   Boolean_t IsOk = TecUtilNewLayout();
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilNewLayout(void);
/**
 *   Open and read in a new layout file.
 *   
 * @param FName
 *   The name of the layout file to open
 *   
 * @param AltInstructions
 *   Alternate instructions for the layout file. In most cases, this will be a string list containing
 *   file names which will override the data files listed in the layout file. Use NULL to load the
 *   data referenced in the layout file
 *   
 * @param Append
 *   TRUE to append the new layout file to the current layout or FALSE to replace the current layout
 *   
 * @return
 *   TRUE if successfull, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilOpenLayout(
 *   &                   FName,
 *   &                   AltInstructionsPtr,
 *   &                   Append)
 *    CHARACTER*(*)   FName
 *    POINTER         (AltInstructionsPtr, AltInstructions)
 *    INTEGER*4       Append
 * </FortranSyntax>
 *   
 *   Open a layout file called "experiment.lay." Then, append a layout called "calculate.lay,"
 *   overriding the first data file referenced in that layout with a file called "newdata.plt":
 *   
 * @code
 *   StringList_pa AltInst = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(AltInst, "newdata.plt");
 *   TecUtilOpenLayout("experiment.lay", (StringList_pa)NULL, FALSE);
 *   TecUtilOpenLayout("calculate.lay", AltInst, TRUE);
 *   TecUtilStringListDealloc(&AltInst);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilOpenLayout(const char    *FName,
                                                StringList_pa AltInstructions,
                                                Boolean_t     Append);
/**
 *   Save the current layout to a file. You must supply the file name.
 *   
 * @param FName
 *   The name of the layout file to save
 *   
 * @param UseRelativePaths
 *   Set to TRUE to make all of the files referenced by the layout file use paths relative to the
 *   current directory. Set to FALSE to make all of the files referenced by absolute paths
 *   
 * @return
 *   TRUE if the current layout was saved, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSaveLayout(
 *   &                   FName,
 *   &                   UseRelativePaths)
 *    CHARACTER*(*)   FName
 *    INTEGER*4       UseRelativePaths
 * </FortranSyntax>
 *   
 *   Save a layout file called temp.lay, using absolute paths:
 *   
 * @code
 *   Boolean_t IsOk = TecUtilSaveLayout("temp.lay", FALSE);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilSaveLayout(const char     *FName,
                                                Boolean_t UseRelativePaths);
/**
 *   Save the current layout to a file.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   Associated value is the file name of the file in which to save the layout.
 *   The file name must be a reference to a non-NULL string who's length is
 *   greater than zero. The file name reference need not be allocated however
 *   the string must not be deallocated until it is no longer referenced by the
 *   argument list.
 *   
 * Name:
 *   SV_INCLUDEDATA
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the layout should be saved as a layout
 *   package where the data is included with the style information or if it
 *   should reference linked data
 *   
 * Name:
 *   SV_INCLUDEPREVIEW
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the layout package should also include a
 *   preview image. This argument only applies if the include data option is
 *   TRUE
 *   
 * Name:
 *   SV_USERELATIVEPATHS
 * Type:
 *   Boolean_t
 * Arg Function:
 *  TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the layout should be saved using relative
 *   paths. This argument only applies if the include data option is FALSE.
 *   
 * Name:
 *   SV_POSTLAYOUTCOMMANDS
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   A character string containing a set of Tecplot macro commands that are
 *   appended to the layout or layout package file.  These can be almost
 *   anything and are generally used to store add-on specific state information
 *   (using $!ADDONCOMMAND commands).  </ArgListTable>
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSaveLayoutX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   Save a layout package named temp.lpk, without a preview image:
 *   
 * @code
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,   "temp.lpk");
 *   TecUtilArgListAppendInt(ArgList,    SV_INCLUDEDATA,    TRUE);
 *   TecUtilArgListAppendInt(ArgList,    SV_INCLUDEPREVIEW, FALSE);
 *   IsOk = TecUtilSaveLayoutX(ArgList);
 * @endcode
 *   
 *   Save a layout named temp.lay using relative path names:
 *   
 * @code
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendString(ArgList, SV_FNAME,  "temp.lpk");
 *   TecUtilArgListAppendInt(ArgList,    SV_USERELATIVEPATHS, TRUE);
 *   TecUtilArgListAppendInt(ArgList,    SV_INCLUDEPREVIEW, FALSE);
 *   IsOk = TecUtilSaveLayoutX(ArgList);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilSaveLayoutX(ArgList_pa ArgList);

/**
 *   Read a stylesheet file into the current frame.
 *   
 * @param FName
 *   The name of the stylesheet file to read
 *   
 * @param IncludePlotStyle
 *   Set to TRUE to process commands related to the style of the plot (that is, mesh color, vector
 *   type, etc.)
 *   
 * @param IncludeText
 *   Set to TRUE to load any text in the stylesheet
 *   
 * @param IncludeGeom
 *   Set to TRUE to load any geometries in the stylesheet
 *   
 * @param IncludeStreamPositions
 *   Set to TRUE to load any streamtrace starting positions in the stylesheet
 *   
 * @param IncludeContourLevels
 *   Set to TRUE to load any contour level information in the stylesheet.
 *   
 * @param MergeStyle
 *   Set to TRUE to merge the current frame style with the new stylesheet. Set to FALSE to reset the
 *   style of the current frame back to factory defaults before reading in the stylesheet
 *   
 * @param IncludeFrameSizeAndPosition
 *   Set to TRUE to resize and position the current frame to the specifications of the stylesheet file.
 *   Set to FALSE to keep the current frame's size and position
 *   
 * @return
 *   TRUE if the input parameters are valid and the specified style was successfully loaded, FALSE
 *   otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReadStylesheet(
 *   &                   FName,
 *   &                   IncludePlotStyle,
 *   &                   IncludeText,
 *   &                   IncludeGeom,
 *   &                   IncludeStreamPositions,
 *   &                   IncludeContourLevels,
 *   &                   MergeStyle,
 *   &                   IncludeFrameSizeAndPosition)
 *    CHARACTER*(*)   FName
 *    INTEGER*4       IncludePlotStyle
 *    INTEGER*4       IncludeText
 *    INTEGER*4       IncludeGeom
 *    INTEGER*4       IncludeStreamPositions
 *    INTEGER*4       IncludeContourLevels
 *    INTEGER*4       MergeStyle
 *    INTEGER*4       IncludeFrameSizeAndPosition
 * </FortranSyntax>
 *   
 *   Load in the stylesheet file1.sty, choosing to reset the style of the current frame back to factory
 *   defaults first. Next, load only text and geometries from the stylesheet file2.sty, adding them to
 *   the current style:
 *   
 * @code
 *   Boolean_t IsOk;
 *   IsOk = TecUtilReadStylesheet("file1.sty", TRUE, TRUE, TRUE,
 *                                TRUE, TRUE, FALSE, TRUE);
 *   IsOk = TecUtilReadStylesheet("file2.sty", FALSE, TRUE, TRUE,
 *                                FALSE, FALSE, TRUE, FALSE);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilReadStylesheet(const char *FName,
                                                    Boolean_t   IncludePlotStyle,
                                                    Boolean_t   IncludeText,
                                                    Boolean_t   IncludeGeom,
                                                    Boolean_t   IncludeStreamPositions,
                                                    Boolean_t   IncludeContourLevels,
                                                    Boolean_t   MergeStyle,    
                                                    Boolean_t   IncludeFrameSizeAndPosition);

/**
 *   Write the style for the current frame to a file.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *   
 * Name:
 *   SV_INCLUDECONTOURLEVELS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to include contour levels
 *   
 * Name:
 *   SV_INCLUDETEXT
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to include text
 *   
 * Name:
 *   SV_INCLUDEAUXDATA
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to include auxiliary data
 *   
 * Name:
 *   SV_INCLUDEGEOM
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to include geometries
 *   
 * Name:
 *   SV_INCLUDEPLOTSTYLE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   --------------------------
 *   
 * Name:
 *   SV_COMPRESS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to compress the stylesheet file
 *   
 * Name:
 *   SV_INCLUDESTREAMPOSITIONS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to include stream positions
 *   
 * Name:
 *   SV_INCLUDEFACTORYDEFAULTS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to include factory defaults
 *   
 * Name:
 *   SV_USERELITIVEPATHS
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to TRUE to save with relative paths for any image geometries in the frame
 * </ArgListTable>
 *   
 * @return
 *   TRUE if successfully, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWriteStylesheetX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   Save a stylesheet "temp.sty" with no text objects included:
 *   
 * @code
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendString(ArgList,SV_FNAME,"temp.sty");
 *   TecUtilArgListAppendString(ArgList,SV_INCLUDETEXT,FALSE);
 *   IsOk = TecUtilWriteStyleSheetX(ArgList);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilWriteStylesheetX(ArgList_pa ArgList);


/**
 *   Write the style for the current frame to a file.
 *   
 * @param FName
 *   File name. Must not be NULL
 *   
 * @param IncludePlotStyle
 *   Set to TRUE to include the plot style
 *   
 * @param IncludeText
 *   Set to TRUE to include text
 *   
 * @param IncludeGeom
 *   Set to TRUE to include geometries
 *   
 * @param IncludeStreamPositions
 *   Set to TRUE to include stream positions
 *   
 * @param IncludeContourLevels
 *   Set to TRUE to include contour levels
 *   
 * @param IncludeFactoryDefaults
 *   Set to TRUE to include factory defaults
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWriteStylesheet(
 *   &                   FName,
 *   &                   IncludePlotStyle,
 *   &                   IncludeText,
 *   &                   IncludeGeom,
 *   &                   IncludeStreamPositions,
 *   &                   IncludeContourLevels,
 *   &                   IncludeFactoryDefaults)
 *    CHARACTER*(*)   FName
 *    INTEGER*4       IncludePlotStyle
 *    INTEGER*4       IncludeText
 *    INTEGER*4       IncludeGeom
 *    INTEGER*4       IncludeStreamPositions
 *    INTEGER*4       IncludeContourLevels
 *    INTEGER*4       IncludeFactoryDefaults
 * </FortranSyntax>
 *   
 *   Write the style for the current frame to the file f1.sty:
 *   
 * @code
 *   TecUtilWriteStylesheet("f1.sty",TRUE,TRUE,TRUE,TRUE,TRUE,TRUE);.
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilWriteStylesheet(const char *FName,
                                                     Boolean_t   IncludePlotStyle,
                                                     Boolean_t   IncludeText,
                                                     Boolean_t   IncludeGeom,
                                                     Boolean_t   IncludeStreamPositions,
                                                     Boolean_t   IncludeContourLevels,
                                                     Boolean_t   IncludeFactoryDefaults);
/**
 *   Load a color map file.
 *   
 * @param FName
 *   Name of the color map file to load
 *   
 * @return
 *   Returns TRUE if the FName was successfully loaded as a color map, otherwise, FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReadColorMap(FName)
 *    CHARACTER*(*) FName
 * </FortranSyntax>
 *   
 *   Load the color map file temp.map.
 *   
 * @code
 *   TecUtilReadColorMap("temp.map");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilReadColorMap(const char *FName);
/**
 *   Assign the RGB values that define the Raw User-Defined color map. This
 *   does not set the color map to use the Raw User-Defined color map. Use
 *   TecUtilColorMapSetBase() to assign the color map.
 *   
 * @param NumRawRGBValues
 *   The number of RGB values to use to define the Raw User-Defined color map.
 *   This must be less than MaxRawColorMapEntries.
 *   
 * @param RawRValues_Array
 *   An array of size NumRawRGBValues which contains the red values to define
 *   the Raw User-Defined color map.
 *   
 * @param RawGValues_Array
 *   An array of size NumRawRGBValues which contains the green values to define
 *   the Raw UserDefined color map.
 *   
 * @param RawBValues_Array
 *   An array of size NumRawRGBValues which contains the blue values to define
 *   the Raw User-Defined color map.
 *   
 * @return
 *   TRUE if successful, FALSE if an invalid number of RGB values was requested.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilRawColorMap(
 *   &                   NumRawRGBValues,
 *   &                   RawRValues_Array,
 *   &                   RawGValues_Array,
 *   &                   RawBValues_Array)
 *    INTEGER*4       NumRawRGBValues
 *    INTEGER*4       RawRValues_Array
 *    INTEGER*4       RawGValues_Array
 *    INTEGER*4       RawBValues_Array
 * </FortranSyntax>
 *   
 *   Define the Raw User-Defined color map to be five RGB values: blue, cyan, green, yellow, and red
 *   and assign Tecplot to use the Raw User-Defined color map.
 *   
 * @code
 *   
 *   ColorIndex_t RValues[5];
 *   ColorIndex_t GValues[5]
 *   ColorIndex_t BValues[5];
 *   
 *   RValues[0] = 0; GValues[0] = 0; BValues[0] = 255;
 *   RValues[1] = 0; GValues[1] = 255; BValues[1] = 255;
 *   RValues[2] = 0; GValues[2] = 255; BValues[2] = 0;
 *   RValues[3] = 255; GValues[3] = 255; BValues[3] = 0;
 *   RValues[4] = 255; GValues[4] = 0; BValues[4] = 0;
 *   
 *   TecUtilRawColorMap(5, RValues, GValues, BValues);
 *   TecUtilColorMapSetBase(ColorMap_RawUserDef);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilRawColorMap(int           NumRawRGBValues,
                                                 ColorIndex_t *RawRValues_Array,
                                                 ColorIndex_t *RawGValues_Array,
                                                 ColorIndex_t *RawBValues_Array);
/**
 *   Write the current color map to a file.
 *   
 * @param FName
 *   File name. Must not be NULL
 *   
 * @return
 *   TRUE if the color map was successfully written, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWriteColorMap(FName)
 *    CHARACTER*(*) FName
 * </FortranSyntax>
 *   
 *   Write the current color map to a file. Mycolors.map is the file name:
 *   
 * @code
 *   TecUtilWriteColorMap("mycolors.map");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilWriteColorMap(const char *FName);

/**
 * Export an image file from Tecplot. See TecUtilExportSetup() for details on
 * settup the exported image type, file name and so forth.
 *   
 * @param Append
 *   Append the image data. This can only be set to TRUE if the export format
 *   is a Raster Metafile or AVI. However, it is recommended that you use
 *   TecUtilExportStart(), TecUtilExportNextFrame(), and TecUtilExportFinish()
 *   for Raster Metafile and AVI formats. This value must be FALSE for all
 *   other formats.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExport(Append)
 *    INTEGER*4 Append
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilExport(Boolean_t Append);
/**
 *   Change the view in the workspace so the currently selected frames (that
 *   is, the frames with picked handles) are fit just inside the edges of the
 *   workspace.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewFitSelectFrames()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewFitSelectFrames(void);
/**
 *   Change the view in the workspace so all frames are fit just inside the edges of the workspace.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewFitAllFrames()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewFitAllFrames(void);
/**
 *   Change the view in the workspace so the entire paper is fit just inside the edges of the
 *   workspace.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewFitPaper()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewFitPaper(void);
/**
 *   Temporarily expand the workspace as large as possible. In the Motif version of Tecplot, the
 *   maximized workspace occupies the entire Tecplot process window. In the Windows version, the
 *   maximized workspace expands to fill the entire screen.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewMaximize()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewMaximize(void);
/**
 *   Return to the previous workspace view.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewLastView()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewLastView(void);
/**
 *   Change the view into the workspace. This has no effect on the local view within any frame in your
 *   layout.
 *   
 * @param X1
 *   X min Corner of the rectangle on the paper to be viewed.
 *   
 * @param Y1
 *   Y min Corner of the rectangle on the paper to be viewed.
 *   
 * @param X2
 *   X max Corner of the rectangle on the paper to be viewed.
 *   
 * @param Y2
 *   Y max Corner of the rectangle on the paper to be viewed.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewZoom(
 *   &                   X1,
 *   &                   Y1,
 *   &                   X2,
 *   &                   Y2)
 *    REAL*8          X1
 *    REAL*8          Y1
 *    REAL*8          X2
 *    REAL*8          Y2
 * </FortranSyntax>
 *   
 *   Make the region in the lower left corner of an 8.5 by 11 paper viewable in the work area. The
 *   paper is in portrait orientation:
 *   
 * @code
 *   TecUtilWorkViewZoom(0.0,5.5,4.25,9.75);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewZoom(double X1, 
                                                  double Y1, 
                                                  double X2, 
                                                  double Y2);
/**
 *   Shift the view of the workspace. This has no effect on the local view within any frame in your
 *   layout.
 *   
 * @param X
 *   Amount to translate in the X-Direction. Value is in inches.
 *   
 * @param Y
 *   Amount to translate in the Y-Direction. Value is in inches.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilWorkViewTranslate(
 *   &                   X,
 *   &                   Y)
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *   
 *   Shift the workspace view to the left by two inches (as measured by the workspace ruler):
 *   
 * @code
 *   TecUtilWorkViewTranslate(-2.0,0.0);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilWorkViewTranslate(double X,
                                                       double Y);

/**
 * Instruct Tecplot to push the current view onto the view stack associated
 * with the frame mode for the current frame. A view will not be pushed if the
 * current view is the same as the top view on the stack. Functions
 * TecUtilViewAxisFit(), TecUtilViewCenter(), TecUtilViewDataFit(),
 * TecUtilViewFit(), and TecUtilViewZoom() automatically push a view onto the
 * stack. Tecplot automatically pushes the current view onto the stack when a
 * $!Redraw command or TecUtilRedraw() is issued and the current view is
 * different from the top view on the view stack.
 *
 * @par Note:
 *   Views pushed with this command are only retrieved using TecUtilViewLast().
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewPush()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewPush(void);
/**
 *   Retrieve the view from the view paste buffer and assign it to the current frame.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewPaste()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewPaste(void);
/**
 * Copy the view from the current frame to the view paste buffer. This view can
 * later be pasted into any frame that uses the same frame mode as the current
 * frame.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewCopy()
 * </FortranSyntax>
 *
 * @sa TecUtilViewPaste()
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewCopy(void);
/**
 *   Retrieve the previous view from the view stack. Each frame mode within each frame maintains its
 *   own view stack.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewLast()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewLast(void);
/**
 *   Change the view by "zooming" into the image. Tecplot will adjust the ranges on the axis to view
 *   the region defined by the rectangle with corners at (X1, Y1) and (X2, Y2). X1, Y1, X2, and Y2 are
 *   defined in the units of the X- and Y-axis (that is, grid coordinates).
 *   
 * @param X1
 *   X min Corner of the rectangle to be viewed.
 *   
 * @param Y1
 *   Y min Corner of the rectangle to be viewed.
 *   
 * @param X2
 *   X max Corner of the rectangle to be viewed.
 *   
 * @param Y2
 *   Y max Corner of the rectangle to be viewed.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewZoom(
 *   &                   X1,
 *   &                   Y1,
 *   &                   X2,
 *   &                   Y2)
 *    REAL*8          X1
 *    REAL*8          Y1
 *    REAL*8          X2
 *    REAL*8          Y2
 * </FortranSyntax>
 *   
 *   Zoom so the rectangular region with corners at (1, 0) and (7, 9) are in view:
 *   
 * @code
 *   TecUtilViewZoom(1.0,0.0,7.0,9.0);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewZoom(double X1, 
                                              double Y1, 
                                              double X2, 
                                              double Y2);
/**
 *   Shift the image in the X- and/or Y-direction. The amount translated is in frame units.
 *   
 * @param X
 *   Amount to shift the data in the X-direction.
 *   
 * @param Y
 *   Amount to shift the data in the Y-direction.
 *   
 * @return
 *   TRUE if successful, FALSE if not
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewTranslate(
 *   &                   X,
 *   &                   Y)
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *   
 *   Translate the view ten percent of the frame width to the right:
 *   
 * @code
 *   TecUtilViewTranslate(10.0,0.0);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewTranslate(double X,
                                                   double Y);

/**
 *   Center the data within the axis grid area.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewCenter()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewCenter(void);
/**
 * @deprecated 
 *   Please use TecUtilViewSetMagnification() instead.
 */


LINKTOADDON Boolean_t STDCALL TecUtilViewScale(double Scale);
/**
 *   Scale (magnify) the view by a given value. Set the magnification factor for the current view
 *   relative to a view where the data fit to full frame size.
 *   
 * @param Magnification
 *   Set the magnification to this value.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewSetMagnification(Magnification)
 *    REAL*8 Magnification
 * </FortranSyntax>
 *   
 *   Scale the view to ten percent of the size at which the data would fit the full frame:
 *   
 * @code
 *   TecUtilViewSetMagnification(0.10);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewSetMagnification(double Magnification);
/**
 *   Reset the range on a specific axis so that it equals the minimum and maximum of the data being
 *   plotted. If the axis dependency is not independent then this action may also affect the range on
 *   another axis.
 *   
 * @param Axis
 *   Valid values are 'X' or 'Y', or 'Z'
 *   
 * @param AxisNum
 *   If the frame mode is XY, then this can be a number between one and five. Otherwise, this must be
 *   set to one
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewAxisFit(
 *   &                   Axis,
 *   &                   AxisNum)
 *    CHARACTER*(*)   Axis
 *    INTEGER*4       AxisNum
 * </FortranSyntax>
 *   
 *   Reset the range on the Y-axis to fit the data being plotted:
 *   
 * @code
 *   TecUtilViewAxisFit('Y',1);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewAxisFit(char Axis,
                                                 short AxisNum);
/**
 *   Fit the data being plotted within the axis grid area. This does not take into consideration text
 *   or geometries.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewDataFit()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewDataFit(void);
/**
 *   Fit the data being plotted within the axis grid area. This also takes into consideration text and
 *   geometries that are plotted using the grid coordinate system.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewFit()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewFit(void);
/**
 *   Fit the data being plotted within the axis grid area leaving a determined border around the edge
 *   of the plot to make in more visually appealing. This also takes into consideration text and
 *   geometries that are plotted using the grid coordinate system.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewNiceFit()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewNiceFit(void);
/**
 *   Give a border to the current view.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewMakeCurViewNice()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewMakeCurViewNice(void);
/**
 *   Reset the range on a specific axis so that it equals a little more than the minimum and maximum of
 *   the data being plotted allowing a small border to enhance visual appeal. If the axis dependency
 *   is not independent then this action may also affect the range on another axis.
 *   
 * @param Axis
 *   Valid values are 'X' or 'Y', or 'Z'.
 *   
 * @param AxisNum
 *   If the frame mode is XY, then this can be a number between one and five. Otherwise, this must be
 *   set to one
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewAxisNiceFit(
 *   &                   Axis,
 *   &                   AxisNum)
 *    CHARACTER*(*)   Axis
 *    INTEGER*4       AxisNum
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewAxisNiceFit(char Axis, 
                                                     short AxisNum);
/**
 *   Makes the curently viewed axis values visually appealing.
 *   
 * @param Axis
 *   Valid values are 'X' or 'Y', or 'Z'.
 *   
 * @param AxisNum
 *   If the frame mode is XY, then this can be a number between one and five. Otherwise, this must be
 *   set to one
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewAxisMakeCurValsNice(
 *   &                   Axis,
 *   &                   AxisNum)
 *    CHARACTER*(*)   Axis
 *    INTEGER*4       AxisNum
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewAxisMakeCurValsNice(char Axis, 
                                                             short AxisNum);
/**
 *   Reset the ranges on the 3-D axes. The current frame mode must be 3D to use this function.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReset3DAxes()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilReset3DAxes(void);
/**
 *   Recalculate the scale factors for the 3-D axes. Aspect ratio limits are taken into account. The
 *   current frame must be in 3D frame mode to use this function.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReset3DScaleFactors()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilReset3DScaleFactors(void);
/**
 * Print the current layout to a printer or send the print instructions to a
 * file. Use the TecUtilPrintSetup() function to configure printing.
 *   
 * @return
 *   Returns TRUE if the printing completed successfully. A FALSE return value
 *   indicates that the user pressed cancel during printing or that the
 *   printing could not complete.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPrint()
 * </FortranSyntax>
 *   
 *   Setup to print to a grayscale PostScript file called "print.ps" with black flooding appearing as a
 *   light gray and then do the printing:
 *   
 * @code
 *   TecUtilPrintSetup("PALETTE", NULL,
 *                     0.0, (ArbParam_t)Palette_Monochrome);
 *   TecUtilPrintSetup("DRIVER", NULL,
 *                     0.0, (ArbParam_t)PD_PS);
 *   TecUtilPrintSetup("PRINTFNAME", NULL,
 *                     0.0, (ArbParam_t)"print.ps");
 *   TecUtilPrintSetup("MONOFLOODMAP", "BLACKSHADE",
 *                     90.0, (ArbParam_t)0);
 *   TecUtilPrint();
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPrint(void);

/**
 *   Pick an object at a specified location. The location is defined as the (X,Y) location in inches
 *   relative to the top-left edge of the paper. See Section 17.4, "The Pick List," in the ADK User's
 *   Manual for a discussion of pick lists.
 *   
 * @param X
 *   X-location (in inches) relative to the left edge of the paper
 *   
 * @param Y
 *   Y-location (in inches) relative to the top edge of the paper
 *   
 * @param CollectingObjects
 *   If FALSE, the list of picked objects is cleared before the attempt is made to add a new object. If
 *   TRUE, the new object is added to the list of picked objects. (This is like holding Shift down
 *   while selecting objects in the workspace.) If CollectingObjects is TRUE, DiggingForObjects must
 *   be FALSE
 *   
 * @param DiggingForObjects
 *   If TRUE, attempt to pick objects below any currently picked objects at this location. If FALSE,
 *   pick the top object at this location. (This is like holding Ctrl down while selecting objects in
 *   the workspace. It allows you to select objects which are beneath other objects.) If
 *   DiggingForObjects is TRUE, CollectingObjects must be FALSE
 *   
 * @return
 *   Currently, TRUE is always returned.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAtPosition(
 *   &                   X,
 *   &                   Y,
 *   &                   CollectingObjects,
 *   &                   DiggingForObjects)
 *    REAL*8          X
 *    REAL*8          Y
 *    INTEGER*4       CollectingObjects
 *    INTEGER*4       DiggingForObjects
 * </FortranSyntax>
 *   
 *   Select the object at the location (1.5, 3.2). Then, select the object underneath that object
 *   instead. Add the object at (5.5, 5.4) to the list of picked objects:
 *   
 * @code
 *   Boolean_t IsOk;
 *   IsOk = TecUtilPickAtPosition(1.5, 3.2, FALSE, FALSE);
 *   IsOk = TecUtilPickAtPosition(1.5, 3.2, FALSE, TRUE);
 *   IsOk = TecUtilPickAtPosition(5.5, 5.4, TRUE,  FALSE);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAtPosition(double X, 
                                                    double Y, 
                                                    Boolean_t CollectingObjects, 
                                                    Boolean_t DiggingForObjects);

/**
 *   Deselect all selected objects. See Section 17.4, "The Pick List," in the ADK User's Manual for a
 *   discussion of pick lists.
 *   
 * @return
 *   Currently TRUE is always returned.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickDeselectAll()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickDeselectAll (void);

/**
 * Adds the frame identified by the unique identifier to the pick list. Each
 * Tecplot frame is assigned a unique ID that can be obtained for the current
 * frame by calling TecUtilFrameGetUniqueID().
 *   
 * @param CollectingObjects
 *   Set to TRUE to add frames identified by the unique ID to the pick list. If
 *   FALSE, all objects previously picked are first removed from the pick list.
 *   
 * @param UniqueID
 *   Unique ID of the frame.
 *   
 * @return
 *   TRUE if the pick was successful, FALSE otherwise.
 *
 * @sa TecUtilFrameGetUniqueID()
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAddFrameByUniqueID(
 *   &                   CollectingObjects,
 *   &                   UniqueID)
 *    INTEGER*4 CollectingObjects
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *   
 * Add the current frame to a new pick list.
 *   
 * @code
 *   {
 *     UniqueID_t FrameID;
 *     Boolean_t Picked;
 *     TecUtilLockStart(AddOnID);
 *     FrameID = TecUtilFrameGetUniqueID();
 *     Picked  = TecUtilPickAddFrameByUniqueID(FALSE, FrameID);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddFrameByUniqueID(Boolean_t  CollectingObjects,
                                                            UniqueID_t UniqueID);

/**
 *   Adds the specified Line-maps to the pick object list. In addition, you can control whether the
 *   Line-maps are added to the list of already picked objects or if the pick object list is cleared
 *   prior to adding them.
 *   
 * @param CollectingObjects
 *   Set to TRUE to add identified linemaps to the pick list. If FALSE, all objects previously picked
 *   are first removed from the pick list.
 *   
 * @param ZoneSet
 *   Set of zones to add to the pick list.
 *   
 * @return
 *   Returns TRUE if the zones could be picked, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAddZones(
 *   &                   CollectingObjects,
 *   &                   ZoneSetPtr)
 *    INTEGER*4       CollectingObjects
 *    POINTER         (ZoneSetPtr, ZoneSet)
 * </FortranSyntax>
 *   
 *   Pick zones 1 and 3
 *   
 * @code
 *   Set_pa ZoneSet;
 *   ZoneSet = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(ZoneSet,1,TRUE);
 *   TecUtilSetAddMember(ZoneSet,3,TRUE);
 *   TecUtilPickAddZones(TRUE,ZoneSet);
 *   TecUtilSetDealloc(&ZoneSet);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddZones(Boolean_t CollectingObjects,
                                                  Set_pa    ZoneSet);


/**
 * @deprecated
 *   Please use TecUTilPickAddLineMaps() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddXYMaps(Boolean_t CollectingObjects,
                                                   Set_pa    XYMapsSet);



/**
 *   Adds the specified Line-maps to the pick object list. In addition, you can control whether the
 *   Line-maps are added to the list of already picked objects or if the pick object list is cleared
 *   prior to adding them.
 *   
 * @param CollectingObjects
 *   Set to TRUE to add identified linemaps to the pick list. If FALSE, all objects previously picked
 *   are removed from the pick list
 *   
 * @param LineMapSet
 *   Set of Line-maps to add to the pick list
 *   
 * @return
 *   Returns TRUE if the linemaps could be picked, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAddLineMaps(
 *   &                   CollectingObjects,
 *   &                   LineMapSetPtr)
 *    INTEGER*4       CollectingObjects
 *    POINTER         (LineMapSetPtr, LineMapSet)
 * </FortranSyntax>
 *   
 *   Pick Line-maps 1 and 3
 *   
 * @code
 *   Set_pa LineMapSet;
 *   LineMapSet = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(LineMapSet,1,TRUE);
 *   TecUtilSetAddMember(LineMapSet,3,TRUE);
 *   TecUtilPickAddLineMaps(TRUE,LineMapSet);
 *   TecUtilSetDealloc(&LineMapSet);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddLineMaps(Boolean_t CollectingObjects,
                                                     Set_pa    LineMapSet);



/**
 *   Add all objects of a certain type to the pick list. See Section 17.4, "The Pick List," in the ADK
 *   User's Manual for a discussion of pick lists.
 *   
 * @param ObjectType
 *   The type of object to select. Except for PickObject_Frame, all objects are selected in the current
 *   frame. The possible values are: PickObject_Text (Text) PickObject_Geom (Geometries)
 *   PickObject_Frame (Frames) PickObject_Zone (Zones) PickObject_LineMapping (X-Y Mappings)
 *   PickObject_StreamtracePosition (Streamtraces starting positions)
 *   
 * @return
 *   Returns TRUE if any objects of the specified type were found.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAddAll(ObjectType)
 *    INTEGER*4 ObjectType
 * </FortranSyntax>
 *   
 *   Select all text and geometries in the current frame:
 *   
 * @code
 *   Boolean_t TextsExist, GeomsExist;
 *   TecUtilPickDeselectAll();
 *   TextsExist = TecUtilPickAddAll(PickObject_Text);
 *   GeomsExist = TecUtilPickAddAll(PickObject_Geom);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddAll(PickObjects_e ObjectType);



/**
 *   Add all objects defined within a specified rectangle to the pick list. The rectangle is defined by
 *   the two corner points (X1, Y1) and (X2, Y2) in inches relative to the top left edge of the paper.
 *   An optional filter can be defined which specifies more details about which objects to select. See
 *   Section 17.4, "The Pick List," in the ADK User's Manual for a discussion of pick lists.
 *   
 * @param X1
 *   X-Value for Top left corner of the rectangle defining the region in which to select objects.
 *   (Specify in inches relative to the top left corner of the paper.)
 *   
 * @param Y1
 *   Y-Value for Top left corner.
 *   
 * @param X2
 *   X-Value for Lower right corner of the rectangle defining the region in which to select objects.
 *   (Specify in inches relative to the top left corner of the paper.)
 *   
 * @param Y2
 *   Y-Value for Lower right corner.
 *   
 * @param ObjectType
 *   The type of object to select within the specified region. Except for PickObject_Frame, all objects
 *   are selected in the current frame within the specified rectangle. The possible values are:
 *   PickObject_Text,PickObject_Geom, PickObject_Frame, PickObject_Zone, PickObject_LineMapping
 *   PickObject_StreamtracePosition, PickObject_Axis, PickObject_ContourLabel.
 *
 *   
 * @param Filter
 *   An optional character string which specifies more details about which objects to select within the
 *   specified rectangle. If no filter is desired, NULL can be used. The filter uses the same text
 *   which is used in the macro language for the $!PICK ADDALLINRECT macro command. The keywords are
 *   COLORFILTER=\<color>, LINEPATTERNFILTER=\<linepattern>, FONTFILTER=\<font>, GEOMFILTER=\<geomtype>.
 *   The filter string can include one or more of these keywords. You can seperate keywords with
 *   spaces or '\\n'. See the Tecplot Reference Manual for more information about the filter.
 *   
 * @return
 *   Returns TRUE if any objects specified by ObjectType and Filter were found within the defined
 *   rectangle.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickAddAllInRect(
 *   &                   X1,
 *   &                   Y1,
 *   &                   X2,
 *   &                   Y2,
 *   &                   ObjectType,
 *   &                   Filter)
 *    REAL*8          X1
 *    REAL*8          Y1
 *    REAL*8          X2
 *    REAL*8          Y2
 *    INTEGER*4       ObjectType
 *    CHARACTER*(*)   Filter
 * </FortranSyntax>
 *   
 *   In the current frame, within the rectangle bounded by (1.5, 1.5) and (7.5, 8.0), select red text,
 *   geometries with a dashed line pattern, and all contour labels:
 *   
 * @code
 *   Boolean_t RedTextsExist, DashedGeomsExist, ContLabelsExist;
 *   RedTextsExist =
 *     TecUtilPickAddAllInRect(1.5, 1.5, 7.5, 8.0,
 *                             PickObject_Text,
 *                             "COLORFILTER = RED");
 *   DashedGeomsExist =
 *     TecUtilPickAddAllInRect(1.5, 1.5, 7.5, 8.0,
 *                             PickObject_Geom,
 *                             "LINEPATTERNFILTER = DASHED");
 *   ContLabelsExist =
 *     TecUtilPickAddAllInRect(1.5, 1.5, 7.5, 8.0,
 *                             PickObject_ContourLabel,
 *                             NULL);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickAddAllInRect(double         X1,
                                                      double         Y1,
                                                      double         X2,
                                                      double         Y2,
                                                      PickObjects_e  ObjectType,
                                                      const char    *Filter);
/**
 * Perform a global edit operation on the currently picked objects. Only one
 * edit operation is allowed per call to TecUtilPickEdit(). Objects are edited
 * only if the supplied parameter is relevant. See Section 17.4, "The Pick
 * List," in the ADK User's Manual for a discussion of pick lists.
 *   
 * @param Action
 *   The action to perform on the currently picked objects. The action uses the
 *   same text which is used in the macro language for the $!PickEdit macro
 *   command. See the Tecplot Reference Manual for more information about
 *   available keywords
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickEdit(Action)
 *    CHARACTER*(*) Action
 * </FortranSyntax>
 *   
 *   Set the color for all picked objects to yellow.  
 *   Set the contour type for all picked zones to be "Flood".
 * @code
 *   TecUtilPickEdit("COLOR = YELLOW");
 *   TecUtilPickEdit("CONTOUR {COUNTOURTYPE = FLOOD}");
 * @endcode
 *   
 * @code
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickEdit(const char *Action);
/**
 *   Copy all objects that are currently picked to the paste buffer and then delete them from the plot.
 *   
 * @return
 *   TRUE if successful, FALSE if not or if no objects are currently picked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickCut()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickCut(void);
/**
 *   Copy all objects that are currently picked to the paste buffer.
 *   
 * @return
 *   TRUE if successful, FALSE if not or if no objects are currently picked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickCopy()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickCopy(void);
/**
 *   Clear (that is, delete) all objects that are currently picked. These objects cannot be retrieved.
 *   
 * @return
 *   TRUE if any objects were deleted.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickClear()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickClear(void);
/**
 *   Paste the currently picked objects from the paste buffer to the workspace.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickPaste()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickPaste(void);
/**
 *   Shift the currently picked objects. Objects are shifted relative to their
 *   starting position. X and Y shift amounts are in paper units (inches). If
 *   snapping is in effect then it is applied after shifting in X and Y.
 *   
 * @param DXPaper
 *   Shift amount in the X-direction. Units are in inches. Positive numbers
 *   will shift objects to the right. Ignored if PointerStyle is
 *   PointerStyle_BottomBracket, PointerStyle_TopBracket, or
 *   PointerStyle_UpDown
 *   
 * @param DYPaper
 *   Shift amount in the Y-direction. Units are in inches. Positive numbers
 *   will shift objects down.  Ignored if PointerStyle is
 *   PointerStyle_LeftBracket, PointerStyle_RightBracket, or
 *   PointerStyle_LeftRight.
 *   
 * @param PointerStyle
 *   The type of shift to perform on the picked objects. This is ignored if not
 *   appropriate for the currently selected objects. PointerStyle_AllDirections
 *   is appropriate for all types of objects. Currently, the other options are
 *   only available for Axis and Frame objects.  See PointerStyle_e.
 *   
 * @return
 *   TRUE if successful, FALSE if no items are selected or if a zone or
 *   Line-mapping is selected and the current mouse mode is not the Adjustor
 *   tool. Objects can only be selected if the current mouse mode is the
 *   selector or the Adjustor tool.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickShift(
 *   &                   DXPaper,
 *   &                   DYPaper,
 *   &                   PointerStyle)
 *    REAL*8          DXPaper
 *    REAL*8          DYPaper
 *    INTEGER*4       PointerStyle
 * </FortranSyntax>
 *   
 *   Select all frames and resize their upper-left corners to be 2.5 inches to
 *   the right and 3.5 inches down from their previous location. Then, select
 *   all text in the current frame and move them 1.5 inches to the left and
 *   0.25 inches up from their previous locations:
 * @code
 *   TecUtilPickSetMouseMode(Mouse_Select);
 *   if (TecUtilPickAddAll(PickObject_Frame))
 *     TecUtilPickShift(2.5, 3.5, PointerStyle_UpperLeftBracket);
 *   TecUtilPickDeselectAll();
 *   if (TecUtilPickAddAll(PickObject_Text))
 *     TecUtilPickShift(-1.5, -0.25, PointerStyle_AllDirections);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickShift(double DXPaper, 
                                               double DYPaper, 
                                               PointerStyle_e PointerStyle);
/**
 *   Magnify all picked objects. The first object picked will remain in place; the other picked objects
 *   will move proportionally as the image is magnified. Only frames, text, and geometries are allowed
 *   to be magnified.
 *   
 * @param MagFactor
 *   Amount to magnify
 *   
 * @return
 *   TRUE if picked objects could be magnified, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickMagnify(MagFactor)
 *    REAL*8 MagFactor
 * </FortranSyntax>
 *   
 *   Double the size of all picked objects.
 *   
 * @code
 *   Boolean_t IsOk = TecUtilPickMagnify(2.0);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickMagnify(double MagFactor);
/**
 *   Change the order in which objects are drawn by pushing the currently picked objects back. Only
 *   frames, text, geometries, and the grid area for 2-D plots are allowed to be pushed.
 *   
 * @return
 *   TRUE if all currently picked objects are allowed to be pushed, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickPush()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickPush(void);
/**
 *   Change the order in which objects are drawn by popping the currently picked objects to the front.
 *   Only frames, text, geometries, and the grid area for 2-D plots are allowed to be popped.
 *   
 * @return
 *   TRUE if all currently picked objects are allowed to be popped, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickPop()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickPop(void);


/**
 * @deprecated 
 *   Please use TecUtilMouseSetMode() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickSetMouseMode(MouseButtonMode_e MouseMode);






/**
 * @deprecated
 *   Please use TecUtilLineMapCopy() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapCopy(EntIndex_t SourceMap,
                                               EntIndex_t DestMap);
/**
 *   Copy attributes from an existing Line-mapping to another.
 *   
 * @param SourceMap
 *   Existing Line-map to copy. Must be greater than or equal to one and less than or equal to the
 *   current number of maps
 *   
 * @param DestMap
 *   Destination map. Must be greater than or equal to one and less than or equal to the current number
 *   of maps plus one
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapCopy(
 *   &                   SourceMap,
 *   &                   DestMap)
 *    INTEGER*4       SourceMap
 *    INTEGER*4       DestMap
 * </FortranSyntax>
 *   
 *   Duplicate the first Line-map:
 *   
 * @code
 *   Boolean_t IsOk = TecUtilLineMapCopy(1,2);
 *   if (!IsOk)
 *   {
 *    // function NOT successful 
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapCopy(EntIndex_t SourceMap,
                                                 EntIndex_t DestMap);


/**
 * @deprecated
 *   Please use TecUtilLineMapCreate() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapCreate(void);



/**
 *   Create a new Line-mapping.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise .
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapCreate()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapCreate(void);



/**
 * @deprecated
 *   Please use TecUtilLineMapDelete() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapDelete(Set_pa MapsToDelete);



/**
 *   Delete one or more Line-maps.
 *   
 * @param MapsToDelete
 *   Set of maps to delete
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapDelete(MapsToDeletePtr)
 *    POINTER (MapsToDeletePtr, MapsToDelete)
 * </FortranSyntax>
 *   
 *   Delete maps 1 and 3:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   if (set)
 *   {
 *     TecUtilSetAddMember(set,1,TRUE);
 *     TecUtilSetAddMember(set,3,TRUE);
 *     TecUtilLineMapDelete(set);
 *     TecUtilSetDealloc(&set);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapDelete(Set_pa MapsToDelete);



/**
 * @deprecated
 *   Please use TecUtilLineMapShiftToTop() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapShiftToTop(Set_pa MapsToShift);



/**
 *   Shift a list of Line-maps to the top of the Line-map list. This in effect causes the selected
 *   Line-maps to be drawn first.
 *   
 * @param MapsToShift
 *   Set of maps to shift. Must not be NULL
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapShiftToTop(MapsToShiftPtr)
 *    POINTER (MapsToShiftPtr, MapsToShift)
 * </FortranSyntax>
 *   
 *   Shift Line-maps 2 and 4 to the top:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,2,FALSE);
 *   TecUtilSetAddMember(set,4,FALSE);
 *   TecUtilLineMapShiftToBottom(set);
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapShiftToTop(Set_pa MapsToShift);


/**
 * @deprecated
 *   Please use TecUtilLineMapShiftToBottom() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapShiftToBottom(Set_pa MapsToShift);



/**
 *   Shift a list of Line-maps to the bottom of the Line-map list. This in effect causes the selected
 *   Line-maps to be drawn last.
 *   
 * @param MapsToShift
 *   Set of maps to shift. Must not be NULL
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapShiftToBottom(MapsToShiftPtr)
 *    POINTER (MapsToShiftPtr, MapsToShift)
 * </FortranSyntax>
 *   
 *   Shift Line-maps 2 and 4 to the bottom:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,2,FALSE);
 *   TecUtilSetAddMember(set,4,FALSE);
 *   TecUtilLineMapShiftToBottom(set);
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapShiftToBottom(Set_pa MapsToShift);



/**
 * @deprecated
 *   Please use TecUtilViewRotate3D() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewRotate(RotateAxis_e  RotateAxis,
                                                double        RotateAmountInDegrees);




/**
 *   Do a 3-D rotation about a given axis.
 *   
 * @param RotateAxis
 *   Axis to rotate about. Valid values are: RotateAxis_X, RotateAxis_Y, RotateAxis_Z, RotateAxis_Psi,
 *   RotateAxis_Theta, RotateAxis_Alpha, RotateAxis_Twist, RotateAxis_VertRollerBall,
 *   RotateAxis_HorzRollerBall, RotateAxis_AboutVector
 *   
 * @param RotateAmountInDegrees
 *   Amount to rotate in degrees
 *   
 * @param VectorX
 *   X-Component for the vector to rotate about when RotateAxis_AboutVector is specified. At least one
 *   of VectorX, VectorY, or VectorZ must be non-zero.
 *   
 * @param VectorY
 *   Y-Component for the vector to rotate about when RotateAxis_AboutVector is specified.
 *   
 * @param VectorZ
 *   Z-Component for the vector to rotate about when RotateAxis_AboutVector is specified.
 *   
 * @param RotateOriginLocation
 *   Specifies the location of the rotation origin for the rotation. Valid values are
 *   RotateOriginLocation_DefinedOrigin or RotateOriginLocation_Viewer. Use
 *   RotateOriginLocation_DefinedOrigin to rotate about the currently defined rotation origin. Use
 *   RotateOriginLocation_Viewer to rotate about the currently defined viewer position
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilViewRotate3D(
 *   &                   RotateAxis,
 *   &                   RotateAmountInDegrees,
 *   &                   VectorX,
 *   &                   VectorY,
 *   &                   VectorZ,
 *   &                   RotateOriginLocation)
 *    INTEGER*4       RotateAxis
 *    REAL*8          RotateAmountInDegrees
 *    REAL*8          VectorX
 *    REAL*8          VectorY
 *    REAL*8          VectorZ
 *    INTEGER*4       RotateOriginLocation
 * </FortranSyntax>
 *   
 *   Rotate about the Psi-Axis by 25 degrees at the currently defined rotation origin.
 *   
 * @code
 *   TecUtilViewRotate3D(RotateAxis_Psi,
 *                                 25.0,
 *                                 0.0,
 *                                 0.0,
 *                                 0.0,
 *                                 RotateOriginLocation_DefinedOrigin);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilViewRotate3D(RotateAxis_e           RotateAxis,
                                                  double                 RotateAmountInDegrees,
                                                  double                 VectorX,
                                                  double                 VectorY,
                                                  double                 VectorZ,
                                                  RotateOriginLocation_e RotateOriginLocation);
/**
 *   Reposition the rotation origin in 3-D to be at the centroid of the data. The current frame must be
 *   in 3D mode to use this function.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReset3DOrigin()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilReset3DOrigin(void);
/**
 *   Sets the distance the viewer is from the plane that is parallel to the screen and passes through
 *   the 3-D rotation origin.
 *   
 * @param EyeDistance
 *   The distance from the 3-D rotation origin to set the viewer
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSet3DEyeDistance(EyeDistance)
 *    REAL*8 EyeDistance
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilSet3DEyeDistance(double EyeDistance);
/**
 *   Reposition the rotation origin in 3-D to the specified location. The current frame must be in 3D
 *   frame mode to use this function.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_ORIGINRESETLOCATION
 * Type:
 *   OriginResetLocation_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   OriginResetLocation_DataCenter
 * Required:
 *   No
 * Notes:
 *   Associated value indicates the location to which the origin should be
 *   reset. Valid values are: OriginResetLocation_DataCenter and
 *   OriginResetLocation_ViewCenter
 * </ArgListTable>
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilReset3DOriginX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   Reset the origin location to the center of the current view.
 *   
 * @code
 *   Boolean_t IsOk = TRUE;
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,
 *                           SV_ORIGINRESETLOCATION,
 *                           OriginResetLocation_ViewCenter);
 *   IsOk = TecUtilReset3DOriginX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilReset3DOriginX(ArgList_pa ArgList);

/**
 *   Reset the length of the vectors. Tecplot will find the vector with the largest magnitude and set
 *   the scaling factor so it will appear on the screen using the default length as specified in the
 *   configuration file with the $!FRAMESETUP VECTDEFLEN macro command. The current frame must be in
 *   3D frame mode to use this function.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilResetVectorLength()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilResetVectorLength(void);



/**
 * Modify the contour levels.
 *   
 * @param ArgList
 *   Set of Arglist entries. This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_CONTOURLEVELACTION
 * Type:
 *   ContourLevelAction_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The contour level action you want to take.  See Table below for 
 *   more details.
 *   
 * Name:
 *   SV_CONTOURGROUP
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Number of the contour group to be modified.
 *   
 * Name:
 *   SV_NUMVALUES
 * Type:
 *   int
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   Number of contour levels added, or number of new contour levels.
 *   
 * Name:
 *   SV_APPROXNUMVALUES
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   The approx number of new contour levels. The actual number will be
 *   adjusted slightly to give "nice" values of the contour variable at the
 *   levels. Use this when SV_CONTOURLEVELACTION is set to
 *   ContourLevelAction_ResetToNice.
 *   
 * Name:
 *   SV_RAWDATA
 * Type:
 *   double *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Required:
 *   Yes, for ContourLevelAction_Add and ContourLevelAction_New.
 * Notes:
 *   A pointer to an aray containing the values of the contour variable at each
 *   of the added levels or at each of the new levels.
 *   
 * Name:
 *   SV_RANGEMIN
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   The value of the contour variable at the minimum end of the range to be
 *   deleted, or the value of the contour variable near the level to be
 *   deleted. Use when SV_CONTOURLEVELACTION is set to
 *   ContourLevelAction_DeleteRange.
 *   
 * Name:
 *   SV_RANGEMAX
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   The value of the contour variable at the maximum end of the range to be
 *   deleted.  Use when SV_CONTOURLEVELACTION is set to
 *   ContourLevelAction_DeleteRange.
 * </ArgListTable>
 *
 * @par SV_CONTOURLEVELACTION
   @verbatim
        SV_CONTOURLEVELACTION      Description
        ------------------------------------------------------------------------
        ContourLevelAction_Add     Add a new set of contour levels to the existing 
                                   set of contour levels. You may specify the number 
                                   of the contour group being modified (SV_CONTOURGROUP)
                                   and the number of contour levels being added 
                                   (SV_ NUMVALUES). You must specify the value of the
                                   contour variable at each of the added levels 
                                   (SV_RAWDATA).

        ContourLevelAction_New     Replace the current set of contour levels with a 
                                   new set. You may specify the number of the
                                   contour group being modified (SV_CONTOURGROUP) and 
                                   the number of contours levels in the new set 
                                   (SV_NUMVALUES). You must specify the value of the 
                                   contour variable at each of the new levels 
                                   (SV_RAWDATA).

        ContourLevelAction_        
        DeleteRange                Delete all contour levels between the specified 
                                   minimum and maximum values of the contour variable 
                                   (inclusive). You may specify the number of the 
                                   contour group being modified (SV_CONTOURGROUP), 
                                   the value of the contour variable at the minimum 
                                   end of the range (SV_RANGEMIN), and the value of 
                                   the contour variable at the maximum end of the 
                                   range (SV_RANGEMAX).

        ContourLevelAction_Reset   Reset the contour levels to a set of evenly 
                                   distributed values spanning the entire range of 
                                   the currently selected contouring variable. You may
                                   specify the number of the contour group being 
                                   modified (SV_CONTOURGROUP) and the number of 
                                   contours levels in the new set (SV_NUMVALUES).

        ContourLevelAction_
        ResetToNice                Reset the contour levels to a set of evenly 
                                   distributed values that approximately spans the 
                                   range of the currently selected contouring variable.
                                   Exact range and number of levels will be adjusted 
                                   to make contour levels have "nice" values. You 
                                   may specify the number of the contour group being 
                                   modified (SV_CONTOURGROUP) and the approximate 
                                   number of contours levels in the new set 
                                   (SV_APPROXNUMVALUES).

        ContourLevelAction_        
        DeleteNearest              Delete the contour level with the value nearest 
                                   the supplied value. You may specify the number of 
                                   the contour group being modified (SV_CONTOURGROUP)
                                   and the value of the contour variable to be deleted
                                   (SV_RANGEMIN).

   @endverbatim
 *   
 * @return
 *   TRUE if modification was successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilContourLevelX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   Add contour levels 0.1, 0.2, and 0.3 to the current set of contour levels for contour group 2:
 *   
 * @code
 *   Boolean_t IsOk = TRUE;
 *   ArgList_pa ArgList;
 *   double levels[3] = {0.1, 0.2, 0.3};
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList, SV_CONTOURLEVELACTION, (LgIndex_t) ContourLevelAction_Add);
 *   TecUtilArgListAppendInt(ArgList, SV_CONTOURGROUP, 2);
 *   TecUtilArgListAppendInt(ArgList, SV_NUMVALUES, 3);
 *   TecUtilArgListAppendArray(ArgList, SV_RAWDATA, (void *)levels);
 *   IsOk = TecUtilContourLevelX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLevelX(ArgList_pa ArgList);


/**
 * @deprecated
 *   Please use TecUtilContourLevelX() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLevelAdd(int           NumEntries, 
                                                     const double *RawData_Array, 
                                                     Boolean_t     ShowTrace);

/**
 * @deprecated
 *   Please use TecUtilContourLevelX() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLevelNew(int           NumEntries, 
                                                     const double *RawData_Array, 
                                                     Boolean_t     ShowTrace);

/**
 * @deprecated
 *   Please use TecUtilContourLevelX() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLevelDeleteRange(double    RangeMin, 
                                                             double    RangeMax, 
                                                             Boolean_t ShowTrace);

/**
 * @deprecated
 *   Please use TecUtilContourLevelX() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLevelReset(int NumEntries);

/**
 * @deprecated
 *   Please use TecUtilContourLevelX() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLevelDelNearest(double    Level, 
                                                            Boolean_t ShowTrace);

/**
 * Manage labels in your plot.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_CONTOURLABELACTION
 * Type:
 *   ContourLabelAction_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_CONTOURGROUP
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *   
 * Name:
 *   SV_X
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 *   
 * Name:
 *   SV_Y
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 *   
 * Name:
 *   SV_Z
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 *   
 * Name:
 *   SV_ISALIGNED
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * </ArgListTable>
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilContourLabelX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 * Add a contour label at position (0.5, 0.25) in a 2-D plot. Make it align
 * itself to the nearest contour line.
 *   
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID)
 *   ArgList = TecUtilArgListAlloc();
 *   
 *   TecUtilArgListAppendInt(ArgList, SV_CONTOURLABELACTION, ContourLabelAction_Add);
 *   TecUtilArgListAppendDouble(ArgList, SV_X, 2);
 *   TecUtilArgListAppendDouble(ArgList, SV_Y, 2);
 *   TecUtilArgListAppendDouble(ArgList, SV_Z, 2);
 *   TecUtilArgListAppendInt(ArgList, SV_ISALIGNED, TRUE);
 *   
 *   TecUtilContourLabelX(ArgList);
 *   
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLabelX(ArgList_pa ArgList);



/**
 * @deprecated
 *   Please use TecUtilContourLabelX() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLabelAdd(double    X, 
                                                     double    Y, 
                                                     double    Z, 
                                                     Boolean_t IsAligned);


/**
 * @deprecated
 *   Please use TecUtilContourLabelX() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourLabelDeleteAll(void);




/**
 *   Add a single streamtrace or a rake of streamtraces to the current frame. The frame must be a 2-D
 *   or 3-D field plot.
 *   
 * @param NumRakePoints
 *   The number of points in the rake of streamtraces which are being added. Use one if just one
 *   streamtrace is being added. The number must be greater than zero
 *   
 * @param StreamType
 *   The type of streamtraces which are being added. The possible values are: Streamtrace_SurfaceLine,
 *   Streamtrace_VolumeLine, Streamtrace_VolumeRibbon, Streamtrace_VolumeRod, Streamtrace_TwoDLine
 *   
 * @param Direction
 *   The direction of the streamtraces which are being added. The possible values are:
 *   StreamDir_Forward, StreamDir_Reverse, or StreamDir_Both
 *   
 * @param StartXPos
 *   X-Coordinate for the starting position of the streamtraces being added. If NumRakePoints is
 *   greater than one, then this is the starting position of the rake of streamtraces which are being
 *   added. StartZPos is only used for 3-D streamtraces
 *   
 * @param StartYPos
 *   Y-Coordinate for the starting position of the streamtraces being added.
 *   
 * @param StartZPos
 *   Z-Coordinate for the starting position of the streamtraces being added. Only used streamtraces are
 *   3-D.
 *   
 * @param AltStartXPos
 *   X-Coordinate for the alternate starting position of the streamtraces which are being added. If
 *   NumRakePoints is one, this value is ignored (except in the case of streamtraces of type ribbon or
 *   rod, in which case the alternate starting position defines the streamtrace orientation).
 *   Otherwise, these values define the ending position of the rake of streamtraces which are being
 *   added.
 *   
 * @param AltStartYPos
 *   Y-Coordinate for the alternate starting position of the streamtraces which are being added. See
 *   AltStartXPos.
 *   
 * @param AltStartZPos
 *   Z-Coordinate for the alternate starting position of the streamtraces which are being added. See
 *   AltStartXPos.
 *   
 * @return
 *   TRUE if successful, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceAdd(
 *   &                   NumRakePoints,
 *   &                   StreamType,
 *   &                   Direction,
 *   &                   StartXPos,
 *   &                   StartYPos,
 *   &                   StartZPos,
 *   &                   AltStartXPos,
 *   &                   AltStartYPos,
 *   &                   AltStartZPos)
 *    INTEGER*4       NumRakePoints
 *    INTEGER*4       StreamType
 *    INTEGER*4       Direction
 *    REAL*8          StartXPos
 *    REAL*8          StartYPos
 *    REAL*8          StartZPos
 *    REAL*8          AltStartXPos
 *    REAL*8          AltStartYPos
 *    REAL*8          AltStartZPos
 * </FortranSyntax>
 *   
 *   Add a single 2-D line streamtrace to the current frame:
 *   
 * @code
 *   Boolean_t IsOk = FALSE;
 *   double    Xp = 0.0;
 *   double    Yp = 0.0;
 *   
 *   Xp = 10.0;
 *   Yp = 15.0;
 *   IsOk = TecUtilStreamtraceAdd(1, Streamtrace_TwoDLine,
 *                                StreamDir_Forward, Xp, Yp, 0.0,
 *                                0.0, 0.0, 0.0);
 *   if (IsOk)
 *     {
 *       // do more processing 
 *         .
 *         .
 *         .
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtraceAdd(int           NumRakePoints,
                                                    Streamtrace_e StreamType,
                                                    StreamDir_e   Direction,
                                                    double        StartXPos,
                                                    double        StartYPos,
                                                    double        StartZPos,
                                                    double        AltStartXPos,
                                                    double        AltStartYPos,
                                                    double        AltStartZPos);
/**
 *   Delete all streamtraces in the current frame. Streamtraces are numbered sequentially.
 *   
 * @return
 *   TRUE if successful, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceDeleteAll()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtraceDeleteAll(void);
/**
 *   Delete a range of streamtraces. Streamtraces are numbered sequentially in the order they were
 *   created.
 *   
 * @param Start
 *   Starting stream in the range of Streamtraces to delete.
 *   
 * @param End
 *   Ending stream in the range of Streamtraces to delete.
 *   
 * @return
 *   TRUE if successful, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceDeleteRange(
 *   &                   Start,
 *   &                   End)
 *    INTEGER*4       Start
 *    INTEGER*4       End
 * </FortranSyntax>
 *   
 *   Delete streamtraces 1 through 5.
 *   
 * @code
 *   Boolean_t IsOk = FALSE;
 *   IsOk = TecUtilStreamtraceDeleteRange(1, 5);
 *   if (IsOk)
 *     {
 *       // do more processing 
 *         .
 *         .
 *         .
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtraceDeleteRange(int Start,
                                                            int End);
/**
 *   Set the position of the termination line for streamtraces.
 *   
 * @param NumPoints
 *   The number of XY-termination line points supplied. The number must be greater than zero, and less
 *   than or equal to the number of XY-termination points supplied
 *   
 * @param XTermLinePts_Array
 *   Array of X-termination line ordinates
 *   
 * @param YTermLinePts_Array
 *   Array of Y-termination line ordinates
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceSetTermLine(
 *   &                   NumPoints,
 *   &                   XTermLinePts_Array,
 *   &                   YTermLinePts_Array)
 *    INTEGER*4       NumPoints
 *    REAL*8          XTermLinePts_Array
 *    REAL*8          YTermLinePts_Array
 * </FortranSyntax>
 *   
 *   Define a line for terminating streamtraces.
 *   
 * @code
 *   Boolean_t IsOk = FALSE;
 *   double    Xp[2];
 *   double    Yp[2];
 *   
 *   Xp[0] = 0.0;
 *   Yp[0] = 0.0;
 *   Xp[1] = 5.0;
 *   Yp[1] = 8.0;
 *   IsOk = TecUtilStreamtraceSetTermLine(2, Xp, Yp);
 *   if (IsOk)
 *     {
 *       // do more processing 
 *         .
 *         .
 *         .
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtraceSetTermLine(int           NumPoints,
                                                            const double *XTermLinePts_Array,
                                                            const double *YTermLinePts_Array);
/**
 *   Resets the time interval between stream markers.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceResetDelta()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilStreamtraceResetDelta(void);
/**
 *   Get the number of active streamtraces.
 *   
 * @return
 *   Returns the number of streamtraces currently defined.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStreamtraceGetCount()
 * </FortranSyntax>
 */
LINKTOADDON int STDCALL TecUtilStreamtraceGetCount(void);
/**
 *   Get the starting position for a given streamtrace.
 *   
 * @param StreamNumber
 *   Number of the streamtrace
 *   
 * @param X
 *   References to the Variable in which to store the X-position of the streamtrace.
 *   
 * @param Y
 *   References to the Variable in which to store the Y-position of the streamtrace.
 *   
 * @param Z
 *   References to the Variable in which to store the Z-position of the streamtrace.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStreamtraceGetPos(
 *   &           StreamNumber,
 *   &           X,
 *   &           Y,
 *   &           Z)
 *    INTEGER*4       StreamNumber
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Z
 * </FortranSyntax>
 *   
 *   Get the position of streamtrace number 3.
 *   
 * @code
 *     double X,Y,Z;
 *     TecUtilStreamtraceGetPos(3,&X,&Y,&Z);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilStreamtraceGetPos(int    StreamNumber,
                                                  double *X,
                                                  double *Y,
                                                  double *Z);

/**
 * Assign a value to a field variable at a specific position. This function
 * does not require you to obtain the handle to the field data as does
 * TecUtilDataValueSetByRef(), however this function is not very efficient.
 * Use TecUtilDataValueSetByRef() if you are setting multiple values in the
 * same zone. If the zone referenced is IJ- or IJK-ordered then the position is
 * calculated by treating the two- or three-dimensional array as a
 * one-dimensional array. TecUtilStateChanged() need not be called after
 * changing data in this way since Tecplot will handle that for you.
 *   
 * @param Zone
 *   The zone number.
 *   
 * @param Var
 *   The variable number.
 *   
 * @param PointIndex
 *   Position in the array of field data values. Position starts at one.
 *   
 * @param Value
 *   New value for the position in the field data.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueSetByZoneVar(
 *   &                   Zone,
 *   &                   Var,
 *   &                   PointIndex,
 *   &                   Value)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    INTEGER*4       PointIndex
 *    REAL*8          Value
 * </FortranSyntax>
 *   
 *   Set the first value of the second variable of zone 5 to be 0.0.
 *   
 * @code
 *   TecUtilDataValueSetByZoneVar(5, 2, 1, 0.0);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueSetByZoneVar(EntIndex_t Zone,
                                                             EntIndex_t Var,
                                                             LgIndex_t  PointIndex,
                                                             double     Value);
/**
 *   Operates on a data set within Tecplot using FORTRAN-like equations. See Section 8.2, "Data
 *   Alteration through Equations," in the Tecplot User's Manual for more information on using
 *   equations in Tecplot.
 *   
 * @param Equation
 *   String containing the equation
 *   
 * @param ZoneSet
 *   Set of zones to operate on. Pass NULL to operate on all zones
 *   
 * @param IMin
 *   Operate on values starting at an I-Index range of IMin. Set to 1 when operating on the entire
 *   range.
 *   
 * @param IMax
 *   Operate on values ending at an I-Index range of IMax. Set to 0 to specify the maximum I index.
 *   
 * @param ISkip
 *   Operate on values skipping by ISkip in the I-Direction. Set to 0 to specify one less than the
 *   maximum I index range.
 *   
 * @param JMin
 *   See IMin.
 *   
 * @param JMax
 *   See IMax.
 *   
 * @param JSkip
 *   See IMax.
 *   
 * @param KMin
 *   See IMin.
 *   
 * @param KMax
 *   See IMax.
 *   
 * @param KSkip
 *   See IMax.
 *   
 * @param DestDataType
 *   Data type for the variable on the left hand side. This is used only if this variable is being
 *   created for the first time. The possible choices are: FieldDataType_Float,
 *   FieldDataType_Double, FieldDataType_Int32, FieldDataType_Int16, FieldDataType_Bit,
 *   FieldDataType_Byte, or FieldDataType_Invalid.  If set to FieldDataType_Invalid, Tecplot
 *   will choose the type for you.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataAlter(
 *   &                   Equation,
 *   &                   ZoneSetPtr,
 *   &                   IMin,
 *   &                   IMax,
 *   &                   ISkip,
 *   &                   JMin,
 *   &                   JMax,
 *   &                   JSkip,
 *   &                   KMin,
 *   &                   KMax,
 *   &                   KSkip,
 *   &                   DestDataType)
 *    CHARACTER*(*)   Equation
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    INTEGER*4       IMin
 *    INTEGER*4       IMax
 *    INTEGER*4       ISkip
 *    INTEGER*4       JMin
 *    INTEGER*4       JMax
 *    INTEGER*4       JSkip
 *    INTEGER*4       KMin
 *    INTEGER*4       KMax
 *    INTEGER*4       KSkip
 *    INTEGER*4       DestDataType
 * </FortranSyntax>
 *   
 *   Add 2 to X in zones 1 and 3. Operate only data points where J is greater than or equal to three
 *   and less than or equal to ten:
 *   
 * @code
 *   if (TecUtilDataSetIsAvailable())
 *     {
 *       Set_pa    ZoneSet;
 *       Boolean_t IsOk = FALSE;
 *       ZoneSet = TecUtilSetAlloc(TRUE);
 *   
 *       if (ZoneSet)
 *         {
 *           TecUtilSetAddMember(ZoneSet,1,TRUE);
 *           TecUtilSetAddMember(ZoneSet,3,TRUE);
 *   
 *           IsOk = TecUtilDataAlter("X = X + 2",
 *                                   ZoneSet,
 *                                   1,0,1,  // All I-Values 
 *                                   3,10,1  // 3 <= J <= 10 
 *                                   1,0,1,  // All K Values 
 *                                   FieldDataType_Float);
 *           TecUtilSetDealloc(&ZoneSet);
 *         }
 *     }
 * @endcode
 *
 *
 *  Fortran Example.  Execute X = X + 2 on all points in all zones:
 *
 * @code
 *      POINTER(NullPtr, Null)
 *      INTEGER*4 IsOk
 *
 *      NullPtr = 0
 *
 *      if (TecUtilDataSetIsAvailable().EQ.TRUE) then
 *        IsOk = TecUtilDataAlter('X = X + 2'//char(0),
 *     &                          NullPtr,
 *     &                          1,0,1,
 *     &                          1,0,1,
 *     &                          1,0,1,
 *     &                          FieldDataType_Float)
 *      Endif
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataAlter(const char     *Equation,
                                               Set_pa          ZoneSet,
                                               LgIndex_t       IMin,
                                               LgIndex_t       IMax,
                                               LgIndex_t       ISkip,
                                               LgIndex_t       JMin,
                                               LgIndex_t       JMax,
                                               LgIndex_t       JSkip,
                                               LgIndex_t       KMin,
                                               LgIndex_t       KMax,
                                               LgIndex_t       KSkip,
                                               FieldDataType_e DestDataType);
/**
 *   Smooth data (that is, reduce the spikes) for selected variables in selected zones.
 *   
 * @param Zone
 *   The number of the zone to smooth. The zone must be an ordered zone (not a finite-element zone)
 *   
 * @param SmoothVar
 *   The number of the variable to smooth. This cannot be a variable which is assigned to an axis
 *   
 * @param NumSmoothPasses
 *   The number of smoothing passes to perform. The normal default value is on
 *   
 * @param SmoothWeight
 *   The relaxation factor for each pass of smoothing. Must be a number between zero and one
 *   (exclusively). Higher numbers indicate a greater smoothing effect. The normal default value is
 *   0.8
 *   
 * @param SmoothBndryCond
 *   The boundary condition by which to smooth.
 *   
 * @return
 *   TRUE if the input parameters are valid and the current frame mode is XY, 2D, or 3D. Otherwise,
 *   FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSmooth(
 *   &                   Zone,
 *   &                   SmoothVar,
 *   &                   NumSmoothPasses,
 *   &                   SmoothWeight,
 *   &                   SmoothBndryCond)
 *    INTEGER*4       Zone
 *    INTEGER*4       SmoothVar
 *    INTEGER*4       NumSmoothPasses
 *    REAL*8          SmoothWeight
 *    INTEGER*4       SmoothBndryCond
 * </FortranSyntax>
 *   
 *   Perform one smoothing pass on variable 3 in zone 2:
 *   
 * @code
 *   TecUtilSmooth(2, 3, 1, 0.8, BoundaryCondition_Fixed);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilSmooth(EntIndex_t          Zone,
                                            EntIndex_t          SmoothVar,
                                            LgIndex_t           NumSmoothPasses,
                                            double              SmoothWeight,
                                            BoundaryCondition_e SmoothBndryCond);



/**
 * @deprecated
 *   Please use TecUtilCurveWriteInfo() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilWriteCurveInfo(const char     *FName,
                                                    EntIndex_t      XYMapNum,
                                                    ProcessXYMode_e ProcessXYMode);



/**
 * Write out the coefficients or the calculated data points for the equations
 * used to draw the curve for a selected line-mapping.
 *   
 * @param FileName
 *   File name. Must not be NULL
 *   
 * @param LineMap
 *   The number of a line-mapping that does some type of curve fit or spline
 *   
 * @param CurveInfoMode
 *   The possible values are:
 *     CurveInfoMode_Coefficients,
 *     CurveInfoMode_RawData, and
 *     CurveInfoMode_Macro
 *   
 * @return
 *   TRUE if the curve information wrote, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCurveWriteInfo(
 *   &                   FileName,
 *   &                   LineMap,
 *   &                   CurveInfoMode)
 *    CHARACTER*(*)   FileName
 *    INTEGER*4       LineMap
 *    INTEGER*4       CurveInfoMode
 * </FortranSyntax>
 *   
 * Write out the coefficients for line-mapping number 3 to the file map3.out:
 *   
 * @code
 *   Boolean_t IsOk;
 *   IsOk = TecUtilCurveWriteInfo("map3.out", 3,CurveInfoMode_Coefficients);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilCurveWriteInfo(const char      *FileName,
                                                    EntIndex_t      LineMap,
                                                    CurveInfoMode_e CurveInfoMode);
/**
 *   Make a copy of a zone.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_SOURCEZONE
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The source zone.
 *   
 * Name:
 *   SV_IMIN
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
 *   SV_IMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   0 = Max
 *   
 * Name:
 *   SV_ISKIP
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
 *   SV_JMIN
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
 *   SV_JMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   0 = Max
 *   
 * Name:
 *   SV_JSKIP
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
 *   SV_KMIN
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
 *   SV_KMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   0 = Max
 *   
 * Name:
 *   SV_KSKIP
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * </ArgListTable>
 *   
 * @return
 *   Returns TRUE if successful, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneCopyX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneCopyX(ArgList_pa ArgList);
/**
 *   Make a copy of an existing zone. You can assign index ranges to create a new zone which is a
 *   sub-set of the source zone.
 *   
 * @param ZoneUsed
 *   Source zone. Must be greater than or equal to one
 *   
 * @param IMin
 *   Minimum I-index. Set to one to duplicate the entire zone
 *   
 * @param IMax
 *   Maximum I-index. Set to zero to duplicate the entire zone
 *   
 * @param ISkip
 *   I skip value. Set to one to duplicate the entire zone
 *   
 * @param JMin
 *   Minimum J-index. Set to one to duplicate the entire zone
 *   
 * @param JMax
 *   Maximum J-index. Set to zero to duplicate the entire zone
 *   
 * @param JSkip
 *   J skip value. Set to one to duplicate the entire zone.
 *   
 * @param KMin
 *   Minimum K-index. Set to one to duplicate the entire zone.
 *   
 * @param KMax
 *   Maximum K-index. Set to zero to duplicate the entire zone
 *   
 * @param KSkip
 *   K skip value. Set to one to duplicate the entire zone
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneCopy(
 *   &                   ZoneUsed,
 *   &                   IMin,
 *   &                   IMax,
 *   &                   ISkip,
 *   &                   JMin,
 *   &                   JMax,
 *   &                   JSkip,
 *   &                   KMin,
 *   &                   KMax,
 *   &                   KSkip)
 *    INTEGER*4       ZoneUsed
 *    INTEGER*4       IMin
 *    INTEGER*4       IMax
 *    INTEGER*4       ISkip
 *    INTEGER*4       JMin
 *    INTEGER*4       JMax
 *    INTEGER*4       JSkip
 *    INTEGER*4       KMin
 *    INTEGER*4       KMax
 *    INTEGER*4       KSkip
 * </FortranSyntax>
 *   
 *   Duplicate zone 3:
 *   
 * @code
 *   Boolean_t IsOK = TecUtilZoneCopy(3,1,0,1,1,0,1,1,0,1);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneCopy(EntIndex_t ZoneUsed,
                                                   LgIndex_t  IMin,
                                                   LgIndex_t  IMax,
                                                   LgIndex_t  ISkip,
                                                   LgIndex_t  JMin,
                                                   LgIndex_t  JMax,
                                                   LgIndex_t  JSkip,
                                                   LgIndex_t  KMin,
                                                   LgIndex_t  KMax,
                                                   LgIndex_t  KSkip);
/**
 *   Create new zones that are mirror images of the source zones.
 *   
 * @param SourceZones
 *   Set of zones to mirror. A separate new zone is created for each zone in the set. Pass NULL to
 *   mirror all zones
 *   
 * @param MirrorVar
 *   Variable to negate. Use the following table to determine the value of MirrorVarin 2-D: Mirror
 *   about: MirrorVarX-Axis 'Y'Y-Axis 'X'in 3-D: Mirror about: MirrorVarX-Y Plane 'Z'Y-Z Plane 'X'X-Z
 *   Plane 'Y'
 *   
 * @return
 *   TRUE if successfull, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateMirrorZones(
 *   &                   SourceZonesPtr,
 *   &                   MirrorVar)
 *    POINTER         (SourceZonesPtr, SourceZones)
 *    CHARACTER*(*)   MirrorVar
 * </FortranSyntax>
 *   
 *   Create two mirror zones about the X-axis:
 *   
 * @code
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   if (set)
 *   {
 *     TecUtilSetAddMember(set,1,FALSE); // mirror zones 1 and 2 
 *     TecUtilSetAddMember(set,2,FALSE);
 *   
 *     TecUtilCreateMirrorZones(set,'X');
 *     TecUtilSetDealloc(&set);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateMirrorZones(Set_pa SourceZones,
                                                       char   MirrorVar);
/**
 *   Create one or more zones out of the currently defined streamtraces. The new zones have the same
 *   number of variables per data point as the other zones in the data set with all non-coordinate
 *   variables interpolated at the positions along the streamtrace.
 *   
 * @param ConcatenateStreams
 *   Set to TRUE to create a single zone out of all common streamtraces. The cell that connects the end
 *   of one streamtrace with the beginning of the next can later be turned off using value-blanking
 *   
 * @return
 *   TRUE if the zone could be created, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateStreamZones(ConcatenateStreams)
 *    INTEGER*4 ConcatenateStreams
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateStreamZones(Boolean_t ConcatenateStreams);
/**
 *   Create finite element surface zones out of iso-surfaces in volume zones. One zone will be made for
 *   each contour level.
 *   
 * @return
 *   TRUE if successfull, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateIsoZones()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateIsoZones(void);
/**
 *   Create surface zones out of all slices currently defined in volume zones. One zone is made for
 *   each defined slice.
 *   
 * @return
 *   TRUE if the zone is created successfully, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateSliceZones()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSliceZones(void);
/**
 *   Create zones from contour lines. Contour lines will only come from those zones with a contour
 *   style set to contour lines or lines and flood. One zone will be made from each contour level. The
 *   resulting zones are of type FE-Triangle.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateContourLineZones()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateContourLineZones(void);
/**
 *   Create zones from contour lines.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_CONTLINECREATEMODE
 * Type:
 *   ContCreateMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ContLineCreateMode_OneZonePerContourLevel
 * Required:
 *   No
 * Notes:
 *   Possible values: ContLineCreateMode_OneZonePerCountourLevel or ContLineCreateMode_OneZonePerIndependentPolyline.
 * </ArgListTable>
 *   
 * @return
 *   TRUE if create zone successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateContourLineZonesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   ArgList_pa ArgList;
 *   
 * @code
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   
 *   TecUtilArgListAppendInt(ArgList, SV_CONTLINECREATEMODE, (LgIndex_t)ContLineCreateMode_OneZonePerContourLevel);
 *   
 *   TecUtilCreateContourLineZonesX(ArgList);
 *   
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateContourLineZonesX(ArgList_pa ArgList);
/**
 *   Zone boundaries for finite element data cannot be turned on or off using the boundary plot layer
 *   in Tecplot. You can, however, create a separate zone which is the boundary of a finite-element
 *   zone. This new zone can then be turned on or off. One requirement for this function to work
 *   correctly is that adjacent cells must share the same node points along their common boundary.
 *   
 * @param SourceZone
 *   Zone to use. This must be a finite-element zone.
 *   
 * @param RemoveBlankedSurfaces
 *   If blanking is turned on, do not include faces of cells that border on blanked regions in the set
 *   of faces that become the resulting finite-element boundary zone.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateFEBoundary(
 *   &                   SourceZone,
 *   &                   RemoveBlankedSurfaces)
 *    INTEGER*4       SourceZone
 *    INTEGER*4       RemoveBlankedSurfaces
 * </FortranSyntax>
 *   
 *   Create a separate zone which is the boundary of the first zone:
 *   
 * @code
 *    TecUtilCreateFEBoundary(1,TRUE);
 *   // zone 1 must be finite element 
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateFEBoundary(EntIndex_t SourceZone,
                                                      Boolean_t  RemoveBlankedSurfaces);




/**
 * @deprecated
 *   Please use TecUtilCreateSliceZoneFromPlane() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSliceZone(double OriginX,
                                                     double OriginY,
                                                     double OriginZ,
                                                     double NormalX,
                                                     double NormalY,
                                                     double NormalZ);




/**
 *   Create a new zone as a slice through the currently active zones that match the specified slice
 *   source and are intersected by a slice plane having the specified origin and normal.
 *   
 * @param SliceSource
 *   Source for slicing. The possible choices
 *   are:SliceSource_SurfaceZonesSliceSource_VolumeZonesSliceSource_SurfacesOfVolumeZones
 *   
 * @param OriginX
 *   X-Coordinate for the slicing plane origin.
 *   
 * @param OriginY
 *   Y-Coordinate for the slicing plane origin.
 *   
 * @param OriginZ
 *   Z-Coordinate for the slicing plane origin.
 *   
 * @param NormalX
 *   X-Component for the vector normal to the slicing plane
 *   
 * @param NormalY
 *   Y-Component for the vector normal to the slicing plane
 *   
 * @param NormalZ
 *   Z-Component for the vector normal to the slicing plane
 *   
 * @return
 *   TRUE if the zone is created successfully, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateSliceZoneFromPlane(
 *   &                   SliceSource,
 *   &                   OriginX,
 *   &                   OriginY,
 *   &                   OriginZ,
 *   &                   NormalX,
 *   &                   NormalY,
 *   &                   NormalZ)
 *    INTEGER*4       SliceSource
 *    REAL*8          OriginX
 *    REAL*8          OriginY
 *    REAL*8          OriginZ
 *    REAL*8          NormalX
 *    REAL*8          NormalY
 *    REAL*8          NormalZ
 * </FortranSyntax>
 *   
 *   Slice 3-D volume zones with a the plane X=2.5:
 *   
 * @code
 *   TecUtilCreateSliceZoneFromPlane(SliceSource_VolumeZones,
 *            2.5, 0., 0., 1., 0., 0.);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSliceZoneFromPlane(SliceSource_e SliceSource,
                                                              double        OriginX,
                                                              double        OriginY,
                                                              double        OriginZ,
                                                              double        NormalX,
                                                              double        NormalY,
                                                              double        NormalZ);
/**
 *   Create a zone from a slice taken from a 3D plot.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_SLICESOURCE
 * Type:
 *   SliceSource_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   SliceSourceVolumeZones
 * Required:
 *   No
 *   
 * Name:
 *   SV_FORCEEXTRACTIONTOSINGLEZONE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Boolean value which forces Tecplot to make the taken slice into a single
 *   zone if made to be TRUE.  This option is turned off with the value of
 *   FALSE
 *   
 * Name:
 *   SV_ORIGINX
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   Sets the origin values of the created slice zone
 *   
 * Name:
 *   SV_ORIGINY
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_ORIGINZ
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_NORMALX
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * Notes:
 *   Sets the normal values of the created slice zone
 *   
 * Name:
 *   SV_NORMALY
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_NORMALZ
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Required:
 *   Yes
 * </ArgListTable>
 *   
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCreateSliceZoneFromPlneX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   ArgList_pa ArgList;
 *   
 * @code
 *   
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   
 *   TecUtilArgListAppendInt(ArgList,    SV_SLICESOURCE,                 (LgIndex_t)SliceSource_SurfaceZones);
 *   
 *   TecUtilArgListAppendInt(ArgList,    SV_FORCEEXTRACTIONTOSINGLEZONE, (LgIndex_t)TRUE);
 *   TecUtilArgListAppendDouble(ArgList, SV_ORIGINX,0.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_ORIGINY,0.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_ORIGINZ,0.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_NORMALX,0.9); TecUtilArgListAppendDouble(ArgList, SV_NORMALY,0.9);
 *   TecUtilArgListAppendDouble(ArgList, SV_NORMALZ,0.9);
 *   
 *   TecUtilCreateSliceZoneFromPlneX(ArgList);
 *   
 *   TecUtilArgListDealloc(&ArgList);
 *    TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilCreateSliceZoneFromPlneX(ArgList_pa ArgList);


/**
 *   Extract data from a 2-D or 3-D field plot
 *   
 * @param PolylineXPts_Array
 *   X-array defining the polyline used to extract the data.
 *   
 * @param PolylineYPts_Array
 *   Y-array defining the polyline used to extract the data.
 *   
 * @param PolylineZPts_Array
 *   Z-array defining the polyline used to extract the data.
 *   
 * @param NumPtsInPolyline
 *   Number of points in the supplied polyline
 *   
 * @param ExtractThroughVolume
 *   If this is TRUE and the current frame mode is 3D then this will extract data from within any
 *   active volume zones in the data set. If the frame mode is 3D and this is FALSE then a projection
 *   is made from each extraction point onto the nearest surface away from your eye with respect to
 *   the eye coordinate system. If the frame mode is 2D then this parameter is ignored
 *   
 * @param ExtractOnlyPointsOnPolyline
 *   Extract only from the points that define the polyline
 *   
 * @param IncludeDistanceVariable
 *   Include the distance variable in the resulting extracted data. This is only available if
 *   ExtractToFile is TRUE.
 *   
 * @param NumPtsToExtractAlongPolyline
 *   Number of points to evenly distribute along the polyline. Data is extracted from these points if
 *   ExtractOnlyPointsOnPolyline is FALSE
 *   
 * @param ExtractToFile
 *   If set to TRUE the data is sent to the file ExtractFName in the form of a valid Tecplot ASCII data
 *   file. If FALSE then a new zone is created within Tecplot
 *   
 * @param ExtractFName
 *   Name of the file where extracted data is sent if ExtractToFile is TRUE
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExtractFromPolyline(
 *   &                   PolylineXPts_Array,
 *   &                   PolylineYPts_Array,
 *   &                   PolylineZPts_Array,
 *   &                   NumPtsInPolyline,
 *   &                   ExtractThroughVolume,
 *   &                   ExtractOnlyPointsOnPolyline,
 *   &                   IncludeDistanceVariable,
 *   &                   NumPtsToExtractAlongPolyline,
 *   &                   ExtractToFile,
 *   &                   ExtractFName)
 *    REAL*8          PolylineXPts_Array
 *    REAL*8          PolylineYPts_Array
 *    REAL*8          PolylineZPts_Array
 *    INTEGER*4       NumPtsInPolyline
 *    INTEGER*4       ExtractThroughVolume
 *    INTEGER*4       ExtractOnlyPointsOnPolyline
 *    INTEGER*4       IncludeDistanceVariable
 *    INTEGER*4       NumPtsToExtractAlongPolyline
 *    INTEGER*4       ExtractToFile
 *    CHARACTER*(*)   ExtractFName
 * </FortranSyntax>
 *   
 *   Extract ten points from specific locations in a field plot. Create a zone with the extracted data:
 *   
 * @code
 *   // raw data 
 *   double X[10] = { 0, 1, 2, 3, 3, 4, 4, 3, 4, 5 };
 *   double Y[10] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
 *   double Z[10] = { 4, 3, 2, 5, 3, 1, 2, 3, 4, 3 };
 *   
 *   TecUtilExtractFromPolyline(X,Y,Z,10,
 *                             TRUE,TRUE,FALSE,10,FALSE,NULL);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilExtractFromPolyline(const double *PolylineXPts_Array,
                                                         const double *PolylineYPts_Array,
                                                         const double *PolylineZPts_Array,
                                                         LgIndex_t     NumPtsInPolyline,
                                                         Boolean_t     ExtractThroughVolume,
                                                         Boolean_t     ExtractOnlyPointsOnPolyline,
                                                         Boolean_t     IncludeDistanceVariable,
                                                         LgIndex_t     NumPtsToExtractAlongPolyline,
                                                         Boolean_t     ExtractToFile,
                                                         const char   *ExtractFName);
/**
 *   Extract data from a 2- or 3-D field plot. The locations at which to extract the data come from a
 *   polyline geometry that must be picked prior to issuing this command.
 *   
 * @param ExtractOnlyPointsOnPolyline
 *   Extract only from the points that define the polyline
 *   
 * @param IncludeDistanceVariable
 *   Include the distance variable in the resulting data extracted. This is only available if
 *   ExtractToFile is TRUE
 *   
 * @param NumPtsToExtractAlongPolyline
 *   Number of points to evenly distribute along the polyline. Data is extracted from these points if
 *   ExtractOnlyPointsOnPolyline is FALSE
 *   
 * @param ExtractToFile
 *   If set to TRUE the data is sent to the file ExtractFName in the form of a valid Tecplot ASCII data
 *   file. If FALSE then a new zone is created within Tecplot
 *   
 * @param ExtractFName
 *   Name of the file where extracted data is sent if ExtractToFile is TRUE.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExtractFromGeom(
 *   &                   ExtractOnlyPointsOnPolyline,
 *   &                   IncludeDistanceVariable,
 *   &                   NumPtsToExtractAlongPolyline,
 *   &                   ExtractToFile,
 *   &                   ExtractFName)
 *    INTEGER*4       ExtractOnlyPointsOnPolyline
 *    INTEGER*4       IncludeDistanceVariable
 *    INTEGER*4       NumPtsToExtractAlongPolyline
 *    INTEGER*4       ExtractToFile
 *    CHARACTER*(*)   ExtractFName
 * </FortranSyntax>
 *   
 *   Extract 20 points from along the currently picked geometry. Send the result to a file called
 *   "extract.dat":
 *   
 * @code
 *   TecUtilExtractFromGeom(FALSE,FALSE,20,TRUE,"extract.dat");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilExtractFromGeom(Boolean_t ExtractOnlyPointsOnPolyline,
                                                     Boolean_t IncludeDistanceVariable,
                                                     LgIndex_t NumPtsToExtractAlongPolyline,
                                                     Boolean_t ExtractToFile,
                                                     const char     *ExtractFName);



/**
 * @deprecated
 *   Please use TecUtilTransformCoordinatesX() instead.
 */
LINKTOADDON Boolean_t  STDCALL TecUtilPolarToRectangular(Set_pa ZoneSet);




/**
 *   Transform Coordinates.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_CREATENEWVARIABLES
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   If TRUE, the transform creates new variables for the transformed
 *   coordinates.  If FALSE, the transformed coordinates are placed in the
 *   specified variable numbers.  Default is FALSE
 *   
 * Name:
 *   SV_THETAVAR
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The variable numbers of the theta and r coordinate variables in polar of
 *   spherical coordinates.  They are required source variable numbers for
 *   polar-to-rectangular transformations.  If SV_CREATENEWVARIABLES is FALSE,
 *   they are required destination variable numbers for the
 *   rectangular-to-polar and rectangular-to-spherical transformation
 *   
 * Name:
 *   SV_RVAR
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_PSIVAR
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The variable number of the psi variable in spherical coordinates. This is
 *   a required source variable for spherical-to-rectangular coordinate
 *   transformations. If SV_CREATENEWVARIABLES is FALSE, it is a required
 *   destinations variable number for the rectangular-to-spherical coordinate
 *   transformation.
 *   
 * Name: 
 *   SV_XVAR 
 * Type: 
 *   EntIndex_t 
 * Arg Function: 
 *   TecUtilArgListAppendInt()
 * Required: 
 *   Yes 
 *  Notes:
 *   The variable numbers of the X and Y coordinate variables in rectangular
 *   coordinates. The are required source variable numbers for the
 *   rectangular-to-polar or rectangular-to-spherical coordinate
 *   transformations.  If SV_CREATENEWVARIABLES is FALSE, they are required
 *   destination variable numbers for the polar-to-rectangular or
 *   spherical-to-rectangular transformations
 *   
 * Name:
 *   SV_YVAR
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_ZVAR
 * Type:
 *   entIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The variable number of the z-coordinate variable in rectangular
 *   coordinates.  It is required source variable number for the
 *   rectangular-to-spherical coordinate transformation.  If
 *   SV_CREATENEWVARIABLES is FALSE, it is required a required destination
 *   variable number for the spherical-to-polar transformation
 *   
 * Name:
 *   SV_ZONESET
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   The set of zones to operate on.  If not supplied then all active zones will be operated on
 *   
 * Name:
 *   SV_ANGLESPEC
 * Type:
 *   ThetaMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ThetaMode_Radians
 * Required:
 *   No
 * Notes:
 *   Units of angle variables (optional).  Possible values are:
 *   ThetaMode_Degrees, ThetaMode_Radians, ThetaMode_Arbitrary.
 *   
 * Name:
 *   SV_TRANSFORMATION
 * Type:
 *   Transform_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The desired transformation (required). Possible values are:
 *   Transform_PolarToRect, Transform_SphericalToRect, Transform_RectToPolar,
 *   Transform_RectToSpherical.
 * </ArgListTable>
 *   
 * @return
 *   TRUE if transformation was successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTransformCoordinatesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   Transform spherical coordinate variables (varibles 1, 2 and 3) to rectangular (cartesian)
 *   coordinate variables.
 *   
 * @code
 *   
 *   ArgList_pa ArgList;
 *   Set_pa ZoneSet = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(ZoneSet, 1, TRUE);
 *   
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *       TecUtilArgListAppendInt(ArgList,
 *    SV_THETAVAR,
 *    (LgIndex_t)1);
 *       TecUtilArgListAppendInt(ArgList,
 *    SV_RVAR,
 *    (LgIndex_t)2);
 *       TecUtilArgListAppendInt(ArgList,
 *    SV_PSIVAR,
 *    (LgIndex_t)3);
 *       TecUtilArgListAppendInt(ArgList,
 *    SV_XVAR,
 *    (LgIndex_t)4);
 *       TecUtilArgListAppendInt(ArgList,
 *    SV_YVAR,
 *    (LgIndex_t)5);
 *       TecUtilArgListAppendInt(ArgList,
 *    SV_ZVAR,
 *    (LgIndex_t)6);
 *       TecUtilArgListAppendSet(ArgList,
 *    SV_ZONESET,
 *    ZoneSet);
 *       TecUtilArgListAppendInt(ArgList,
 *    SV_ANGLESPEC,
 *    (LgIndex_t)ThetaMode_Arbitrary);
 *       TecUtilArgListAppendInt(ArgList,
 *    SV_TRANSFORMATION,
 *    (LgIndex_t)Transform_SphericalToRect);
 *   
 *   TecUtilTransformCoordinatesX(ArgList);
 *   
 *   TecUtilArgListDealloc(&ArgList);
 *   
 *   TecUtilSetDealloc(&ZoneSet);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t  STDCALL TecUtilTransformCoordinatesX(ArgList_pa ArgList);




/**
 * @deprecated
 *   Please use TecUtilDataRotate2D() instead.
 */
LINKTOADDON Boolean_t  STDCALL TecUtilRotate2D(Set_pa ZoneSet,
                                               double RotateAmountInDegrees,
                                               double XOrigin,
                                               double YOrigin);
/**
 *   Rotate field data in 2-D about any point.
 *   
 * @param ZoneSet
 *   Zones to rotate
 *   
 * @param RotateAmountInDegrees
 *   Angle of rotation in degrees
 *   
 * @param XOrigin
 *   X-origin about which to rotate
 *   
 * @param YOrigin
 *   Y-origin about which to rotate
 *   
 * @return
 *   Returns TRUE if append is successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataRotate2D(
 *   &                   ZoneSetPtr,
 *   &                   RotateAmountInDegrees,
 *   &                   XOrigin,
 *   &                   YOrigin)
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    REAL*8          RotateAmountInDegrees
 *    REAL*8          XOrigin
 *    REAL*8          YOrigin
 * </FortranSyntax>
 */




LINKTOADDON Boolean_t  STDCALL TecUtilDataRotate2D(Set_pa ZoneSet,
                                                   double RotateAmountInDegrees,
                                                   double XOrigin,
                                                   double YOrigin);
/**
 *   Most plotting in Tecplot is based on using values at the corners (nodes)
 *   of each cell. If you have some field values that are cell-centered then
 *   you need a way to interpolate those values to cell corners.
 *   TecUtilAverageCellCenterData() is used to interpolate cell centered data
 *   in IJ- or IJK-ordered datasets from the cell-center to the nodes. This
 *   same objective can be accomplished by using one of the interpolation
 *   options in Tecplot but that is usually more involved and requires more
 *   processing time. In preparation for using this function, you must: \n
 *      -# Determine which variables are cell-centered values in your data.
 *      -# Construct the Tecplot input data file as follows;
 *           -# Treat all node data in the usual manner.
 *           -# Assign the cell-centered data to the lowest \n
 *              indexed node of the cell it represents.
 *           -# Assign dummy values (0.0) to all nodes at I=IMAX, J=JMAX,\n
 *              (and K=KMAX, for IJK-ordered data).
 *      -# Load your data into Tecplot and call this function.\n
 *
 *   The data from the lowest indexed corners of each cell are treated as if
 *   they are positioned in the cell center.TecUtilAverageCellCenterData() then
 *   takes these values and replaces the values at the nodes with the new
 *   interpolated values.
 *
 *   If your objective is only to view a flooded contour plot where each cell
 *   is filled with a single color representing the cell-centered value, then
 *   do steps 1 and 2 above and set the contour plot type to Corner.  Only call
 *   this routine if the current frame is in 2D or 3D frame mode.
 *   
 * @param ZoneSet
 *   Set of zones to operate on. NULL will do all zones.
 *   
 * @param VarSet
 *   Set of variables to shift from the cell center to the nodes. NULL will do all variables not
 *   assigned to the X-, Y- or Z-axis.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAverageCellCenterData(
 *   &                   ZoneSetPtr,
 *   &                   VarSetPtr)
 *    POINTER         (ZoneSetPtr, ZoneSet)
 *    POINTER         (VarSetPtr, VarSet)
 * </FortranSyntax>
 *   
 *   Average the cell-centered values for variables 4, 7, 8, and 9 in zones 1-3:
 *   
 * @code
 *   if (TecUtilFrameGetMode() == Frame_TwoD ||
 *       TecUtilFrameGetMode() == Frame_ThreeD)
 *     {
 *       Set_pa    ZoneSet;
 *       Set_pa    VarSet;
 *       Boolean_t IsOk = FALSE;
 *   
 *       ZoneSet = TecUtilSetAlloc(TRUE);
 *       VarSet  = TecUtilSetAlloc(TRUE);
 *       if (ZoneSet && VarSet)
 *         {
 *           TecUtilSetAddMember(ZoneSet,1,TRUE);
 *           TecUtilSetAddMember(ZoneSet,2,TRUE);
 *           TecUtilSetAddMember(ZoneSet,3,TRUE);
 *           TecUtilSetAddMember(VarSet,4,TRUE);
 *           TecUtilSetAddMember(VarSet,7,TRUE);
 *           TecUtilSetAddMember(VarSet,8,TRUE);
 *           TecUtilSetAddMember(VarSet,9,TRUE);
 *           IsOk = TecUtilAverageCellCenterData(ZoneSet,VarSet);
 *           TecUtilSetDealloc(&ZoneSet);
 *           TecUtilSetDealloc(&VarSet);
 *         }
 *     }
 * @endcode
 */




LINKTOADDON Boolean_t STDCALL TecUtilAverageCellCenterData(Set_pa ZoneSet,
                                                           Set_pa VarSet);
/**
 *   Interpolate selected variables from a set of source zones to a destination zone using linear
 *   interpolation. The source zones cannot be I-ordered. Values assigned to the destination zone are
 *   equivalent to the results of using the Probe tool in Tecplot. See Section 8.8.3, "Linear
 *   Interpolation," in the Tecplot User's Manual for more information about linear interpolation and
 *   its available options.
 *   
 * @param SourceZones
 *   Set of zones used to obtain the field values for interpolation. Use NULL to specify all zones
 *   except DestZone.
 *   
 * @param DestZone
 *   Destination zone for interpolation
 *   
 * @param VarList
 *   Set of variables to interpolate. Use NULL to specify all variables except those assigned to the
 *   axes
 *   
 * @param LinearInterpConst
 *   Constant value to which all points outside the data field are set. Only used if LinearInterpMode
 *   is LinearInterpMode_SetToConst. (Normal default value is 0.0.)
 *   
 * @param LinearInterpMode
 *   How to deal with points that are outside the source zones' data field. (Normal default value is
 *   LinearInterpMode_SetToConst.) The possible values are: LinearInterpMode_DontChange (Preserves the
 *   points values). LinearInterpMode_SetToConst (Sets all points to LinearInterpConst).
 *   
 * @return
 *   Returns TRUE if the interpolation completes successfully. A FALSE return value indicates that the
 *   user pressed cancel during the interpolation process or that the input parameters are not valid.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLinearInterpolate(
 *   &                   SourceZonesPtr,
 *   &                   DestZone,
 *   &                   VarListPtr,
 *   &                   LinearInterpConst,
 *   &                   LinearInterpMode)
 *    POINTER         (SourceZonesPtr, SourceZones)
 *    INTEGER*4       DestZone
 *    POINTER         (VarListPtr, VarList)
 *    REAL*8          LinearInterpConst
 *    INTEGER*4       LinearInterpMode
 * </FortranSyntax>
 *   
 *   Interpolate all of the variables (except those assigned to the axes) from zones 1-3 to zone 4
 *   using linear interpolation:
 *   
 * @code
 *   Boolean_t IsOk;
 *   Set_pa Zones = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(Zones, 1, TRUE);
 *   TecUtilSetAddMember(Zones, 2, TRUE);
 *   TecUtilSetAddMember(Zones, 3, TRUE);
 *   IsOk = TecUtilLinearInterpolate(
 *            Zones,
 *            4,
 *            (Set_pa)NULL,
 *            0.0,
 *            LinearInterpMode_SetToConst);
 *   TecUtilSetDealloc(&Zones);
 * @endcode
 */




LINKTOADDON Boolean_t STDCALL TecUtilLinearInterpolate(Set_pa             SourceZones,
                                                       EntIndex_t         DestZone,
                                                       Set_pa             VarList,
                                                       double             LinearInterpConst,
                                                       LinearInterpMode_e LinearInterpMode);
/**
 *   Interpolate selected variables from one or more zones onto a destination zone using the inverse
 *   distance method. See Section 8.8.1, "Inverse-Distance Interpolation," in the Tecplot User's
 *   Manual for more information about inverse distance interpolation and its available options.
 *   
 * @param SourceZones
 *   Set of zones used to obtain the field values from for the interpolation. Use NULL to specify all
 *   zones except DestZone.
 *   
 * @param DestZone
 *   Destination zone for the interpolation
 *   
 * @param VarList
 *   Set of variables to interpolate. Use NULL to specify all variables except those assigned to the
 *   axes
 *   
 * @param InvDistExponent
 *   Exponent for the inverse-distance weighting. (Normal default value is 3.5.)
 *   
 * @param InvDistMinRadius
 *   Minimum distance used for the inverse-distance weighting. (Normal default value is 0.0.)
 *   
 * @param InterpPtSelection
 *   Method for determining which source points to consider for each destination data point. (Normal
 *   default value is PtSelection_OctantN.) The possible values are: PtSelection_All (All points in
 *   the source zone). PtSelection_NearestN (Closest N points to the destination point).
 *   PtSelection_OctantN (Closest N points selected by coordinate-system octants)
 *   
 * @param InterpNPoints
 *   Number of source points to consider for each destination data point. Only used if
 *   InterpPtSelection is PtSelection_NearestN or PtSelection_OctantN. (Normal default value is
 *   eight.) Must be greater than zero
 *   
 * @return
 *   Returns TRUE if the interpolation completes successfully. A FALSE return value indicates that the
 *   user pressed cancel during the interpolation process or that the input parameters are not valid.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilInverseDistInterpolation(
 *   &                   SourceZonesPtr,
 *   &                   DestZone,
 *   &                   VarListPtr,
 *   &                   InvDistExponent,
 *   &                   InvDistMinRadius,
 *   &                   InterpPtSelection,
 *   &                   InterpNPoints)
 *    POINTER         (SourceZonesPtr, SourceZones)
 *    INTEGER*4       DestZone
 *    POINTER         (VarListPtr, VarList)
 *    REAL*8          InvDistExponent
 *    REAL*8          InvDistMinRadius
 *    INTEGER*4       InterpPtSelection
 *    INTEGER*4       InterpNPoints
 * </FortranSyntax>
 *   
 *   Interpolate all of the variables (except those assigned to the axes) from zones 1-3 to zone 4
 *   using inverse-distance.
 *   
 * @code
 *   Boolean_t IsOk;
 *   Set_pa Zones = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(Zones, 1, TRUE);
 *   TecUtilSetAddMember(Zones, 2, TRUE);
 *   TecUtilSetAddMember(Zones, 3, TRUE);
 *   IsOk = TecUtilInverseDistInterpolation(
 *            Zones,
 *            4,
 *            (Set_pa)NULL,
 *            3.5,
 *            0.0,
 *            PtSelection_OctantN,
 *            8);
 *   TecUtilSetDealloc(&Zones);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilInverseDistInterpolation(Set_pa          SourceZones,
                                                              EntIndex_t      DestZone,
                                                              Set_pa          VarList,
                                                              double          InvDistExponent,
                                                              double          InvDistMinRadius,
                                                              PtSelection_e   InterpPtSelection,
                                                              LgIndex_t       InterpNPoints);




/**
 *   Interpolate selected variables from a set of source zones to a destination zone using the kriging
 *   method. See Section 8.8.2, "Kriging," in the Tecplot User's Manual for more information about
 *   kriging and its available options.
 *   
 * @param SourceZones
 *   Set of zones used to obtain the field values for interpolation. Use NULL to specify all zones
 *   except DestZone
 *   
 * @param DestZone
 *   Destination zone for interpolation
 *   
 * @param VarList
 *   Set of variables to interpolate. Use NULL to specify all variables except those assigned to the
 *   axes
 *   
 * @param KrigRange
 *   Distance beyond which source points become insignificant. (Normal default value is 0.3.) Must be
 *   between zero and one, inclusive
 *   
 * @param KrigZeroValue
 *   Semi-variance at each source data point on a normalized scale from zero to one. (Normal default
 *   value is 0.0.)
 *   
 * @param KrigDrift
 *   Overall trend for the data. (Normal default value is Drift_Linear.) The possible values are:
 *   Drift_None (No trend). Drift_Linear (Linear trend). Drift_Quad (Quadratic trend).
 *   
 * @param InterpPtSelection
 *   Method for determining which source points to consider for each destination data point. The
 *   possible values are: PtSelection_All (All points in the source zone). PtSelection_NearestN
 *   (Closest N points to the destination point). PtSelection_OctantN (Closest N points selected by
 *   coordinate-system octants)
 *   
 * @param InterpNPoints
 *   Number of source points to consider for each destination data point. Only used if
 *   InterpPtSelection is PtSelection_NearestN or PtSelection_OctantN. (Normal default value is
 *   eight.) Must be greater than zero
 *   
 * @return
 *   Returns TRUE if the interpolation completes successfully. A FALSE return value indicates that the
 *   user pressed cancel during the interpolation process or that the input parameters are not valid.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilKrig(
 *   &                   SourceZonesPtr,
 *   &                   DestZone,
 *   &                   VarListPtr,
 *   &                   KrigRange,
 *   &                   KrigZeroValue,
 *   &                   KrigDrift,
 *   &                   InterpPtSelection,
 *   &                   InterpNPoints)
 *    POINTER         (SourceZonesPtr, SourceZones)
 *    INTEGER*4       DestZone
 *    POINTER         (VarListPtr, VarList)
 *    REAL*8          KrigRange
 *    REAL*8          KrigZeroValue
 *    INTEGER*4       KrigDrift
 *    INTEGER*4       InterpPtSelection
 *    INTEGER*4       InterpNPoints
 * </FortranSyntax>
 *   
 *   Interpolate all of the variables (except those assigned to the axes) from zones 1-3 to zone 4
 *   using kriging:
 *   
 * @code
 *   Boolean_t IsOk;
 *   Set_pa Zones = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(Zones, 1, TRUE);
 *   TecUtilSetAddMember(Zones, 2, TRUE);
 *   TecUtilSetAddMember(Zones, 3, TRUE);
 *   IsOk = TecUtilKrig(
 *            Zones,
 *            4,
 *            (Set_pa)NULL,
 *            0.3,
 *            0.0,
 *            Drift_Linear,
 *            PtSelection_OctantN,
 *            8);
 *   TecUtilSetDealloc(&Zones);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilKrig(Set_pa         SourceZones,
                                           EntIndex_t     DestZone,
                                           Set_pa         VarList,
                                           double         KrigRange,
                                           double         KrigZeroValue,
                                           Drift_e        KrigDrift,
                                           PtSelection_e  InterpPtSelection,
                                           LgIndex_t      InterpNPoints);
/**
 *   Create a new zone by forming triangles from data points in existing zones.
 *   
 * @param SourceZones
 *   Set of zones to triangulate
 *   
 * @param DoBoundary
 *   If TRUE, BoundaryZones must specify one or more I-ordered zones that define the boundaries across
 *   which no triangles can be created
 *   
 * @param BoundaryZones
 *   Set of zones for DoBoundary. Required if DoBoundary is TRUE, ignored otherwise
 *   
 * @param IncludeBoundaryPts
 *   TRUE if you also want the boundary points to be used to create triangles
 *   
 * @param NumCoincidentPts
 *   Returns the number of coincident points
 *   
 * @param TriangleKeepFactor
 *   A number between zero and 0.5. The smaller the number, the more likely it will be that highly
 *   obtuse triangles will be created opening toward the outside of the triangulated zone
 *   
 * @return
 *   TRUE if successful, FALSE otherwise
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTriangulate(
 *   &                   SourceZonesPtr,
 *   &                   DoBoundary,
 *   &                   BoundaryZonesPtr,
 *   &                   IncludeBoundaryPts,
 *   &                   NumCoincidentPts,
 *   &                   TriangleKeepFactor)
 *    POINTER         (SourceZonesPtr, SourceZones)
 *    INTEGER*4       DoBoundary
 *    POINTER         (BoundaryZonesPtr, BoundaryZones)
 *    INTEGER*4       IncludeBoundaryPts
 *    INTEGER*4       NumCoincidentPts
 *    REAL*8          TriangleKeepFactor
 * </FortranSyntax>
 *   
 *   Create a zone by triangulating data points from zones 1 and 2:
 *   
 * @code
 *   LgIndex_t NumCoincidentPts;
 *   Set_pa set = TecUtilSetAlloc(FALSE);
 *   TecUtilSetAddMember(set,1,FALSE);
 *   TecUtilSetAddMember(set,2,FALSE);
 *   TecUtilTriangulate(set,FALSE,NULL,FALSE,&NumCoincidentPts,0.25);
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilTriangulate(Set_pa     SourceZones,
                                                 Boolean_t  DoBoundary,
                                                 Set_pa     BoundaryZones,
                                                 Boolean_t  IncludeBoundaryPts,
                                                 LgIndex_t *NumCoincidentPts,
                                                 double     TriangleKeepFactor);

/**
 * Animate zones.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_ZONEANIMATIONMODE
 * Type:
 *   ZoneAnimationMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ZoneAnimationMode_StepByNumber
 * Required:
 *   No
 * Notes:
 *   Used to select how zones are animated. If "StepByNumber" then individual
 *   zones are animated by successively activating/deactivating one zone at a
 *   time, skipped by the specified skip value. If "GroupStepByNumber" then
 *   groups of zones are animated by successively activating/deactivating each
 *   zone group. Tecplot uses the skip value to delineate groups of zones. If
 *   "StepByTime" groups of zones by time are animated by successively
 *   activating/deactivating each time group. Tecplot uses the auxiliary zone
 *   data scalar named "Common.Time" to group zones by time.
 *
 * Name:
 *   SV_START
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   These are the start, end and skip for the zones to animate.
 *   
 * Name:
 *   SV_END
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_SKIP
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *   
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *   
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateZonesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   Animate zones 1-10. Write the results to an AVI file named Animzones.avi.
 *   
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList, SV_EXPORTFORMAT,    ExportFormat_AVI);
 *   TecUtilArgListAppendInt(ArgList, SV_FNAME,           "Animzones.avi");
 *   TecUtilArgListAppendInt(ArgList, SV_START,           1);
 *   TecUtilArgListAppendInt(ArgList, SV_END,             10);
 *   TecUtilArgListAppendInt(ArgList, SV_CREATEMOVIEFILE, TRUE);
 *   IsOk = TecUtilAnimateZonesX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateZonesX(ArgList_pa ArgList);

/**
 * @deprecated
 *   Please use TecUtilAnimateLineMapsX() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateXYMapsX(ArgList_pa ArgList);

/**
 * Animate Line-mapping.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_START
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   These are the start, end and skip for the Line-mappings to animate.
 *   
 * Name:
 *   SV_END
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_SKIP
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *   
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file.
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *   
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateLineMapsX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 * Animate Line-maps 5-10. Write the results to an AVI file named Animxy.avi.
 *   
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList, SV_EXPORTFORMAT,    ExportFormat_AVI);
 *   TecUtilArgListAppendInt(ArgList, SV_FNAME,           "Animxy.avi");
 *   TecUtilArgListAppendInt(ArgList, SV_START,           5);
 *   TecUtilArgListAppendInt(ArgList, SV_END,             10);
 *   TecUtilArgListAppendInt(ArgList, SV_CREATEMOVIEFILE, TRUE);
 *   IsOk = TecUtilAnimateLineMapsX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateLineMapsX(ArgList_pa ArgList);




/**
 * Animate contour levels.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_START
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   These are the start, end and skip for the contour levels to animate.
 *   
 * Name:
 *   SV_END
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_SKIP
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 *   
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *   
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateContourLevelsX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 * Animate contour levels 1-10. Write the results to an AVI file named animc.avi.
 *   
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc()
 *   TecUtilArgListAppendInt(ArgList, SV_EXPORTFORMAT,    ExportFormat_AVI);
 *   TecUtilArgListAppendInt(ArgList, SV_FNAME,           "animc.avi");
 *   TecUtilArgListAppendInt(ArgList, SV_START,           1);
 *   TecUtilArgListAppendInt(ArgList, SV_END,             10);
 *   TecUtilArgListAppendInt(ArgList, SV_CREATEMOVIEFILE, TRUE);
 *   IsOk = TecUtilAnimateContourLevelsX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateContourLevelsX(ArgList_pa ArgList);





/**
 * Animate I-, J-, or K-planes.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_START
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   These are the start, end and skip for the planes to animate.
 *   
 * Name:
 *   SV_END
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_SKIP
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
 *   SV_PLANES
 * Type:
 *   short
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   `I'
 * Required:
 *   No
 * Notes:
 *   Associated value indicates which planeto animate. Use T J or K (typecast to LgIndex_t)
 *   
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file.
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *   
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateIJKPlanesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 * Animate J-planes 5-10.
 *   
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList, SV_EXPORTFORMAT,    ExportFormat_AVI);
 *   TecUtilArgListAppendInt(ArgList, SV_FNAME,           "IJKPlanes.avi");
 *   TecUtilArgListAppendInt(ArgList, SV_START,           5);
 *   TecUtilArgListAppendInt(ArgList, SV_END,             10);
 *   TecUtilArgListAppendInt(ArgList, SV_SKIP,            1);
 *   TecUtilArgListAppendInt(ArgList, SV_PLANES,          (LgIndex_t)'J');
 *   TecUtilArgListAppendInt(ArgList, SV_CREATEMOVIEFILE, TRUE);
 *   TecUtilAnimateIJKPlanesX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateIJKPlanesX(ArgList_pa ArgList);




/**
 * Animate IJK-blanking.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_IMINFRACT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.1
 * Required:
 *   No
 * Notes:
 *   Assoc. values are the minimum fractions for blanking at the start of
 *   animation. Actual I-index is minfract*IMAX. This is similiar for J and K.
 *   
 * Name:
 *   SV_JMINFRACT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.1
 * Required:
 *   No
 *   
 * Name:
 *   SV_KMINFRACT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.1
 * Required:
 *   No
 *   
 * Name:
 *   SV_IMAXFRACT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 * Notes:
 *   Assoc. values are the maximum fractions for blanking at the start of
 *   animation. Actual I-index is maxfract*IMAX. This is similiar for J and K.
 *   
 * Name:
 *   SV_JMAXFRACT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 *   
 * Name:
 *   SV_KMAXFRACT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 *   
 * Name:
 *   SV_IMINFRACT2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.8
 * Required:
 *   No
 * Notes:
 *   Assoc. values are the minimum fractions for blanking at the start of 
 *   animation. Actual I-index is minfract*IMAX. This is similiar for J and K.
 *   
 * Name:
 *   SV_JMINFRACT2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.8
 * Required:
 *   No
 *   
 * Name:
 *   SV_KMINFRACT2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.8
 * Required:
 *   No
 *   
 * Name:
 *   SV_IMAXFRACT2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 * Notes:
 *   Assoc. values are the maximum fractions for blanking at the start 
 *   of animation. Actual I-index is maxfract*IMAX. This is similiar for J and K.
 *   
 * Name:
 *   SV_JMAXFRACT2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 *   
 * Name:
 *   SV_KMAXFRACT2
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 *   
 * Name:
 *   SV_NUMSTEPS
 * Type:
 *   int
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Number of intermediate steps for the animation. This parameter is required
 *   
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *   
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateIJKBlankingX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 * Animate IJK-blanking starting with fractions 0, 0.1, 0.1 (the defaults) for
 * the minimum fractions, 1.0, 1.0, 1.0 for the maximum fractions. End with
 * 0.7, 0.7, 0.7 for the minimum fractions and 0.75, 0.75, 0.75 for the maximum
 * fractions. Do animation in ten steps.
 *   
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,    SV_EXPORTFORMAT,   ExportFormat_RM);
 *   TecUtilArgListAppendInt(ArgList,    SV_FNAME,          "IJKBlanking.rm");
 *   TecUtilArgListAppendDouble(ArgList, SV_IMAXFRACT,      1.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_JMAXFRACT,      1.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_KMAXFRACT,      1.0);
 *   TecUtilArgListAppendDouble(ArgList, SV_IMINFRACT2,     0.7);
 *   TecUtilArgListAppendDouble(ArgList, SV_JMINFRACT2,     0.7);
 *   TecUtilArgListAppendDouble(ArgList, SV_KMINFRACT2,     0.7);
 *   TecUtilArgListAppendDouble(ArgList, SV_IMAXFRACT2,     0.75);
 *   TecUtilArgListAppendDouble(ArgList, SV_JMAXFRACT2,     0.75);
 *   TecUtilArgListAppendDouble(ArgList, SV_KMAXFRACT2,     0.75);
 *   TecUtilArgListAppendInt(ArgList,    SV_NUMSTEPS,       10);
 *   TecUtilArgListAppendInt(ArgList,    SV_CREATEMOVIEFILE,TRUE);
 *   TecUtilAnimateIJKBlankingX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateIJKBlankingX(ArgList_pa ArgList);



/**
 * Animate streamtraces.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_NUMCYCLES
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   4
 * Required:
 *   No
 * Notes:
 *   Associated value is the number of complete time cycles to animate.
 *   
 * Name:
 *   SV_STEPSPERCYCLE
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   10
 * Required:
 *   No
 * Notes:
 *   Associated value is the number of steps per cycle.
 *   
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file.
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *   
 * @return
 *   TRUE if animation was successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateStreamX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 * Make an animation with two cycles and ten steps per cycle. Create an AVI movie file.
 *   
 * @code
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList, SV_EXPORTFORMAT,    ExportFormat_AVI);
 *   TecUtilArgListAppendInt(ArgList, SV_FNAME,           "Stream.avi");
 *   TecUtilArgListAppendInt(ArgList, SV_NUMCYCLES,       2);
 *   TecUtilArgListAppendInt(ArgList, SV_STEPSPERCYCLE,   10);
 *   TecUtilArgListAppendInt(ArgList, SV_CREATEMOVIEFILE, TRUE);
 *   TecUtilAnimateStreamX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateStreamX(ArgList_pa ArgList);




/**
 * Animates slices.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_START
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Start and end indices are based on NumSlices. EndSlice cannot be greater than 
 *   NumSlices. In most cases, StartSlice will be 1, and EndSlice will be equal to NumSlices.
 *   
 * Name:
 *   SV_END
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   2
 * Required:
 *   No
 *   
 * Name:
 *   SV_NUMSLICES
 * Type:
 *   SmInteger_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   2
 * Required:
 *   No
 * Notes:
 *   The number of slices to distribute between the currently defined Slice 1 and Slice 2 locations.
 *   
 * Name:
 *   SV_CREATEMOVIEFILE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Associated value indicates if the animation should be saved to a file.
 *
 * Name:
 *   SV_FNAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   File to write to.
 *
 * Name:
 *   SV_EXPORTFORMAT
 * Type:
 *   ExportFormat_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   The format used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the format used for the export.
 *
 * Name:
 *   SV_ANIMATIONSPEED
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   The animation speed used for the previous animation.
 * Required:
 *   No
 * Notes:
 *   Specifies the animation speed used for the export.
 * </ArgListTable>
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateSlicesX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 * Animate 50 slices, using the currently defined Slice 1 and Slice 2 positions
 * as the beginning and end of the animation. Send the animation to a movie
 * file.
 *   
 * @code
 *   Boolean_t IsOk = TRUE;
 *   ArgList_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList, SV_EXPORTFORMAT,    ExportFormat_AVI);
 *   TecUtilArgListAppendInt(ArgList, SV_FNAME,           "AnimateSlice.avi");
 *   TecUtilArgListAppendInt(ArgList, SV_START,           1);
 *   TecUtilArgListAppendInt(ArgList, SV_END,             50);
 *   TecUtilArgListAppendInt(ArgList, SV_NUMSLICES,       50);
 *   TecUtilArgListAppendInt(ArgList, SV_CREATEMOVIEFILE, TRUE);
 *   IsOk = TecUtilAnimateSlicesX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateSlicesX(ArgList_pa ArgList);

/**
 *   Produce an animation showing one zone at a time.
 *   
 * @param StartZone
 *   Starting zone for the animation.
 *   
 * @param EndZone
 *   Ending zone for the animation.
 *   
 * @param ZoneSkip
 *   Zone skip for the animation.
 *   
 * @param CreateMovieFile
 *   Set to TRUE if you want Tecplot to create a movie file.
 *   
 * @param MovieFName
 *   Name of the file to create if CreateMovieFile is TRUE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateZones(
 *   &                   StartZone,
 *   &                   EndZone,
 *   &                   ZoneSkip,
 *   &                   CreateMovieFile,
 *   &                   MovieFName)
 *    INTEGER*4       StartZone
 *    INTEGER*4       EndZone
 *    INTEGER*4       ZoneSkip
 *    INTEGER*4       CreateMovieFile
 *    CHARACTER*(*)   MovieFName
 * </FortranSyntax>
 *   
 *   Create an animation showing (in sequence) zones 2, 4, 6, 8, and 10:
 *   
 * @code
 *   TecUtilAnimateZones(2, 10, 2, FALSE, NULL);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateZones(EntIndex_t  StartZone,
                                                  EntIndex_t  EndZone,
                                                  EntIndex_t  ZoneSkip,
                                                  Boolean_t   CreateMovieFile,
                                                  const char *MovieFName);



/**
 * @deprecated
 *   Please use TecUtilAnimateLineMapsX() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateXYMaps(EntIndex_t  StartMap,
                                                   EntIndex_t  EndMap,
                                                   EntIndex_t  MapSkip,
                                                   Boolean_t   CreateMovieFile,
                                                   const char *MovieFName);



/**
 *   Produce an animation of a contour line plot by showing a single level at a time. The animation
 *   varies according to the currently defined contour levels.
 *   
 * @param StartLevel
 *   The starting contour level for the animation.
 *   
 * @param EndLevel
 *   The ending contour levels for the animation.
 *   
 * @param LevelSkip
 *   The level skip.
 *   
 * @param CreateMovieFile
 *   Set to TRUE to create a Raster Metafile of the animation.
 *   
 * @param MovieFName
 *   String specifying the file name for the movie file. This parameter is used only if CreateMovieFile
 *   is TRUE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateContourLevels(
 *   &                   StartLevel,
 *   &                   EndLevel,
 *   &                   LevelSkip,
 *   &                   CreateMovieFile,
 *   &                   MovieFName)
 *    INTEGER*4       StartLevel
 *    INTEGER*4       EndLevel
 *    INTEGER*4       LevelSkip
 *    INTEGER*4       CreateMovieFile
 *    CHARACTER*(*)   MovieFName
 * </FortranSyntax>
 *   
 *   Animate contour levels 1-10:
 *   
 * @code
 *   TecUtilAnimateContourLevels(1,10,1,FALSE,(char *)NULL);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateContourLevels(SmInteger_t StartLevel,
                                                          SmInteger_t EndLevel,
                                                          SmInteger_t LevelSkip,
                                                          Boolean_t   CreateMovieFile,
                                                          const char *MovieFName);
/**
 *   Produce an animation that cycles through I-, J-, or K-planes in an IJK-ordered data set.
 *   
 * @param IJOrK
 *   This can be set to `I', `J' or `K'
 *   
 * @param StartIndex
 *   The starting plane index for the animation.
 *   
 * @param EndIndex
 *   The ending plane index for the animation.
 *   
 * @param IndexSkip
 *   The plane index skip for the animation.
 *   
 * @param CreateMovieFile
 *   Set to TRUE if you want Tecplot to create a movie file.
 *   
 * @param MovieFName
 *   Name of the file to create if CreateMovieFile is TRUE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateIJKPlanes(
 *   &                   IJOrK,
 *   &                   StartIndex,
 *   &                   EndIndex,
 *   &                   IndexSkip,
 *   &                   CreateMovieFile,
 *   &                   MovieFName)
 *    CHARACTER*(*)   IJOrK
 *    INTEGER*4       StartIndex
 *    INTEGER*4       EndIndex
 *    INTEGER*4       IndexSkip
 *    INTEGER*4       CreateMovieFile
 *    CHARACTER*(*)   MovieFName
 * </FortranSyntax>
 *   
 *   Produce an animation that shows every other J-plane from J=1 to J=11:
 *   
 * @code
 *   TecUtilAnimateIJKPlanes('J',
 *                           1,  // Start at J=1 
 *                           11, // End at J=11 
 *                           2,  // Skip  every other J-Plane 
 *                           FALSE,
 *                           (char *)NULL);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateIJKPlanes(char       IJOrK,
                                                      LgIndex_t  StartIndex,
                                                      LgIndex_t  EndIndex,
                                                      LgIndex_t  IndexSkip,
                                                      Boolean_t  CreateMovieFile,
                                                      const char      *MovieFName);
/**
 *   Produce an animation of different IJK-blankings in your plot. The animation starts at one
 *   IJK-blanking setting and marches through intermediate steps to a second setting.
 *   
 * @param StartIMinFract
 *   Min I-Index fraction for the start of the animation. For example, to start the animation with
 *   blanking in the I direction at I=20 when IMax is 200 set StartIMinFract to 0.10.
 *   
 * @param StartJMinFract
 *   Min J-Index fraction for the start of the animation.
 *   
 * @param StartKMinFract
 *   Min K-Index fraction for the start of the animation.
 *   
 * @param StartIMaxFract
 *   Max I-Index fraction for the start of the animation. For example, to start the animation with
 *   blanking in the I direction at I=20 when IMax is 200 set StartIMinFract to 0.10.
 *   
 * @param StartJMaxFract
 *   Max J-Index fraction for the start of the animation.
 *   
 * @param StartKMaxFract
 *   Max K-Index fraction for the start of the animation.
 *   
 * @param EndIMinFract
 *   Min I-Index fraction for the end of the animation.
 *   
 * @param EndJMinFract
 *   Min J-Index fraction for the end of the animation.
 *   
 * @param EndKMinFract
 *   Min K-Index fraction for the end of the animation.
 *   
 * @param EndIMaxFract
 *   Max I-Index fraction for the end of the animation.
 *   
 * @param EndJMaxFract
 *   Max J-Index fraction for the end of the animation.
 *   
 * @param EndKMaxFract
 *   Max K-Index fraction for the end of the animation.
 *   
 * @param NumSteps
 *   Number of steps or frames to create. The index blanking ranges are linearly interpolated based on
 *   the step and the Start and End fractions.
 *   
 * @param CreateMovieFile
 *   Set to TRUE if you want Tecplot to create a movie file.
 *   
 * @param MovieFName
 *   Name of the file to create if CreateMovieFile is TRUE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateIJKBlanking(
 *   &                   StartIMinFract,
 *   &                   StartJMinFract,
 *   &                   StartKMinFract,
 *   &                   StartIMaxFract,
 *   &                   StartJMaxFract,
 *   &                   StartKMaxFract,
 *   &                   EndIMinFract,
 *   &                   EndJMinFract,
 *   &                   EndKMinFract,
 *   &                   EndIMaxFract,
 *   &                   EndJMaxFract,
 *   &                   EndKMaxFract,
 *   &                   NumSteps,
 *   &                   CreateMovieFile,
 *   &                   MovieFName)
 *    REAL*8          StartIMinFract
 *    REAL*8          StartJMinFract
 *    REAL*8          StartKMinFract
 *    REAL*8          StartIMaxFract
 *    REAL*8          StartJMaxFract
 *    REAL*8          StartKMaxFract
 *    REAL*8          EndIMinFract
 *    REAL*8          EndJMinFract
 *    REAL*8          EndKMinFract
 *    REAL*8          EndIMaxFract
 *    REAL*8          EndJMaxFract
 *    REAL*8          EndKMaxFract
 *    INTEGER*4       NumSteps
 *    INTEGER*4       CreateMovieFile
 *    CHARACTER*(*)   MovieFName
 * </FortranSyntax>
 *   
 *   Produce an animation showing a band of I-planes traversing the entire data field using six steps:
 *   
 * @code
 *   TecUtilAnimateIJKBlanking(0.1,0.0,0.0,
 *                             1.0,1.0,1.0,
 *                             1.0,0.0,0.0,
 *                             1.0,1.0,1.0,
 *                             6,
 *                             FALSE,
 *                             (char *)NULL);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateIJKBlanking(double      StartIMinFract,
                                                        double      StartJMinFract,
                                                        double      StartKMinFract,
                                                        double      StartIMaxFract,
                                                        double      StartJMaxFract,
                                                        double      StartKMaxFract,
                                                        double      EndIMinFract,
                                                        double      EndJMinFract,
                                                        double      EndKMinFract,
                                                        double      EndIMaxFract,
                                                        double      EndJMaxFract,
                                                        double      EndKMaxFract,
                                                        int         NumSteps,
                                                        Boolean_t   CreateMovieFile,
                                                        const char *MovieFName);
/**
 *   Produce an animation of stream markers or dashes, moving along the currently defined streamtrace
 *   paths.
 *   
 * @param NumStepsPerCycle
 *   This is the number of steps to take to complete one full cycle in time where one cycle is defined
 *   as the time it takes to complete one dash on and one dash off. The larger this number, the
 *   smoother and slower the animation.
 *   
 * @param NumCycles
 *   The number of cycles to complete during the animation.
 *   
 * @param CreateMovieFile
 *   Set to TRUE if you want Tecplot to create a movie file.
 *   
 * @param MovieFName
 *   Name of the file to create if CreateMovieFile is TRUE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateStream(
 *   &                   NumStepsPerCycle,
 *   &                   NumCycles,
 *   &                   CreateMovieFile,
 *   &                   MovieFName)
 *    INTEGER*4       NumStepsPerCycle
 *    INTEGER*4       NumCycles
 *    INTEGER*4       CreateMovieFile
 *    CHARACTER*(*)   MovieFName
 * </FortranSyntax>
 *   
 *   Animate streamtraces for three cycles with each cycle using 20 steps:
 *   
 * @code
 *   TecUtilAnimateStream(20,3,FALSE,(char *)NULL);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateStream(int         NumStepsPerCycle,
                                                   int         NumCycles,
                                                   Boolean_t   CreateMovieFile,
                                                   const char *MovieFName);
/**
 *   Animates the currently defined 3-D slices.
 *   
 * @param StartSlice
 *   Starting slice for the animation. Must be less than or equal to NumSlices. This is most often set
 *   to 1.
 *   
 * @param EndSlice
 *   Ending slice for the animation. EndSlice must be greator than or equal to StartSlice.
 *   
 * @param NumSlices
 *   The number of slices to distribute between the currently defined Slice 1 and Slice 2 locations.
 *   
 * @param CreateMovieFile
 *   Set to TRUE if you want Tecplot to create a movie file.
 *   
 * @param MovieFName
 *   Name of the file to create if CreateMovieFile is TRUE.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAnimateSlices(
 *   &                   StartSlice,
 *   &                   EndSlice,
 *   &                   NumSlices,
 *   &                   CreateMovieFile,
 *   &                   MovieFName)
 *    INTEGER*4       StartSlice
 *    INTEGER*4       EndSlice
 *    INTEGER*4       NumSlices
 *    INTEGER*4       CreateMovieFile
 *    CHARACTER*(*)   MovieFName
 * </FortranSyntax>
 *   
 *   Animate 50 slices, using the currently defined Slice 1 and Slice 2 positions as the beginning and
 *   end of the animation. Send the animation to a movie file.
 *   
 * @code
 *   SmInteger_t NumSlices = 50;
 *             TecUtilAnimateSlices(1,
 *                                  NumSlices,
 *                                  NumSlices,
 *                                  TRUE,
 *                                  "SliceAnimation.rm");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAnimateSlices(SmInteger_t  StartSlice,
                                                   SmInteger_t  EndSlice,
                                                   SmInteger_t  NumSlices,
                                                   Boolean_t    CreateMovieFile,
                                                   const char  *MovieFName);
/**
 *   Delay Tecplot execution for a specified time.
 *   
 * @param Seconds
 *   Number of seconds to delay
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDelay(Seconds)
 *    INTEGER*4 Seconds
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilDelay(LgIndex_t Seconds);
/**
 *   Run a macro function. See the Tecplot Reference Manual for details about Tecplot macro functions.
 *   
 * @param QuickMacroName
 *   The name of the macro function to run
 *   
 * @param MacroParameters
 *   Any parameters which QuickMacroName requires. Pass NULL for macro functions which require no
 *   parameters
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroRunFunction(
 *   &                   QuickMacroName,
 *   &                   MacroParameters)
 *    CHARACTER*(*)   QuickMacroName
 *    CHARACTER*(*)   MacroParameters
 * </FortranSyntax>
 *   
 *   Run a macro function called "Calculate" which takes no parameters and another macro function
 *   called "Display" which takes the name of a layout file and an integer as parameters:
 *   
 * @code
 *   TecUtilMacroRunFunction("Calculate", (char *)NULL);
 *   TecUtilMacroRunFunction("Display", "(\"contour.lay\", 2)");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroRunFunction(const char *QuickMacroName,
                                                      const char *MacroParameters);
/**
 *   Set the title for the current data set.
 *   
 * @param DataSetTitle
 *   New title for the current frame's data set.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetSetTitle(DataSetTitle)
 *    CHARACTER*(*) DataSetTitle
 * </FortranSyntax>
 *   
 *   Set the current frame's data set title to be "Tecplot Data Set #1":
 *   
 * @code
 *   TecUtilDataSetSetTitle("Tecplot Data Set #1");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetSetTitle(const char *DataSetTitle);
/**
 *   Rename a data set variable in Tecplot.
 *   
 * @param VarNum
 *   The number of the variable to be renamed. The variable must be greater than zero and the variable
 *   must be enabled
 *   
 * @param VarName
 *   A string containing the new variable name. Must not be NULL
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarRename(
 *   &                   VarNum,
 *   &                   VarName)
 *    INTEGER*4       VarNum
 *    CHARACTER*(*)   VarName
 * </FortranSyntax>
 *   
 *   Rename the first variable (assumed to be enabled for this example):
 *   
 * @code
 *   TecUtilVarRename(1,"NewNameForVariable1");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarRename(EntIndex_t  VarNum,
                                               const char *VarName);
/**
 *   Rename a data set zone in Tecplot.
 *   
 * @param Zone
 *   The number of the zone to be renamed. The first zone in Tecplot is at position 1
 *   
 * @param ZoneName
 *   A string containing the new zone name. Must not be NULL
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneRename(
 *   &                   Zone,
 *   &                   ZoneName)
 *    INTEGER*4       Zone
 *    CHARACTER*(*)   ZoneName
 * </FortranSyntax>
 *   
 *   Rename the first zone:
 *   
 * @code
 *   TecUtilZoneRename(1,"New Zone Name");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneRename(EntIndex_t  Zone,
                                                const char *ZoneName);
/**
 *   Instruct Tecplot to submit and execute an operating system specific command.
 *   
 * @param Command
 *   Operating system specific command to execute
 *   
 * @param Wait
 *   If TRUE then Tecplot will wait for the command to finish processing. If FALSE then the command is
 *   run concurrently in another process and Tecplot returns immediately
 *   
 * @return
 *   On UNIX platforms, TRUE is returned if there is a shell to execute (such as /bin/ksh). On Windows
 *   platforms, TRUE is returned if the process is successfully created. Otherwise, FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSystem(
 *   &                   Command,
 *   &                   Wait)
 *    CHARACTER*(*)   Command
 *    INTEGER*4       Wait
 * </FortranSyntax>
 *   
 *   Use the polygrid utility supplied with Tecplot to translate a PolyGrid file to a Tecplot ASCII
 *   input data file.
 *   
 * @code
 *   TecUtilSystem("polygrid < sphere.pol > sphere.dat", TRUE);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilSystem(const char *Command,
                                            Boolean_t Wait);
/**
 * Add a title to a page in the Quick Macro Panel.
 *   
 * @param Title
 *   String containing the title. Each call to TecUtilMacroPanelAddTitle() adds
 *   a title to successive pages in the Quick Macro Panel
 *   
 * @return
 *   TRUE if successfull, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroPanelAddTitle(Title)
 *    CHARACTER*(*) Title
 * </FortranSyntax>
 *   
 *   Add the title "Flow Options" to the next macro panel:
 *   
 * @code
 *   TecUtilMacroPanelAddTitle("Flow Options");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroPanelAddTitle(const char *Title);
/**
 * Launch a Tecplot interface dialog. 
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *   
 * @param DialogToLaunch
 *   Dialog to launch. See Dialog_e for list of dialogs that you may launch.
 *   
 * @return
 *   For modelss dialog's TRUE is returned if the dialog was launch or FALSE if
 *   the dialog could not be launched programmatically in the given context.
 *   For modal dialog's TRUE is returned if the dialog was launched and the
 *   action was performed or FALSE if the user canceled the operation or the
 *   dialog could not be launched programmatically in the given context.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogLaunch(DialogToLaunch)
 *    INTEGER*4 DialogToLaunch
 * </FortranSyntax>
 *   
 *   Launch the Color Map dialog:
 * @code
 *   TecUtilDialogLaunch(Dialog_ColorMap);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogLaunch(Dialog_e DialogToLaunch);
/**
 *   Drop a Tecplot interface dialog.
 *   
 * @param DialogToDrop
 *   To remove Dialog from the screen. See Dialog in GLOBAL.h (or FGLUE.INC) for possible values.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogDrop(DialogToDrop)
 *    INTEGER*4 DialogToDrop
 * </FortranSyntax>
 *   
 *   Drop the Color Map dialog:
 *   
 * @code
 *   TecUtilDialogDrop(Dialog_ColorMap);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogDrop(Dialog_e DialogToDrop);
/**
 *   Run a macro file. See the Tecplot Reference Manual for details about Tecplot macro files.
 *   
 * @param FName
 *   Name of the file containing macro commands
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroRunFile(FName)
 *    CHARACTER*(*) FName
 * </FortranSyntax>
 *   
 *   Load and run the macro file mymacro.mcr:
 *   
 * @code
 *   TecUtilMacroRunFile("mymacro.mcr");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroRunFile(const char *FName);

/**
 * Allow or disallow the unloading of an add-on on Tecplot exit. Some add-ons
 * have demonstrated crashing problems if unloaded before Tecplot exits. Often
 * this is due to a 3rd party library linked with the add-on that registers
 * with the operating system what is typically called an "atexit" function to
 * automatically perform cleanup operations when the loading program exits if
 * the client neglects to perform them. This is a very bad thing for a
 * dynamically loaded/unloaded library to do. Well written libraries usually
 * provide an API for cleaning up and preventing the registration of an
 * "atexit" function. In the event that a 3rd party library does not provide an
 * API you may have to prevent the add-on from being unloaded by Tecplot with
 * this function.
 *   
 * @param AddOnID
 *   ID of the add-on
 *   
 * @param DoAllowUnload
 *   Set to FALSE to disable unloading of the add-on on Tecplot exit. Set to
 *   TRUE to allow unloading (the default).
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAddOnAllowUnload(
 *   &           AddOnIDPtr,
 *   &           DoAllowUnload)
 *    POINTER         (AddOnIDPtr, AddOnID)
 *    INTEGER*4       DoAllowUnload
 * </FortranSyntax>
 *   
 * Instruct Tecplot to no unload the add-on, identified by it's AddOnID, before
 * Tecplot exits.
 * @code
 *   TecUtilAddOnAllowUnload(AddonID,FALSE);
 * @endcode
 */
LINKTOADDON void      STDCALL TecUtilAddOnAllowUnload(AddOn_pa   AddOnID,
                                                      Boolean_t  DoAllowUnload);
/**
 * Load an add-on into Tecplot.
 *   
 * @param LibName
 *   Name of add-on file.
 *   
 * @param InitFunctionName
 *   Name of initialization function in add-on. If NULL is passed, the default
 *   name InitTecAddOn is used.
 *   
 * @param AddOnType
 *   The possible values are: LibraryType_V7Standard or LibraryType_V7ActiveX
 *   (Windows only).
 *   
 * @return
 *   Returns TRUE if the add-on was successfully loaded, otherwise, FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAddOnLoad(
 *   &                   LibName,
 *   &                   InitFunctionName,
 *   &                   AddOnType)
 *    CHARACTER*(*)   LibName
 *    CHARACTER*(*)   InitFunctionName
 *    INTEGER*4       AddOnType
 * </FortranSyntax>
 *   
 * To load the add-on cstream use:
 * @code
 *   if (!TecTuilAddOnLoad("cstream", NULL, LibraryType_V7Standard))
 *     {
 *       // failed to load cstream add-on 
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAddOnLoad(const char   *LibName,
                                               const char   *InitFunctionName,
                                               LibraryType_e AddOnType);


/**
 *   Checks to see if Tecplot is currently recording an animation.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExportIsRecording()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilExportIsRecording(void);
/**
 *   This function assembles all of the exported animation frames into the final animation file. Note
 *   that the final *.avi or *.rm file is not created until you call this function. This function will
 *   assert if there is no current animation.
 *   
 * @return
 *   TRUE if Tecplot is currently recording an animation, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExportFinish()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilExportFinish(void);

/**
 * This functions starts a new animation and exports the first frame. Use
 * TecUtilExportIsRecording() to make sure an export session is not in progress
 * before calling this function.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise. The return value will be FALSE only
 *   if Tecplot is unable to allocate a small amount of memory.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExportStart()
 * </FortranSyntax>
 *   
 *   This example makes use of the following export functions: \n
 *              TecUtilExportIsRecording() \n
 *              TecUtilExportSetup() \n
 *              TecUtilExportStart() \n
 *              TecUtilExportCancel() \n
 *              TecUtilExportNextFrame() \n
 *              TecUtilExportFinish() \n
 *   
 * @code
 *   This example may be used as a template when creating an animation routine.
 *   {
 *     int ii;
 *     Boolean_t IsOk;
 *     TecUtilLockStart(AddOnID);
 *   
 *     // Make sure there is no current export session before starting a new one.
 *     if ( TecUtilExportIsRecording() )
 *       {
 *         TecUtilDialogErrMsg("An export session is alread in progress.");
 *         IsOk = FALSE;
 *       }
 *     else
 *       {
 *         // Setup the export format and the file name. 
 *         TecUtilExportSetup(SV_EXPORTFNAME, NULL,
 *                            0.0, (ArbParam_t)"export.avi");
 *         TecUtilExportSetup(SV_EXPORTFORMAT, NULL,
 *                            0.0, (ArbParam_t)ExportFormat_AVI);
 *         // Start the export session. 
 *         IsOk = TecUtilExportStart();
 *       }
 *   
 *     if ( IsOk )
 *       {
 *         for (ii=0; ii<4; ii++)
 *           {
 *             // Rotate the plot around the PSI axis by 5 degrees. 
 *             IsOk = TecUtilViewRotate3D(RotateAxis_Psi,
 *                                        5.0,
 *                                        0.0,
 *                                        0.0,
 *                                        0.0,
 *                                        RotateOriginLocation_DefinedOrigin);
 *   
 *             // Export the new image. 
 *             if ( IsOk )
 *               {
 *                 IsOk = TecUtilExportNextFrame();
 *               }
 *   
 *             // If the rotation or the exporting of the next frame
 *             // was not sucessful, abandon the export session.
 *              
 *             if ( !IsOk )
 *               {
 *                 TecUtilExportCancel();
 *                 break;
 *               }
 *           }
 *       }
 *     // If everything was successful, finish the export session.
 *     if ( IsOk )
 *       IsOk = TecUtilExportFinish();
 *   
 *     if ( !IsOk )
 *       TecUtilDialogErrMsg("Error exporting, file may not be complete.");
 *   
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilExportStart(void);
/**
 * Export the next frame of a movie animation.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExportNextFrame()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilExportNextFrame(void);
/**
 * Cancel a movie export which is in progress. This function is only valid
 * after you have called TecUtilExportStart() but before you have called
 * TecUtilExportFinish(). It ensures that any resources used by the animation
 * are properly released and that no partial files are created or left on the
 * user's drive.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilExportCancel()
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL TecUtilExportCancel(void);

/**
 * Queries the 3-D viewer angles.
 *   
 * @param PsiAngle
 *   Receives the Psi angle. May be NULL
 *   
 * @param ThetaAngle
 *   Recevies the Theta angle. May be NULL
 *   
 * @param AlphaAngle
 *   Receives the Alpha angle. May be NULL
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilThreeDViewGetViewerAngle(
 *   &           PsiAngle,
 *   &           ThetaAngle,
 *   &           AlphaAngle)
 *    REAL*8          PsiAngle
 *    REAL*8          ThetaAngle
 *    REAL*8          AlphaAngle
 * </FortranSyntax>
 *   
 *   Query the 3-D view angles.
 *   
 * @code
 *   {
 *     double P,T,A;
 *     TecUtilLockStart(AddOnID);
 *     TecUtilThreeDViewGetAngle(&P,&T,&A);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilThreeDViewGetViewerAngle( double *PsiAngle,
                                                  double *ThetaAngle,
                                                  double *AlphaAngle );
/**
 * Queries the 3-D viewer angles.
 *   
 * @param XPos
 *   Receives the viewer X-location
 *   
 * @param YPos
 *   Recevies the viewer Y-location
 *   
 * @param ZPos
 *   Receives the viewer Z-location
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilThreeDViewGetViewerPos(
 *   &           XPos,
 *   &           YPos,
 *   &           ZPos)
 *    REAL*8          XPos
 *    REAL*8          YPos
 *    REAL*8          ZPos
 * </FortranSyntax>
 *   
 *   Query the 3-D viewer position.
 *   
 * @code
 *   {
 *     double XPos,YPos,ZPos;
 *     TecUtilLockStart(AddOnID);
 *     TecUtilThreeDViewGetViewerPos(&XPos,&YPos,&ZPos);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilThreeDViewGetViewerPos(double *XPos,
                                                       double *YPos,
                                                       double *ZPos);

/**
 * Queries the 3-D view attributes.
 *   
 * @param FieldOfView
 *   Receives the amount of the plot (in terms of spherical arc) in front of the viewer which may be
 *   seen. You may pass NULL for this parameter
 *   
 * @param ViewWidth
 *   Receives the amount of the plot (in X-axis units) in front of the viewer
 *   which may be seen. You may pass NULL for this parameter. Zooming in or out
 *   of a 3-D orthographic plot changes this number, but not the viewer's
 *   position.
 *   
 * @param IsInPerspective
 *   Receives TRUE if Tecplot is drawing the plot with perspective projection, FALSE otherwise. You may
 *   pass NULL for this parameter
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilThreeDViewGetProjection(
 *   &           FieldOfView,
 *   &           ViewWidth,
 *   &           IsInPerspective)
 *    REAL*8          FieldOfView
 *    REAL*8          ViewWidth
 *    INTEGER*4       IsInPerspective
 * </FortranSyntax>
 *   
 * Query the 3-D view attributes.
 *   
 * @code
 *   {
 *     double FieldOfView,WidthWidth;
 *     Boolean_t IsInPerspective;
 *   
 *     TecUtilLockStart(AddOnID);
 *     TecUtilThreeDViewGetProjection(&FieldOfView,
 *    &ViewWidth,
 *    &IsInPerspective);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilThreeDViewGetProjection(double    *FieldOfView,
                                                        double    *ViewWidth,
                                                        Boolean_t *IsInPerspective);

/**
 * Converts from 3-D world coordinates to Tecplot's 3-D grid coordinates. Note
 * that these 3-D grid coordinates are only to be used for zoom and 2-D grid
 * mode text and geometries in 3-D. Converted grid coordinates should be used
 * immediately. This transformation may change in future versions of Tecplot.
 * You must call TecUtilSetupTransformations() before calling this function,
 * although TecUtilSetupTransformations() need only be called once for each
 * sequence of calls to TecUtilConvert3DPositionToGrid().
 *   
 * @param XPosition
 *   X-position in world coordinates.
 *   
 * @param YPosition
 *   Y-position in world coordinates.
 *   
 * @param ZPosition
 *   Z-position in world coordinates.
 *   
 * @param XGridPosition
 *   Receives X-grid position. You may pass NULL for this parameter.
 *   
 * @param YGridPosition
 *   Receives Y-grid position. You may pass NULL for this parameter.
 *   
 * @param ZGridPosition
 *   Receives Z-grid position. You may pass NULL for this parameter.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilConvert3DPositionToGrid(
 *   &           XPosition,
 *   &           YPosition,
 *   &           ZPosition,
 *   &           XGridPosition,
 *   &           YGridPosition,
 *   &           ZGridPosition)
 *    REAL*8          XPosition
 *    REAL*8          YPosition
 *    REAL*8          ZPosition
 *    REAL*8          XGridPosition
 *    REAL*8          YGridPosition
 *    REAL*8          ZGridPosition
 * </FortranSyntax>
 *   
 *   Convert from 3-D world coordinates to Tecplot's 3-D grid coordinates.
 *   
 * @code
 *   {
 *     double ZPlaneFloor;
 *     double X,Y,Z;
 *   
 *     TecUtilLockStart(AddOnID);
 *     TecUtilSetupTransformations();
 *     ZPlaneFloor = TecUtil3DViewGetNearZPlane();
 *   
 *     TecUtilConvert3DPositionToGrid(1.0,1.0,ZPlaneFloor,&X,&Y,&Z);
 *   
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilConvert3DPositionToGrid(double XPosition,
                                                        double YPosition,
                                                        double ZPosition,
                                                        double *XGridPosition,
                                                        double *YGridPosition,
                                                        double *ZGridPosition);

/**
 * Returns the Z-value below which you can no longer trust any values of
 * TecUtilConvert3DPositionToGrid().
 *   
 * @return
 *   Returns the Z-value below which you can no longer trust any values of
 *   TecUtilConvert3DPositionToGrid().
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilThreeDViewGetNearZPlane()
 * </FortranSyntax>
 *   
 *   Convert from 3-D world coordinates to Tecplot's 3-D grid coordinates.
 *   
 * @code
 *   {
 *     double ZPlaneFloor;
 *     double X,Y,Z;
 *   
 *     TecUtilLockStart(AddOnID);
 *     TecUtilSetupTransformations();
 *     ZPlaneFloor = TecUtilThreeDViewGetNearZPlane();
 *   
 *     TecUtilConvert3DPositionToGrid(1.0,1.0,ZPlaneFloor,&X,&Y,&Z);
 *   
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON double STDCALL TecUtilThreeDViewGetNearZPlane(void);




/**
 * @deprecated
 *   Please use TecUtilThreeDViewGetNearZPlane() instead.
 */
LINKTOADDON double STDCALL TecUtil3DViewGetNearZPlane(void);




/**
 * Initializes transformation information so that coordinate conversion
 * functions can be used. This function must be called before any sequence of
 * calls to TecUtilConvert3DPositionToGrid().
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilSetupTransformations()
 * </FortranSyntax>
 *   
 *   Convert from 3-D world coordinates to Tecplot's 3-D grid coordinates.
 *   
 * @code
 *   {
 *     double ZPlaneFloor;
 *     double X,Y,Z;
 *   
 *     TecUtilLockStart(AddOnID);
 *     TecUtilSetupTransformations();
 *     ZPlaneFloor = TecUtil3DViewGetNearZPlane();
 *   
 *     TecUtilConvert3DPositionToGrid(1.0,1.0,ZPlaneFloor,&X,&Y,&Z);
 *   
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilSetupTransformations(void);
/**
 * Initialize lightweight frame pop. Lightweight frame pops have much less
 * overhead compared to TecUtilFramePop(), but you are more restricted in what
 * you can do after each pop. Thus this function can be used when:
 *
 *   * Speed is important
 *   * You plan to cycle through and pop all of the frames
 *   * You will not do anything which will cause a redraw of the frame after the frame pop.
 *   
 *  Call this function before calling TecUtilFrameLightweightPopNext().
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilFrameLightweightPopStart()
 * </FortranSyntax>
 *   
 *   Set the name of all frames to Hi Mom.
 *   
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     TecUtilFrameLightweightPopStart();
 *     do
 *       {
 *         TecUtilFrameSetName("Hi Mom");
 *       } while ( TecUtilFrameLightweightPopNext() );
 *   
 *   
 *     TecUtilFrameLightweightPopEnd();
 *     // Don't redraw until TecUtilFrameLightweightPopEnd() is called 
 *     TecUtilRedraw(TRUE);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilFrameLightweightPopStart(void);
/**
 * Pop the next frame. See TecUtilLightweightPopStart() for more information on
 * lightweight frame popping.
 *   
 * @return
 *   TRUE if there are more frames to pop, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameLightweightPopNext()
 * </FortranSyntax>
 *   
 *   Set the name of all frames to Hi Mom.
 *   
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     TecUtilFrameLightweightPopStart();
 *     do
 *       {
 *         TecUtilFrameSetName("Hi Mom");
 *       } while ( TecUtilFrameLightweightPopNext() );
 *   
 *   
 *     TecUtilFrameLightweightPopEnd();
 *     // Don't redraw until TecUtilFrameLightweightPopEnd() is called 
 *     TecUtilRedraw(TRUE);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilFrameLightweightPopNext(void);
/**
 * Ends a sequence of lightweight frame pops. See TecUtilLightweightPopStart()
 * for more information on lightweight frame popping.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilFrameLightweightPopEnd()
 * </FortranSyntax>
 *   
 *   Set the name of all frames to Hi Mom.
 *   
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     TecUtilFrameLightweightPopStart();
 *     do
 *       {
 *         TecUtilFrameSetName("Hi Mom");
 *       } while ( TecUtilFrameLightweightPopNext() );
 *   
 *   
 *     TecUtilFrameLightweightPopEnd();
 *     // Don't redraw until TecUtilFrameLightweightPopEnd() is called 
 *     TecUtilRedraw(TRUE);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilFrameLightweightPopEnd(void);

/* Custom Exporters */
/**
 * Call this function to initialize the export state if you are exporting using
 * the RGB functions.  Must be called before calling
 * TecUtilImageRGBGetScanLine() and TecUtilImageGetDimensions().
 *   
 * @param Region
 *   Region to export. Must be a valid region
 *   
 * @return
 *   TRUE if the export state was successfully initialized, FALSE otherwise
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImageRGBBitmapCreate(Region)
 *    INTEGER*4 Region
 * </FortranSyntax>
 *   
 *   Create an RGB (24-bit) bitmap.
 *   
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     if (TecUtilImageRGBBitmapCreate(BitDumpRegion_CurrentFrame))
 *       {
 *         // Do the export 
 *         TecUtilImageBitmapDestroy();
 *       }
 *   
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *   
 *   See the Example for TecUtilImageRGBBitmapCreate().
 *   
 * @code
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilImageRGBBitmapCreate(BitDumpRegion_e Region);
/**
 * Call this function to initialize the export state if you are exporting using
 * the index functions.  Must be called before calling
 * TecUtilImageIndexedGetScanLine() and TecUtilImageGetDimensions().
 *   
 * @param Region
 *   Region to export. Must be a valid region
 *   
 * @param RedColorTable_Array
 *   Receives the color table Red component. Caller must allocate this array
 *   and they must have 256 elements. You can also pass NULL for this arrays,
 *   using TecUtilImageGetColorTable() to get the color table. Note that in
 *   Version 9.0 or higher, the indices are guaranteed to be less than 256
 *   
 * @param GreenColorTable_Array
 *   Receives the color table Green component.
 *   
 * @param BlueColorTable_Array
 *   Receives the color table Blue component.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImageIndexedBitmapCreate(
 *   &                   Region,
 *   &                   RedColorTable_Array,
 *   &                   GreenColorTable_Array,
 *   &                   BlueColorTable_Array)
 *    INTEGER*4       Region
 *    INTEGER*4       RedColorTable_Array
 *    INTEGER*4       GreenColorTable_Array
 *    INTEGER*4       BlueColorTable_Array
 * </FortranSyntax>
 *   
 *   Create an indexed bitmap.
 *   
 * @code
 *   {
 *     short RedColorTable[256];
 *     short BlueColorTable[256];
 *     short GreenColorTable[256];
 *   
 *     TecUtilLockStart(AddOnID);
 *     if (TecUtilImageIndexedBitmapCreate(BitDumpRegion_CurrentFrame,
 *                                         RedColorTable,
 *                                         GreenColorTable,
 *                                         BlueColorTable))
 *       {
 *         // Bitmap has been created 
 *         TecUtilImageBitmapDestroy(); // When you're finished with it 
 *       }
 *   
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilImageIndexedBitmapCreate(BitDumpRegion_e  Region,
                                                              short           *RedColorTable_Array,
                                                              short           *GreenColorTable_Array,
                                                              short           *BlueColorTable_Array);
/**
 *   Destroy the bitmap buffer.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilImageBitmapDestroy()
 * </FortranSyntax>
 */
LINKTOADDON void      STDCALL TecUtilImageBitmapDestroy(void);
/**
 * Gets the image dimensions. You must call TecUtilImageRGBBitmapCreate()
 * before using this function.
 *   
 * @param Width
 *   Receives the width of the image in pixels. May be NULL.
 *   
 * @param Height
 *   Receives the height of the image in scan lines. May be NULL
 *   
 * @return
 *   TRUE if successful, FALSE otherwise
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImageGetDimensions(
 *   &                   Width,
 *   &                   Height)
 *    INTEGER*4       Width
 *    INTEGER*4       Height
 * </FortranSyntax>
 *   
 *   Create an indexed bitmap and get the dimensions.
 *   
 * @code
 *   {
 *     short Width,Height;
 *     short ColorTable[256]; // Must be at least 256 
 *   
 *     TecUtilLockStart(AddOnID);
 *   
 *     if (TecUtilImageIndexedBitmapCreate(BitDumpRegion_CurrentFrame,
 *                                         ColorTable))
 *       {
 *         TecUtilImageGetDimensions(&Width,&Height);
 *         // Dimensions are now in the Width and Height variables 
 *         TecUtilImageBitmapDestroy(); // Through with the bitmap 
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilImageGetDimensions(short *Width,
                                                        short *Height);
/**
 * Gets the RGB values of a scan line. The calling application must
 * allocate/free the arrays and ensure that they have enough space.
 *   
 * @param ScanLine
 *   One-based scan line to get the RGB values from. Must be between one and
 *   the height of the image
 *   
 * @param Red_Array
 *   Array which receives the RED components. Caller must allocate this array.
 *   May not be NULL.
 *   
 * @param Green_Array
 *   Array which receives the GREEN components. Caller must allocate this
 *   array. May not be NULL
 *   
 * @param Blue_Array
 *   Array which receives the BLUE components. Caller must allocate this array.
 *   May not be NULL
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImageRGBGetScanLine(
 *   &                   ScanLine,
 *   &                   Red_Array,
 *   &                   Green_Array,
 *   &                   Blue_Array)
 *    INTEGER*4       ScanLine
 *    INTEGER*4       Red_Array
 *    INTEGER*4       Green_Array
 *    INTEGER*4       Blue_Array
 * </FortranSyntax>
 *   
 *   Get the first scan line.
 *   
 * @code
 *   {
 *     // 24-bit color 
 *     short *Red;
 *     short *Green;
 *     short *Blue;
 *     short Width,Height;
 *   
 *     TecUtilLockStart(AddOnID);
 *   
 *     if (TecUtilImageRGBBitmapCreate(BitDumpRegion_CurrentFrame))
 *      {
 *        TecUtilImageGetDimensions(&Width,&Height);
 *        Red = (short*) malloc(Width * sizeof(short));
 *        Green = (short*) malloc(Width * sizeof(short));
 *        Blue = (short*) malloc(Width * sizeof(short));
 *    
 *        TecUtilImageRGBGetScanLine(1,Red,Green,Blue);
 *    
 *        TecUtilImageBitmapDestroy();
 *      }
 *   
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilImageRGBGetScanLine( short ScanLine,
                                                          short *Red_Array,
                                                          short *Green_Array,
                                                          short *Blue_Array);
/**
 *   Gets the color table indices for a scan line. The calling application must
 *   allocate/free the RGBIndex array and ensure that it has enough space.
 *   
 * @param ScanLine
 *   One-based scan line to get the RGB values from. Must be between one and
 *   the height of the image
 *   
 * @param RGBIndex_Array
 *   Array which receives the indicies. These values can be indexed into the
 *   color table returned by TecUtilImageIndexedBitmapCreate().
 *   
 * @return
 *   TRUE if successful, FALSE otherwise
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImageIndexedGetScanLine(
 *   &                   ScanLine,
 *   &                   RGBIndex_Array)
 *    INTEGER*4       ScanLine
 *    INTEGER*4       RGBIndex_Array
 * </FortranSyntax>
 *   
 *   Get the color table indices for the first scan line line.
 *   
 * @code
 *   {
 *     short Width,Height;
 *     short *RGBIndex = NULL;
 *     short ColorTableR[256]; // Must be at least 256 
 *     short ColorTableG[256];
 *     short ColorTableB[256];
 *   
 *     TecUtilLockStart(AddOnID);
 *   
 *     if  (TecUtilImageIndexedBitmapCreate(BitDumpRegion_CurrentFrame,
 *                                          ColorTableR)
 *                                          ColorTableG)
 *                                          ColorTableB)
 *       {
 *         TecUtilImageGetDimensions(&Width,&Height);
 *         RGBIndex = (short*) malloc(Width * sizeof(short));
 *         TecUtilImageIndexedGetScanLine(1, // scan lines are 1-based
 *                                        RGBIndex);
 *         TecUtilImageBitmapDestroy();
 *       }
 *   
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilImageIndexedGetScanLine( short ScanLine,
                                                              short *RGBIndex_Array);

/**
 *   Get the color table, that is, the palette, of a color-reduced image. You must call either
 *   TecUtilImageIndexedBitmapCreate() or TecUtilImageBitmapCreateX() before calling this function.
 *   The caller of this function must allocate three arrays of at least 256 bytes and pass them to
 *   this function.
 *   
 * @param Red_Array
 *   Pointer to 256-byte array which will receive the red values of the color table. Must not be NULL
 *   
 * @param Green_Array
 *   Pointer to 256-byte array which will receive the green values of the color table. Must not be NULL
 *   
 * @param Blue_Array
 *   Pointer to 256-byte array which will receive the blue values of the color table. Must not be NULL.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilImageGetColorTable(
 *   &           Red_Array,
 *   &           Green_Array,
 *   &           Blue_Array)
 *    INTEGER*4       Red_Array
 *    INTEGER*4       Green_Array
 *    INTEGER*4       Blue_Array
 * </FortranSyntax>
 *   
 *   
 * @code
 *   {
 *     Byte_t Red[256];
 *     Byte_t Green[256];
 *     Byte_t Blue[256];
 *     short ColorTable[256]; // Must be at least 256 
 *   
 *     TecUtilLockStart(AddOnID);
 *     TecUtilImageBitmapCreateX(NULL);
 *     // Will create a 256-color image. 
 *     TecUtilImageGetColorTable(Red,Green,Blue);
 *     // Export the image. 
 *     TecUtilImageBitmapDestroy();
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilImageGetColorTable(Byte_t *Red_Array,
                                                   Byte_t *Green_Array,
                                                   Byte_t *Blue_Array);

/**
 * Create a true color or color-reduced bitmap of a specified width.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_CONVERTTO256COLORS 
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set this to TRUE to create a color-reduced bitmap. A 256-color indexed bitmap will 
 *   be created. After rendering the image using this function, call 
 *   TecUtilImageGetColorTable() to get the palette, and TecUtilImageIndexedGetScanLine() 
 *   to retrieve the palette indices for a scan line.  Set this to FALSE to create a true 
 *   color image. After rendering the image using this function, call 
 *   TecUtilImageRGBGetScanLine() to get the red, green, and blue values for a scan line.
 *   
 * Name:
 *   SV_IMAGEWIDTH
 * Type:
 *   ScreenDim_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   512
 * Required:
 *   No
 * Notes:
 *   This value specifies the resolution in pixels of the created image. The
 *   height is automatically calculated based on the export region and the
 *   width. The approximate amount of memory required to generate an image,
 *   either color-reduced or true color, is Width by Height by three. Thus,
 *   very large values for this parameter will require a large amount of
 *   memory. This function will return FALSE if there is insufficient memory to
 *   render an image of the requested size
 *   
 * Name:
 *   SV_EXPORTREGION
 * Type:
 *   DumpRegion_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ExportRegion_CurrentFrame
 * Required:
 *   No
 * Notes:
 *   Export region. Set this ExportRegion_Currentframe, ExportRegion_AllFrames,
 *   or ExportRegion_WorkArea
 * </ArgListTable>
 *   
 * @return
 *   TRUE if the image was successfully created and rendered, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImageBitmapCreateX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   ArgList_pa ArgList;
 *   
 * @code
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendInt(ArgList,SV_CONVERTTO256COLORS,FALSE);
 *   // Create a true color image. 
 *   TecUtilArgListAppendInt(ArgList,SV_IMAGEWIDTH,1000);
 *   // The image will be rendered with a width of 1000 pixels. 
 *   TecUtilArgListAppendInt(ArgList,SV_EXPORTREGION,(LgIndex_t)ExportRegion_WorkArea);
 *   TecUtilImageBitmapCreateX(ArgList);
 *   // Export the image. 
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilImageBitmapCreateX(ArgList_pa ArgList);









