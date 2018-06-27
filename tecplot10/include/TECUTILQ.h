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
#if defined TECUTILQMODULE
#define EXTERN
#else
#define EXTERN extern
#endif


/*{{ <motif_only> TecUtilInterfaceGetMotifHandles </motif_only> }}*/

/*{{<exclude_fglue> 
                    TecUtilDataValueRefGetGetFunc FieldValueGetFunction_pf
                    TecUtilDataValueRefGetSetFunc FieldValueSetFunction_pf

  </exclude_fglue> }}*/


#ifdef MOTIF
/* Add new Motif only functions here */
/**
 * Get the handles to the graphical user interface. This function is available
 * only under UNIX.
 *   
 * @param AppContext
 *   The application context
 *   
 * @param MainWidget
 *   The widget for the Tecplot process main window.
 *   
 * Get the handles to Tecplot's GUI for use with Motif calls:
 *   
 * @code
 *   XtAppContext AppContext;
 *   Widget MainWidget;
 *   TecUtilInterfaceGetMotifHandles(&AppContext,&MainWidget);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilInterfaceGetMotifHandles(XtAppContext *AppContext,
                                                         Widget       *MainWidget);
#endif


/**
 * Indicates if the field layer of interest is active or not.
 *   
 * @param LayerShowFlag
 *   The show flag for the field layer of interest. Possible values are:
 *   SV_SHOWMESH, SV_SHOWCONTOUR, SV_SHOWVECTOR, SV_SHOWSCATTER, SV_SHOWSHADE
 *   or SV_SHOWBOUNDARY
 *   
 * @return
 *   TRUE if the specified field layer is active.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFieldLayerIsActive(LayerShowFlag)
 *    CHARACTER*(*) LayerShowFlag
 * </FortranSyntax>
 *   
 *   Determine if the field's contour layer is on:
 *   
 * @code
 *   if (TecUtilFieldLayerIsActive(SV_SHOWCONTOUR))
 *     {
 *       // do something with the field's contour level
 *   
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilFieldLayerIsActive(const char *LayerShowFlag);
/**
 *   Indicates if the line plot layer of interest is active or not.
 *   
 * @param LayerShowFlag
 *   Show flag for the line plot layer of interest.
 *   
 * @return
 *   TRUE if the specific layer in a line plot is active, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLinePlotLayerIsActive(LayerShowFlag)
 *    CHARACTER*(*) LayerShowFlag
 * </FortranSyntax>
 *   
 *   Determine if the line plot's symbol layer is on:
 *   
 * @code
 *   if (TecUtilLinePlotLayerIsActive(SV_SHOWSYMBOLS))
 *     {
 *       // Do something with the line plot's symbols
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilLinePlotLayerIsActive(const char *LayerShowFlag);



/**
 * @deprecated
 *   Please use TecUtilLinkingGetValue() instead.
 */
LINKTOADDON ArbParam_t STDCALL TecUtilFrameGetLinking(const char *Attribute); 
/**
 *   Gets frame linking attributes.
 *   
 * @param Attribute
 *   Valid values: SV_BETWEENFRAMES and SV_WITHINFRAME.
 *   
 * @param SubAttribute
 *   Attribute to set. For Attribute SV_BETWEENFRAMES the subattribute must be
 *   one of SV_LINKCONTOURLEVELS, SV_LINKFRAMESIZEANDPOSITION,
 *   SV_LINKXAXISRANGE, SV_LINKYAXISRANGE, SV_LINK3DVIEW, SV_LINKGROUP. For
 *   Attribute SV_WITHINFRAME the subattribute must be one of SV_LINKAXISSTYLE,
 *   SV_LINKGRIDLINESTYLE, SV_LINKLAYERLINECOLOR, SV_LINKLAYERLINEPATTERN
 *   
 * @return
 *   The type of return value is dependent upon the attribute parameter. If the
 *   subattribute is SV_LINKGROUP, the return value is the Group Number and
 *   should be cast to a SmInteger_t, otherwise the return value is TRUE or
 *   FALSE and should be cast to a Boolean_t.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLinkingGetValue(
 *   &           Attribute,
 *   &           SubAttribute,
 *   &           ResultPtr)
 *    CHARACTER*(*)   Attribute
 *    CHARACTER*(*)   SubAttribute
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Query the group number of the current frame:
 *   
 * @code
 *   SmInteger_t GroupNumber;
 *   GroupNumber = (SmInteger_t)TecUtilFrameGetLinking(SV_BETWEENFRAMES, 
 *               SV_LINKGROUP);
 * @endcode
 */
LINKTOADDON ArbParam_t STDCALL TecUtilLinkingGetValue(const char *Attribute,
                                                      const char *SubAttribute);
/**
 *   Get the dimensions of the currently defined paper in the Tecplot workspace.
 *   
 * @param Width
 *   Width of the paper (in inches).
 *   
 * @param Height
 *   Height of the paper (in inches).
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPaperGetDimensions(
 *   &           Width,
 *   &           Height)
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *   
 *   Get the current paper's position and size:
 *   
 * @code
 *   double width, height;
 *   TecUtilPaperGetDimensions(&width, &height);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilPaperGetDimensions(double *Width,
                                                   double *Height);

/**
 * Determine if the specified data point in the specified zone is visible or if
 * it is not drawn due to value-blanking or IJK-blanking.
 *   
 * @param Zone
 *   The number of the zone in which the PointIndex is located.
 *   
 * @param PointIndex
 *   The index of the point of interest.
 *   
 * @return
 *   TRUE if the data point is visible, FALSE if it is blanked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilBlankingCheckDataPoint(
 *   &                   Zone,
 *   &                   PointIndex)
 *    INTEGER*4       Zone
 *    INTEGER*4       PointIndex
 * </FortranSyntax>
 *   
 * Zone 4 is IJ-ordered and has IMax = 12, JMax = 8. Determine if the point
 * (I=3, J=2) of zone 4 is visible:
 *   
 * @code
 *   Boolean_t IsVisible = TecUtilBlankingCheckDataPoint(4,(2-1)*8+3);
 * @endcode
 *   
 *   Zone 5 is finite-element. Determine if the point (N=17) of zone 5 is visible:
 *   
 * @code
 *   
 *   Boolean_t IsVisible = TecUtilBlankingCheckDataPoint(5, 17);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilBlankingCheckDataPoint(EntIndex_t Zone,
                                                            LgIndex_t  PointIndex);
 
/**
 * Determine if the specified element in the specified finite-element zone is
 * visible or if it is not drawn due to value-blanking.
 *   
 * @param Zone
 *   The number of the zone in which the element is located. The zone must be finite-element.
 *   
 * @param CellIndex
 *   The index of the element of interest.
 *   
 * @return
 *   TRUE if the element is visible, FALSE if it is blanked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilBlankingCheckFECell(
 *   &                   Zone,
 *   &                   CellIndex)
 *    INTEGER*4       Zone
 *    INTEGER*4       CellIndex
 * </FortranSyntax>
 *   
 *   Zone 5 is finite-element. Determine if element number 13 of zone 5 is visible:
 *   
 * @code
 *   Boolean_t IsVisible =
 *    TecUtilBlankingCheckFECell(5, 13);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilBlankingCheckFECell(EntIndex_t Zone,
                                                         LgIndex_t  CellIndex);
 
/**
 *   Determine if the specified cell in the specified IJK-ordered zone is visible or if it is not drawn
 *   due to value-blanking or IJK-blanking.
 *   
 * @param Zone
 *   The number of the zone in which the cell is located. The zone must be IJK-ordered.
 *   
 * @param ZonePlane
 *   The plane in which the cell resides. For I or IJ-ordered data, use
 *   Planes_K. For IJK-ordered data, this determines what to use to determine
 *   the cell. The possible values are: Planes_I, Planes_J, Planes_K, or
 *   Planes_Volume.
 *   
 * @param CellIndex
 *   The index of the cell of interest.
 *   
 * @return
 *   TRUE if the cell is visible, FALSE if it is blanked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilBlankingCheckIJKCell(
 *   &                   Zone,
 *   &                   ZonePlane,
 *   &                   CellIndex)
 *    INTEGER*4       Zone
 *    INTEGER*4       ZonePlane
 *    INTEGER*4       CellIndex
 * </FortranSyntax>
 *   
 *   Zone 4 is IJ-ordered. Determine if cell 13 of zone 4 is visible:
 *   
 * @code
 *    Boolean_t IsVisible =
 *      TecUtilBlankingCheckIJKCell(4, Planes_K, 13);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilBlankingCheckIJKCell(EntIndex_t  Zone,
                                                          IJKPlanes_e ZonePlane,
                                                          LgIndex_t   CellIndex);

/**
 * Determine how many levels of locking are currently active in Tecplot. In
 * other words, return the number of nested calls to TecUtilLockStart() without
 * matching calls to TecUtilLockFinish(). See Chapter 10, "Locking and
 * Unlocking Tecplot," in the ADK User's Manual for more information on locks
 * in Tecplot.
 *   
 * @return
 *   The number of times Tecplot has been locked without being unlocked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLockGetCount()
 * </FortranSyntax>
 *   
 *   If Tecplot is not locked, lock it. Later, get the lock count.
 *   
 * @code
 *   if (!TecUtilLockIsOn())
 *     {
 *       int LockCount;
 *       Addon_pa AddonID =
 *       TecUtilAddOnRegister(10,"test","v1.23","author");
 *       TecUtilLockStart(AddonID);
 *       .
 *       .
 *       LockCount = TecUtilLockGetCount();
 *       //  LockCount will be at least 1
 *       .
 *       .
 *       TecUtilLockFinish(AddonID);
 *     }
 * @endcode
 */
LINKTOADDON int STDCALL TecUtilLockGetCount(void);
/**
 * Determine if Tecplot is locked. See the Chapter "Locking and Unlocking
 * Tecplot," in the ADK User's Manual for more information on locks in Tecplot.
 *   
 * @return
 *   TRUE, if Tecplot is locked. FALSE, if it is not locked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLockIsOn()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilLockIsOn(void);

/**
 * Get the version of Tecplot.
 *   
 * @return
 *   The current Tecplot version. This is expressed as 750000 for Tecplot 7.5,
 *   750105 for Tecplot 7.5-1-5, 900000 for Tecplot 9.0, and so on.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGetTecplotVersion()
 * </FortranSyntax>
 *   
 *   Get the current version of Tecplot:
 *   
 * @code
 *   LgIndex_t Ver = TecUtilGetTecplotVersion();
 *   if (Ver >= 750000) && (Ver <= 900000)
 *     .
 *     .
 *   else if (Ver >= 900000)
 *     .
 *     .
 * @endcode
 *
 * @deprecated
 *   Please use TecUtilTecplotGetMajorVersion(),
 *   TecUtilTecplotGetMinorVersion(), TecUtilTecplotGetMajorRevision(), and
 *   TecUtilTecplotGetMinorRevision() instead.
 */
LINKTOADDON LgIndex_t STDCALL TecUtilGetTecplotVersion(void);

/**
 * Gets Tecplot's major version number. Tecplot's version number has the form:
 * "MajorVersion.MinorVersion-MajorRevision-MinorRevision". Each part can be
 * obtained separately.
 *
 * @since
 *   10.0-3-129
 *
 * @return
 *   Tecplot's major version number.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecplotGetMajorVersion()
 * </FortranSyntax>
 *   
 * @sa TecUtilTecplotGetMinorVersion(), TecUtilTecplotGetMajorRevision(),
 *     TecUtilTecplotGetMinorRevision()
 */
LINKTOADDON SmInteger_t STDCALL TecUtilTecplotGetMajorVersion(void);

/**
 * Gets Tecplot's minor version number. Tecplot's version number has the form:
 * "MajorVersion.MinorVersion-MajorRevision-MinorRevision". Each part can be
 * obtained separately.
 *
 * @since
 *   10.0-3-129
 *
 * @return
 *   Tecplot's minor version number.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecplotGetMinorVersion()
 * </FortranSyntax>
 *
 * @sa TecUtilTecplotGetMajorVersion(), TecUtilTecplotGetMajorRevision(),
 *     TecUtilTecplotGetMinorRevision()
 */
LINKTOADDON SmInteger_t STDCALL TecUtilTecplotGetMinorVersion(void);

/**
 * Gets Tecplot's major revision number. Tecplot's version number has the form:
 * "MajorVersion.MinorVersion-MajorRevision-MinorRevision". Each part can be
 * obtained separately.
 *
 * @since
 *   10.0-3-129
 *
 * @return
 *   Tecplot's major revision number.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecplotGetMajorRevision()
 * </FortranSyntax>
 *
 * @sa TecUtilTecplotGetMajorVersion(), TecUtilTecplotGetMinorVersion(),
 *     TecUtilTecplotGetMinorRevision()
 */
LINKTOADDON SmInteger_t STDCALL TecUtilTecplotGetMajorRevision(void);

/**
 * Gets Tecplot's minor revision number. Tecplot's version number has the form:
 * "MajorVersion.MinorVersion-MajorRevision-MinorRevision". Each part can be
 * obtained separately.
 *
 * @since
 *   10.0-3-129
 *
 * @return
 *   Tecplot's minor revision number.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecplotGetMinorRevision()
 * </FortranSyntax>
 *
 * @sa TecUtilTecplotGetMajorVersion(), TecUtilTecplotGetMinorVersion(),
 *     TecUtilTecplotGetMajorRevision()
 */
LINKTOADDON SmInteger_t STDCALL TecUtilTecplotGetMinorRevision(void);

/**
 * Get the Tecplot home directory.
 *   
 * @return
 *   The Tecplot home directory. This is specified by the TEC100HOME
 *   environment variable, or the -h flag used when launching Tecplot from the
 *   command line.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTecplotGetHomeDirectory(
 *   &           Result,
 *   &           ResultLength)
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *   
 *   Get the Tecplot home directory.
 *   
 * @code
 *   char *TecHome = NULL;
 *   
 *   TecUtilLockStart(AddOnID);
 *   TecHome = TecUtilTecplotGetHomeDirectory();
 *   .....
 *   TecUtilStringDealloc(&TecHome);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON char * STDCALL TecUtilTecplotGetHomeDirectory(void);

/**
 *   Get a count of the number of frames currently defined.
 *   
 * @return
 *   Number of frames defined.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameGetCount()
 * </FortranSyntax>
 */
LINKTOADDON int STDCALL TecUtilFrameGetCount(void);



/**
 * @deprecated
 *   Please use TecUtilFrameGetPlotType() instead.
 */
LINKTOADDON FrameMode_e STDCALL TecUtilFrameGetMode(void);




/**
 *   Get the plot type of the current frame.
 *   
 * @return
 *   The Plot Type can be one of the following possible values: PlotType_Automatic,
 *   PlotType_Cartesian2D, PlotType_Cartesian3D, PlotType_XYLine or PlotType_Sketch
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameGetPlotType()
 * </FortranSyntax>
 *   
 *   Do something when the frame mode is XY Line:
 *   
 * @code
 *   if ( TecUtilFrameGetPlotType() == PlotType_XYLine )
 *     {
 *       // do something
 *     }
 * @endcode
 */
LINKTOADDON PlotType_e STDCALL TecUtilFrameGetPlotType(void);





/**
 * Get the number of items currently in the pick list. See Section "The
 * Pick List," in the ADK User's Manual for a discussion of pick lists.
 *   
 * @return
 *   Returns the number of items currently in the pick list.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetCount()
 * </FortranSyntax>
 *   
 *   Get the number of items in the pick list and loop through the pick list:
 *   
 * @code
 *   int PickIndex, NumPickedItems = TecUtilPickListGetCount();
 *   for (PickIndex = 1; PickIndex <= NumPickedItems; PickIndex++)
 *     {
 *        //   Go through each object in the pick list using   
 *        //   TecUtilPickListGetType(PickIndex) to determine  
 *        //   the type of object                              
 *     }
 * @endcode
 */
LINKTOADDON int STDCALL TecUtilPickListGetCount(void);





/**
 *   Gets the type of object from the pick list at the specified index. See
 *   Section "The Pick List," in the ADK User's Manual for a discussion
 *   of pick lists.
 *   
 * @param PickListIndex
 *   Index into the pick list
 *   
 * @return
 *   The type of object from the pick list at the specified index.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetType(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *   
 *   Go through the pick list, checking object types:
 * @code
 *   int PickIndex, NumPickedItems = TecUtilPickListGetCount();
 *   for (PickIndex = 1; PickIndex <= NumPickedItems; PickIndex++)
 *     {
 *       PickObjects_e ObjectType = TecUtilPickListGetType(PickIndex);
 *       switch (ObjectType)
 *        {
 *        case PickObject_Geom :
 *          // Do something with picked geometries
 *          break;
 *        case PickObject_Text :
 *          // Do something with picked text
 *          break;
 *        case PickObject_Zone :
 *          // Do something with picked zones
 *          break;
 *        case PickObject_LineMapping :
 *          // Do something with picked line mappings
 *          break;
 *          .
 *          .
 *        }
 *     }
 * @endcode
 */
LINKTOADDON PickObjects_e STDCALL TecUtilPickListGetType(int PickListIndex);





/**
 *   Get the name of the frame from the pick list at the specified index. The
 *   object in the pick list at the specified index must be of type
 *   PickObject_Frame. See Section "The Pick List," in the ADK User's
 *   Manual for a discussion of pick lists.
 *   
 * @param PickListIndex
 *   Index into the pick list.
 *   
 * @return
 *   The name of the frame from the pick list at the specified index. You must
 *   call TecUtilStringDealloc() on the returned string.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPickListGetFrameName(
 *   &           PickListIndex,
 *   &           Result,
 *   &           ResultLength)
 *    INTEGER*4       PickListIndex
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *   
 * If the first object in the pick list is a frame, get its name:
 * @code
 *   if (TecUtilPickListGetType(1) == PickObject_Frame)
 *     {
 *       char *FrameName = TecUtilPickListGetFrameName(1);
 *       .
 *       .
 *       TecUtilStringDealloc(&FrameName);
 *     }
 * @endcode
 */
LINKTOADDON char * STDCALL TecUtilPickListGetFrameName(int PickListIndex);





/**
 * Get the unique identifier of the frame from the pick list at the specified
 * index. The object in the pick list at the specified index must be of type
 * PickObject_Frame. See Section "The Pick List," in the ADK User's
 * Manual for a discussion of pick lists.
 *   
 * @param PickListIndex
 *   Index into the pick list
 *   
 * @return
 *   The unique identifier of the frame from the pick list at the specified index.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetFrameUniqueID(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *   
 *   If the first object in the pick list is a frame, get its unique identifier:
 *   
 * @code
 *   if (TecUtilPickListGetType(1) == PickObject_Frame)
 *     {
 *       UniqueID_t UniqueID = TecUtilPickListGetFrameUniqueID(1);
 *       .
 *       .
 *       .
 *     }
 * @endcode
 */
LINKTOADDON UniqueID_t STDCALL TecUtilPickListGetFrameUniqueID(int PickListIndex);




/**
 *   Get the current minimum and maximum values for the specified axis.
 *   
 * @param Axis
 *   The axis to query. This can be one of `X', `Y' or `Z'
 *   
 * @param AxisNum
 *   The axis number. For XY-plots this can be any number from one to five. For all other plots this
 *   must be one.
 *   
 * @param AxisMin
 *   The current axis minimum value
 *   
 * @param AxisMax
 *   The current axis maximum value
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAxisGetRange(
 *   &           Axis,
 *   &           AxisNum,
 *   &           AxisMin,
 *   &           AxisMax)
 *    CHARACTER*(*)   Axis
 *    INTEGER*4       AxisNum
 *    REAL*8          AxisMin
 *    REAL*8          AxisMax
 * </FortranSyntax>
 *   
 *   Get the range on Y-Axis number 2 (assume that it was previously determined
 *   that the frame mode is currently XY):
 *   
 * @code
 *   double YMin,YMax;
 *   TecUtilAxisGetRange('Y',2,&YMin,&YMax);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilAxisGetRange(char    Axis,
                                             short   AxisNum,
                                             double *AxisMin,
                                             double *AxisMax);








/**
 * Get the kind of axis (X, Y, or Z) from the pick list at the specified index.
 * The object in the pick list at the specified index must be of type
 * PickObject_Axis. See Section "The Pick List," in the ADK User's Manual
 * for a discussion of pick lists.
 *   
 * @param PickListIndex
 *   Index into the pick list
 *   
 * @return
 *   The kind of axis from the pick list at the specified index.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetAxisKind(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *   
 *   If an axis object is picked, get its type and number:
 *   
 * @code
 *   int PickIndex, NumPickedItems = TecUtilPickListGetCount();
 *   for (PickIndex = 1; PickIndex <= NumPickedItems; PickIndex++)
 *     {
 *       if (TecUtilPickListGetType(PickIndex) == PickObject_Axis)
 *         {
 *           char AxisKind = TecUtilPickListGetAxisKind(PickIndex);
 *           int  AxisNum = TecUtilPickListGetAxisNumber(PickIndex);
 *           .
 *           .
 *         }
 *     }
 * @endcode
 */





LINKTOADDON char STDCALL TecUtilPickListGetAxisKind(int PickListIndex);
/**
 * Get the number of the axis from the pick list at the specified index. The
 * object in the pick list at the specified index must be of type
 * PickObject_Axis. See Section "The Pick List," in the ADK User's Manual
 * for a discussion of pick lists.
 *   
 * @param PickListIndex
 *   Index into the pick list
 *   
 * @return
 *   For Sketch, 2-D, and 3-D plots, the return value is one. For XY plots, returns the axis number
 *   from the pick list at the specified index.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetAxisNumber(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 */




LINKTOADDON int STDCALL TecUtilPickListGetAxisNumber(int PickListIndex);
/**
 * Get the number of the zone from the pick list at the specified index. The
 * object in the pick list at the specified index must be of type
 * PickObject_Zone. See Section "The Pick List," in the ADK User's Manual
 * for a discussion of pick lists.
 *   
 * @param PickListIndex
 *   Index into the pick list
 *   
 * @return
 *   The number of the zone from the pick list at the specified index.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetZoneNumber(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 */





LINKTOADDON EntIndex_t STDCALL TecUtilPickListGetZoneNumber(int PickListIndex);
/**
 * Get the specific point that was selected in the zone from the pick list at
 * the specified index.  The object in the pick list at the specified index
 * must be of type PickObject_Zone. See Section "The Pick List," in the
 * ADK User's Manual for a discussion of pick lists.
 *   
 * @param PickListIndex
 *   Index into the pick list
 *   
 * @param IIndex
 *   The I-index value of the point that was selected in the zone
 *   
 * @param JIndex
 *   The J-index value of the point that was selected in the zone
 *   
 * @param KIndex
 *   The K-index value of the point that was selected in the zone
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPickListGetZoneIndices(
 *   &           PickListIndex,
 *   &           IIndex,
 *   &           JIndex,
 *   &           KIndex)
 *    INTEGER*4       PickListIndex
 *    INTEGER*4       IIndex
 *    INTEGER*4       JIndex
 *    INTEGER*4       KIndex
 * </FortranSyntax>
 *   
 *   If the first object in the pick list is a zone, get its number and indices:
 *   
 * @code
 *   if (TecUtilPickListGetType(1) == PickObject_Zone)
 *     {
 *      LgIndex_t  IIndex, JIndex, KIndex;
 *      EntIndex_t ZoneNumber = TecUtilPickListGetZoneNumber(1);
 *      TecUtilPickListGetZoneIndices(1, &IIndex, &JIndex, &KIndex);
 *      .
 *      .
 *     }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilPickListGetZoneIndices(int        PickListIndex,
                                                       LgIndex_t *IIndex,
                                                       LgIndex_t *JIndex,
                                                       LgIndex_t *KIndex);





/**
 * @deprecated
 *   Please use TecUtilPickListGetLineMapNumber() instead.
 */
LINKTOADDON EntIndex_t STDCALL TecUtilPickListGetXYMapNumber(int PickListIndex);




/**
 * Get the number of the Line-mapping from the pick list at the specified
 * index. The object in the pick list at the specified index must be of type
 * PickObject_LineMapping. See Section "The Pick List," in the ADK User's
 * Manual for a discussion of pick lists.
 *   
 * @param PickListIndex
 *   Index into the pick list.
 *   
 * @return
 *   The number of the Line-mapping from the pick list at the specified index.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetLineMapNumber(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 */
LINKTOADDON EntIndex_t STDCALL TecUtilPickListGetLineMapNumber(int PickListIndex);



/**
 * @deprecated
 *   Please use TecUtilPickListGetLineMapIndex() instead.
 */
LINKTOADDON LgIndex_t STDCALL TecUtilPickListGetXYMapIndex(int PickListIndex);





/**
 * Get the index value of the specific point that was selected in the
 * Line-mapping from the pick list at the specified index. The object in the
 * pick list at the specified index must be of type PickObject_LineMapping. In
 * order to get useful information from this function, the mouse mode should be
 * of the type Mouse_Adjust. See Section "The Pick List," in the ADK
 * User's Manual for a discussion of pick lists.
 *   
 * @param PickListIndex
 *   Index into the pick list.
 *   
 * @return
 *   The index value of the specific point that was selected in the
 *   Line-mapping from the pick list at the specified index if the Adjustor
 *   mode is being used. Otherwise, zero.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickListGetLineMapIndex(PickListIndex)
 *    INTEGER*4 PickListIndex
 * </FortranSyntax>
 *   
 *   If the first object in the pick list is an Line-mapping, get its number and index:
 *   
 * @code
 *   if (TecUtilPickListGetType(1) == PickObject_LineMapping)
 *     {
 *       EntIndex_t MapNumber = TecUtilPickListGetLineMapNumber(1);
 *       LgIndex_t  MapIndex  = TecUtilPickListGetLineMapIndex(1);
 *   
 *       // Do Something here
 *   
 *     }
 * @endcode
 */
LINKTOADDON LgIndex_t STDCALL TecUtilPickListGetLineMapIndex(int PickListIndex);









/**
 * Get the text from the pick list at the specified index. The object in the
 * pick list at the specified index must be of type PickObject_Text. See
 * Section "The Pick List," in the ADK User's Manual for a discussion of
 * pick lists.
 *   
 * @param PickListIndex
 *   Index into the pick list
 *   
 * @return
 *   The text ID from the pick list at the specified index.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPickListGetText(
 *   &           PickListIndex,
 *   &           ResultPtr)
 *    INTEGER*4      PickListIndex
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   For every text object which is picked, set it to have a filled text box:
 *   
 * @code
 *   int PickIndex, NumPickedItems = TecUtilPickListGetCount();
 *   for (PickIndex = 1; PickIndex <= NumPickedItems; PickIndex++)
 *     {
 *       if (TecUtilPickListGetType(PickIndex) == PickObject_Text)
 *         {
 *           Text_ID TID = TecUtilPickListGetText(PickIndex);
 *           TecUtilTextBoxSetType(TID, TextBox_Filled);
 *         }
 *     }
 * @endcode
 */
LINKTOADDON Text_ID STDCALL TecUtilPickListGetText(int PickListIndex);



/**
 * Get the geometry from the pick list at the specified index. The object in
 * the pick list at the specified index must be of type PickObject_Geom. See
 * Section "The Pick List," in the ADK User's Manual for a discussion of
 * pick lists.
 *   
 * @param PickListIndex
 *   Index into the pick list
 *   
 * @return
 *   The geometry ID from the pick list at the specified index.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPickListGetGeom(
 *   &           PickListIndex,
 *   &           ResultPtr)
 *    INTEGER*4      PickListIndex
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 * If a specific point of a polyline geometry object is picked, set the
 * coordinates for that point to be X = 4.5, Y = 3.2 :
 *   
 * @code
 *   int PickIndex, NumPickedItems = TecUtilPickListGetCount();
 *   for (PickIndex = 1; PickIndex <= NumPickedItems; PickIndex++)
 *     {
 *       if (TecUtilPickListGetType(PickIndex) == PickObject_Geom)
 *         {
 *           SmInteger_t PolylineNum;
 *           LgIndex_t   PointIndex;
 *           Geom_ID GID = TecUtilPickListGetGeom(PickIndex);
 *           if (TecUtilGeomGetType(GID) == GeomForm_LineSegs)
 *             TecUtilPickListGetGeomInfo(PickIndex,
 *                                        &PolylineNum,
 *                                        &PointIndex);
 *           if ((PolylineNum > 0) && (PointIndex > 0))
 *             TecUtilGeom2DMPolySetPoint(GID, PolylineNum,
 *                                        PointIndex, 4.5, 3.2);
 *         }
 *     }
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilPickListGetGeom(int PickListIndex);
/**
 * Get the specific point that was selected in the geometry from the pick list
 * at the specified index. The object in the pick list at the specified index
 * must be of type PickObject_Geom. In order to get useful information from
 * this function, the geometry should be of type GeomForm_LineSegs or
 * GeomForm_LineSegs3D. The mouse mode should be Mouse_Adjust. See Section
 * "The Pick List," in the ADK User's Manual for a discussion of pick
 * lists.
 *   
 * @param PickListIndex
 *   Index into the pick list.
 *   
 * @param PolylineNum
 *   The number of the polyline that was selected if the geometry is a line
 *   segment and the Adjustor mode is being used. Otherwise, zero
 *   
 * @param PointIndex
 *   The index of the specific point that was selected in the polyline if the
 *   geometry is a line segment and the Adjustor mode is being used. Otherwise,
 *   zero.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPickListGetGeomInfo(
 *   &           PickListIndex,
 *   &           PolylineNum,
 *   &           PointIndex)
 *    INTEGER*4       PickListIndex
 *    INTEGER*4       PolylineNum
 *    INTEGER*4       PointIndex
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL TecUtilPickListGetGeomInfo(int          PickListIndex,
                                                    SmInteger_t *PolylineNum,
                                                    LgIndex_t   *PointIndex);

/**
 *   Gets the minimum and maximum values of a variable.
 *   
 * @param Var
 *   Index of the variable. Must be greater than zero and the variable must be
 *   enabled
 *   
 * @param VarMin
 *   Receives the minimum value of the variable. Must not be NULL
 *   
 * @param VarMax
 *   Receives the maximum value of the variable. Must not be NULL
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilVarGetMinMax(
 *   &           Var,
 *   &           VarMin,
 *   &           VarMax)
 *    INTEGER*4       Var
 *    REAL*8          VarMin
 *    REAL*8          VarMax
 * </FortranSyntax>
 *   
 *   Get the minimum and maximum values of the first variable in a data set:
 *   
 * @code
 *   double VarMin,VarMax;
 *   TecUtilVarGetMinMax(1,&VarMin,&VarMax);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilVarGetMinMax(EntIndex_t Var,
                                             double     *VarMin,
                                             double     *VarMax);


/**
 *   Get the indices for the nodes of a finite-element cell.
 *   
 * @param Zone
 *   Zone in which the cell exists. This must be a finite-element zone.
 *   
 * @param Face
 *   Face of the finite-element cell. If the zone uses tetrahedrons this is a
 *   number between one and four. If the zone uses bricks this is a number
 *   between one and six. If the zone uses triangles or quadrilaterals then
 *   this is ignored.
 *   
 * @param CellIndex
 *   The cell index (that is, the element number) to query.
 *   
 * @param I1
 *   First Node index.
 *   
 * @param I2
 *   Second Node index.
 *   
 * @param I3
 *   Third Node index.
 *   
 * @param I4
 *   Fourth Node index (not valid if triangle element).
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFECellGetNodes(
 *   &           Zone,
 *   &           Face,
 *   &           CellIndex,
 *   &           I1,
 *   &           I2,
 *   &           I3,
 *   &           I4)
 *    INTEGER*4       Zone
 *    INTEGER*4       Face
 *    INTEGER*4       CellIndex
 *    INTEGER*4       I1
 *    INTEGER*4       I2
 *    INTEGER*4       I3
 *    INTEGER*4       I4
 * </FortranSyntax>
 *   
 *   Get the node indices for face number 2 of cell number 23 in zone 3:
 *   
 * @code
 *   LgIndex_t I1,I2,I3,I4;
 *   
 *   // Add code here to make sure data exists, and the zone
 *   // exists and is the right size and type
 *   TecUtilDataFECellGetNodes(3,
 *                             2,
 *                             23,
 *                             &I1,&I2,&I3,&I4);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDataFECellGetNodes(EntIndex_t Zone,
                                                   int        Face,
                                                   LgIndex_t  CellIndex,
                                                   LgIndex_t *I1,
                                                   LgIndex_t *I2,
                                                   LgIndex_t *I3,
                                                   LgIndex_t *I4);

/**
 * Get the indices for the nodes a cell in an ordered zone.
 *   
 * @param Zone
 *   Zone in which the cell exists
 *   
 * @param Plane
 *   Plane in which the cell resides. The possible values are: Planes_I,
 *   Planes_J, Planes_K or Planes_Volume. For I- or IJ-ordered data use
 *   Planes_K. For IJK-ordered data this determines which of the three faces
 *   (I, J, or K) to use to determine which cell to query.
 *   
 * @param CellIndex
 *   The index of the lowest indexed corner of the cell to query
 *   
 * @param I1
 *   First node index for the cell. If the zone is IJ-ordered or IJK-ordered,
 *   these indices are calculated from treating the two- or three-dimensional
 *   array as a one-dimensional array.
 *   
 * @param I2
 *   Second node index for the cell. If the zone is IJ-ordered or IJK-ordered,
 *   these indices are calculated from treating the two- or three-dimensional
 *   array as a one-dimensional array.
 *   
 * @param I3
 *   Third node index for the cell. If the zone is IJ-ordered or IJK-ordered,
 *   these indices are calculated from treating the two- or three-dimensional
 *   array as a one-dimensional array.
 *   
 * @param I4
 *   Fourth node index for the cell. If the zone is IJ-ordered or IJK-ordered,
 *   these indices are calculated from treating the two- or three-dimensional
 *   array as a one-dimensional array.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataIJKCellGetIndices(
 *   &           Zone,
 *   &           Plane,
 *   &           CellIndex,
 *   &           I1,
 *   &           I2,
 *   &           I3,
 *   &           I4)
 *    INTEGER*4       Zone
 *    INTEGER*4       Plane
 *    INTEGER*4       CellIndex
 *    INTEGER*4       I1
 *    INTEGER*4       I2
 *    INTEGER*4       I3
 *    INTEGER*4       I4
 * </FortranSyntax>
 *   
 * Get the node indices for the face that lies in the J-plane of the cell that
 * resides at (1, 1, 1) in zone 2 which is an IJK-ordered data set:
 *   
 * @code
 *   LgIndex_t I1,I2,I3,I4;
 *   
 *   // Add code here to make sure data exists and the zone
 *   // exists and is the right size and type
 *   TecUtilDataIJKCellGetIndices(2,
 *                                Planes_J,
 *                                1,
 *                                &I1,&I2,&I3,&I4);
 *   
 *   LgIndex_t I1,I2,I3,I4;
 *   LgIndex_t CellIndex;
 *   
 *   // Add code here to make sure data exists, and the zone
 *   // exists and is the right size and type
 *   
 *   CellIndex = 2+10*((5-1)+20*(13-1));
 *   
 *   TecUtilDataIJKCellGetIndices(2,
 *                                Planes_K,
 *                                CellIndex,
 *                                &I1,&I2,&I3,&I4);
 * @endcode
 *   
 * Get the nodel indices for the face that lies in the K-plane of the cell that
 * resides at (2, 5, 13) in zone 2, which is an IJK-ordered data set
 * dimensioned 10 by 20 by 30:
 *   
 * @code
 *   
 *   LgIndex_t I1,I2,I3,I4;
 *   LgIndex_t CellIndex;
 *   
 *   // Add code here to make sure data exists, and the zone
 *   // exists and is the right size and type
 *   
 *   CellIndex = 2+10*((5-1)+20*(13-1));
 *   
 *   TecUtilDataIJKCellGetIndices(2,
 *                                Planes_K,
 *                                CellIndex,
 *                                &I1,&I2,&I3,&I4);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDataIJKCellGetIndices(EntIndex_t  Zone,
                                                      IJKPlanes_e Plane,
                                                      LgIndex_t   CellIndex,
                                                      LgIndex_t  *I1,
                                                      LgIndex_t  *I2,
                                                      LgIndex_t  *I3,
                                                      LgIndex_t  *I4);




/**
 * Low level function used to get most frame and base attribute values in
 * Tecplot. The parameters to TecUtilStyleGetLowLevelX() mimic the Macro Frame
 * SetValue Commands described in the Tecplot Reference Manual.
 *   
 * @par Note:
 *   The ArgList entries described below define the atributes to get.
 *   Attributes in Tecplot are defined hierarchically. These parameters follow
 *   the same order as you would use when constructing a macro comman to set a
 *   value. These parameters are actually strings but you should use the
 *   supplied SV_constants from the SV.h include file. Using the SV_ constants
 *   will help prevent misspellings and other errors.  At the time of printing,
 *   only the following SV_P1 commands are available: SV_FIELD, SV_LINEMAP,
 *   SV_GLOBALCONTOUR, SV_GLOBALSCATTER, SV_GLOBALSOSURFACE, SV_GLOBALSLICE,
 *   SV_GLOBALTWODVECTOR, and SV_GLOBALTHREEDVECTOR.
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
 * Default:
 *   ---
 * Required:
 *   No
 * Notes:
 *   
 * Name:
 *   SV_P2
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   ---
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
 *   ---
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
 *   ---
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
 *   ---
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
 *   ---
 * Required:
 *   No
 *   
 * Name:
 *   SV_OFFSET1
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ---
 * Required:
 *   No
 * Notes:
 *   Depending on the command the first offset is used to denote the zone, line
 *   map, or contour group of interest.
 *   
 * Name:
 *   SV_OFFSET2
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ---
 * Required:
 *   No
 * Notes:
 *   The second offset is only used if the first is already being used to
 *   identify a zone, linemap or contour group such as color map override
 *   number for a specified contour group
 *   
 * Name:
 *   SV_DVALUE
 * Type:
 *   double *
 * Arg Function:
 *   TecUtilArgListAppendDoublePtr()
 * Default:
 *   ---
 * Required:
 *   No
 * Notes:
 *   Address to a variable of type double where the double valued result of the
 *   query can be stored
 *   
 * Name:
 *   SV_IVALUE
 * Type:
 *   ArbParam_t *
 * Arg Function:
 *   TecUtilArgListAppendArbParamPtr()
 * Default:
 *   ---
 * Required:
 *   No
 * Notes:
 *   Address to a variable of type ArbParam_t where the ArbParam_t valued
 *   result of the query can be stored. Note that some queries assign values
 *   that were allocated and must be deallocated by the addon. If the resulting
 *   ArbParam_t value is a string it must be deallocated using
 *   TecUtilStringDealloc(). If is is an array it must be deallocated using
 *   TecUtilArrayDealloc().
 * </ArgListTable>
 *   
 * @return
 *   The function return value is of type GetValueReturnCode_e with the
 *   following possible values:
 *   
 * @verbatim
     GetValue_Ok              Value was assigned to either the double 
                              or ArbParam_t value given.
  
     GetValue_ResultTypeError Resulting type mismatch with the supplied 
                              DValue or IValue.
     
     GetValue_SyntaxError     SV_P# parameters did not follow the macro 
                              command syntax. The parameters must mimic the 
                              macro command language and only for the 
                              branches of the language tree that are
                              available. 
   @endverbatim
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStyleGetLowLevelX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   Make some miscellaneous queries:
 *   
 * @code
 *   ArgList_pa   ArgList;
 *   ArbParam_t   IValue;
 *   double       DValue;
 *   GetValueReturnCode_e GVRC;
 *   
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   
 *   // get mesh color for zone 2... assuming Tecplot's plot type is cartesian
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendInt(ArgList,         SV_OFFSET1, 2);
 *   TecUtilArgListAppendString(ArgList,      SV_P1,      SV_FIELD);
 *   TecUtilArgListAppendString(ArgList,      SV_P2,      SV_MESH);
 *   TecUtilArgListAppendString(ArgList,      SV_P3,      SV_COLOR);
 *   TecUtilArgListAppendArbParamPtr(ArgList, SV_IVALUE,  &IValue);
 *   GVRC = TecUtilStyleGetLowLevelX(ArgList);
 *   if (GVRC == GetValue_Ok)
 *     {
 *       ColorIndex_t MeshColor = (ColorIndex_t)IValue;
 *       printf("Zone 2's mesh color is %d\n", MeshColor);
 *     }
 *   
 *   // get the line thickness of zone 3... same assumptions as above
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendInt(ArgList,       SV_OFFSET1, 3);
 *   TecUtilArgListAppendString(ArgList,    SV_P1,      SV_FIELD);
 *   TecUtilArgListAppendString(ArgList,    SV_P2,      SV_MESH);
 *   TecUtilArgListAppendString(ArgList,    SV_P3,      SV_LINETHICKNESS);
 *   TecUtilArgListAppendDoublePtr(ArgList, SV_DVALUE,  &DValue);
 *   GVRC = TecUtilStyleGetLowLevelX(ArgList);
 *   if (GVRC == GetValue_Ok)
 *     {
 *       double MeshLineThickness = DValue;
 *       printf("Zone 3's mesh line thickness is %lg\n", MeshLineThickenss);
 *     }
 *   
 *   // get the positive prefix number for contour group 1's contour legend
 *   TecUtilArgListClear(ArgList);
 *   TecUtilArgListAppendInt(ArgList,         SV_OFFSET1, 1);
 *   TecUtilArgListAppendString(ArgList,      SV_P1,      SV_GLOBALCONTOUR);
 *   TecUtilArgListAppendString(ArgList,      SV_P2,      SV_LEGEND);
 *   TecUtilArgListAppendString(ArgList,      SV_P3,      SV_NUMFORMAT);
 *   TecUtilArgListAppendString(ArgList,      SV_P3,      SV_POSITIVEPREFIX);
 *   TecUtilArgListAppendArbParamPtr(ArgList, SV_IVALUE,  &IValue);
 *   if (GVRC == GetValue_Ok)
 *     {
 *       char *PositivePrefixStr = (char *)IValue;
 *       if (PositivePrefixStr != NULL)
 *         {
 *           printf("Positive prefix number format for "
 *                  "contour group 1's contour legend is:%s\n",
 *                  PositivePrefixStr);
 *           TecUtilStringDealloc(&PositivePrefixStr);
 *         }
 *       else
 *         printf("Positive prefix number format for "
 *                "contour group 1's contour legend "
 *                "was not specified.\n");
 *     }
 *   
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON GetValueReturnCode_e STDCALL TecUtilStyleGetLowLevelX(ArgList_pa ArgList);





/**
 *   Queries a zone attribute. You can use this function to query any plot
 *   attribute that is not a floating point value. To query a floating point
 *   plot attribute, use TecUtilFieldStyleGetDoubleValue().
 *   
 * @return
 *   The queried attribute. This must be cast to the appropriate type.
 *
 * @param Zone 
 *   Zone number to query
 * @param S1 
 *   First parameter that defines the attribute to query. The parameters follow
 *   the same order that you would use when constructing a set value macro
 *   command. If a parameter is not used, then it must be NULL.  If you are not
 *   sure of the possible values for an enumerated type, you can find the
 *   definitions in the Include/GLOBAL.h directory, below the Tecplot home
 *   directory.  These parameters are actually strings, but you can use the
 *   supplied SV_ constants from the SV.h include file. Using the SV_ constants
 *   will help prevent misspellings and other errors.
 * @param S2 
 *   Second parameter that defines the attribute to query. See S1.
 * @param S3 
 *   Third parameter that defines the attribute to query. See S1.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilFieldStyleGetArbValue(
 *   &           Zone,
 *   &           S1,
 *   &           S2,
 *   &           S3,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    CHARACTER*(*)   S1
 *    CHARACTER*(*)   S2
 *    CHARACTER*(*)   S3
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Query the mesh color for zone 2.
 *   
 * @code
 *   {
 *     // Equivalent macro command to set the color is: 
 *     // $!FIELD [2] MESH {Color = ...}
 *
 *     ColorIndex_t MeshColor;
 *     ArbParam_t   Result;
 *   
 *     TecUtilLockStart(AddOnID);
 *   
 *     MeshColor = (ColorIndex_t)
 *     TecUtilFieldStyleGetArbValue(2,SV_MESH,SV_COLOR,NULL);
 *   
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *   
 */
LINKTOADDON ArbParam_t STDCALL TecUtilFieldStyleGetArbValue(EntIndex_t  Zone,
                                                            const char *S1,
                                                            const char *S2,
                                                            const char *S3);




/**
 *   Queries a zone attribute. You can use this function to query any plot
 *   attribute that is a floating point value. To query a non-floating point
 *   plot attribute, use TecUtilFieldStyleGetArbValue().
 *   
 * @param Zone
 *   Zone number to query.
 *   
 * @param S1
 *   First parameter used to define the attribute to query. The parameters
 *   follow the same order that you would use when constructing the $!FIELD
 *   macro command.
 *   
 * @param S2
 *   Second parameter used to define the attribute to query. See S1.
 *   
 * @param S3
 *   Third parameter used to define the attribute to query. See S1.
 *   
 * @return
 *   The queried attribute.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilFieldStyleGetDoubleValue(
 *   &                   Zone,
 *   &                   S1,
 *   &                   S2,
 *   &                   S3)
 *    INTEGER*4       Zone
 *    CHARACTER*(*)   S1
 *    CHARACTER*(*)   S2
 *    CHARACTER*(*)   S3
 * </FortranSyntax>
 *   
 *   Query the mesh pattern length for for zone 2.
 *   
 * @code
 *   {
 *     // Equivalent macro command to set the color is:
 *     //   $!FIELD [2] MESH {P  ATTERNLENGTH = ... }
 *     double  MeshPatternLength;
 *   
 *     TecUtilLockStart(AddOnID);
 *   
 *     MeshPatternLength = TecUtilFieldStyleGetDoubleValue(2,SV_MESH,
 *                                                         SV_PATTERNLENGTH,NULL);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON double STDCALL TecUtilFieldStyleGetDoubleValue(EntIndex_t  Zone,
                                                           const char *S1,
                                                           const char *S2,
                                                           const char *S3);



/**
 * @deprecated
 *   Please use TecUtilLineMapStyleGetArbValue() instead.
 */
LINKTOADDON ArbParam_t STDCALL TecUtilXYMapStyleGetArbValue(EntIndex_t  XYMap,
                                                            const char *S1,
                                                            const char *S2,
                                                            const char *S3);



/**
 * Queries an Line-map attribute. You can use this function to query any
 * Line-map attribute that is not a floating point value. To query a floating
 * point attribute, use TecUtilLineMapStyleGetDoubleValue().
 *   
 * @param LineMap
 *   Line-map number to query
 *   
 * @param S1
 *   First parameter used to define the attribute to query. The parameters
 *   follow the same order that you would use when constructing a set value
 *   macro command. If a parameter is not used, then it must be NULL.These
 *   parameters are actually strings, but you can use the supplied SV_
 *   constants from the SV.h include file. Using the SV_ constants will help
 *   prevent misspellings and other errors.If you are not sure of the possible
 *   values for an enumerate type, you can find the definitions in
 *   Include/GLOBAL.h below the Teclot home directory.
 *   
 * @param S2
 *   Second parameter used to define the attribute to query. See S1.
 *   
 * @param S3
 *   Third parameter used to define the attribute to query. See S1.This
 *   function will assert if the combination of parameters or the LineMap
 *   number is invalid.
 *   
 * @return
 *   The queried attribute. This must be cast to the appropriate type (see the
 *   table below). If the return type is a char *, then you must call
 *   TecUtilStringDealloc() to free the string.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLineMapStyleGetArbValue(
 *   &           LineMap,
 *   &           S1,
 *   &           S2,
 *   &           S3,
 *   &           ResultPtr)
 *    INTEGER*4       LineMap
 *    CHARACTER*(*)   S1
 *    CHARACTER*(*)   S2
 *    CHARACTER*(*)   S3
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Query the name of Line-map number 2.
 *   
 * @code
 *   {
 *     // Equivalent macro command to set the color is: $!LINEMAP [2] NAME = "..." 
 *     char *LineMapName = NULL;
 *   
 *     TecUtilLockStart(AddOnID);
 *   
 *     MapName = (const char *) TecUtilLineMapStyleGetArbValue(2,SV_NAME,NULL,NULL);
 *   
 *     // Use MapName.
 *   
 *     TecUtilStringDealloc(&MapName); // When finished
 *   
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 *
 *
 *    Get the error bar variable for line map 7:
 *
 * @code
 *     EntIndex_t ErrorBarVar;
 *
 *     TecUtilLockStart(AddOnID);
 *     LineThickness = (EntIndex_t)TecUtilLineMapStyleGetArbValue(7,
 *                                                                SV_ERRORBARS,
 *                                                                SV_VAR,
 *                                                                NULL);
 *     TecUtilLockFinish(AddOnID);
 * @endcode
 *
 */
LINKTOADDON ArbParam_t STDCALL TecUtilLineMapStyleGetArbValue(EntIndex_t  LineMap,
                                                              const char *S1,
                                                              const char *S2,
                                                              const char *S3);


/**
 * @deprecated
 *   Please use TecUtilLineMapStyleGetDoubleValue() instead.
 */
LINKTOADDON double STDCALL TecUtilXYMapStyleGetDoubleValue(EntIndex_t  XYMap,
                                                           const char *S1,
                                                           const char *S2,
                                                           const char *S3);

/**
 * Queries an Line-map attribute. You can use this function to query any
 * attribute that is a floating point value. To query a non-floating point
 * attribute, use TecUtilLineMapStyleGetArbValue().
 *   
 * @param LineMap
 *   Line-map number to query
 *   
 * @param S1
 *   First parameter used to define the attribute to query. The parameters
 *   follow the same order that you would use when constructing a set value
 *   macro command. If a parameter is not used, then it must be NULL.These
 *   parameters are actually strings, but you can use the supplied SV_
 *   constants from the SV.h include file. Using the SV_ constants will help
 *   prevent misspellings and other errors.If you are not sure of the possible
 *   values for an enumerate type, you can find the definitions in
 *   Include/GLOBAL.h below the Teclot home directory.
 *   
 * @param S2
 *   Second parameter used to define the attribute to query. See S1.
 *   
 * @param S3
 *   Third parameter used to define the attribute to query. See S1.
 *   
 * @return
 *   The queried attribute.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilLineMapStyleGetDoubleValue(
 *   &                   LineMap,
 *   &                   S1,
 *   &                   S2,
 *   &                   S3)
 *    INTEGER*4       LineMap
 *    CHARACTER*(*)   S1
 *    CHARACTER*(*)   S2
 *    CHARACTER*(*)   S3
 * </FortranSyntax>
 *   
 * Query the pattern length for Line-map number 2.
 *   
 * @code
 *   {
 *     // Equivalent macro command to set the pattern 
 *     // length is: $!LINEMAP [2] LINES {  PATTERNLENGTH = ... } 
 *   
 *     double  LinePatternLength;
 *   
 *     TecUtilLockStart(AddOnID);
 *   
 *     LinePatternLength = TecUtilLineMapStyleGetDoubleValue(2,SV_LINES,
 *                                                           SV_PATTERNLENGTH,
 *                                                           NULL);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON double STDCALL TecUtilLineMapStyleGetDoubleValue(EntIndex_t  LineMap,
                                                             const char *S1,
                                                             const char *S2,
                                                             const char *S3);
/**
 * Query Tecplot to find out if Tecplot is in the middle of processing the data
 * journal.
 *   
 * @return
 *   Returns TRUE if Tecplot is processing the data journal, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateIsProcessingJournal()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateIsProcessingJournal(void);
/**
 *   Query Tecplot to find out if Tecplot is in the middle of processing a layout.
 *   
 * @return
 *   Returns TRUE if Tecplot is processing a layout, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateIsProcessingLayout()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateIsProcessingLayout(void);

/**
 * Gets a unique ID for a frame. A unique ID is an integer that is unique to a
 * frame. The frame can be compared to other frames or pushed or popped by this
 * unique ID using TecUtilFramePopByUniqueID() and
 * TecUtilFramePushByUniqueID().
 *   
 * @return
 *   A unique ID for the current frame.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFrameGetUniqueID()
 * </FortranSyntax>
 *   
 * Push the current frame using a unique ID:
 *   
 * @code
 *   {
 *     UniqueID_t ID;
 *     TecUtilLockStart(AddOnID);
 *     ID = TecUtilFrameGetUniqueID();
 *     TecUtilFramePushByUniqueID(ID);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON UniqueID_t STDCALL TecUtilFrameGetUniqueID(void);

/**
 * Gets a unique ID for a data set. A unique ID is an integer that is unique to
 * a data set. This ID can be used to determine if the data sets from several
 * frames are the same.
 *   
 * @return
 *   A unique ID for the current data set.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetGetUniqueID()
 * </FortranSyntax>
 *   
 * Determine if the datasets of the top 2 frames are the same:
 *   
 * @code
 *   {
 *     UniqueID_t ID;
 *     TecUtilLockStart(AddOnID);
 *     ID = TecUtilDataSetGetUniqueID();
 *     TecUtilFramePushTop();
 *     if ( ID == TecUtilDataSetGetUniqueID() )
 *       {
 *         // Datasets are the same for both frames
 *       }
 *     else
 *       {
 *         // Datasets are different
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON UniqueID_t STDCALL TecUtilDataSetGetUniqueID(void);

/**
 * Gets a unique ID for a zone. A unique ID is an integer that uniquely
 * identifies a zone. An addon can use these IDs to internally keep track of a
 * set of zones. TecUtilZoneGetNumByUniqueID() can be used to convert between a
 * unique ID and a zone number.
 *   
 * @param Zone
 *   Zone number to query.
 *   
 * @return
 *   A unique ID for a zone.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetUniqueID(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *   
 * Get the UniqueID for zone 1:
 *   
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailable() && TecUtilZoneIsEnabled(1) )
 *       {
 *         UniqueID_t ID = TecUtilZoneGetUniqueID(1);
 *         ...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON UniqueID_t STDCALL TecUtilZoneGetUniqueID(EntIndex_t Zone);

/**
 * Gets a unique ID for a variable. A unique ID is an integer that uniquely
 * identifies a variable. An addon can use these IDs to internally keep track
 * of a set of variables.  TecUtilVarGetNumByUniqueID() can be used to convert
 * between a unique ID and a variable number.
 *   
 * @param Var
 *   Variable number to query.
 *   
 * @return
 *   A unique ID for a variable.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetUniqueID(Var)
 *    INTEGER*4 Var
 * </FortranSyntax>
 *   
 * Get the unique ID for variable 1:
 *   
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailable() && TecUtilVarIsEnabled(1) )
 *       {
 *         UniqueID_t ID = TecUtilVarGetUniqueID(1);
 *         ...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON UniqueID_t STDCALL TecUtilVarGetUniqueID(EntIndex_t Var);

/**
 * Gets a unique ID for a line map. A unique ID is an integer that uniquely
 * identifies a line map. An addon can use these IDs to internally keep track
 * of a set of line maps. TecUtilLineMapGetNumByUniqueID() can be used to
 * convert between a unique ID and a line map number.
 *
 * @since
 *   10.0-3-129
 *   
 * @param LineMap
 *   Line map number to query.
 *   
 * @return
 *   A unique ID for a line map.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapGetUniqueID(LineMap)
 *    INTEGER*4 Map
 * </FortranSyntax>
 *   
 * Get the unique ID for line map N:
 *   
 * @code
 *   ... N is assigned...
 *   {
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailable() && TecUtilLineMapGetCount() >= N )
 *       {
 *         UniqueID_t ID = TecUtilLineMapGetUniqueID(N);
 *         ...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON UniqueID_t STDCALL TecUtilLineMapGetUniqueID(EntIndex_t LineMap);

/**
 * Gets a variable number, given a unique ID.
 *   
 * @param UniqueID
 *   Unique ID of the variable
 *   
 * @return
 *   The variable number of the variable represented by the unique ID. If there
 *   is no variable number for the given unique ID, the return value is
 *   \ref TECUTILBADID.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetNumByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *   
 * Get a variable number from a unique ID:
 *   
 * @code
 *   {
 *     extern UniqueID_t ID; // previously initialized
 *   
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailable() )
 *       {
 *         EntIndex_t VarNum = TecUtilVarGetNumByUniqueID(ID);
 *         if (VarNum != TECUTILBADID)
 *           {
 *             ...
 *           }
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON EntIndex_t STDCALL TecUtilVarGetNumByUniqueID(UniqueID_t UniqueID);

/**
 * Gets a line map number, given a unique ID.
 *
 * @since
 *   10.0-3-129
 *   
 * @param UniqueID
 *   Unique ID of the line map.
 *   
 * @return
 *   The line map number of the line map represented by the unique ID. If there
 *   is no line map number for the given unique ID, the return value is
 *   \ref TECUTILBADID.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapGetNumByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *   
 * Get a line map number from a unique ID:
 *   
 * @code
 *   {
 *     extern UniqueID_t ID; // previously initialized
 *   
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailable() )
 *       {
 *         EntIndex_t MapNum = TecUtilLineMapGetNumByUniqueID(ID);
 *         if (MapNum != TECUTILBADID)
 *           {
 *             ...
 *           }
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON EntIndex_t STDCALL TecUtilLineMapGetNumByUniqueID(UniqueID_t UniqueID);

/**
 * Gets a zone number, given a unique ID.
 *   
 * @param UniqueID
 *   Unique ID of the zone
 *   
 * @return
 *   The zone number of the vairable represented by the unique ID. If
 *   there is no zone number for the given unique ID, the return value is
 *   \ref TECUTILBADID.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetNumByUniqueID(UniqueID)
 *    INTEGER*4 UniqueID
 * </FortranSyntax>
 *   
 * Get a zone number from a unique ID:
 *   
 * @code
 *   {
 *     extern UniqueID_t ID; // previously initialized
 *   
 *     TecUtilLockStart(AddOnID);
 *     if ( TecUtilDataSetIsAvailable() )
 *       {
 *         EntIndex_t ZoneNum = TecUtilZoneGetNumByUniqueID(ID);
 *         if (ZoneNum != TECUTILBADID)
 *           {
 *             ...
 *           }
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON EntIndex_t STDCALL TecUtilZoneGetNumByUniqueID(UniqueID_t UniqueID);

/**
 * Gets the number (that is, the index) of a variable based on the variable assignment.
 *   
 * @param Var
 *   Variable to get. The frame mode must be 2-D or 3-D.If the frame mode is
 *   2-D, select one of 'X', 'Y', 'U', 'V', 'B', 'C', or 'S'.If the frame mode
 *   is 3-D, select one of 'X','Y','Z','U','V','W', 'B', 'C', or 'S'Table 0-1.
 *   Variable assignment identifiers (Var) and descriptions.
 *   
 * @param Var
 *   Description'X' X-axis variable'Y' Y-axis variable'Z' Z-axis variable'U'
 *   U-velocity variable'V' V-velocity variable'W' W-velocity variable'B'
 *   Blanking variable'C' Contouring variable'S' Scatter sizing variable
 *   
 * @return
 *   The index (number) of the variable referenced by Var.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetNumByAssignment(Var)
 *    CHARACTER*(*) Var
 * </FortranSyntax>
 *   
 * Get the index of the 'X' variable:
 *   
 * @code
 *   // frame mode must be 2-D or 3-D
 *   EntIndex_t i = TecUtilVarGetNumByAssignment('X');
 * @endcode
 */
LINKTOADDON EntIndex_t STDCALL TecUtilVarGetNumByAssignment(char Var);

/**
 * Gets the number (that is, the index) of a variable based on variable name.
 *   
 * @param VarName
 *   Name of the variable. Must not be NULL
 *   
 * @return
 *   The index (number) of the variable with name VarName, otherwise
 *   TECUTILSETNOTMEMBER if the variable is not a member of the current frame's
 *   data set.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetNumByName(VarName)
 *    CHARACTER*(*) VarName
 * </FortranSyntax>
 *   
 *   Get the index of the variable Rainfall:
 *   
 * @code
 *   EntIndex_t i = TecUtilVarGetNumByName("Rainfall");
 * @endcode
 */
LINKTOADDON EntIndex_t STDCALL TecUtilVarGetNumByName(const char *VarName);



/**
 * Get a handle to the raw field data in the data set attached to the
 * current frame. If possible this function provides direct access to
 * a Tecplot variable's internal representation. If performance is not
 * a concern consider using TecUtilDataValueGetByRef(),
 * TecUtilDataValueSetByRef(), TecUtilDataValueArrayGetByRef(), and
 * TecUtilDataValueArraySetByRef(). If high performance is essential
 * then use the accessor functions used by Tecplot by calling
 * TecUtilDataValueRefGetGetFunc() and TecUtilDataValueRefGetSetFunc().
 * Note that these high performance functions are a very thin layer over a
 * Tecplot variable's internal representation and unlike the raw field data
 * pointer provided by this function, they are portable.
 *
 * @par Note:
 *   Do not assume that raw data internal to Tecplot remain in the same
 *   location at all times. Always call this function again after any event
 *   where Tecplot itself may move/alter the raw data. Make sure to call
 *   TecUtilStateChanged() after any field values have changed.
 *   
 * @param Zone
 *   Number of the zone for which to get the raw field data.
 *   
 * @param Var
 *   Number of the variable for which to get the raw field data.
 *   
 * @param DataPtr
 *   Receives the address of the raw field data.  May return NULL if
 *   the type is too complex. If the type is too complex, you may use
 *   TecUtilDataValueRefGetGetFunc() and TecUtilDataValueRefGetSetFunc()
 *   to get functions that will deal with the data at a lower level than
 *   TecUtilDataValueGetByRef() and TecUtilDataValueSetByRef().
 *   
 * @param FieldDataType
 *   Receives the data type of the raw field data. The following table shows
 *   the possible values for FieldDataType along with the corresponding data
 *   type that DataPtr references:
 *
   @verbatim
     FieldDataType            DataPtr references
     -------------------------------------------
     FieldDataType_Float      float *
     FieldDataType_Double     double *
     FieldDataType_Int32      Int32_t *
     FieldDataType_Int16      Int16_t *
     FieldDataType_Byte       char *
     FieldDataType_Bit        UInt32_t *
     FieldDataType_Invalid    too complex
   @endverbatim
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetRawPtr(
 *   &           Zone,
 *   &           Var,
 *   &           DataPtr,
 *   &           FieldDataType)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    POINTER         (DataPtrPtr, DataPtr)
 *    INTEGER*4       FieldDataType
 * </FortranSyntax>
 *   
 * Get and then set the first and second values of the second variable of zone
 * 5 using a raw data pointer. Note that the first and second values are
 * accessed by zero and one respectively since the raw data is accessed with a
 * zero-base numbering system. Be sure to call TecUtilStateChanged() when you
 * change the data in this way:
 *
 * @code
 *   EntIndex_t zone = 5;
 *   EntIndex_t var = 2;
 *   void *raw_fd_ptr = NULL;
 *   FieldDataType_e field_data_type;
 *   TecUtilDataValueGetRawPtr(zone, var, &raw_fd_ptr, &field_data_type);
 *   if ( raw_fd_ptr )
 *     {
 *       if ( field_data_type == FieldData_Float )
 *         {
 *           Set_pa altered_vars = TecUtilSetAlloc(TRUE);
 *           float *float_ptr = (float *)raw_fd_ptr;
 *           float v1 = float_ptr[0];
 *           float v2 = float_ptr[1];
 *           // alter v1 and v2 in some way
 *           v1 = 2.0 * v1;
 *           v2 = 2.0 * v2;
 *           float_ptr[0] = v1;
 *           float_ptr[1] = v2;
 *           // inform Tecplot of var value change
 *           TecUtilSetAddMember(altered_vars, var, TRUE);
 *           TecUtilStateChanged(StateChange_VarsAltered,
 *                               (ArbParam_t)altered_vars);
 *           TecUtilSetDealloc(&altered_vars);
 *         }
 *     }
 * @endcode
 *
 * @sa TecUtilDataValueGetRef(), TecUtilDataValueRefGetGetFunc(),
 *     and TecUtilDataValueRefGetSetFunc() for portable, high performance
 *     alternatives for accessing data.
 */
LINKTOADDON void STDCALL TecUtilDataValueGetRawPtr(EntIndex_t        Zone, /* <-activex> */
                                                   EntIndex_t        Var,
                                                   void            **DataPtr,
                                                   FieldDataType_e  *FieldDataType);





/**
 * Get a handle to the raw node map data in the data set attached to the
 * current frame. Use this function with extreme caution and only as a last
 * resort. TecUtilDataNodeGetByRef() and TecUtilDataNodeSetByRef() should be
 * used instead in most cases.
 *
 * @par Note:
 *   Do not assume that raw data internal to Tecplot remain in the same
 *   location at all times. Always call this function again after any event
 *   where Tecplot itself may move or alter the raw data. Also, make sure to
 *   call TecUtilStateChanged() after any values have changed.
 *   
 * @param Zone
 *   Number of the zone for which to get the raw node map data. This must be a
 *   finite-element zone.
 *   
 * @param NodeMapPtr
 *   Receives address of the raw node map data
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataNodeGetRawPtr(
 *   &           Zone,
 *   &           NodeMapPtr)
 *    INTEGER*4       Zone
 *    POINTER         (NodeMapPtrPtr, NodeMapPtr)
 * </FortranSyntax>
 *   
 * Get and then set the first and second node of the 43rd element of zone 5
 * using a raw data pointer.  The 43rd element is accessed by
 * 42*nodes_per_element and the first and second nodes are accessed by +0 and
 * +1 respectively since the raw data is accessed with a zero-base numbering
 * system. Be sure to call TecUtilStateChanged() when you change the node map
 * in this way:
 *   
 * @code
 *   // assume we already know zone 5 is a finite element zone with
 *   // triangle elements (by using TecUtilZoneGetInfo() or
 *   // TecUtilZoneGetType())
 *   LgIndex_t zone = 5;
 *   NodeMap_t *raw_nm_ptr = NULL;
 *   LgIndex_t nodes_per_element = 3; // triangular
 *   TecUtilDataNodeGetRawPtr(zone, &raw_nm_ptr);
 *   if ( raw_nm_ptr )
 *     {
 *       Set_pa altered_zones = TecUtilSetAlloc(TRUE);
 *       LgIndex n1, n2;
 *       n1 = raw_nm_ptr[42*nodes_per_element + 0]; // zero-based numbering
 *       n2 = raw_nm_ptr[42*nodes_per_element + 1];
 *       // alter n1 and n2 in some way
 *       n1++;
 *       n2++;
 *       raw_nm_ptr[42*nodes_per_element + 0] = n1;
 *       raw_nm_ptr[42*nodes_per_element + 1] = n2;
 *       // inform Tecplot of node map change
 *       TecUtilSetAddMember(altered_zones, 5, TRUE);
 *       TecUtilStateChanged(StateChange_NodeMapsAltered,
 *                           (ArbParam_t)altered_zones);
 *       TecUtilSetDealloc(&altered_zones);
 *     }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDataNodeGetRawPtr(EntIndex_t   Zone,
                                                  NodeMap_t  **NodeMapPtr);


/**
 * @deprecated
 *   There is no replacement for this function. Please do not access face
 *   neighbors using raw pointers anymore.
 */
LINKTOADDON void STDCALL TecUtilDataFaceNbrGetRawPtr(EntIndex_t   Zone,
                                                     LgIndex_t  **FNPtr);





/**
 * Get the name of a specified zone in the data set attached to the current
 * frame.
 *   
 * @param Zone
 *   Number of the zone for which to get the zone name information
 *   
 * @param ZName
 *   Receives the name of the specified zone. You must free the returned string
 *   with TecUtilStringDealloc().
 *   
 * @return
 *   TRUE if successful, FALSE if not. FALSE usually indicates an invalid zone
 *   or that the current frame does not have an attached data set.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetName(
 *   &                   Zone,
 *   &                   ZName,
 *   &                   ZNameLength)
 *    INTEGER*4       Zone
 *    CHARACTER*(*)   ZName
 *    INTEGER*4       ZNameLength
 * </FortranSyntax>
 *   
 *   Get the name of the first zone:
 *   
 * @code
 *   char *name = NULL;
 *   if (TecUtilZoneGetName(1,&name)
 *   {
 *     // do something with the name here
 *     TecUtilStringDealloc(&name);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneGetName(EntIndex_t   Zone,
                                                 char       **ZName);
/**
 * Get the name of a variable in the data set attached to the current frame.
 * There must be a data set attached to the current frame.
 *   
 * @param VarNum
 *   Number of the variable for which to get the variable name information.
 *   Must be greater than zero, and the variable must be enabled
 *   
 * @param VName
 *   Receives the name of the specified variable. Must not be NULL. You must
 *   free this string with TecUtilStringDealloc().
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetName(
 *   &                   VarNum,
 *   &                   VName,
 *   &                   VNameLength)
 *    INTEGER*4       VarNum
 *    CHARACTER*(*)   VName
 *    INTEGER*4       VNameLength
 * </FortranSyntax>
 *   
 *   Get the name of the first variable:
 *   
 * @code
 *   char buffer[100];
 *   VarName_t Name;
 *   TecUtilVarGetName(1,&Name);
 *   sprintf(buffer,"The name of the first variable is %s",Name);
 *   TecUtilStringDealloc(&Name);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarGetName(EntIndex_t   VarNum,
                                                char       **VName);



/**
 * @deprecated
 *   Please use TecUtilLineMapGetName() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapGetName(EntIndex_t   Map,
                                                  char       **Name);
/**
 *   Get the name of an Line-map.
 *   
 * @param Map
 *   Number of the Line-map
 *   
 * @param Name
 *   Allocated string containing the Line-map name
 *   
 * @return
 *   Returns TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapGetName(
 *   &                   Map,
 *   &                   Name,
 *   &                   ReturnedNameLength)
 *    INTEGER*4       Map
 *    CHARACTER*(*)   Name
 *    INTEGER*4       ReturnedNameLength
 * </FortranSyntax>
 *   
 *   Get the name of Line-map 3:
 *   
 * @code
 *   Boolean_t IsOk;
 *   char *MapName = NULL;
 *   IsOk = TecUtilLineMapGetName(3,&MapName);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapGetName(EntIndex_t   Map,
                                                    char       **Name);


/**
 * Convenience function used to obtain information about a specific zone.  This
 * function is primarily targeted for use with 2D and 3D frame modes. If the
 * frame mode is XY, only the zone dimensions can be queried. To get a field
 * data pointer to axis variables when the frame mode is XY use
 * TecUtilLineMapGetAssignment().
 *   
 * @param CurZone
 *   Number of the zone to use
 *   
 * @param IMax
 *   Receives the I-dimension for ordered data. Number of data points for
 *   FE-data. May be NULL
 *   
 * @param JMax
 *   Receives the J-dimension for ordered data. Number of elements for FE-data.
 *   May be NULL
 *   
 * @param KMax
 *   Receives the K-dimension for ordered data. Number of nodes per cell for
 *   FE-data. May be NULL
 *   
 * @param XVar
 *   Receives the handle to field data for X. May be NULL. If the frame mode is
 *   XY this parameter must be NULL
 *   
 * @param YVar
 *   Receives the handle to field data for Y. May be NULL. If the frame mode is
 *   XY this parameter must be NULL
 *   
 * @param ZVar
 *   Receives the handle to field data for Z. May be NULL. If the frame mode is
 *   XY this parameter must be NULL
 *   
 * @param NMap
 *   Receives the handle for the connectivity list. May be NULL. If the frame
 *   mode is XY this parameter must be NULL
 *   
 * @param UVar
 *   Receives the Handle to field data for U. May be NULL. If the frame mode is
 *   XY this parameter must be NULL
 *   
 * @param VVar
 *   Receives the handle to field data for V. May be NULL. If the frame mode is
 *   XY this parameter must be NULL
 *   
 * @param WVar
 *   Receives the handle to field data for W. May be NULL. If the frame mode is
 *   XY this parameter must be NULL
 *   
 * @param BVar
 *   Receives the handle to field data for the blanking variable. May be NULL.
 *   If the frame mode is XY this parameter must be NULL
 *   
 * @param CVar
 *   Receives the handle for the contouring variable. May be NULL. If the frame
 *   mode is XY this parameter must be NULL
 *   
 * @param SVar
 *   Receives the handle for the scatter sizing variable. May be NULL. If the
 *   frame mode is XY this parameter must be NULL
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilZoneGetInfo(
 *   &           CurZone,
 *   &           IMax,
 *   &           JMax,
 *   &           KMax,
 *   &           XVarPtr,
 *   &           YVarPtr,
 *   &           ZVarPtr,
 *   &           NMapPtr,
 *   &           UVarPtr,
 *   &           VVarPtr,
 *   &           WVarPtr,
 *   &           BVarPtr,
 *   &           CVarPtr,
 *   &           SVarPtr)
 *    INTEGER*4       CurZone
 *    INTEGER*4       IMax
 *    INTEGER*4       JMax
 *    INTEGER*4       KMax
 *    POINTER         (XVarPtr, XVar)
 *    POINTER         (YVarPtr, YVar)
 *    POINTER         (ZVarPtr, ZVar)
 *    POINTER         (NMapPtr, NMap)
 *    POINTER         (UVarPtr, UVar)
 *    POINTER         (VVarPtr, VVar)
 *    POINTER         (WVarPtr, WVar)
 *    POINTER         (BVarPtr, BVar)
 *    POINTER         (CVarPtr, CVar)
 *    POINTER         (SVarPtr, SVar)
 * </FortranSyntax>
 *   
 *   Get IMAX for the first zone:
 *   
 * @code
 *   LgIndex_t IMax;
 *   // Use NULL for values we're not interested in
 *   TecUtilZoneGetInfo(1,&IMax,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
 *                      NULL,NULL,NULL,NULL,NULL);
 * @endcode
 *
 *
 * FORTRAN example to get IMAX for the first zone:
 *
 * @code
 *      INTEGER*4 IMax
 *      INTEGER*4 ZoneNum
 *      POINTER   (NullPntr, Null)
 *             .
 *             .
 *             .
 *      NullPntr = 0
 *      ZoneNum  = 1
 *
 *      Call TecUtilZoneGetInfo(ZoneNum,
 *     &                        IMax,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null,
 *     &                        Null)
 * @endcode
 *
 */
LINKTOADDON void STDCALL TecUtilZoneGetInfo(EntIndex_t     CurZone,
                                            LgIndex_t     *IMax,
                                            LgIndex_t     *JMax,
                                            LgIndex_t     *KMax,
                                            FieldData_pa  *XVar,
                                            FieldData_pa  *YVar,
                                            FieldData_pa  *ZVar,
                                            NodeMap_pa    *NMap,
                                            FieldData_pa  *UVar,
                                            FieldData_pa  *VVar,
                                            FieldData_pa  *WVar,
                                            FieldData_pa  *BVar,
                                            FieldData_pa  *CVar,
                                            FieldData_pa  *SVar);


/**
 * Get the title, number of zones, and number of variables of the data set
 * attached to the current frame.
 *   
 * @param DataSetTitle
 *   Character string containing the title of the data set attached to the
 *   current frame. If you pass NULL, this will not be assigned. Deallocate the
 *   returned string with TecUtilStringDealloc() when you are done with it.
 *   
 * @param NumZones
 *   The number of zones in the data set attached to the current frame. If you
 *   pass NULL, this will not be assigned
 *   
 * @param NumVars
 *   The number of variables in the data set attached to the current frame. If
 *   you pass NULL, this will not be assigned.
 *   
 * @return
 *   TRUE if successful, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetGetInfo(
 *   &                   DataSetTitle,
 *   &                   DataSetTitleLength,
 *   &                   NumZones,
 *   &                   NumVars)
 *    CHARACTER*(*)   DataSetTitle
 *    INTEGER*4       DataSetTitleLength
 *    INTEGER*4       NumZones
 *    INTEGER*4       NumVars
 * </FortranSyntax>
 *   
 * Get the data set title and number of zones and variables from the current
 * data set:
 *   
 * @code
 *   char *dataset_title = NULL;
 *   EntIndex_t nzones, nvars;
 *   
 *   TecUtilDataSetGetInfo(&dataset_title, &nzones, &nvars);
 *   // use dataset_title
 *   TecUtilStringDealloc(&dataset_title);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetGetInfo(char       **DataSetTitle,
                                                    EntIndex_t  *NumZones,
                                                    EntIndex_t  *NumVars);

/**
 * Query Tecplot to see if the journal for the data set attached to the current
 * frame is valid. This is a concern if a layout file is to be generated from
 * an addon. When layouts are generated from an addon the layout must be able
 * to reproduce all data sets via named files and journal entries.  A data set
 * will require saving if any un-journaled data operations are performed on it.
 *   
 * @return
 *   Returns TRUE if the data set journal is valid, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetJournalIsValid()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetJournalIsValid(void);



/**
 * @deprecated
 *   Please use TecUtilDataSetJournalIsValid() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetRequiresSaving(void);

/**
 * Get the position and size of a frame.
 *   
 * @param X
 *   Returned X-Coordinate for left hand side of the frame (in inches) relative
 *   to the left hand side of the paper.
 *   
 * @param Y
 *   Returned Y-Coordinate for top of the frame (in inches) relative to the top
 *   of the paper.
 *   
 * @param Width
 *   Width of the frame (in inches).
 *   
 * @param Height
 *   Height of the frame (in inches).
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilFrameGetPosAndSize(
 *   &           X,
 *   &           Y,
 *   &           Width,
 *   &           Height)
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *   
 *   Get the current frame's position and size:
 *   
 * @code
 *   double x, y, width, height;
 *   TecUtilFrameGetPosAndSize(&x, &y, &width, &height);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilFrameGetPosAndSize(double *X,
                                                   double *Y,
                                                   double *Width,
                                                   double *Height);

/**
 * Get the first text object from the list of text objects maintained by the
 * current frame.
 * 
 * @sa TecUtilTextGetNext() and TecUtilTextGetPrev()
 *   
 * @return
 *   First text object of the list maintained by the current frame. If no text
 *   objects are maintained by the current frame then \ref TECUTILBADID is
 *   returned.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextGetBase(ResultPtr)
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Examine all text labels in the current frame and see if any of them are
 *   red.
 *   
 * @code
 *   TextID Text;
 *   for (Text = TecUtilTextGetBase();
 *        Text != TECUTILBADID;
 *        Text = TecUtilTextGetNext(Text))
 *     {
 *       if (TecUtilTextGetColor(Text) == Red_C)
 *         {
 *           // do something with the red text
 *         }
 *     }
 * @endcode
 */
LINKTOADDON Text_ID STDCALL TecUtilTextGetBase(void);

/**
 * Get the base geometry attached to the current frame.
 *   
 * @return
 *   Returns \ref TECUTILBADID if the current frame does not have any attached
 *   geometries, otherwise, a valid geometry ID. Use TecUtilGeomGetNext() to
 *   get successive geometries.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomGetBase(ResultPtr)
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomGetBase(void);

/**
 * Use Tecplot's probe capability to return field values at a specified X, Y,
 * [Z] location. The current frame must have an attached data set when this
 * function is called.
 *   
 * @param X
 *   The X-position at which to probe.
 *   
 * @param Y
 *   The Y-position at which to probe.
 *   
 * @param Z
 *   The Z-position at which to probe. Only required if plot type is 3D.  If
 *   the frame mode is 3D and SearchVolume is FALSE, then only X and Y are used
 *   and they are assumed to be the XY coordinates in the eye coordinate
 *   system. The returned probe will be the location on the first surface
 *   encountered in the eye coordinate system
 *   
 * @param ICell
 *   Returns the I-Index of the cell in which the data point was found. If
 *   StartWithLocalCell is TRUE, then this must be pre-set to the I-index of
 *   the cell in which to start looking.
 *   
 * @param JCell
 *   Returns the J-Index of the cell in which the data point was found. If
 *   StartWithLocalCell is TRUE, then this must be pre-set to the J-index of
 *   the cell in which to start looking.
 *   
 * @param KCell
 *   Returns the K-Index of the cell in which the data point was found. If
 *   StartWithLocalCell is TRUE, then this must be pre-set to the K-index of
 *   the cell in which to start looking.
 *   
 * @param Plane
 *   If the current frame is 3D, the plane of the cell in which the data point
 *   was found is returned.  (Ignore if SearchVolume is TRUE.) Plane must be
 *   pre-set if the current frame is 2D and StartWithLocalCell is TRUE and the
 *   zone is not finite element. Plane can be one of: Planes_I Planes_J
 *   Planes_K
 *   
 * @param CurZone
 *   Returns the zone of the cell in which the data point was found
 *   
 * @param StartWithLocalCell
 *   TRUE if the search should be started in the area of cell
 *   *ICell,*JCell,*KCell. FALSE if the search should not be started in any
 *   particular cell
 *   
 * @param VValue_Array
 *   An array of doubles which is the size of the number of variables in the
 *   data set. The array must be allocated by the calling function. If
 *   TecUtilProbeAtPosition() returns TRUE and GetZoneOnly is FALSE, the array
 *   will be filled with the values of each variable at the probed position
 *   
 * @param SourceZones
 *   The set of zones to which to limit the search. Set to NULL to search all
 *   zones
 *   
 * @param SearchVolume
 *   Set to TRUE if the XYZ coordinates represent a data point inside of a 3-D
 *   volume zone. Set to FALSE to use the XY coordinates only which will return
 *   a point on the surface of a zone
 *   
 * @param GetZoneOnly
 *   Set to TRUE to do the minimal work necessary to only update the CurZone
 *   variable
 *   
 * @param GetNearestPoint
 *   TRUE to return values for the nearest grid point, FALSE to return
 *   interpolated values for the exact XYZ coordinates. A value of TRUE will
 *   cause this function to return FALSE if the initial probe does not fall
 *   within a cell
 *   
 * @return
 *   TRUE if successful, FALSE if not. FALSE usually indicates that the data
 *   point could not be found.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeAtPosition(
 *   &                   X,
 *   &                   Y,
 *   &                   Z,
 *   &                   ICell,
 *   &                   JCell,
 *   &                   KCell,
 *   &                   Plane,
 *   &                   CurZone,
 *   &                   StartWithLocalCell,
 *   &                   VValue_Array,
 *   &                   SourceZonesPtr,
 *   &                   SearchVolume,
 *   &                   GetZoneOnly,
 *   &                   GetNearestPoint)
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Z
 *    INTEGER*4       ICell
 *    INTEGER*4       JCell
 *    INTEGER*4       KCell
 *    INTEGER*4       Plane
 *    INTEGER*4       CurZone
 *    INTEGER*4       StartWithLocalCell
 *    REAL*8          VValue_Array
 *    POINTER         (SourceZonesPtr, SourceZones)
 *    INTEGER*4       SearchVolume
 *    INTEGER*4       GetZoneOnly
 *    INTEGER*4       GetNearestPoint
 * </FortranSyntax>
 *   
 *   Assuming a 2-D plot, probe at the location (3.2, 1.45):
 *   
 * @code
 *   LgIndex_t    ICell, JCell, KCell;
 *   IJKPlanes_e  Plane;
 *   EntIndex_t   SourceZone;
 *   EntIndex_t   NumVars;
 *   double      *VValues;
 *   
 *   TecUtilDataSetGetInfo((char **)NULL,
 *                         (EntIndex_t *)NULL,
 *                          &NumVars);
 *   
 *   VValues = (double *)malloc(NumVars*sizeof(double));
 *   
 *   if (TecUtilProbeAtPosition(3.2, 1.45, 0.0,
 *                              &ICell,
 *                              &JCell,
 *                              &KCell,
 *                              &Plane,
 *                              &SourceZone,
 *                              FALSE,
 *                              VValues,
 *                              (Set_pa )NULL,
 *                              FALSE,
 *                              FALSE,
 *                              FALSE))
 *      {
 *          // Look at VValues[VarNum-1] to find the
 *          // value for a particular variable.
 *      }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilProbeAtPosition(double       X,
                                                     double       Y,
                                                     double       Z,
                                                     LgIndex_t   *ICell,
                                                     LgIndex_t   *JCell,
                                                     LgIndex_t   *KCell,
                                                     IJKPlanes_e *Plane,
                                                     EntIndex_t  *CurZone,
                                                     Boolean_t    StartWithLocalCell,
                                                     double      *VValue_Array,
                                                     Set_pa       SourceZones,
                                                     Boolean_t    SearchVolume,
                                                     Boolean_t    GetZoneOnly,
                                                     Boolean_t    GetNearestPoint);
  



/**
 * Get the set of enabled zones. Zones are enabled/disabled when they are read
 * in.
 *   
 * @param EnabledZones
 *   Receives the set of enabled zones. You must free this pointer by calling
 *   TecUtilSetDealloc().
 *   
 * @return
 *   TRUE if successful, FALSE otherwise
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetEnabled(EnabledZonesPtr)
 *    POINTER (EnabledZonesPtr, EnabledZones)
 * </FortranSyntax>
 *   
 *   Get the set of enabled zones:
 *   
 * @code
 *   Set_pa set = NULL;
 *   if (TecUtilZoneGetEnabled(&set))
 *   {
 *    // do something with the set here
 *    TecUtilSetDealloc(&set);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneGetEnabled(Set_pa *EnabledZones);
/**
 * Get the set of enabled variables. Variables are enabled/disabled when they
 * are read in. There must be a data set attached to the current frame.
 *   
 * @param EnabledVars
 *   Set of enabled variables. Must not be NULL
 *   
 * @return
 *   TRUE if successful, FALSE otherwise
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarGetEnabled(EnabledVarsPtr)
 *    POINTER (EnabledVarsPtr, EnabledVars)
 * </FortranSyntax>
 *   
 *   Get the set of enabled variables. It is assumed that a data set has been
 *   created:
 *   
 * @code
 *   Set_pa set = NULL;
 *   TecUtilVarGetEnabled(&set);
 *   // Do something with set
 *   TecUtilSetDealloc(&set);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarGetEnabled(Set_pa *EnabledVars);
/**
 * Obtain the set of active field zones.
 *   
 * @param ActiveZones
 *   Receives the set of active field zones. You must call TecUtilSetDealloc()
 *   when you are through using the set.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetActive(ActiveZonesPtr)
 *    POINTER (ActiveZonesPtr, ActiveZones)
 * </FortranSyntax>
 *   
 *   Do something if zone 3 is active:
 *   
 * @code
 *   Set_pa zone_set = NULL;
 *   TecUtilZoneGetActive(&zone_set);
 *   if ( TecUtilSetIsMember(zone_set, 3) )
 *     {
 *       // do something
 *     }
 *   TecUtilSetDealloc(&zone_set);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneGetActive(Set_pa *ActiveZones);



/**
 * @deprecated
 *   Please use TecUtilLineMapGetActive() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapGetActive(Set_pa *ActiveXYMaps);



/**
 * Obtain the set of active Line-maps.
 *   
 * @param ActiveLineMaps
 *   Receives the set of active Line-maps. You must call TecUtilSetDealloc()
 *   when you are through using the set. It must not be NULL.
 *   
 * @return
 *   TRUE if successful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapGetActive(ActiveLineMapsPtr)
 *    POINTER (ActiveLineMapsPtr, ActiveLineMaps)
 * </FortranSyntax>
 *   
 *   Get the set of active Line-maps:
 *   
 * @code
 *   Set_pa s = NULL;
 *   if (TecUtilLineMapGetActive(&s))
 *   {
 *    // maps are now in s
 *    TecUtilSetDealloc(&s);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapGetActive(Set_pa *ActiveLineMaps);




/**
 * @deprecated
 *   Please use TecUtilLineMapGetAssignment() instead.
 */
LINKTOADDON void STDCALL TecUtilXYMapGetAssignment(EntIndex_t            XYMap,
                                                   EntIndex_t           *Zone,
                                                   EntIndex_t           *XAxisVar,
                                                   EntIndex_t           *YAxisVar,
                                                   SmInteger_t          *XAxis,
                                                   SmInteger_t          *YAxis,
                                                   FunctionDependency_e *FunctionDependency);
/**
 * Get the assignment information for a given Line-map.
 *   
 * @param LineMap
 *   Map number to query
 *   
 * @param Zone
 *   Zone number assigned to the Line-map. Set to NULL if you are not
 *   interested in this returned value.
 *   
 * @param XOrThetaVar
 *   Receives the X-axis or Theta variable number assigned to the Line-map.
 *   Will only receive a Theta value if plot is in polar mode. Set to NULL if
 *   you are not interested in this returned value
 *   
 * @param YOrRVar
 *   Receives the Y-axis or Radian variable number assigned to the Line-map.
 *   Will only receive Radian value if plot is in polar mode. Set to NULL if
 *   you are not interested in this returned value.
 *   
 * @param XAxis
 *   Receives the X-axis number assigned to the Line-map. Set to NULL if you
 *   are not interested in this returned value.
 *   
 * @param YAxis
 *   Receives the Y-axis number assigned to the Line-map. Set to NULL if you
 *   are not interested in this returned value
 *   
 * @param FunctionDependency
 *   Receives thefFunction dependency assigned to the Line-map. Set to NULL if
 *   you are not interested in this returned value
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLineMapGetAssignment(
 *   &           LineMap,
 *   &           Zone,
 *   &           XOrThetaVar,
 *   &           YOrRVar,
 *   &           XAxis,
 *   &           YAxis,
 *   &           FunctionDependency)
 *    INTEGER*4       LineMap
 *    INTEGER*4       Zone
 *    INTEGER*4       XOrThetaVar
 *    INTEGER*4       YOrRVar
 *    INTEGER*4       XAxis
 *    INTEGER*4       YAxis
 *    INTEGER*4       FunctionDependency
 * </FortranSyntax>
 *   
 *   Obtain the zone number and which variables are used for X and Y for
 *   Line-map number 3:
 *   
 * @code
 *     EntIndex_t Zone;
 *     EntIndex_t XVar;
 *     EntIndex_t YVar;
 *   
 *     TecUtilLineMapGetAssignment(3,
 *                               &Zone,
 *                               &XVar,
 *                               &YVar,
 *                               (SmInteger_t *)NULL,
 *                               (SmInteger_t *)NULL,
 *                               (FunctionDependency_e *)NULL);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilLineMapGetAssignment(EntIndex_t            LineMap,
                                                     EntIndex_t           *Zone,
                                                     EntIndex_t           *XOrThetaVar,
                                                     EntIndex_t           *YOrRVar,
                                                     SmInteger_t          *XAxis,
                                                     SmInteger_t          *YAxis,
                                                     FunctionDependency_e *FunctionDependency);
/**
 * Determine if a zone in the data set attached to the current frame contains
 * finite-element data.
 *   
 * @param Zone
 *   Number of the zone for which to get the zone type information
 *   
 * @return
 *   TRUE if the zone is a finite-element zone, FALSE if it is not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneIsFiniteElement(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *   
 *   Check if the first zone is finite element:
 *   
 * @code
 *   if (TecUtilZoneIsFiniteElement(1))
 *   {
 *     // sure is!
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneIsFiniteElement(EntIndex_t Zone);




/**
 * Determine if the specified zone in the data set attached to the current
 * frame contains ordered data.
 *   
 * @param Zone
 *   Number of the zone for which to get the zone type information
 *   
 * @return
 *   TRUE if the zone is an I-ordered, IJ-ordered, or IJK-ordered zone; FALSE
 *   if it is not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneIsOrdered(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *   
 *   Check if the first zone is ordered:
 *   
 * @code
 *   if (TecUtilZoneIsOrdered(1))
 *   {
 *     // sure is!
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneIsOrdered(EntIndex_t Zone);




/**
 * Get the type of a specified zone in the data set attached to the current
 * frame.
 *   
 * @param Zone
 *   Number of the zone for which to get the zone type information
 *   
 * @return
 *   The zone type.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneGetType(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *   
 *   Get the time of the first zone:
 *   
 * @code
 *   ZoneType_e type = TecUtilZoneGetType(1);
 * @endcode
 */
LINKTOADDON ZoneType_e STDCALL TecUtilZoneGetType(EntIndex_t Zone);



/**
 * Get a field data value. This function does not require you to obtain the
 * handle to the field data as does TecUtilDataValueGetByRef(), however, this
 * function is not very efficient. Use TecUtilDataValueGetByRef() if you are
 * getting multiple values from the same zone.
 *   
 * @param Zone
 *   The zone number
 *   
 * @param Var
 *   The variable number
 *   
 * @param PointIndex
 *   Position in the array of field data values. Position starts at one. If
 *   FieldData came from an IJ- or IJK-ordered zone then the position is
 *   calculated by treating the two- or three-dimensional array as a
 *   one-dimensional array
 *   
 * @return
 *   The variable value at a specific point in a zone.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilDataValueGetByZoneVar(
 *   &                   Zone,
 *   &                   Var,
 *   &                   PointIndex)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    INTEGER*4       PointIndex
 * </FortranSyntax>
 *   
 *   Get the twenty-first value of the second variable of zone 5:
 *   
 * @code
 *   double dd = TecUtilDataValueGetByZoneVar(5, 2, 21);
 *   // Use val.
 * @endcode
 */
LINKTOADDON double STDCALL TecUtilDataValueGetByZoneVar(EntIndex_t Zone,
                                                        EntIndex_t Var,
                                                        LgIndex_t  PointIndex);

/**
 * Get a handle to the data for the specified zone and variable in the data set
 * attached to the current frame.
 *   
 * @param Zone
 *   Number of the zone for which to get the field data
 *   
 * @param Var
 *   Number of the variable for which to get the field data
 *   
 * @return
 *   The field data handle to the specified zone and variable in the data set
 *   attached to the current frame.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetRef(
 *   &           Zone,
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 *
 * Function that loads the values of a field variable into 
 * a supplied double precision array.  Assume the array is
 * already dimensioned correctly and the dataset, zone, and 
 * variable exist.
 * @code
 * static void ReadVals(double     Var[],
 *                      EntIndex_t ZoneNum,
 *                      EntIndex_t VarNum)
 * {
 *   int          i;
 *   LgIndex_t    NumPointsInZone;
 *   FieldData_pa FieldData;
 *
 *   NumPointsInZone = GetNumPointsInZone(ZoneNum);
 *
 *   FieldData = TecUtilDataValueGetRef(ZoneNum, VarNum);
 *
 *   if (FieldData)
 *     {
 *       // Remember that the GetByRef function is 1-based....
 *       for (i = 0; i < NumPointsInZone; i++)
 *         Var[i] = TecUtilDataValueGetByRef(FieldData, i+1);
 *     }
 * }
 * @endcode
 */
LINKTOADDON FieldData_pa STDCALL TecUtilDataValueGetRef(EntIndex_t Zone,
                                                        EntIndex_t Var);


/**
 * Get the low-level "get value" function associated with a field
 * data handle. In general, using this function is faster than calling
 * TecUtilDataValueGetByRef().
 *
 * @since
 *   10.0-3-128
 *   
 * @par Note:
 *   Do not assume that raw data internal to Tecplot remain in the same
 *   location at all times. Always call TecUtilDataValueGetRef() and this
 *   function again after any event here Tecplot itself may move/alter the raw
 *   data. Make sure to call TecUtilStateChanged() after any field values have
 *   changed.
 *
 * @param FD
 *   A field data handle usually obtained via TecUtilDataValueGetRef().
 *
 * @return
 *   The low-level "get value" function associated with a field data handle.
 *   This function takes zero-based values and has no assertions associated
 *   with it.
 *   
 * <FortranSyntax>
 *   None.
 * </FortranSyntax>
 *
 *   Efficiently get the first twenty-one values of the second variable of zone 5:
 *   
 * @code
 *   FieldData_pa FD = TecUtilDataValueGetRef(5, 2);
 *   FieldValueGetFunction_pf GetFunction = TecUtilDataValueRefGetGetFunc(FD);
 *   LgIndex pt;
 *   for ( pt = 0; pt < 21; pt++ ) // use =0 and <21 becaused SetFunction is 0-based
 *     {
 *       double val = GetFunction(fd, pt);
 *       // Use val.
 *     }
 * @endcode
 */
LINKTOADDON FieldValueGetFunction_pf STDCALL TecUtilDataValueRefGetGetFunc(FieldData_pa FD);


/**
 * Get the low-level "set value" function associated with a field data handle.
 * In general, using this function is faster than calling
 * TecUtilDataValueSetByRef().
 *
 * @since
 *   10.0-3-128
 *   
 * @par Note:
 *   Do not assume that raw data internal to Tecplot remain in the same
 *   location at all times. Always call TecUtilDataValueGetRef() and this
 *   function again after any event here Tecplot itself may move/alter the raw
 *   data. Make sure to call TecUtilStateChanged() after any field values have
 *   changed.
 *
 * @param FD
 *   A field data handle usually obtained via TecUtilDataValueGetRef().
 *
 * @return
 *   The low-level "set value" function associated with a field data handle.
 *   This function takes zero-based values and has no assertions associated
 *   with it.
 *   
 * <FortranSyntax>
 *   None.
 * </FortranSyntax>
 *   
 *   Efficiently set the twenty-first values of the second variable of zone 5 to 17.6:
 *   
 * @code
 *   Set_pa altered_vars = TecUtilSetAlloc(TRUE);
 *   FieldData_pa FD = TecUtilDataValueGetRef(5, 2);
 *   FieldValueSetFunction_pf SetFunction = TecUtilDataValueRefSetGetFunc(FD);
 *   LgIndex pt;
 *   for ( pt = 0; pt < 21; pt++ ) // use =0 and <21 becaused SetFunction is 0-based
 *     SetFunction(fd, pt, 17.6);
 *   // inform Tecplot of var value change
 *   TecUtilSetAddMember(altered_vars, 2, TRUE);
 *   TecUtilStateChanged(StateChange_VarsAltered,
 *                       (ArbParam_t)altered_vars);
 *   TecUtilSetDealloc(&altered_vars);
 * @endcode
 */
LINKTOADDON FieldValueSetFunction_pf STDCALL TecUtilDataValueRefGetSetFunc(FieldData_pa FD);

/**
 * Get a finite-element node map handle to the specified zone in the data set
 * attached to the current frame.
 *   
 * @param Zone
 *   Number of the zone for which to get the node map handle. This must be a
 *   finite-element zone
 *   
 * @return
 *   The finite-element node map handle to the specified zone in the data set
 *   attached to the current frame.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataNodeGetRef(
 *   &           Zone,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 */
LINKTOADDON NodeMap_pa STDCALL TecUtilDataNodeGetRef(EntIndex_t Zone);
/**
 * Get a face neighbor handle to the specified zone in the data set attached to
 * the current frame.
 *   
 * @param Zone
 *   Number of the zone for which to get the face neighbor handle. This must be
 *   a finite-element zone
 *   
 * @return
 *   The face neighbor handle to the specified zone in the data set attached to
 *   the current frame.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceNbrGetRef(
 *   &           Zone,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 */
LINKTOADDON FaceNeighbor_pa STDCALL TecUtilDataFaceNbrGetRef(EntIndex_t Zone);
/**
 *   Get the field data type of a field data handle.
 *   
 * @param FieldData
 *   The field data handle for which to get the type
 *   
 * @return
 *   The field data type of FieldData. The possible values are:
 *   FieldDataType_Float, FieldDataType_Double, FieldDataType_Int32,
 *   FieldDataType_Int16, FieldDataType_Byte, or FieldDataType_Bit.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueGetRefType(FieldDataPtr)
 *    POINTER (FieldDataPtr, FieldData)
 * </FortranSyntax>
 *   
 *   Get the type of the data for variable 2 in zone 5 of the data set attached
 *   to the current frame and do something special if that type is
 *   FieldData_Bit:
 *   
 * @code
 *   FieldData_pa fd = TecUtilDataValueGetRef(5, 2);
 *   FieldDataType_e field_data_type = TecUtilDataValueGetRefType(fd);
 *   if ( field_data_type == FieldData_Bit )
 *     {
 *       // do something special
 *     }
 * @endcode
 */
LINKTOADDON FieldDataType_e STDCALL TecUtilDataValueGetRefType(FieldData_pa FieldData);




/**
 * Queries for the location of the variable.
 *   
 * @param Zone
 *   The zone number.
 *   
 * @param Var
 *   The variable number
 *   
 * @return
 *   The data type of the variable.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueGetLocation(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *   
 *
 *   Get the location of variable 2 in zone 5:
 *   
 * @code
 *   ValueLocation_e ValLoc = TecUtilDataValueGetLocation(5, 2);
 * @endcode
 */
LINKTOADDON ValueLocation_e STDCALL TecUtilDataValueGetLocation(EntIndex_t Zone,
                                                                EntIndex_t Var);
/**
 * Get the instructions of the last data loader used to load the data into the
 * data set attached to the current frame. If a foreign data set loader addon
 * was used to load the data, then the instruction string passed to the loader
 * is returned. If the data was loaded by Tecplot, then the DataSetReaderName
 * returned is "TECPLOT" and each file name in the data set is returned in the
 * DataSetLoaderInstructions stringlist parameter. The current frame must have
 * an attached data set when this function is used.
 *   
 * @par Note:
 *   This function now has less usefulness in Tecplot than it once did in
 *   previous versions. Tecplot now maintains a data journal which allows for
 *   more complex data load sequences. It is important to note that other data
 *   altering instructions may follow your data loader instructions. This
 *   function does not provide any such information to the add-on.
 *
 * @param DataSetLoaderName
 *   Name of the data set loader. You must use TecUtilStringDealloc() to free
 *   this string when you are done with it.
 *   
 * @param DataSetLoaderInstructions
 *   The data set loader instructions. You must use TecUtilStringListDealloc() to
 *   free this string list when you are done with it.
 *   
 * @return
 *   Returns TRUE if the data was loaded using a data set loader.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImportGetLoaderInstr(
 *   &                   DataSetLoaderName,
 *   &                   DataSetLoaderNameLength,
 *   &                   DataSetLoaderInstructionsPtr)
 *    CHARACTER*(*)   DataSetLoaderName
 *    INTEGER*4       DataSetLoaderNameLength
 *    POINTER         (DataSetLoaderInstructionsPtr, DataSetLoaderInstructions)
 * </FortranSyntax>
 *   
 * Get the data set loader and instructions used to load the current frame's
 * data set:
 *   
 * @code
 *   char *LoaderName;
 *   StringList_pa LoaderInstructs;
 *   Boolean_t IsOk = TecUtilImportGetLoaderInstr(&LoaderName,
 *                                                &LoaderInstructs);
 *   if (IsOk                    &&
 *       LoaderName != NULL      &&
 *       LoaderInstructs != NULL &&
 *       strcmp(LoaderName, "BANANA") == 0)
 *     {
 *       ...
 *     }
 *
 *   if (LoaderName != NULL)
 *     TecUtilStringDealloc(&LoaderName);
 *   if (LoaderInstructs != NULL)
 *     TecUtilStringListDealloc(&LoaderInstructs);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilImportGetLoaderInstr(char          **DataSetLoaderName,
                                                          StringList_pa  *DataSetLoaderInstructions);

/**
 * Display a message to the user and, if desired, prompt for yes or no input.
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *   
 * @param Message
 *   Character string to display at the top of the dialog. Must not be NULL
 *   
 * @param MessageBoxType
 *   The possible values are: Buttons Included:MessageBox_Error :
 *   OKMessageBox_Warning : OKMessageBox_Information : OKMessageBox_Question :
 *   OK, CancelMessageBox_YesNo : Yes, No
 *   
 * @return
 *   TRUE if the OK or Yes is clicked, FALSE if Cancel or No is clicked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogMessageBox(
 *   &                   Message,
 *   &                   MessageBoxType)
 *    CHARACTER*(*)   Message
 *    INTEGER*4       MessageBoxType
 * </FortranSyntax>
 *   
 *   Prompt the user for a response to a question:
 *   
 * @code
 *   if (TecUtilDialogMessageBox("Do you like green eggs and ham?",
 *                               MessageBox_Question))
 *    {
 *      // do something here...
 *    }
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogMessageBox(const char       *Message,
                                                      MessageBoxType_e  MessageBoxType);
/**
 * Launch a dialog that prompts the user for the minimum, maximum, and skip
 * values of a range.
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *   
 * @param MaxRangeValue
 *   Maximum value for the range. Currently, this value is displayed at the top
 *   of the Enter Index Range dialog and also used to interpret zero and
 *   negative values for the other parameters, but it does not limit what
 *   values the user can enter. You must check the values of the other
 *   parameters upon return and check their validity in your code. A value of
 *   zero for MaxRangeValue will turn off the maximum value processing
 *   
 * @param Min
 *   The value passed in is the default value displayed in the minimum text
 *   field. Upon return, this value holds the user-specified minimum value. If
 *   MaxRangeValue is not zero, a Min of zero is interpreted as MaxRangeValue,
 *   and negative values of Min are interpreted as subtracted from
 *   MaxRangeValue
 *   
 * @param Max
 *   The value passed in is the default value displayed in the maximum text
 *   field. Upon return, this value holds the user-specified maximum value.
 *   Zero and negative values are interpreted as with Min above
 *   
 * @param Skip
 *   The value passed in is the default value displayed in the skip text field.
 *   Upon return, this value holds the user-specified skip value. Zero and
 *   negative values are interpreted as with Min above
 *   
 * @return
 *   TRUE if successful, FALSE if not. FALSE usually indicates that Cancel on
 *   the dialog was clicked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetIndexRange(
 *   &                   MaxRangeValue,
 *   &                   Min,
 *   &                   Max,
 *   &                   Skip)
 *    INTEGER*4       MaxRangeValue
 *    INTEGER*4       Min
 *    INTEGER*4       Max
 *    INTEGER*4       Skip
 * </FortranSyntax>
 *   
 *   Launch a dialog that prompts the user for the minimum, maximum, and skip values of a range:
 *   
 * @code
 *   
 *   LgIndex_t Min = 0, Max = 100, Skip = 1;
 *   TecUtilDialogGetIndexRange(100,&Min,&Max,&Skip);
 *   // values returned in Min,Max, and skip
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetIndexRange(LgIndex_t  MaxRangeValue,
                                                         LgIndex_t *Min,
                                                         LgIndex_t *Max,
                                                         LgIndex_t *Skip);
/**
 * Launch a dialog that prompts the user for one, two, or three variables.
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *   
 * @param Instructions
 *   Character string displayed at the top of the dialog. Must not be NULL
 *   
 * @param TextField1Label
 *   Character string displayed to the left of the first variable's selection
 *   menu. May be NULL
 *   
 * @param TextField2Label
 *   Character string displayed to the left of the second variable's selection
 *   menu. Only used if Var2 is not NULL. May be NULL
 *   
 * @param TextField3Label
 *   Character string displayed to the left of the third variable's selection
 *   menu. Only used if Var3 is not NULL. May be NULL.
 *   
 * @param Var1
 *   Value passed in is the default value displayed in the first variable's
 *   selection menu. Must not be NULL, and must be a valid variable number.
 *   Upon return, this value holds the user-selected first variable value
 *   
 * @param Var2
 *   Value passed in is the default value displayed in the second variable's
 *   selection menu. Use NULL to prompt for only one variable. May be NULL, but
 *   if not NULL, must be a valid variable number.  Upon return, this value
 *   holds the user-selected second variable value
 *   
 * @param Var3
 *   Value passed in is the default value displayed in the third variable's
 *   selection menu. Use NULL to prompt for only one or two variables. May be
 *   NULL, but if not NULL, must be a valid variable number. Upon return, this
 *   value holds the user-selected third variable value.
 *   
 * @return
 *   TRUE if successful, FALSE if not. FALSE usually indicates that Cancel on the dialog was clicked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetVariables(
 *   &                   Instructions,
 *   &                   TextField1Label,
 *   &                   TextField2Label,
 *   &                   TextField3Label,
 *   &                   Var1,
 *   &                   Var2,
 *   &                   Var3)
 *    CHARACTER*(*)   Instructions
 *    CHARACTER*(*)   TextField1Label
 *    CHARACTER*(*)   TextField2Label
 *    CHARACTER*(*)   TextField3Label
 *    INTEGER*4       Var1
 *    INTEGER*4       Var2
 *    INTEGER*4       Var3
 * </FortranSyntax>
 *   
 *   Prompt the user for two variables:
 *   
 * @code
 *   EntIndex_t Var1 = 1,Var2 = 1;
 *   if (TecUtilDialogGetVariables("Enter the values for V1 and V2:",
 *        "V1:", "V2:", NULL, &Var1, &Var2, NULL)
 *   {
 *     // values successfully entered
 *   }
 * @endcode
 *
 *
 * FORTRAN Example.  
 *
 * @code
 *   INTEGER*4 IErr
 *   INTEGER*4 Var1
 *   INTEGER*4 Var2
 *   POINTER   (NullPntr, Null)
 *   INTEGER*4 Null
 *
 *   NullPntr = 0
 *
 *   Call TecUtilLockStart(AddOnID)
 *
 *   Var1 = 1
 *   Var2 = 1
 *
 *   IErr = TecUtilDialogGetVariables(
 *  &       'Get some variables'//char(0),
 *  &       'label 1'//char(0),
 *  &       'label 2'//char(0),
 *  &       char(0),
 *  &       Var1,
 *  &       Var2,
 *  &       Null)
 *
 *  .... do something with Var1 and Var2 .....
 *
 *  Call TecUtilLockFinish(AddOnID)
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetVariables(const char *Instructions,
                                                        const char *TextField1Label,
                                                        const char *TextField2Label,
                                                        const char *TextField3Label,
                                                        EntIndex_t *Var1,
                                                        EntIndex_t *Var2,
                                                        EntIndex_t *Var3);






/**
 * Launch a dialog that prompts the user for a color or for a multi-color
 * setting.
 *
 * @since
 *   10.0-3-129
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *   
 * @param AllowMultiColor
 *   Set to TRUE if you wish to allow the user to select RGB or multi-color.
 *   
 * @param Color
 *   The returned color value. If AllowMultiColor is FALSE then this is in the
 *   range between Black_C and Custom56_C (See GLOBAL.h). If AllowMultiColor
 *   is TRUE then the following constants may also be returned:
 *
 * @verbatim
     MultiColor_C             The user selected the first "MultiColor" option
                              which implies they want the object colored by
                              the contour variable used by contour group 1.

     MultiColor2_C            The user selected the "C2" button
                              which implies they want the object colored by
                              the contour variable used by contour group 2.

     MultiColor3_C            The user selected the "C3" button
                              which implies they want the object colored by
                              the contour variable used by contour group 3.

     MultiColor4_C            The user selected the "C4" button
                              which implies they want the object colored by
                              the contour variable used by contour group 4.

     RGBColor_C               The user selected the "RGB" button
                              which implies they want the object colored by
                              RGB.
   @endverbatim
 *   
 * @return
 *   TRUE if successful, FALSE if the user pressed the "Cancel" button in the
 *   dialog.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetColor(
 *   &                   AllowMultiColor,
 *   &                   Color)
 *    INTEGER*4       AllowMultiColor
 *    INTEGER*4       Color
 * </FortranSyntax>
 *   
 * Prompt the user for a basic color (no multi-color options):
 *   
 * @code
 *   ColorIndex_t Color;
 *   if (TecUtilDialogGetColor(FALSE, // AllowMultiColor
 *                             &Color))
 *     {
 *       // Do something with Color.
 *     }
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetColor(Boolean_t     AllowMultiColor,
                                                    ColorIndex_t *Color);




/**
 * Launch a dialog to prompt the user to input into a simple text field.
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *   
 * @param Instructions
 *   String containing the instructions for the user. Tecplot will wrap the
 *   instructions for you.  However, if you include a newline in the string it
 *   will force a new line. Under Windows you are limited to three lines of
 *   text
 *   
 * @param DefaultText
 *   Set to TRUE if the Text parameter being passed in has been preset with a
 *   default value for the user. May be NULL
 *   
 * @param Text
 *   The resulting text string is placed here.
 *   
 * @return
 *   TRUE if the user enters text and clicks OK.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetSimpleText(
 *   &                   Instructions,
 *   &                   DefaultText,
 *   &                   Text,
 *   &                   TextLength)
 *    CHARACTER*(*)   Instructions
 *    CHARACTER*(*)   DefaultText
 *    CHARACTER*(*)   Text
 *    INTEGER*4       TextLength
 * </FortranSyntax>
 *   
 *   Prompt the user for text:
 *   
 * @code
 *   char *Text = NULL;
 *   
 *   if (TecUtilDialogGetSimpleText("Enter your name","Fred",&Text))
 *      {
 *       // Do somthing with Text
 *       TecUtilStringDealloc(&Text);
 *      }
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetSimpleText(const char  *Instructions,
                                                         const char  *DefaultText,
                                                         char       **Text);


/**
 *   Get the position of the four corners of the box surrounding the text object.
 *   
 * @param T
 *   Handle to a text object.
 *   
 * @param X1
 *   Returned X-Coordinate for bottom left corner of the text box.
 *   
 * @param Y1
 *   Returned Y-Coordinate for bottom left corner of the text box.
 *   
 * @param X2
 *   Returned X-Coordinate for bottom right corner of the text box.
 *   
 * @param Y2
 *   Returned Y-Coordinate for bottom right corner of the text box.
 *   
 * @param X3
 *   Returned X-Coordinate for upper right corner of the text box.
 *   
 * @param Y3
 *   Returned Y-Coordinate for upper right corner of the text box.
 *   
 * @param X4
 *   Returned X-Coordinate for upper left corner of the text box.
 *   
 * @param Y4
 *   Returned Y-Coordinate for upper left corner of the text box.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextBoxGetPosition(
 *   &           T,
 *   &           X1,
 *   &           Y1,
 *   &           X2,
 *   &           Y2,
 *   &           X3,
 *   &           Y3,
 *   &           X4,
 *   &           Y4)
 *    INTEGER*4       T
 *    REAL*8          X1
 *    REAL*8          Y1
 *    REAL*8          X2
 *    REAL*8          Y2
 *    REAL*8          X3
 *    REAL*8          Y3
 *    REAL*8          X4
 *    REAL*8          Y4
 * </FortranSyntax>
 *   
 *   Obtain the four corners of the text referenced by T.
 *   
 * @code
 *   // variable T of type Text_ID already obtained at this point.
 *   double X1,Y1,X2,Y2,X3,Y3,X4,Y4;
 *   TecUtilTextBoxGetPosition(T,&X1,&Y1,&X2,&Y2,&X3,&Y3,&X4,&Y4);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilTextBoxGetPosition(Text_ID  T,
                                                   double  *X1,
                                                   double  *Y1,
                                                   double  *X2,
                                                   double  *Y2,
                                                   double  *X3,
                                                   double  *Y3,
                                                   double  *X4,
                                                   double  *Y4);



/**
 * Query Tecplot to see if a macro function called FunctionName exists.
 *
 * @par Note:
 *   This function requires Tecplot Version 7.5-0-6 or newer.
 *   
 * @param FunctionName
 *   Name of the macro function.
 *   
 * @return
 *   TRUE if the function exists, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroFunctionExists(FunctionName)
 *    CHARACTER*(*) FunctionName
 * </FortranSyntax>
 *   
 *   If the macro function "abc" exists, then execute it.
 *   
 * @code
 *   if (TecUtilMacroFunctionExists("abc"))
 *      {
 *         TecUtilMacroRunFunction("abc",(char *)NULL);
 *      }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroFunctionExists(const char *FunctionName);
/**
 *   Determine if Tecplot is currently running in batch mode.
 *   
 * @return
 *   TRUE if Tecplot is in batch mode, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroIsBatchModeActive()
 * </FortranSyntax>
 *   
 *   Perform some operations if Tecplot is not running in batch mode:
 *   
 * @code
 *   if (!TecUtilMacroIsBatchModeActive())
 *     {
 *       // Perform some operations
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroIsBatchModeActive(void);


/**
 *   
Get the number of pixels per inch in the vertical and horizontal directions on the screen.
 *   
 * @param VDotsPerInch
 *   The number of pixels per inch in the vertical direction
 *   
 * @param HDotsPerInch
 *   The number of pixels per inch in the horizontal direction.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilInterfaceGetDotsPerInch(
 *   &           VDotsPerInch,
 *   &           HDotsPerInch)
 *    REAL*8          VDotsPerInch
 *    REAL*8          HDotsPerInch
 * </FortranSyntax>
 *   
 *   Get the pixels per inch values:
 *   
 * @code
 *   double VertPixels, HorzPixels;
 *   TecUtilInterfaceGetDotsPerInch(&VertPixels, &HorzPixels);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilInterfaceGetDotsPerInch(double *VDotsPerInch,
                                                        double *HDotsPerInch);

#if defined COMPILE_BUT_HIDE_FROM_DOC
LINKTOADDON int STDCALL TecUtilInterfaceGetBaseFontSize(void);
#endif


/**
 * Fetch an Array of values by reference.
 * This function fetches the specified number of values from the source field
 * data starting at the specified source item offset and copies them to the
 * base of the destination value array. Note that the destination value array
 * must be of the same data type as the source field data.
 *
 * @since
 *     10.0-3-12
 *
 * @param SourceFieldData
 *     Field data containing the data to fetch.
 * @param SourceOffset
 *     Member offset in the source field data to begin fetching values.
 * @param SourceCount
 *     Number of values to fetch from the source field data.
 * @param DestValueArray
 *     Pre-allocated array large enough to hold the requested members. The
 *     first member is placed at the base of the array.
 */
LINKTOADDON void STDCALL TecUtilDataValueArrayGetByRef(FieldData_pa  SourceFieldData,
                                                       LgIndex_t     SourceOffset,
                                                       LgIndex_t     SourceCount,
                                                       void         *DestValueArray);
/**
 * Get a field data value. To use this function you must have already obtained
 * a handle to field data.
 *   
 * @param FieldData
 *   Handle to the field data. Use TecUtilDataValueGetRef() or
 *   TecUtilZoneGetInfo() to get handles to field data.
 *   
 * @param PointIndex
 *   Position in the array of field data values. Position starts at one. If
 *   FieldData came from an IJ- or IJK-ordered zone then the position is
 *   calculated by treating the two- or three-dimensional array as a
 *   one-dimensional array.
 *   
 * @return
 *   The value at a given position in field data FieldData
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilDataValueGetByRef(
 *   &                   FieldDataPtr,
 *   &                   PointIndex)
 *    POINTER         (FieldDataPtr, FieldData)
 *    INTEGER*4       PointIndex
 * </FortranSyntax>
 *   
 *   Get the first twenty data values for the second variable in zone 5:
 *   
 * @code
 *   LgIndex_t numpts = 20; // assume we know this
 *   FieldData_pa fd;
 *   fd = TecUtilDataValueGetRef(5, 2);
 *   if ( fd )
 *     {
 *       int ii;
 *       for ( ii = 1; ii <= numpts; ii++ )
 *         {
 *           double val = TecUtilDataValueGetByRef(fd, ii);
 *           // do something with val
 *         }
 *     }
 * @endcode
 *
 * @sa TecUtilDataValueRefGetGetFunc() for obtaining a function as a high
 *     performance alternative.
 */
LINKTOADDON double STDCALL TecUtilDataValueGetByRef(FieldData_pa FieldData,
                                                    LgIndex_t    PointIndex);
/**
 * Get the minimum and maximum values for a tecplot variable using a reference
 * to the variable.
 *   
 * @param FieldData
 *   Handle to the variable to be examined
 *   
 * @param Min
 *   Returned minimum value.
 *   
 * @param Max
 *   Returned maximum value.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetMinMaxByRef(
 *   &           FieldDataPtr,
 *   &           Min,
 *   &           Max)
 *    POINTER         (FieldDataPtr, FieldData)
 *    REAL*8          Min
 *    REAL*8          Max
 * </FortranSyntax>
 *   
 * Get the minimum and maximum values for the third variable in zone 2 in the
 * current data set.
 *   
 * @code
 *   FieldData_pa FD;
 *   double       Min;
 *   double       Max;
 *   
 *   FD = TecUtilDataValueGetRef(2,3);
 *   TecUtilDataValueGetMinMaxByRef(FD,&Min,&Max);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDataValueGetMinMaxByRef(FieldData_pa  FieldData,
                                                        double       *Min,
                                                        double       *Max);
/**
 * Get the node index for a particular corner of a finite-element. This
 * function does not require you to obtain the handle to the node map as does
 * TecUtilDataNodeGetByRef(), however, this function is not very efficient. Use
 * TecUtilDataNodeGetByRef() if you are getting multiple nodes from the same
 * zone.
 *   
 * @param Zone
 *   Zone number. This must be a finite-element zone.
 *   
 * @param Element
 *   The element number (starts at 1)
 *   
 * @param Corner
 *   The element corner (starts at 1)
 *   
 * @return
 *   The index of the node.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataNodeGetByZone(
 *   &                   Zone,
 *   &                   Element,
 *   &                   Corner)
 *    INTEGER*4       Zone
 *    INTEGER*4       Element
 *    INTEGER*4       Corner
 * </FortranSyntax>
 *   
 *   Get the third node of the 43rd element of zone 5:
 *   
 * @code
 *   
 *   LgIndex_t n3;
 *   n3 = TecUtilDataNodeGetByZone(5, 43, 3);
 * @endcode
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataNodeGetByZone(EntIndex_t Zone,
                                                       LgIndex_t  Element,
                                                       LgIndex_t  Corner);

/**
 *   Get the node index for a particular corner of a finite-element. To use
 *   this function you must have already obtained a handle to a node map.
 *   
 * @param NodeMapPtr
 *   Handle to the connectivity list (that is, the node map). Use
 *   TecUtilDataNodeGetByRef() or TecUtilZoneGetInfo() to get handles to the
 *   zone map
 *   
 * @param Element
 *   The element number (starts at 1)
 *   
 * @param Corner
 *   The element corner (starts at 1)
 *   
 * @return
 *   The index of the node.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataNodeGetByRef(
 *   &                   NodeMapPtrPtr,
 *   &                   Element,
 *   &                   Corner)
 *    POINTER         (NodeMapPtrPtr, NodeMapPtr)
 *    INTEGER*4       Element
 *    INTEGER*4       Corner
 * </FortranSyntax>
 *   
 *   Get the first two nodes of the 43rd element of zone 5:
 *   
 * @code
 *   NodeMap_pa nm;
 *   nm = TecUtilDataNodeGetRef(5);
 *   if ( nm )
 *     {
 *       LgIndex_t n1, n2;
 *       n1 = TecUtilDataNodeGetByRef(nm, 43, 1);
 *       n2 = TecUtilDataNodeGetByRef(nm, 43, 2);
 *       // use n1 and n2
 *     }
 * @endcode
 */
LINKTOADDON LgIndex_t STDCALL TecUtilDataNodeGetByRef(NodeMap_pa NodeMapPtr,
                                                      LgIndex_t  Element,
                                                      LgIndex_t  Corner);
/**
 * @deprecated
 *   Please use TecUtilDataFaceNbrGetNbrByRef() instead.
 *
 * @sa TecUtilDataFaceNbrGetNumNByRef() and TecUtilDataFaceNbrGetModeByRef()
 */
LINKTOADDON LgIndex_t STDCALL  TecUtilDataFaceNbrGetByZone(EntIndex_t Zone,
                                                           LgIndex_t  Element,
                                                           LgIndex_t  Face);
/**
 * @deprecated
 *   Please use TecUtilDataFaceNbrGetNbrByRef() instead.
 *
 * @sa TecUtilDataFaceNbrGetNumNByRef() and TecUtilDataFaceNbrGetModeByRef()
 */
LINKTOADDON LgIndex_t STDCALL  TecUtilDataFaceNbrGetByRef(FaceNeighbor_pa FaceNeighbor,
                                                          LgIndex_t       Element,
                                                          LgIndex_t       Face);

/**
 *   Returns the FaceNeigborMode_e value for the referenced face neighbor.
 *   
 * @param FaceNeighbor
 *   The face neighbor handle to the specified zone in the data set attached to the current frame
 *   
 * @return
 *   Mode of the face neighbor.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceNbrGetModeByRef(FaceNeighborPtr)
 *    POINTER (FaceNeighborPtr, FaceNeighbor)
 * </FortranSyntax>
 */
LINKTOADDON FaceNeighborMode_e STDCALL TecUtilDataFaceNbrGetModeByRef(FaceNeighbor_pa FaceNeighbor);
   
/**
 *   Gets the number of face neighbors for the elements's face.
 *   
 * @param FaceNeighbor
 *   Handle to the face neighbors. Use TecUtilDataFaceNbrGetRef() to get
 *   handles to the face neighbors
 *   
 * @param Element
 *   The element number (starts at one)
 *   
 * @param Face
 *   The face number of the element. Different element types have different
 *   number of faces. Use TecUtilZoneGetType() to get the element type for a
 *   particular zone. Face numbers start at one.ZoneType_FETriangle: Three
 *   faces.ZoneType_FEQuad: Four faces.ZoneType_FETetra: Four
 *   faces.ZoneType_FEBrick: Six faces
 *   
 * @return
 *   Number of neighbors for the element's face.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceNbrGetNumNByRef(
 *   &                   FaceNeighborPtr,
 *   &                   Element,
 *   &                   Face)
 *    POINTER         (FaceNeighborPtr, FaceNeighbor)
 *    INTEGER*4       Element
 *    INTEGER*4       Face
 * </FortranSyntax>
 *   
 *   Get the number of neighbors for face 5 of element 23 of zone 2. It is assumed that zone 2 is of
 *   type ZoneType_FEBrick.
 *   
 * @code
 *   LgIndex_t NumNeighbors;
 *   FaceNeighbor_pa FNbr;
 *   FNbr = TecUtilDataFaceNbrGetRef(2);
 *   NumNeighbors = TecUtilDataFaceNbrGetNumNByRef(FNbr, 23, 5);
 * @endcode
 */
LINKTOADDON LgIndex_t STDCALL  TecUtilDataFaceNbrGetNumNByRef(FaceNeighbor_pa FaceNeighbor,
                                                              LgIndex_t       Element,
                                                              LgIndex_t       Face);
/**
 * Get the cell index of the element the is a neighbor of the specified Element
 * and Face. To use this function you must have already obtained a handle to
 * face neighbors.
 *   
 * @param FaceNeighbor
 *   Handle to the face neighbors. Use TecUtilDataFaceNbrGetRef() to get
 *   handles to the face neighbors.
 *   
 * @param Element
 *   The element number (starts at one).
 *   
 * @param Face
 *   The face number of the element. Different element types have different
 *   number of faces. Use TecUtilZoneGetType() to get the element type for a
 *   particular zone. Face numbers start at one.ZoneType_FETriangle: Three
 *   faces.ZoneType_FEQuad: Four faces.ZoneType_FETetra: Four
 *   faces.ZoneType_FEBrick: Six faces.Please see section 4.1.2.g "Face
 *   Neighbors" of the Tecplot User's Manual for a description of how nodes and
 *   faces map to one another.
 *   
 * @param NeighborNumber
 *   Specify which neighbor to retrieve. Use TecUtilDataFaceNbrGetNumNByRef() to get the number of
 *   neighbors
 *   
 * @param NeighborElem
 *   Pointer that gives the value of the neighboring element number
 *   
 * @param NeighborZone
 *   Pointer that gives the value of the neighboring zone number
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataFaceNbrGetNbrByRef(
 *   &           FaceNeighborPtr,
 *   &           Element,
 *   &           Face,
 *   &           NeighborNumber,
 *   &           NeighborElem,
 *   &           NeighborZone)
 *    POINTER         (FaceNeighborPtr, FaceNeighbor)
 *    INTEGER*4       Element
 *    INTEGER*4       Face
 *    INTEGER*4       NeighborNumber
 *    INTEGER*4       NeighborElem
 *    INTEGER*4       NeighborZone
 * </FortranSyntax>
 *   
 *   Get the cell index of the cell next to face 5 of element 23 in zone 2. It is assumed that zone 2
 *   is of type ZoneType_FEBrick.
 *   
 * @code
 *   FaceNeighbor_pa FNPtr = NULL;
 *   LgIndex_t NeighborElem;
 *   EntIndex_t NeighborZone;
 *   FNPtr = TecUtilDataFaceNbrGetRef(2);
 *   If (FNPtr != NULL)
 *     {
 *       TecUtilDataFaceNbrGetNbrByRef(FNPtr, 23, 5, 1,
 *    &NeighborElem,
 *    &NeighborZone);
 *   
 *       // Do something with NeighborElem and NeighborZone.
 *    }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDataFaceNbrGetNbrByRef(FaceNeighbor_pa  FaceNeighbor,
                                                       LgIndex_t        Element,
                                                       LgIndex_t        Face,
                                                       LgIndex_t        NeighborNumber,
                                                       LgIndex_t       *NeighborElem,
                                                       EntIndex_t      *NeighborZone);
/**
 * Clears any previous user defined face neighbor assignments and opens a new
 * face neighbor assignment context. One and only one face neighbor assignment
 * context must be open prior to calling TecUtilDataFaceNbrAssign(). The
 * context must be closed with a call to TecUtilDataFaceNbrEndAssign() when all
 * face neighbor assignments have been delivered.
 *   
 * @param Zone
 *   Zone number to which the face neighors are to be assigned.
 *   
 * @return
 *   TRUE if successful in opening the context, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceNbrBeginAssign(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL  TecUtilDataFaceNbrBeginAssign(EntIndex_t Zone);
/**
 *   Sets the user defined face neighbors within an open face neighbor assignment context for the
 *   specified element and face.
 *   
 * @param Element
 *   The element number (starts at one)
 *   
 * @param Face
 *   Face for which the face neighbor information is desired
 *   
 * @param NeighborsCompletelyObscure
 *   Set to TRUE if the supplied neighbors completely obscure the face.
 *   
 * @param NumNeighbors
 *   Number of neighbors for this face
 *   
 * @param NeighborElems
 *   Array containing the element numbers of the neighbors
 *   
 * @param NeighborZones
 *   Array containing the zone numbers of the neighbors
 *   
 * @return
 *   TRUE if successful in assigning to the context, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceNbrAssign(
 *   &                   Element,
 *   &                   Face,
 *   &                   NeighborsCompletelyObscure,
 *   &                   NumNeighbors,
 *   &                   NeighborElems,
 *   &                   NeighborZones)
 *    INTEGER*4       Element
 *    INTEGER*4       Face
 *    INTEGER*4       NeighborsCompletelyObscure
 *    INTEGER*4       NumNeighbors
 *    INTEGER*4       NeighborElems
 *    INTEGER*4       NeighborZones
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL  TecUtilDataFaceNbrAssign(LgIndex_t   Element,
                                                        LgIndex_t   Face,
                                                        Boolean_t   NeighborsCompletelyObscure,
                                                        LgIndex_t   NumNeighbors,
                                                        LgIndex_t  *NeighborElems,
                                                        EntIndex_t *NeighborZones);
/**
 * Closes the open face neighbor assignment context and packs the assignments
 * into an efficient storage within Tecplot.
 *   
 * @return
 *   TRUE if successful in assigning to the context, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataFaceNbrEndAssign()
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL  TecUtilDataFaceNbrEndAssign(void);


/**
 * @deprecated
 *   Please use TecUtilLineMapGetCount() instead.
 */
LINKTOADDON EntIndex_t STDCALL TecUtilXYMapGetCount(void);




/**
 *   Returns the number of Line-maps.
 *   
 * @return
 *   The number of Line-maps.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapGetCount()
 * </FortranSyntax>
 */
LINKTOADDON EntIndex_t STDCALL TecUtilLineMapGetCount(void);

/**
 *   Determine if Tecplot is currently recording a macro.
 *   
 * @return
 *   TRUE if Tecplot is recording macros, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroIsRecordingActive()
 * </FortranSyntax>
 *   
 *   If Tecplot is currently recording a macro, record the macro command "GO"
 *   for the addon "BANANA":
 *   
 * @code
 *   if (TecUtilMacroIsRecordingActive())
 *     {
 *       TecUtilMacroRecordAddOnCommand("BANANA", "GO");
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroIsRecordingActive(void);

/**
 * Query Tecplot for a limit value. These are the same values you can set using
 * the $!LIMITS command in the Tecplot macro language. Note that this function
 * requires Tecplot Version 7.5-0-6 or newer.
 *   
 * @param LimitString
 *   This must be one of the following: MaxPtsInALine, MaxChrsInTextLabels,
 *   MaxNumContourLevels, MaxPreplotZones, IPCTimeoutSeconds.
 *   
 * @return
 *   Returns the limit value as an integer.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLimitGetValue(LimitString)
 *    CHARACTER*(*) LimitString
 * </FortranSyntax>
 *   
 *   Get the number of points allowed per polyline in Tecplot:
 *   
 * @code
 *   LgIndex_t MaxPtsPerLine;
 *   MaxPtsPerLine = TecUtilLimitGetValue("MaxPtsInALine");
 * @endcode
 */
LINKTOADDON LgIndex_t STDCALL TecUtilLimitGetValue(const char *LimitString);

/**
 *   Determine if the current frame has a data set attached.
 *   
 * @return
 *   TRUE if the current frame has an attached data set, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetIsAvailable()
 * </FortranSyntax>
 *   
 *   Get the number of zones for the data set for the current frame, or use
 *   zero if there is no data set:
 *   
 * @code
 *   EntIndex_t nzones = 0;
 *   if ( TecUtilDataSetIsAvailable() )
 *     TecUtilDataSetGetInfo(NULL, &nzones, NULL);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetIsAvailable(void);

/**
 *   Determine if a variable is enabled.
 *   
 * @return
 *   TRUE, if a variable is enabled, otherwise, FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilVarIsEnabled(Var)
 *    INTEGER*4 Var
 * </FortranSyntax>
 *   
 *   Check if the first variable is enabled:
 *   
 * @code
 *   if (TecUtilVarIsEnabled(1))
 *     {
 *       // sure is!
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilVarIsEnabled(EntIndex_t Var);

/**
 *   Determine if a zone is enabled.
 *   
 * @param Zone
 *   Number of the zone for which to get the zone type information
 *   
 * @return
 *   TRUE, if a zone is enabled, otherwise, FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneIsEnabled(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *   
 *   Check if the first zone is enabled:
 *   
 * @code
 *   if (TecUtilZoneIsEnabled(1))
 *   {
 *     // sure is!
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneIsEnabled(EntIndex_t Zone);

/**
 *   Determine if a zone is active.
 *   
 * @return
 *   Returns TRUE if the zone is active, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneIsActive(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneIsActive(EntIndex_t Zone);

/**
 * @deprecated
 *   Please use TecUtilLineMapIsActive() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilXYMapIsActive(EntIndex_t XYMap);
/**
 *   Determine if an Line-map is active.
 *   
 * @return
 *   Returns TRUE if the Line-map is active, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilLineMapIsActive(LineMap)
 *    INTEGER*4 LineMap
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilLineMapIsActive(EntIndex_t LineMap);



/**
 * @deprecated
 *   Please use TecUtilFileGetTempName() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilGetTempFileName(char **TempFileName);



/**
 * Creates a temporary file name, including a full path. An empty file with the
 * temporary is file name is also created.
 *   
 * @param TempFileName
 *   Receives the temporary file name, including path. You must later free this
 *   string with TecUtilStringDealloc()
 *   
 * @return
 *   TRUE if successful, FALSE otherwise. If the return value is FALSE,
 *   *FileName is set to NULL.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFileGetTempName(
 *   &                   TempFileName,
 *   &                   TempFileNameLength)
 *    CHARACTER*(*)   TempFileName
 *    INTEGER*4       TempFileNameLength
 * </FortranSyntax>
 *   
 *   Create a temporary file.
 *   
 * @code
 *   
 *   {
 *     FILE *TempFile = NULL;
 *     char *TempFileName = NULL;
 *   
 *     if ( TecUtilFileGetTempName(&TempFileName) )
 *       {
 *         TempFile = fopen(TempFileName,"w");
 *         if ( TempFile )
 *           {
 *             // Do something with the temp file.
 *             fclose(TempFile);
 *           }
 *         TecUtilStringDealloc(&TempFileName);
 *         // Be sure to deallocate the string when finished.
 *       }
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilFileGetTempName(char **TempFileName);

/**
 *   Get the RGB components of a basic color.
 *   
 * @param BasicColor
 *   Index of basic color to query.   It is best to use the supplied
 *   constants in GLOBAL.h (e.g. Black_C, Red_C, Custom9_C).  
 *   
 * @param Red
 *   Receives red component of the color. This parameter may be NULL.
 *   
 * @param Green
 *   Receives green component of the color. This parameter may be NULL.
 *   
 * @param Blue
 *   Receives blue component of the color. This parameter may be NULL
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilColorMapGetBasicColorRGB(
 *   &           BasicColor,
 *   &           Red,
 *   &           Green,
 *   &           Blue)
 *    INTEGER*4       BasicColor
 *    INTEGER*4       Red
 *    INTEGER*4       Green
 *    INTEGER*4       Blue
 * </FortranSyntax>
 *   
 *   Get the RGB components of the Custom2_C basic color.
 *   
 * @code
 *   {
 *     Byte R,G,B;
 *     TecUtilColorMapGetBasicColorRGB(Custom2_C,&R,&G,&B);
 *   }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilColorMapGetBasicColorRGB(ColorIndex_t  BasicColor,
                                                         ColorIndex_t *Red,
                                                         ColorIndex_t *Green,
                                                         ColorIndex_t *Blue );

/**
 *   Get the number of basic colors in Tecplot.
 *   
 * @return
 *   The number of basic colors in Tecplot.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilColorMapNumBasicColors()
 * </FortranSyntax>
 *   
 *   Get the number of basic colors in Tecplot.
 *   
 * @code
 *   {
 *     LgIndex_t NumBasicColors;
 *   
 *     TecUtilLockStart(AddOnID);
 *     NumBasicColors = TecUtilColorMapNumBasicColors();
 *   
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON LgIndex_t STDCALL TecUtilColorMapNumBasicColors(void);

/**
 *   Queries the auto redraw state.
 *   
 * @return
 *   Returns TRUE if Auto Redraw is active, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAutoRedrawIsActive()
 * </FortranSyntax>
 *   
 *   Query the auto redraw state.
 *   
 * @code
 *   {
 *   Boolean_t AutoRedrawIsActive;
 *   TecUtilLockStart(AddOnID);
 *   AutoRedrawIsActive = TecUtilAutoRedrawIsActive();
 *   TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAutoRedrawIsActive(void);

/**
 * Gets a reference to the current data set's auxiliary data.
 *   
 * @return
 *   Reference to the current data set's auxiliary data.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataDataSetGetRef(ResultPtr)
 *    POINTER (ResultPtr, Result)
 * </FortranSyntax>
 */
LINKTOADDON AuxData_pa STDCALL TecUtilAuxDataDataSetGetRef(void);

/**
 * Gets a reference to the specified line map's auxiliary data.
 *
 * @since
 *   10.0-3-129
 *   
 * @param Map
 *   Line map number for which the auxiliary data is desired.
 *   
 * @return
 *   Reference to the specified line map's auxiliary data.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataLineMapGetRef(
 *   &           Map,
 *   &           ResultPtr)
 *    INTEGER*4       Map
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 */
LINKTOADDON AuxData_pa STDCALL TecUtilAuxDataLineMapGetRef(EntIndex_t Map);

/**
 * Gets a reference to the current frame's auxiliary data.
 *   
 * @return
 *   Reference to the current frame's auxiliary data..
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataFrameGetRef(ResultPtr)
 *    POINTER (ResultPtr, Result)
 * </FortranSyntax>
 */
LINKTOADDON AuxData_pa STDCALL TecUtilAuxDataFrameGetRef(void);

/**
 * Gets a reference to the specified zone's auxiliary data.
 *   
 * @param Zone
 *   Zone number for which the auxiliary data is desired.
 *   
 * @return
 *   Reference to the specified zone's auxiliary data.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataZoneGetRef(
 *   &           Zone,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 */
LINKTOADDON AuxData_pa STDCALL TecUtilAuxDataZoneGetRef(EntIndex_t Zone);

/**
 * Gets a reference to the specified variable's auxiliary data.
 *
 * @since
 *   10.0-3-129
 *   
 * @param Var
 *   Variable number for which the auxiliary data is desired.
 *   
 * @return
 *   Reference to the specified variable's auxiliary data.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataVarGetRef(
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 */
LINKTOADDON AuxData_pa STDCALL TecUtilAuxDataVarGetRef(EntIndex_t Var);

/**
 * Gets the current number of auxiliary data items maintained the auxiliary
 * data reference.
 *   
 * @param AuxDataRef
 *   Reference to auxiliary data.
 *   
 * @return
 *   Number of items maintained by the auxiliary data.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAuxDataGetNumItems(AuxDataRefPtr)
 *    POINTER (AuxDataRefPtr, AuxDataRef)
 * </FortranSyntax>
 *   
 * Find the number of auxiliary data items linked to the frame:
 *   
 * @code
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       LgIndex_t NumItems = TecUtilAuxDataGetNumItems(AuxDataRef);
 *       if (NumItems != 0)
 *         {
 *           // ... do something with the 1..NumItems items ...
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 */
LINKTOADDON LgIndex_t STDCALL TecUtilAuxDataGetNumItems(AuxData_pa AuxDataRef);

/**
 * Gets a reference to the current frame's auxiliary data.
 *   
 * @param AuxDataRef
 *   Reference to auxiliary data.
 *   
 * @param Name
 *   Name used for search.
 *   
 * @param ItemIndex
 *   Address to hold the index of the found item.
 *   
 * @return
 *   TRUE if the named item was found, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAuxDataGetItemIndex(
 *   &                   AuxDataRefPtr,
 *   &                   Name,
 *   &                   ItemIndex)
 *    POINTER         (AuxDataRefPtr, AuxDataRef)
 *    CHARACTER*(*)   Name
 *    INTEGER*4       ItemIndex
 * </FortranSyntax>
 *   
 * If it exists, get the item index of the frame's auxiliary data item named
 * "MachNumber":
 *
 * @code
 *   // If it exists, get the item index of the frame's
 *   // auxiliary data item named "MachNumber".
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       LgIndex_t ItemIndex;
 *       if (TecUtilAuxDataGetItemIndex(AuxDataRef,
 *                                      "MachNumber",
 *                                      &ItemIndex))
 *         {
 *           // ... do something with the item index ...
 *         }
 *       else
 *         {
 *           // ... item not found ...
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAuxDataGetItemIndex(AuxData_pa  AuxDataRef,
                                                         const char *Name,
                                                         LgIndex_t  *ItemIndex);

/**
 * Gets the auxiliary data item at the specified index. The resulting name and
 * value are allocated copies and therefore it is the client's responsibility
 * to release them when no longer needed.
 *
 * @sa TecUtilStringDealloc()
 *   
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *   
 * @param Index
 *   Index of the auxiliary data of interest.
 *   
 * @param Name
 *   Address to hold the auxiliary data item name.
 *   
 * @param Value
 *   Address to hold the auxiliary data item value.
 *   
 * @param Type
 *   Address to hold the auxiliary data item type.
 *   
 * @param Retain
 *   Address to hold the auxiliary data item retain flag.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataGetItemByIndex(
 *   &           AuxDataRefPtr,
 *   &           Index,
 *   &           Name,
 *   &           NameLength,
 *   &           ValuePtr,
 *   &           Type,
 *   &           Retain)
 *    POINTER         (AuxDataRefPtr, AuxDataRef)
 *    INTEGER*4       Index
 *    CHARACTER*(*)   Name
 *    INTEGER*4       NameLength
 *    POINTER         (ValuePtr, Value)
 *    INTEGER*4       Type
 *    INTEGER*4       Retain
 * </FortranSyntax>
 *   
 * Get the frame's fourth auxiliary data item:
 *   
 * @code
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       char          *Name;
 *       ArbParam_t     Value;
 *       AuxDataType_e  Type;
 *       Boolean_t      Retain;
 *       TecUtilAuxDataGetItemByIndex(AuxDataRef, 4,
 *                                    &Name,
 *                                    &Value,
 *                                    &Type,
 *                                    &Retain);
 *       if (Type == AuxDataType_String)
 *         // currently the only type supported
 *         {
 *           char *ValueString = (char *)Value;
 *           if (ValueString != NULL)
 *             {
 *              // ... do something with the value string ...
 *              // release the allocated string copy
 *               TecUtilStringDealloc(&ValueString);
 *             }
 *           else
 *             {
 *               // ... handle the NULL condition ...
 *             }
 *         }
 *       else
 *         {
 *           // value type not yet supported by this addon
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilAuxDataGetItemByIndex(AuxData_pa      AuxDataRef,
                                                      LgIndex_t       Index,
                                                      char          **Name,
                                                      ArbParam_t     *Value,
                                                      AuxDataType_e  *Type,
                                                      Boolean_t      *Retain);

/**
 * Gets the auxiliary data item by the specified name if it exists.
 *
 * @par Note:
 *   The resulting value is an allocated copy and therefore it is the caller's
 *   responsibility to release using TecUtilStringDealloc() when no longer
 *   needed.
 *   
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *   
 * @param Name
 *   Address to hold the auxiliary data item name.
 *   
 * @param Value
 *   Address to hold the auxiliary data item value.
 *   
 * @param Type
 *   Address to hold the auxiliary data item type.
 *   
 * @param Retain
 *   Address to hold the auxiliary data item retain flag. If Retain is set to
 *   TRUE, then saving the data set will include this item.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAuxDataGetItemByName(
 *   &                   AuxDataRefPtr,
 *   &                   Name,
 *   &                   ValuePtr,
 *   &                   Type,
 *   &                   Retain)
 *    POINTER         (AuxDataRefPtr, AuxDataRef)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 *    INTEGER*4       Type
 *    INTEGER*4       Retain
 * </FortranSyntax>
 *   
 * If it exists, get the frame's auxiliary data item named "MachNumber":
 *   
 * @code
 *   // If it exists, get the frame's auxiliary
 *   // data item named "MachNumber".
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       ArbParam_t    Value;
 *       AuxDataType_e Type;
 *       Boolean_t     Retain;
 *       if (TecUtilAuxDataGetItemByName(AuxDataRef, "MachNumber",
 *                                       &Value, &Type, &Retain))
 *         {
 *           if (Type == AuxDataType_String) // currently the only type supported
 *             {
 *               char *ValueString = (char *)Value;
 *               if (ValueString != NULL)
 *                 {
 *                   // ... do something with the value string ...
 *                   double MachNumber;
 *                   if (sscanf(ValueString, "%lf", &MachNumber) == 1)
 *                     {
 *                     }
 *                   else
 *                     {
 *                       // ... invalid value ...
 *                     }
 *   
 *                   // release the allocated string copy
 *                   TecUtilStringDealloc(&ValueString);
 *                 }
 *               else
 *                 {
 *                   // ... handle the NULL condition ...
 *                 }
 *             }
 *           else
 *             {
 *               // ... value type not yet supported by this addon ...
 *             }
 *         }
 *       else
 *         {
 *           // ... item not found ...
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAuxDataGetItemByName(AuxData_pa     AuxDataRef,
                                                          const char    *Name,
                                                          ArbParam_t    *Value,
                                                          AuxDataType_e *Type,
                                                          Boolean_t     *Retain);

/**
 * Adds the auxiliary data item to the auxiliary data or replaces it if one
 * already exists by the same name.
 *   
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *   
 * @param Name
 *   Auxiliary data item's name. Be sure to consider a unique naming convention
 *   for your auxiliary data to avoid naming conflicts with auxiliary data
 *   produed by other addonaddons or macros. By convention addons that wish to
 *   share auxiliary data can prepend the name with the "Common" prefix. For
 *   example, the Plot3D loader uses CommonReferenceMachNumber for the
 *   reference Mach number.
 *   
 * @param Value
 *   Value of item to set.
 *   
 * @param Type
 *   Type of item being set.
 *   
 * @param Retain
 *   Flag specifying whether or not to retain this item on file export such as
 *   writing out a datafile, etc.Add an item named "MachNumber" to the frame's
 *   auxiliary data:AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   
 * @return
 *   TRUE if the item was added to the auxiliay data, FALSE if otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAuxDataSetItem(
 *   &                   AuxDataRefPtr,
 *   &                   Name,
 *   &                   ValuePtr,
 *   &                   Type,
 *   &                   Retain)
 *    POINTER         (AuxDataRefPtr, AuxDataRef)
 *    CHARACTER*(*)   Name
 *    POINTER         (ValuePtr, Value)
 *    INTEGER*4       Type
 *    INTEGER*4       Retain
 * </FortranSyntax>
 *   
 * Add an item named "MachNumber" to the frame's auxiliary data.
 *   
 * @code
 *   AuxData_pa AuxDataRef = TecUtilAuxDataFrameGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       ArbParam_t Value = (ArbParam_t)"1.75";
 *       if (TecUtilAuxDataSetItem(AuxDataRef,
 *                                 "MachNumber",
 *                                 Value,
 *                                 AuxDataType_String,
 *                                 TRUE)) // Retain
 *         {
 *           // ... item was added ...
 *         }
 *       else
 *         {
 *           // ... item failed to be added ...
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAuxDataSetItem(AuxData_pa     AuxDataRef,
                                                    const char    *Name,
                                                    ArbParam_t     Value,
                                                    AuxDataType_e  Type,
                                                    Boolean_t      Retain);

/**
 * Deletes the auxiliary data item.
 *   
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *   
 * @param Index
 *   Index of the auxiliary data item of interest.
 *   
 * @return
 *   Reference to the current frame's auxiliary data..
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAuxDataDeleteItemByIndex(
 *   &           AuxDataRefPtr,
 *   &           Index)
 *    POINTER         (AuxDataRefPtr, AuxDataRef)
 *    INTEGER*4       Index
 * </FortranSyntax>
 *   
 * Delete the dataset's fourth auxiliary data item:
 *   
 * @code
 *   AuxData_pa AuxDataRef = TecUtilAuxDataDataSetGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       TecUtilAuxDataDeleteItemByIndex(AuxDataRef, 4);
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilAuxDataDeleteItemByIndex(AuxData_pa AuxDataRef,
                                                         LgIndex_t  Index);

/**
 * Deletes the auxiliary data item by the specified name if it exists.
 *   
 * @param AuxDataRef
 *   Reference to the auxiliary data.
 *   
 * @param Name
 *   Name used for the search.
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAuxDataDeleteItemByName(
 *   &                   AuxDataRefPtr,
 *   &                   Name)
 *    POINTER         (AuxDataRefPtr, AuxDataRef)
 *    CHARACTER*(*)   Name
 * </FortranSyntax>
 *   
 *   If it exists, delete the dataset's auxiliary data item named "MachNumber":
 *   
 * @code
 *   AuxData_pa AuxDataRef = TecUtilAuxDataDataSetGetRef();
 *   if (AuxDataRef != NULL)
 *     {
 *       if (TecUtilAuxDataDeleteItemByName(AuxDataRef, "MachNumber"))
 *         {
 *           // ... item found and deleted ...
 *         }
 *       else
 *         {
 *           // ... item not found ...
 *         }
 *     }
 *   else
 *     {
 *       // ... allocation failure ...
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAuxDataDeleteItemByName(AuxData_pa  AuxDataRef,
                                                             const char *Name);



/**
 * Query to see if variable and connectivity sharing is permitted for this
 * dataset. You must still call TecUtilDataValueIsSharingOk() for variables
 * and TecUtilDataConnectIsSharingOk() for connectivity to determine if a
 * particular variable or if the connectivity may be shared.
 *   
 * @return
 *   Returns TRUE if sharing is allowed in Tecplot, FALSE if otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetIsSharingAllowed()
 * </FortranSyntax>
 *
 * @code
 *   Boolean_t IsSharing;
 *   IsSharing = TecUtilDataSetIsSharingAllowed();.
 * @endcode
 *
 * @sa TecUtilDataValueIsSharingOk() and TecUtilDataConnectIsSharingOk().
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetIsSharingAllowed(void);



/**
 * Determine if it is ok to share a variable between zones.  A variable can be
 * shared if the dimensions, type and value location are the same in the source
 * and destination zones and if the zones are of the same type.
 *
 * @param SourceZone
 *   The source zone (the zone where the values will be stored).
 * @param DestZone
 *   The destination zone (the zone acquiring the shared values).
 * @param Var
 *   The variable to be shared.
 *
 * @return
 *   Returns TRUE if the specified variable sharing is allowed, FALSE if
 *   otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueIsSharingOk(
 *   &                   SourceZone,
 *   &                   DestZone,
 *   &                   Var)
 *    INTEGER*4       SourceZone
 *    INTEGER*4       DestZone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *
 * If sharing is ok for variable 7 between zones 3 and 5 then share the variable.
 * @code
 *   if (TecUtilDataValueIsSharingOk(3,5,7))
 *     TecUtilDataValueShare(3,5,7);
 * @endcode
 *
 * @sa TecUtilDataSetIsSharingAllowed() and TecUtilDataConnectIsSharingOk().
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueIsSharingOk(EntIndex_t SourceZone,
                                                          EntIndex_t DestZone,
                                                          EntIndex_t Var);
/**
 * Determine if it is ok to share the connectivity between zones.
 *
 * @param SourceZone
 *   The source zone (the zone where the values will be stored).
 * @param DestZone
 *   The destination zone (the zone acquiring the shared values).
 *
 * @return
 *   Returns TRUE if the specified connectivity sharing is allowed, FALSE if
 *   otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataConnectIsSharingOk(
 *   &                   SourceZone,
 *   &                   DestZone)
 *    INTEGER*4       SourceZone
 *    INTEGER*4       DestZone
 * </FortranSyntax>
 *
 * If sharing is ok for connectivity between zones 3 and 5 then share the
 * connectivity.
 * @code
 *   if (TecUtilDataConnectIsSharingOk(3,5))
 *     TecUtilDataConnectShare(3,5);
 * @endcode
 *
 * @sa TecUtilDataSetIsSharingAllowed() and TecUtilDataValueIsSharingOk().
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataConnectIsSharingOk(EntIndex_t SourceZone,
                                                            EntIndex_t DestZone);

/**
 * Gets the set of zones that share the variable with the specified zone. If
 * the specified zone's variable is shared then it is also a member of the
 * resulting set otherwise an empty set is returned.
 *
 * @param Zone
 *   Zone for which sharing information is desired.
 * @param Var
 *   Variable for which sharing information is desired.
 *
 * @return
 *   Allocated set of zones that share the variable with the specified zone.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueGetShareZoneSet(
 *   &           Zone,
 *   &           Var,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 */
LINKTOADDON Set_pa STDCALL TecUtilDataValueGetShareZoneSet(EntIndex_t Zone,
                                                           EntIndex_t Var);

/**
 * Gets the set of zones that share the connectivity with the specified zone.
 * If the specified zone's connectivity is shared then it is also a member of
 * the resulting set otherwise an empty set is returned.
 *
 * @param Zone
 *   Zone for which sharing information is desired.
 *
 * @return
 *   Allocated set of zones that share the connectivity with the specified
 *   zone.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilConnectGetShareZoneSet(
 *   &           Zone,
 *   &           ResultPtr)
 *    INTEGER*4       Zone
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 */
LINKTOADDON Set_pa STDCALL TecUtilConnectGetShareZoneSet(EntIndex_t Zone);

/**
 * Get a count for the number of times a particular variable is shared.
 *   
 * @param Zone
 *   Zone in which the shared variable is located.
 *   
 * @param Var
 *   Variable that is shared.
 *   
 * @return
 *   Returns share count for the given variable within the given zone. This is
 *   the number of times the data handle is shared. 1 means not shared (shared
 *   once), 2 means two zones share it, etc.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueGetShareCount(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *   
 *   Get share count of variable 2 in zone 1.
 *   
 * @code
 *   EntIndex_t ShareCount = TecUtilDataValueGetShareCount(1, 2);
 * @endcode
 */
LINKTOADDON EntIndex_t STDCALL TecUtilDataValueGetShareCount(EntIndex_t Zone,
                                                             EntIndex_t Var);
/**
 * Returns the share count for connectivity for the given zone. This is the
 * number of times the connectivity is shared. 1 means not shared (shared
 * once), 2 means two zones share it, etc.
 *   
 * @param Zone
 *   Zone number where connectivity is to be branched.
 *   
 * @return
 *   Number of zones sharing connectivity.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataConnectGetShareCount(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 */
LINKTOADDON EntIndex_t STDCALL TecUtilDataConnectGetShareCount(EntIndex_t Zone);



#if defined COMPILE_BUT_HIDE_FROM_DOC
LINKTOADDON AddOnProduct_e STDCALL TecUtilAddOnProductGetName(void);
#endif

/**
 * Gets the number of contour levels and contour level values currently
 * defined for the specified contour group. The LevelValues array must be
 * deallocated by the addon using TecUtilArrayDealloc().
 *   
 * @param ContourGroup
 *   The contour group of interest and must be an integer between 1 and 4.
 *   
 * @param NumLevels
 *   Pointer to an LgIndex_t variable that will receive the current number of
 *   levels for the specified contour group.
 *   
 * @param LevelValues
 *   Pointer to a double pointer variable that will receive the allocated
 *   double array of level values or NULL if there are none
 *   
 * @return
 *   FALSE if an allocation error occured otherwise TRUE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilContourGetLevels(
 *   &                   ContourGroup,
 *   &                   NumLevels,
 *   &                   LevelValues)
 *    INTEGER*4       ContourGroup
 *    INTEGER*4       NumLevels
 *    REAL*8(*)       LevelValues
 * </FortranSyntax>
 *   
 *   Fetch the number of contour levels for contour group 2:
 *   
 * @code
 *   Boolean_t IsOk;
 *   LgIndex_t NumLevels;
 *   double    *LevelValues;
 * 
 *   TecUtilLockStart(AddOnID);
 *   IsOk = TecUtilContourGetLevels(2, &NumLevels, &LevelValues);
 *   if (IsOk)
 *     {
 *       if (NumLevels != 0)
 *         {
 *           LgIndex_t LIndex;
 *           printf("There are %d levels for contour group #2:\n", 
 *                  NumLevels);   
 *           for (LIndex = 0; LIndex < NumLevels; LIndex++)
 *             printf("  %lg\n", LevelValues[LIndex]);
 *           TecUtilArrayDealloc((void **)&LevelValues);
 *         }
 *       else
 *         printf("No levels are specified for contour group #2\n");
 *     }
 *
     TecUtilLockFinish(AddOnID);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilContourGetLevels(SmInteger_t   ContourGroup,
                                                      LgIndex_t    *NumLevels,
                                                      double      **LevelValues);
