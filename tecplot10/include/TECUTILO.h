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
#if defined TECUTILOMODULE
#define EXTERN
#else
#define EXTERN extern
#endif

/*{{<windows_only> TecUtilInterfaceWinAddPreMsgFn PreTranslateMessage_pf </windows_only> }}*/

#ifdef MSWIN 
/* Add new Windows-only functions here */
/**
 * Add a PreTranslateMessage function to Tecplot's message loop so that
 * modeless dialogs messages can be dispatched correctly. See the ADK User's
 * Manual for information on using TecUtilInterfaceWinAddPreMsgFn(). This
 * function is only available under Windows.
 *   
 * @param PreTranslateMessageProc
 *   Function to call to pretranslate messages. The PreTranslateMessage
 *   function you provide will have the following syntax: Boolean_t
 *   PreTranslateMessage(MSG *pMsg)Where pMsg is the message to preprocess
 *   
 * @return
 *   TRUE if successfully installed, FALSE otherwise.
 *   
 *   Install a PreTranslateMessage function for modeless dialogs for an add-on
 *   written in MFC:
 *   
 * @code
 *   Boolean_t PreTranslateMessage(MSG *pMsg)
 *   {
 *     AFX_MANAGE_STATE(AfxGetStaticModuleState());
 *     return AfxGetApp()->PreTranslateMessage(pMsg);
 *   }
 *   .
 *   .
 *   .
 *   
 *     // In InitTecAddon Function 
 *     TecUtilInterfaceWinAddPreMsgFn(PreTranslateMessage);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilInterfaceWinAddPreMsgFn(PreTranslateMessage_pf PreTranslateMessageProc);
#endif




/*{{<exclude_fglue> 
                    TecUtilTimerAddCallback AddOnTimerCallback_pf 
                    TecUtilDataSetAddVar
                    TecUtilInterfaceGetMotifHandles
                    TecUtilStringAlloc 
                    TecUtilStringListGetRawStringPtr
                    TecUtilDataSetAddZone
                    TecUtilStateChangeAddCallback 
                    TecUtilStateChangeSetMode 
                    StateChangeAddOnCallback_pf
                    StateChangeAddOnCallbackV2_pf

  </exclude_fglue> }}*/

/**
 * Adds a timer callback, a function which is called by Tecplot at a set
 * interval of time.
 *   
 * @par Note:
 *   In your timer callback you must check to see if Tecplot is locked before
 *   calling any TecUtil functions, as illustrated in the source code example.
 *
 * @param Interval
 *   Timeout interval in milliseconds.
 *   
 * @param ClientData
 *   This can be any 32-bit value and will be passed to the timer callback.
 *   Typically this is a pointer to a structure
 *   
 * @param TimerCallback
 *   Function to be called at each timeout interval. This function should
 *   return TRUE to continue the timer, or FALSE to stop it.
 *   
 * @return
 *   TRUE if the callback was successfully added, FALSE otherwise. This
 *   function returns FALSE only if Tecplot is unable to obtain a timer from
 *   the operating system.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTimerAddCallback(
 *   &                   Interval,
 *   &                   ClientDataPtr,
 *   &                   TimerCallback)
 *    INTEGER*4          Interval
 *    POINTER            (ClientDataPtr, ClientData)
 *    EXTERNAL           Callback
 * </FortranSyntax>
 *   
 *   Set up a timer with an interval of one second.
 *   
 * @code
 *   static Boolean_t MyTimerCallback(ArbParam_t MyData)
 *   {
 *     if (!TecUtilLockIsOn())
 *       {
 *         // Tecplot isn't currently locked, so it's safe to call TecUtil functions 
 *         TecUtilLockStart(AddOnID); // Lock Tecplot for ourselves
 *   
 *         // ... Do processing with TecUtil functions
 *   
 *         // Release Tecplot for other addons
 *         TecUtilLockFinish(AddOnID); 
 *       }
 *     else
 *       {
 *         // Another addon has locked Tecplot. It is NOT safe to
 *         // call any TecUtil functions, so do nothing.
 *       }
 *   
 *     // Return TRUE to continue the timer, return FALSE to stop the timer.
 *     return TRUE; 
 *   }
 *   // Make the above function called every second.
 *   TecUtilTimerAddCallback(1000,NULL,MyTimerCallback);
 * @endcode
 *
 * @par Note:
 *   For a complete example of an add-on which uses timers, see the timetest
 *   sample add-on.
 *
 * @sa TecUtilOnIdleQueueAddCallback()
 */
LINKTOADDON Boolean_t STDCALL   TecUtilTimerAddCallback(UInt32_t               Interval,
                                                        ArbParam_t             ClientData,
                                                        AddOnTimerCallback_pf  TimerCallback);

/**
 * Adds the function to a list of functions that Tecplot calls only one time
 * when Tecplot is in an idle state. This is particularly important for any
 * addon that needs to perform potentially disruptive Tecplot operations (such
 * as modifying data) in response to a particular state change.  It is
 * important for an addon to set a local flag when it registers the callback
 * (and check that the flag is not set before registering) and clears the flag
 * after the registered callback is called by Tecplot so that only one on-idle
 * callback is registered to handle the pending operations.
 *   
 * @param Callback
 *   Function to be called only one time when Tecplot becomes idle (and is not
 *   locked). After the call the function is removed from the queue. No attempt
 *   is made to ensure only one instance of Callback is registered so it is up
 *   to the addon to keep track of registration (see the use of the
 *   PendingMyUpdateFuncOnIdle variable in the example illustrated below).
 *   
 * @param ClientData
 *   This can be any 32-bit value and will be passed to the on-idle callback
 *   when the on idle callback is invoked. Typically this is a pointer to a
 *   structure needed by the callback function
 *   
 * @return
 *   TRUE if the callback was successfully added, FALSE otherwise.
 *   
 * <FortranSyntax>
 * C
 * C  Note: Only one on-idle callback function can active for each FORTRAN add-on.
 * C  Registering another on-idle callback function overwrites the first.
 * C  You may use ClientData to differentiate between different on-idle situations.
 * C
 *    INTEGER*4 FUNCTION TecUtilOnIdleQueueAddCallback(
 *   &                   Callback,
 *   &                   ClientDataPtr)
 *    EXTERNAL           Callback
 *    POINTER            (ClientDataPtr, ClientData)
 * </FortranSyntax>
 *   
 * Clear the current layout. 
 *   
 * @code
 *   
 *   // Make sure only one on-idle function is registered at a time.
 *   static Boolean_t PendingMyUpdateFuncOnIdle = FALSE;
 *   
 *   static void MyUpdateFuncOnIdle(ArbParam_t ClientData)
 *   {
 *     // ... do some potentially disruptive work in Tecplot 
 *   
 *     // clear the flag so we can register another update if needed
 *     PendingMyUpdateFuncOnIdle = FALSE;
 *   }
 *   
 *   // A state change monitor usually used to update dialogs and
 *   // other addon state when Tecplot (or another addon) changes
 *   // the Tecplot's current state
 *   
 *   void MyStateChangeMonitor(StateChange_e StateChange,
 *                             ArbParam_t    CallData)
 *   {
 *     if (StateChange == StateChange_VarsAltered)
 *       {
 *         // Addon needs to do something that is disruptive so we
 *         // need to register an on-idle callback to do the work
 *         // when Tecplot is idle.
 *   
 *         // if one is not already registered ...
 *         if (!PendingMyUpdateFuncOnIdle)
 *           {
 *             // keep track of our registration 
 *             PendingMyUpdateFuncOnIdle = TRUE;
 *             // Queue the callback
 *             TecUtilOnIdleQueueAddCallback(MyUpdateFuncOnIdle,
 *                                           (ArbParam_t)MyData);
 *           }
 *       }
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilOnIdleQueueAddCallback(OnIdleCallback_pf Callback,
                                                            ArbParam_t        ClientData);

/**
 * Launch a dialog to prompt the user for a folder name.
 *
 * @param FolderName
 *    Returns a string containing the name of the folder selected.  You must
 *    initialize this parameter to NULL.  If TecUtilDialogGetFolderName()
 *    returns TRUE, you must call TecUtilStringDealloc() after using this
 *    parameter.  If the return value is FALSE, FolderName is not changed and
 *    should not be deallocated.
 *
 * @param Title
 *    Null-terminated string that is displayed above the tree view control in
 *    the dialog box. This string can be used to specify instructions to the
 *    user.
 *
 * @return
 *    TRUE if a folder name was successfully entered. FALSE if there was an
 *    error or if the user cancelled.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetFolderName(
 *   &                   Title,
 *   &                   FolderName)
 *    CHARACTER*(*)   Title
 *    CHARACTER*(*)   FileName
 *
 * </FortranSyntax>
 *
 * Prompt the user for a single folder to read:
 *   
 * @code
 *   char *FolderName = NULL; // must initialize to NULL
 *   
 *   if (TecUtilDialogGetFolderName("Please select a folder",
                                     &FolderName)
 *      {
 *           .
 *           .
 *           .
 *           do something with FolderName
 *           .
 *           .
 *           .
 *        // free Tecplot's copy
 *        TecUtilStringDealloc(&FolderName);
 *      }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetFolderName(const char *Title,
                                                         char **FolderName);

/**
 * Launch a dialog to prompt the user for a file name.
 *
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *   
 * @param DialogOption
 *   Choose the mode of operation for the dialog. The possible values
 *   are: \ref SelectFileOption_ReadSingleFile (allows you to read a file).
 *   \ref SelectFileOption_WriteFile (allows you to bring up single file
 *   selection dialog to choose a file to write to).
 *   
 * @param FileName
 *   Returns a string containing the name of the file selected. If
 *   TecUtilDialogGetFileName() returns TRUE, you must call
 *   TecUtilStringDealloc() after using this parameter. If the return value is
 *   FALSE, FileName is not changed and should not be deallocated.
 *   
 * @param FileTypeName
 *   A string describing the file type. Example: "Text file." Must not be NULL.
 *   
 * @param DefaultFileName
 *   The initial file name. May be NULL
 *   
 * @param DefaultFilter
 *   The default filter (that is, extension). Example: "*.txt." May be NULL
 *   
 * @return
 *   TRUE if a file name was successfully entered, FALSE otherwise. FALSE
 *   usually indicates that Cancel on the dialog was clicked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetFileName(
 *   &                   DialogOption,
 *   &                   FileName,
 *   &                   FileNameLength,
 *   &                   FileTypeName,
 *   &                   DefaultFileName,
 *   &                   DefaultFilter)
 *    INTEGER*4       DialogOption
 *    CHARACTER*(*)   FileName
 *    INTEGER*4       FileNameLength
 *    CHARACTER*(*)   FileTypeName
 *    CHARACTER*(*)   DefaultFileName
 *    CHARACTER*(*)   DefaultFilter
 * </FortranSyntax>
 *   
 * Prompt the user for a single file to read:
 *   
 * @code
 *   char *FileName = NULL; // should initialize to NULL
 *   
 *   if (TecUtilDialogGetFileName(SelectFileOption_ReadSingleFile,
 *    &FileName, "Text Files", "myfile.txt", "*.txt"))
 *      {
 *           .
 *           .
 *           .
 *           do something with FileName
 *           .
 *           .
 *           .
 *        // free Tecplot's copy
 *        TecUtilStringDealloc(&FileName);
 *      }
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetFileName(SelectFileOption_e   DialogOption,
                                                       char               **FileName,
                                                       const char          *FileTypeName,
                                                       const char          *DefaultFileName,
                                                       const char          *DefaultFilter);
/**
 * Launch a dialog to prompt the user for one or more file names. It is assumed
 * that the files selected will be opened only for reading. Use
 * TecUtilGetFileName() to open a file for writing.
 *   
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *   
 * @param DialogOption
 *   Choose the mode of operation for the dialog. The possible values are:
 *   \ref SelectFileOption_ReadMultiFile (brings up the multi-file section
 *   dialog), \ref SelectFileOption_AllowMultiFileRead (brings up single file
 *   section to start with but includes a button the user can press to get a
 *   multi-file selection dialog)
 *   
 * @param FileNames
 *   Returns a string containing the name of the file selected. If
 *   TecUtilDialogGetFileNames() returns TRUE, you must call
 *   TecUtilStringListDealloc() after using this parameter. If the return value
 *   is FALSE, FileNames is not changed and should not be deallocated.
 *   
 * @param FileTypeName
 *   A string describing the file type. Example: "Text file." Must not be NULL.
 *   
 * @param DefaultFileNames
 *   A string list containing the default file name(s). May be NULL.
 *   
 * @param DefaultFilter
 *   The default filter (that is, extension). Example: "*.txt." May be NULL
 *   
 * @return
 *   TRUE if a file name was successfully entered, FALSE otherwise. FALSE
 *   usually indicates that Cancel on the dialog was clicked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogGetFileNames(
 *   &                   DialogOption,
 *   &                   FileNamesPtr,
 *   &                   FileTypeName,
 *   &                   DefaultFileNamesPtr,
 *   &                   DefaultFilter)
 *    INTEGER*4       DialogOption
 *    POINTER         (FileNamesPtr, FileNames)
 *    CHARACTER*(*)   FileTypeName
 *    POINTER         (DefaultFileNamesPtr, DefaultFileNames)
 *    CHARACTER*(*)   DefaultFilter
 * </FortranSyntax>
 *   
 *   Prompt the user for one or more files to read:
 *   
 * @code
 *   StringList_pa FileNames; = NULL;
 *   StringList_pa DefaultName = TecUtilStringListAlloc();
 *   
 *   TecUtilStringListAppendString(DefaultName,"myfile.txt");
 *   
 *   if (TecUtilDialogGetFileNames(SelectFileOption_ReadMultiFile,
 *                                 &FileNames,
 *                                 "Text Files",
 *                                 DefaultName,
 *                                 "*.txt"))
 *     {
 *       // get the first file name
 *       char *f = TecUtilStringListGetString(FileNames,1);
 *       .
 *       .
 *       .
 *       do something with FileNames 
 *       .
 *       .
 *       .
 *       TecUtilStringDealloc(&f); // and free Tecplot's copy
 *    
 *       TecUtilStringListDealloc(FileNames);// done with str list
 *     }
 *   
 *   TecUtilStringListDealloc(&DefaultName); // done with str list
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogGetFileNames(SelectFileOption_e   DialogOption,
                                                        StringList_pa       *FileNames,
                                                        const char          *FileTypeName,
                                                        StringList_pa       DefaultFileNames,
                                                        const char          *DefaultFilter);


/**
 * Download a file given a valid URL.
 *   
 * @param SourceURL
 *   A string representing the URL for the file to download. Must be valid URL syntax
 *   
 * @param LocalDestinationFile
 *   A string representing a local filename where the data is to be stored
 *   
 * @param IsAscii
 *   Set to TRUE if the file is to be treated as an ASCII file during the
 *   download. Set to FALSE if it is to be treated as a Binary file during the
 *   download
 *   
 * @param ConfirmOverwrite
 *   Set to TRUE if you want the user to be prompted with a message and options
 *   if LocalDestinationFile exists. The user can then choose to cancel the
 *   operation. If set to FALSE and LocalDestinationFile exists it will be
 *   overwritten with no warning
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFileDownloadURL(
 *   &                   SourceURL,
 *   &                   LocalDestinationFile,
 *   &                   IsAscii,
 *   &                   ConfirmOverwrite)
 *    CHARACTER*(*)   SourceURL
 *    CHARACTER*(*)   LocalDestinationFile
 *    INTEGER*4       IsAscii
 *    INTEGER*4       ConfirmOverwrite
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilFileDownloadURL(const char  *SourceURL,
                                                     const char  *LocalDestinationFile,
                                                     Boolean_t    IsAscii,
                                                     Boolean_t    ConfirmOverwrite);
/**
 *   Upload a file given a valid URL.
 *   
 * @param LocalSourceFile
 *   A string representing the local file to upload
 *   
 * @param DestinationURL
 *   A string representing the remote filename where the data is to be stored. Must be valid URL
 *   syntax,
 *   
 * @param IsAscii
 *   Set to TRUE if the file is to be treated as an ASCII file during the upload. Set to FALSE if it is
 *   to be treated as a Binary file during the upload
 *   
 * @param ConfirmOverwrite
 *   Set to TRUE if you want the user to be prompted with a message and options if exists. The user can
 *   then choose to cancel the operation. If set to FALSE and DestinationURL exists it will be
 *   overwritten with no warning
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFileUploadURL(
 *   &                   LocalSourceFile,
 *   &                   DestinationURL,
 *   &                   IsAscii,
 *   &                   ConfirmOverwrite)
 *    CHARACTER*(*)   LocalSourceFile
 *    CHARACTER*(*)   DestinationURL
 *    INTEGER*4       IsAscii
 *    INTEGER*4       ConfirmOverwrite
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilFileUploadURL(const char *LocalSourceFile,
                                                   const char *DestinationURL,
                                                   Boolean_t   IsAscii,
                                                   Boolean_t   ConfirmOverwrite);
/**
 * Convenience function that will determine if a supplied string uses valid URL
 * syntax.
 *   
 * @param *URLFName
 *   A string containing the URL to test
 *   
 * @return
 *   Returns TRUE if URL is valid, FALSE otherwise
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilFileIsURL(URLFName)
 *    CHARACTER*(*) URLFName
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilFileIsURL(const char *URLFName);

/**
 * Deallocates an array that was allocated and given to an addon by Tecplot and
 * assigns the pointer to NULL. This function may not be used for deallocating
 * memory allocated by any other facility (such as the addon itself).
 *   
 * @param Array
 *   Pointer to the Array to be deallocated.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilArrayDealloc(Array)
 *    POINTER (ArrayPtr, Array)
 * </FortranSyntax>
 *   
 * @sa See TecGUIListGetSelectedItems() for a complete example.
 */
LINKTOADDON void STDCALL TecUtilArrayDealloc(void **Array);

/**
 * Get Tecplot's current mouse mode.
 *   
 * @return
 *   Returns the current mode.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMouseGetCurrentMode()
 * </FortranSyntax>
 */
LINKTOADDON MouseButtonMode_e STDCALL TecUtilMouseGetCurrentMode(void);

/**
 * This function will tell you if the specified mouse mode is valid for the
 * current state of Tecplot.
 *   
 * @param MouseMode
 *   Mouse mode to check.
 *   
 * @return
 *   Returns TRUE if the specified mode is appropriate, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMouseIsValidMode(MouseMode)
 *    INTEGER*4 MouseMode
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilMouseIsValidMode(MouseButtonMode_e MouseMode);

/**
 * Sets Tecplot's mouse mode to the one specified. This service request behaves
 * exactly as if the user had selected the mouse mode via Tecplot's user
 * interface.
 * 
 * @param MouseMode
 *   Desired mouse mode.
 *
 * @return
 *    TRUE if the requested mouse mode was set, otherwise FALSE.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMouseSetMode(MouseMode)
 *    INTEGER*4 MouseMode
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilMouseSetMode(MouseButtonMode_e MouseMode);




/**
 * @deprecated 
 *   Please use TecUtilMouseSetMode() and TecUtilMouseIsValidMode() instead.
 */
LINKTOADDON void STDCALL TecUtilSidebarAutoSensitivity(Boolean_t DoAuto);



/**
 * @deprecated 
 *   Please use TecUtilMouseSetMode() and TecUtilMouseIsValidMode() instead.
 */
LINKTOADDON void STDCALL TecUtilSidebarSetSensitivity(MouseButtonMode_e MouseMode,
                                                      Boolean_t         IsSensitive);




/**
 * If the current frame mode is XY, 2D, or 3D, change the mouse mode to be the
 * probe tool and instruct Tecplot to call a different function when the user
 * completes a probe-like operation in the work area. This function callback
 * will remain in effect until the mouse mode is changed in the Tecplot
 * interface.
 *   
 * @param ProbeDestination
 *   Function to call when the probe event takes place.
 *   
 * @param InformationLineText
 *   Text to write on the status line when the override is in effect.
 *   
 * @return
 *   TRUE if successfully installed.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeInstallCallback(
 *   &                   ProbeDestination,
 *   &                   InformationLineText)
 *    EXTERNAL        ProbeDestination
 *    CHARACTER*(*)   InformationLineText
 * </FortranSyntax>
 *   
 * Override the behavior of the probe tool in Tecplot. When a probe occurs,
 * just print out the results of the probe to simulate some of the capabilities
 * of the probe dialogs in Tecplot.  The task is to provide a mechanism whereby
 * your own probe callback overrides the default behavior when a probe event
 * occurs in Tecplot. One way to do this is to add an option to the Tools menu
 * in Tecplot. The callback function assigned to the menu option would then
 * turn around and install the probe callback with the following code: .
 *   
 * @code
 *   .
 *   TecUtilProbeInstallCallback(MyProbeCallback,
 *                       "Click to print out my probe information");
 *   .
 *   TecUtilProbeAllowCOBs()
 *   .
 *   
 *   The function MyProbeCallback is then defined as:
 *   void MyProbeCallback(Boolean_t IsNearestPoint)
 *   {
 *     FrameMode_e FrameMode;
 *     TecUtilLockStart(AddOnID);
 *     FrameMode = TecUtilFrameGetMode();
 *   
 *     if (FrameMode == Frame_XY)
 *       {
 *         printf("XY Probe, Independent value is: %G\n",
 *                TecUtilProbeLinePlotGetIndValue());
 *         if (IsNearestPoint)
 *           {
 *             double DepValue;
 *             TecUtilProbeXYGetDepValue(1,&DepValue);
 *             printf("Nearest Point:  Dependent Value = %G\n",
 *                    DepValue);
 *             printf("                Source LineMap    = %d\n",
 *                    TecUtilProbeLinePlotGetMap());
 *             printf("                Point Index     = %d\n",
 *                    TecUtilProbeGetPointIndex());
 *           }
 *         else
 *           {
 *             EntIndex_t M;
 *             for (M = 1; M <= TecUtilLineMapGetCount(); M++)
 *               {
 *                 double DepValue;
 *                 printf("LineMap = %02d, Dependent Value = ",M);
 *                 if (TecUtilProbeLinePlotGetDepValue(M,&DepValue))
 *                   printf("%G\n",DepValue);
 *                 else
 *                   printf("Unknown\n");
 *               }
 *           }
 *       }
 *     else
 *       {
 *         EntIndex_t  SourceZone = TecUtilProbeFieldGetZone();
 *         ZoneType_e  ZoneType   = TecUtilZoneGetType(SourceZone);
 *         IJKPlanes_e Plane      = TecUtilProbeFieldGetPlane();
 *         EntIndex_t  V;
 *         EntIndex_t  NumVars;
 *   
 *         TecUtilDataSetGetInfo((char **)NULL,
 *                               (EntIndex_t *)NULL,
 *                               &NumVars);
 *   
 *         if (IsNearestPoint)
 *           printf("Nearest point probe:\n");
 *         else
 *          {
 *            LgIndex_t ProbeCell = TecUtilProbeFieldGetCell();
 *            printf("Interpolated  probe:(Cell = %d)\n",ProbeCell);
 *          }
 *   
 *         for (V = 1; V <= NumVars; V++)
 *           {
 *             char *VName;
 *             if (TecUtilVarGetName(V,&VName))
 *               {
 *                 printf("%20s : ",VName);
 *                 TecUtilStringDealloc(&VName);
 *               }
 *             else
 *               printf("<Unknown>            : ");
 *             printf("%G\n",TecUtilProbeFieldGetValue(V));
 *           }
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 *   A complete example can be found in the probtest sample addon included in the distribution.
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilProbeInstallCallback(ProbeDestination_pf ProbeDestination,
                                                          const char         *InformationLineText);
/**
 * Instructs Tecplot to include COBs (iso-surfaces, slices, streamtraces, and
 * so forth) along with zones during a probe when an addon has a callback
 * registered with TecUtilProbeInstallCallback() and if the user is pressing
 * the Alt key. By default the Alt key is ignored for an addon's installed
 * probe callback.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilProbeAllowCOBs()
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL TecUtilProbeAllowCOBs(void);



/**
 * @deprecated
 *   Please use TecUtilProbeLinePlotGetIndValue() instead.
 */
LINKTOADDON double STDCALL TecUtilProbeXYGetIndValue(void);



/**
 * Call this function from a probe destination callback to get the independent
 * value from the previous probe event in an Line-plot.
 *   
 * @return
 *   The independent value from the previous probe event.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilProbeLinePlotGetIndValue()
 * </FortranSyntax>
 */
LINKTOADDON double STDCALL TecUtilProbeLinePlotGetIndValue(void);


/**
 * @deprecated
 *   Please use TecUtilProbeLinePlotGetDepValue() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilProbeXYGetDepValue(EntIndex_t MapNum,
                                                        double    *DepValue);
/**
 * Call this function from a probe destination callback to get a dependent
 * value from the previous probe event in an Line-plot.
 *   
 * @param MapNum
 *   Number of the Line-map to query for its dependent value at the previous
 *   probe.
 *   
 * @param DepValue
 *   Dependent value resulting from previous probe. This parameter is ignored
 *   if the previous probe was a nearest-point probe.
 *   
 * @return
 *   TRUE if the dependent value for the selected Line-map number is valid,
 *   otherwise FALSE. A dependent value can be invalid if a probe occurs
 *   outside the range of the independent variable for a given Line-map.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeLinePlotGetDepValue(
 *   &                   MapNum,
 *   &                   DepValue)
 *    INTEGER*4       MapNum
 *    REAL*8          DepValue
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilProbeLinePlotGetDepValue(EntIndex_t MapNum,
                                                              double    *DepValue);



/**
 * @deprecated
 *   Please use TecUtilProbeLinePlotGetSourceMap() instead.
 */
LINKTOADDON EntIndex_t STDCALL TecUtilProbeXYGetSourceMap(void);



/**
 * Call this function from a probe destination callback to get the Line-map
 * whose point was selected in the previous nearest-point probe. In order to
 * use this function, the value passed to the probe destination callback must
 * have been TRUE, indicating a nearest-point probe.
 *   
 * @return
 *   The number of the Line-map.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeLinePlotGetSourceMa()
 * </FortranSyntax>
 * @par Note:
 *   IMPORTANT!  Note that the FORTRAN Name for this function is truncated
 *   to 31 characters!
 */
LINKTOADDON EntIndex_t STDCALL TecUtilProbeLinePlotGetSourceMap(void);




/**
 * Call this function from a probe destination callback to get the point index
 * from the previous nearest-point probe event in a field plot or an XY-plot.
 * In order to use this function, the value passed to the probe destination
 * callback must have been TRUE, indicating a nearest-point probe.
 *   
 * @return
 *   The index of the data point selected in the previous nearest point probe.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeGetPointIndex()
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilProbeGetPointIndex(void);
/**
 * Indicates type type of COB or zone that was selected during the probe event.
 *   
 * @return
 *   Type of COB selected by the probe. This can be one of seven values:
 *   CZType_FieldDataZone CZType_FEBoundaryCOB CZType_IsoSurfaceCOB
 *   CZType_SliceCOB CZType_StreamtraceCOB CZType_StreamtraceMarkerCOB
 *   CZType_StreamtraceArrowheadCOB.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetCZType()
 * </FortranSyntax>
 */
LINKTOADDON CZType_e STDCALL TecUtilProbeFieldGetCZType(void);
/**
 * Call this function from a probe destination callback to get a field variable
 * value from the previous probe event in a field plot.
 *   
 * @param VarNum
 *   The variable number for which to get the field value.
 *   
 * @return
 *   The value for variable VarNum at the previous probe data point.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilProbeFieldGetValue(VarNum)
 *    INTEGER*4 VarNum
 * </FortranSyntax>
 */
LINKTOADDON double STDCALL TecUtilProbeFieldGetValue(EntIndex_t VarNum);
/**
 * Call this function from a probe destination callback to get the zone number
 * from the previous probe event in a field plot.
 *   
 * @return
 *   The number of the zone which was probed.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetZone()
 * </FortranSyntax>
 */
LINKTOADDON EntIndex_t STDCALL TecUtilProbeFieldGetZone(void);
/**
 * Call this function from a probe destination callback to get the I-, J-, or
 * K-plane from the previous probe event in a field plot.
 *   
 * @return
 *   The plane which was probed. This can be one of Planes_I, Planes_J, or
 *   Planes_K.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetPlane()
 * </FortranSyntax>
 */
LINKTOADDON IJKPlanes_e STDCALL TecUtilProbeFieldGetPlane(void);

/**
 * Call this function from a probe destination callback to get the face number
 * from the previous probe event in a field plot. In order to use this
 * function, the value passed to the probe destination callback must have been
 * FALSE, indicating an interpolated probe.
 *
 * @return
 *   The face number of the cell that was probed. For ordered volume and FE
 *   brick data the value is between 1 and 6. For ordered surface, FE
 *   tetra, and FE quandrangle data the value is between 1 and 4. For FE
 *   triangular data the value is between 1 and 3. For ordered or FE linear
 *   data the value is 0.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetFaceNumber()
 * </FortranSyntax>
 */
LINKTOADDON SmInteger_t STDCALL TecUtilProbeFieldGetFaceNumber(void);

/**
 * Call this function from a probe destination callback to get the cell from
 * the previous probe event in a field plot. In order to use this function, the
 * value passed to the probe destination callback must have been FALSE,
 * indicating an interpolated probe.
 *
 * @par Note:
 *   For all zone types other than ordered volume data this function returns
 *   the same information as TecUtilProbeFieldGetCell(). For ordered volume
 *   data the distinction is that this function returns the face cell whereas
 *   TecUtilProbeFieldGetCell() returns the volume cell.
 *   
 * @return
 *   The index of the cell which was probed. For ordered data, this is
 *   equivalent to the index value of the node in the lowest indexed corner of
 *   the face cell. For finite-element data, this is the element number.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetFaceCell()
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t  STDCALL TecUtilProbeFieldGetFaceCell(void);

/**
 * Call this function from a probe destination callback to get the cell from
 * the previous probe event in a field plot. In order to use this function, the
 * value passed to the probe destination callback must have been FALSE,
 * indicating an interpolated probe.
 *
 * @par Note:
 *   For all zone types other than ordered volume data this function returns
 *   the same information as TecUtilProbeFieldGetFaceCell(). For ordered volume
 *   data the distinction is that this function returns the volume cell whereas
 *   TecUtilProbeFieldGetFaceCell() returns the face cell.
 *   
 * @return
 *   The index of the cell which was probed. For ordered data, this is
 *   equivalent to the index value of the node in the lowest indexed corner of
 *   the cell. For finite-element data, this is the element number.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilProbeFieldGetCell()
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t  STDCALL TecUtilProbeFieldGetCell(void);



/**
 * If the current frame is 2D or 3D, change the mouse mode to be the extract
 * discrete points tool and instruct Tecplot to call a different function when
 * the user completes an extract-like operation in the work area. This function
 * callback will remain in effect until the user changes mouse modes in the
 * Tecplot interface.
 *   
 * @param ExtractDestination
 *   Function to call when the extract event has been completed
 *   
 * @param InformationLineText
 *   Text to write on the information line when the override is in effect
 *   
 * @return
 *   TRUE if successfully installed.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilExtractInstallCallback(
 *   &                   ExtractDestination,
 *   &                   InformationLineText)
 *    EXTERNAL        ExtractDestination
 *    CHARACTER*(*)   InformationLineText
 * </FortranSyntax>
 *   
 *   Change the mouse mode to be the Extract Discrete Points tool and install an extract callback
 *   function.
 *   
 * @code
 *   void MyExtractFunction(LgIndex_t NumPts,
 *                                  double   *XValues,
 *                                  double   *YValues)
 *   {
 *     // do something 
 *   }
 *   .
 *   .
 *   .
 *     // elsewhere in the addon 
 *     TecUtilExtractInstallCallback(MyExtractFunction,
 *                             "Status line info for my function");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilExtractInstallCallback(ExtractDestination_pf ExtractDestination,
                                                            const char           *InformationLineText);




/**
 * Create a new data set and attach it to the current frame. This only
 * allocates space for a data set specification. You must immediately begin to
 * add zones to the data set by calling TecUtilDataSetAddZone() after creating
 * a data set.
 *   
 * @param DataSetTitle
 *   Title for the data set.
 *   
 * @param VarNames
 *   String list of variable names. See TecUtilStringListXXX functions for
 *   string list details
 *   
 * @param ResetStyle
 *   Clears out all style information for the current frame before creating the
 *   data set. It is highly recommended that you always pass TRUE for this
 *   parameter.
 *   
 * @return
 *   Returns TRUE if a data set could be allocated and attached.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetCreate(
 *   &                   DataSetTitle,
 *   &                   VarNamesPtr,
 *   &                   ResetStyle)
 *    CHARACTER*(*)   DataSetTitle
 *    POINTER         (VarNamesPtr, VarNames)
 *    INTEGER*4       ResetStyle
 * </FortranSyntax>
 *   
 *   Create a data set with two variables:
 *   
 * @code
 *   StringList_pa sl = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(sl,"V1");  // first variable 
 *   TecUtilStringListAppendString(sl,"V2");  // second variable 
 *   
 *   if ( TecUtilDataSetCreate("My Data Set",sl, TRUE));
 *      {
 *   
 *      // Immediately call TecUtilDataSetAddZone() here 
 *      }
 *   TecUtilStringListDealloc(&sl);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetCreate(const char    *DataSetTitle,
                                                   StringList_pa VarNames,
                                                   Boolean_t     ResetStyle);

#if defined COMPILE_BUT_HIDE_FROM_DOC
LINKTOADDON Boolean_t STDCALL TecUtilOEMDataSetCreate(const char    *LoaderName,
                                                      const char    *DataSetTitle,
                                                      StringList_pa VarNames,
                                                      Boolean_t     ResetStyle);
#endif






/**
 * Get the variable load mode for the current data set.
 *   
 * @return
 *   The variable load mode. Possible values are VarLoadMode_ByName and VarLoadMode_ByPosition.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetGetVarLoadMode()
 * </FortranSyntax>
 *   
 *   Get the variable load mode for the current data set:
 *   
 * @code
 *   VarLoadMode_e VarLoadMode;
 *   if ( TecUtilDataSetIsAvailable() )
 *     {
 *        VarLoadMode = TecUtilDataSetGetVarLoadMode();
 *       ...
 *     }
 * @endcode
 */
LINKTOADDON VarLoadMode_e STDCALL TecUtilDataSetGetVarLoadMode(void);

/**
 * Instruct Tecplot to either build or forgo building zone optimization
 * information. Zone optimization information enhances interactive performance
 * but has an upfront performance cost. This function can be called any time
 * after the zone has been created.
 *
 * @param Zone
 *   Zone for which the decision to build zone optimization information
 *   needs changing.
 *
 * @param BuildZoneOptInfo
 *   Indicates if Tecplot should build zone optimization if needed.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilZoneSetBuildZoneOptInfo(
 *   &           Zone,
 *   &           BuildZoneOptInfo)
 *    INTEGER*4       Zone
 *    INTEGER*4       BuildZoneOptInfo
 * </FortranSyntax>
 *
 * @sa TecUtilDataSetAddZoneX() allows you to create a zone with the
 *     appropriate setting.
 */
LINKTOADDON void STDCALL TecUtilZoneSetBuildZoneOptInfo(EntIndex_t Zone,
                                                        Boolean_t  BuildZoneOptInfo);

/**
 * Notifies Tecplot that the add-on is finished loading all the variables.
 * Tecplot will examine the dataset and perform the necessary post processing
 * to make sure that deferred unassigned variables are allocated and set to
 * zero. The use of this function is only applicable if the variable creation
 * mode was deferred. See the SV_DEFERVARCREATION option for
 * TecUtilDataSetAddZoneX() and TecUtilDataSetAddVarX().
 *
 * @since
 *   10.0-3-129
 *
 * @param IsDataSetOk
 *     Tells Tecplot if the zones or variables were sucessfully added by the
 *     add-on to the current dataset. If TRUE Tecplot will perform the
 *     necessary post processing of the dataset.
 *
 * @return
 *     TRUE if the IsDataSetOk parameter is TRUE and Tecplot was sucessful at
 *     doing the post processing of the dataset, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtiltDataSetDefVarLoadFinish(IsDataSetOk)
 *    INTEGER*4 IsDataSetOk
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtiltDataSetDefVarLoadFinish(Boolean_t IsDataSetOk);

/**
 * Add a zone to the data set attached to the current frame. This function call
 * does not load any data into the zone. In the case of finite-element zones,
 * this function also does not assign values to the connectivity list. This
 * function only allocates space for the zone. Call TecUtilStateChanged() after
 * adding a zone.
 *   
 * @param Name
 *   Name of the zone.
 *   
 * @param IMax
 *   I-Dimension of the zone if ordered. If the zone is finite-element then
 *   IMax is the number of data points.
 *   
 * @param JMax
 *   J-Dimension of the zone if ordered. If the zone is finite-element then
 *   IMax is the number of data points, JMax is the number of elements, and
 *   KMax is not used
 *   
 * @param KMax
 *   K-Dimension of the zone. If the zone is finite-element KMax is not used.
 *   
 * @param ZoneType
 *   The possible values are: ZoneType_Ordered, ZoneType_FETriangle,
 *   ZoneType_FEQuad, ZoneType_FETetra or ZoneType_FEBrick
 *   
 * @param VarDataType_Array
 *   This is an array of the data types to use for each variable. If you set
 *   this to NULL then the data types of the variables in zone 1 of the
 *   existing data set are used or FieldDataType_Float if this is the first
 *   zone. The possible values are: FieldDataType_Float, FieldDataType_Double,
 *   FieldDataType_Int32, FieldDataType_Int16, FieldDataType_Byte, or FieldDataType_Bit.
 *   
 * @return
 *   Returns TRUE if the zone was successfully added, otherwise FALSE.
 *   
 *   Add a 10 by 10 by 10 zone to the current data set:
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetAddZone(
 *   &                   ZoneName,
 *   &                   IMax,
 *   &                   JMax,
 *   &                   KMax,
 *   &                   ZoneType,
 *   &                   VarFieldDataType)
 *    CHARACTER*(*)   AddOnIDString
 *    INTEGER*4       IMax
 *    INTEGER*4       JMax
 *    INTEGER*4       KMax
 *    INTEGER*4       ZoneType
 *    INTEGER*4       VarFieldDataType(1)
 * </FortranSyntax>
 *
 * @par Note:
 *   VarFieldDataType is an INTEGER*4 array dimensioned by the number of 
 *   variables.
 *
 * @code
 *   if ( TecUtilDataSetAddZone("New Ordered Zone", 10, 10, 1,
 *                              ZoneType_Ordered, NULL) )
 *     {
 *       Set_pa zones_added = TecUtilSetAlloc(TRUE);
 *       // new zone is always last zone 
 *       EntIndex_t newzone;
 *       TecUtilDataSetGetInfo(NULL, &newzone, NULL);
 *       // fill new zone with values for all variables 
 *       .
 *       .
 *       .
 *       // inform Tecplot of new zone 
 *       TecUtilSetAddMember(zones_added, newzone, TRUE);
 *       TecUtilStateChanged(StateChange_ZonesAdded,
 *                           (ArbParam_t)zones_added);
 *       TecUtilSetDealloc(&zones_added);
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddZone(const char      *Name,
                                                    LgIndex_t        IMax,
                                                    LgIndex_t        JMax,
                                                    LgIndex_t        KMax,
                                                    ZoneType_e       ZoneType,
                                                    FieldDataType_e *VarDataType_Array);




/**
 * Add a zone to the current data set. This function was extended from
 * TecUtilDataSetAddZone() to allow addition of zones in locations where zombie
 * zones currently exist in a data set. For a simpler, less flexible interface
 * use TecUtilDataSetAddZone() instead. Be sure to call TecUtilStateChanged()
 * after adding zones.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_NAME             
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   The name of the zone to create.
 *   
 * Name:
 *   SV_ZONETYPE         
 * Type:
 *   ZoneType_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ZoneType_Ordered 
 * Required:
 *   No
 * Notes:
 *   The possible values are: ZoneType_Ordered, ZoneType_FETriangle,
 *   ZoneType_FEQuad, ZoneType_FETetra or ZoneType_FEBrick
 *   
 * Name:
 *   SV_ZONE             
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   NumZones+1       
 * Required:
 *   No
 * Notes:
 *   Number of the zone to add. If ommitted a zone number will be NumZones+1
 *   where NumZones is the previous number of zones. This value can only be set
 *   to the number of a zone that does not yet exist. You cannot use this
 *   function to replace a zone in a data set
 *   
 * Name:
 *   SV_BUILDZONEOPTINFO
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TRUE
 * Required:
 *   No
 * Notes:
 *   Set to FALSE to instruct Tecplot to forgo building zone optimization
 *   information. Zone optimization information enhances interactive
 *   performance but has an upfront performance cost. This value can be enabled
 *   or disabled at a later time by calling TecUtilZoneSetBuildZoneOptInfo().
 *   
 * Name:
 *   SV_IMAX             
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1              
 * Required:
 *   No
 * Notes:
 *   For an ordered zone SV_IMAX is the number of data points in the I
 *   dimension and for a finite-element zone it is the number of data points.
 *   
 * Name:
 *   SV_JMAX             
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1              
 * Required:
 *   No
 * Notes:
 *   For an ordered zone SV_JMAX is the number of data points in the J
 *   dimension and for a finite-element zone it is the number of elements.
 *   
 * Name:
 *   SV_KMAX             
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1              
 * Required:
 *   No
 * Notes:
 *   For an ordered zone SV_KMAX is the number of data points in the K
 *   dimension and for a finite-element zone it is not used.
 *   
 * Name:
 *   SV_VARDATATYPE      
 * Type:
 *   FieldDataType_e *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Default:
 *   NULL           
 * Required:
 *   No
 * Notes:
 *   An array of FieldDataType_e dimensioned by the number of variables
 *   currently defined in the dataset where each member specifies the type of
 *   data. If you set this to NULL then the data types of the variables in zone
 *   1 of the existing data set are used or FieldDataType_Float if this is the
 *   first zone. The possible values are: FieldDataType_Float,
 *   FieldDataType_Double, FieldDataType_Int32, FieldDataType_Int16,
 *   FieldDataType_Byte, or FieldDataType_Bit.
 *
 * Name:
 *   SV_VALUELOCATION
 * Type:
 *   ValueLocation_e *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   An array of zone numbers dimensioned by the number of variables currently
 *   defined in the dataset where each member specifies the data value
 *   location. If NULL all variables will use ValueLocation_Nodal.
 *   
 * Name:
 *   SV_VARSHAREZONELIST
 * Type:
 *   EntIndex_t *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   An array of zone numbers dimensioned by the number of variables currently
 *   defined in the dataset where each member specifies the zone used for
 *   sharing each variable. Set the zone number to 0 to specify no sharing for
 *   a specific variable. This option is mutually exclusive with
 *   SV_DEFERVARCREATION.
 *
 * Name:
 *   SV_DEFERVARCREATION
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Indicates if the creation of the variable should be deferred until the
 *   variable data is ready to be populated. Nevertheless a data loader must
 *   allocate or map the variable with TecUtilDataValueAlloc() or
 *   TecUtilDataValueMemMapData() before returning control to Tecplot. This
 *   option is mutually exclusive with SV_VARSHAREZONELIST.
 *
 * Name:
 *   SV_CONNECTSHAREZONE
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TECUTILBADZONENUMBER
 * Required:
 *   No
 * Notes:
 *   Number of the zone to use for sharing of connectivity information. If not
 *   supplied the connectivity will not be shared.
 *   
 * Name:
 *   SV_FACENEIGHBORMODE
 * Type:
 *   FaceNeighborMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FaceNeighborMode_LocalOneToOne
 * Required:
 *   No
 * Notes:
 *   Specifies the face-neighbor mode.  Options are:
 *   FaceNeighborMode_LocalOneToOne, FaceNeighborMode_LocalOneToMany,
 *   FaceNeighborMode_GlobalOneToOne, FaceNeighborMode_GlobalOneToMany.  The
 *   default is FaceNeighborMode_LocalOneToOne
 * </ArgListTable>
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetAddZoneX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   Add a 10 by 20 ordered zone as zone number 3.
 *   
 * @code
 *   Arglist_pa ArgList;
 *   TecUtilLockStart(AddOnID);
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendString(ArgList, SV_NAME, "New Zone");
 *   TecUtilArgListAppendInt(ArgList, SV_ZONE, 3);
 *   TecUtilArgListAppendInt(ArgList, SV_ZONETYPE,
 *                           (ArbParam_t)ZoneType_Ordered);
 *   TecUtilArgListAppendInt(ArgList, SV_IMAX, 10);
 *   TecUtilArgListAppendInt(ArgList, SV_JMAX, 20);
 *   TecUtilDataSetAddZoneX(ArgList);
 *   TecUtilArgListDealloc(&ArgList);
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @sa TecUtiltDataSetDefVarLoadFinish(), TecUtilDataSetAddVarX()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddZoneX(ArgList_pa ArgList);







/**
 * Add a variable to the current data set. Make sure and call
 * TecUtilStateChanged() after adding variables.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_NAME
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 * Notes:
 *   Name of newly created variable.
 *   
 * Name:
 *   SV_VARDATATYPE
 * Type:
 *   FieldDataType_e *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Array of FieldDataType_e dimensioned by the number of zones currently
 *   defined in the dataset where each member specifies type of data.
 *   
 * Name:
 *   SV_VALUELOCATION
 * Type:
 *   ValueLocation_e *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Array of ValueLocation_e dimensioned by the number of zones currently
 *   defined in the dataset where each member specifies the data value
 *   location.
 *   
 * Name:
 *   SV_TRYSHAREVARWITHALLZONES
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   A boolean property that instructs Tecplot to share the variable with all
 *   applicable zones in the dataset. This option is mutually exclusive with
 *   SV_DEFERVARCREATION.
 *
 * Name:
 *   SV_DEFERVARCREATION
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   Indicates if the creation of the variable should be deferred until the
 *   variable data is ready to be populated. Nevertheless a data loader must
 *   allocate or map the variable with TecUtilDataValueAlloc() or
 *   TecUtilDataValueMemMapData() before returning control to Tecplot. This
 *   option is mutually exclusive with SV_TRYSHAREVARWITHALLZONES.
 * </ArgListTable>
 *   
 * @return
 *   TRUE if the variable was added, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetAddVarX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 *   
 *   Add a variable called "newvar" to the dataset.
 *
 * @code
 *   ArgList_pa ArgList;
 *   FieldDataType_e *VarDataType;   // dimension by zone 
 *   ValueLocation_e *ValueLocation; // dimension by zone 
 *   
 *   TecUtilLockStart(AddOnID);
 *   
 *   ...
 *   
 *   //
 *   // Allocate and populate VarDataType and ValueLocation
 *   // with the appropriate value for each zone.
 *   
 *   ...
 *   
 *   // collect the function arguments 
 *   ArgList = TecUtilArgListAlloc();
 *   TecUtilArgListAppendString(ArgList,
 *                              SV_NAME,
 *                              "newvar");
 *   TecUtilArgListAppendArray(ArgList,
 *                             SV_VARDATATYPE,
 *                             (void *)VarDataType);
 *   TecUtilArgListAppendArray(ArgList,
 *                             SV_VALUELOCATION,
 *                             (void *)ValueLocation);
 *   TecUtilArgListAppendInt(ArgList,
 *                           SV_SHAREVARWITHALLZONES,
 *                           FALSE);
 *   
 *   // add the variable for each zone 
 *   TecUtilDataSetAddVarX(ArgList);
 *   
 *   // cleanup 
 *   TecUtilArgListDealloc(&ArgList);
 *   
 *   // Inform Tecplot that a variable was added 
 *   
 *   VarsAdded = TecUtilSetAlloc(FALSE);
 *   if (VarsAdded)
 *     {
 *       EntIndex_t NumVars;
 *       TecUtilDataSetGetInfo((char **)NULL,
 *                             (EntIndex_t *)NULL,
 *                             &NumVars);
 *       TecUtilSetAddMember(VarsAdded,NumVars,FALSE);
 *       TecUtilStateChanged(StateChange_VarsAdded,
 *                           (ArbParam_t)VarsAdded);
 *       TecUtilSetDealloc(&VarsAdded);
 *     }
 *   
 *   ...
 *   
 *   // cleanup VarDataType and ValueLocation allocations 
 *   
 *   TecUtilLockFinish(AddOnID);
 * @endcode
 *
 * @sa TecUtiltDataSetDefVarLoadFinish(), TecUtilDataSetAddZoneX()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddVarX(ArgList_pa ArgList);


/**
 *  Reallocate a zone in the data set attached to the current frame. This in
 *  effect redimensions the raw data referenced by the zone.   
 *
 *  Data in the realloced zone is preserved as much as possible.  If the zone
 *  is reduced in size all field data should be preserved where like I,J,K
 *  subscripted locations in the old zone (using the old dimensions to
 *  calculate the offset) are copied to the same I,J,K subscripted locations in
 *  the new zone (using the new dimensions).  If the dimensions of the zone are
 *  increased then the field data at subscripts beyond the original dimensions are
 *  initialized to zero.
 *
 *  If the zone is finite element and an element contains a point that is no
 *  longer available (because the zone was reduced in size) it is reset to the
 *  first point in the dataset.  If the connectivity list is expanded then all
 *  nodes in the newly created elements will reference the first point in the
 *  dataset.
 *
 *   
 * @param Zone
 *   One-based index of the zone to reallocate
 *   
 * @param NewIMaxOrNumDataPoints
 *   New IMax or number of data points
 *   
 * @param NewJMaxOrNumElements
 *   New JMax or number of elements
 *   
 * @param NewKMax
 *   New KMax
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilZoneRealloc(
 *   &                   Zone,
 *   &                   NewIMaxOrNumDataPoints,
 *   &                   NewJMaxOrNumElements,
 *   &                   NewKMax)
 *    INTEGER*4       Zone
 *    INTEGER*4       NewIMaxOrNumDataPoints
 *    INTEGER*4       NewJMaxOrNumElements
 *    INTEGER*4       NewKMax
 * </FortranSyntax>
 *   
 *   Reallocate the first zone:
 * @code
 *   TecUtilZoneRealloc(1,15,4,1);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilZoneRealloc(EntIndex_t Zone,
                                                 LgIndex_t  NewIMaxOrNumDataPoints,
                                                 LgIndex_t  NewJMaxOrNumElements,
                                                 LgIndex_t  NewKMax);


/**
 * Add a variable to the current data set. Call TecUtilStateChanged() after
 * adding a variable. This function is superceded by TecUtilDataSetAddVarX().
 *   
 * @param VarName
 *   Name of the variable being added.
 *   
 * @param FieldDataType_Array
 *   This is an array of the data types to use for each zone (i.e., it must
 *   be dimensioned by the number of zones). If you pass NULL,
 *   the data types of the variables in variable 1 of the existing data set are
 *   used. The possible choices are: FieldDataType_Float, FieldDataType_Double,
 *   FieldDataType_Int32, FieldDataType_Int16, FieldDataType_Byte, or
 *   FieldDataType_Bit.
 *   
 * @return
 *   Returns TRUE if the variable was added successfully, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetAddVar(
 *   &                   VarName,
 *   &                   VarFieldDataType)
 *    CHARACTER*(*)   AddOnIDString
 *    INTEGER*4       VarFieldDataType(1)
 * </FortranSyntax>
 *
 * @par Note:
 *   VarFieldDataType is an INTEGER*4 array dimensioned by the number of 
 *   zones.
 *
 * Add a variable to the current data set:
 *   
 * @code
 *   if ( TecUtilDataSetAddVar("New Variable", NULL) )
 *     {
 *       // New variable is always last variable. 
 *       EntIndex_t newvar;
 *       TecUtilDataSetGetInfo(NULL, NULL, &newvar);
 *       // Fill new var with values for all zones. 
 *       .
 *       .
 *       .
 *       // Inform Tecplot a variable has been added. 
 *       TecUtilStateChanged(StateChange_VarsAdded,
 *                           (ArbParam_t)NULL);
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddVar(const char *VarName,
                                                   FieldDataType_e *FieldDataType_Array);
 

/**
 * Include a function in the list of functions to call when Tecplot is
 * considering shutting down. If you are building an addon that could
 * potentially be in a state where it is not convenient to shut down, then use
 * this function. If the addon does not wish for Tecplot to quit then the
 * function referenced in this call can issue an error message to the user as
 * to why this is the case and then return FALSE. A FALSE return value back to
 * Tecplot will abort the shutdown sequence in Tecplot. TRUE return values by
 * all MopupQuery functions will allow the shut down sequence to occur.  The
 * quit query callback is not to be used to do mopup operations such as closing
 * of files and freeing of memory. Instead use TecUtilStateChangeAddCallback()
 * to register a function that will listen for the message
 * \ref StateChange_QuitTecplot which is sent when Tecplot has begun its
 * shutdown sequence.
 *   
 * @return
 *   TRUE if the callback has been added. FALSE only if there is not enough
 *   room to allocate space for an additional callback (highly unlikely).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilQuitAddQueryCallback(QuitQueryCallback)
 *    EXTERNAL QuitQueryCallback
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilQuitAddQueryCallback(MopupQueryAddOnCallback_pf QuitQueryCallback);

/**
 *   Set the mode in which state changes are propagated to the specified state change callback.
 *   
 * @param Callback
 *   Function already registered to receive state change callbacks
 *   
 * @param Mode
 *   Mode you want state changes propagated to your state change callback
 *   function. Choose either StateChangeMode_v80 or StateChangeMode_v90. See
 *   the section "Sending State Changes" in the ADK User's Manual for a
 *   complete description on what is different between these two options
 *   
 * @return
 *   Returns TRUE successful (that is, the callback function was registered), FALSE otherwise.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 TecUtilStateChangeSetMode(
 *   &                   StateChangeCallback,
 *   &                   Mode)
 *    EXTERNAL   StateChangeCallback
 *    INTEGER*4  Mode
 * </FortranSyntax>
 *   
 *   Set the mode state change callbacks to the function BananaCallbackFunction to use the v90 mode.
 *   
 * @code
 *     TecUtilStateChangeSetMode(BananaCallbackFunction,
 *                               StateChangeMode_v90);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeSetMode(StateChangeAddOnCallback_pf Callback,
                                                        StateChangeMode_e           Mode);

/**
 * Include a function in the list of functions to call when a state change
 * occurs in Tecplot. For more detailed discussion, either see
 * TecUtilOnIdleQueueAddCallback() or the ADK User's Manual. If you want to
 * take advantage of newer capabilities with regard to state changes then use
 * TecUtilStateChangeAddCallbackX() instead.
 *   
 * @param StateChangeCallback
 *   This is the name of the callback function you provide. This function will
 *   be called by Tecplot each time a state change occurs. See
 *   
 * @return
 *   Returns TRUE if callback has been added. Returns FALSE only if there is
 *   not enough room to allocate space for an additional callback (highly
 *   unlikely).
 *   
 * The following example will set up a state change callback. This callback
 * will do the following:
 *   
 *   -# When the user picks an object, check and see if it is a zone. If it is, then change its color to red.
 *   -# If anything has happened to the fourth variable, then show an error message.
 *   -# If Tecplot is quitting, then close some files that are open.
 *
 * First, in the Tecplot initialization code add the callback:
 * @code
 *   IsOk=TecUtilStateChangeAddCallback(MyStateChangeCallback);
 * @endcode
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeAddCallback(StateChangeCallback)
 *    EXTERNAL StateChangeCallback
 * </FortranSyntax>
 *
 *
 *   
 * And add the state change callback:
 * @code
 *   void MyStateChangeCallback(StateChange_e StateChange,
 *                              ArbParam_t    CallData)
 *   {
 *     .
 *     .
 *     .
 *     //
 *     // Item 1.  Check for the case when the user picks a zone.
 *     //
 *     if (StateChange == StateChange_PickListSingleSelect)
 *       {
 *         int NumPickedObjects;
 *         NumPickedObjects = TecUtilPickListGetCount();
 *   
 *          //
 *          // Check to see if the last object picked is a zone.
 *          //
 *         if (TecUtilPickListGetType(NumPickedObjects)==
 *             PickObject_Zone)
 *           {
 *             EntIndex_t ZonePicked;
 *             Set_pa     ZoneSet;
 *   
 *             // Get the number of the zone picked 
 *             ZonePicked = TecUtilPickListGetZoneNumber(NumPickedObjects);
 *   
 *             //
 *             // Build the zone set to pass to TecUtilZoneSetMesh().
 *             // In this case there is only one zone
 *             //
 *             ZoneSet = TecUtilSetAlloc(FALSE);
 *             if (ZoneSet)
 *               {
 *                 TecUtilSetAddMember(ZoneSet,ZonePicked,TRUE);
 *   
 *                 //
 *                 // Change the mesh color attribute.
 *                 //
 *                 TecUtilZoneSetMesh(SV_COLOR, ZoneSet, 0.0,
 *                                    (ArbParam_t)Red_C);
 *                 TecUtilSetDealloc(&ZoneSet);
 *               }
 *           }
 *       }
 *       //
 *       // Item 2. Check for a change in the 4th variable.
 *       //
 *       else if (StateChange == StateChange_VarsAltered)
 *       {
 *         if (TecUtilSetIsMember((Set_pa)CallData, (SetIndex_t)4))
 *           TecUtilDialogErrMsg("Var number 4 was altered");
 *       }
 *   
 *       //
 *       // Item 3.  Close an open file if Tecplot is
 *       // shutting down.
 *       //
 *       else if (StateChange == StateChange_QuitTecplot)
 *       {
 *         fclose(SomeOpenFile);
 *       }
 *       .
 *       .
 *       .
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeAddCallback(StateChangeAddOnCallback_pf StateChangeCallback);






/**
 * Inform tecplot of a state change.  Currently this must be called in the following
 * situations:
 *
 *       - Launch and dismiss of modal dialogs (Windows only).
 *       - After a variable has been added and subsequently modified.
 *       - After a variable has been modified.
 *       - After TecUtilDataSetAddZone() has been called and the field data has
 *         been modified (Use \ref StateChange_ZonesAdded).
 *       - After the node map has been altered.
 *
 * The CallData parameter is required for the following state changes:
 *
 * @verbatim
                                  CallData 
     StateChange                  Type      Represents
     --------------------------------------------------------------------------
     StateChange_VarsAltered      Set_pa    set of variables altered
     StateChange_VarsAdded        Set_pa    set of variables added
     StateChange_ZonesDeleted     Set_pa    set of zones deleted
     StateChange_ZonesAdded       Set_pa    set of zones added
     StateChange_NodeMapsAltered  Set_pa    set of zones where nodemaps were altered
   @endverbatim
 *   
 * @param StateChange
 *   Specifies the state change of which to inform Tecplot. addons are only
 *   allowed to send specific state change messages. See the ADK User's Manual
 *   
 * @param CallData
 *   Extra information for the state change.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStateChanged(
 *   &           StateChange,
 *   &           CallDataPtr)
 *    INTEGER*4       StateChange
 *    POINTER         (CallDataPtr, CallData)
 * </FortranSyntax>
 *   
 *   Create a zone, modify its variable values, and inform Tecplot of the state
 *   change:
 *   
 * @code
 *   if (TecUtilDataSetAddZone("Banana", 10, 10, 1,
 *                             ZoneType_Ordered, NULL))
 *     {
 *       EntIndex_t Zone;
 *       Set_pa     ZSet = TecUtilSetAlloc(TRUE);
 *       TecUtilDataSetGetInfo(NULL, &Zone, NULL);
 *   
 *       //  Set up the variable values for the new zone  
 *   
 *       TecUtilSetAddMember(ZSet, Zone,TRUE);
 *       TecUtilStateChanged(StateChange_ZonesAdded,(ArbParam_t)ZSet);
 *       TecUtilSetDealloc(&ZSet);
 *     }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilStateChanged(StateChange_e StateChange,
                                             ArbParam_t    CallData);





/**
 * Register a callback to receive state changes.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_STATECHANGEMODE
 * Type:
 *   StateChangeMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   StateChangeMode_V100
 * Required:
 *   No
 * Notes:
 *   Assigns the state change mode.  Possible values are StateChangeMode_v75,
 *   StateChangeMode_v80 and StateChangeMode_v100.
 *   
 * Name:
 *   SV_CALLBACKFUNCTION
 * Type:
 *   void *
 * Arg Function:
 *   TecUtilArgListAppendFunction()
 * Required:
 *   Yes
 * Notes:
 *   Callback Function for state changes.   See StateChangeAddOnCallbackV2_pf.
 * </ArgListTable>
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeAddCallbackX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *   
 * Register the function MyStateChageCallback with Tecplot:
 *   
 * @code
 *   static void MyStateChangeCallback(StateChange_e StateChange)    
 *   {
 *     TecUtilLockStart(AddOnID);
 *     switch (StateChange)
 *       {
 *         case StateChange_VarsAltered,
 *           {
 *             Set_pa VarsAlteredRef = NULL;
 *             if (TecUtilStateChangeGetVarSet(&VarsAlteredRef))
 *               {
 *                 Set_pa ZonesAlteredRef = NULL;
 *                 if (TecUtilStateChangeGetZoneSet(&ZonesAlteredRef))
 *                   {
 *                     ... take action knowing which vars in which
 *                     ... were altered.
 *                   }
 *                 else
 *                   {
 *                     ... assume all zones were affected.  Take action
 *                     ... knowing which vars were altered.
 *                   }
 *               }
 *           } break;
 *         ... and so on...
 *       }
 *     TecUtilLockFinish(AddOnID);
 *   }
 *   .
 *   .
 *   .
 *   void InitTecAddOn(void)
 *   {
 *     ArgList_pa ArgList;
 *     .
 *     .
 *     .
 *     ArgList = TecUtilArgListAlloc();
 *     TecUtilArgListAppendFunction(ArgList,
 *                                  SV_CALLBACKFUNCTION,
 *                                  MyStateChangeCallback);
 *     TecUtilStateChangeAddCallbackX(ArgList);
 *     TecUtilArgListDealloc(&ArgList);
 *     .
 *     .
 *     .
 *   }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeAddCallbackX(ArgList_pa ArgList);

/**
 *   Retrieve Index supplemental information from the previous state change.
 *   
 * @param Index
 *   Retrieved Index value from the previous state change
 *   
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetIndex(Index)
 *    INTEGER*4 Index
 * </FortranSyntax>
 *   
 *   Your state change callback was just called with \ref StateChange_VarsAltered.
 *   Take action if you can retrieve which data point index was altered.
 *   
 * @code
 *     LgIndex_t IndexValue;
 *     if (TecUtilStateChangeGetIndex(&IndexValue))
 *       {
 *           .... take some action using IndexValue.
 *       }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetIndex(LgIndex_t *Index);
/**
 *   Retrieve enumerated supplemental information from the previous state change.
 *   
 * @param ArbEnum
 *   Retrieved enumerated value from the previous state change. Type cast this to the appropriate enum
 *   to get the value
 *   
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetArbEnum(ArbEnum)
 *    INTEGER*4 ArbEnum
 * </FortranSyntax>
 *   
 *   Your state change callback was just called with \ref StateChange_View.
 *   Take action if the view type was \ref View_Zoom
 *   
 * @code
 *     LgIndex_t ArbEnumValue;
 *     TecUtilStateChangeGetArbEnum(&ArbEnumValue);
 *     if ((View_e)ArbEnumValue == View_Zoom)
 *        {
 *           .... take some action.
 *        }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetArbEnum(LgIndex_t *ArbEnum);

/**
 * Retrieve a reference to the set of zones associated with the previous state
 * change.
 *
 * @par Note:
 *   Unlike many other properties retrieved from Tecplot this set is not an
 *   allocated copy and should not be deallocated.
 *   
 * @param ZoneSetRef
 *   Read-only reference to a retreived set of zones associated with the
 *   previous state change.
 *   
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetZoneSet(ZoneSetRefPtr)
 *    POINTER (ZoneSetRefPtr, ZoneSetRef)
 * </FortranSyntax>
 *   
 * Your state change callback was just called with \ref StateChange_VarsAltered.
 * Take action using the retrieved set of variables that were altered. In
 * addition, if possible, make use of the set of zones that were altered if you
 * can retrieve that information.
 *   
 * @code
 *   Set_pa VarsAlteredRef;
 *   if (TecUtilStateChangeGetVarSet(&VarsAlteredRef))
 *     {
 *        Set_pa ZonesAlteredRef;
 *        if (TecUtilStateChangeGetZoneSet(&ZonesAlteredRef))
 *           {
 *              ... take action knowing both what zones and
 *              ... what vars were altered.
 *           }
 *         else
 *           {
 *              .... take some action using only VarsAltered.
 *              .... assume all zones were altered.
 *           }
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetZoneSet(Set_pa *ZoneSetRef);

/**
 * Retrieve a reference to the set of variables associated with the previous
 * state change.
 *   
 * @par Note:
 *   Unlike many other properties retrieved from Tecplot this set is not an
 *   allocated copy and should not be deallocated.
 *   
 * @param VarSetRef
 *   Read-only reference to a retrieved set of variables associated with the
 *   previous state change.
 *   
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetVarSet(VarSetRefPtr)
 *    POINTER (VarSetRefPtr, VarSetRef)
 * </FortranSyntax>
 *   
 * Your state change callback was just called with \ref StateChange_VarsAltered.
 * Take action using the retrieved set of variables that were altered.
 *   
 * @code
 *   Set_pa VarsAlteredRef;
 *   if (TecUtilStateChangeGetVarSet(&VarsAlteredRef))
 *     {
 *         .... take some action using VarsAltered.
 *         .... Do not dealloc VarsAltered when finished.
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetVarSet(Set_pa *VarSetRef);

/**
 * Retrieve a reference to the name associated with the previous state change.
 *
 * @since
 *   10.0-3-129
 *   
 * @par Note:
 *   Unlike many other properties retrieved from Tecplot this name string is
 *   not an allocated copy and should not be deallocated.
 *   
 * @param NameRef
 *   Read-only reference to a retrieved name associated with the previous state
 *   change.
 *   
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetName(
 *   &                   Name,
 *   &                   NameLength)
 *    CHARACTER*(*)   Name
 *    INTEGER*4       NameLength
 * </FortranSyntax>
 *   
 * Your state change callback was just called with \ref StateChange_AuxDataAdded.
 * Take action if it was "COMMON.TIME" auxiliary data associated with zone 2.
 *   
 * @code
 *     LgIndex_t   ArbEnumValue;
 *     EntIndex_t  Zone;
 *     char       *Name;
 *     if ((TecUtilStateChangeGetArbEnum(&ArbEnumValue) &&      
 *          ((AuxDataLocation_e)ArbEnumValue == AuxDataLocation_Zone)) &&
 *         (TecUtilStateChangeGetZone(&Zone) &&
 *          Zone == 2)                                                 &&
 *         (TecUtilStateChangeGetName(&Name) &&
 *          strcmp(Name, "COMMON.TIME") == 0))
 *        {
 *           .... take some knowing aux data "COMMON.TIME" was just added
 *           .... to zone Zone.
 *        }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetName(char **NameRef);

/**
 * Retrieve the number of the zone associated with the previous state change.
 *   
 * @param Zone
 *   Retreived zone number associated with previous state change
 *   
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStateChangeGetZone(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 *   
 * Your state change callback was just called with \ref StateChange_AuxDataAdded.
 * Take action if it was auxiliary data associated with zone 2.
 *   
 * @code
 *     LgIndex_t  ArbEnumValue;
 *     EntIndex_t Zone;
 *     if ((TecUtilStateChangeGetArbEnum(&ArbEnumValue) &&      
 *          ((AuxDataLocation_e)ArbEnumValue == AuxDataLocation_Zone)) &&
 *         (TecUtilStateChangeGetZone(&Zone) &&
 *          Zone == 2))
 *        {
 *           .... take some knowing aux data was just added
 *           .... to zone Zone.
 *        }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetZone(EntIndex_t *Zone);
/**
 * Retrieve one of the P1,P2,P3,P4,P5, or P6 style parameters from the previous
 * style state change.
 *   
 * @param Param
 *   The parameter number to retrieve. Must be a number between 1 and 6
 *   
 * @param StyleParam
 *   Style parameter retrieved.  DO NOT FREE THIS STRING.
 *   
 * @return
 *   Returns TRUE successful, FALSE otherwise.
 *   
 *   Your state change callback was just called with \ref StateChange_Style. Take
 *   action if the first two style parameters are SV_INTERFACE and
 *   SV_USEAPPROXIMATEPLOTS.
 *   
 * @code
 *     char *P1;
 *     char *P2;
 *     if (TecUtilStateChangeGetStyleParam(1,&P1) &&
 *         TecUtilStateChangeGetStyleParam(2,&P2) &&
 *         (strcmp(P1,SV_INTERFACE) == 0)             &&
 *         (strcmp(P2,SV_USEAPPROXIMATEPLOTS) == 0))
 *       {
 *           .... take some action.
 *       }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStateChangeGetStyleParam(int          Param,
                                                              const char **StyleParam);


/**
 *   Inform Tecplot of a state change.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_STATECHANGE
 * Type:
 *   StateChange_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   The state change to send to Tecplot.  See Section 13.3, "Sending State
 *   Changes," in the ADK User's Manual for a list of possible state changes
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
 *   No, (see below)
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
 *   No, (see below)
 *   
 * Name:
 *   SV_INDEX
 * Type:
 *   Set_pa
 * Arg Function:
 *   TecUtilArgListAppendSet()
 * Default:
 *   NULL
 * Required:
 *   No, (see below)
 * </ArgListTable>
 *
 * @par Note:
 *   The VARLIST, ZONELIST and INDEX arguments are required or optional based
 *   on the value of StateChange. The table below discribes when you can/must
 *   supply these arguments:
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStateChangedX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 *
 *
 *
 * Inform tecplot that variables have been altered.  Tell 
 * Tecplot that variable 2 in zones 3 and 4 has been altered.
 *
 * @code
 * Set_pa     ZoneList = NULL;
 * Set_pa     VarList  = NULL;
 * ArgList_pa ArgList  = NULL;
 *
 * ArgList  = TecUtilArgListAlloc();
 * ZoneList = TecUtilSetAlloc(FALSE);
 * VarList  = TecUtilSetAlloc(FALSE);
 *
 * TecUtilSetAddMember(VarList,2,FALSE);
 *
 * TecUtilSetAddMember(ZoneList,3,FALSE);
 * TecUtilSetAddMember(ZoneList,4,FALSE);
 *
 * TecUtilArgListAppendInt(ArgList,    SV_STATECHANGE,      StateChange_VarsAltered);
 * TecUtilArgListAppendSet(ArgList,    SV_ZONELIST,         ZoneList);
 * TecUtilArgListAppendSet(ArgList,    SV_VARLIST,          VarList);
 * TecUtilStateChangedX(ArgList);
 *
 * TecUtilArgListDealloc(&ArgList);
 * TecUtilSetDealloc(&ZoneList);
 * TecUtilSetDealloc(&VarList);
 * @endcode
 *
 *
 * Same example as above but for FORTRAN.
 *
 * @code
 *       INTEGER*4 DummyArgList
 *       pointer (ArgListPtr,DummyArgList)
 *       pointer (ZoneListPtr,DummyZoneList)
 *       INTEGER*4 DummyZoneList
 *       pointer (VarListPtr,DummyVarList)
 *       INTEGER*4 DummyVarList
 *       INTEGER*4 IShowErr,IErr
 * 
 *       IShowErr = 0
 * 
 *       Call TecUtilArgListAlloc(ArgListPtr)
 *       Call TecUtilSetAlloc(IShowErr,ZoneListPtr)
 *       Call TecUtilSetAlloc(IShowErr,VarListPtr)
 * 
 *       IErr = TecUtilSetAddMember(VarListPtr,2,IShowErr)
 *
 *       IErr = TecUtilSetAddMember(ZoneListPtr,3,IShowErr)
 *       IErr = TecUtilSetAddMember(ZoneListPtr,4,IShowErr)
 * 
 *       IErr = TecUtilArgListAppendInt(ArgListPtr,
 *      &                               'STATECHANGE'//char(0),
 *      &                               StateChange_VarsAltered)
 *       IErr = TecUtilArgListAppendSet(ArgListPtr,
 *      &                               'ZONELIST'//char(0),
 *      &                               ZoneListPtr)
 *       IErr = TecUtilArgListAppendSet(ArgListPtr,
 *      &                               'VARLIST'//char(0),
 *      &                               VarListPtr)
 * 
 *       Call TecUtilStateChangedX(ArgListPtr)
 * 
 *       Call TecUtilArgListDealloc(ArgListPtr)
 *       Call TecUtilSetDealloc(ZoneListPtr)
 *       Call TecUtilSetDealloc(VarListPtr)
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilStateChangedX(ArgList_pa ArgList);




/**
 *   Include a function in the list of functions to call when the
 *   $!ADDONCOMMAND macro command is processed. This in effect allows you to
 *   extend Tecplot's macro language so that commands needed to perform
 *   operations in your addon can be included in a Tecplot macro.
 *   
 * @param AddOnIDString
 *   A unique string used to determine the function to call when an
 *   $!ADDONCOMMAND macro command is processed. Each addon should have its own
 *   unique ID string. For example, if a file converter addon responsible for
 *   converting DXF files for Tecplot defines an ID string of
 *   "DXFCONVERTTOOL-1.2" then this same ID string must be used in the calls to
 *   TecUtilMacroRecordAddOnCommand() and TecUtilMacroAddCommandCallback().
 *   
 * @param MacroCommandCallback
 *   Name of the function to call when the $!ADDONCOMMAND macro command is
 *   processed. The callback function you provide will have the following
 *   syntax:.
 *   
 * @return
 *   Returns TRUE if callback has been added. Returns FALSE if the AddOnIDString is invalid or if there
 *   is not enough room to allocate space for an additional callback (highly unlikely).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroAddCommandCallback(
 *   &                   AddOnIDString,
 *   &                   MacroCommandCallback)
 *    CHARACTER*(*)   AddOnIDString
 *    EXTERNAL        MacroCommandCallback
 * </FortranSyntax>
 *   
 *   The following example shows how an addon can augment the Tecplot macro
 *   language with the commands "GO" and "STOP." First create a function that
 *   will be called by Tecplot when a "GO" or "STOP" command is encountered in
 *   a macro file.
 *   
 * @code
 *   Boolean_t ProcessBananaCommands(char *Command,
 *                                   char **ErrMsg)
 *   {
 *     Boolean_t IsOk = TRUE;
 *     if (strcmp(Command,"GO") == 0)
 *       {
 *         // code here to execute a GO command 
 *       }
 *     else if (strcmp(Command, "STOP") == 0)
 *       {
 *         // code here to execute a STOP command 
 *       }
 *    else
 *       {
 *         *ErrMsg = TecUtilStringAlloc(80, "Error message string");
 *         sprintf(*ErrMsg, "Unknown BANANA command");
 *         IsOk = FALSE;
 *       }
 *     return IsOk;
 *   }
 *   In the initialization code for your addon, register the function with:
 *     .
 *     .
 *     TecUtilMacroAddCommandCallback("BANANA",
 *                                    ProcessBananaCommands);
 *     .
 *     .
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroAddCommandCallback(const char                  *AddOnIDString,
                                                             MacroCommandAddOnCallback_pf MacroCommandCallback);


/**
 * Instruct Tecplot to record a macro command for your addon to the macro file
 * which is currently being recorded.
 *   
 * @param AddOnIDString
 *   Unique string to identify command as belonging to a particular add-on. Use
 *   TecUtilMacroAddCommandCallback() to install a callback function for a
 *   particular add-on
 *   
 * @param Command
 *   Character string containing the command. This command must be one that
 *   your addon understands since it will be passed to the function you
 *   register with TecUtilMacroAddCommandCallback().
 *   
 * @return
 *   TRUE if successful. FALSE if an I/O error occurs while writing the command to a file.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroRecordAddOnCommand(
 *   &                   AddOnIDString,
 *   &                   Command)
 *    CHARACTER*(*)   AddOnIDString
 *    CHARACTER*(*)   Command
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroRecordAddOnCommand(const char *AddOnIDString,
                                                             const char *Command);


/**
 * Instruct Tecplot to record an addon macro command, that includes raw data,
 * to the macro file.
 *   
 * @param AddOnIDString
 *   Unique string to identify command as belonging to a particular add-on. Use
 *   TecUtilMacroAddCommandCallback() to install a callback function for a
 *   particular add-on.
 *   
 * @param Command
 *   Character string containing the command. This command must be one that
 *   your addon understands since it will be passed to the function you
 *   register with TecUtilMacroAddCommandCallback().
 *   
 * @param RawData
 *   Character string containing the raw data. This text will follow a RAWDATA
 *   marker in the macro file. Use of newlines to organize the raw data in a
 *   readable fashion is encouraged. The RawData section cannot contain the $!
 *   since the $! marks the start of a Tecplot macro command. The # may be used
 *   in the raw data, however all text following the # up to the following
 *   newline will be discarded when the macro is processed. When the
 *   $!ADDONCOMMAND is later processed by Tecplot the text in the RAWDATA
 *   section will be concatenated to the command string (including a newline
 *   (\\n) to separate the command from the raw data.
 *   
 * @return
 *   TRUE if successful; FALSE if an I/O error occurs while writing the command to a file.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroRecordAddOnComRaw(
 *   &                   AddOnIDString,
 *   &                   Command,
 *   &                   RawData)
 *    CHARACTER*(*)   AddOnIDString
 *    CHARACTER*(*)   Command
 *    CHARACTER*(*)   RawData
 * </FortranSyntax>
 *   
 *   Record an addon command that has the text "3.7 9.4" in the RAWDATA section:
 *   
 * @code
 *   If (TecUtilMacroIsRecordingActive())
 *      {
 *         TecUtilMacroRecordAddOnComRaw("MYADDON",
 *                                       "LOADXY",
 *                                       "3.7 9.4");
 *      }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroRecordAddOnComRaw(const char *AddOnIDString,
                                                            const char *Command,
                                                            const char *RawData);

/**
 *   Send anything you want to the Tecplot record file.
 *   
 * @param Command
 *   Character string to write to the record file. You can send anything you want
 *   
 * @return
 *   TRUE if successful; FALSE if an I/O error occurs while writing the command to a file.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroRecordRawCommand(Command)
 *    CHARACTER*(*) Command
 * </FortranSyntax>
 *   
 * Record commands that will cause Tecplot to loop to animate 3 zones when the macro is played back:
 *   
 * @code
 *   if (TecUtilMacroIsRecordingActive())
 *     {
 *       TecUtilMacroRecordRawCommand("$!Loop 3\n"
 *                                    "$!ActiveFieldZones[|Loop|]\n"
 *                                    "$!Redraw\n"
 *                                    "$!EndLoop\n");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroRecordRawCommand(const char *Command);


/**
 *   Adds a command to the data journal.
 *   
 * @param AddOnIDString
 *   The ID string of the addon
 *   
 * @param Instructions
 *   Command Instrunctions
 *   
 * @param RawData
 *   Raw Data.
 *   
 * @return
 *   Returns TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetAddJournalCommand(
 *   &                   AddOnIDString,
 *   &                   Instructions,
 *   &                   RawData)
 *    CHARACTER*(*)   AddOnIDString
 *    CHARACTER*(*)   Instructions
 *    CHARACTER*(*)   RawData
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddJournalCommand(const char *AddOnIDString,
                                                              const char *Instructions,
                                                              const char *RawData);

/**
 *   Adds a raw macro command to the data journal.
 *   
 * @param Command
 *   The raw macro command to add to the journal.
 *   
 * @return
 *   Returns TRUE if successful (i.e., the command is valid and could be attached to the journal),
 *   FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetAddRawJournalCom(Command)
 *    CHARACTER*(*) Command
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddRawJournalCom(const char *Command);


/**
 * @deprecated
 *   Please use TecUtilDataSetAddJournalCommand() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetAddPostConvInstr(const char *AddOnIDString,
                                                             const char *Instructions,
                                                             const char *RawData);



#if defined COMPILE_BUT_HIDE_FROM_DOC
LINKTOADDON void STDCALL TecUtilDataSetPostReadFinished(Boolean_t IsReadOk);
#endif



#if defined COMPILE_BUT_HIDE_FROM_DOC
LINKTOADDON Boolean_t STDCALL TecUtilConvAddPostReadCallback(const char                   *ConverterIDString,
                                                             ConverterPostReadCallback_pf ConverterPostReadCallback);
#endif



/**
 *   Stops Tecplot for altering or marking the loaded dataset.
 *   
 * @param DoSuspend
 *   A TRUE value to suspend the marking of the dataset and a FALSE value to allow marking.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataSetSuspendMarking(DoSuspend)
 *    INTEGER*4 DoSuspend
 * </FortranSyntax>
 *   
 *   Set the Marking property to be TRUE:
 *   
 * @code
 *   TecUtilDataSetSuspendMarking(TRUE);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDataSetSuspendMarking(Boolean_t DoSuspend);


/**
 *   Send an event to the Tecplot event dispatcher. This can be used to simulate a user action in the
 *   work area.
 *   
 * @param I
 *   I-Location in the work area where the event occurs (in screen coordinates) with 0 being the left
 *   edge of the work area.
 *   
 * @param J
 *   J-Location in the work area where the event occurs (in screen coordinates) with 0 being the top
 *   edge of the work area.
 *   
 * @param ButtonOrKey
 *   The ButtonOrKey parameter assumes the following: If the event is a button press, then: ButtonOrKey
 *   = 1 ....... Mouse button 1. ButtonOrKey = 2 ....... Mouse button 2. If the event is a key press
 *   then: ButtonOrKey = 32 to 127 for ASCII characters on the keyboard.
 *   
 * @param Event
 *   Event type. The possible values are: Event_ButtonPress, Event_ButtonRelease,
 *   Event_ButtonDoublePress, Event_Motion, Event_Drag or Event_KeyPress
 *   
 * @param IsShifted
 *   Set to TRUE if Shift is held down
 *   
 * @param IsAlted
 *   Set to TRUE if Alt is held down
 *   
 * @param IsControlled
 *   Set to TRUE if Ctrl is held down.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDispatchWorkAreaEvent(
 *   &           I,
 *   &           J,
 *   &           ButtonOrKey,
 *   &           Event,
 *   &           IsShifted,
 *   &           IsAlted,
 *   &           IsControlled)
 *    INTEGER*4       I
 *    INTEGER*4       J
 *    INTEGER*4       ButtonOrKey
 *    INTEGER*4       Event
 *    INTEGER*4       IsShifted
 *    INTEGER*4       IsAlted
 *    INTEGER*4       IsControlled
 * </FortranSyntax>
 *   
 *   Simulate a mouse button one click.
 *   
 * @code
 *   TecUtilDispatchWorkAreaEvent(0,0,1,Event_ButtonPress,
 *    FALSE,FALSE,FALSE);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDispatchWorkAreaEvent(int       I,
                                                      int       J,
                                                      int       ButtonOrKey,
                                                      Event_e   Event,
                                                      Boolean_t IsShifted,
                                                      Boolean_t IsAlted,
                                                      Boolean_t IsControlled);

/**
 *   Create a simple menu option in Tecplot's main menu.
 *   For more options when adding menu options use TecUtilMenuInsertOption() instead.
 *   
 * @param MenuPath
 *   This is used to specify where you want your menu option placed in
 *   Tecplot's menu structure. You cannot put menu options in the File, View,
 *   or Plot menus unless you first call TecUtilMenuClearAll(). Use the newline
 *   character ('\\n') to create options in sub-menu. Each sub-menu may have a
 *   mnemonic specified by putting a '&' in front of the desired letter. The
 *   mnemonic is used only if the sub-menu does not already exist, and thus is
 *   created by this call. (A mnemonic is a keyboard short-cut to access the
 *   menu.)
 *   
 * @param MenuLabel
 *   Text to put on the menu option. You may specify a mnemonic for the option
 *   by putting a '&' in front of the desired letter. If you want to specify a
 *   '&' in the menu option itself, use "&&". (A mnemonic is a keyboard short-cut
 *   to access the menu.) You may specify an accelerator key by separating it
 *   from the MenuLabel with a tab ('\\t'). An accelerator key is a key that can
 *   be used from within Tecplot to perform the function without going through
 *   the menu system. Accelerators should be of the form "Ctrl+L" or "Shift+L"
 *   or "Alt+L" or a combination such as "Ctrl+Shift+L" where "L" can be any letter
 *   or number.
 *   
 * @param Mnemonic
 *   Should be set to TECUTILAUTOMNEMONIC. (If set to any other value, Mnemonic
 *   is assumed to be the character in MenuLabel that should be used as a mnemonic,
 *   and Tecplot will change the string accordingly. If set to '\\0', the MenuLabel
 *   string is used unaltered.)
 *   
 * @param MenuOptionCallback
 *   Function you create which will be called when the new menu option is selected. See
 *   DynamicMenuCallback_pf.
 *   
 * @return
 *   Returns TRUE if the menu option was successfully installed.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMenuAddOption(
 *   &                   MenuPath,
 *   &                   MenuLabel,
 *   &                   Mnemonic,
 *   &                   MenuOptionCallback)
 *    CHARACTER*(*)   MenuPath
 *    CHARACTER*(*)   MenuLabel
 *    CHARACTER*(*)   Mnemonic
 *    EXTERNAL        MenuOptionCallback
 * </FortranSyntax>
 *   
 *   Add an option to the Tools menu in Tecplot called "Banana." When selected,
 *   have Tecplot call the function BananaCallback. 
 *
 *   First create the BananaCallback function:
 *   
 * @code
 *   void BananaCallback(void)
 *   {
 *      // code executed when "Banana" is selected in the
 *      // "Tools" menu.
 *   }
 * @endcode
 *
 *   In the initialization code for the addon add:
 *
 * @code
 *    IsOk = TecUtilMenuAddOption("Tools",
 *                                "&Banana",
 *                                TECUTILAUTOMNEMONIC,
 *                                BananaCallback);
 * @endcode
 *
 *   To put the menu item in a sub-menu of Tools called "Fruit" use:
 *
 * @code
 *   IsOk = TecUtilMenuAddOption("Tools\n&Fruit", "&Banana", TECUTILAUTOMNEMONIC, BananaCallback);
 * @endcode
 *   
 *   The sub-menu "Fruit" of "Tools" will be created if necessary, and if it is created, it will be
 *   given a mnemonic of 'F'.
 *
 *   To install an accelerator of Shift+B for this option, use:
 *
 * @code
 *   IsOk = TecUtilMenuAddOption("Tools\n&Fruit", "&Banana\tShift+B", TECUTILAUTOMNEMONIC, BananaCallback);
 * @endcode
 *   
 *   The user can then invoke the BananaCallback without using the menu system. Hitting Shift+B in the
 *   work area of Tecplot will call BananaCallback.
 */
LINKTOADDON Boolean_t STDCALL TecUtilMenuAddOption(const char            *MenuPath,
                                                   const char            *MenuLabel,
                                                   char                   Mnemonic,
                                                   DynamicMenuCallback_pf MenuOptionCallback);

/**
 *   Adds menu separator to the end of the specified parent menu.
 *   For more options when adding separators use TecUtilMenuInsertSeparator() instead.
 *   
 * @param MenuPath
 *   Specify the path to where the separator should be placed.
 *   
 * @return
 *   Returns TRUE if the menu separator was successfully added, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMenuAddSeparator(MenuPath)
 *    CHARACTER*(*) MenuPath
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilMenuAddSeparator(const char *MenuPath);


/**
 * @deprecated
 *   This function was never implemented. Please use TecUtilMenuRegisterSensitivityCallback() instead.
 */
LINKTOADDON Boolean_t STDCALL TecUtilMenuSetSensitivity(const char *MenuPath,
                                                        const char *MenuLabel,
                                                        Boolean_t   IsSensitive);

/**
 *   Gets the menu handle for Tecplot's main menu. This handle may then be used
 *   with other functions such as TecUtilMenuInsertSubMenu().
 *   
 * @return
 *   Returns menu handle for Tecplot's main menu, or NULL if there is no main menu (as
 *   happens when Tecplot is running in batch mode).
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuGetMain(ResultPtr)
 *    POINTER (ResultPtr, Result)
 * </FortranSyntax>
 *
 * @sa TecUtilMenuInsertSubMenu() for an example of TecUtilMenuGetMain().
 */
LINKTOADDON Menu_pa STDCALL TecUtilMenuGetMain(void);

/**
 * Gets the menu handle for a top-level standard Tecplot menu. This handle may
 * then be used with other functions such as TecUtilMenuInsertOption().
 *   
 * @param StandardMenu
 *   Specify the top-level menu for which to get the handle.
 *   
 * @return
 *   Returns menu handle for the specified top-level menu, or NULL if there is
 *   no such menu (as happens when Tecplot is running in batch mode or when the
 *   menu has been deleted).
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuGetStandard(
 *   &           StandardMenu,
 *   &           ResultPtr)
 *    INTEGER*4       StandardMenu
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 * @sa TecUtilMenuInsertOption() for an example of TecUtilMenuGetStandard().
 */
LINKTOADDON Menu_pa STDCALL TecUtilMenuGetStandard(StandardMenu_e StandardMenu);

/**
 *   Inserts a submenu into a menu. A submenu displays other menu items when it
 *   is selected in the menu system.
 *
 * @param ParentMenu
 *   Specify the menu in which to insert the new submenu.
 *   
 * @param InsertPos
 *   Specify the position in the parent menu for the new submenu.
 *   MENU_POSITION_FIRST will put the new submenu first in the parent menu.
 *   MENU_POSITION_LAST will put the new submenu last in the parent menu.
 *   
 * @param SubMenuLabel
 *   Specify the text for the new submenu. You may specify a mnemonic for the submenu
 *   option by putting a '&' in front of the desired letter. If you want to specify a
 *   '&' in the submenu itself, use "&&". (A mnemonic is a keyboard short-cut
 *   to access the menu.)
 *
 * @return
 *   Returns menu handle for the new submenu, or NULL if creating the submenu failed (as
 *   happens when Tecplot is running in batch mode).
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuInsertSubMenu(
 *   &           ParentMenuPtr,
 *   &           InsertPos,
 *   &           SubMenuLabel,
 *   &           ResultPtr)
 *    POINTER         (ParentMenuPtr, ParentMenu)
 *    INTEGER*4       InsertPos
 *    CHARACTER*(*)   SubMenuLabel
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 *   Insert a submenu in the main menu called "Extras".
 *   This submenu will appear first in the main menu.
 *   This submenu has a keyboard short-cut of "X".  That is, the user can activate
 *   this submenu by simply pressing the letter "X" while in the main menu.  Since
 *   the option is in the main menu, "Alt+X" will work on some systems as well.
 *   
 * @code
 *    Menu_pa MainMenu = TecUtilMenuGetMain();
 *    IsOk = TecUtilMenuInsertSubMenu(MainMenu,
 *                                    MENU_POSITION_FIRST,
 *                                    "E&xtras");
 * @endcode
 */
LINKTOADDON Menu_pa STDCALL TecUtilMenuInsertSubMenu(Menu_pa     ParentMenu,
                                                     int         InsertPos,
                                                     const char *SubMenuLabel);
                                                     
/**
 * Inserts a menu option into a menu. A menu option performs some action when
 * selected in the menu system.
 *
 * @param ParentMenu
 *   Specify the menu in which to insert the new option.
 *   
 * @param InsertPos
 *   Specify the position in the parent menu for the new option.
 *   MENU_POSITION_FIRST will put the new option first in the parent menu.
 *   MENU_POSITION_LAST will put the new option last in the parent menu.
 *   
 * @param OptionLabel
 *   Specify the text for the new option. You may specify a mnemonic for the 
 *   option by putting a '&' in front of the desired letter. If you want to specify a
 *   '&' in the menu option itself, use "&&". (A mnemonic is a keyboard short-cut
 *   to access the menu.) You may specify an accelerator key at the end of the label
 *   by separating it from the rest with a tab ('\\t'). An accelerator key is a key that can
 *   be used from within Tecplot to activate the optionwithout going through
 *   the menu system. Accelerators should be of the form "Ctrl+L" or "Shift+L"
 *   or "Alt+L" or a combination such as "Ctrl+Shift+L" where "L" can be any letter
 *   or number.
 *
 * @param ActivateCallback
 *    Specify callback function to call when new menu option is activated.
 *
 * @param ActivateClientData
 *    Specify data to send to ActivateCallback when called for this new menu option.
 *
 * @return
 *   Returns menu handle for the new menu option, or NULL if creating the menu option failed (as
 *   happens when Tecplot is running in batch mode).
 *   
 * <FortranSyntax>
 * C
 * C  Note: Only one menu option callback function can active for each FORTRAN add-on.
 * C  Registering another menu option callback function overwrites the first.
 * C  You may use ClientData to differentiate between different menu options.
 * C
 *    SUBROUTINE TecUtilMenuInsertOption(
 *   &           ParentMenuPtr,
 *   &           InsertPos,
 *   &           OptionLabel,
 *   &           ActivateCallback,
 *   &           ActivateClientDataPtr,
 *   &           ResultPtr)
 *    POINTER         (ParentMenuPtr, ParentMenu)
 *    INTEGER*4       InsertPos
 *    CHARACTER*(*)   OptionLabel
 *    EXTERNAL        ActivateCallback
 *    POINTER         (ActivateClientDataPtr, ActivateClientData)
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Insert into the Tools menu two new options called "Do Something 1" and "Do Something 2".
 *   These menu options will appear as the first two options in the Tools menu.
 *   
 *   First create a DoSomethingCallback function:
 *   
 * @code
 *   void DoSomethingCallback(ArbParam_t ClientData)
 *   {
 *      // code executed when one of the "Do Something" options is selected in the
 *      // "Tools" menu.  ClientData will be 1 for the "Do Something 1" menu option,
 *      // and ClientData will be 2 for the "Do Something 2" menu option.
 *   }
 * @endcode
 *
 *   Elsewhere (probably in the initialization of the add-on) add the menu items to
 *   the "Tools" menu.
 *
 * @code
*    Menu_pa ToolsMenu, DoSomething1Option, DoSomething2Option;
 *   ToolsMenu = TecUtilMenuGetStandard(StandardMenu_Tools);
 *   DoSomething1Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                MENU_POSITION_FIRST,
 *                                                "Do Something 1",
 *                                                DoSomething,
 *                                                1);
 *   DoSomething2Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                2, // second item in menu
 *                                                "Do Something 2",
 *                                                DoSomething,
 *                                                2);
 * @endcode
 *
 *   To assign keyboard short-cuts to the menu options, use the following code instead.
 *   "Do Something 1" has a keyboard short-cut of '1'.  That is, it
 *   can be accessed quickly while in the "Tools" menu by simply pressing one.  Likewise
 *   "Do Something 2" has a keyboard short-cut of '2'.
 *
 * @code
 *   DoSomething1Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                MENU_POSITION_FIRST,
 *                                                "Do Something &1",
 *                                                DoSomething,
 *                                                1);
 *   DoSomething2Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                2, // second item in menu
 *                                                "Do Something &2",
 *                                                DoSomething,
 *                                                2);
 * @endcode
 *   To assign keyboard accelerators to the menu option, use the following code instead.
 *   "Do Something 1" still has the keyboard short-cut, but it also has an accelerator
 *   key of "Alt+1".  An accelerator key allows the user to activate the menu option without
 *   accessing the menu.  Pressing "Alt+1" while in the Tecplot main menu will activate
 *   the "Do Something 1" menu option.  Likewise, "Alt+2" activates the "Do Something 2"
 *   menu option.
 *
 * @code
 *   DoSomething1Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                MENU_POSITION_FIRST,
 *                                                "Do Something &1\tAlt+1",
 *                                                DoSomething,
 *                                                1);
 *   DoSomething2Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                2, // second item in menu
 *                                                "Do Something &2\tAlt+2",
 *                                                DoSomething,
 *                                                2);
 * @endcode
  */
LINKTOADDON Menu_pa STDCALL TecUtilMenuInsertOption(Menu_pa                 ParentMenu,
                                                    int                     InsertPos,
                                                    const char             *OptionLabel,
                                                    MenuActivateCallback_pf ActivateCallback,
                                                    ArbParam_t              ActivateClientData);

/**
 *   Inserts a menu toggle into a menu. Like a menu option, a menu toggle performs
 *   some action when selected in the menu system, but a menu toggle also displays
 *   an on/off state.
 *
 * @param ParentMenu
 *   Specify the menu in which to insert the new toggle.
 *   
 * @param InsertPos
 *   Specify the position in the parent menu for the new toggle.
 *   MENU_POSITION_FIRST will put the new toggle first in the parent menu.
 *   MENU_POSITION_LAST will put the new toggle last in the parent menu.
 *   
 * @param ToggleLabel
 *   Specify the text for the new toggle. You may specify a mnemonic for the 
 *   toggle by putting a '&' in front of the desired letter. If you want to specify a
 *   '&' in the menu toggle itself, use "&&". (A mnemonic is a keyboard short-cut
 *   to access the menu.) You may specify an accelerator key at the end of the label
 *   by separating it from the rest with a tab ('\\t'). An accelerator key is a key that can
 *   be used from within Tecplot to activate the menu toggle without going through
 *   the menu system. Accelerators should be of the form "Ctrl+L" or "Shift+L"
 *   or "Alt+L" or a combination such as "Ctrl+Shift+L" where "L" can be any letter
 *   or number.
 *
 * @param ActivateCallback
 *    Specify callback function to call when new menu toggle is activated (either to
 *    turn it on or to turn it off).
 *
 * @param ActivateClientData
 *    Specify data to send to ActivateCallback when called for this new menu toggle.
 *   
 * @param GetToggleStateCallback
 *    Specify callback function that will return the current state of the new menu toggle.
 *    This callback is called whenever Tecplot needs to know the checked/unchecked stated
 *    of the toggle. If the callback returns TRUE, the toggle is checked. If the callback
 *    returns FALSE, the toggle is unchecked.
 *
 * @param GetToggleStateClientData
 *    Specify data to send to GetToggleStateCallback when called for this new menu toggle.
 *   
 * @return
 *   Returns menu handle for the new menu toggle, or NULL if creating the menu toggle failed (as
 *   happens when Tecplot is running in batch mode).
 *   
 * <FortranSyntax>
 * C
 * C  Note: Only one menu toggle activate callback function can active for each FORTRAN add-on.
 * C  Registering another menu toggle activate callback function overwrites the first.
 * C  You may use ClientData to differentiate between different menu toggles.
 * C  Likewise, only one menu toggle get-state callback can be active for each FORTRAN add-on.
 * C
 *    SUBROUTINE TecUtilMenuInsertToggle(
 *   &           ParentMenuPtr,
 *   &           InsertPos,
 *   &           ToggleLabel,
 *   &           ActivateCallback,
 *   &           ActivateClientDataPtr,
 *   &           GetToggleStateCallback,
 *   &           GetToggleStateClientDataPtr,
 *   &           ResultPtr)
 *    POINTER         (ParentMenuPtr, ParentMenu)
 *    INTEGER*4       InsertPos
 *    CHARACTER*(*)   ToggleLabel
 *    EXTERNAL        ActivateCallback
 *    POINTER         (ActivateClientDataPtr, ActivateClientData)
 *    EXTERNAL        GetToggleStateCallback
 *    POINTER         (GetToggleStateClientDataPtr, GetToggleStateClientData)
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Insert into the Tools menu a toggle menu item called "Simple Toggle".
 *   This toggle will appear last in the the Tools menu.
 *   
 *   First create a SimpleToggleActivate callback function.  This is called
 *   whenever the toggle is selected.
 *   
 * @code
 *   // global variable to track the state of the toggle
 *   static Boolean_t SimpleToggleState = TRUE; // checked by default
 *
 *   void SimpleToggleActivate(ArbParam_t ClientData)
 *   {
 *      // code executed when the "Simple Toggle" is selected in the
 *      // "Tools" menu.  ClientData will be 0 because that is what was
 *      // past in the call to TeCUtilMenuInsertToggle() below.
 *      //
 *      // For this simple case, we just invert a global variable.
 *      //
 *      SimpleToggleState = !SimpleToggleState;
 *   }
 * @endcode
 *
 *   Next create a SimpleToggleActivate callback function.  This is called
 *   whenever the state of the toggle is needed.
 *   
 * @code
 *   Boolean_t GetSimpleToggleState(ArbParam_t ClientData)
 *   {
 *      // code executed whenever the state of the "Simple Toggle" is needed.
 *      // ClientData will be 1 because that is what was past in the call
 *      // to TeCUtilMenuInsertToggle() below.
 *      //
 *      // For this simple case, we just look at the value of a global variable.
 *      //
 *      return SimpleToggleState;
 *   }
 * @endcode
 *
 * Elsewhere (probably in the initialization of the add-on) add the toggle to
 * the "Tools" menu.
 *
 * @code
 *   Menu_pa ToolsMenu, SimpleToggle;
 *   ToolsMenu = TecUtilMenuGetStandard(StandardMenu_Tools);
 *   SimpleToggle = TecUtilMenuInsertToggle(ToolsMenu,
 *                                          MENU_POSITION_FIRST,
 *                                          "Simple Toggle",
 *                                          SimpleToggleActivate,
 *                                          0, //ActivateClientData
 *                                          GetSimpleToggleState,
 *                                          1); //GetToggleStateClientData
 * @endcode
 *
 * @par Note:
 *   Menu toggles can have keyboard short-cuts and accelerators just like menu
 *   options.
 *
 * @sa TecUtilMenuInsertOption() for examples.
 */
LINKTOADDON Menu_pa STDCALL TecUtilMenuInsertToggle(Menu_pa                       ParentMenu,
                                                    int                           InsertPos,
                                                    const char                   *ToggleLabel,
                                                    MenuActivateCallback_pf       ActivateCallback,
                                                    ArbParam_t                    ActivateClientData,
                                                    MenuGetToggleStateCallback_pf GetToggleStateCallback,
                                                    ArbParam_t                    GetToggleStateClientData);

/**
 * Inserts a separator into a menu. A separator is a line displayed between menu items, it cannot
 * be selected from the menu system.
 *
 * @param ParentMenu
 *   Specify the menu in which to insert the new separator.
 *   
 * @param InsertPos
 *   Specify the position in the parent menu for the new separator.
 *   MENU_POSITION_FIRST will put the new separator first in the parent menu.
 *   MENU_POSITION_LAST will put the new separator last in the parent menu.
 *   
 * @return
 *   Returns menu handle for the new separator, or NULL if creating the separator failed (as
 *   happens when Tecplot is running in batch mode).
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuInsertSeparator(
 *   &           ParentMenuPtr,
 *   &           InsertPos,
 *   &           ResultPtr)
 *    POINTER         (ParentMenuPtr, ParentMenu)
 *    INTEGER*4       InsertPos
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *
 *   Add some options to the top of the "Tools" menu and then separate them
 *   from the rest of that menu by including a separator.
 *
 * @code
 *   Menu_pa ToolsMenu, DoSomething1Option, DoSomething2Option, Separator;
 *   ToolsMenu = TecUtilMenuGetStandard(StandardMenu_Tools);
 *   DoSomething1Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                MENU_POSITION_FIRST,
 *                                                "Do Something 1",
 *                                                DoSomething,
 *                                                1);
 *   DoSomething2Option = TecUtilMenuInsertOption(ToolsMenu,
 *                                                2, // second item in menu
 *                                                "Do Something 2",
 *                                                DoSomething,
 *                                                2);
 *   Separator = TecUtilMenuItemInsertSeparator(ToolsMenu,
 *                                              3); // third item in the menu
 * @endcode
 */
LINKTOADDON Menu_pa STDCALL TecUtilMenuInsertSeparator(Menu_pa ParentMenu,
                                                       int     InsertPos);

/**
 *   Inserts a standard Tecplot menu into another menu as a submenu.
 *
 * @param ParentMenu
 *   Specify the menu in which to insert the standard menu.
 *   
 * @param InsertPos
 *   Specify the position in the parent menu for the standard menu.
 *   MENU_POSITION_FIRST will put the standard menu first in the parent menu.
 *   MENU_POSITION_LAST will put the standard menu last in the parent menu.
 *   
 * @param StandardMenu
 *   Specify the top-level menu for which to get the handle.
 *   This menu cannot exist already in Tecplot.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuInsertStandard(
 *   &           ParentMenuPtr,
 *   &           InsertPos,
 *   &           ResultPtr)
 *    POINTER         (ParentMenuPtr, ParentMenu)
 *    INTEGER*4       InsertPos
 *    INTEGER*4       StandardMenu
 * </FortranSyntax>
 *
 *   Move the Edit menu to be the first menu in the system.  First delete the
 *   Edit menu if it exists, then readd it to the menu system as the first
 *   item in the main menu.
 *
 * @code
 *   Menu_pa MainMenu, EditMenu;
 *   MainMenu = TecUtilMenuGetMain();
 *   EditMenu = TecUtilMenuGetStandard(StandardMenu_Edit);
 *   if ( EditMenu != NULL )
 *     TecUtilMenuDelete(EditMenu);
 *   TecUtilMenuInsertStandard(MainMenu,
 *                             MENU_POSITION_FIRST,
 *                             StandardMenu_Edit);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilMenuInsertStandard(Menu_pa                 ParentMenu,
                                                   int                     InsertPos,
                                                   StandardMenu_e          StandardMenu);

/**
 *   Registers a function that will determine the sensitivity of a menu item (i.e.,
 *   whether the item is enabled for user input or grayed out).  If this function is
 *   not used for a particular menu item, the item will always be available.  If called
 *   again on the same menu item, the new function replaces the old one.
 *
 * @param MenuItem
 *   Specify the menu item for which to register a sensitivity
 *   function. This item can be a option, toggle, or submenu, but it cannot be a separator.
 *   
 * @param GetSensitivityCallback
 *    Specify callback function to call to determine the sensitivity of the menu item.  This
 *    function returns TRUE if the menu is sensitive, and FALSE if the menu is insensive (gray).
 *
 * @param GetSensitivityClientData
 *    Specify data to send to GetSensitivityCallback when called for the specified menu item.
 *   
 * <FortranSyntax>
 * C
 * C  Note: Only one menu sensitivity callback function can active for each FORTRAN add-on.
 * C  Registering another menu sensitivity callback callback function overwrites the first.
 * C  You may use ClientData to differentiate between different menu items.
 * C
 *    SUBROUTINE TecUtilMenuRegisterSensitivityCallback(
 *   &           MenuItemPtr,
 *   &           InsertPos,
 *   &           ToggleLabel,
 *   &           ActivateCallback,
 *   &           ActivateClientDataPtr,
 *   &           GetToggleStateCallback,
 *   &           GetToggleStateClientDataPtr,
 *   &           ResultPtr)
 *    POINTER         (MenuItemPtr, MenuItem)
 *    EXTERNAL        GetSensitivityCallback
 *    POINTER         (GetSensitivityClientDataPtr, GetSensitivityClientData)
 * </FortranSyntax>
 *
 *   Set the sensitivities of two menu items.  The first will be available only when
 *   the frame is 3D.  The second will be available only when the frame is a polar
 *   line plot.
 *
 *   First create a sensitivity callback function.  This one uses
 *   the ClientData parameter to check different plot types.
 *   
 * @code
 *   Boolean_t CheckIfCurrentFrameIsOfType(ArbParam_t ClientData)
 *   {
 *     PlotType_e PlotType = (PlotType_e)ClientData;
 *     return TecUtilFrameGetPlotType()==PlotType;
 *   }
 * @endcode
 *
 *   Then add the sensitivity callback to the menu item.  Usually this is
 *   done when the item is created.
 *   
 * @code
 *   extern Menu_pa DoSomething1Option, DoSomething2Option; // created elsewhere
 *   TecUtilMenuRegisterSensitivityCallback(DoSomething1Option,
 *                                          CheckIfCurrentFrameIsOfType,
 *                                          (ArbParam_t)PlotType_Cartesian3D);
 *   TecUtilMenuRegisterSensitivityCallback(DoSomething2Option,
 *                                          CheckIfCurrentFrameIsOfType,
 *                                          (ArbParam_t)PlotType_PolarLine);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilMenuRegisterSensitivityCallback(Menu_pa                       MenuItem,
                                                                MenuGetSensitivityCallback_pf GetSensitivityCallback,
                                                                ArbParam_t                    GetSensitivityClientData);

/**
 * Deletes a menu item. If that menu item is a submenu, all of its sub menu
 * items are deleted as well. Deleting submenus can delete the menu items of
 * other add-ons and may make those add-ons inaccessible.
 *
 * @par Note:
 *   This function should not be called as a direct response to a menu
 *   selection event as it will delete the menu system out from under it and
 *   produce unpredictable results. To handle this case register an on-idle
 *   callback to clear the menu system. See TecUtilOnIdleQueueAddCallback for
 *   registering an on-idle callback.
 *
 * @param MenuItemPtr
 *   Specify a pointer to the menu item to delete. This item can be an option,
 *   toggle, submenu, or separator.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuDelete(MenuItemPtr)
 *    POINTER (MenuItemPtr, MenuItem)
 * </FortranSyntax>
 *
 *   To delete the File menu.
 *
 * @code
 *   Menu_pa FileMenu = TecUtilMenuGetStandard(StandardMenu_File);
 *   TecUtilMenuDelete(FileMenu);
 * @endcode
 *
 * This code does the same thing using as TecUtilMenuClearAll(). The main menu
 * always exists, so TecUtilMenuDelete() on the main menu only deletes the
 * submenus of the main menu.
 *
 * @code
 *   Menu_pa MainMenu = TecUtilMenuGetMain();
 *   TecUtilMenuDelete(MainMenu);
 *   // main menu is still around, so add an item to it
 *   TecUtilMenuInsertOption(MainMenu,
 *                           MENU_POSITION_FIRST,
 *                           "Do Something &1\tAlt+1",
 *                           DoSomething,
 *                           1);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilMenuDelete(Menu_pa *MenuItemPtr);

/**
 * Remove all menus, submenus, and menu items from the Tecplot menu bar. This
 * will clear menu items added by other add-ons, making those add-ons
 * inaccessible. The add-on containing this call should either be the only
 * add-on loaded into Tecplot, or it should be loaded into Tecplot first and it
 * should clear the menus during initialization before the other add-ons are
 * loaded. For more control over which menu items are deleted, use
 * TecUtilMenuDelete() instead.
 *
 * @par Note:
 *   This function should not be called as a direct response to a menu
 *   selection event as it will delete the menu system out from under it and
 *   produce unpredictable results. To handle this case register an on-idle
 *   callback to clear the menu system. See TecUtilOnIdleQueueAddCallback for
 *   registering an on-idle callback.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilMenuClearAll()
 * </FortranSyntax>
 *   
 * To wipe out Tecplot's standard menu and replace it with a single option
 * called "Banana" which is located under a menu called "Go":
 *   
 * @code
 *   TecUtilMenuClearAll():
 *   IsOk = TecUtilMenuAddOption("&Go", "Banana", 'B', BananaCallBack);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilMenuClearAll(void);


/****************************************************************************
 *          DataSet Converters and Loaders.                                 *
 *                                                                          *
 * A Converter is an addon that connects to tecplot in a way such that      *
 * tecplot uses it's own file/io dialogs to read the non-Tecplot data into  *
 * tecplot and the converter is a simple function that only knows how       *
 * to convert from it's own data format into tecplot's binary format.       *
 *                                                                          *
 * Converters are registered with tecplot by calling:                       *
 *                                                                          *
 *              TecUtilImportAddConverter()                                 *
 *                                                                          *
 * An loader works like this:                                               *
 *     - TecUtilMenuAddOption() is called to register a callback            *
 *       (most often used to bring up a dialog) to the loader addon         *
 *                                                                          *
 *     - The loader registers itself with tecplot by calling                *
 *       TecUtilImportAddLoader()                                           *
 *                                                                          *
 *     When a request is made to use the loader function:                   *
 *                                                                          *
 *        - The loader creates a new dataset and loads the data into        *
 *          tecplot by hand.                                                *
 *                                                                          *
 *        - The loader then calls TecUtilImportSetLoaderInstr()             *
 *          to assign the specific instructions needed to load the data     *
 *          into the current dataset. These instructions will be included   *
 *          in any layout files written out.                                *
 ****************************************************************************/
/**
 * Register a data set converter with Tecplot. This will add an option to the
 * list of data imports accessed via the File/Import menu option. See Section
 * 9.2, "Data Set Loaders," in the ADK User's Manual for a discussion of data
 * set loaders.
 *   
 * @param ConverterCallback
 *   Name of the function to call to convert data to the Tecplot binary format.
 *   
 * @param ConverterName
 *   Unique name given to the data set converter. This name is used in the list
 *   of importers in the dialog launched by choosing File/Import. If a layout
 *   file is created the $READDATASET macro command will also use this name to
 *   identify the converter to use.
 *   
 * @param FNameExtension
 *   This is the file name extension used by files converted with this data set
 *   converter
 *   
 * @return
 *   Returns TRUE if the data set converter is added.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImportAddConverter(
 *   &                   ConverterCallback,
 *   &                   ConverterName,
 *   &                   FNameExtension)
 *    POINTER         (ConverterCallbackPtr, ConverterCallback)
 *    CHARACTER*(*)   ConverterName
 *    CHARACTER*(*)   FNameExtension
 * </FortranSyntax>
 *   
 *   An addon is created that has the following data set converter function:
 *   
 * @code
 *   Boolean_t  ConvertBananaData(char  *DataFName,
 *                                char  *TempBinFName,
 *                                char **MessageString);
 *   {
 *      Boolean_t IsOk = TRUE;
 *      //
 *      // Code here to open DataFName,
 *      // read in the data and write out a binary
 *      // Tecplot datafile to TempBinFName using
 *      // calls to TecUtilTecXxx functions. If there is
 *      // a problem, call TecUtilStringAlloc() on
 *      // MessageString, supply a message describing the
 *      // issue, and set ISOk to FALSE.
 *      //
 *      return (IsOk);
 *   }
 *   The call to register the data set converter with Tecplot is then accomplished using the following:
 *   .
 *   .
 *       IsOk = TecUtilImportAddConverter(ConvertBananaData,
 *                                        "BANANA",
 *                                        "*.ban");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilImportAddConverter(DataSetConverter_pf ConverterCallback,
                                                          const char         *ConverterName,
                                                          const char         *FNameExtension);


/**
 *   Register a data set loader with Tecplot. This will add an option to the list of data imports
 *   accessed via the File/Import menu option. Data set loaders are more complex than data set
 *   converters, but provide you with greater flexibility in terms of the graphical user interface and
 *   how the data can be retrieved. See Section 9.2, "Data Set Loaders," in the ADK User's Manual for
 *   a discussion of data set loaders.
 *   
 * @param LoaderCallback
 *   Name of the function to call to load non-Tecplot format data into Tecplot. The data set loader
 *   itself calls this function when a request is made to load non-Tecplot format data in via the user
 *   interface. Tecplot also calls this function when processing a $!READDATASET macro command that
 *   identifies this loader.
 *   
 * @param DataSetLoaderName
 *   Unique name given to the DataSet Loader. This name is used in the list of importers in the dialog
 *   launched by choosing File/Import. If a layout file is created, the $READDATASET macro command
 *   will also use this name to identify the loader to use.
 *   
 * @param LoaderSelectedCallback
 *   Name of the function that is called when the user selects this data set loader from the list of
 *   importers in the File/Import dialog. This function typically will launch a custom dialog to
 *   prompt the user to identify the data to be loaded
 *   
 * @param InstructionOverrideCallback
 *   Name of the function to call when the user chooses to override the data source for a given data
 *   set when a layout file is being read in. If set to NULL then Tecplot will issue an error message
 *   stating that this operation is not available. If provided, this function typically will launch a
 *   dialog that shows the user what the current settings are to load the data and provide a means by
 *   which the user can alter these instructions. The Instructions stringlist is updated according to
 *   changes made by the user and the new information is then used to load the data.
 *   
 * @return
 *   Returns TRUE if the data set loader is added.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImportAddLoader(
 *   &                   LoaderCallback,
 *   &                   DataSetLoaderName,
 *   &                   LoaderSelectedCallback,
 *   &                   InstructionOverrideCallback)
 *    POINTER         (LoaderCallbackPtr, LoaderCallback)
 *    CHARACTER*(*)   DataSetLoaderName
 *    POINTER         (LoaderSelectedCallbackPtr, LoaderSelectedCallback)
 *    POINTER         (InstructionOverrideCallbackPtr, InstructionOverrideCallback)
 * </FortranSyntax>
 *   
 *   An addon is created that has the following data set loader function:
 *   
 * @code
 *   Boolean_t LoadBananaData (StringList_pa Instructions)
 *   {
 *     Boolean_t IsOk = TRUE;
 *     //
 *     // Add code to scan through instructions
 *     // and load the data.  When done, inform
 *     // Tecplot about the instructions used to
 *     // load the data.
 *     //
 *      if (IsOk)
 *        TecUtilImportSetLoaderInstr("BANANA", Instructions);
 *      return (IsOk);
 *   }
 *   
 *   A function is also created to handle user requests to use the loader from the File/Import dialog:
 *   
 *     void BananaLoaderDialog (void)
 *     {
 *      //
 *      // Launch a custom dialog to prompt the
 *      // user to identify the data to be loaded.
 *      //
 *     }
 *   A function is also created to handle user requests to modify the instructions.  This function is optional.
 *     Boolean_t OverrideBananaInstructions
 *              (StringList_pa Instructions)
 *     {
 *        Boolean_t IsOk = TRUE;
 *        //
 *        // Code here to view the current instructions and present
 *        // an interface to the user to change them.
 *        //
 *         return (IsOk);
 *     }
 *   The call to register the data set loader with Tecplot is then accomplished using the following:
 *      .
 *      .
 *      IsOk = TecUtilImportAddLoader(LoadBananaData,
 *                                    "BANANA",
 *                                    BananaLoaderDialog,
 *                                    OverrideBananaInstructions);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilImportAddLoader(DataSetLoader_pf                    LoaderCallback,
                                                      const char                         *DataSetLoaderName,
                                                      DynamicMenuCallback_pf              LoaderSelectedCallback,
                                                      DataSetLoaderInstructionOverride_pf InstructionOverrideCallback);


/**
 * Inform Tecplot about the instructions used to load the current data set. It
 * is assumed that the current data set was loaded via a data set loader. The
 * current frame must have an attached data set when this function is used.
 *   
 * @param DataSetLoaderName
 *   Unique loader name. This same name must be used in
 *   TecUtilImportAddLoader().
 *   
 * @param Instructions
 *   Instructions used to load the current data set
 *   
 * @return
 *   Returns TRUE if the instructions were successfully loaded.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilImportSetLoaderInstr(
 *   &                   DataSetLoaderName,
 *   &                   InstructionsPtr)
 *    CHARACTER*(*)   DataSetLoaderName
 *    POINTER         (InstructionsPtr, Instructions)
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilImportSetLoaderInstr(const char    *DataSetLoaderName,
                                                          StringList_pa Instructions);


                                                                     
/**
 * Writes a $!READDATASET macro command to the macro file if macro
 * recording is on.
 *
 * @par Note:
 *   Since TecUtilImportSetLoaderInstr() will automatically call this function,
 *   you normally do not need to call this function.  If you are writing a
 *   loader that does not use an instruction string, that is it does not call
 *   TecUtilImportSetLoaderInstr(), then you should call this function before
 *   displaying your data.
 *   
 * @param DataSetLoaderName
 *   Unique loader name. This same name must be used when calling
 *   TecUtilImportAddLoader().
 *   
 * @param Instructions
 *   Instructions used to load the current data set. If you are not calling
 *   TecUtilImportSetLoaderInstr(), then typically this would be the filename
 *   which was used to load your data.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilImportWriteLoaderInstr(
 *   &           DataSetLoaderName,
 *   &           InstructionsPtr)
 *    CHARACTER*(*)   DataSetLoaderName
 *    POINTER         (InstructionsPtr, Instructions)
 * </FortranSyntax>
 *   
 *   Set up an instruction containing a filename and write a $!READDATASET macro command to the current
 *   macro file:
 *   
 * @code
 *   StringList_pa Instructs = TecUtilStringListAlloc();
 *   TecUtilStringListAppendString(Instructs, "myfile.dat");
 *   TecUtilImportWriteLoaderInstr("BANANA", Instructs);
 *   TecUtilStringListDealloc(&Instructs);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilImportWriteLoaderInstr(const char   *DataSetLoaderName,
                                                        StringList_pa Instructions);
/**
 * @deprecated
 *   Please use TecUtilAddOnRegister() instead.
 */
LINKTOADDON void STDCALL TecUtilAddOnRegisterInfo(const char *OfficialName,
                                                  const char *Version,
                                                  const char *Author);


/**
 * Register information about your addon with Tecplot. This information will
 * mainly be used in an addon information dialog accessible via the Help menu
 * option.
 *
 * @par Note:
 *   This function must be called from within your addon initialization
 *   function, and cannot be called from anywhere else.
 *   
 * @param TecplotVersionNumber
 *   The TecUtilAPIVersion() use 100 for v10. This tells Tecplot that your
 *   addon was built "Tecplot version number" aware. For example, if you use
 *   100, then Tecplot will assume that your addon knows how to deal with
 *   shared variables since they were introduced in version 10.
 *   
 * @param OfficialName
 *   The official name of your addon.
 *   
 * @param Version
 *   A string indicating the version of your addon.
 *   
 * @param Author
 *   A string indicating the author of the addon (usually the company name).
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilAddOnRegister(
 *   &           TecplotVersionNumber,
 *   &           OfficialName,
 *   &           Version,
 *   &           Author,
 *   &           ResultPtr)
 *    INTEGER*4       TecplotVersionNumber
 *    CHARACTER*(*)   OfficialName
 *    CHARACTER*(*)   Version
 *    CHARACTER*(*)   Author
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *  *   
 * @code
 *   To register an addon called Circular Stream from Tecplot, Inc.:
 *   AddOn_pa AddOnID;
 *   void InitTecAddOn(void)
 *   {
 *     TecUtilLockOn();
 *     AddOnID=TecUtilAddOnRegister(100,"Circular Stream",
 *                              "1.0 - 05/01/1998",
 *                              "Tecplot, Inc.");
 *   // other initialization 
 *     TecUtilLockOff();
 *   }
 * @endcode
 */
LINKTOADDON AddOn_pa STDCALL TecUtilAddOnRegister(int         TecplotVersionNumber,
                                                  const char *OfficialName,
                                                  const char *Version,
                                                  const char *Author);
/**
 * Query Tecplot's list of loaded addons for the specified addon's version and
 * author strings.
 *   
 * @param OfficialName
 *   Official name of the addon. This is the same unique name with which an
 *   addon was registered.
 *   
 * @param Version
 *   A pointer to a character pointer. If the addon is loaded, *Version is
 *   assigned a copy of the addon's version string, otherwise a value of NULL
 *   is assigned. You must free this string with TecUtilStringDealloc().
 *   
 * @param Author
 *   A pointer to a character pointer. If the addon is loaded, *Author is
 *   assigned a copy of the addon's version string, otherwise a value of NULL
 *   is assigned. You must free this string with TecUtilStringDealloc().
 *   
 * @return
 *   Returns TRUE if the addon is loaded, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilAddOnGetRegisteredInfo(
 *   &                   OfficialName,
 *   &                   Version,
 *   &                   VersionLength,
 *   &                   Author,
 *   &                   AuthorLength)
 *    CHARACTER*(*)   OfficialName
 *    CHARACTER*(*)   Version
 *    INTEGER*4       VersionLength
 *    CHARACTER*(*)   Author
 *    INTEGER*4       AuthorLength
 * </FortranSyntax>
 *   
 *   To find out if the addon Circular Stream is loaded into Tecplot:
 *   
 * @code
 *   char *version = NULL;
 *   char *author = NULL;
 *   if (TecUtilAddOnGetRegisteredInfo("Circular Stream".&version, &author))
 *    {
 *    // Circular Stream loaded, do something special 
 *    DeallocString(&version);
 *    DeallocString(&author);
 *    }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilAddOnGetRegisteredInfo(const char *OfficialName,
                                                    char **Version,
                                                    char **Author);


#if defined COMPILE_BUT_HIDE_FROM_DOC
LINKTOADDON void STDCALL TecUtilAddOnRegForeignLibLoader(const char          *ForeignLibID, 
                                                         ForeignLibLoader_pf ForeignLibLoader, 
                                                         ArbParam_t          ClientData);
#endif


/**
 * Registers an extended curve fit addon. This will add an option to the single
 * selection list launched by the Curve Type/Extended option on the Curves page
 * of the Plot Attributes dialog.
 *   
 * @param CurveFitName
 *   Unique name given to the extended curve fit. This name is used in the list
 *   of extended curve fits in the Extended Curve Fits dialog, launched from
 *   Curve Type/Extended in the Plot Attributes dialog.
 *   
 * @param GetLinePlotDataPointsCallback
 *   The name of the function that will calculate the curve fit. This is the
 *   only function that needs to be defined to create an extended curve fit
 *   addon.
 *   
 * @param GetProbeValueCallback
 *   The name of the function that will return the dependent value when the
 *   extended curve fit is probed at a given independent value. If this
 *   function is set to NULL, Tecplot will perform a linear interpolation based
 *   on the values returned by the GetLinePlotDataPoints function.
 *   
 * @param GetCurveInfoStringCallback
 *   The name of the function that will create a string to be presented in the
 *   Data/LinePlot Curve Info dialog. This callback may be set to NULL if you
 *   do not wish to present a string to the LinePlot Curve Info dialog.
 *   
 * @param GetCurveSettingsCallback
 *   The name of the function that is called when the Curve Settings button on
 *   the Curves page of the Plot Attributes dialog is pressed while the
 *   extended curve fit is set as the Curve Type. This function may be set to
 *   NULL if there are not configurable settings for the extended curve fit. If
 *   settings are changed, it is the responsibility of the addon writer to
 *   inform Tecplot of the change by calling the function
 *   TecUtilCurveSetExtendedSettings(). This function is usually called when OK
 *   is clicked on the addon dialog.
 *   
 * @param GetAbbreviatedSettingsStringCallback
 *   See \ref GetAbbreviatedSettingsStringCallback_pf.
 *   
 * @return
 *   Returns TRUE if the extended curve fit was added.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilCurveRegisterExtCrvFit(
 *   &                   CurveFitName,
 *   &                   GetLinePlotDataPointsCallback,
 *   &                   GetProbeValueCallback,
 *   &                   GetCurveInfoStringCallback,
 *   &                   GetCurveSettingsCallback,
 *   &                   GetAbbreviatedSettingsStringCallback)
 *    CHARACTER*(*)   CurveFitName
 *    POINTER         (GetLinePlotDataPointsCallbackPtr, GetLinePlotDataPointsCallback)
 *    POINTER         (GetProbeValueCallbackPtr, GetProbeValueCallback)
 *    POINTER         (GetCurveInfoStringCallbackPtr, GetCurveInfoStringCallback)
 *    POINTER         (GetCurveSettingsCallbackPtr, GetCurveSettingsCallback)
 *    POINTER         (GetAbbreviatedSettingsStringCallbackPtr, GetAbbreviatedSettingsStringCallback)
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilCurveRegisterExtCrvFit(const char   *CurveFitName,
                                  GetLinePlotDataPointsCallback_pf        GetLinePlotDataPointsCallback,
                                  GetProbeValueCallback_pf                GetProbeValueCallback,
                                  GetCurveInfoStringCallback_pf           GetCurveInfoStringCallback,
                                  GetCurveSettingsCallback_pf             GetCurveSettingsCallback,                                  
                                  GetAbbreviatedSettingsStringCallback_pf GetAbbreviatedSettingsStringCallback);


/**
 *   Launch a dialog with an error message.
 *   
 * @param Message
 *   String containing the error message.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDialogErrMsg(Message)
 *    CHARACTER*(*) Message
 * </FortranSyntax>
 *   
 *   Display an error message:
 *   
 * @code
 *   TecUtilDialogErrMsg("File not found.");
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDialogErrMsg(const char *Message);



/**
 * Sets the properties of the variable so that it is shared between source and destination zones
 * (using the source for values). Both zones must have the same structure (both Ordered with the
 * same I,J, and K values; or both are finite-elements with the same element type and same number of
 * nodes.
 *   
 * @param SourceZone
 *   The zone number where the data values are based.
 *   
 * @param DestZone
 *   The zone number where the data values will be shared from the source zone.
 *   
 * @param Var
 *   The variable to be shared.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueShare(
 *   &           SourceZone,
 *   &           DestZone,
 *   &           Var)
 *    INTEGER*4       SourceZone
 *    INTEGER*4       DestZone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *   
 *   Set the first variable of zone 3 to be shared with zone 2:
 *   
 * @code
 *   TecUtilDataValueShare(2, 3, 1);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDataValueShare(EntIndex_t SourceZone,
                                               EntIndex_t DestZone,
                                               EntIndex_t Var);
/**
 * Copies the data from the source zone's variable to the destination zone. The
 * destination zone's variable must already be allocated or memory mapped and
 * it may not be shared. Both zones must have the same structure (both Ordered
 * with the same I,J, and K values; or both are finite-elements with the same
 * element type and same number of nodes.
 *   
 * @param SourceZone
 *   The zone number where the data values are based.
 *   
 * @param DestZone
 *   The zone number where the data values will be copied from the source zone.
 *   
 * @param Var
 *   The variable to be copied.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueCopy(
 *   &           SourceZone,
 *   &           DestZone,
 *   &           Var)
 *    INTEGER*4       SourceZone
 *    INTEGER*4       DestZone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *   
 * Copy the values from variable 1 of zone 2 to zone 3.
 *   
 * @code
 *   TecUtilDataValueCopy(2, 3, 1);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDataValueCopy(EntIndex_t SourceZone,
                                              EntIndex_t DestZone,
                                              EntIndex_t Var);

/**
 * Allocates the space needed for the variable. If the variable was previously
 * allocated or mapped it will be deallocated or unmaped before being
 * reallocated. Typically this function is used in conjunction with deferred
 * variable creation. See the SV_DEFERVARCREATION option for
 * TecUtilDataSetAddZoneX() and TecUtilDataSetAddVarX() for details.
 *
 * @since
 *   10.0-3-129
 *   
 * @param Zone
 *   The zone needing the variable allocated.
 *   
 * @param Var
 *   The variable to be allocated.
 *
 * @return
 *   TRUE if the variable was sucessfully allocated, FALSE otherwise.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueAlloc(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *   
 * Allocate the first variable of zone 3. Note that this example is only valid
 * if the zone was added with the deferred variable creation option set to
 * true.
 *   
 * @code
 *   IsOk = TecUtilDataValueAlloc(3, 1);
 * @endcode
 *
 * @sa TecUtilDataValueMemMapData()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueAlloc(EntIndex_t Zone,
                                                    EntIndex_t Var);

/**
 * Indicates if Tecplot can map the requested variable to a file. In order to
 * map a Tecplot variable to a file certain requirements must be met. Some of
 * them are checked by this function while others rely on Scout's honor.
 *
 * The following conditions are checked by this function and must be true
 * before a variable can be memory mapped to a file:
 *   - Memory mapped I/O must be enabled for Tecplot: $!FileConfig UseMemoryMappedIO = Yes
 *   - Offset has appropriate alignment for the mapping.
 *   - The number of bytes needed for the variable must be of an appropriate
 *     size to make efficient use of the memory map.
 *   - If the zone is ordered the variable must have a nodal value locatation.
 *   - Finite element data can be nodal or cell centered.
 *
 * The add-on must ensure the following condition:
 *   - The entire data for the variable must be continguous.
 *   - The file is at least open for reading.
 *   - The file remains unmodified while mapped.
 *
 * @param Zone
 *   Zone to containing the variable to map to the file.
 *
 * @param Var
 *   Variable to map to the file.
 *
 * @param Offset
 *   Offset of the start of the variable in the file.
 *
 * @param IsDataNativeByteOrder
 *   Indicates if the byte ordering of the data in the file maches the
 *   machine's native byte ordering.
 *
 * @return
 *   TRUE if the variable can be memory mapped to the file, FALSE otherwise.
 *
 * @sa TecUtilDataValueMemMapData()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueCanMemMapData(EntIndex_t     Zone,
                                                            EntIndex_t     Var,
                                                            MemMapOffset_t Offset,
                                                            Boolean_t      IsDataNativeByteOrder);
/**
 * Maps the Tecplot variable to the file. If the variable was previously mapped
 * or allocated it will be unmapped or deallocated before being mapped to the
 * new location. Typically this function is used in conjunction with deferred
 * variable creation. See the SV_DEFERVARCREATION option for
 * TecUtilDataSetAddZoneX() and TecUtilDataSetAddVarX() for details.
 *
 * @since
 *   10.0-3-129
 *
 * @par Note:
 *   A call to TecUtilDataValueCanMemMapData() must preceded the call to this
 *   function to determine if the variable can be memory mapped. The file must
 *   at least be open for reading at the time of this call but does not need to
 *   remain open after the variable is memory mapped.  However, it is the
 *   responsibility of the add-on to ensure that the file remains present and
 *   unaltered while Tecplot maintains a reference to this mapping as Tecplot
 *   uses the file as it's backing store for the variable's data.
 *
 * @par Note:
 *   Under Windows you must link with the release version of the C run-time library
 *   in order to use this function, even when building a debug version of your add-on.
 *   If you link with the standard debug run-time libraries, this function will fail
 *   or act erratically.
 *
 * @param Zone
 *   The zone needing the variable mapped to the file.
 *
 * @param Var
 *   Variable to map to the file.
 *
 * @param FileDescriptor
 *   File descriptor to an open file containing the variable to map. The file
 *   must at least be open for reading at the time of this call but does not
 *   need to remain open after the variable is memory mapped.
 *
 * @param Offset
 *   Absolute offset to the start of the variable in the file.
 *
 * @param IsDataNativeByteOrder
 *   Indicates if the byte ordering of the data in the file maches the
 *   machine's native byte ordering.
 *
 * @return
 *   TRUE if the mapping was sucessful, FALSE otherwise.
 *
 * @sa TecUtilDataValueCanMemMapData() and TecUtilDataValueAlloc()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueMemMapData(EntIndex_t     Zone,
                                                         EntIndex_t     Var,
                                                         int            FileDescriptor,
                                                         MemMapOffset_t Offset,
                                                         Boolean_t      IsDataNativeByteOrder);
/**
 *   Branch off a shared variable. The specified variable of the specified zone is branched so it is no
 *   longer shared with anything.
 *   
 * @param Zone
 *   Zone in which the shared variable is located.
 *   
 * @param Var
 *   Variable that will be branched
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataValueBranchShared(
 *   &                   Zone,
 *   &                   Var)
 *    INTEGER*4       Zone
 *    INTEGER*4       Var
 * </FortranSyntax>
 *   
 * Branch variable 2 in zone 1.
 *   
 * @code
 *   TecUtilDataValueBranchShared(1, 2);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataValueBranchShared(EntIndex_t Zone,
                                                           EntIndex_t Var);




/**
 * Sets the properties of the connectivity so that it is shared between source and destination zones
 * (using the source for values). Both zones must have the same structure (both Ordered with the
 * same I,J, and K values; or both are finite-elements with the same element type and same number of
 * nodes).  Both zones must also have the same local face neighbor mode.
 *   
 * @param SourceZone
 *   The zone number where the connectivity is based.
 *   
 * @param DestZone
 *   The zone number where the connectivity will be shared from the source zone.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataConnectShare(
 *   &           SourceZone,
 *   &           DestZone)
 *    INTEGER*4       SourceZone
 *    INTEGER*4       DestZone
 * </FortranSyntax>
 *   
 *   Set the connectivity in zone 3 to be shared with zone 2:
 *   
 * @code
 *   TecUtilDataConnectShare(2, 3);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDataConnectShare(EntIndex_t SourceZone,
                                                 EntIndex_t DestZone);






/**
 * Branch the connectivity information. Returns False if out of memory.
 *   
 * @param Zone
 *   Zone number where connectivity is to be branched.
 *   
 * @return
 *   TRUE if connectivity is branched, FALSE if out of memory.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataConnectBranchShared(Zone)
 *    INTEGER*4 Zone
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataConnectBranchShared(EntIndex_t Zone);




/**
 * Copies the specified number of values from the base of the source value
 * array to the destination field data starting at the specified offset. Note
 * that the source value array must be of the same data type as the destination
 * field data.
 *
 * @since
 *     10.0-3-12
 *
 * @param DestFieldData
 *     Field data to receive the source values.
 * @param DestOffset
 *     Member offset in the destination field data to begin assigning values.
 * @param DestCount
 *     Number of values to assign to the destination field data.
 * @param SourceValueArray
 *     An array containing the members to copy. The first member is assumed to
 *     be at the base of the array.
 */
LINKTOADDON void STDCALL TecUtilDataValueArraySetByRef(FieldData_pa  DestFieldData,
                                                       LgIndex_t     DestOffset,
                                                       LgIndex_t     DestCount,
                                                       void         *SourceValueArray);
/**
 * Assign a value to a field variable at a specific position. If the zone
 * referenced is IJ- or IJK-ordered, the position is calculated by treating the
 * two- or three-dimensional array as a one-dimensional array. Be sure to call
 * TecUtilStateChanged() after changing field data in this way.
 *   
 * @param FD
 *   Handle to the field data. Use TecUtilDataValueGetRef() or
 *   TecUtilZoneGetInfo() to get handles to field data
 *   
 * @param PointIndex
 *   Position in the array of field data values. Position starts at one. For
 *   cell centered variables in ordered zones, the array includes values for
 *   IMax, JMax and KMax, even though these values are not used. You must
 *   account for these "ghost" cells in calculating the PointIndex. The formula
 *   for PointIndex in terms of I,J, and K is the same for both Nodal and cell
 *   centered variables. PointIndex = I + (J-1)*IMax + (K-1)*IMax*JMax;
 *   
 * @param Value
 *   New value for the position in the field data
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataValueSetByRef(
 *   &           FDPtr,
 *   &           PointIndex,
 *   &           Value)
 *    POINTER         (FDPtr, FD)
 *    INTEGER*4       PointIndex
 *    REAL*8          Value
 * </FortranSyntax>
 *   
 * Set the first two values of the second variable of zone 5 to be 1.25 and
 * 1.35 respectively:
 *   
 * @code
 *   Set_pa altered_vars;
 *   FieldData_pa fd;
 *   fd = TecUtilDataValueGetRef(5, 2);
 *   if ( fd )
 *     {
 *       TecUtilDataValueSetByRef(fd, 1, 1.25);
 *       TecUtilDataValueSetByRef(fd, 2, 1.35);
 *   
 *       // inform Tecplot of var value change 
 *   
 *       altered_vars = TecUtilSetAlloc(TRUE);
 *   
 *       TecUtilSetAddMember(altered_vars, var, TRUE);
 *       TecUtilStateChanged(StateChange_VarsAltered,
 *                           (ArbParam_t)altered_vars);
 *       TecUtilSetDealloc(&altered_vars);
 *     }
 * @endcode
 *
 * @sa TecUtilDataValueRefGetSetFunc() for obtaining a function as a high
 *     performance alternative.
 */
LINKTOADDON void STDCALL TecUtilDataValueSetByRef(FieldData_pa FD,
                                                  LgIndex_t    PointIndex,
                                                  double       Value);

/**
 * Set the node index for a particular corner of a finite-element. This
 * function does not require you to obtain the handle to the node map as does
 * TecUtilDataNodeSetByRef(), however, this function is not very efficient. Use
 * TecUtilDataNodeSetByRef() if you are setting multiple nodes for the same
 * zone. You do not need to call TecUtilStateChanged() after calling this
 * function as Tecplot does that for you.
 *   
 * @param Zone
 *   Zone number.
 *   
 * @param Element
 *   The element number (starts at 1).
 *   
 * @param Corner
 *   The element corner (starts at 1).
 *   
 * @param Node
 *   The new node index for that element at that corner.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataNodeSetByZone(
 *   &           Zone,
 *   &           Element,
 *   &           Corner,
 *   &           Node)
 *    INTEGER*4       Zone
 *    INTEGER*4       Element
 *    INTEGER*4       Corner
 *    INTEGER*4       Node
 * </FortranSyntax>
 *   
 *   Set the third node of the 43rd element of zone 5 to be 129:
 *   
 * @code
 *   TecUtilDataNodeSetByRef(5, 43, 3, 129);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDataNodeSetByZone(EntIndex_t Zone,
                                                       LgIndex_t  Element,
                                                       LgIndex_t  Corner,
                                                       LgIndex_t  Node);

/**
 * Set the node index for a particular corner of a finite-element. To use this
 * function you must have already obtained the handle to the node map. Be sure
 * to call TecUtilStateChanged() after using this function.
 *   
 * @param NM
 *   Handle to the connectiivty list (that is, the node map). Use
 *   TecUtilDataNodeGetRef() or TecUtilZoneGetInfo() to get a handle to the
 *   node map.
 *   
 * @param Element
 *   The element number (starts at 1)
 *   
 * @param Corner
 *   The element corner (starts at 1).
 *   
 * @param Node
 *   The new node index for that element at that corner
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDataNodeSetByRef(
 *   &           NMPtr,
 *   &           Element,
 *   &           Corner,
 *   &           Node)
 *    POINTER         (NMPtr, NM)
 *    INTEGER*4       Element
 *    INTEGER*4       Corner
 *    INTEGER*4       Node
 * </FortranSyntax>
 *   
 * Set the first two nodes of the 43rd element of zone 5 to be 127 and 128
 * respectively:
 *   
 * @code
 *   Set_pa altered_zones;
 *   NodeMap_pa nm;
 *   nm = TecUtilDataNodeGetRef(5);
 *   if ( nm )
 *     {
 *       TecUtilDataNodeSetByRef(nm, 43, 1, 127);
 *       TecUtilDataNodeSetByRef(nm, 43, 2, 128);
 *       // inform Tecplot of node map change 
 *       altered_zones = TecUtilSetAlloc(TRUE);
 *       TecUtilSetAddMember(altered_zones, 5, TRUE);
 *       TecUtilStateChanged(StateChange_NodeMapsAltered,
 *                           (ArbParam_t)altered_zones);
 *       TecUtilSetDealloc(&altered_zones);
 *   }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilDataNodeSetByRef(NodeMap_pa  NM,
                                             LgIndex_t   Element,
                                             LgIndex_t   Corner,
                                             LgIndex_t   Node);

/**
 * Lock Tecplot. For every call to TecUtilLockOn(), you must have a matching
 * call to TecUtilLockOff(). However, both these functions have been replaced
 * by and TecUtilLockFinish(). The only time in which these depricated
 * functions should be used is in the InitTecAddon function when an AddonID has
 * not yet been created.  See Chapter 10, "Locking and Unlocking Tecplot," in
 * the ADK User's Manual for more information on locks in Tecplot.
 *
 * @sa TecUtilLockStart()
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLockOn()
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL TecUtilLockOn(void);




/**
 * Lock Tecplot. For every call to TecUtilLockOff(), you must have a matching
 * call to TecUtilLockOn().  However, both these functions have been replaced
 * by TecUtilLockStart() and TecUtilLockFinish(). The only time in which these
 * depricated functions should be used is in the InitTecAddon function when an
 * AddonID has not yet been created. See Chapter 10, "Locking and Unlocking
 * Tecplot," in the ADK User's Manual for more information on locks in Tecplot.
 *
 * @sa TecUtilLockFinish()
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLockOff()
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL TecUtilLockOff(void);



/**
 * Lock Tecplot. For every call to TecUtilLockStart(), you must have a matching
 * call to TecUtilLockFinish(). See Chapter 10, "Locking and Unlocking
 * Tecplot," in the ADK User's Manual for more information on locks in Tecplot.
 *   
 * @param AddOn
 *   The addon id from which the function is called
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLockStart(AddOnPtr)
 *    POINTER (AddOnPtr, AddOn)
 * </FortranSyntax>
 */
LINKTOADDON void   STDCALL TecUtilLockStart(AddOn_pa AddOn);
/**
 * Unlock Tecplot. Call only after you have first called TecUtilLockStart().
 * See Chapter 10, "Locking and Unlocking Tecplot," in the ADK User's Manual
 * for more information on locks in Tecplot.
 *   
 * @param AddOn
 *   The addon id from which the function is called
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLockFinish(AddOnPtr)
 *    POINTER (AddOnPtr, AddOn)
 * </FortranSyntax>
 */
LINKTOADDON void   STDCALL TecUtilLockFinish(AddOn_pa AddOn);
/**
 * Queries for and returns the name of the object currently locking Tecplot.
 *   
 * @return
 *   Character string containing the name of the lock owner. You must free this
 *   string using TecUtilStringDealloc() when finished.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilLockGetCurrentOwnerName(
 *   &           Result,
 *   &           ResultLength)
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *   
 *   char *Name = NULL;
 *   
 * @code
 *   name = TecUtilLockGetCurrentOwnerName(void);
 *   if (Name)
 *     {
 *      TecUtilStringDealloc(&Name);
 *     }
 * @endcode
 */
LINKTOADDON char * STDCALL TecUtilLockGetCurrentOwnerName(void);

/**
 *   Start monitoring the percent an operation is done.
 *   
 * @param PercentDoneText
 *   Text string to display as operation is done
 *   
 * @param ShowStopButton
 *   TRUE to show button, FALSE not to.
 *   
 * @param ShowProgressBar
 *   TRUE to show progress bar, FALSE not to
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStatusStartPercentDone(
 *   &           PercentDoneText,
 *   &           ShowStopButton,
 *   &           ShowProgressBar)
 *    CHARACTER*(*)   PercentDoneText
 *    INTEGER*4       ShowStopButton
 *    INTEGER*4       ShowProgressBar
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL      TecUtilStatusStartPercentDone(const char *PercentDoneText,
                                                            Boolean_t   ShowStopButton,
                                                            Boolean_t   ShowProgressBar);
/**
 *   Sets string to be displayed when operation completed.
 *   
 * @param PercentDoneText
 *   String of texts to be displayed as operation reaches completion.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStatusSetPercentDoneText(PercentDoneText)
 *    CHARACTER*(*) PercentDoneText
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL      TecUtilStatusSetPercentDoneText(const char *PercentDoneText);
/**
 *   Checks percent of current operation is completed.
 *   
 * @param PercentDone
 *   Int value for when the percent of completion of an operation is to be checked
 *   
 * @return
 *   TRUE if successful, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStatusCheckPercentDone(PercentDone)
 *    INTEGER*4 PercentDone
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilStatusCheckPercentDone(int PercentDone);
/**
 *   Checks when current operation is completed.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStatusFinishPercentDone()
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL      TecUtilStatusFinishPercentDone(void);

/**
 *   Launch the Percent Done dialog.
 *   
 * @param Label
 *   Text to describe the action about to be performed.
 *   
 * @param ShowTheScale
 *   Set to TRUE if you want the scale to be used, otherwise set to FALSE
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDialogLaunchPercentDone(
 *   &           Label,
 *   &           ShowTheScale)
 *    CHARACTER*(*)   Label
 *    INTEGER*4       ShowTheScale
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL      TecUtilDialogLaunchPercentDone(const char     *Label,
                                                         Boolean_t ShowTheScale);
/**
 *   Update the text in the Percent Done dialog.
 *   
 * @param Text
 *   Text to display in the percent done dialog.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDialogSetPercentDoneText(Text)
 *    CHARACTER*(*) Text
 * </FortranSyntax>
 *   
 *   Update the text in the Percent Done dialog to say "Phase II."
 *   
 * @code
 *   TecUtilDialogSetPercentDoneText("Phase II");
 * @endcode
 */
LINKTOADDON void STDCALL      TecUtilDialogSetPercentDoneText(const char *Text);
/**
 * Set the current value of the Percent Done dialog and check to see if the
 * user has clicked Cancel.
 *   
 * @par Note:
 *   This function cannot be called when Tecplot is running in batch mode.
 *   
 * @param PercentDone
 *   Value to which Percent Done dialog is to be set. If the
 *   TecUtilDialogLaunchPercentDone() call had ShowTheScale set to FALSE, then
 *   this parameter is ignored.
 *   
 * @return
 *   Returns TRUE if the user has not clicked Cancel (that is, it is OK to
 *   continue processing).  Returns FALSE if Cancel has been clicked.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDialogCheckPercentDone(PercentDone)
 *    INTEGER*4 PercentDone
 * </FortranSyntax>
 *   
 *   Launch, check, and destroy Percent Done dialog.
 *   
 * @code
 *   TecUtilDialogLaunchPercentDone("Calculate",TRUE);
 *   // do some processing 
 *   if (!TecUtilDialogCheckPercentDone(35))
 *     {
 *       // user pressed cancel button 
 *     }
 *   else
 *     {
 *       // do some more processing 
 *     }
 *   // finished processing 
 *   TecUtilDialogDropPercentDone();
 * @endcode
 *
 * @sa TecUtilMacroIsBatchModeActive()
 */
LINKTOADDON Boolean_t STDCALL TecUtilDialogCheckPercentDone(int PercentDone);
/**
 *   Drop the Percent Done dialog.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDialogDropPercentDone()
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL      TecUtilDialogDropPercentDone(void);

/**
 *   Instruct Tecplot to execute a single macro command. The macro command is supplied as a string.
 *   Currently this command is restricted as follows:
· Only commands that do not require raw data are
 *   accepted.
· Command must be all on one line-no newlines.
See the Tecplot Reference Manual for
 *   details about Tecplot's macro language.
 *   
 * @param Command
 *   Macro command. This must not be NULL
 *   
 * @return
 *   TRUE if Command executed successfully, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroExecuteCommand(Command)
 *    CHARACTER*(*) Command
 * </FortranSyntax>
 *   
 *   Execute a macro command to animate the I-planes:
 *   
 * @code
 *   TecUtilMacroExecuteCommand("$!ANIMATEIJKPLANES PLANES = I");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroExecuteCommand(const char *Command);
/**
 *   Set the value for a macro variable. Any macro executed after this call may then reference the
 *   value using |macrovar|.
 *   
 * @param MacroVar
 *   Name of the macro variable you want to assign a value.
 *   
 * @param ValueString
 *   Value to assign to MacroVar. Must be a valid string of length greater than zero
 *   
 * @return
 *   TRUE if the MacroVar is a valid variable name and memory could be allocated to store ValueString,
 *   FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilMacroSetMacroVar(
 *   &                   MacroVar,
 *   &                   ValueString)
 *    CHARACTER*(*)   MacroVar
 *    CHARACTER*(*)   ValueString
 * </FortranSyntax>
 *   
 *   Assign a file name to the macro variable FNAME, then use it in a Tecplot macro:
 *   
 * @code
 *   IsOk = TecUtilMacroSetMacroVar("FName","/home/george/test.dat");
 *   ....
 *   In a later macro you can reference |FName|:
 *   $!ReadDataSet "|FName|"
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilMacroSetMacroVar(const char *MacroVar,
                                                      const char *ValueString);

/**
 *   Interrupt Tecplot execution. This is mainly for use with addons which use timers.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilInterrupt()
 * </FortranSyntax>
 *   
 *   The function NewDataIsReady is assumed to be called when a timer goes off and some new data is
 *   ready to be read into Tecplot. OldDataIsNotDoneDrawingYet is a boolean which is TRUE if the
 *   previous data is still being drawn in Tecplot's workspace. Interrupt Tecplot if the new data is
 *   ready before the old data is finished redrawing.
 *   
 * @code
 *   extern Boolean_t OldDataIsNotDoneDrawingYet;
 *   void NewDataIsReady (void)
 *   {
 *     if (OldDataIsNotDoneDrawingYet)
 *       TecUtilInterrupt();
 *     //  Load the new data into Tecplot and redraw  
 *   }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilInterrupt(void);


/**
 *   Deletes the specified geometry object.
 *   
 * @param GID
 *   Handle to a geometry object
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomDelete(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 *   
 *   Delete the first geometry object from the list of geometry objects maintained by the current
 *   frame.
 *   
 * @code
 *   Geom_ID Geom;
 *   Geom = TecUtilGeomGetBase();
 *   if (Geom != TECUTILBADID)
 *     {
 *       TecUtilGeomDelete(Geom);
 *     }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeomDelete(Geom_ID GID);
/**
 *   Deletes the specified text object.
 *   
 * @param TID
 *   Handle to a text object
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextDelete(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 *   
 *   Delete the first text object from the list of text objects maintained by the current frame.
 *   
 * @code
 *   Text_ID Text;
 *   
 *   Text = TecUtilTextGetBase();
 *   if (Text != TECUTILBADID)
 *     {
 *       TecUtilTextDelete(Text);
 *     }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilTextDelete(Text_ID TID);

/**
 *   Add the specified geometry to the pick list. See Section 17.4, "The Pick List," in the ADK User's
 *   Manual for a discussion of pick lists.
 *   
 * @param GID
 *   Geometry ID to pick
 *   
 * @return
 *   TRUE if successful. A return value of FALSE usually indicates that Tecplot's limit on the number
 *   of picked objects has been exceeded.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickGeom(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 *   
 *   Pick the first geometry in the current frame:
 *   
 * @code
 *   Geom_ID gid;
 *   gid = TecUtilGeomGetBase();
 *   if (gid ! = NULL)
 *      TecUtilPickGeom(gid);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickGeom(Geom_ID GID);
/**
 *   Add the specified text to the pick list. See the ADK User's Manual for a discussion of pick lists.
 *   
 * @param TID
 *   Text ID to pick
 *   
 * @return
 *   TRUE if successful. A return value of FALSE usually indicates that Tecplot's limit on the number
 *   of picked objects has been exceeded.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilPickText(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 *   
 *   Pick the first text in the current frame.
 *   
 * @code
 *   Text_ID tid;
 *   tid = TecUtilTextGetBase();
 *   if (tid ! = NULL)
 *      TecUtilPickText(tid);
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilPickText(Text_ID TID);

/**
 *   Validate a geometry ID.
 *   
 * @param GID
 *   Geometry ID.
 *   
 * @return
 *   TRUE if GID is a valid geometry ID. FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomIsValid(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 *   
 *   Determine if a geometry ID is still valid, and if it is valid, change its color to red:
 *   
 * @code
 *   extern Geom_ID g; // created elsewhere 
 *   if ( TecUtilGeomIsValid(g) )
 *      TecUtilGeomSetColor(g, Red_C);
 * @endcode
 */
LINKTOADDON Boolean_t  STDCALL TecUtilGeomIsValid(Geom_ID GID);
/**
 *   Determine if the text object is valid in the current frame context.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   TRUE if TID is a valid text object, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextIsValid(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t  STDCALL TecUtilTextIsValid(Text_ID TID);
/**
 *   Convert a text string using the old formatting syntax into the new formatting syntax.
 *   
 * @param OldString
 *   Character string containing old formatting syntax.
 *   
 * @param BaseFont
 *   Assumed base font used by the old string.
 *   
 * @return
 *   Returns the converted character string. You must free this string when you are finished with it.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringConvOldFormatting(
 *   &           OldString,
 *   &           BaseFont,
 *   &           Result,
 *   &           ResultLength)
 *    CHARACTER*(*)   OldString
 *    INTEGER*4       BaseFont
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 */
LINKTOADDON char * STDCALL TecUtilStringConvOldFormatting(const char *OldString,
                                                          Font_e      BaseFont);
/**
 *   Allocate a character string. Use TecUtilStringDealloc() to deallocate
 *   strings allocated using TecUtilStringAlloc(). For the Tecplot ADK ActiveX
 *   Component, this function is not currently supported and is never required
 *   to be used.
 *   
 * @param MaxLength
 *   The usable length of the string. The size must be greater than or equal to
 *   zero
 *   
 * @param DebugInfo
 *   Character string identifying the reason why the string is being allocated.
 *   This parameter is not yet enabled, but you still must supply a string
 *   
 * @return
 *   Returns the address of the string or NULL if the memory cannot be allocated.
 *   
 *   The following example will allocate a string for displaying an error
 *   message and then deallocate it.
 *   
 * @code
 *   char *S;
 *   S = TecUtilStringAlloc(80,"error message string");
 *   sprintf(S, "The error occurred on step %d", IStep);
 *   TecUtilDialogErrMsg(S);
 *   TecUtilStringDealloc(&S);
 * @endcode
 */
LINKTOADDON char * STDCALL TecUtilStringAlloc(int         MaxLength, /* <-activex> */
                                              const char *DebugInfo);
/**
 * Free a string previously allocated with TecUtilStringAlloc(), or one that
 * was allocated and returned as the result of calling any other TecUtilXxx
 * function. For the Tecplot ADK ActiveX Component, this function is not
 * currently supported and is never required to be used.
 *   
 * @param S
 *   Reference to a valid string handle. Use TecUtilStringAlloc() to create a string
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringDealloc(
 *   &           S,
 *   &           SLength)
 *    CHARACTER*(*)   S
 *    INTEGER*4       SLength
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL TecUtilStringDealloc(char **S); /* <-activex> */


/* STRING LIST FUNCTIONS */
/**
 * Remove all members of the string list. See the Chapter "Using String Lists,"
 * in the ADK User's Manual for a discussion of string lists.
 *   
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a
 *   string list
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListClear(StringListPtr)
 *    POINTER (StringListPtr, StringList)
 * </FortranSyntax>
 *   
 *   Clear a string list so that it no longer maintains any strings items.
 *   
 * @code
 *   Boolean_t     ClearNames = FALSE;
 *   StringList_pa Names = NULL;
 *   
 *   // do some processing to get names 
 *     .
 *     .
 *     .
 *   
 *   if (ClearNames)
 *     {
 *       TecUtilStringListClear(Names);
 *       TecUtilDialogMessageBox("All names cleared.",
 *                               MessageBox_Information);
 *     }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilStringListClear(StringList_pa StringList);

/**
 * Remove the specified number of strings beginning at the nth string. The
 * members following the items removed are shifted to fill the vacated space.
 * See the Chapter "Using String Lists," in the ADK User's Manual for a
 * discussion of string lists.
 *   
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a
 *   string list
 *   
 * @param StringNumber
 *   Start position in the string list. Value must be greater than or equal to
 *   one, and less than or equal to the number of items maintained by the
 *   string list. Use TecUtilStringListGetCount() to get the number of strings in
 *   the string list
 *   
 * @param Count
 *   Number of items to remove from the string list. Value must be greater than
 *   or equal to one, and less than or equal to the number of items remaining,
 *   including the string at the start position.  Use TecUtilStringListGetCount()
 *   to get the number of strings in the string list
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListRemoveStrings(
 *   &           StringListPtr,
 *   &           StringNumber,
 *   &           Count)
 *    POINTER         (StringListPtr, StringList)
 *    INTEGER*4       StringNumber
 *    INTEGER*4       Count
 * </FortranSyntax>
 *   
 *   Remove all but the first and last item from a name list.
 *   
 * @code
 *   LgIndex_t     Count = 0;
 *   StringList_pa Names = NULL;
 *   
 *   // do some processing to get names 
 *     .
 *     .
 *     .
 *   
 *   Count = TecUtilStringListGetCount(Names);
 *   TecUtilStringListRemoveStrings(Names, 2, Count-2);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilStringListRemoveStrings(StringList_pa StringList,
                                                        LgIndex_t     StringNumber,
                                                        LgIndex_t     Count);
/**
 * Remove the nth string from the string list. The members following the
 * removed item are shifted to fill the vacated space. See the Chapter "Using
 * String Lists," in the ADK User's Manual for a discussion of string lists.
 *   
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a
 *   string list.
 *   
 * @param StringNumber
 *   Number of the string to remove. Must be greater than or equal to one, and
 *   less than or equal to the number of items maintained by the string list.
 *   Use TecUtilStringListGetCount() to get the number of items
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListRemoveString(
 *   &           StringListPtr,
 *   &           StringNumber)
 *    POINTER         (StringListPtr, StringList)
 *    INTEGER*4       StringNumber
 * </FortranSyntax>
 *   
 *   Remove the first name from a name list.
 *   
 * @code
 *   StringList_pa Names = NULL;
 *   
 *   // do some processing to get names 
 *     .
 *     .
 *     .
 *   
 *   TecUtilStringListRemoveString(Names, 1);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilStringListRemoveString(StringList_pa StringList,
                                                       LgIndex_t     StringNumber);
/**
 * Deallocate the string list members and handle, and set the handle to NULL.
 * See the Chapter "Using String Lists," in the ADK User's Manual for a
 * discussion of string lists.
 *   
 * @param StringList
 *   Reference to a valid string list handle. Use TecUtilStringListAlloc() to create a string list
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListDealloc(StringListPtr)
 *    POINTER (StringListPtr, StringList)
 * </FortranSyntax>
 *   
 *   Create and then deallocate a string list:
 *   
 * @code
 *   StringList_pa MyStrList = TecUtilStringListAlloc();
 *   .
 *   .
 *   .
 *   TecUtilStringListDealloc(&MyStrList);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilStringListDealloc(StringList_pa *StringList);

/**
 * Create an empty string list. See the Chapter "Using String Lists," in the
 * ADK User's Manual for a discussion of string lists. Use
 * TecUtilStringListDealloc() to deallocate the string list when it is no longer
 * needed.
 *   
 * @return
 *   Handle to an empty string list. A handle of NULL is returned if sufficient memory is not
 *   available.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListAlloc(ResultPtr)
 *    POINTER (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Allocate and deallocate a string list.
 *   
 * @code
 *   StringList_pa Names = NULL;
 *   
 *   Names = TecUtilStringListAlloc();
 *   if (Names != NULL)
 *     {
 *       // do something with the name list, append, clear, etc 
 *         .
 *         .
 *         .
 *   
 *       // get rid of the name list 
 *       TecUtilStringListDealloc(&Names);
 *     }
 * @endcode
 */
LINKTOADDON StringList_pa STDCALL TecUtilStringListAlloc(void);

/**
 * Append a copy of the string to the string list. The string list expands to
 * accommodate the additional item. See the Chapter "Using String Lists," in
 * the ADK User's Manual for a discussion of string lists.
 *   
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a string list.
 *   
 * @param String
 *   A copy of String is appended to the string list. String may be NULL
 *   
 * @return
 *   A return value of TRUE indicates the operation was successful. A return
 *   value of FALSE indicates that sufficient memory was not available for the
 *   additional item.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStringListAppendString(
 *   &                   StringListPtr,
 *   &                   String)
 *    POINTER         (StringListPtr, StringList)
 *    CHARACTER*(*)   String
 * </FortranSyntax>
 *   
 *   Append two variable names to a string list
 *   
 * @code
 *   Boolean_t     IsOk = FALSE;
 *   StringList_pa Names = NULL;
 *   
 *   Names = TecUtilStringListAlloc();
 *   if (Names != NULL)
 *     {
 *       IsOk = TecUtilStringListAppendString(Names, "X");
 *       IsOk = TecUtilStringListAppendString(Names, "Y");
 *       if (IsOk)
 *         {
 *           // do some processing with the name list 
 *             .
 *             .
 *             .
 *         }
 *   
 *       // get rid of the name list 
 *       TecUtilStringListDealloc(&Names);
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStringListAppendString(StringList_pa StringList,
                                                            const char    *String);
/**
 * Count the number of strings currently maintained by the string list. See the
 * Chapter "Using String Lists," in the ADK User's Manual for a discussion of
 * string lists.
 *   
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to create a string list.
 *   
 * @return
 *   The number of strings maintained by the string list.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStringListGetCount(StringListPtr)
 *    POINTER (StringListPtr, StringList)
 * </FortranSyntax>
 *   
 *   Get each instruction used to load the current frame's data set:
 *   
 * @code
 *   StringList_pa LoaderInstructs;
 *   char *LoaderName = NULL;
 *   if (TecUtilImportGetLoaderInstr(&LoaderName,&LoaderInstructs))
 *     {
 *      LgIndex_t ii, Count =
 *        TecUtilStringListGetCount(LoaderInstructs);
 *      for (ii = 1; ii <= Count; ii++)
 *       {
 *        char *Instruct =
 *          TecUtilStringListGetString(LoaderInstructs, ii);
 *        // Do some processing 
 *        .
 *        .
 *        .
 *        TecUtilStringDealloc(&Instruct);
 *       }
 *     }
 * @endcode
 */
LINKTOADDON LgIndex_t STDCALL TecUtilStringListGetCount(StringList_pa StringList);

/**
 * Return a reference to the nth string in a string list. See the Chapter
 * "Using String Lists", in the ADK User's Manual for a discussion of string
 * lists.
 *   
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a string list
 *   
 * @param StringNumber
 *   Position of string to be copied into the string list. StringNumber must be greater than or equal
 *   to one, and less than or equal to the number of items maintained by the string list. Use
 *   TecUtilStringListGetCount() to get the number of items.
 *   
 * @return
 *   Returns a REFERENCE to the string. DO OT DEALLOCATE THIS REFERENCE.
 *   
 * Operate on the set of files retrieved using TecUtilDialogGetFileNames().
 * @code
 *   StringList_pa FileNames = NULL;
 *   
 *   if (TecUtilDialogGetFileNames(SelectFileOption_ReadMultiFile,
 *                                 &FileNames,
 *                                 "any file",
 *                                 (StringList_pa)NULL,
 *                                 "*"))
 *     {
 *       LgIndex_t N,NumFiles;
 *   
 *       NumFiles = TecUtilStringListGetCount(FileNames);
 *       for (N = 1; N < Numfiles; N++)
 *         {
 *           const char *RawFNamePtr = TecUtilStringListGetRawStringPtr(FileNames,N);
 *   
 *           //
 *           // Do something with RawFNamePtr.  DO NOT DEALLOCATE RawFNamePtr.
 *           //
 *         }
 *   
 *       //
 *       // We do however dealloc the stringlist itself.
 *       //
 *   
 *       TecUtilStringListDealloc(&FileNames);
 *     }
 * @endcode
 */
LINKTOADDON const char * STDCALL TecUtilStringListGetRawStringPtr(StringList_pa StringList,
                                                                  LgIndex_t     StringNumber);
/**
 * Return a copy of the nth string from a string list. See the Chapter "Using
 * String Lists," in the ADK User's Manual for a discussion of string lists.
 *
 * @par Note: 
 *   The caller is responsible for de-allocating the copy of the string when it
 *   is no longer needed.
 *   
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a string list
 *   
 * @param StringNumber
 *   Position of string to be copied into the string list. StringNumber must be
 *   greater than or equal to one, and less than or equal to the number of
 *   items maintained by the string list. Use TecUtilStringListGetCount() to get
 *   the number of items
 *   
 * @return
 *   Copy of the nth string.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListGetString(
 *   &           StringListPtr,
 *   &           StringNumber,
 *   &           Result,
 *   &           ResultLength)
 *    POINTER         (StringListPtr, StringList)
 *    INTEGER*4       StringNumber
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 */
LINKTOADDON char * STDCALL TecUtilStringListGetString(StringList_pa StringList,
                                                      LgIndex_t     StringNumber);
/**
 * Place a copy of the specified string at the nth position in the string list.
 * If the position is beyond the end of the string list, the string list is
 * resized, so that the string references between the last item of the string
 * list in its original state and the last item of the string list in its new
 * state are assigned NULL. If the position is within the boundaries of the
 * original string list, the string at the specified position is replaced by
 * the new value. See the Chapter "Using String Lists," in the ADK User's
 * Manual for a discussion of string lists.
 *   
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a
 *   string list
 *   
 * @param StringNumber
 *   Item position in the string list. Value must be greater than or equal to one
 *   
 * @param String
 *   A copy of String is appended to the string list. String may be NULL
 *   
 * @return
 *   A return value of TRUE indicates the operation was successful. FALSE
 *   indicates that sufficient memory was not available for the additional item
 *   at the specified position.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStringListSetString(
 *   &                   StringListPtr,
 *   &                   StringNumber,
 *   &                   String)
 *    POINTER         (StringListPtr, StringList)
 *    INTEGER*4       StringNumber
 *    CHARACTER*(*)   String
 * </FortranSyntax>
 *   
 *   Replace the first item of a name list with a new value and put a new item ten positions past the
 *   current last item.
 *   
 * @code
 *   LgIndex_t     Count = 0;
 *   StringList_pa Names = NULL;
 *   
 *   // do some processing to get names 
 *     .
 *     .
 *     .
 *   
 *   IsOk = TecUtilStringListSetString(Names, 1,
 *                                     "New First Name");
 *   Count = TecUtilStringListGetCount(Names);
 *   IsOk = TecUtilStringListSetString(Names, Count+10,
 *                                     "New Last Name");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStringListSetString(StringList_pa StringList,
                                                         LgIndex_t     StringNumber,
                                                         const char    *String);
/**
 * Insert a copy of the string into the nth position of the string list. The
 * string list expands and the items are shifted to accommodate the additional
 * item. See the Chapter "Using String Lists," in the ADK User's Manual for a
 * discussion of string lists.
 *   
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a string list
 *   
 * @param StringNumber
 *   Position where string is inserted in the string list. This value must be
 *   greater than or equal to one, and less than or equal to the number of
 *   items maintained by the string list. Use TecUtilStringListGetCount() to get
 *   the number of items
 *   
 * @param String
 *   A copy of String is inserted into the the the string list. String may be NULL
 *   
 * @return
 *   A return value of TRUE indicates the operation was successful. FALSE indicates that the memory
 *   available was not sufficient for the additional item.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStringListInsertString(
 *   &                   StringListPtr,
 *   &                   StringNumber,
 *   &                   String)
 *    POINTER         (StringListPtr, StringList)
 *    INTEGER*4       StringNumber
 *    CHARACTER*(*)   String
 * </FortranSyntax>
 *   
 *   Insert a string at the beginning and end of an existing list.
 *   
 * @code
 *   Boolean_t     IsOk = FALSE;
 *   StringList_pa Names = NULL;
 *   LgIndex_t     Count = 0;
 *   
 *   // do some processing to get names 
 *     .
 *     .
 *     .
 *   
 *   // insert a name at the beginning and end of the list 
 *   IsOk = TecUtilStringListInsertString(Names, 1,
 *                                        "Very First Name");
 *   Count = TecUtilStringListGetCount(Names);
 *   IsOk = TecUtilStringListInsertString(Names, Count+1,
 *                                        "Very Last Name");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStringListInsertString(StringList_pa StringList,
                                                            LgIndex_t     StringNumber,
                                                            const char    *String);
/**
 * Return a handle to a duplicate of the specified string list and its
 * contents. See the Chapter "Using String Lists," in the ADK User's Manual for
 * a discussion of string lists.
 *
 * @par Note: 
 *   The caller is responsible for deallocating the string list when it is no
 *   longer needed.
 *   
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to create a string list.
 *   
 * @return
 *   A handle to a duplicate string list is returned if the operation was
 *   successful. A handle of NULL is returned if sufficient memory is not
 *   available.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListCopy(
 *   &           StringListPtr,
 *   &           ResultPtr)
 *    POINTER         (StringListPtr, StringList)
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Make a copy of a string list.
 *   
 * @code
 *   StringList_pa Names = NULL;
 *   StringList_pa CopyOfNames = NULL;
 *   
 *   // do some processing to get names 
 *     .
 *     .
 *     .
 *   
 *   CopyOfNames = TecUtilStringListCopy(Names);
 *   if (CopyOfNames != NULL)
 *     {
 *       // do some processing on the name list copy 
 *         .
 *         .
 *         .
 *   
 *       // get rid of the name list copy 
 *       TecUtilStringListDealloc(&CopyOfNames);
 *     }
 * @endcode
 */
LINKTOADDON StringList_pa STDCALL TecUtilStringListCopy(StringList_pa StringList);

/**
 * Append a copy of the contents of the source string list to the target string
 * list. See the Chapter "Using String Lists," in the ADK User's Manual for a
 * discussion of string lists.
 *   
 * @param Target
 *   String list to which the Source string list is appended. Use
 *   TecUtilStringListAlloc() to allocate a string list
 *   
 * @param Source
 *   String list to append to the Target. Use TecUtilStringListAlloc() to
 *   allocate a string list.
 *   
 * @return
 *   A return value of TRUE indicates the operation was successful. A return
 *   value of FALSE indicates that sufficient memory was not available for the
 *   request.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilStringListAppend(
 *   &                   TargetPtr,
 *   &                   SourcePtr)
 *    POINTER         (TargetPtr, Target)
 *    POINTER         (SourcePtr, Source)
 * </FortranSyntax>
 *   
 *   Append one string list to another.
 *   
 * @code
 *   Boolean_t     IsOk = FALSE;
 *   StringList_pa Names = NULL;
 *   StringList_pa DefaultNames = NULL;
 *   
 *   // call some function to get a names list from the user 
 *   Names = MyFuncForGettingNamesFromUser();
 *   
 *   // call some function to get some default name list 
 *   DefaultNames = MyFuncForGettingDefaultNames();
 *   
 *   // combine the two name lists into one 
 *   if (Names != NULL && DefaultNames != NULL)
 *     {
 *       IsOk = TecUtilStringListAppend(Names, DefaultNames);
 *       if (IsOk)
 *         {
 *           // do more processing 
 *             .
 *             .
 *             .
 *   
 *           // get rid of the name lists 
 *           TecUtilStringListDealloc(&Names);
 *           TecUtilStringListDealloc(&DefaultNames);
 *         }
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilStringListAppend(StringList_pa Target,
                                                      StringList_pa Source);
/**
 * Return a newline delimited string representation of the string list. A
 * newline delimited string is a character string with newlines (\\n) used to
 * separate one substring from the next. See the chapter on "Using String
 * Lists" in the ADK User's Manual for a discussion of string lists.
 *
 * @par Note:
 *   The caller is responsible for de-allocating the copy of the newline
 *   delimited string when it is no longer needed.
 *   
 * @param StringList
 *   Handle to a valid string list. Use TecUtilStringListAlloc() to allocate a string list.
 *   
 * @return
 *   A newline, (\\n), delimited string representation of the string list.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListToNLString(
 *   &           StringListPtr,
 *   &           Result,
 *   &           ResultLength)
 *    POINTER         (StringListPtr, StringList)
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 *   
 *   Given a string list containing 3 members: "Hello", "", and "World", the function will return the
 *   following string: "Hello\\n\\nWorld".
 *   
 * @code
 *   StringList_pa List = NULL;
 *   
 *   List = TecUtilStringListAlloc();
 *   if (List != NULL)
 *     {
 *       // add items to the string list 
 *       TecUtilStringListAppendString(List, "Hello");
 *       TecUtilStringListAppendString(List, "");
 *       TecUtilStringListAppendString(List, "World");
 *   
 *       //print the newline separated string representation 
 *       String = TecUtilStringListToNLString(List);
 *       if (String != NULL)
 *         {
 *           printf("%s\n", String);
 *           TecUtilStringDealloc(&String);
 *         }
 *   
 *       // get rid of the list 
 *       TecUtilStringListDealloc(&List);
 *     }
 * @endcode
 */
LINKTOADDON char * STDCALL TecUtilStringListToNLString(StringList_pa StringList);

/**
 * Create a string list from a newline delimited string. A newline delimited
 * string is a character string with newlines (\\n) used to separate one
 * substring from the next. See the Chapter "Using String Lists," in the ADK
 * User's Manual for a discussion of string lists.
 *
 * @par Note:
 *   The caller is responsible for deallocating the string list when it is no
 *   longer needed.
 *   
 * @param String
 *   The newline delimited string
 *   
 * @return
 *   Handle to the created string list. A handle of NULL is returned if
 *   sufficient memory is not available.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListFromNLString(
 *   &           String,
 *   &           ResultPtr)
 *    CHARACTER*(*)   String
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Given the string, Hello\\n\\nWorld, the function will return a string list containing 3 members:
 *   "Hello, "" (that is, an empty string), and "World."
 *   
 * @code
 *   StringList_pa List = NULL;
 *   
 *   List = TecUtilStringListFromNLString("Hello\\n\\nWorld");
 *   if (List != NULL)
 *     {
 *       LgIndex_t I = 0;
 *       LgIndex_t Count = 0;
 *   
 *       // print each element of the string list 
 *       for (I = 0, Count = TecUtilStringListGetCount(List);
 *             I < Count;
 *             I++)
 *         {
 *           String = TecUtilStringListGetString(List, I+1);
 *           if (String != NULL)
 *             {
 *               printf("Item #%d: %s\n", I+1, String);
 *               TecUtilStringDealloc(&String);
 *             }
 *         }
 *   
 *       // get rid of the list 
 *       TecUtilStringListDealloc(&List);
 *     }
 * @endcode
 */
LINKTOADDON StringList_pa STDCALL TecUtilStringListFromNLString(const char *String);

/**
 * Sorts the string list by repeatedly calling the 'Comparator' function until
 * the list is in order.
 *
 * @since
 *     10.0-3-129
 *
 * @param StringList
 *     String list to sort.
 * @param Comparator
 *     Function called to compare two string list strings or NULL for the
 *     default sort. The default sorting handles NULL elements and uses the
 *     system's strcmp utility for comparing valid strings elements.
 * @param ClientData
 *     Contextual information that is passed along to the comparator function.
 *     Client data isn't used by the default comparator and can be passed any
 *     value. For specialized comparator functions the client data is used to
 *     hold contextual information so that global variable do not have to be
 *     used.
 *
 * <FortranSyntax>
 *    SUBROUTINE TecUtilStringListSort(
 *   &           StringListPtr,
 *   &           ComparatorPtr,
 *   &           ClientDataPtr)
 *    POINTER         (StringListPtr, StringList)
 *    POINTER         (ComparatorPtr, Comparator)
 *    POINTER         (ClientDataPtr, ClientData)
 * </FortranSyntax>
 *
 * Sort the variable string list using Tecplot's default comparator:
 * @code
 *   TecUtilStringListSort(MyVarList, NULL, 0);
 * @endcode
 *
 * Sort the variable string list using own own comparator function. We pass
 * some client data to our own comparator function simply to show how to use
 * it. In this case all the client data is used for is to keep track of the
 * number of times our comparator function was called... not very useful.
 * @code
 *   static int MyStrComparator(const char *String1,
 *                              const char *String2,
 *                              ArbParam_t  ClientData)
 *   {
 *     int Result = 0;
 *     LgIndex_t *NumTimesCalled;
 *
 *     REQUIRE(VALID_REF(String1) || String1 == NULL);
 *     REQUIRE(VALID_REF(String2) || String2 == NULL);
 *
 *     NumTimesCalled = (LgIndex_t *)ClientData;
 *     (*NumTimesCalled) += 1;
 *
 *     if (String1 != NULL && String2 != NULL)
 *       Result = strcmp(String1, String2);
 *     else if (String1 == NULL && String2 == NULL)
 *       Result = 0;
 *     else if (String1 == NULL)
 *       Result = -1;
 *     else if (String2 == NULL)
 *       Result = 1;
 *     else
 *       CHECK(FALSE);
 *
 *     return Result;
 *   }
 *
 * ...
 *
 *   // After calling TecUtilStringListSort NumTimesCalled will contain the
 *   // number of times that our comparator was called.
 *   LgIndex_t NumTimesCalled = 0;
 *   TecUtilStringListSort(MyVarList, MyStrComparator, &NumTimesCalled);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilStringListSort(StringList_pa                  StringList,
                                               StringListStringComparator_pf  Comparator,
                                               ArbParam_t                     ClientData);

/* * SET FUNCTIONS * */

/**
 * Allocate a new empty set. See the chapter "Using Sets" in the ADK User's
 * Manual for a discussion of sets.
 *   
 * @param ShowErr
 *   TRUE to display an error message if the function's return value is FALSE;
 *   FALSE to display no error message
 *   
 * @return
 *   The new set if successful, NULL if not. An unsuccessful return value
 *   indicates that there was not enough memory to create a new set.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilSetAlloc(
 *   &           ShowErr,
 *   &           ResultPtr)
 *    INTEGER*4       ShowErr
 *    POINTER         (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Create two sets, A and B:
 *   
 * @code
 *   Set_pa A, B;
 *   A = TecUtilSetAlloc(TRUE);
 *   B = TecUtilSetAlloc(TRUE);
 * @endcode
 */
LINKTOADDON Set_pa     STDCALL TecUtilSetAlloc(Boolean_t ShowErr);
/**
 *   Free all memory associated with the specified set and assign the set to be NULL. See the
 *   chapter "Using Sets" in the ADK User's Manual for a discussion of sets.
 *   
 * @param Set
 *   The set to deallocate.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilSetDealloc(SetPtr)
 *    POINTER (SetPtr, Set)
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilSetDealloc(Set_pa *Set);
/**
 * Copy one set to another. See the chapter "Using Sets" in the ADK User's
 * Manual for a discussion of sets.
 *   
 * @param DstSet
 *   The destination set, which must already be allocated with
 *   TecUtilSetAlloc().
 *   
 * @param SrcSet
 *   The source set
 *   
 * @param ShowErr
 *   Set to TRUE to display an error message if an error occurs during the call
 *   
 * @return
 *   TRUE if successful, FALSE if not. FALSE indicates that SrcSet contains elements that cannot be
 *   added to DstSet.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetCopy(
 *   &                   DstSetPtr,
 *   &                   SrcSetPtr,
 *   &                   ShowErr)
 *    POINTER         (DstSetPtr, DstSet)
 *    POINTER         (SrcSetPtr, SrcSet)
 *    INTEGER*4       ShowErr
 * </FortranSyntax>
 *   
 * Make a copy of the set of active Line-maps:
 *   
 * @code
 *   Set_pa MySet, LineMaps = NULL;
 *   MySet = TecUtilSetAlloc(TRUE);
 *   TecUtilLineMapGetActive(&LineMaps);
 *   TecUtilSetCopy(MySet, LineMaps, TRUE);
 *   .
 *   .
 *   .
 *   TecUtilSetDealloc(&MySet);
 *   TecUtilSetDealloc(&LineMaps);
 * @endcode
 */
LINKTOADDON Boolean_t  STDCALL TecUtilSetCopy(Set_pa    DstSet,
                                              Set_pa    SrcSet,
                                              Boolean_t ShowErr);
/**
 *   Empties the specified set. See the chapter "Using Sets" in the ADK User's Manual for a discussion
 *   of sets.
 *   
 * @param Set
 *   The set to empty
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilSetClear(SetPtr)
 *    POINTER (SetPtr, Set)
 * </FortranSyntax>
 *   
 *   Get the set of active zones, then clear the set so it can be used again:
 *   
 * @code
 *   Set_pa Zones = NULL;
 *   TecUtilZoneGetActive(&Zones);
 *   .
 *   . // Use the set of active zones 
 *   .
 *   TecUtilSetClear(Zones);
 *   .
 *   . // Use the set for something else 
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilSetClear(Set_pa Set);
/**
 *   Add the specified member to the specified set. See the chapter "Using Sets" in the ADK User's
 *   Manual for a discussion of sets.
 *   
 * @param Set
 *   The set to which to add the specified member.
 *   
 * @param Member
 *   The item to add to the specified set. Members start at one.
 *   
 * @param ShowErr
 *   TRUE to display an error message if the function's return value is FALSE; FALSE to display no
 *   error message
 *   
 * @return
 *   TRUE if successful, FALSE if not. A FALSE value is highly unlikely and only occurs if the set
 *   cannot be expanded to accomodate the new member.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetAddMember(
 *   &                   SetPtr,
 *   &                   Member,
 *   &                   ShowErr)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Member
 *    INTEGER*4       ShowErr
 * </FortranSyntax>
 *   
 *   Create a set called ZonesToDelete and add zones 2 and 4 to it:
 *   
 * @code
 *   Set_pa ZonesToDelete = TecUtilSetAlloc(TRUE);
 *   TecUtilSetAddMember(ZonesToDelete, 2, TRUE);
 *   TecUtilSetAddMember(ZonesToDelete, 4, TRUE);
 * @endcode
 */
LINKTOADDON Boolean_t  STDCALL TecUtilSetAddMember(Set_pa     Set,
                                                   SetIndex_t Member,
                                                   Boolean_t  ShowErr);
/**
 *   Remove a member from a set. See the chapter "Using Sets" in the ADK User's Manual for a
 *   discussion of sets.
 *   
 * @param Set
 *   The set from which to remove the specified member.
 *   
 * @param Member
 *   The member to remove from the specified set. Members start at one
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilSetRemoveMember(
 *   &           SetPtr,
 *   &           Member)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Member
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilSetRemoveMember(Set_pa     Set,
                                                    SetIndex_t Member);
/**
 *   Determine if the specified member is in the specified set. See the chapter "Using Sets" in the
 *   ADK User's Manual for a discussion of sets.
 *   
 * @param Set
 *   The set to check for the specified member.
 *   
 * @param Member
 *   The item for which to check the specified set. Members start at one
 *   
 * @return
 *   TRUE if Member is a member of Set, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetIsMember(
 *   &                   SetPtr,
 *   &                   Member)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Member
 * </FortranSyntax>
 *   
 *   Determine if the set MySet contains the member MyMember, and if so, remove MyMember from MySet:
 *   
 * @code
 *   Set_pa MySet;
 *   SetIndex_t MyMember;
 *   .
 *   .
 *   if (TecUtilSetIsMember(MySet, MyMember))
 *     TecUtilSetRemoveMember(MySet, MyMember);
 * @endcode
 */
LINKTOADDON Boolean_t  STDCALL TecUtilSetIsMember(Set_pa     Set,
                                            SetIndex_t Member);
/**
 *   Determine if the specified set is NULL or contains no members. See the chapter "Using Sets" in
 *   the ADK User's Manual for a discussion of sets.
 *   
 * @param Set
 *   The set to check for members
 *   
 * @return
 *   TRUE if Set is NULL or contains no members, FALSE if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetIsEmpty(SetPtr)
 *    POINTER (SetPtr, Set)
 * </FortranSyntax>
 *   
 *   Determine if the set MySet is empty or contains no members:
 *   
 * @code
 *   Set_pa MySet;
 *   
 *   if (TecUtilSetIsEmpty(MySet))
 *     {
 *       .... take action based on the set being empty.
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t  STDCALL TecUtilSetIsEmpty(Set_pa Set);



/**
 *   Get the count of the number of members in a set. See the chapter "Using Sets" in the ADK User's
 *   Manual for a discussion of sets.
 *   
 * @param Set
 *   The set for which to get the count
 *   
 * @return
 *   The count of the number of members in the set Set.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetGetMemberCount(SetPtr)
 *    POINTER (SetPtr, Set)
 * </FortranSyntax>
 */
LINKTOADDON SetIndex_t STDCALL TecUtilSetGetMemberCount(Set_pa Set);



/**
 *   Determines if the specified sets are equal (have the same members). See the chapter "Using Sets" 
 *   in the ADK User's Manual for a discussion of sets.
 *   
 * @param Set1
 *   The set to compare with Set2.
 *   
 * @param Set2
 *   The set to compare with Set1
 *   
 * @return
 *   TRUE if the specified sets are equal, FALSE if they are not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetIsEqual(
 *   &                   Set1Ptr,
 *   &                   Set2Ptr)
 *    POINTER         (Set1Ptr, Set1)
 *    POINTER         (Set2Ptr, Set2)
 * </FortranSyntax>
 *   
 *   Determine if all enabled zones are active:
 *   
 * @code
 *   Boolean_t AllEnabledZonesAreActive;
 *   Set_pa ActiveZones  = NULL;
 *   Set_pa EnabledZones = NULL;
 *
 *   TecUtilZoneGetActive(&ActiveZones);
 *   TecUtilZoneGetEnabled(&EnabledZones);
 *   AllEnabledZonesAreActive = TecUtilSetIsEqual(ActiveZones, EnabledZones);
 * @endcode
 */
LINKTOADDON Boolean_t  STDCALL TecUtilSetIsEqual(Set_pa Set1,
                                                Set_pa Set2);
/**
 *   
Get the member of the specified set at the specified position. See the chapter "Using Sets" in
 *   the ADK User's Manual for a discussion of sets.
 *   
 * @param Set
 *   The set from which to get the member.
 *   
 * @param Position
 *   The position in the set.
 *   
 * @return
 *   The member of the specified set at the specified position. Members start at one. If the set does
 *   not contain a member at the specified position, the return value is TECUTILSETNOTMEMBER.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetGetMember(
 *   &                   SetPtr,
 *   &                   Position)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Position
 * </FortranSyntax>
 *   
 *   Get each member from the set MySet:
 *   
 * @code
 *   Set_pa MySet;
 *   .
 *   .
 *   SetIndex_t Member;
 *   SetIndex_t Count;
 *   SetIndex_t Position;
 *
 *   Count = TecUtilSetGetMemberCount(MySet);
 *   for (Position = 1; Position <= Count; Position++)
 *     {
 *       Member = TecUtilSetGetMember(MySet, Position);
 *       .
 *       .
 *     }
 * @endcode
 */
LINKTOADDON SetIndex_t STDCALL TecUtilSetGetMember(Set_pa     Set,
                                                   SetIndex_t Position);
/**
 *   
 * Get the position in the specified set at which the specified member is located.
 * See the chapter "Using Sets" in the ADK User's Manual for a discussion of sets.
 *   
 * @param Set
 *   The set from which to get the member
 *   
 * @param Member
 *   The member after which to get the position. Members start at one.
 *   
 * @return
 *   The position in the specified set at which the specified member is located. If the specified
 *   member is not found, the return value is TECUTILSETNOTMEMBER.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetGetPosition(
 *   &                   SetPtr,
 *   &                   Member)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Member
 * </FortranSyntax>
 *   
 *   Get the position of the member MyMember of the set MySet:
 *   
 * @code
 *   Set_pa MySet;
 *   SetIndex_t Member;
 *   .
 *   .
 *   SetIndex_t Position =
 *     TecUtilSetGetPosition(MySet, MyMember);
 * @endcode
 */
LINKTOADDON SetIndex_t STDCALL TecUtilSetGetPosition(Set_pa     Set,
                                                     SetIndex_t Member);
/**
 *   Get the next member in the specified set which is located after the specified member. See Chapter
 *   20, "Using Sets," in the ADK User's Manual for a discussion of sets.
 *   
 * @param Set
 *   The set from which to get the member
 *   
 * @param Member
 *   The member after which to return the next member. Members start at one. Use TECUTILSETNOTMEMBER to
 *   get the first member of the set
 *   
 * @return
 *   The next member of the specified set after the specified member. Members start at one. If the
 *   specified member is not found or if it is the last member in the set, the return value is
 *   TECUTILSETNOTMEMBER.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilSetGetNextMember(
 *   &                   SetPtr,
 *   &                   Member)
 *    POINTER         (SetPtr, Set)
 *    INTEGER*4       Member
 * </FortranSyntax>
 *   
 *   Loop through all members of the set MySet:
 *   
 * @code
 *   Set_pa MySet;
 *   .
 *   .
 *   SetIndex_t Member = TecUtilSetGetNextMember(MySet,
 *                                               TECUTILSETNOTMEMBER);
 *
 *   while (Member != TECUTILSETNOTMEMBER)
 *      {
 *        .
 *        .
 *        Member = TecUtilSetGetNextMember(MySet, Member);
 *      }
 * @endcode
 */
LINKTOADDON SetIndex_t STDCALL TecUtilSetGetNextMember(Set_pa     Set,
                                                       SetIndex_t Member);

/**
 * Convenience macro for looping through all members in a set.
 *
 * @param Member
 *   Set member variable. This parameter does not have to be initialized before
 *   use.
 *
 * @param Set
 *   Any valid Tecplot set.
 *
 * Perform some processing on all active zones:
 * @code
 *   Set_pa ActiveZones;
 *   if ( TecUtilDataSetIsAvailable() &&
 *        TecUtilZoneGetActive(&ActiveZones))
 *     {
 *       TecUtilSetForEachMember(Zone, ActiveZones)
 *         {
 *           // do something with the active Zone
 *           ...
 *         }
 *       TecUtilSetDealloc(&ActiveZones);
 *     }
 * @endcode
 */
#define TecUtilSetForEachMember(Member, Set) \
            for (Member = TecUtilSetGetNextMember(Set, TECUTILSETNOTMEMBER); \
                 Member != TECUTILSETNOTMEMBER; \
                 Member = TecUtilSetGetNextMember(Set, Member))

/**
 * Convert the specified X-coordinate value from one coordinate system to another.
 *   
 * @param OldCoordSys
 *   The coordinate system in which OldX is currently defined. The possible values are: CoordSys_Grid,
 *   CoordSys_Frame, CoordSys_FrameOffset, CoordSys_Paper, or CoordSys_Screen.
 *   
 * @param NewCoordSys
 *   The coordinate system into which to transform OldX. See OldCoordSys for the possible values
 *   
 * @param OldX
 *   The value to convert from one coordinate system to another
 *   
 * @return
 *   The value OldX converted into the new coordinate system.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilConvertXPosition(
 *   &                   OldCoordSys,
 *   &                   NewCoordSys,
 *   &                   OldX)
 *    INTEGER*4       OldCoordSys
 *    INTEGER*4       NewCoordSys
 *    REAL*8          OldX
 * </FortranSyntax>
 *   
 * Determine the position on the paper of a text label. Assume the Text_ID has
 * already been obtained (See TecUtilPickListXxx functions or TecUtilTextXxx
 * functions for examples on how to obtain a Text_ID).
 *   
 * @code
 *   Text_ID TID;
 *   double  XPos,YPos;
 *   double PaperXPos,PaperYPos;
 *   
 *   ... Text_ID obtained....
 *   
 *   TecUtilTextGetXYPos(TID,&XPos,&YPos);
 *   
 *   PaperXPos = TecUtilConvertXPosition(TecUtilTextGetPositionCoordSys(TID),
 *                                       CoordSys_Paper,
 *                                       XPos);
 *   PaperYPos = TecUtilConvertYPosition(TecUtilTextGetPositionCoordSys(TID),
 *                                       CoordSys_Paper,
 *                                       YPos);
 * @endcode
 */
LINKTOADDON double STDCALL TecUtilConvertXPosition(CoordSys_e  OldCoordSys,
                                                   CoordSys_e  NewCoordSys,
                                                   double      OldX);
/**
 *   Convert the specified horizontal dimension from one coordinate system to another.
 *   
 * @param OldCoordSys
 *   Coordinate system in which OldDimension is measured. The possible values are: CoordSys_Grid,
 *   CoordSys_Frame, CoordSys_Paper, CoordSys_Screen or CoordSys_Hardcopy
 *   
 * @param NewCoordSys
 *   Coordinate system in which the return value is measured. The possible values are: CoordSys_Grid,
 *   CoordSys_Frame, CoordSys_Paper, CoordSys_Screen or CoordSys_Hardcopy
 *   
 * @param OldDimension
 *   Dimension to convert
 *   
 * @return
 *   Converted dimension in the new coordinate system.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilConvertXDimension(
 *   &                   OldCoordSys,
 *   &                   NewCoordSys,
 *   &                   OldDimension)
 *    INTEGER*4       OldCoordSys
 *    INTEGER*4       NewCoordSys
 *    REAL*8          OldDimension
 * </FortranSyntax>
 *   
 *   Find the size of the current frame in screen pixels.
 *   
 * @code
 *   double X, Y, Width, Height;
 *   // get frame width and height in inches 
 *   TecUtilFrameGetPosAndSize(&X,&Y, &Width, &Height);
 *   // convert width and height to screen coordinates (pixels) 
 *   Width = TecUtilConvertXDimension(CoordSys_Paper,
 *                                    CoordSys_Screen, Width);
 *   Height = TecUtilConvertYDimension(CoordSys_Paper,
 *                                     CoordSys_Screen, Height);
 * @endcode
 */
LINKTOADDON double STDCALL TecUtilConvertXDimension(CoordSys_e  OldCoordSys,
                                                    CoordSys_e  NewCoordSys,
                                                    double      OldDimension);
/**
 *   Convert the specified Y-coordinate value from one specified coordinate system to another specified
 *   coordinate system.
 *   
 * @param OldCoordSys
 *   The coordinate system in which OldY is currently defined. The possible values are: CoordSys_Grid,
 *   CoordSys_Frame, CoordSys_FrameOffset, CoordSys_Paper, or CoordSys_Screen
 *   
 * @param NewCoordSys
 *   The coordinate system into which to transform OldY. See OldCoordSys for the possible values
 *   
 * @param OldY
 *   The value to convert from one coordinate system to another
 *   
 * @return
 *   The value OldY converted into the new coordinate system.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilConvertYPosition(
 *   &                   OldCoordSys,
 *   &                   NewCoordSys,
 *   &                   OldY)
 *    INTEGER*4       OldCoordSys
 *    INTEGER*4       NewCoordSys
 *    REAL*8          OldY
 * </FortranSyntax>
 */
LINKTOADDON double STDCALL TecUtilConvertYPosition(CoordSys_e  OldCoordSys,
                                                   CoordSys_e  NewCoordSys,
                                                   double      OldY);
/**
 *   Convert the specified vertical dimension from one coordinate system to another.
 *   
 * @param OldCoordSys
 *   Coordinate system in which OldDimension is measured. The possible values are: CoordSys_Grid,
 *   CoordSys_Frame, CoordSys_Paper, CoordSys_Screen or CoordSys_Hardcopy
 *   
 * @param NewCoordSys
 *   Coordinate system in which OldDimension is measured. The possible values are: CoordSys_Grid,
 *   CoordSys_Frame, CoordSys_Paper, CoordSys_Screen or CoordSys_Hardcopy
 *   
 * @param OldDimension
 *   Dimension to convert
 *   
 * @return
 *   Converted dimension in the new coordinate system.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilConvertYDimension(
 *   &                   OldCoordSys,
 *   &                   NewCoordSys,
 *   &                   OldDimension)
 *    INTEGER*4       OldCoordSys
 *    INTEGER*4       NewCoordSys
 *    REAL*8          OldDimension
 * </FortranSyntax>
 */
LINKTOADDON double STDCALL TecUtilConvertYDimension(CoordSys_e  OldCoordSys,
                                                    CoordSys_e  NewCoordSys,
                                                    double      OldDimension);
/**
 *   Convert from one measurement system to another.
 *   
 * @param OldUnits
 *   Units in which OldSize is measured. The possible values are: Units_Grid, Units_Frame, Units_Point
 *   or Units_Screen.
 *   
 * @param NewUnits
 *   Unit space in which the return value is measured. The possible values are: Units_Grid,
 *   Units_Frame, Units_Point or Units_Screen.
 *   
 * @param OldSize
 *   Size in the old measurement system.
 *   
 * @return
 *   Converted size in the new measurement system.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilConvertUnits(
 *   &                   OldUnits,
 *   &                   NewUnits,
 *   &                   OldSize)
 *    INTEGER*4       OldUnits
 *    INTEGER*4       NewUnits
 *    REAL*8          OldSize
 * </FortranSyntax>
 *   
 *   To create a line with a line thickness of three points:
 *   
 * @code
 *   Geom_ID g;
 *   double frame_units;
 *   frame_units = TecUtilConvertUnits(Units_Point, Units_Frame, 3.);
 *   g = TecUtilGeom2DLineSegmentCreate(CoordSys_Frame, 5., 5., 95., 95.);
 *   TecUtilGeomSetLineThickness(g, frame_units)
 * @endcode
 */
LINKTOADDON double STDCALL TecUtilConvertUnits(Units_e OldUnits,
                                               Units_e NewUnits,
                                               double  OldSize);


#if defined COMPILE_BUT_HIDE_FROM_DOC
/* Deprecated or Function that should not be otherwise documented. */
LINKTOADDON Boolean_t STDCALL TecUtilReadBinaryData(Boolean_t       GetHeaderInfoOnly, /* <-activex> */
                                                    const char     *FName,
                                                    short          *IVersion,
                                                    char          **DataSetTitle,
                                                    EntIndex_t     *NumZones,
                                                    EntIndex_t     *NumVars,
                                                    StringList_pa  *VarNames,
                                                    StringList_pa  *ZoneNames,
                                                    LgIndex_t     **NumPtsI,
                                                    LgIndex_t     **NumPtsJ,
                                                    LgIndex_t     **NumPtsK,
                                                    ZoneType_e    **ZoneType,
                                                    StringList_pa  *UserRec,
                                                    Boolean_t       RawDataspaceAllocated,
                                                    NodeMap_t    ***NodeMap, 
                                                    double       ***VDataBase);
#endif

/**
 * Sets the byte ordering request for subsequent calls to TecUtilTecIni(). The
 * byte ordering request will remain in effect until changed by another call to
 * this function. Calling this function after a call to TecUtilTecIni() has no
 * effect on any files opened by previous calls to TecUtilTecIni() and only
 * effects future files created by TecUtilTecIni().
 *
 * @since
 *     10.0-3-129
 *
 * @param OutputForeignByteOrder
 *  Reference to a boolean value indicating if subsequent calls to
 *  TecUtilTecIni() should create files with foreign or native byte ordering.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTecForeign(
 *   &                   OutputForeignByteOrder)
 *    INTEGER*4       OutputForeignByteOrder
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL TecUtilTecForeign(LgIndex_t *OutputForeignByteOrder);

/**
 * Initializes the process of writing a binary data file. This must be called
 * first before any other TecUtilTecXxx calls are made. You may write to
 * multiple files by calling TecUtilTecIni() more than once. Each time
 * TecUtilTecIni() is called, a new file is opened. Use TecUtilTecFil() to
 * switch between files.
 *   
 * @param Title
 *   Title of the data set. Must be NULL terminated
 *   
 * @param Variables
 *   List of variable names. Separate variable names with a comma or space.
 *   Must be NULL terminated
 *   
 * @param FName
 *   Name of the file to create. Must be NULL terminated
 *   
 * @param ScratchDir
 *   Name of the directory to put the scratch file. Must be NULL terminated
 *   
 * @param Debug
 *   Handle to the integer flag for debugging. Set to 0 for no debugging or 1
 *   to debug
 *   
 * @param VIsDouble
 *   Handle to the integer flag for specifying whether field data generated in
 *   future calls to TecUtilTecIni() are to be written in single or double
 *   precision. Set to 0 for single precision or 1 for double
 *   
 * @return
 *   0 if successful, -1 if unsuccessful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecIni(
 *   &                   Title,
 *   &                   Variables,
 *   &                   FName,
 *   &                   ScratchDir,
 *   &                   Debug,
 *   &                   VIsDouble)
 *    CHARACTER*(*)   Title
 *    CHARACTER*(*)   Variables
 *    CHARACTER*(*)   FName
 *    CHARACTER*(*)   ScratchDir
 *    INTEGER*4       Debug
 *    INTEGER*4       VIsDouble
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecIni(const char  *Title,
                                            const char  *Variables,
                                            const char  *FName,
                                            const char  *ScratchDir,
                                            LgIndex_t   *Debug,
                                            LgIndex_t   *VIsDouble);

/**
 * Writes header information about the next zone to be added to the data file.
 * After TECZNE is called, you must call TECDAT one or more times (and then
 * call TECNOD if the data format is FEBLOCK or FEPOINT).
 *   
 * @param ZoneTitle
 *   Title of the zone. Must be NULL terminated
 *   
 * @param IMx
 *   Pointer to integer specifying I-Dimension of the zone. If the data is
 *   finite-element then IMx is the number of data points.
 *   
 * @param JMx
 *   Pointer to integer specifying J-Dimension of the zone if ordered otherwise
 *   the number of elements if finite element.
 *   
 * @param KMx
 *   Pointer to integer specifying K-Dimension of the zone if ordered otherwise
 *   is set according to the following chart:
 *   
 * @param ZFormat
 *   Must be set to one of BLOCK, POINT, FEBLOCK, or FEPOINT. Must be NULL
 *   terminated
 *   
 * @param DupList
 *   This parameter specifies a list of variables to duplicate from the
 *   preceding zone. For a complete explination of the DupList parameter, see
 *   the Tecplot User's Manual. The DupList parameter is a string of the
 *   following form: "[n1,n2,...,nn][,FECONNECT]"where n1...nn are the numbers
 *   of the variables to duplicate. If the zone is finite element, you may
 *   optionally include FECONNECT, which will duplicate the connectivity list
 *   from the last zone. Notes for using the DupList parameter:1. You cannot
 *   use the DupList parameter for the first zone, since in that case there is
 *   nothing to duplicate.2. If you use FECONNECT, you cannot call TECNOD for
 *   this zone, since FECONNECT specifies that the entire connectivity list
 *   from the previous zone will be duplicated.3. For finite-element zones, you
 *   can pass "FECONNECT" to duplicate only the connectivity list.4. You may
 *   pass either NULL or a zero length string if you are not using this
 *   parameter. KMx for Triangles is 0, quads, is 1, tets is 2, and bricks
 *   is 3.
 *   
 * @return
 *   0 if successful, -1 if unsuccessful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecZne(
 *   &                   ZoneTitle,
 *   &                   IMx,
 *   &                   JMx,
 *   &                   KMx,
 *   &                   ZFormat,
 *   &                   DupList)
 *    CHARACTER*(*)   ZoneTitle
 *    INTEGER*4       IMx
 *    INTEGER*4       JMx
 *    INTEGER*4       KMx
 *    CHARACTER*(*)   ZFormat
 *    CHARACTER*(*)   DupList
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecZne(const char  *ZoneTitle,
                                            LgIndex_t   *IMx,
                                            LgIndex_t   *JMx,
                                            LgIndex_t   *KMx,
                                            const char  *ZFormat,
                                            const char  *DupList);

/**
 * Writes the zone to the data file.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_TITLE
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   Zone
 * Required:
 *   No
 * Notes:
 *   Zone title
 *   
 * Name:
 *   SV_ZONETYPE
 * Type:
 *   ZoneType_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Zone type may be any of the following values: ZoneType_Ordered,
 *   ZoneType_FETriangle, ZoneType_FEQuad, ZoneType_FETetra, ZoneType_FEBrick,
 *   ZoneType_FELineSeg.
 *   
 * Name:
 *   SV_IMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Number of points for the zone.  The values must each be greater than or
 *   equal to 1.  For ordered zones these ar the I,J, and K index dimensions.
 *   For Finite-element zones IMax is the number of nodes, JMax is the number
 *   of elements
 *   
 * Name:
 *   SV_JMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_KMAX
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_ISBLOCK
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   If TRUE the variavles are written in block formate otherwise the less
 *   efficient point format is used.
 *   
 * Name:
 *   SV_NUMFACECONNECTIONS
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0
 * Required:
 *   No
 * Notes:
 *   Indicates the number of user defined face neighbor connections that will
 *   be supplied.  The value must be greater than or equal to zero
 *   
 * Name:
 *   SV_FACENEIGHBORMODE
 * Type:
 *   FaceNeighborMode_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FaceNeighborMode_LocalOneToOne
 * Required:
 *   No
 * Notes:
 *   None
 *   
 * Name:
 *   SV_VALUELOCATION
 * Type:
 *   LgIndex_t *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Default:
 *   NULL (all nodal)
 * Required:
 *   No
 * Notes:
 *   Array dimensioned by the number of variables where each member contains
 *   the value location for the associated variable.  Each member may have any
 *   of the following values: ValueLocation_CellCentered or ValueLocation_Nodal
 *   
 * Name:
 *   SV_VARSHAREZONE
 * Type:
 *   LgIndex_t *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Default:
 *   NULL (no sharing)
 * Required:
 *   No
 * Notes:
 *   Array dimensioned by the number of variables where each member contains
 *   the zone which the variable shares its data from or specified zero if it
 *   doesn't share with any zone.  If a zone number is specified it must always
 *   be less than the current zone number
 *   
 * Name:
 *   SV_CONNECTSHAREZONE
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   0 (no sharing)
 * Required:
 *   No
 * Notes:
 *   Zone number with which the connectivity shares its data from or zero if it
 *   doesn't share with any zone.  If a zone number is specidied it must always
 *   be less than the current zone number </ArgListTable>
 *   
 * @return
 *   TRUE if the input parameters are valid and the data was successfully
 *   written, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecZneX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecZneX(ArgList_pa ArgList);

/**
 * Writes an array of data to the data file.
 *
 * If the ZoneFormat specified in TecUtilTecZne() is BLOCK, the array must be
 * dimensioned (IMax, JMax, KMax, NumVars) (FORTRAN syntax, where the first
 * element moves the fastest).
 *
 * If the ZoneFormat is POINT, the data must be dimensioned (NumVars, IMax,
 * JMax, KMax).
 *
 * If the ZoneFormat is FEBLOCK, then the data must be dimensioned (NumPts,
 * NumVars).
 *
 * If the ZoneFormat is FEPOINT, then the data must be dimensioned
 * (NumVars,NumPts).
 *
 * TecUtilTecDat() allows you to write your data in a piecemeal fashion in case
 * it is not contained in one contiguous block in your program. Enough calls to
 * TECDAT must be made that the correct number of values are written for each
 * zone and that the aggregate order for the data is correct.
 *
 * In the above summary, NumVars is based on the number of variable names
 * supplied in a previous call to TecUtilTecIni().
 *   
 * @param N
 *   Handle to an integer value specifying number of values to write
 *   
 * @param FieldData_Array
 *   Array of single or double precision data values
 *   
 * @param IsDouble
 *   Handle to the integer flag stating whether the array Data is single (0) or
 *   double (1) precision.
 *   
 * @return
 *   0 if successful, -1 if unsuccessful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecDat(
 *   &                   N,
 *   &                   FieldData_Array,
 *   &                   IsDouble)
 *    INTEGER*4       N
 *    POINTER         (FieldData_ArrayPtr, FieldData_Array)
 *    INTEGER*4       IsDouble
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecDat(LgIndex_t *N,
                                            void      *FieldData_Array,
                                            LgIndex_t *IsDouble);

/**
 * Writes an array of node data to the binary data file. This is the
 * connectivity list for finite element zones.
 *   
 * @param NData_Array
 *   Array of integers. This is the connectivity list, dimensioned (m,JMax) (m
 *   moving fastest), where m is 3 for triangles, 4 for quads and tets, and 8
 *   for bricks.
 *   
 * @return
 *   0 if successful, -1 if unsuccessful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecNod(NData_Array)
 *    INTEGER*4 NData_Array
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecNod(LgIndex_t *NData_Array);

/**
 * Must be called to close out the current data file. There must be a
 * corresponding TecUtilTecEnd() for each TecUtilTecIni().
 *   
 * @return
 *   0 if successful, -1 if unsuccessful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecEnd()
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecEnd(void);

/**
 * Write a set of custom labels to the data file.
 *   
 * @param S
 *   Character string of custom labels. Separate labels by a comma or space.
 *   For example, a set of custom labels for each day of the week is:"Sun Mon
 *   Tue Wed Thu Fri Sat."
 *   
 * @return
 *   0 if successful, -1 if unsuccessful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecLab(S)
 *    CHARACTER*(*) S
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecLab(const char *S);

/**
 * Add a user-defined record, in the form of a character string, to the Tecplot
 * data file. Tecplot currently ignores this record when reading Tecplot data
 * files.
 *   
 * @param S
 *   String used in the user-defined record
 *   
 * @return
 *   0 if successful, -1 if unsuccessful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecUsr(S)
 *    CHARACTER*(*) S
 * </FortranSyntax>
 *   
 * This function is comparable to $!TECUSR, a Tecplot macro command. Please
 * refer to the examples given for $!TECUSR in the Tecplot Reference Manual.
 *   
 * @code
 *   Insert a user-defined record with the string "Hi Mom" into the data file.
 *   TecUtilTecUsr("Hi Mom");
 * @endcode
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecUsr(const char *S);

/**
 * Switch output context to a different file. Each time TecUtilTecIni() is
 * called, a new file "context" is switched to. This allows you to write
 * multiple data files at the same time.
 *   
 * @param F
 *   Handle to integer specifying file number to switch to
 *   
 * @return
 *   0 if successful, -1 if unsuccessful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecFil(F)
 *    INTEGER*4 F
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecFil(LgIndex_t *F);

/**
 * Write a text label to a binary tecplot data file. This function mimicks the
 * TECTXT function that is part of the TecIO library.
 *   
 * @param XPos
 *   X-Anchor position of the text
 *   
 * @param YPos
 *   Y-Anchor position of the text
 *   
 * @param PosCoordMode
 *   Coordinate system used by the anchor position 0=Grid, 1=Frame
 *   
 * @param AttachToZone
 *   Flag specifying whether or not to attach the text to a zone. One=Attach
 *   0=Don't attach
 *   
 * @param Zone
 *   Zone to attach to
 *   
 * @param Font
 *   Font to use. Zero=Helv, 1=HelvBold, 2=Greek, 3=Math, 4=UserDef, 5=Times,
 *   6=TimesItalic, 7=TimesBold, 8=TimesItalicBold, 9=Courier, 10=CourierBold
 *   
 * @param FontHeightUnits
 *   Units for the font height. Zero=Grid, 1=Frame, 2=Point
 *   
 * @param FontHeight
 *   Height of the text
 *   
 * @param BoxType
 *   Type of box to use. Zero=None, 1=Filled, 2=Hollow
 *   
 * @param BoxMargin
 *   Box Margin in percentage of the font height
 *   
 * @param BoxLineThickness
 *   Line thickness of the box in frame units.
 *   
 * @param TextColor
 *   Color of the text, 0=Black, 1=Red, 2=Green, 3=Blue, 4=Cyan, 5=Yellow,
 *   6=Purple, 7=White, 8-64 are Custom colors.
 *   
 * @param BoxColor
 *   Color of the text box outline. 0=Black, 1=Red, 2=Green, 3=Blue, 4=Cyan,
 *   5=Yellow, 6=Purple, 7=White, 8-64 are Custom colors.
 *   
 * @param BoxFillColor
 *   Color of the text box interior. 0=Black, 1=Red, 2=Green, 3=Blue, 4=Cyan,
 *   5=Yellow, 6=Purple, 7=White, 8-64 are Custom colors.
 *   
 * @param Angle
 *   Angle of the text in degrees
 *   
 * @param Anchor
 *   Anchor position of the text. Zero=Left, 1=Center, 2=Right, 3=MidLeft,
 *   4=MidCenter, 5=MidRight, 6=HeadLeft, 7=HeadCenter, 8=HeadRight
 *   
 * @param LineSpacing
 *   Line spacing of the text
 *   
 * @param Scope
 *   Scope for the text. Zero=Global, 1=Local
 *   
 * @param Text
 *   Actual text string
 *   
 * @param MacroFunctionCommand
 *   Macro command to execute when user cntrl-clicks on the text label. Set to
 *   NULL to not use.
 *   
 * @return
 *   Returns 0 if successful, -1 if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecTxt(
 *   &                   XPos,
 *   &                   YPos,
 *   &                   PosCoordMode,
 *   &                   AttachToZone,
 *   &                   Zone,
 *   &                   Font,
 *   &                   FontHeightUnits,
 *   &                   FontHeight,
 *   &                   BoxType,
 *   &                   BoxMargin,
 *   &                   BoxLineThickness,
 *   &                   BoxColor,
 *   &                   BoxFillColor,
 *   &                   Angle,
 *   &                   Anchor,
 *   &                   LineSpacing,
 *   &                   TextColor,
 *   &                   Scope,
 *   &                   Text,
 *   &                   MacroFunctionCommand)
 *    REAL*8          XPos
 *    REAL*8          YPos
 *    INTEGER*4       PosCoordMode
 *    INTEGER*4       AttachToZone
 *    INTEGER*4       Zone
 *    INTEGER*4       Font
 *    INTEGER*4       FontHeightUnits
 *    REAL*8          FontHeight
 *    INTEGER*4       BoxType
 *    REAL*8          BoxMargin
 *    REAL*8          BoxLineThickness
 *    INTEGER*4       BoxColor
 *    INTEGER*4       BoxFillColor
 *    REAL*8          Angle
 *    INTEGER*4       Anchor
 *    REAL*8          LineSpacing
 *    INTEGER*4       TextColor
 *    INTEGER*4       Scope
 *    CHARACTER*(*)   Text
 *    CHARACTER*(*)   MacroFunctionCommand
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecTxt(double    *XPos,
                                            double    *YPos,
                                            LgIndex_t *PosCoordMode,
                                            LgIndex_t *AttachToZone,
                                            LgIndex_t *Zone,
                                            LgIndex_t *Font,
                                            LgIndex_t *FontHeightUnits,
                                            double    *FontHeight,
                                            LgIndex_t *BoxType,
                                            double    *BoxMargin,
                                            double    *BoxLineThickness,
                                            LgIndex_t *BoxColor,
                                            LgIndex_t *BoxFillColor,
                                            double    *Angle,
                                            LgIndex_t *Anchor,
                                            double    *LineSpacing,
                                            LgIndex_t *TextColor,
                                            LgIndex_t *Scope,
                                            const char*Text,
                                            const char*MacroFunctionCommand);

/**
 * Writes the text item to the data file.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_TEXT
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_XPOS
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   Anchor position for the text
 *   
 * Name:
 *   SV_YPOS
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
 *   SV_ZPOS
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
 *   SV_POSITIONCOORDSYS
 * Type:
 *   CoordSys_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   CoordSys_Grid
 * Required:
 *   No
 * Notes:
 *   Coordinate system to which the text belongs and may have any of the
 *   following values: CoordSys_Grid, CoordSys_Frame, CoordSys_Grid3D.
 *   
 * Name:
 *   SV_ATTACHZONE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   A value of TRUE intedictates that the text is attached to the specified
 *   zone or map.  A value of FALSE indicates that the text is not attached to
 *   a specific zone or map
 *   
 * Name:
 *   SV_ZONE
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Zone or map number to which the text is attached
 *   
 * Name:
 *   SV_FONT
 * Type:
 *   Font_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Font_HelveticaBold
 * Required:
 *   No
 * Notes:
 *   Font used to render the text may have any of the following values:
 *   Font_Helvetica, Font_HelveticaBold Font_Greek, Font_Math,
 *   Font_UserDefined, Font_Times, Font_TimesItalic, Font_TimesBold,
 *   Font_TimesItalicBold, Font_Courier, Font_CourierBold
 *   
 * Name:
 *   SV_SIZEUNITS
 * Type:
 *   Units_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Units_Point
 * Required:
 *   No
 * Notes:
 *   Text sizing untis may have any of the following values: Untis_Grid,
 *   Units_Frame or Untis_Point
 *   
 * Name:
 *   SV_HEIGHT
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   14.0
 * Required:
 *   No
 * Notes:
 *   Text height in the specified units
 *   
 * Name:
 *   SV_BOXTYPE
 * Type:
 *   TextBox_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TextBox_None
 * Required:
 *   No
 * Notes:
 *   Text box type may have any of the following values:  TextBox_None,
 *   TextBox_Filled, or TextBox_Hollow
 *   
 * Name:
 *   SV_MARGIN
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   20.0
 * Required:
 *   No
 * Notes:
 *   Margin between the text and the text box may be a value greater than or
 *   equal to zero and less than or equal to 2000
 *   
 * Name:
 *   SV_LINETHICKNESS
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.1
 * Required:
 *   No
 * Notes:
 *   Text box line thickness may be a value greater than or equal to 0.001 or
 *   less than or equal to 100
 *   
 * Name:
 *   SV_COLOR
 * Type:
 *   ColorIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Black_C
 * Required:
 *   No
 * Notes:
 *   Text box line color may be a value greater or equal to zero.
 *   
 * Name:
 *   SV_FILLCOLOR
 * Type:
 *   ColorIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   White_C
 * Required:
 *   No
 * Notes:
 *   Text box fill color may be a value greater or equal to zero
 *   
 * Name:
 *   SV_ANGLE
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   Text angle may have a value greater than or equal to -360 and less than or
 *   equal to 360
 *   
 * Name:
 *   SV_ANCHOR
 * Type:
 *   TextAnchor_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   TextAnchor_Left
 * Required:
 *   No
 * Notes:
 *   Text anchor may have any of the following values: TextAnchor_Left,
 *   TextAnchor_Center, TextAnchor_Right, TextAnchor_MidLeft,
 *   TextAnchor_MidCenter, TextAnchor_MidRight, TextAnchor_HeadLeft,
 *   TextAnchor_HeadCenter, TextAnchor_HeadRight, TextAnchor_OnSide,
 *   
 * Name:
 *   SV_LINESPACING
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   1.0
 * Required:
 *   No
 * Notes:
 *   Line spacing may have a value greater that or equal to zero and less than
 *   or equal to 50
 *   
 * Name:
 *   SV_TEXTCOLOR
 * Type:
 *   ColorIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Black_C
 * Required:
 *   No
 * Notes:
 *   Text color may be a value greater than or equal to zero
 *   
 * Name:
 *   SV_SCOPE
 * Type:
 *   Scope_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Scope_Local
 * Required:
 *   No
 * Notes:
 *   Text with local scope is displayed onlu in the frame in which it is
 *   created.  If the text is defined as having global scope, it will appear in
 *   all "like" frames, that is, those frames using the same data set as the
 *   one in which the text was created.  Scope may have any of the following
 *   values: Scope_Global or Scope_Local.
 *   
 * Name:
 *   SV_CLIPPING
 * Type:
 *   Clipping_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Clipping_ClipToViewport
 * Required:
 *   No
 * Notes:
 *   Clipping applied to the text may have any of the following values:
 *   Clipping_ClipToViewport or Clipping_ClipToFrame
 *   
 * Name:
 *   SV_MACROFUNCTIONCOMMAND
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Macro function command associated with the text
 * </ArgListTable>
 *   
 * @return
 *   TRUE if the input parameters are valid and the data was successfully
 *   written, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecTxtX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecTxtX(ArgList_pa ArgList);

/**
 * Write a geometry to a binary tecplot datafile. This function mimicks the
 * TecGeo function that is part of the TecIO library.
 *   
 * @param MacroFunctionCommand
 *   Macro command to execute when user cntrl-clicks on the geometry. Set to
 *   NULL to not use
 *   
 * @param XPos
 *   X-Anchor position of the geometry
 *   
 * @param YPos
 *   Y-Anchor position of the geometry
 *   
 * @param ZPos
 *   Z-Anchor position of the geometry
 *   
 * @param PosCoordMode
 *   Position coordinate mode of the geometry. Zero=Grid, one=Frame
 *   
 * @param AttachToZone
 *   Flag specifying whether or not to attach the geometry to a zone.
 *   Zero=Attach, one=Don't attach
 *   
 * @param Zone
 *   Zone to attach to
 *   
 * @param Color
 *   Color of the geometry. (0-63)
 *   
 * @param FillColor
 *   Fill Color of the geometry. (0-63)
 *   
 * @param IsFilled
 *   Flag specifying whether or not to fill the geometry. One=Fill 0=Don't fill
 *   
 * @param GeomType
 *   Type of geometry. Zero=2D line segments, 1=Rectangle, 2=Square, 3=Circle,
 *   4=Ellipse, 5=3D line segments
 *   
 * @param LinePattern
 *   Line pattern. Zero=Solid, 1=Dashed, 2=DashDot, 3=Dotted, 4=LongDash,
 *   5=DashDotDot
 *   
 * @param PatternLength
 *   Line Pattern Length in frame units (0 < L <= 100.0).
 *   
 * @param LineThickness
 *   Line thickness in frame units (0 < L <= 100.0)
 *   
 * @param NumEllipsePts
 *   Number of points to use to draw ellipses or circles
 *   
 * @param ArrowheadStyle
 *   Style of arrowhead. Zero=Plain, 1=Filled, 2=Hollow
 *   
 * @param ArrowheadAttachment
 *   How to attach the arrowhead(s). Zero=None, 1=Beginning, 2=End, 3=Both.
 *   
 * @param ArrowheadSize
 *   Size of the arrowhead in frame units
 *   
 * @param ArrowheadAngle
 *   Angle of the arrowhead in degrees
 *   
 * @param Scope
 *   Scope for the geometry. Zero=Global, 1=Local
 *   
 * @param NumSegments
 *   Number of polyline segments in the geometry
 *   
 * @param NumSegPts
 *   Array of the number of points in each polyline segment
 *   
 * @param XGeomData
 *   Array of X-values for the geometry
 *   
 * @param YGeomData
 *   Array of Y-values for the geometry
 *   
 * @param ZGeomData
 *   Array of Z-values for the geometry
 *   
 * @return
 *   Returns 0 if successful, -1 if not.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecGeo(
 *   &                   XPos,
 *   &                   YPos,
 *   &                   ZPos,
 *   &                   PosCoordMode,
 *   &                   AttachToZone,
 *   &                   Zone,
 *   &                   Color,
 *   &                   FillColor,
 *   &                   IsFilled,
 *   &                   GeomType,
 *   &                   LinePattern,
 *   &                   PatternLength,
 *   &                   LineThickness,
 *   &                   NumEllipsePts,
 *   &                   ArrowheadStyle,
 *   &                   ArrowheadAttachment,
 *   &                   ArrowheadSize,
 *   &                   ArrowheadAngle,
 *   &                   Scope,
 *   &                   NumSegments,
 *   &                   NumSegPts,
 *   &                   XGeomData,
 *   &                   YGeomData,
 *   &                   ZGeomData,
 *   &                   MacroFunctionCommand)
 *    REAL*8          XPos
 *    REAL*8          YPos
 *    REAL*8          ZPos
 *    INTEGER*4       PosCoordMode
 *    INTEGER*4       AttachToZone
 *    INTEGER*4       Zone
 *    INTEGER*4       Color
 *    INTEGER*4       FillColor
 *    INTEGER*4       IsFilled
 *    INTEGER*4       GeomType
 *    INTEGER*4       LinePattern
 *    REAL*8          PatternLength
 *    REAL*8          LineThickness
 *    INTEGER*4       NumEllipsePts
 *    INTEGER*4       ArrowheadStyle
 *    INTEGER*4       ArrowheadAttachment
 *    REAL*8          ArrowheadSize
 *    REAL*8          ArrowheadAngle
 *    INTEGER*4       Scope
 *    INTEGER*4       NumSegments
 *    INTEGER*4       NumSegPts
 *    REAL*4          XGeomData
 *    REAL*4          YGeomData
 *    REAL*4          ZGeomData
 *    CHARACTER*(*)   MacroFunctionCommand
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecGeo(double    *XPos,
                                            double    *YPos,
                                            double    *ZPos,
                                            LgIndex_t *PosCoordMode,
                                            LgIndex_t *AttachToZone,
                                            LgIndex_t *Zone,
                                            LgIndex_t *Color,
                                            LgIndex_t *FillColor,
                                            LgIndex_t *IsFilled,
                                            LgIndex_t *GeomType,
                                            LgIndex_t *LinePattern,
                                            double    *PatternLength,
                                            double    *LineThickness,
                                            LgIndex_t *NumEllipsePts,
                                            LgIndex_t *ArrowheadStyle,
                                            LgIndex_t *ArrowheadAttachment,
                                            double    *ArrowheadSize,
                                            double    *ArrowheadAngle,
                                            LgIndex_t *Scope,
                                            LgIndex_t *NumSegments,
                                            LgIndex_t *NumSegPts,
                                            float     *XGeomData,
                                            float     *YGeomData,
                                            float     *ZGeomData,
                                            const char*MacroFunctionCommand);

/**
 * Writes the geometry item to the data file.
 *   
 * @param ArgList
 *   Set of Arglist entries.  This is built using calls to
 *   TecUtilArgListAppendXXXX functions.
 * <ArgListTable>
 *   
 * Name:
 *   SV_GEOMTYPE
 * Type:
 *   GeomType_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Geometry type may have any of the following values: GeomType_LineSegs,
 *   GeomType_Rectangle, GeomType_Square, GeomType_Circle, GeomType_Ellipse,
 *   GeomType_LineSegs3D,
 *   
 * Name:
 *   SV_NUMGEOSEGMENTS
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Required:
 *   Yes
 * Notes:
 *   Only applies to a geometry of type GeomType_LineSegs and is the number of
 *   geometry line segments. The value must be greater than or equal to 1
 *   
 * Name:
 *   SV_NUMSEGPTS
 * Type:
 *   LgIndex_t *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Required:
 *   Yes
 * Notes:
 *   Only applies to a geometry of type GeomType_LineSegs and is an array
 *   dimensioned by the number of geometry segments where each member of the
 *   array holds the number of points defining each geometry segment
 *   
 * Name:
 *   SV_ARROWHEADSTYLE
 * Type:
 *   ArrowheadStyle_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Arrowhead_Plain
 * Required:
 *   No
 * Notes:
 *   Only applies to a geometry of type GeomType_LineSegs and is the arrowhead
 *   style may have any of the following values: Arrowhead_Plain,
 *   Arrowhead_Filled, Arrowhead_Hollow.
 *   
 * Name:
 *   SV_ARROHEADATTACHMENT
 * Type:
 *   ArroheadAttachment_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   ArrowheadAttach_None
 * Required:
 *   No
 * Notes:
 *   Only applies to a geometry of type GeomType_LineSegs and is the arrowhead
 *   attachment may have any of the following values: ArrowheadAttach_None,
 *   ArrowheadAttach_AtBeginning, ArrowheadAttach_AtEnd,
 *   ArrowheadAttach_AtBothEnds,
 *   
 * Name:
 *   SV_ARROWHEADSIZE
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   5.0
 * Required:
 *   No
 * Notes:
 *   Only applies to a geometry of type GeomType_LineSegs and is the arrowhead
 *   size if assigned. The value must be greater than or equal to zero and less
 *   than or equal to 50
 *   
 * Name:
 *   SV_ARROWHEADANGLE
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   12.0
 * Required:
 *   No
 * Notes:
 *   Only applies to a geometry of type GeomType_LineSegs and is the arrowhead
 *   angle must be greater than or equal to 1 and less than or equal to 90
 *   
 * Name:
 *   SV_NUMELLIPSEPTS
 * Type:
 *   LgIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   72.0
 * Required:
 *   No
 * Notes:
 *   Only applies to a geometry of type GeomType_Ellipse and is the number of
 *   points used to define an ellipse. The value must be greater than or equal
 *   to 3 and less than or equal to 720
 *   
 * Name:
 *   SV_XGEOMDATA
 * Type:
 *   float *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Required:
 *   Yes
 * Notes:
 *   Floating point arrays containing the point data for the specific geometry
 *   
 * Name:
 *   SV_YGEOMDATA
 * Type:
 *   float *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_ZGEOMDATA
 * Type:
 *   float *
 * Arg Function:
 *   TecUtilArgListAppendArray()
 * Required:
 *   Yes
 *   
 * Name:
 *   SV_XPOS
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.0
 * Required:
 *   No
 * Notes:
 *   Anchor position for the geometry
 *   
 * Name:
 *   SV_YPOS
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
 *   SV_ZPOS
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
 *   SV_POSITIONCOORDSYS
 * Type:
 *   CoordSys_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   CoordSys_Grid
 * Required:
 *   No
 * Notes:
 *   Coordinate system to which the geometry belongs and may have any of the
 *   following values: CoordSys_Grid, CoordSys_Frame, CoordSys_Grid3D,
 *   
 * Name:
 *   SV_ATTACHTOZONE
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   A value of TRUE indicates that the geometry is attached to the specified
 *   zone or map. A value of FALSE indicates that the geometry is not attached
 *   to a specific zone or map
 *   
 * Name:
 *   SV_ZONE
 * Type:
 *   EntIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   1
 * Required:
 *   No
 * Notes:
 *   Zone or map number to which the geometry is attached
 *   
 * Name:
 *   SV_COLOR
 * Type:
 *   ColorIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Black_C
 * Required:
 *   No
 * Notes:
 *   Line color for the geometry may be a value greater than or equal to zero
 *   
 * Name:
 *   SV_FILLCOLOR
 * Type:
 *   ColorIndex_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   White_C
 * Required:
 *   No
 * Notes:
 *   Fill color for the geometry may be a value greater than or equal to zero
 *   
 * Name:
 *   SV_ISFILLED
 * Type:
 *   Boolean_t
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   FALSE
 * Required:
 *   No
 * Notes:
 *   If applicable a value of TRUE indicates the geometry is filled.
 *   
 * Name:
 *   SV_LINEPATTERN
 * Type:
 *   LinePattern_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   LinePattern_Solid
 * Required:
 *   No
 * Notes:
 *   Line pattern to use for the drawing the lines and may have any ofthe
 *   following values: LinePattern_Solid, LinePattern_Dashed,
 *   LinePattern_DashDot, LinePattern_Dotted, LinePattern_LongDash,
 *   LinePattern_DashDotDot.
 *   
 * Name:
 *   SV_PATTERNLENGTH
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   2.0
 * Required:
 *   No
 * Notes:
 *   Pattern length may be a value greater than or equal to zero and less than
 *   or equal to 100
 *   
 * Name:
 *   SV_LINETHICKNESS
 * Type:
 *   double
 * Arg Function:
 *   TecUtilArgListAppendDouble()
 * Default:
 *   0.1
 * Required:
 *   No
 * Notes:
 *   Line thickness may be a value greater than or equal to zero and less than
 *   or equal to 100
 *   
 * Name:
 *   SV_SCOPE
 * Type:
 *   Scope_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Scope_Local
 * Required:
 *   No
 * Notes:
 *   A geometry with local scope is displayed only in the frame in which it is
 *   created. If the the geometry is defined as having global scope it will
 *   appear in all "like" frames, that is, those frames using the same data set
 *   as the one in which the geometry was created. Scope may be either
 *   Scope_Global or Scope_Local.
 *   
 * Name:
 *   SV_CLIPPING
 * Type:
 *   Clipping_e
 * Arg Function:
 *   TecUtilArgListAppendInt()
 * Default:
 *   Clipping_ClipToViewport
 * Required:
 *   No
 * Notes:
 *   Clipping applied to the geometry may be either Clipping_ClipToViewport or
 *   Clipping_ClipToFrame.
 *   
 * Name:
 *   SV_MACROFUNCTIONCOMMAND
 * Type:
 *   char *
 * Arg Function:
 *   TecUtilArgListAppendString()
 * Default:
 *   NULL
 * Required:
 *   No
 * Notes:
 *   Macro function command associated with the geometry
 * </ArgListTable>
 *   
 * @return
 *   TRUE if the input parameters are valid and the data was successfully
 *   written, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecGeoX(ArgListPtr)
 *    POINTER (ArgListPtr, ArgList)
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecGeoX(ArgList_pa ArgList);

/**
 * Writes the name/value data set auxiliary data pair to the data file..
 *   
 * @param Name
 *   Name of the data set auxiliary item
 *   
 * @param Value
 *   The value associates with the named data set auxiliary data item
 *   
 * @return
 *   0 if successful, -1 if unsuccessful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecAux(
 *   &                   Name,
 *   &                   Value)
 *    CHARACTER*(*)   Name
 *    CHARACTER*(*)   Value
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecAux(char *Name,
                                            char *Value);

/**
 * Writes the name/value zone auxiliary data pair to the data file.
 *   
 * @param Name
 *   Name of the zone auxiliary data item.
 *   
 * @param Value
 *   The value associated with the named zone auxiliary data item
 *   
 * @return
 *   0 if successful, -1 if unsuccessful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecZAux(
 *   &                   Name,
 *   &                   Value)
 *    CHARACTER*(*)   Name
 *    CHARACTER*(*)   Value
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecZAux(char *Name,
                                             char *Value);

/**
 * Writes the name/value variable auxiliary data pair to the data file.
 *
 * @since
 *   10.0-3-129
 *   
 * @param Var
 *   Variable number assoicated with the auxiliary data.
 *
 * @param Name
 *   Name of the variable auxiliary data item.
 *
 * @param Value
 *   The value associated with the named variable auxiliary data item.
 *   
 * @return
 *   0 if successful, -1 if unsuccessful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecVAux(
 *   &                   Var,
 *   &                   Name,
 *   &                   Value)
 *    INTEGER*4       Var
 *    CHARACTER*(*)   Name
 *    CHARACTER*(*)   Value
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecVAux(LgIndex_t *Var,
                                             char      *Name,
                                             char      *Value);

/**
 * Writes the face neighbor connections to the data file.
 *   
 * @param FaceConnections
 *   Array of face connections dimensioned by the number of face neighbor
 *   connections (supplied in the call to TecUtilTecZneX()) multiplied by the
 *   number of values needed for each connection. See the ADK reference manual
 *   for details.
 *   
 * @return
 *   0 if successful, -1 if unsuccessful.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTecFace(FaceConnections)
 *    INTEGER*4 FaceConnections
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilTecFace(LgIndex_t *FaceConnections);


/**
 *   Get the clipping properties of a text object.
 *   
 * @param TID
 *   ID of the text object
 *   
 * @return
 *   Returns the clipping type. Can be one of Clipping_ClipToViewport or Clipping_ClipToFrame.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetClipping(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON Clipping_e    STDCALL TecUtilTextGetClipping(Text_ID TID);

/**
 * Get the anchor coordinate position of the text object in the current
 * coordinate system.
 *   
 * @param TID
 *   Handle to a text object
 *   
 * @param XOrThetaPos
 *   Pointer to the text object's X or Theta anchor position.
 *   
 * @param YOrRPos
 *   Pointer to the text object's Y or R (radius) anchor position
 *   
 * @param ZPos
 *   Pointer to the text object's anchor Z position.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextGetAnchorPos(
 *   &           TID,
 *   &           XOrThetaPos,
 *   &           YOrRPos,
 *   &           ZPos)
 *    INTEGER*4       TID
 *    REAL*8          XOrThetaPos
 *    REAL*8          YOrRPos
 *    REAL*8          ZPos
 * </FortranSyntax>
 *   
 *   Text_ID Text;
 *   
 * @code
 *   double  XOrThetaPos;
 *   double  YOrRPos;
 *   double  ZPos;
 *   
 *   // use TecUtilTextDelete() when 'Text' is no longer needed 
 *   Text = TecUtilTextCreate(CoordSys_Frame, 50.0, 50.0,
 *                            Units_Point,30.0, "Hello");
 *   if (Text != TECUTILBADID)
 *     {
 *       // do all sorts of things 
 *         .
 *         .
 *         .
 *   
 *       // get the current anchor position of the text 
 *       TecUtilTextGetAnchorPos(Text, &XOrThetaPos, &YOrRPos,
 *    &ZPos);
 *     }
 * @endcode
 *
 * @sa TecUtilTextSetCoordSysAndUnits()
 */
LINKTOADDON void          STDCALL TecUtilTextGetAnchorPos(Text_ID TID,
                                                          double  *XOrThetaPos,
                                                          double  *YOrRPos,
                                                          double  *ZPos);

/**
 * @deprecated
 *   Please use TecUtilTextGetAnchorPos() instead.
 */
LINKTOADDON void          STDCALL TecUtilTextGetXYPos (Text_ID  TID,
                                                       double  *XPos,
                                                       double  *YPos);
/**
 *   Get the coordinate system to which the text is associated.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   Text object's coordinate system. The possible values are: CoorSys_Grid, CoorSys_Frame.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetPositionCoordSys(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON CoordSys_e    STDCALL TecUtilTextGetPositionCoordSys (Text_ID TID);
/**
 * Get the zone or map with which the text object is associated (if it is
 * attached).
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   Zone or map.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetZoneOrMap(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 *
 * @sa TecUtilTextIsAttached() and TecUtilTextSetAttached().
 */
LINKTOADDON EntIndex_t    STDCALL TecUtilTextGetZoneOrMap (Text_ID TID);
/**
 * Determine if the text object is attached to a zone or map.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   TRUE if attached, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextIsAttached(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 *
 * @sa TecUtilTextGetZoneOrMap() and TecUtilGeomSetZoneOrMap().
 */
LINKTOADDON Boolean_t     STDCALL TecUtilTextIsAttached (Text_ID TID);
/**
 *   Get the color of the text object.
 *   
 * @param TID
 *   Handle to a text object
 *   
 * @return
 *   Text color. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C, Purple_C, Yellow_C,
 *   White_C, CustomXX_C where XX is in the range 1-64.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetColor(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON ColorIndex_t  STDCALL TecUtilTextGetColor (Text_ID TID);
/**
 *   Get the font used for the text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   Text font. The possible values are: Font_Helvetica, Font_HelveticaBold, Font_Greek, Font_Math,
 *   Font_UserDefined, Font_Times, Font_TimesItalic, Font_TimesItalicBold, Font_TimesBold,
 *   Font_Courier, Font_CourierBold.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetFont(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON Font_e        STDCALL TecUtilTextGetFont (Text_ID TID);
/**
 *   Get the text height in the currently defined text size units.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   Text height measured in the currently defined text size units.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilTextGetHeight(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON double        STDCALL TecUtilTextGetHeight (Text_ID TID);
/**
 * Get the size units for the text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   Text size units. The possible values are: Units_Grid, Units_Frame or Units_Point.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetSizeUnits(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 *
 * @sa TecUtilTextGetHeight()
 */
LINKTOADDON Units_e       STDCALL TecUtilTextGetSizeUnits (Text_ID TID);
/**
 * Get the type of the box surrounding the text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   The possible values are: TextBox_None, TextBox_Filled, TextBox_Hollow.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextBoxGetType(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON TextBox_e     STDCALL TecUtilTextBoxGetType (Text_ID TID);
/**
 *   Get the margin between the text and the box surrounding the text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   The text box margin in frame units.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilTextBoxGetMargin(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON double        STDCALL TecUtilTextBoxGetMargin (Text_ID TID);
/**
 *   Get the line thickness of the text box border.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   The text box line thickness in frame units.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilTextBoxGetLineThickness(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON double        STDCALL TecUtilTextBoxGetLineThickness (Text_ID TID);
/**
 *   Get the line color of the box surrounding the text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   The text box color.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextBoxGetColor(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON ColorIndex_t  STDCALL TecUtilTextBoxGetColor (Text_ID TID);
/**
 *   Get the fill color of the box surrounding the text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   The text box fill color.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextBoxGetFillColor(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON ColorIndex_t  STDCALL TecUtilTextBoxGetFillColor (Text_ID TID);
/**
 *   Get the text angle.
 *   
 * @param TID
 *   Handle to a text object
 *   
 * @return
 *   The text angle in degrees.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilTextGetAngle(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON double        STDCALL TecUtilTextGetAngle (Text_ID TID);
/**
 *   Get the text anchor style.
 *   
 * @param TID
 *   Handle to a text object
 *   
 * @return
 *   The text anchor style. The possible values are: TextAnchor_Left, TextAnchor_Center,
 *   TextAnchor_Right, TextAnchor_MidLeft, TextAnchor_MidCenter, TextAnchor_MidRight,
 *   TextAnchor_HeadLeft, TextAnchor_HeadCenter, TextAnchor_HeadRight, TextAnchor_OnSide.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetAnchor(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON TextAnchor_e  STDCALL TecUtilTextGetAnchor (Text_ID TID);
/**
 * Get the spacing between lines of text.
 *
 * @par Note:
 *   A newline (\\n) character within a string causes the line to wrap at that
 *   point.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   Vertical line spacing between multiple lines of a ext object.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilTextGetLineSpacing(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON double        STDCALL TecUtilTextGetLineSpacing (Text_ID TID);
/**
 *   Get the scope of the text object. Text with local scope is displayed only in the frame in which it
 *   is created. If the the text is defined as having global scope it will appear in all "like"
 *   frames, that is, those frames using the same data set as the one in which the text was created.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   Text scope. The possible values are: Scope_Local or Scope_Global.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetScope(TID)
 *    INTEGER*4 TID
 * </FortranSyntax>
 */
LINKTOADDON Scope_e       STDCALL TecUtilTextGetScope (Text_ID TID);



/**
 * Get the macro function command string associated with the text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @param MacroFunctionCommand
 *   Handle to a macro function command string. The result must be deallocated
 *   with TecUtilStringDealloc() when it is no longer needed
 *   
 * @return
 *   TRUE if sufficient memory is available for the string, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetMacroFunctionCmd(
 *   &                   TID,
 *   &                   MacroFunctionCommand,
 *   &                   MacroFunctionCommandLength)
 *    INTEGER*4       TID
 *    CHARACTER*(*)   MacroFunctionCommand
 *    INTEGER*4       MacroFunctionCommandLength
 * </FortranSyntax>
 *   
 *   Boolean_t IsOk;
 *   
 * @code
 *   Text_ID   Text;
 *   char      *MacroCommand = NULL;
 *     .
 *     .
 *     .
 *   
 *   // get the text's associated macro command 
 *   IsOk = TecUtilTextGetMacroFunctionCmd(Text, &MacroCommand);
 *   if (IsOk)
 *     {
 *       // do something with command 
 *         .
 *         .
 *         .
 *   
 *       // cleanup; macro command no longer needed 
 *       TecUtilStringDealloc(&MacroCommand);
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t     STDCALL TecUtilTextGetMacroFunctionCmd (Text_ID   TID,
                                                                  char    **MacroFunctionCommand);
/**
 * Get the string associated with the text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @param TextString
 *   String of the text object. Result must be deallocated with
 *   TecUtilStringDealloc() when no longer needed.
 *   
 * @return
 *   TRUE if sufficient memory is avialable for the string, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextGetString(
 *   &                   TID,
 *   &                   TextString,
 *   &                   TextStringLength)
 *    INTEGER*4       TID
 *    CHARACTER*(*)   TextString
 *    INTEGER*4       TextStringLength
 * </FortranSyntax>
 *   
 *   Text_ID Text;
 *   
 * @code
 *   Boolean_t IsOk = FALSE;
 *   char      *TextString = NULL;
 *   
 *   Text = TecUtilTextCreate(CoordSys_Frame,50.0, 50.0, Units_Point,
 *                            30.0, "Hello");
 *   if (Text != TECUTILBADID)
 *     {
 *       // do some things with the text 
 *         .
 *         .
 *         .
 *   
 *       // change the string of the text object 
 *       TecUtilTextSetString(Text, "Hello World");
 *   
 *       // do some more things 
 *         .
 *         .
 *         .
 *   
 *       // print the contents of the text 
 *       // string to standard output      
 *       IsOk = TecUtilTextGetString(Text, &TextString);
 *       if (IsOk)
 *         {
 *           printf("%s\n", TextString);
 *           TecUtilStringDealloc(&TextString);
 *         }
 *     }
 * @endcode
 */
LINKTOADDON Boolean_t     STDCALL TecUtilTextGetString (Text_ID   TID,
                                                        char    **TextString);
/**
 * Get the next text object, relative to the specified text object, from the
 * list of text objects maintained by the current frame.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @return
 *   Text object following the specified text object. If the specified text
 *   object is the last in the list then \ref TECUTILBADID is returned.
 *   
 * <FortranSyntax>
 *   SUBROUTINE TecUtilTextGetNext(
 *  &           TIDPtr,
 *  &           ResultPtr)
 *   POINTER        (TIDPtr, TID)
 *   POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 */
LINKTOADDON Text_ID       STDCALL TecUtilTextGetNext (Text_ID TID);
/**
 * Get the previous text object, relative to the specified text object, from the list of text objects
 * maintained by the current frame.
 *   
 * @param TID
 *   Handle to a text object defined in the current frame.
 *   
 * @return
 *   Text object preceeding the specified text object. If the specified text object is the first in the
 *   list then the last text object in the list is returned.
 *   
 * <FortranSyntax>
 *   SUBROUTINE TecUtilTextGetPrev(
 *  &           TIDPtr,
 *  &           ResultPtr)
 *   POINTER        (TIDPtr, TID)
 *   POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *
 * @sa TecUtilTextGetBase() and TecUtilTextGetNext().
 */
LINKTOADDON Text_ID       STDCALL TecUtilTextGetPrev (Text_ID TID);

/**
 */
/**
 *   Function will get the clipping properties of a geometry.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetClipping(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 *   
 *   Create a red circle and set the clipping to "ClipToFrame":
 *   
 * @code
 *   Geom_ID g;
 *   Clipping_e clip;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   clip = TecUtilGeomGetClipping(g);
 * @endcode
 */
LINKTOADDON Clipping_e    STDCALL TecUtilGeomGetClipping(Geom_ID GID);
/**
 */
/**
 *   Gets the anchor postion of the specified geometry..
 *   
 * @param GID
 *   Geometry ID. The must be an ellipse geometry
 *   
 * @param XOrThetaPos
 *   The X or Theta axis position of the geometry anchor
 *   
 * @param YOrRPos
 *   The Y or Radian axis position of the geometry anchor
 *   
 * @param ZPos
 *   The Z axis position of the geometry anchor
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomGetAnchorPos(
 *   &           GID,
 *   &           XOrThetaPos,
 *   &           YOrRPos,
 *   &           ZPos)
 *    INTEGER*4       GID
 *    REAL*8          XOrThetaPos
 *    REAL*8          YOrRPos
 *    REAL*8          ZPos
 * </FortranSyntax>
 *   
 *   Get the achor position of a newly created circle:
 *   
 * @code
 *   
 *   double XPos, YPos, ZPos;
 *   
 *   Geom_ID Geom;
 *   Geom = TecUtilGeomCircleCreate(CoordSys_Grid,
 *     4.0, 3.0, 5.0);
 *   
 *   TecUtilGeomGetAnchorPos(Geom, &XPos, &YPos, &ZPos);
 * @endcode
 */
LINKTOADDON void          STDCALL TecUtilGeomGetAnchorPos(Geom_ID  GID,
                                                          double  *XOrThetaPos,
                                                          double  *YOrRPos,
                                                          double  *ZPos);

/**
 *   Queries the state of the "preserve aspect ratio" toggle for an image geometry.
 *   
 * @param GID
 *   Geometry ID. Must be an image geometry
 *   
 * @param MaintainAspectRatio
 *   TRUE to preserve the aspect ratio when drawing an image geometry
 *   
 * @return
 *   TRUE if the "preserve aspect ratio" toggle is set, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageSetUseRatio(
 *   &           GID,
 *   &           MaintainAspectRatio)
 *    INTEGER*4       GID
 *    INTEGER*4       MaintainAspectRatio
 * </FortranSyntax>
 *   
 *   Set the state of the preserve aspect ration toggle in an image to TRUE:
 *   
 * @code
 *   TecUtilGeomImageSetUseRatio(GID, TRUE);
 * @endcode
 */
LINKTOADDON void          STDCALL TecUtilGeomImageSetUseRatio(Geom_ID GID,
                                                              Boolean_t MaintainAspectRatio);


/**
 * @deprecated
 *   Please use TecUtilGeomGetAnchorPos() instead.
 */
LINKTOADDON void          STDCALL TecUtilGeomGetXYZAnchorPos(Geom_ID  GID,
                                                             double  *XPos,
                                                             double  *YPos,
                                                             double  *ZPos);
/**
 * Get the zone or Line-mapping to which the geometry is attached. Use
 * TecUtilGeomIsAttached() to determine whether or not the geometry is attached
 * at all.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @return
 *   The zone number or the Line-mapping number.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetZoneOrMap(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 *   
 * See TecUtilGeomIsAttached() for an example of using
 * TecUtilGeomGetZoneOrMap().
 */
LINKTOADDON EntIndex_t    STDCALL TecUtilGeomGetZoneOrMap (Geom_ID GID);




/**
 * Determine whether or not a geometry is attached to a zone or Line-mapping.
 * Use TecUtilGeomGetZoneOrMap() to get the zone or Line-mapping number to
 * which the geometry is attached.
 *   
 * @param GID
 *   Geometry ID. Must be an image geometry
 *   
 * @return
 *   TRUE if the geometry is attached to a zone or Line-mapping, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomIsAttached(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 *   
 *   Determine the zone or Line-mapping that a geometry is attached to:
 *   
 * @code
 *   extern Geom_ID g; // created elsewhere 
 *   EntIndex_t zone = 0;
 *   EntIndex_t LineMap = 0;
 *   if ( TecUtilFrameGetMode() == Frame_XY )
 *      LineMap = TecUtilGeomGetZoneOrMap(g);
 *   else if ( TecUtilFrameGetMode() != Frame_Sketch )
 *      zone = TecUtilGeomGetZoneOrMap(g);
 * @endcode
 */
LINKTOADDON Boolean_t     STDCALL TecUtilGeomIsAttached (Geom_ID GID);
/**
 *   Get the geometry line color.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @return
 *   The line color of the geometry. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C,
 *   Purple_C, Yellow_C, White_C, or CustomXX_C where XX ranges from 1 to 64.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetColor(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON ColorIndex_t  STDCALL TecUtilGeomGetColor (Geom_ID GID);
/**
 * Get the geometry fill color. Use TecUtilGeomGetIsFilled() to determine
 * whether or not the geometry is filled with a color.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @return
 *   The geometry fill color. The possible values are: Black_C, Blue_C, Red_C,
 *   Green_C, Cyan_C, Purple_C, Yellow_C, White_C, or CustomXX_C where XX
 *   ranges from 1 to 64.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetFillColor(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON ColorIndex_t  STDCALL TecUtilGeomGetFillColor (Geom_ID GID);
/**
 *   Determine if a geometry if filled.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @return
 *   TRUE if the geometry is filled, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetIsFilled(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t     STDCALL TecUtilGeomGetIsFilled (Geom_ID GID);
/**
 *   Get the geometry type.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @return
 *   The geometry type. This can be one of: GeomType_LineSegs (includes 2-D and 3-D line, polyline and
 *   multi-polyline geometries), GeomType_Rectangle, GeomType_Square, GeomType_Circle, GeomType_Ellipse.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetType(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON GeomForm_e    STDCALL TecUtilGeomGetType (Geom_ID GID);
/**
 *   Get the line pattern of a geometry.
 *   
 * @param GID
 *   ID of a geometry. This must be a circle or ellipse geometry
 *   
 * @return
 *   The geometry line pattern. The possible values are: LinePattern_Solid,
 *   LinePattern_Dashed, LinePattern_DashDot, LinePattern_Dotted,
 *   LinePattern_LongDash, LinePattern_DashDotDot.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetLinePattern(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON LinePattern_e STDCALL TecUtilGeomGetLinePattern (Geom_ID GID);
/**
 *   Get the geometry line pattern length.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @return
 *   The line pattern length in frame units.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGeomGetPatternLength(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON double        STDCALL TecUtilGeomGetPatternLength (Geom_ID GID);
/**
 *   Get the geometry line thickness.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @return
 *   The geometry line thickness in frame units.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGeomGetLineThickness(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON double        STDCALL TecUtilGeomGetLineThickness (Geom_ID GID);
/**
 *   Get the number of points used to draw a circle or ellipse geometry.
 *   
 * @param GID
 *   ID of a geometry. This must be a circle or ellipse geometry
 *   
 * @return
 *   The number of points used to draw the circle or geometry.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomEllipseGetNumPoints(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON SmInteger_t   STDCALL TecUtilGeomEllipseGetNumPoints (Geom_ID GID);
/**
 *   Get the geometry arrowhead style.
 *   
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *   
 * @return
 *   The arrowhead style. The possible values are: Arrowhead_Plain, Arrowhead_Filled or
 *   Arrowhead_Hollow.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomArrowheadGetStyle(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON ArrowheadStyle_e        STDCALL TecUtilGeomArrowheadGetStyle (Geom_ID GID);
/**
 *   Get the geometry arrowhead attachment.
 *   
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *   
 * @return
 *   The arrowhead attachment. The possible values are: ArrowheadAttach_None,
 *   ArrowheadAttach_AtBeginning, ArrowheadAttach_AtEnd,
 *   ArrowheadAttach_AtBothEnds.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomArrowheadGetAttach(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON ArrowheadAttachment_e   STDCALL TecUtilGeomArrowheadGetAttach (Geom_ID GID);
/**
 *   Get the geometry arrowhead size.
 *   
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *   
 * @return
 *   The arrowhead size in frame units.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGeomArrowheadGetSize(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON double        STDCALL TecUtilGeomArrowheadGetSize (Geom_ID GID);
/**
 *   Get the geometry arrowhead angle.
 *   
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *   
 * @return
 *   The arrowhead angle in degrees.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGeomArrowheadGetAngle(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON double        STDCALL TecUtilGeomArrowheadGetAngle (Geom_ID GID);
/**
 *   Get the geometry scope.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @return
 *   The geometry scope. The possible values are: Scope_Local (show in current
 *   frame only), Scope_Global (show in all frames with the same data set as
 *   the current frame).
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetScope(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON Scope_e       STDCALL TecUtilGeomGetScope (Geom_ID GID);
/**
 *   Get the geometry position coordinate system.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @return
 *   The coordinate system. The possible values are: CoordSys_Grid3D or CoordSys_Frame.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetPositionCoordSys(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON CoordSys_e    STDCALL TecUtilGeomGetPositionCoordSys (Geom_ID GID);
/**
 *   Get the name of the file associated with an image geometry.
 *   
 * @param GID
 *   Geometry ID. Must be an image geometry
 *   
 * @return
 *   The resize filter of the indicated geometry. See ImageResizeFilter_e.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomImageGetResizeFilter(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 *   
 *   Get the resize filter of a geometry.
 *   
 * @code
 *   ImageResizeFilter_e Filter =
 *                       TecUtilGeomImageSetResizeFilter(G);
 * @endcode
 */
LINKTOADDON ImageResizeFilter_e STDCALL TecUtilGeomImageGetResizeFilter (Geom_ID GID);
/**
 *   Sets the resize filter of an image geometry.
 *   
 * @param GID
 *   Geometry ID. Must be an image geometry
 *   
 * @param ResizeFilter
 *   Resize filter. See ImageResizeFilter_e.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageSetResizeFilter(
 *   &           GID,
 *   &           ResizeFilter)
 *    INTEGER*4       GID
 *    INTEGER*4       ResizeFilter
 * </FortranSyntax>
 *   
 *   Set the resize filter for an image geometry to Box:
 *   
 * @code
 *   TecUtilGeomImageSetResizeFilter(G, ImageResizeFilter_Box);
 * @endcode
 */
LINKTOADDON void          STDCALL TecUtilGeomImageSetResizeFilter(Geom_ID GID,
                                                                  ImageResizeFilter_e ResizeFilter);
/**
 * Get the name of the file associated with an image geometry.
 *   
 * @param GID
 *   Geometry ID. Must be an image geometry
 *   
 * @param FileName
 *   Receives the file name associated with this geometry. This is always an
 *   absolute path. You must call TecUtilStringDealloc() to free this string
 *   when you are done with it
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageGetFileName(
 *   &           GID,
 *   &           FileName,
 *   &           FileNameLength)
 *    INTEGER*4       GID
 *    CHARACTER*(*)   FileName
 *    INTEGER*4       FileNameLength
 * </FortranSyntax>
 *   
 *   Get the name of the file associated with a geometry.
 *   
 * @code
 *   char *GeomFileName = NULL;
 *   GTecUtilGeomImageGetFileName(G, &GeomFileName);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeomImageGetFileName(Geom_ID GID,
                                                     char **FileName);

/**
 *   Sets the width of an image geometry. Note that the size of a geometry when it is displayed in a
 *   frame is not necessarily the size of the image in the file. The image will be resized to fit the
 *   dimensions specified when calling this function.
 *   
 * @param GID
 *   Geometry ID. Must be an image geometry
 *   
 * @param Width
 *   New width of the geometry. Must be greater than 0.0
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageSetWidth(
 *   &           GID,
 *   &           Width)
 *    INTEGER*4       GID
 *    REAL*8          Width
 * </FortranSyntax>
 *   
 *   Set the width of an image geometry to 5.0:
 *   
 * @code
 *   TecUtilGeomImageSetWidth(GID, 5.0);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeomImageSetWidth(Geom_ID GID,
                                                 double Width);

/**
 *   Sets the Height of an image geometry. Note that the size of a geometry when it is displayed in a
 *   frame is not necessarily the size of the image in the file. The image will be resized to fit the
 *   dimension specified when calling this function.
 *   
 * @param GID
 *   Geometry ID. Must be an image geometryGeometry ID. Must be an image geometry
 *   
 * @param Height
 *   New height of the image, must be greater than 0.0
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageSetHeight(
 *   &           GID,
 *   &           Height)
 *    INTEGER*4       GID
 *    REAL*8          Height
 * </FortranSyntax>
 *   
 *   Set the height of an image geometry to 5.0:
 *   
 * @code
 *   TecUtilGeomImageSetHeight(GID, 5.0);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeomImageSetHeight(Geom_ID GID,
                                                 double Height);

/**
 *   Get the width and height of an image geometry.
 *   
 * @param GID
 *   Geometry ID. Must be an image geometry
 *   
 * @param Width
 *   Receives the width of the specified image geometry.
 *   
 * @param Height
 *   Receives the height of the specified image geometry.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageGetSize(
 *   &           GID,
 *   &           Width,
 *   &           Height)
 *    INTEGER*4       GID
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *   
 *   Get the width and height of an image in an image geometry.
 *   
 * @code
 *   double Width;
 *   double Height;
 *   TecUtilGeomImageGetFileName(GID, &Width, &Height);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeomImageGetSize(Geom_ID GID,
                                                 double *Width,
                                                 double *Height);

/**
 *   Resets the aspect ratio after any changes have been made in the position
 *   of an image geometry.
 *   
 * @param GID
 *   Geometry ID. Must be an image geometry
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageResetAspectRati(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 *
 * @par Note:
 *   IMPORTANT!  Note that the FORTRAN Name for this function is truncated
 *   to 31 characters!
 *   
 *   Reset the aspect ratio of an image geometry:
 *   
 * @code
 *   
 *   TecUtilGeomImageResetAspectRatio(GID);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeomImageResetAspectRatio(Geom_ID GID);



#if defined COMPILE_BUT_HIDE_FROM_DOC
LINKTOADDON Boolean_t STDCALL TecUtilGeomImageGetImage(Geom_ID         GID,
                                                       ScreenDim_t    *Width,
                                                       ScreenDim_t    *Height,
                                                       Byte_t **RGBData);
#endif

/**
 * Get the geometry macro function command.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @param MacroFunctionCmd
 *   Character string containing the macro command. You must free this string
 *   using TecUtilStringDealloc() when you are done with it
 *   
 * @return
 *   Returns TRUE if space can be allocated for the command string, otherwise
 *   FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetMacroFunctionCmd(
 *   &                   GID,
 *   &                   MacroFunctionCmd,
 *   &                   MacroFunctionCmdLength)
 *    INTEGER*4       GID
 *    CHARACTER*(*)   MacroFunctionCmd
 *    INTEGER*4       MacroFunctionCmdLength
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilGeomGetMacroFunctionCmd (Geom_ID   GID,
                                                              char    **MacroFunctionCmd);

/**
 * Create an image geometry. Use the ID obtained from this function to set
 * geometry attributes such as position and coordinates system.
 *   
 * @param FName
 *   Image file to attach. The format of this file must be Microsoft Windows
 *   Bitmap (*.bmp), JPEG (*.jpg or *.jpeg) or Portable Network Graphics
 *   (*.png)
 *   
 * @param CornerX
 *   X coordinate for the location to initially place the image (frame
 *   coordinates).
 *   
 * @param CornerY
 *   Y coordinate for the location to initially place the image (frame
 *   coordinates).
 *   
 * @param Size
 *   The default size of the image. You may change the size later using
 *   TecUtilImageSetWidth() and TecUtilImageSetHeight().
 *   
 * @return
 *   If successfully created, then the return is a valid ID that you may use to
 *   further set attributes for this geometry. Otherwise, \ref TECUTILBADID is
 *   returned. If the return value is \ref TECUTILBADID, then the most likely
 *   cause is the file does not exist.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomImageCreate(
 *   &           FName,
 *   &           CornerX,
 *   &           CornerY,
 *   &           Size,
 *   &           ResultPtr)
 *    CHARACTER*(*)  FName
 *    REAL*8         CornerX
 *    REAL*8         CornerY
 *    REAL*8         Size
 *    POINTER        (ResultPtr, Result)
 *    
 * </FortranSyntax>
 *   
 *   Create an image geometry anchored at (0.1,0.1) with a size of 0.5, using the file "myimage.png":
 *   
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeomImageCreate("c:\\myimage.png",0.1,0.1,0.5);
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomImageCreate(const char *FName,
                                                   double      CornerX,
                                                   double      CornerY,
                                                   double      Size);



/**
 * Get the next geometry in the list of geometries attached to the current
 * frame.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @return
 *   Returns the ID of the next geometry or \ref TECUTILBADID if there are no
 *   more geometries.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomGetNext(
 *   &           GIDPtr,
 *   &           ResultPtr)
 *    POINTER        (GIDPtr, GID)
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Change all geometries in the current frame to be red:
 *   
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomGetBase();
 *   while ( g != TECUTILBADID )
 *     {
 *       TecUtilGeomSetColor(g, Red_C);
 *       g = TecUtilGeomGetNext(g);
 *     }
 * @endcode
 */
LINKTOADDON Geom_ID       STDCALL TecUtilGeomGetNext (Geom_ID GID);
/**
 * Get the previous geometry in the list of geometries attached to the current
 * frame.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @return
 *   Returns the ID of the previous geometry or \ref TECUTILBADID if GID was
 *   the base geometry.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomGetPrev(
 *   &           GIDPtr,
 *   &           ResultPtr)
 *    POINTER        (GIDPtr, GID)
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Create a circle with color of the previous circle:
 *   
 * @code
 *   Geom_ID new_geom, prev_geom;
 *   new_geom = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   prev_geom = TecUtilGeomGetPrev(new_geom);
 *   while ( prev_geom != TECUTILBADID &&
 *           TecUtilGeomGetType(prev_geom) != Geom_Circle )
 *     prev_geom = TecUtilGeomGetPrev(prev_geom);
 *   if ( prev_geom != TECUTILBADID )
 *     TecUtilGeomSetColor(new_geom,TecUtilGeomGetColor(prev_geom));
 * @endcode
 */
LINKTOADDON Geom_ID       STDCALL TecUtilGeomGetPrev (Geom_ID GID);

/**
 */
/**
 *   Set the clipping properties of a text object.
 *   
 * @param TID
 *   ID of the text object
 *   
 * @param Clipping
 *   New clipping property for the text object. The possible values are: Clipping_ClipToViewport and
 *   Clipping_ClipToFrame.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetClipping(
 *   &           TID,
 *   &           Clipping)
 *    INTEGER*4       TID
 *    INTEGER*4       Clipping
 * </FortranSyntax>
 *   
 *   Create a texts string "Test Text Object" and set the clipping to "ClipToFrame":
 *   
 * @code
 *   Text_ID TID;
 *   TID = TecUtilTextCreate(CoordSys_Frame,20,30,
 *                           Units_Point,
 *                           15,"Test Text Object");
 *   TecUtilTextSetClipping(t, Clipping_ClipToFrame);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilTextSetClipping(Text_ID    TID,
                                                      Clipping_e Clipping);
/**
 */
/**
 * Set the XY-position for the text object. The text is always anchored in the
 * coordinate system specified by TecUtilTextSetCoordSysAndUnits() each time
 * the text object is drawn.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @param XOrThetaPos
 *   The text object's X or Theta anchor position.
 *   
 * @param YOrRPos
 *   The text object's Y or R (radius) anchor position.
 *   
 * @param ZPos
 *   The text object's Z position.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetAnchorPos(
 *   &           TID,
 *   &           XOrThetaPos,
 *   &           YOrRPos,
 *   &           ZPos)
 *    INTEGER*4       TID
 *    REAL*8          XOrThetaPos
 *    REAL*8          YOrRPos
 *    REAL*8          ZPos
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilTextSetAnchorPos(Text_ID TID,
                                                       double  XOrThetaPos,
                                                       double  YOrRPos,
                                                       double  ZPos);
/**
 * @deprecated
 *   Please use TecUtilTextSetAnchorPos() instead.
 */
LINKTOADDON void       STDCALL TecUtilTextSetXYPos (Text_ID TID,
                                                    double XPos,
                                                    double YPos);
/**
 * Set the coordinate system for the position and the units for the character height of a text
 * object. The text object's position and text height are adjusted so that it remains identical to
 * its visual appearance in the original coordinate and unit system.
 *   
 * @param TID
 *   Handle to the text object.
 *   
 * @param PositionCoordSys
 *   Coordinate system in which the text is positioned. The possible values
 *   are: \ref CoordSys_Frame or \ref CoordSys_Grid.
 *   
 * @param HeightUnits
 *   Units for the character height of the text. If CoordSys is
 *   \ref CoordSys_Frame, units must be \ref Units_Frame or \ref Units_Point.
 *   If CoordSys is \ref CoordSys_Grid, units must be \ref Units_Frame or
 *   \ref Units_Grid.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetCoordSysAndUnits(
 *   &           TID,
 *   &           PositionCoordSys,
 *   &           HeightUnits)
 *    INTEGER*4       TID
 *    INTEGER*4       PositionCoordSys
 *    INTEGER*4       HeightUnits
 * </FortranSyntax>
 *   
 *   Create a text which is positioned and sized in the grid coordinate system that reads "Hello."
 *   Then, change the text to be positioned and sized in the frame coordinate system :
 *   
 * @code
 *   Text_ID Text;
 *   Text = TecUtilTextCreate(CoordSys_Grid, 0.25, 0.25,
 *                            Units_Grid, 0.25, "Hello.");
 *   if (Text != TECUTILBADID)
 *     {
 *        .
 *        .
 *      // Change the text position and size to   
 *      // be in the frame coordinate system and  
 *      // set the text position to be the center 
 *      // of the frame coordinate system         
 *       TecUtilTextSetCoordSysAndUnits(Text,CoordSys_Frame,
 *                                      Units_Frame);
 *       TecUtilTextSetXYPos(Text, 50.0, 50.0);
 *         .
 *         .
 *       TecUtilTextDelete(Text);
 *     }
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilTextSetCoordSysAndUnits (Text_ID TID,
                                                               CoordSys_e PositionCoordSys,
                                                               Units_e    HeightUnits);
/**
 * Set the zone or map to which the text object is associated (if it is attached).
 *   
 * @param TID
 *   Handle to the text object.
 *   
 * @param ZoneOrMap
 *   Zone or Map.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetZoneOrMap(
 *   &           TID,
 *   &           ZoneOrMap)
 *    INTEGER*4       TID
 *    INTEGER*4       ZoneOrMap
 * </FortranSyntax>
 *
 * @sa TecUtilTextIsAttached() and TecUtilTextSetAttached().
 */
LINKTOADDON void       STDCALL TecUtilTextSetZoneOrMap (Text_ID TID,
                                                        EntIndex_t ZoneOrMap);
/**
 * Indicate if the the text object should be attached to a zone or map.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @param Attached
 *   Set to TRUE to attach, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetAttached(
 *   &           TID,
 *   &           Attached)
 *    INTEGER*4       TID
 *    INTEGER*4       Attached
 * </FortranSyntax>
 *
 * @sa TecUtilTextGetZoneOrMap() and TecUtilTextSetZoneOrMap().
 */
LINKTOADDON void       STDCALL TecUtilTextSetAttached (Text_ID TID,
                                                       Boolean_t Attached);
/**
 *   Set the color of a text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @param Color
 *   Text color. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C, Purple_C, Yellow_C,
 *   White_C, Custom1_C, Custom2_C, Custom3_C, Custom4_C, Custom5_C, Custom6_C, Custom7_C, Custom8_C.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetColor(
 *   &           TID,
 *   &           Color)
 *    INTEGER*4       TID
 *    INTEGER*4       Color
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilTextSetColor (Text_ID TID,
                                                    ColorIndex_t Color);
/**
 *   Set the font for a text object.
 *   
 * @param TID
 *   Handle to the text object.
 *   
 * @param Font
 *   Text font. The possible values are: Font_Helvetica, Font_HelveticaBold, Font_Greek, Font_Math,
 *   Font_UserDefined, Font_Times, Font_TimesItalic, Font_TimesItalicBold, Font_TimesBold,
 *   Font_Courier, Font_CourierBold
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetFont(
 *   &           TID,
 *   &           Font)
 *    INTEGER*4       TID
 *    INTEGER*4       Font
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilTextSetFont (Text_ID TID,
                                                   Font_e Font);
/**
 * Set the character height for a text object.
 *   
 * @param TID
 *   Handle to the text object.
 *   
 * @param Height
 *   Character height in the current text size units.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetHeight(
 *   &           TID,
 *   &           Height)
 *    INTEGER*4       TID
 *    REAL*8          Height
 * </FortranSyntax>
 *
 * @sa TecUtilTextSetCoordSysAndUnits()
 */
LINKTOADDON void       STDCALL TecUtilTextSetHeight (Text_ID TID,
                                                     double Height);
/**
 *   Set the type of the box surrounding the text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @param TextBoxType
 *   Text box type. The possible values are: TextBox_None, TextBox_Filled, TextBox_Hollow
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextBoxSetType(
 *   &           TID,
 *   &           TextBoxType)
 *    INTEGER*4       TID
 *    INTEGER*4       TextBoxType
 * </FortranSyntax>
 *   
 *   Create a hollow boxed text label.
 *   
 * @code
 *   Text_ID Text;
 *   Text = TecUtilTextCreate(CoordSys_Frame,50.0,50.0,Units_Points,
 *                              30.0,"Hi Mom");
 *   TecUtilTextBoxSetType(Text, TextBox_Hollow);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilTextBoxSetType (Text_ID TID,
                                                      TextBox_e TextBoxType);
/**
 *   Set the margin between the text and the box surrounding the text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @param Margin
 *   Margin between the text and the box in percentage of the text height
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextBoxSetMargin(
 *   &           TID,
 *   &           Margin)
 *    INTEGER*4       TID
 *    REAL*8          Margin
 * </FortranSyntax>
 *   
 *   Create a boxed text label with a box margin of 60 percent of the height of the text:
 *   
 * @code
 *   Text_ID Text;
 *   Text = TecUtilTextCreate(CoordSys_Frame,50.0,50.0,Units_Points,30.0,
 *                            "Hi Mom");
 *   TecUtilTextBoxSetType(Text, TextBox_Hollow);
 *   TecUtilTextBoxSetMargin(Text, 60.0);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilTextBoxSetMargin (Text_ID TID,
                                                        double Margin);
/**
 *   Set the line thickness of the box surrounding the text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @param LineThickness
 *   Line thickness of the box, in frame units
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextBoxSetLineThickness(
 *   &           TID,
 *   &           LineThickness)
 *    INTEGER*4       TID
 *    REAL*8          LineThickness
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilTextBoxSetLineThickness (Text_ID TID,
                                                               double LineThickness);
/**
 *   Set the line color for the box surrounding a text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @param BoxColor
 *   Line color of the box. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C, Purple_C,
 *   Yellow_C, White_C, CustomXX_C where XX is in the range 1-64.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextBoxSetColor(
 *   &           TID,
 *   &           BoxColor)
 *    INTEGER*4       TID
 *    INTEGER*4       BoxColor
 * </FortranSyntax>
 *   
 *   Create a boxed text label with the box color set to red.
 *   
 * @code
 *   Text_ID Text;
 *   Text = TecUtilTextCreate(CoordSys_Frame,50.0,50.0,Units_Point,
 *                            30.0,"Hi Mom");
 *   TecUtilTextBoxSetType(Text, TextBox_Hollow);
 *   TecUtilTextBoxSetColor(Text, Red_C);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilTextBoxSetColor (Text_ID TID,
                                                       ColorIndex_t BoxColor);
/**
 *   Set the fill color of the box surrounding a text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @param BoxFillColor
 *   Fill color of the box. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C, Purple_C,
 *   Yellow_C, White_C, CustomXX_C where XX is in the range 1-64.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextBoxSetFillColor(
 *   &           TID,
 *   &           BoxFillColor)
 *    INTEGER*4       TID
 *    INTEGER*4       BoxFillColor
 * </FortranSyntax>
 *   
 *   Create a filled boxed text label with a fill color of blue.
 *   
 * @code
 *   Text_ID Text;
 *   Text = TecUtilTextCreate(CoordSys_Frame,50.0,50.0,Units_Point,30.0,
 *                            "Hi Mom");
 *   TecUtilTextBoxSetType(Text, TextBox_Filled);
 *   TecUtilTextBoxSetColor(Text, Blue_C);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilTextBoxSetFillColor (Text_ID TID,
                                                           ColorIndex_t BoxFillColor);
/**
 *   Set the angle in degrees for a text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @param Angle
 *   Text angle in degrees that must be between the inclusive angles of -360 and 360.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetAngle(
 *   &           TID,
 *   &           Angle)
 *    INTEGER*4       TID
 *    REAL*8          Angle
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilTextSetAngle (Text_ID TID,
                                                    double Angle);
/**
 *   Set the anchor style for a text object.
 *   
 * @param TID
 *   Handle to a text object.
 *   
 * @param Anchor
 *   Anchor style. The possible values are: TextAnchor_Left, TextAnchor_Center, TextAnchor_Right,
 *   TextAnchor_MidLeft, TextAnchor_MidCenter, TextAnchor_MidRight, TextAnchor_HeadLeft,
 *   TextAnchor_HeadCenter, TextAnchor_HeadRight, TextAnchor_OnSide
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetAnchor(
 *   &           TID,
 *   &           Anchor)
 *    INTEGER*4       TID
 *    INTEGER*4       Anchor
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilTextSetAnchor (Text_ID TID,
                                                     TextAnchor_e Anchor);
/**
 *   Set the line spacing for a text object. Line spacing is dependent on the height of the text and
 *   the size unit system in which it is drawn.
 *   
 * @param TID
 *   Handle to the text object.
 *   
 * @param LineSpacing
 *   Vertical spacing between multiple lines of a text object. Multiple lines are achieved by inserting
 *   newline (\\n) characters within the string of the text object
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetLineSpacing(
 *   &           TID,
 *   &           LineSpacing)
 *    INTEGER*4       TID
 *    REAL*8          LineSpacing
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilTextSetLineSpacing (Text_ID TID,
                                                          double LineSpacing);
/**
 *   Set the scope of the text object. Text with local scope is displayed only in the frame in which it
 *   is created. If the the text is defined as having global scope it will appear in all "like"
 *   frames, that is, those frames using the same data set as the one in which the text was created.
 *   
 * @param TID
 *   Handle to the text object.
 *   
 * @param Scope
 *   Text scope. The possible values are Scope_Local or Scope_Global.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextSetScope(
 *   &           TID,
 *   &           Scope)
 *    INTEGER*4       TID
 *    INTEGER*4       Scope
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilTextSetScope (Text_ID TID,
                                                    Scope_e Scope);
/**
 *   Set the macro function command associated with a text object.
 *   
 * @param TID
 *   Handle to the text object.
 *   
 * @param Command
 *   Macro function command string.
 *   
 * @return
 *   TRUE if sufficient memory was available to make a copy of Command, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextSetMacroFunctionCmd(
 *   &                   TID,
 *   &                   Command)
 *    INTEGER*4       TID
 *    CHARACTER*(*)   Command
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t  STDCALL TecUtilTextSetMacroFunctionCmd (Text_ID TID,
                                                               const char *Command);
/**
 *   Set the text string for a text object.
 *   
 * @param TID
 *   Handle to the text object.
 *   
 * @param TextString
 *   String copied into the text object.
 *   
 * @return
 *   TRUE if sufficient memory exists for the string copy, otherwise FALSE.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilTextSetString(
 *   &                   TID,
 *   &                   TextString)
 *    INTEGER*4       TID
 *    CHARACTER*(*)   TextString
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t  STDCALL TecUtilTextSetString (Text_ID TID,
                                                     const char *TextString);

/**
 */
/**
 *   Set the clipping properties of a geometry.
 *   
 * @param GID
 *   ID of the geometry
 *   
 * @param Clipping
 *   New clipping property for the geometry. The possible values are: Clipping_ClipToViewport and
 *   Clipping_ClipToFrame
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetClipping(
 *   &           GID,
 *   &           Clipping)
 *    INTEGER*4       GID
 *    INTEGER*4       Clipping
 * </FortranSyntax>
 *   
 *   Create a red circle and set the clipping to "ClipToFrame":
 *   
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilGeomSetClipping(g, Clipping_ClipToFrame);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomSetClipping(Geom_ID    GID,
                                                      Clipping_e Clipping);
/**
 */
/**
 *   Set the anchor position for a geometry. For circles and ellipses, this is the center. For squares
 *   and rectangles, this is the base corner. For lines, this is the offset added to all points of the
 *   geometry.
 *   
 * @param GID
 *   ID of a geometry
 *   
 * @param XPos
 *   X-anchor position of geometry
 *   
 * @param YPos
 *   Y-anchor position of geometry
 *   
 * @param ZPos
 *   Z-anchor position of geometry (3-D geometries only)
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetAnchorPos(
 *   &           GID,
 *   &           XPos,
 *   &           YPos,
 *   &           ZPos)
 *    INTEGER*4       GID
 *    REAL*8          XPos
 *    REAL*8          YPos
 *    REAL*8          ZPos
 * </FortranSyntax>
 *   
 *   Create a circle and then move it:
 *   
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilRedraw();
 *   TecUtilGeomSetAnchorPos(45., 45., 0.);
 *   TecUtilRedraw();
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomSetAnchorPos(Geom_ID GID,
                                                       double  XPos,
                                                       double  YPos,
                                                       double  ZPos);
/**
 * @deprecated
 *   Please use TecUtilGeomGetAnchorPos() instead.
 */
LINKTOADDON void       STDCALL TecUtilGeomSetXYZAnchorPos(Geom_ID GID,
                                                          double  XPos,
                                                          double  YPos,
                                                          double  ZPos);
/**
 * Set the zone or Line-mapping attachment for a geometry. Use
 * TecUtilGeomSetAttached() to specify whether or not the geometry is attached
 * to a zone or Line-mapping.
 *   
 * @param GID
 *   ID of a geometry
 *   
 * @param ZoneOrMap
 *   Zone number or mapping number to which the geometry should be attached
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetZoneOrMap(
 *   &           GID,
 *   &           ZoneOrMap)
 *    INTEGER*4       GID
 *    INTEGER*4       ZoneOrMap
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilGeomSetZoneOrMap (Geom_ID GID,
                                                        EntIndex_t ZoneOrMap);
/**
 * Set whether or not a geometry is attached to a zone or Line-mapping. Use
 * TecUtilGeom() to set which zone or Line-mapping the geometry is attached to.
 *   
 * @param GID
 *   Id of the geometry.
 *   
 * @param Attached
 *   TRUE to attach the geometry to a zone or an Line-mapping.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetAttached(
 *   &           GID,
 *   &           Attached)
 *    INTEGER*4       GID
 *    INTEGER*4       Attached
 * </FortranSyntax>
 *   
 *   Attach a geometry to zone or mapping 5:
 *   
 * @code
 *   extern Geom_ID g; // created elsewhere 
 *   TecUtilGeomSetAttached(g, TRUE);
 *   TecUtilGeomSetZoneOrMap(g, 5);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomSetAttached (Geom_ID GID,
                                                       Boolean_t Attached);
/**
 *   Set the line color of a geometry.
 *   
 * @param GID
 *   ID of the geometry
 *   
 * @param Color
 *   New line color for the geometry. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C,
 *   Purple_C, Yellow_C, White_C, or CustomXX_C where XX ranges from 1 to 64.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetColor(
 *   &           GID,
 *   &           Color)
 *    INTEGER*4       GID
 *    INTEGER*4       Color
 * </FortranSyntax>
 *   
 *   Create a red circle:
 *   
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilGeomSetColor(g, Red_C);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomSetColor (Geom_ID GID,
                                                     ColorIndex_t Color);
/**
 * Set the fill color of a geometry. Use TecUtilGeomSetIsFilled() to specify
 * whether or not a geometry is filled with color.
 *   
 * @param GID
 *   ID of the geometry
 *   
 * @param FillColor
 *   New fill color for the geometry. The possible values are: Black_C, Blue_C, Red_C, Green_C, Cyan_C,
 *   Purple_C, Yellow_C, White_C, or CustomXX_C where XX ranges from 1 to 64
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetFillColor(
 *   &           GID,
 *   &           FillColor)
 *    INTEGER*4       GID
 *    INTEGER*4       FillColor
 * </FortranSyntax>
 *   
 *   Create a red circle filled with yellow:
 *   
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilGeomSetColor(g, Red_C);
 *   TecUtilGeomSetFillColor(g, Yellow_C);
 *   TecUtilGeomSetIsFilled(g, TRUE);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomSetFillColor (Geom_ID GID,
                                                         ColorIndex_t FillColor);
/**
 *   Set whether or not a geometry is filled with a color. Use TecUtilGeomSetFillColor() to specify the
 *   actual color to fill the geometry with.
 *   
 * @param GID
 *   ID of a geometry
 *   
 * @param IsFilled
 *   TRUE to fill the geometry, FALSE to not fill.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetIsFilled(
 *   &           GID,
 *   &           IsFilled)
 *    INTEGER*4       GID
 *    INTEGER*4       IsFilled
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilGeomSetIsFilled (Geom_ID GID,
                                                       Boolean_t IsFilled);
/**
 *   Set the line pattern for a geometry.
 *   
 * @param GID
 *   ID of a geometry
 *   
 * @param LinePattern
 *   Line pattern for the geometry. The possible values are LinePattern_Solid, LinePattern_Dashed,
 *   LinePattern_DashDot, LinePattern_Dotted, LinePattern_LongDash or LinePattern_DashDotDot
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetLinePattern(
 *   &           GID,
 *   &           LinePattern)
 *    INTEGER*4       GID
 *    INTEGER*4       LinePattern
 * </FortranSyntax>
 *   
 *   Create a dotted circle:
 *   
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilGeomSetLinePattern(g, LinePattern_Dotted);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomSetLinePattern (Geom_ID GID,
                                                          LinePattern_e LinePattern);
/**
 *   Set the line pattern length for a geometry.
 *   
 * @param GID
 *   ID of a geometry
 *   
 * @param PatternLength
 *   Length of the line pattern in frame units.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetPatternLength(
 *   &           GID,
 *   &           PatternLength)
 *    INTEGER*4       GID
 *    REAL*8          PatternLength
 * </FortranSyntax>
 *   
 *   Create two concentric dashed circles of different line pattern lengths (two and ten percent):
 *   
 * @code
 *   Geom_ID g1, g2;
 *   g1 = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 20.);
 *   TecUtilGeomSetLinePattern(g1, Pattern_Dashed);
 *   TecUtilGeomSetPatternLength(g1, 2.);
 *   g2 = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 30.);
 *   TecUtilGeomSetLinePattern(g2, Pattern_Dashed);
 *   TecUtilGeomSetPatternLength(g2, 10.);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomSetPatternLength (Geom_ID GID,
                                                            double PatternLength);
/**
 *   Set the line thickness for a geometry.
 *   
 * @param GID
 *   ID of a geometry
 *   
 * @param LineThickness
 *   Thickness of the lines in frame units
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetLineThickness(
 *   &           GID,
 *   &           LineThickness)
 *    INTEGER*4       GID
 *    REAL*8          LineThickness
 * </FortranSyntax>
 *   
 *   Create a circle with five percent thick lines:
 *   
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilGeomSetLineThickness(g, 10.);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomSetLineThickness (Geom_ID GID,
                                                            double LineThickness);
/**
 *   Set the number of points used to draw a circle or an ellipse geometry.
 *   
 * @param GID
 *   GID of a geometry. This must be a circle or ellipse geometry
 *   
 * @param NumEllipsePts
 *   The number of points use to draw the circle or ellipse. This must be at least three points
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomEllipseSetNumPoints(
 *   &           GID,
 *   &           NumEllipsePts)
 *    INTEGER*4       GID
 *    INTEGER*4       NumEllipsePts
 * </FortranSyntax>
 *   
 *   Create a circle approximated by only five points. (This will look like a pentagon.)
 *   
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeomCircleCreate(CoordSys_Frame, 50., 50., 25.);
 *   TecUtilGeomEllipseSetNumPoints(g, 5);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomEllipseSetNumPoints (Geom_ID GID,
                                                            SmInteger_t NumEllipsePts);
/**
 *   Set the arrowhead style for a geometry.
 *   
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *   
 * @param ArrowheadStyle
 *   The arrowhead style. The possible values are: Arrowhead_Plain, Arrowhead_Filled or
 *   Arrowhead_Hollow.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomArrowheadSetStyle(
 *   &           GID,
 *   &           ArrowheadStyle)
 *    INTEGER*4       GID
 *    INTEGER*4       ArrowheadStyle
 * </FortranSyntax>
 *   
 *   Create a line with a filled arrowhead at the end:
 *   
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeom2DLineSegmentCreate(CoordSys_Frame, 5., 5., 95., 95.);
 *   TecUtilGeomArrowheadSetAttach(g, ArrowheadAttach_AtEnd);
 *   TecUtilGeomArrowheadSetStyle(g, Arrowhead_Filled);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomArrowheadSetStyle (Geom_ID GID,
                                                             ArrowheadStyle_e ArrowheadStyle);
/**
 *   Set the arrowhead attachment for a geometry.
 *   
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *   
 * @param ArrowheadAttachment
 *   The arrowhead attachment style. The possible values are: ArrowheadAttach_None
 *   ArrowheadAttach_AtBeginning ArrowheadAttach_AtEnd ArrowheadAttach_AtBothEnds
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomArrowheadSetAttach(
 *   &           GID,
 *   &           ArrowheadAttachment)
 *    INTEGER*4       GID
 *    INTEGER*4       ArrowheadAttachment
 * </FortranSyntax>
 *   
 *   Create a line with arrowheads at both ends:
 *   
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeom2DLineSegmentCreate(CoordSys_Frame, 5., 5., 95., 95.);
 *   TecUtilGeomArrowheadSetAttach(g, ArrowheadAttach_AtBothEnds);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomArrowheadSetAttach (Geom_ID GID,
                                                              ArrowheadAttachment_e ArrowheadAttachment);
/**
 *   Set the arrowhead size for a geometry.
 *   
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *   
 * @param ArrowheadSize
 *   The arrowhead size in frame units
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomArrowheadSetSize(
 *   &           GID,
 *   &           ArrowheadSize)
 *    INTEGER*4       GID
 *    REAL*8          ArrowheadSize
 * </FortranSyntax>
 *   
 *   Create a line with a ten percent (frame units) arrowhead at the end:
 *   
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeom2DLineSegmentCreate(CoordSys_Frame, 5., 5., 95., 95.);
 *   TecUtilGeomArrowheadSetAttach(g, ArrowheadAttach_AtEnd);
 *   TecUtilGeomArrowheadSetSize(g, 10.);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomArrowheadSetSize (Geom_ID  GID,
                                                            double   ArrowheadSize);
/**
 *   Set the arrowhead angle for a geometry.
 *   
 * @param GID
 *   ID of the geometry. This must be a polyline or a multi-polyline geometry
 *   
 * @param ArrowheadAngle
 *   Angle for the arrowhead measured in degrees
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomArrowheadSetAngle(
 *   &           GID,
 *   &           ArrowheadAngle)
 *    INTEGER*4       GID
 *    REAL*8          ArrowheadAngle
 * </FortranSyntax>
 *   
 *   Create a line with a 15 degree arrowhead at the end:
 *   
 * @code
 *   Geom_ID g;
 *   g = TecUtilGeom2DLineSegmentCreate(CoordSys_Frame, 5., 5., 95., 95.);
 *   TecUtilGeomArrowheadSetAttach(g, ArrowheadAttach_AtEnd);
 *   TecUtilGeomArrowheadSetAngle(g, 15.);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomArrowheadSetAngle (Geom_ID  GID,
                                                             double   ArrowheadAngle);

/**
 *   Sets the draw order of a geometry.
 *   
 * @param GID
 *   ID of the geometry
 *   
 * @param DrawOrder
 *   Must be DrawOrder_BeforeData or DrawOrder_AfterData.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetDrawOrder(
 *   &           GID,
 *   &           DrawOrder)
 *    INTEGER*4       GID
 *    INTEGER*4       DrawOrder
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilGeomSetDrawOrder(Geom_ID GID,
                                                       DrawOrder_e DrawOrder);

/**
 *   Queries the state of the "preserve aspect ratio" toggle for an image geometry.
 *   
 * @param GID
 *   Geometry ID. Must be an image geometry
 *   
 * @return
 *   Returns TRUE if the "preserve aspect ratio" toggle for an image geometry, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomImageGetUseRatio(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 *   
 *   Get the state of the preserve aspect ration toggle in an image geometry:
 *   
 * @code
 *   Boolean_t UsePreserveAspect = TecUtilGeomImageSetUseRatio(GID);
 * @endcode
 */
LINKTOADDON Boolean_t   STDCALL TecUtilGeomImageGetUseRatio(Geom_ID GID);

/**
 *   Gets the draw order of a geometry.
 *   
 * @param GID
 *   ID of a geometry.
 *   
 * @return
 *   The draw order of the geometry. Returns either DrawOrder_BeforeData or DrawOrder_AfterData.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomGetDrawOrder(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 *   
 *   Get the draw order of a geometry:
 *   
 * @code
 *   DrawOrder_e DrawOrder = TecUtilGeomGetDrawOrder(GID);
 * @endcode
 */
LINKTOADDON DrawOrder_e STDCALL TecUtilGeomGetDrawOrder(Geom_ID GID);

/**
 *   Set the scope for a geometry.
 *   
 * @param GID
 *   ID of a geometry
 *   
 * @param Scope
 *   Scope of geometry. The possible values are:Scope_Local (Show in the current frame
 *   only).Scope_Global (Show in all frames with the same data set as the current frame).
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetScope(
 *   &           GID,
 *   &           Scope)
 *    INTEGER*4       GID
 *    INTEGER*4       Scope
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilGeomSetScope (Geom_ID  GID,
                                                    Scope_e  Scope);
/**
 *   Set the position coordinate system for a geometry. This will convert all values in the geometry as
 *   well as the anchor position such that the geometry remains in the same position on the screen.
 *   
 * @param GID
 *   ID of a geometry. This must not be a 3-D polyline.
 *   
 * @param CoordSys
 *   Coordinate system for the position of the geometry. The possible values are: CoordSys_Frame or
 *   CoordSys_Grid
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSetPositionCoordSys(
 *   &           GID,
 *   &           CoordSys)
 *    INTEGER*4       GID
 *    INTEGER*4       CoordSys
 * </FortranSyntax>
 *   
 *   Create a 2-D line segment in frame coordinates and then convert those coordinates to grid
 *   coordinates. The geometry will be in the same location on the screen as its initial frame
 *   coordinates would indicate until the next time the view for that frame is changed.
 *   
 * @code
 *   Geom_ID  g;
 *   g = TecUtilGeom2DLineSegmentCreate(CoordSys_Frame, 5, 5, 95, 95);
 *   TecUtilGeomSetPositionCoordSys(CoordSys_Grid);
 * @endcode
 */
LINKTOADDON void       STDCALL TecUtilGeomSetPositionCoordSys (Geom_ID    GID,
                                                               CoordSys_e CoordSys);
/**
 *   Set the macro function command for a geometry.
 *   
 * @param GID
 *   ID of a geometry
 *   
 * @param Command
 *   Macro function (and parameters) to be executed when the user holds down Ctrl and clicks the right
 *   mouse button on the geometry
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomSetMacroFunctionCmd(
 *   &                   GID,
 *   &                   Command)
 *    INTEGER*4       GID
 *    CHARACTER*(*)   Command
 * </FortranSyntax>
 *   
 *   Set a geometry so that macro function "PlotData" is called whenever the user holds down Ctrl and
 *   clicks the right mouse button on the geometry.
 *   
 * @code
 *   extern Geom_ID g; // created elsewhere 
 *   TecUtilGeomSetMacroFunctionCmd(g, "PlotData");
 * @endcode
 */
LINKTOADDON Boolean_t  STDCALL TecUtilGeomSetMacroFunctionCmd (Geom_ID  GID,
                                                               const char    *Command);
/**
 *   Forces drop of opening banner. If this function is not called, the opening banner will stay up
 *   until all addons are loaded.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilDropOpeningBanner()
 * </FortranSyntax>
 */
LINKTOADDON void       STDCALL TecUtilDropOpeningBanner (void);


/* Geometry/text convenience functions */

/**
 * Creates a text object. Use the handle obtained from this function to set
 * text attributes using the TecUtilTextSetXxx functions. Units are in frame
 * coordinates by default.
 *   
 * @param PositionCoordSys
 *   Coordinate system used to position the text object. The possible values
 *   are: \ref CoordSys_Grid or \ref CoordSys_Frame
 *   
 * @param PosX
 *   X-Coordinate for anchor position of the text in the specified
 *   PositionCoordSys coordinate system.
 *   
 * @param PosY
 *   Y-Coordinate for anchor position of the text in the specified
 *   PositionCoordSys coordinate system.
 *   
 * @param HeightUnits
 *   Units for the character height of the text. If PositionCoordSys is
 *   \ref CoordSys_Frame, units must be \ref Units_Frame or \ref Units_Point.
 *   If PositionCoordSys is \ref CoordSys_Grid, units must be \ref Units_Frame
 *   or \ref Units_Grid.
 *   
 * @param Height
 *   Character height of the text in the specified HeightUnits units
 *   
 * @param Text
 *   String to use to create the text object. Cannot be NULL
 *   
 * @return
 *   If successfully created then the return value is a valid ID that you may
 *   use to further set attributes for this text object. Otherwise, \ref
 *   TECUTILBADID is returned.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilTextCreate(
 *   &           PositionCoordSys,
 *   &           PosX,
 *   &           PosY,
 *   &           HeightUnits,
 *   &           Height,
 *   &           Text,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         PosX
 *    REAL*8         PosY
 *    INTEGER*4      HeightUnits
 *    REAL*8         Height
 *    CHARACTER*(*)  Text
 *    POINTER        (ResultPtr, Result)
 *  
 * </FortranSyntax>
 *   
 *   Create a simple text label:
 *   
 * @code
 *   Text_ID Text;
 *   Text = TecUtilTextCreate(CoordSys_Frame, 50.0, 50.0,
 *                            Units_Point, 30.0, "Hello");
 * @endcode
 */
LINKTOADDON Text_ID STDCALL TecUtilTextCreate(CoordSys_e  PositionCoordSys,
                                              double      PosX, 
                                              double      PosY,
                                              Units_e     HeightUnits,
                                              double      Height,
                                              const char *Text);

/**
 *   Create a 3D text label in Tecplot.
 *   
 * @param PosX
 *   The X-position of the text label.
 *   
 * @param PosY
 *   The Y-position of the text label.
 *   
 * @param PosZ
 *   The Z-position of the text label.
 *   
 * @param HeightUnits
 *   The units to use for the text height.
 *   
 * @param Height
 *   The height of the text.
 *   
 * @param Text
 *   The text character string.
 *   
 * @return
 *   Returns the ID for the created text.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilText3DCreate(
 *   &           PosX,
 *   &           PosY,
 *   &           PosZ,
 *   &           HeightUnits,
 *   &           Height,
 *   &           Text,
 *   &           ResultPtr)
 *    REAL*8         PosX
 *    REAL*8         PosY
 *    REAL*8         PosZ
 *    INTEGER*4      HeightUnits
 *    REAL*8         Height
 *    CHARACTER*(*)  Text
 *    POINTER        (ResultPtr, Result)
 *    
 * </FortranSyntax>
 */
LINKTOADDON Text_ID STDCALL TecUtilText3DCreate(double      PosX, 
                                                double      PosY,
                                                double      PosZ,
                                                Units_e     HeightUnits,
                                                double      Height,
                                                const char *Text);

/**
 * Create a square geometry. Use the ID obtained from this function to set
 * geometry attributes such as line style and color using the TecUtilGeomSetXxx
 * functions.
 *   
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. The possible values are:
 *   \ref CoordSys_Grid or \ref CoordSys_Frame
 *   
 * @param CornerX
 *   X-Coordinate for Lower left corner of the square
 *   
 * @param CornerY
 *   Y-Coordinate for Lower left corner of the square
 *   
 * @param Size
 *   Width/height of the square. Must be non-zero
 *   
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSquareCreate(
 *   &           PositionCoordSys,
 *   &           CornerX,
 *   &           CornerY,
 *   &           Size,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         CornerX
 *    REAL*8         CornerY
 *    REAL*8         Size
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Create a square of width 0.5 and anchored at (0.1, 0.1):
 *   
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeomSquareCreate(CoordSys_Grid,0.1,0.1,0.5);
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomSquareCreate(CoordSys_e PositionCoordSys,
                                                    double     CornerX,
                                                    double     CornerY,
                                                    double     Size);

/**
 * Create a circle geometry. Use the ID obtained from this function to set
 * geometry attributes such as line style and color using the TecUtilGeomSetXxx
 * functions. To set the number of points used to draw the circle use
 * TecUtilGeomEllipseSetNumPoints().
 *   
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. The possible values are:
 *   \ref CoordSys_Grid or \ref CoordSys_Frame
 *   
 * @param CenterX
 *   X-Coordinate for the center of the circle.
 *   
 * @param CenterY
 *   Y-Coordinate for the center of the circle.
 *   
 * @param Radius
 *   Radius of the circle. Must be non-zero
 *   
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomCircleCreate(
 *   &           PositionCoordSys,
 *   &           CenterX,
 *   &           CenterY,
 *   &           Radius,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         CenterX
 *    REAL*8         CenterY
 *    REAL*8         Radius
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Create a circle at 0.5, 0.5, with a radius of 0.2:
 *   
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeomCircleCreate(CoordSys_Grid,0.5,0.5,0.2);
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomCircleCreate(CoordSys_e PositionCoordSys,
                                                    double     CenterX,
                                                    double     CenterY,
                                                    double     Radius);

/**
 * Create a rectangle geometry. Use the ID obtained from this function to set
 * geometry attributes such as line style and color using the TecUtilGeomSetXxx
 * functions.
 *   
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. The possible values are:
 *   \ref CoordSys_Grid or \ref CoordSys_Frame
 *   
 * @param CornerX
 *   X-Coordinate for rectangle anchor position (left side of the rectangle).
 *   
 * @param CornerY
 *   Y-Coordinate for rectangle anchor position (top of the rectangle).
 *   
 * @param Width
 *   Width the rectangle. Must be non-zero.
 *   
 * @param Height
 *   Height of the rectangle. Must be non-zero.
 *   
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomRectangleCreate(
 *   &           PositionCoordSys,
 *   &           CornerX,
 *   &           CornerY,
 *   &           Width,
 *   &           Height,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         CornerX
 *    REAL*8         CornerY
 *    REAL*8         Width
 *    REAL*8         Height
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Create a rectangle anchored at (0.1, 0.1), with a width of 0.2 and a height of 0.3:
 *   
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeomRectangleCreate(.1,.1,0.2,0.3);
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomRectangleCreate(CoordSys_e PositionCoordSys,
                                                       double     CornerX, 
                                                       double     CornerY, 
                                                       double     Width, 
                                                       double     Height);

/**
 * Create an ellipse geometry. Use the ID obtained from this function to set
 * geometry attributes such as line style and color using the TecUtilGeomSetXxx
 * functions.
 *   
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. Valid values are
 *   \ref CoordSys_Grid or \ref CoordSys_Frame
 *   
 * @param CenterX
 *   X-Coordinate for the center of the ellipse
 *   
 * @param CenterY
 *   Y-Coordinate for the center of the ellipse
 *   
 * @param HAxis
 *   Length of the horizontal axis. Must be non-zero
 *   
 * @param VAxis
 *   Length of the vertical Axis. Must be non-zero
 *   
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomEllipseCreate(
 *   &           PositionCoordSys,
 *   &           CenterX,
 *   &           CenterY,
 *   &           HAxis,
 *   &           VAxis,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         CenterX
 *    REAL*8         CenterY
 *    REAL*8         HAxis
 *    REAL*8         VAxis
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Create an ellipse centered at 0.5, 0.5, with a horizontal axis of length 0.2 and a vertical axis
 *   of length 0.3:
 *   
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeomEllipseCreate(CoordSys_Grid, .5,.5,.2,.3);
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomEllipseCreate(CoordSys_e PositionCoordSys,
                                                     double     CenterX, 
                                                     double     CenterY, 
                                                     double     HAxis, 
                                                     double     VAxis);

/**
 * Create a 2-D polyline geometry. Use the ID obtained from this function to
 * set geometry attributes such as line style and color using the
 * TecUtilGeomSetXxx functions. By default, the anchor position is set to
 * (0,0). All points are drawn relative to the anchor position. The anchor
 * position can be changed using TecUtilGeomSetXYZAnchorPos().
 *   
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. The possible values are:
 *   \ref CoordSys_Grid or \ref CoordSys_Frame.
 *   
 * @param PtsX_Array
 *   Array of X-coordinates of the polyline
 *   
 * @param PtsY_Array
 *   Array of Y-coordinates of the polyline
 *   
 * @param NumPts
 *   Number of points in the array (that is, number of points in the polyline).
 *   Must be at least two points.
 *   
 * @return
 *   If successfully created, then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DPolylineCreate(
 *   &           PositionCoordSys,
 *   &           PtsX_Array,
 *   &           PtsY_Array,
 *   &           NumPts,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         PtsX_Array
 *    REAL*8         PtsY_Array
 *    INTEGER*4      NumPts
 *    POINTER        (ResultPtr, Result)
 *    
 * </FortranSyntax>
 *   
 * Create a 2-D polyline with four points:
 * @code
 *   double X[4] = {.2,.5,.6,.4}; //x coords of the polyline
 *   double Y[4] = {.2,.5,.1,.7}; //y coords of the polyline
 *   Geom_ID G;
 *   G = TecUtilGeom2DPolylineCreate(CoordSys_Grid,X,Y,4);
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeom2DPolylineCreate(CoordSys_e PositionCoordSys,
                                                        double    *PtsX_Array,
                                                        double    *PtsY_Array,
                                                        LgIndex_t  NumPts);

/**
 * Create a 3-D polyline geometry. Use the ID obtained from this function to
 * set geometry attributes such as line style and color using the
 * TecUtilGeomSetXxx function. By default, the anchor position is set to (0,0).
 * All points are drawn relative to the anchor position. The anchor position
 * can be changed using TecUtilGeomSetXYZAnchorPos(). All units are in grid
 * coordinates.
 *   
 * @param PtsX_Array
 *   Array of X-coordinates of the polyline.
 *   
 * @param PtsY_Array
 *   Array of Y-coordinates of the polyline.
 *   
 * @param PtsZ_Array
 *   Array of Z-coordinates of the polyline.
 *   
 * @param NumPts
 *   Number of points in the array (that is, number of points in the polyline).
 *   Must be at least two points
 *   
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DPolylineCreate(
 *   &           PtsX_Array,
 *   &           PtsY_Array,
 *   &           PtsZ_Array,
 *   &           NumPts,
 *   &           ResultPtr)
 *    REAL*8         PtsX_Array
 *    REAL*8         PtsY_Array
 *    REAL*8         PtsZ_Array
 *    INTEGER*4      NumPts
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Create a 3-D polyline with four points:
 *   
 * @code
 *   double X[4] = {.2,.5,.6,.4}; //x coords of the polyline
 *   double Y[4] = {.2,.5,.1,.7}; //y coords of the polyline
 *   double Z[4] = {.1,.2,.3,.4}; //z coords of the polyline
 *   Geom_ID G;
 *   G = TecUtilGeom3DPolylineCreate(X,Y,Z,4)
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeom3DPolylineCreate(double   *PtsX_Array,
                                                        double   *PtsY_Array,
                                                        double   *PtsZ_Array,
                                                        LgIndex_t NumPts);
/**
 *   Create a 2-D multi-polyline geometry. After creating the 2-D
 *   multi-polyline geometry, you must assign values to the points in it with
 *   TecUtilGeom2DPolylineSetPoint() or TecUtilGeom2DMPolySetPolyline(). Use
 *   the ID obtained from this function to set geometry attributes such as line
 *   style and color using the TecUtilGeomSetXxx functions.
 *   
 * @param PositionCoordSys
 *   Coordinate System. The possible values are: CoordSys_Grid or
 *   CoordSys_Frame.
 *   
 * @param NumPolys
 *   Number of polylines in the multi-polyline. Must be greater than zero
 *   
 * @param NumPointsInPolylines_Array
 *   Array of points in each polyline. Each polyline must have at least two
 *   points
 *   
 * @return
 *   The geometry ID of the 2-D multi polyline.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeom2DMPolyCreate(
 *   &                   PositionCoordSys,
 *   &                   NumPolys,
 *   &                   NumPointsInPolylines_Array)
 *    INTEGER*4       PositionCoordSys
 *    INTEGER*4       NumPolys
 *    INTEGER*4       NumPointsInPolylines_Array
 * </FortranSyntax>
 *   
 *   Create a 2-D multi-polyline with two polylines. The first polyline has three points, the second
 *   has two:
 *   
 * @code
 *   LgIndex_t pts_per_line[2] = { 3, Z }; // two polylines 
 *   double x_polyline_1 = { 0.0, 1.0, 2.0 }; // three points 
 *   double y_polyline_1 = { 0.0, 1.0, 0.0 };
 *   double x_polyline_2 = { 1.0, 2.0 }; // two points 
 *   double y_polyline_2 = { 1.0, 0.0 };
 *   Geom_ID g;
 *   
 *   g = TecUtilGeom2DMPolyCreate(CoordSys_Grid, Z, pts_per_line);
 *   TecUtilGeom2DMPolySetPolyline(g, 1, x_polyline_1, y_polyline_1);
 *   TecUtilGeom2DMPolySetPolyline(g, 2, x_polyline_2, y_polyline_2);
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeom2DMPolyCreate(CoordSys_e PositionCoordSys,
                                                     LgIndex_t  NumPolys,
                                                     LgIndex_t  *NumPointsInPolylines_Array);

/**
 * Create a 3-D multi-polyline geometry. After creating the 3-D multi-polyline,
 * you must assign values to the points in it with either
 * TecUtilGeom3DMPolySetPoint() or TecUtilGeom3DMPolySetPolyline(). Use the ID
 * obtained from this function to set geometry attributes such as line style
 * and color using the TecUtilGeomSetXxx functions.
 *   
 * @param NumPolys
 *   Number of polylines in the 3-D multi-polyline. Must be greater than zero.
 *   
 * @param NumPointsInPolylines_Array
 *   Array of points in each polyline. Each polyline must have at least two
 *   points.
 *   
 * @return
 *   The geometry ID of the 3-D multi-polyline.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DMPolyCreate(
 *   &           NumPolys,
 *   &           NumPointsInPolylines_Array,
 *   &           ResultPtr)
 *    INTEGER*4      NumPolys
 *    INTEGER*4      NumPointsInPolylines_Array
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 * Create a 3-D multi-polyline with 2 polylines. The first polyline has three
 * points, the second has two:
 *   
 * @code
 *   LgIndex_t pts_per_line[2] = { 3, 2 }; // two polylines 
 *   double x_polyline_1 = { 0.0, 1.0, 2.0 }; // three points 
 *   double y_polyline_1 = { 0.0, 1.0, 0.0 };
 *   double z_polyline_1 = { 0.0, 0.5, 0.0 };
 *   double x_polyline_2 = { 1.0, 2.0 }; // two points 
 *   double y_polyline_2 = { 1.0, 0.0 };
 *   double z_polyline_2 = { 0.5, 0.5 };
 *   Geom_ID g;
 *   
 *   g = TecUtilGeom3DMPolyCreate(CoordSys_Grid, 2, pts_per_line);
 *   TecUtilGeom3DMPolySetPolyline(g, 1, x_polyline_1,
 *                                 y_polyline_1, z_polyline_1);
 *   TecUtilGeom3DMPolySetPolyline(g, 2, x_polyline_2,
 *                                 y_polyline_2, z_polyline_2);
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeom3DMPolyCreate(LgIndex_t NumPolys,
                                                     LgIndex_t *NumPointsInPolylines_Array);
/**
 * Create a 2-D arc. The arc is currently implemented as a 2-D polyline
 * geometry, thus, the type of object returned is a 2-D polyline geometry
 * object. Use the ID obtained from this function to set geometry attributes
 * such as line style and color using the TecUtilGeomSetXxx functions.
 *   
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. Valid values are
 *   \ref CoordSys_Grid or \ref CoordSys_Frame.
 *   
 * @param CenterX
 *   X-Coordinate for the Center of the arc.
 *   
 * @param CenterY
 *   Y-Coordinate for the Center of the arc.
 *   
 * @param Radius
 *   Radius of the arc. Must be greater than zero
 *   
 * @param StartAngle
 *   Starting angle of the arc in degrees. Must be between zero and 360. (The
 *   2-D polyline that is created has on segment per degree of arc.)
 *   
 * @param EndAngle
 *   Ending angle of the arc in degrees.
 *   
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomArcCreate(
 *   &           PositionCoordSys,
 *   &           CenterX,
 *   &           CenterY,
 *   &           Radius,
 *   &           StartAngle,
 *   &           EndAngle,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         CenterX
 *    REAL*8         CenterY
 *    REAL*8         Radius
 *    REAL*8         StartAngle
 *    REAL*8         EndAngle
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Create an arc of a circle of radius 0.5 centered at (0, 0) with an arc angle from 35 to 90 degrees
 *   (a 2-D polyline with 56 points, one point at each degree between 35 and 90):
 *   
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeomArcCreate(CoordSys_Grid, .3,.3,.5,35,90);
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeomArcCreate(CoordSys_e PositionCoordSys,
                                                 double     CenterX, 
                                                 double     CenterY, 
                                                 double     Radius, 
                                                 double     StartAngle, 
                                                 double     EndAngle);

/**
 * Create a 2-D line geometry. Use the ID obtained from this function to set
 * geometry attributes such as line style and color using the TecUtilGeomSetXxx
 * functions.
 *   
 * @param PositionCoordSys
 *   Coordinate system used to position the geometry. The possible values are:
 *   \ref CoordSys_Grid or \ref CoordSys_Frame.
 *   
 * @param X1
 *   X-Coordinate for the starting position of the line.
 *   
 * @param Y1
 *   Y-Coordinate for the starting position of the line.
 *   
 * @param X2
 *   X-Coordinate for the ending position of the line.
 *   
 * @param Y2
 *   Y-Coordinate for the ending position of the line.
 *   
 * @return
 *   If successfully created then the return value is a valid ID that you may
 *   use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DLineSegmentCreate(
 *   &           PositionCoordSys,
 *   &           X1,
 *   &           Y1,
 *   &           X2,
 *   &           Y2,
 *   &           ResultPtr)
 *    INTEGER*4      PositionCoordSys
 *    REAL*8         X1
 *    REAL*8         Y1
 *    REAL*8         X2
 *    REAL*8         Y2
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Create a 2-D line geometry from (0.1, 0.2) to (0.5, 0.6):
 *   
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeom2DLineSegmentCreate(CoordSys_Grid,
 *                                      .1,.2,.5,.6);
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeom2DLineSegmentCreate(CoordSys_e PositionCoordSys,
                                                           double     X1, 
                                                           double     Y1, 
                                                           double     X2, 
                                                           double     Y2);

/**
 * Create a 3-D line. Use the ID obtained from this function to set geometry
 * attributes such as line style and color using the TecUtilGeomSetXxx
 * functions. All coordinates are in grid coordinates.
 *   
 * @param X1
 *   X-Coordinate for Starting position of the line.
 *   
 * @param Y1
 *   Y-Coordinate for Starting position of the line.
 *   
 * @param Z1
 *   Z-Coordinate for Starting position of the line.
 *   
 * @param X2
 *   X-Coordinate for ending position of the line.
 *   
 * @param Y2
 *   Y-Coordinate for ending position of the line.
 *   
 * @param Z2
 *   Z-Coordinate for ending position of the line.
 *   
 * @return
 *   If successfully created then the return value is a valid ID that
 *   you may use to further set attributes for this geometry. Otherwise,
 *   \ref TECUTILBADID is returned.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DLineSegmentCreate(
 *   &           X1,
 *   &           Y1,
 *   &           Z1,
 *   &           X2,
 *   &           Y2,
 *   &           Z2,
 *   &           ResultPtr)
 *    REAL*8         X1
 *    REAL*8         Y1
 *    REAL*8         Z1
 *    REAL*8         X2
 *    REAL*8         Y2
 *    REAL*8         Z2
 *    POINTER        (ResultPtr, Result)
 * </FortranSyntax>
 *   
 *   Create a 3-D line geometry from (0.1, 0.2, 0.2) to (0.5, 0.6, 0.1):
 *   
 * @code
 *   Geom_ID G;
 *   G = TecUtilGeom3DLineSegmentCreate(.1,.2,.2,.5,.6,.1);
 * @endcode
 */
LINKTOADDON Geom_ID STDCALL TecUtilGeom3DLineSegmentCreate(double X1, 
                                                           double Y1, 
                                                           double Z1, 
                                                           double X2, 
                                                           double Y2, 
                                                           double Z2);
/**
 *   Get the number of polylines in a multi-polyline geometry.
 *   
 * @param GID
 *   Geometry ID. Must be a multi-polyline geometry
 *   
 * @return
 *   The number of polylines in a multi-polyline geometry.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomMPolyGetPolylineCnt(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 *   
 *   To determine the number of polylines in a multi-polyline geometry:
 *   
 * @code
 *   extern Geom_ID g; //created elsewhere, must be a multi-polyline geometry
 *   LgIndex_t npolylines = TecUtilGeomMPolyGetPolylineCnt(g, 2);
 * @endcode
 */
LINKTOADDON LgIndex_t STDCALL TecUtilGeomMPolyGetPolylineCnt(Geom_ID GID);
/**
 *   Get the number of points in a polyline geometry.
 *   
 * @param GID
 *   Geometry ID. Must be a multi-polyline geometry
 *   
 * @return
 *   The number of points in a polyline geometry.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomPolyGetPointCount(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON LgIndex_t STDCALL TecUtilGeomPolyGetPointCount(Geom_ID GID);
/**
 *   Get information about the number of points in a polyline of a multi-polyline geometry.
 *   
 * @param GID
 *   Geometry ID. Must be a multi-polyline geometry
 *   
 * @param PolyNum
 *   Polyline of the multi-polyline geometry (starts at one)
 *   
 * @return
 *   The number of points in the polyline.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilGeomMPolyGetPointCount(
 *   &                   GID,
 *   &                   PolyNum)
 *    INTEGER*4       GID
 *    INTEGER*4       PolyNum
 * </FortranSyntax>
 *   
 *   To determine the number of points in the second polyline of a multi-polyline geometry:
 *   
 * @code
 *   extern Geom_ID g; //created elsewhere, must be a multi-polyline geometry
 *   LgIndex_t npts_2nd_polyline = TecUtilGeomMPolyGetPointCount(g, 2);
 * @endcode
 */
LINKTOADDON LgIndex_t STDCALL TecUtilGeomMPolyGetPointCount(Geom_ID   GID,
                                                            LgIndex_t PolyNum);
/**
 *   Gets the 2-D (X,Y) value of point in a 2-D multi-polyline geometry.
 *   
 * @param GID
 *   Geometry ID. This must be a 2-D multi-polyline geometry
 *   
 * @param PolyNum
 *   Polyline number. Must be greater than or equal to one, and less than or equal to the number of
 *   polylines in the geometry
 *   
 * @param PointIndex
 *   Index of the point in the polyline. Must be greater than or equal to one, and less than or equal
 *   to the number of points in the polyline
 *   
 * @param X
 *   Receives the X-value of the point. Must not be NULL
 *   
 * @param Y
 *   Receives the Y-value of the point. Must not be NULL
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DMPolyGetPoint(
 *   &           GID,
 *   &           PolyNum,
 *   &           PointIndex,
 *   &           X,
 *   &           Y)
 *    INTEGER*4       GID
 *    INTEGER*4       PolyNum
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *   
 *   Get the value of the tenth point in the second polyline of a 2-D multi-polyline geometry:
 *   
 * @code
 *   double X,Y;
 *   extern Geom_ID g; // assume this was allocated somewhere 
 *   TecUtilGeom2DMPolyGetPoint(g,2,10,&X,&Y);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeom2DMPolyGetPoint(Geom_ID   GID,
                                                    LgIndex_t PolyNum,
                                                    LgIndex_t PointIndex,
                                                    double    *X,
                                                    double    *Y);
/**
 *   Get a point (X,Y) of a 2-D polyline.
 *   
 * @param GID
 *   Geometry ID. This must be a 2-D multi-polyline geometry
 *   
 * @param PointIndex
 *   Index of the point in the polyline. Must be greater than or equal to one, and less than or equal
 *   to the number of points in the polyline
 *   
 * @param X
 *   Receives the X-value of the point. Must not be NULL
 *   
 * @param Y
 *   Receives the Y-value of the point. Must not be NULL
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DPolylineGetPoint(
 *   &           GID,
 *   &           PointIndex,
 *   &           X,
 *   &           Y)
 *    INTEGER*4       GID
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *   
 *   Get the second point of a 2-D polyline geometry:
 *   
 * @code
 *   double X,Y;
 *   extern Geom_ID g; // allocated somewhere else 
 *   TecUtilGeom2DPolylineGetPoint(g,2,&X,&Y);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeom2DPolylineGetPoint(Geom_ID   GID,
                                                       LgIndex_t PointIndex,
                                                       double    *X,
                                                       double    *Y);
/**
 *   Set the 2-D (X,Y) value of point in a 2-D multi-polyline geometry.
 *   
 * @param GID
 *   Geometry ID. This must be a 2-D multi-polyline geometry
 *   
 * @param PolyNum
 *   Polyline number. Must be greater than or equal to one, and less than or equal to the number of
 *   polylines in the geometry
 *   
 * @param PointIndex
 *   Index of the point in the polyline. Must be greater than or equal to one, and less than or equal
 *   to the number of points in the polyline
 *   
 * @param X
 *   New X-value of the point
 *   
 * @param Y
 *   New Y-value of the point
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DMPolySetPoint(
 *   &           GID,
 *   &           PolyNum,
 *   &           PointIndex,
 *   &           X,
 *   &           Y)
 *    INTEGER*4       GID
 *    INTEGER*4       PolyNum
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *   
 *   Set the value of the tenth point in the second polyline of a 2-D multi-polyline geometry:
 *   
 * @code
 *   extern Geom_ID g; // assume this was allocated somewhere 
 *   TecUtilGeom2DMPolySetPoint(g,2,10,1.5,2.2);//set to (1.5,2.2)
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeom2DMPolySetPoint(Geom_ID   GID,
                                                    LgIndex_t PolyNum,
                                                    LgIndex_t PointIndex,
                                                    double    X,
                                                    double    Y);
/**
 *   Set a point (X,Y) of a 2-D polyline.
 *   
 * @param GID
 *   Geometry ID. This must be a 2-D multi-polyline geometry
 *   
 * @param PointIndex
 *   Index of the point to set. Must be greater than or equal to one, and less than or equal to the
 *   number of points in the polyline
 *   
 * @param X
 *   The new X-value of the point
 *   
 * @param Y
 *   The new Y-value of the point
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DPolylineSetPoint(
 *   &           GID,
 *   &           PointIndex,
 *   &           X,
 *   &           Y)
 *    INTEGER*4       GID
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 * </FortranSyntax>
 *   
 *   Set the second point of a 2-D polyline geometry:
 *   
 * @code
 *   extern Geom_ID g; // allocated somewhere else 
 *   
 *   // set to (1.1,2.5) 
 *   TecUtilGeom2DPolylineSetPoint(g,2,1.1,2.5);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeom2DPolylineSetPoint(Geom_ID   GID,
                                                       LgIndex_t PointIndex,
                                                       double    X,
                                                       double    Y);
/**
 *   Set the points for a polyline in a 2-D multi-polyline geometry.
 *   
 * @param GID
 *   Geometry ID. This must be a 2-D multi-polyline geometry
 *   
 * @param PolyNum
 *   Polyline number. Must be greater than or equal to one, and less than or equal to the number of
 *   polylines in the geometry
 *   
 * @param X_Array
 *   Array of X-values. The number of X-values must be equal to the number of points in the polyline
 *   specified by PolyNum. Must not be NULL
 *   
 * @param Y_Array
 *   Array of Y-values. The number of Y-values must be equal to the number of points in the polyline
 *   specified by PolyNum. Must not be NULL
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom2DMPolySetPolyline(
 *   &           GID,
 *   &           PolyNum,
 *   &           X_Array,
 *   &           Y_Array)
 *    INTEGER*4       GID
 *    INTEGER*4       PolyNum
 *    REAL*8          X_Array
 *    REAL*8          Y_Array
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL TecUtilGeom2DMPolySetPolyline(Geom_ID   GID,
                                                       LgIndex_t PolyNum,
                                                       double    *X_Array,
                                                       double    *Y_Array);
/**
 *   Get the 3-D (X, Y, Z) value of point in a 3-D multi-polyline geometry.
 *   
 * @param GID
 *   Geometry ID. This must be a 3-D multi-polyline geometry
 *   
 * @param PolyNum
 *   Polyline number. Must be greater than or equal to one and less than or equal to the number of
 *   polylines in the geometry
 *   
 * @param PointIndex
 *   Index of the point in the polyline. Must be greater than or equal to one and less than or equal to
 *   the number of points in the polyline
 *   
 * @param X
 *   Receives the X-value of the point. Must not be NULL.
 *   
 * @param Y
 *   Receives the Y-value of the point. Must not be NULL
 *   
 * @param Z
 *   Receives the Z-value of the point. Must not be NULL
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DMPolyGetPoint(
 *   &           GID,
 *   &           PolyNum,
 *   &           PointIndex,
 *   &           X,
 *   &           Y,
 *   &           Z)
 *    INTEGER*4       GID
 *    INTEGER*4       PolyNum
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Z
 * </FortranSyntax>
 *   
 *   Get the value of the tenth point in the second 3-D polyline of a multi-polyline geometry:
 *   
 * @code
 *   double X,Y,Z;
 *   extern Geom_ID g; // assume this was allocated somewhere 
 *   TecUtilGeom3DMPolyGetPoint(g,2,10,&X,&Y,&Z);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeom3DMPolyGetPoint(Geom_ID   GID,
                                                    LgIndex_t PolyNum,
                                                    LgIndex_t PointIndex,
                                                    double    *X,
                                                    double    *Y,
                                                    double    *Z);
/**
 *   Get a point (X, Y, Z) of a 3-D polyline
 *   
 * @param GID
 *   Geometry ID. This must be a 3-D multi-polyline geometry
 *   
 * @param PointIndex
 *   Index of the point to get. Must be greater than or equal to one and less than or equal to the
 *   number of points in the polyline.
 *   
 * @param X
 *   Receives the X-value of the point. Must not be NULL
 *   
 * @param Y
 *   Receives the Y-value of the point. Must not be NULL
 *   
 * @param Z
 *   Receives the Z-value of the point. Must not be NULL
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DPolylineGetPoint(
 *   &           GID,
 *   &           PointIndex,
 *   &           X,
 *   &           Y,
 *   &           Z)
 *    INTEGER*4       GID
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Z
 * </FortranSyntax>
 *   
 *   Get the second point of a 3-D polyline geometry:
 *   
 * @code
 *   double X,Y,Z;
 *   extern Geom_ID g; // allocated somewhere else 
 *   TecUtilGeom3DPolylineGetPoint(g,2,&X,&Y,&Z);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeom3DPolylineGetPoint(Geom_ID   GID,
                                                       LgIndex_t PointIndex,
                                                       double    *X,
                                                       double    *Y,
                                                       double    *Z);
/**
 *   Set the 3-D (X, Y, Z) value of point in a 3-D multi-polyline geometry.
 *   
 * @param GID
 *   Geometry ID. This must be a 3-D multi-polyline geometry
 *   
 * @param PolyNum
 *   Polyline number. Must be greater than or equal to one and less than or equal to the number of
 *   polylines in the geometry
 *   
 * @param PointIndex
 *   Index of the point in the polyline. Must be greater than or equal to one and less than or equal to
 *   the number of points in the polyline
 *   
 * @param X
 *   New X-value of the point
 *   
 * @param Y
 *   New Y-value of the point
 *   
 * @param Z
 *   New Z-value of the point
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DMPolySetPoint(
 *   &           GID,
 *   &           PolyNum,
 *   &           PointIndex,
 *   &           X,
 *   &           Y,
 *   &           Z)
 *    INTEGER*4       GID
 *    INTEGER*4       PolyNum
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Z
 * </FortranSyntax>
 *   
 *   Set the value of the tenth point in the second polyline of a 3-D multi-polyline geometry:
 *   
 * @code
 *   extern Geom_ID g; // assume this was allocated somewhere 
 *   // set to (2.3,5.4,1.1) 
 *   TecUtilGeom3DMPolySetPoint(g,2,10,2.3,5.4,1.1);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeom3DMPolySetPoint(Geom_ID   GID,
                                                    LgIndex_t PolyNum,
                                                    LgIndex_t PointIndex,
                                                    double    X,
                                                    double    Y,
                                                    double    Z);
/**
 *   Set a point (X, Y, Z) of a 3-D polyline.
 *   
 * @param GID
 *   Geometry ID. This must be a 3-D multi-polyline geometry
 *   
 * @param PointIndex
 *   Index of the point to set. Must be greater than or equal to one and less than or equal to the
 *   number of points in the polyline.
 *   
 * @param X
 *   The new X-value of the point
 *   
 * @param Y
 *   The new Y-value of the point
 *   
 * @param Z
 *   The new Z-value of the point
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DPolylineSetPoint(
 *   &           GID,
 *   &           PointIndex,
 *   &           X,
 *   &           Y,
 *   &           Z)
 *    INTEGER*4       GID
 *    INTEGER*4       PointIndex
 *    REAL*8          X
 *    REAL*8          Y
 *    REAL*8          Z
 * </FortranSyntax>
 *   
 *   Set the second point of a 3-D polyline geometry:
 *   
 * @code
 *   extern Geom_ID g; // allocated somewhere else 
 *   TecUtilGeom3DPolylineSetPoint(g,2,1.1,2.5,1.0);
 *   // set to (1.1,2.5,1.0) 
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeom3DPolylineSetPoint(Geom_ID   GID,
                                                       LgIndex_t PointIndex,
                                                       double    X,
                                                       double    Y,
                                                       double    Z);
/**
 *   Set the points for a polyline in a 3-D multi-polyline geometry.
 *   
 * @param GID
 *   Geometry ID. This must be a 3-D multi-polyline geometry
 *   
 * @param PolyNum
 *   Polyline number. Must be greater than or equal to one and less than or equal to the number of
 *   polylines in the geometry
 *   
 * @param X_Array
 *   Array of X-values. The number of X-values must be equal to the number of points in the polyline
 *   specified by PolyNum. Must not be NULL
 *   
 * @param Y_Array
 *   Array of Y-values. The number of Y-values must be equal to the number of points in the polyline
 *   specified by PolyNum. Must not be NULL
 *   
 * @param Z_Array
 *   Array of Z-values. The number of Z-values must be equal to the number of points in the polyline
 *   specified by PolyNum. Must not be NULL
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeom3DMPolySetPolyline(
 *   &           GID,
 *   &           PolyNum,
 *   &           X_Array,
 *   &           Y_Array,
 *   &           Z_Array)
 *    INTEGER*4       GID
 *    INTEGER*4       PolyNum
 *    REAL*8          X_Array
 *    REAL*8          Y_Array
 *    REAL*8          Z_Array
 * </FortranSyntax>
 */
LINKTOADDON void STDCALL TecUtilGeom3DMPolySetPolyline(Geom_ID   GID,
                                                       LgIndex_t PolyNum,
                                                       double    *X_Array,
                                                       double    *Y_Array,
                                                       double    *Z_Array);
/**
 *   Return the radius of a circle geometry.
 *   
 * @param GID
 *   Geometry ID. This must be a circle geometry
 *   
 * @return
 *   The radius of the circle.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGeomCircleGetRadius(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 */
LINKTOADDON double STDCALL TecUtilGeomCircleGetRadius(Geom_ID GID);
/**
 *   Set the radius of a circle geometry.
 *   
 * @param GID
 *   Geometry ID. This must be a circle geometry
 *   
 * @param Radius
 *   New radius of the circle. This must be non-zero
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomCircleSetRadius(
 *   &           GID,
 *   &           Radius)
 *    INTEGER*4       GID
 *    REAL*8          Radius
 * </FortranSyntax>
 *   
 *   Set the radius of a circle to one:
 *   
 * @code
 *   extern Geom_id g; // must be a circle 
 *   TecUtilGeomCircleSetRadius(g,1.0);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeomCircleSetRadius(Geom_ID GID,
                                                    double  Radius);
/**
 *   Get the size of a square geometry.
 *   
 * @param GID
 *   Geometry ID. Must be a square geometry.
 *   
 * @return
 *   The size of the square.
 *   
 * <FortranSyntax>
 *    REAL*8 FUNCTION TecUtilGeomSquareGetSize(GID)
 *    INTEGER*4 GID
 * </FortranSyntax>
 *   
 *   Get the size of a square geometry:
 *   
 * @code
 *   extern Geom_ID g; // must be a square 
 *   double size = TecUtilGeomSquareGetSize(g);
 * @endcode
 */
LINKTOADDON double STDCALL TecUtilGeomSquareGetSize(Geom_ID GID);
/**
 *   Set the size of a square geometry.
 *   
 * @param GID
 *   Geometry ID. Must be a square geometry.
 *   
 * @param Size
 *   New size of the square. Must be non-zero.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomSquareSetSize(
 *   &           GID,
 *   &           Size)
 *    INTEGER*4       GID
 *    REAL*8          Size
 * </FortranSyntax>
 *   
 *   Set the size of a square to one:
 *   
 * @code
 *   extern Geom_id g; // must be a square 
 *   TecUtilGeomSquareSetSize(g,1.0);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeomSquareSetSize(Geom_ID GID,
                                                  double  Size);
/**
 *   Get the width and height of a rectangle geometry.
 *   
 * @param GID
 *   Geometry ID. Must be a rectangle geometry
 *   
 * @param Width
 *   Receives the width of the rectangle. Must not be NULL
 *   
 * @param Height
 *   Receives the height of the rectangle. Must not be NULL
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomRectangleGetSize(
 *   &           GID,
 *   &           Width,
 *   &           Height)
 *    INTEGER*4       GID
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *   
 *   Get the width and height of a rectangle:
 *   
 * @code
 *   double W,H;
 *   extern Geom_ID g; // must be a rectangle 
 *   TecUtilGeomRectangleGetSize(g,&W,&H);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeomRectangleGetSize(Geom_ID GID,
                                                     double  *Width,
                                                     double  *Height);
/**
 *   Set the width and height of a rectangle geometry.
 *   
 * @param GID
 *   Geometry ID. Must be a rectangle geometry
 *   
 * @param Width
 *   New width of the rectangle. Must be non-zero
 *   
 * @param Height
 *   New height of the rectangle. Must be non-zero
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomRectangleSetSize(
 *   &           GID,
 *   &           Width,
 *   &           Height)
 *    INTEGER*4       GID
 *    REAL*8          Width
 *    REAL*8          Height
 * </FortranSyntax>
 *   
 *   Set the width and height of a rectangle:
 *   
 * @code
 *   extern Geom_ID g; // must be a rectangle 
 *   TecUtilGeomRectangleSetSize(g,4,1);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeomRectangleSetSize(Geom_ID GID,
                                                     double  Width,
                                                     double  Height);
/**
 *   Get length of the axes of an ellipse.
 *   
 * @param GID
 *   ID of a geometry. This must be an ellipse geometry
 *   
 * @param HAxis
 *   Receives the length of the horizontal axis. Must not be NULL
 *   
 * @param VAxis
 *   Receives the length of the vertical axis. Must not be NULL
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomEllipseGetSize(
 *   &           GID,
 *   &           HAxis,
 *   &           VAxis)
 *    INTEGER*4       GID
 *    REAL*8          HAxis
 *    REAL*8          VAxis
 * </FortranSyntax>
 *   
 *   Get the length of the axes of an ellipse:
 *   
 * @code
 *   extern Geom_ID g; // must be an ellipse 
 *   double A,B;
 *   TecUtilGeomEllipseGetSize(g,&A,&B);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeomEllipseGetSize(Geom_ID GID,
                                                   double  *HAxis,
                                                   double  *VAxis);
/**
 *   Set the length of the axes of an ellipse.
 *   
 * @param GID
 *   Geometry ID. The must be an ellipse geometry
 *   
 * @param HAxis
 *   The length for the horizontal axis. This must be non-zero
 *   
 * @param VAxis
 *   The length for the vertical axis. This must be non-zero
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGeomEllipseSetSize(
 *   &           GID,
 *   &           HAxis,
 *   &           VAxis)
 *    INTEGER*4       GID
 *    REAL*8          HAxis
 *    REAL*8          VAxis
 * </FortranSyntax>
 *   
 *   Set the major and minor axes of an ellipse:
 *   
 * @code
 *   extern Geom_ID g; // must be an ellipse 
 *   TecUtilGeomEllipseGetSize(g,2.0,1.0);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilGeomEllipseSetSize(Geom_ID GID,
                                                   double  HAxis,
                                                   double  VAxis);


/**
 * Get the current layout file name.
 *   
 * @return
 *   The current layout file name including the path. You must call
 *   TecUtilStringDealloc() on the returned string.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilGetCurLayoutFName(
 *   &           Result,
 *   &           ResultLength)
 *    CHARACTER*(*)   Result
 *    INTEGER*4       ResultLength
 * </FortranSyntax>
 */
LINKTOADDON char * STDCALL TecUtilGetCurLayoutFName(void);

/* CORE SOURCE CODE REMOVED */


/**
 *   Use either WinHelp or a browser to view help information. WinHelp is only
 *   available under Windows unless you have access to the Bristol HyperHelp
 *   compiler for UNIX.
 *   
 * @param HelpFileOrURL 
 *   This represents either a WinHelp file or a valid URL
 *   for a browser. If HelpFileOrURL ends with .hlp then this is assumed to be a
 *   WinHelp file, and in Windows, the Windows help engine will be launched. In
 *   UNIX, HyperHelp will be launched. You can specify an absolute path, or just
 *   use the base name and Tecplot will prepend the path to the help subdirectory
 *   below the Tecplot home directory for you. If this parameter does not end in
 *   .hlp then this is assumed to be a local HTML file or a valid URL. If just a
 *   base file name is supplied, then file:/ and the help directory below the
 *   Tecplot home directory will be prepended for you. If http:/ or file:/ is not
 *   prepended, then file:/ will be prepended for you. Specifying
 *   crunch/index.html will launch the browser with
 *   file:/xxxxx/help/crunch/index.html where xxxxx is the Tecplot home
 *   directory. You must begin HelpFileOrURL with http:/ or www. if you wish to
 *   reference something on the web. In Windows a query is made to determine the
 *   currently registered browser. In UNIX you must specify the command to use to
 *   launch your browser. To specify the browser command, add the following to
 *   your tecplot.cfg file:This requires that Netscape is already up and running
 *   and is iconified. If Netscape is not up then use:$!Interface
 *   UnixBrowserLaunchCmd = "/opt/netscape/netscape 'OpenURL(@)'".
 *   
 * @param GoToID
 *   Flag specifying whether or not to skip into the help file. This is only
 *   used if a WinHelp file is being loaded. Set to TRUE if you want to skip to
 *   the location in the help file identified by HelpID. If set to FALSE, then
 *   the index page appears first.
 *   
 * @param HelpID
 *   Location to go to in the help file. This is only used if a WinHelp file is
 *   being loaded. If GotoID is TRUE, then set this to the marker number to
 *   skip to in the .hlp file.
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilHelp(
 *   &           HelpFileOrURL,
 *   &           GoToID,
 *   &           HelpID)
 *    CHARACTER*(*)   HelpFileOrURL
 *    INTEGER*4       GoToID
 *    INTEGER*4       HelpID
 * </FortranSyntax>
 *   
 *   Use a browser to launch the help file myaddon.html from the help sub-directory below the Tecplot
 *   home directory:
 *   
 * @code
 *   TecUtilHelp("myaddon.html",FALSE,0);
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilHelp(const char *HelpFileOrURL,
                                     Boolean_t   GoToID,
                                     int         HelpID);



/**
 *   Lock the data set attached to the current frame.
 *   
 * @param LockString
 *   Unique string identifier originally used to lock the data set.
 *   
 * @return
 *   Returns TRUE if the data set can be locked, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetLockOn(LockString)
 *    CHARACTER*(*) LockString
 * </FortranSyntax>
 *   
 *   Lock the data set using the identifier "banana."
 *   
 * @code
 *   Boolean_t IsBananaLocked = FALSE;
 *   if (!TecUtilLockIsOn((char **)NULL))
 *     {
 *       IsLocked = TecUtilDataSetLockOn("banana");
 *     }
 *   ...
 *   if (IsBananaLocked)
 *     TecUtilDataSetLockOff("banana");
 * @endcode
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetLockOn(const char *LockString);
/**
 *   Unlock the data set attached to the current frame.
 *   
 * @param LockString
 *   Unique string identifier originally used to lock the data set.
 *   
 * @return
 *   Returns TRUE if the data set can be unlocked, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetLockOff(LockString)
 *    CHARACTER*(*) LockString
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetLockOff(const char *LockString);
/**
 * Query to see of the data set attached to the current frame is locked.
 *   
 * @param LockString
 *   Allocated return string telling you the identifier originally used to lock the data set. You must
 *   deallocate this string when you are through with it. You can pass NULL for this parameter if you
 *   do not need to know who locked the data set
 *   
 * @return
 *   Returns TRUE if the data set is locked, FALSE otherwise.
 *   
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION TecUtilDataSetIsLocked(
 *   &                   LockString,
 *   &                   LockStringLength)
 *    CHARACTER*(*)   LockString
 *    INTEGER*4       LockStringLength
 * </FortranSyntax>
 */
LINKTOADDON Boolean_t STDCALL TecUtilDataSetIsLocked(char **LockString);

/**
 * Shows or hides the wait cursor and optionally displayes a wait message.
 * This function can be used to display an hourglass cursor and optional
 * message during non-cancellable proceedures. (If you need a cancel button
 * and/or progress indicator, then you must use
 * TecUtilDialogLaunchPercentDone() and TecUtilDialogDropPercentDone() rather
 * than this function.)
 *   
 * @param WaitMessage
 *   Status message to be shown. The previous status message will be restored
 *   when the status is popped. If WaitMessage is NULL then "Working..." is
 *   placed on the status line
 *   
 * @param DoWait
 *   Use TRUE at the start of a wait, FALSE at the end (calls must balance).
 *   
 * <FortranSyntax>
 *    SUBROUTINE TecUtilPleaseWait(
 *   &           WaitMessage,
 *   &           DoWait)
 *    CHARACTER*(*)   WaitMessage
 *    INTEGER*4       DoWait
 * </FortranSyntax>
 *   
 *   Show a wait message for a lengthy operation:
 *   
 * @code
 *   {
 *     TecUtilLockStart(AddOnID);
 *     TecUtilPleaseWait("Computing the first 1,000,000 digits of PI",TRUE);
 *     ComputePI(1000000); // This may take awhile on a slow machine 
 *     TecUtilPleaseWait(NULL,FALSE); // Pop the wait cursor and 
 *          // restore it to its previous state 
 *     TecUtilDialogMessageBox("Finished computing PI",
 *                              MessageBox_Information);
 *     TecUtilLockFinish(AddOnID);
 *   }
 * @endcode
 */
LINKTOADDON void STDCALL TecUtilPleaseWait(const char *WaitMessage,
                                           Boolean_t   DoWait);

#if defined COMPILE_BUT_HIDE_FROM_DOC
LINKTOADDON void STDCALL TecUtilUndoStateBegin(UndoStateCategory_e Category);

LINKTOADDON void STDCALL TecUtilUndoStateEnd(Boolean_t DoInvalidate,
                                             Boolean_t DoCommit);
#endif
/**
 * Determine if you can undo the last operation.
 *
 * @return
 *   Returns TRUE if the last operation is undoable otherwise FALSE.
 */
LINKTOADDON Boolean_t STDCALL TecUtilUndoCanUndo(void);
/**
 * Undo the last opeartion.
 *
 * @return
 *   Returns TRUE if the undo is successful otherwise FALSE.
 */
LINKTOADDON Boolean_t STDCALL TecUtilUndoDoUndo(void);
