#ifndef _ADDON_H
#define _ADDON_H

/* CORE SOURCE CODE REMOVED */


/**
 * This callback will be called whenever there is a state change in Tecplot.
 * Use one or more of the TecUtilStateChangeGetXXXX functions to retrieve 
 * supplemental information corresponding the the current state change.  See
 * The ADK users manual for more information.
 *
 * @param StateChange
 *   Identifies the state that changed.
 *
 * <FortranSyntax>
 *   SUBROUTINE MyStateChangeV2Callback(
 *  &            StateChange)
 *   INTEGER*4 StateChange
 * </FortranSyntax>
 */
typedef void (STDCALL * StateChangeAddOnCallbackV2_pf)(StateChange_e StateChange);




/**
 * This is the earlier version (before version 10) of the State Change Callback
 * function. This callback will be called whenever there is a state change in
 * Tecplot.
 *
 * @param StateChange
 *   Identifies the state that changed.
 *
 * @param CallData
 *   Provides further information on the state change. The possible values for 
 *   StateChange and the meaning of CallData for specific StateChange values are 
 *   all described in the ADK Users manual.
 *
 * <FortranSyntax>
 *   SUBROUTINE MyStateChangeCallback(
 *  &            StateChange,
 *  &            CallDataPtr)
 *   INTEGER*4 StateChange
 *   POINTER   (CallDataPtr, DummyCallData)
 * </FortranSyntax>
 */
typedef void (STDCALL * StateChangeAddOnCallback_pf)(StateChange_e StateChange,
                                                     ArbParam_t    CallData);




/**
 * Execute an addon specific macro command.
 * This is the callback that is responsible for performing the macro command action 
 * when the $!ADDONCOMMAND macro command associated with the AddOnIDString is processed.
 *
 * @return 
 *   Return TRUE if the macro command could be processed otherwise FALSE.
 *   if FALSE then you must allocate space for, and populate ErrMsg with
 *   an error message.
 *
 *
 * @param CommandString
 *   The command string.  The syntax for this string is predetermined by the
 *   addon.
 *
 * @param ErrMsg
 *   An error message string which is allocated and set by the callback if the 
 *   callback function's result is FALSE. The error message string states the 
 *   nature of the problem and assigns the value to the parameter *ErrMsg. If 
 *   the callback functionhs result is TRUE then the value of the *ErrMsg
 *   parameter must be NULL. If there is an error then use the TecUtilStringAlloc 
 *   function to allocate the error message so that Tecplot can release it 
 *   after displaying the error.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION MyMacroCommandCallback(
 *   &                   CommandString,
 *   &                   ErrMsgString)
 *    CHARACTER*(*)   CommandString
 *    CHARACTER*(*)   ErrMsgString
 * </FortranSyntax>
 *
 */
typedef Boolean_t (STDCALL * MacroCommandAddOnCallback_pf)(char *CommandString,
                                                           char **ErrMsg);



/**
 * This callback is called when tecplot is in the initial phases of quitting.
 *
 * @return
 *   Return FALSE if you want to stop tecplot from quitting.  Return TRUE if
 *   your addon is ok with allowing tecplot to quit.
 *
 * <FortranSyntax>
 * INTEGER*4 FUNCTION MyMopupQueryAddOnCallback()
 * </FortranSyntax>
 */
typedef Boolean_t (STDCALL * MopupQueryAddOnCallback_pf)(void);




/**
 *  This callback is not available as yet in the current API.
 */
typedef Boolean_t (*ForeignLibLoader_pf)(const char *LibraryName, 
                                         const char *InitFunctionName,
                                         ArbParam_t ClientData); 


/* CORE SOURCE CODE REMOVED */

#endif /* _ADDON_H */
