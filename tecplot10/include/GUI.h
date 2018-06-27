#if !defined(GUI_H_)
#define GUI_H_

/*
*****************************************************************
*****************************************************************
*******                                                  ********
****** (C) Copyright 1989-1999  by AMTEC ENGINEERING INC.********
*******       All Rights Reserved.                       ********
*******                                                  ********
*****************************************************************
*****************************************************************
*/

/* This file must be included *AFTER* "TECADDON.h" */

/* WINDOWS */

#if defined (MSWIN)

#if !defined (_GLOBAL_H)
# error Please include "TECADDON.h" before including this file
#endif

# if !defined (STDCALL)
#  define STDCALL __stdcall
# endif /* ! STDCALL */

# if defined (__cplusplus)
#  define WINGUI_EXTERN_C extern "C"
# else
#  define WINGUI_EXTERN_C
# endif

# if defined (WINGUI_STATIC)
#  define DECL_SPEC
# else
#  if defined (WINGUI_KERNEL)
#   define DECL_SPEC WINGUI_EXTERN_C __declspec ( dllexport )
#  else
#   define DECL_SPEC WINGUI_EXTERN_C __declspec ( dllimport )
#  endif /* ! WINGUI_KERNEL */
# endif /* ! WINGUI_STATIC */

# define LINKTOGUI WINGUI_EXTERN_C DECL_SPEC
#endif /* MSWIN */



/* MOTIF */
#if !defined (MSWIN)
# if !defined (STDCALL)
#   define STDCALL
# endif
# if !defined (LINKTOGUI)
#   if defined __cplusplus
#     define LINKTOGUI extern "C"
#   else
#     define LINKTOGUI
#   endif
# endif
# if !defined (MANAGESTATE)
#   define MANAGESTATE
# endif
#endif

typedef TecGUITextCallback_pf GUITextCallback_pf;
typedef TecGUIIntCallback_pf  GUIIntCallback_pf;
typedef TecGUIVoidCallback_pf GUIVoidCallback_pf;

/*
 Set/Unset the dialog to always be on top of other windows
*/

LINKTOGUI void STDCALL GUI_DialogEnableActionArea(int DialogID,
                                                  int ShowActionArea);

LINKTOGUI void STDCALL GUI_DialogSetTopmost(int Dialog,int MakeTopmost);

#if !defined(WINGUI_STATIC)
LINKTOGUI int STDCALL GUI_GetVersion(void);
#endif


LINKTOGUI void GUI_DialogSetLaunchPosition(int               DialogID,
                                           AnchorAlignment_e Placement,
                                           int               OffsetX,
                                           int               OffsetY);

LINKTOGUI int STDCALL GUI_DialogCreateModeless(int                 DialogParent,
                                               int                 Width,
                                               int                 Height,
                                               const char         *Title,
                                               GUIVoidCallback_pf  HelpButtonCallback,
                                               GUIVoidCallback_pf  CloseButtonCallback,
                                               GUIVoidCallback_pf  InitCallback);

LINKTOGUI int STDCALL GUI_DialogCreateModal(int                 DialogParent,
                                            int                 Width,
                                            int                 Height,
                                            const char         *Title,
                                            GUIVoidCallback_pf  HelpButtonCallback,
                                            GUIVoidCallback_pf  OkButtonCallback,
                                            GUIVoidCallback_pf  CancelButtonCallback,
                                            GUIVoidCallback_pf  InitCallback);
LINKTOGUI int STDCALL GUI_DialogCreateModal2(int                 DialogParent,
                                             int                 Width,
                                             int                 Height,
                                             const char         *Title,
                                             GUIVoidCallback_pf  HelpButtonCallback,
                                             GUIVoidCallback_pf  OkButtonCallback,
                                             GUIVoidCallback_pf  CancelButtonCallback,
                                             GUIVoidCallback_pf  InitCallback,
                                             GUIVoidCallback_pf  ApplyButtonCallback);

LINKTOGUI void STDCALL GUI_BlockForModalDialog(Boolean_t *DoneWithModalDialog);

LINKTOGUI int STDCALL GUI_ButtonAdd(int                ParentDialog,
                                    int                X,
                                    int                Y,
                                    int                Width,
                                    int                Height,
                                    const char        *Label,
                                    GUIVoidCallback_pf ButtonCallback);
LINKTOGUI void STDCALL GUI_ButtonSetDefault(int Dialog,
                                            int Button);
LINKTOGUI void STDCALL GUI_ButtonSetText(int Button,
                                         const char *NewText);




void GUI_Assert(const char *expression,
                const char *explanation,
                const char *utility);



LINKTOGUI void STDCALL GUI_SetSensitivity(int Control,
                                          int IsSensitive);

LINKTOGUI void STDCALL GUI_SetVisibility(int Control,
                                         int IsVisible);

LINKTOGUI int STDCALL GUI_OptionMenuAdd(int               ParentDialog,
                                        int               X,
                                        int               Y,
                                        int               Width,
                                        int               Height,
                                        const char       *OptionList,
                                        GUIIntCallback_pf ValueChangedCallback);

LINKTOGUI void STDCALL GUI_OptionMenuSet(int OptionMenu,
                                         int Selection);

LINKTOGUI int STDCALL GUI_OptionMenuGet(int OptionMenu);


LINKTOGUI int STDCALL GUI_ListAdd(int               ParentDialog,
                                  int               X,
                                  int               Y,
                                  int               Width,
                                  int               Height,
                                  int               IsMultiSelection,
                                  GUIIntCallback_pf ValueChangedCallback);



LINKTOGUI int STDCALL GUI_ListGetItemCount(int List);

LINKTOGUI void STDCALL GUI_ListAppendItem(int  List,
                                          const char *Item);

LINKTOGUI char * STDCALL GUI_ListGetString(int List,
                                           int Position);

LINKTOGUI void STDCALL GUI_ListReplaceItem(int  List,
                                           const char *Item,
                                           int  Position);

LINKTOGUI void STDCALL GUI_ListDeleteAllItems(int List);

LINKTOGUI void STDCALL GUI_ListDeleteItemAtPos(int List,
                                               int Position);

LINKTOGUI void STDCALL GUI_ListDeselectAllItems(int List);

LINKTOGUI void STDCALL GUI_ListSetSelectedItem(int List,
                                               int Position);

LINKTOGUI void STDCALL GUI_ListGetSelectedItems(int List, /* {{-auto (exclude from auto fortran generator) */
                                                int **SelectedItemList,
                                                int *SelectedItemCount);

LINKTOGUI void STDCALL GUI_ListSetSelectedItems(int List,
                                                int *SelectedItemList,
                                                int SelectedItemCount);
LINKTOGUI void STDCALL GUI_ListDeallocItemList(int **ItemList);

LINKTOGUI int STDCALL GUI_ToggleAdd(int                ParentDialog,
                                    int                X,
                                    int                Y,
                                    int                Width,
                                    int                Height,
                                    const char        *Label,
                                    GUIIntCallback_pf  ValueChangedCallback);

LINKTOGUI void STDCALL GUI_ToggleSet(int ToggleID,
                                     int SetOn);



LINKTOGUI int STDCALL GUI_ToggleGet(int ToggleID);

LINKTOGUI int STDCALL GUI_RadioBoxAdd(int               ParentDialog,
                                      int               X,
                                      int               Y,
                                      int               Width,
                                      int               Height,
                                      const char       *Label1,
                                      const char       *Label2,
                                      const char       *Label3,
                                      const char       *Label4,
                                      const char       *Label5,
                                      GUIIntCallback_pf ValueChangedCallback);

LINKTOGUI void STDCALL GUI_RadioBoxSetToggle(int RadioBox,
                                             int ToggleNumber);


LINKTOGUI int STDCALL GUI_RadioBoxGetToggle(int RadioBox);

LINKTOGUI int STDCALL GUI_LabelAdd(int      ParentDialog,
                                   int      X,
                                   int      Y,
                                   const char *Label);

LINKTOGUI void STDCALL GUI_LabelSetText(int  Label,
                                        const char *LabelString);

LINKTOGUI int STDCALL GUI_TextFieldAdd(int                ParentDialog,
                                       int                X,
                                       int                Y,
                                       int                Width,
                                       int                Height,
                                       GUITextCallback_pf ValueChangedCallback);

LINKTOGUI int STDCALL GUI_TextAdd(int                ParentDialog,
                                  int                X,
                                  int                Y,
                                  int                Width,
                                  int                Height,
                                  int          IsReadOnly,
                                  GUITextCallback_pf ValueChangedCallback);
LINKTOGUI void STDCALL GUI_TextSetInsertPos(int Text,
                                            int Position);
LINKTOGUI void STDCALL GUI_TextSetMinInsertPos(int Text);
LINKTOGUI void STDCALL GUI_TextSetMaxInsertPos(int Text);
LINKTOGUI void STDCALL GUI_TextSetString(int          Text,
                                         const char  *TextString);
LINKTOGUI char * STDCALL GUI_TextGetString(int Text);
LINKTOGUI void STDCALL GUI_TextInsertString(int          Text,
                                            const char  *TextString);
LINKTOGUI void STDCALL GUI_TextAppendString(int          Text,
                                            const char  *TextString);

LINKTOGUI int STDCALL GUI_ScaleAdd(int               ParentDialog,
                                   int               X,
                                   int               Y,
                                   int               Width,
                                   int               Height,
                                   int               ScaleMin,
                                   int               ScaleMax,
                                   int               DecimalPrecision,
                                   GUIIntCallback_pf ValueChangedCallback,
                                   GUIIntCallback_pf DragValueChangedCallback);

LINKTOGUI void STDCALL GUI_ScaleSetValue(int Scale,
                                         int NewValue);

LINKTOGUI void STDCALL GUI_ScaleSetLimits(int Scale,
                                          int ScaleMin,
                                          int ScaleMax,
                                          int DecimalPrecision);

LINKTOGUI int STDCALL GUI_ScaleGetValue(int Scale);

LINKTOGUI int STDCALL GUI_VertSeparatorAdd(int    ParentDialog,
                                           int    X,
                                           int    Y,
                                           int    Height);

LINKTOGUI int STDCALL GUI_HorzSeparatorAdd(int    ParentDialog,
                                           int    X,
                                           int    Y,
                                           int    Width);

LINKTOGUI int STDCALL GUI_FrameAdd(int         ParentDialog,
                                   int         X,
                                   int         Y,
                                   int         Width,
                                   int         Height,
                                   const char *Label);

LINKTOGUI void STDCALL GUI_TextFieldSetString(int   TextField,
                                              const char *TextString);
LINKTOGUI char * STDCALL GUI_TextFieldGetString(int TextField);

LINKTOGUI void STDCALL GUI_DialogLaunch(int DialogID);

LINKTOGUI void STDCALL GUI_DialogDismiss(int DialogID);

LINKTOGUI int STDCALL GUI_DialogIsUp(int DialogID);

LINKTOGUI void STDCALL GUI_DialogSetTitle(int DialogID,
                                          const char *NewTitle);

LINKTOGUI void STDCALL GUI_TextAppendString(int          Text,
                                            const char  *TextString);
                                            
LINKTOGUI int STDCALL GUI_MenuBarAdd(int ParentDialog);

LINKTOGUI int STDCALL GUI_MenuAdd(int         ParentMenu,
                                  const char *Label);

LINKTOGUI int STDCALL GUI_MenuAddItem(int                 ParentMenu,
                                      const char         *Label,
                                      GUIVoidCallback_pf  Callback);

LINKTOGUI int STDCALL GUI_MenuAddToggle(int                ParentMenu,
                                        const char        *Label,
                                        GUIIntCallback_pf  Callback);

LINKTOGUI void STDCALL GUI_MenuAddSeparator(int ParentMenu);

LINKTOGUI void STDCALL GUI_MenuItemSetText(int         MenuItem,
                                           const char *NewText);

LINKTOGUI void STDCALL GUI_MenuSetToggle(int MenuItem,
                                         int SetOn);

LINKTOGUI void STDCALL GUI_MenuDeleteItem(int MenuItem);

/*** Tabbed dialogs ***/
LINKTOGUI int GUI_TabAdd(int ParentDialogID,
                         int X,
                         int Y,
                         int Width,
                         int Height,
                         GUIIntCallback_pf ActivateCallback,
                         GUIIntCallback_pf DeactivateCallback);

LINKTOGUI int GUI_TabAddPage(int TabID,
                             const char *Caption);

LINKTOGUI void GUI_TabSetCurrentPage(int TabID,
                                     int PageID);




/*** Forms ***/
LINKTOGUI int GUI_FormAdd(int ParentDialogID,
                          int X,
                          int Y,
                          int Width,
                          int Height);

LINKTOGUI int GUI_FormAddPage(int ParentFormID);

LINKTOGUI void GUI_FormSetCurrentPage(int FormID);



/* Spin controls */
LINKTOGUI int GUI_SpinTextFieldAdd(int ParentDialogID,
                                   int X,
                                   int Y,
                                   int Width,
                                   int Height,
                                   GUITextCallback_pf ValueChangedCallback,
                                   GUIVoidCallback_pf ButtonUpCallback,
                                   GUIVoidCallback_pf ButtonDownCallback);

/* Dynamic option menus */
LINKTOGUI void GUI_OptionMenuDeleteItemAtPos(int OptionMenu,
                                             int Position);

LINKTOGUI void GUI_OptionMenuAppendItem(int OptionMenu,
                                        const char *Item);

LINKTOGUI int GUI_OptionMenuGetItemCount(int OptionMenu);

LINKTOGUI void GUI_OptionMenuDeleteAllItems(int OptionMenu);

LINKTOGUI char *GUI_OptionMenuGetString(int OptionMenu,
                                        int Position);

LINKTOGUI void GUI_OptionMenuReplaceItem(int OptionMenu,
                                         const char *NewText,
                                         int Position);


/* Scale */
LINKTOGUI void GUI_ScaleShowNumericDisplay(int ScaleID,
                                           int ShowDisplay);

/* List */
LINKTOGUI void GUI_ListSelectAllItems(int ListID);

LINKTOGUI Boolean_t GUI_TextFieldGetDouble(int         TextFieldID,
                                           double     *DValue,
                                           double      Min,
                                           double      Max,
                                           const char *ParamName);
LINKTOGUI Boolean_t GUI_TextFieldGetLgIndex(int         TextFieldID,
                                            LgIndex_t  *IValue,
                                            LgIndex_t   Min,
                                            LgIndex_t   Max,
                                            const char *ParamName,
                                            Boolean_t   AllowMx);
LINKTOGUI Boolean_t GUI_TextFieldGetSet(int         TextFieldID,
                                        Set_pa     *Set,
                                        const char *ParamName);
LINKTOGUI void      GUI_TextFieldSetLgIndex(int       TextFieldID,
                                            LgIndex_t Value,
                                            Boolean_t UseMx);
LINKTOGUI void      GUI_TextFieldSetDouble(int         TextFieldID,
                                           double      Value,
                                           const char *Format);
LINKTOGUI void      GUI_TextFieldSetSet(int       TextFieldID,
                                        Set_pa    Set,
                                        Boolean_t IncludeSquareBraces);

LINKTOGUI void GUI_LabelSetLgIndex(int       LabelID,
                                   LgIndex_t Value);
LINKTOGUI void GUI_LabelSetDouble(int         LabelID,
                                  double      Value,
                                  const char *Format);
LINKTOGUI void GUI_LabelSetSet(int       LabelID,
                               Set_pa    Set,
                               Boolean_t IncludeSquareBraces);

LINKTOGUI int GUI_ListGetSelectedItem(int ListID);

LINKTOGUI int GUI_OptionMenuSetByString(int ID, 
                                        const char *str);

/* Help with button up/down callbacks */
LINKTOGUI int GUI_ProcessLgIndexSpinCallback(int TextID,
                                             const char *ParamName,
                                             Boolean_t IsButtonUp, /* FALSE --> ButtonDown */
                                             int Min,
                                             int Max,
                                             int Inc);

LINKTOGUI double GUI_ProcessDoubleSpinCallback(int TextID,
                                               const char *Fmt,
                                               const char *ParamName,
                                               Boolean_t IsButtonUp, /* FALSE --> ButtonDown */
                                               double Min,
                                               double Max,
                                               double Inc);

/* this line must be last */
#endif /* GUI_H_ */
