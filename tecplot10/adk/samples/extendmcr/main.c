#include "TECADDON.h"
#include "ADDGLBL.h"
#include <string.h>
#include <sys/stat.h>
#include "ADKUTIL.h"

#define MAXSTRLEN             5012
#define MAXINTRINSICVARSTRLEN 50

/* command strings */
#define QUERY_ZONENAMEBYNUM_CMD      "QUERY.ZONENAMEBYNUM"
#define QUERY_MAPNAMEBYNUM_CMD       "QUERY.MAPNAMEBYNUM"
#define QUERY_VARNAMEBYNUM_CMD       "QUERY.VARNAMEBYNUM"
#define QUERY_ZONENUMBYNAME_CMD      "QUERY.ZONENUMBYNAME"
#define QUERY_VARNUMBYASSIGNMENT_CMD "QUERY.VARNUMBYASSIGNMENT"
#define QUERY_DATASETTITLE_CMD       "QUERY.DATASETTITLE"
#define STRING_FINDPATTERN_CMD       "STRING.FINDPATTERN"
#define STRING_LENGTH_CMD            "STRING.LENGTH"
#define STRING_SUBSTRING_CMD         "STRING.SUBSTRING"
#define QUERY_ACTIVEZONES_CMD        "QUERY.ACTIVEZONES"
#define QUERY_ISZONEACTIVE_CMD       "QUERY.ISZONEACTIVE"
#define QUERY_ISADDONLOADED_CMD      "QUERY.ISADDONLOADED"
#define QUERY_FILEEXISTS_CMD         "QUERY.FILEEXISTS"



/* This is for QUERY_FILEEXISTS_CMD */
/* make the UNIXX and MSWIN platforms have the same syntax for stat() usage */
#if defined UNIXX
#   define _stat    stat
#   define _S_ISDIR S_ISDIR
#elif defined MSWIN
#   define _S_ISDIR(mode) ((mode & _S_IFMT) == _S_IFDIR)
#endif
/**/


AddOn_pa AddOnID;

static Boolean_t IsCommand(char *MacroCommandString,
                           char *Command)
{
  Boolean_t IsMatch = FALSE;
  if (!Str_ustrncmp(MacroCommandString,Command,strlen(Command)))
    IsMatch = TRUE;
  return (IsMatch);
}

static void StuffErrMsg(char *ErrMsgString,
                        char **ErrMsg)
{
  *ErrMsg = TecUtilStringAlloc(strlen(ErrMsgString)+1,"String for Error Message");
   strcpy(*ErrMsg,ErrMsgString);
}


Boolean_t SetMacroVarValue(char *IntrinsicVarName,
                           char *ValueString,
                           char **ErrMsg)
{
  Boolean_t IsOk;

  IsOk = TecUtilMacroSetMacroVar(IntrinsicVarName,ValueString);
  if (!IsOk)
    StuffErrMsg("Invalid macro variable name",ErrMsg);
  return (IsOk);
}

/*
 * This function is called when
 * the $!ADDONCOMMAND macro command is
 * processed.
 */
static Boolean_t STDCALL MacroCommandCallback(char *MacroCommandString,  /* IN */
                                              char **ErrMsg)             /* OUT (only if returning FALSE) */
{

  Boolean_t IsOk = TRUE;

  /* 
   * MacroCommandString is the add-on macro command string needing processing.
   *
   * *ErrMsg is an error message string which must be allocated and set by this
   * function if and only if the return value is FALSE.
   */
  
  TecUtilLockStart(AddOnID);
  
  if (IsCommand(MacroCommandString,QUERY_ZONENAMEBYNUM_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ZONENAMEBYNUM nnn VVV'
       *
       * Get the string for zone nnn and stuff into variable VVV
       */

      char       IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];
      LgIndex_t  IZoneNum;
      char      *ZoneName = NULL;
      char      *CPtr;

      CPtr = &MacroCommandString[strlen(QUERY_ZONENAMEBYNUM_CMD)+1];

      IZoneNum = 0;
      Str_RemoveWhiteSpace(&CPtr);

      if (Str_ScanForLgIndex(&CPtr,&IZoneNum) &&
          Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName))
        {
          if (TecUtilDataSetIsAvailable() &&
              TecUtilZoneIsEnabled((EntIndex_t)IZoneNum))
            {
              TecUtilZoneGetName((EntIndex_t)IZoneNum,&ZoneName);
            }
        }
      if (ZoneName)
        {
          IsOk = SetMacroVarValue(IntrinsicVarName,
                                  ZoneName,
                                  ErrMsg);
          TecUtilStringDealloc(&ZoneName);
        }
      else
        {
          StuffErrMsg("No Dataset or bad parameters in call to QUERY.ZONENAMEBYNUM",ErrMsg);
          IsOk = FALSE;
        }
    }
  else if (IsCommand(MacroCommandString,QUERY_MAPNAMEBYNUM_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.MAPNAMEBYNUM nnn VVV'
       *
       * Get the string for map nnn and stuff into variable VVV
       */

      char       IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];
      LgIndex_t  IMapNum;
      char      *MapName = NULL;
      char      *CPtr;

      CPtr = &MacroCommandString[strlen(QUERY_MAPNAMEBYNUM_CMD)+1];

      IMapNum = 0;
      Str_RemoveWhiteSpace(&CPtr);

      if (Str_ScanForLgIndex(&CPtr,&IMapNum) &&
          Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName))
        {
          if (TecUtilDataSetIsAvailable() &&
              ((TecUtilFrameGetPlotType() == PlotType_XYLine) ||
               (TecUtilFrameGetPlotType() == PlotType_PolarLine)) &&
              (TecUtilLineMapGetCount() >= IMapNum))
            {
              TecUtilLineMapGetName((EntIndex_t)IMapNum,&MapName);
            }
        }
      if (MapName)
        {
          IsOk = SetMacroVarValue(IntrinsicVarName,
                                  MapName,
                                  ErrMsg);
          TecUtilStringDealloc(&MapName);
        }
      else
        {
          StuffErrMsg("PlotType is not XY-Line or Polar-Line or bad parameters in call to QUERY.MAPNAMEBYNUM",ErrMsg);
          IsOk = FALSE;
        }
    }
  else if (IsCommand(MacroCommandString,QUERY_VARNAMEBYNUM_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.VARNAMEBYNUM nnn VVV'
       *
       * Get the string for var nnn and stuff into variable VVV
       */

      char       IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];
      LgIndex_t  IVarNum;
      char      *VarName = NULL;
      char      *CPtr;

      CPtr = &MacroCommandString[strlen(QUERY_VARNAMEBYNUM_CMD)+1];

      IVarNum = 0;
      Str_RemoveWhiteSpace(&CPtr);

      if (Str_ScanForLgIndex(&CPtr,&IVarNum) &&
          Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName))
        {
          if (TecUtilDataSetIsAvailable() &&
              TecUtilVarIsEnabled((EntIndex_t)IVarNum))
            {
              TecUtilVarGetName((EntIndex_t)IVarNum,&VarName);
            }
        }
      if (VarName)
        {
          IsOk = SetMacroVarValue(IntrinsicVarName,VarName,ErrMsg);
          TecUtilStringDealloc(&VarName);
        }
      else
        {
          StuffErrMsg("No Dataset or bad parameters in call to QUERY.VARNAMEBYNUM",ErrMsg);
          IsOk = FALSE;
        }
    }
  else if (IsCommand(MacroCommandString,QUERY_ZONENUMBYNAME_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ZONENUMBYNAME "zonename" VVV'
       *
       * Get the number of zone named zonename and stuff into variable VVV
       */

      EntIndex_t IZoneNum;
      char      *CPtr;
      Boolean_t  ZoneNumFound = FALSE;
      char       IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];

      CPtr = &MacroCommandString[strlen(QUERY_ZONENUMBYNAME_CMD)+1];

      IZoneNum = 0;
      Str_RemoveWhiteSpace(&CPtr);
      if (*CPtr == '"')
        {
          char  ZoneName[MAXSTRLEN+1];
          /*
           * Note: str_gettoken can handle quoted strings
           */
          if (Str_GetToken(&CPtr,MAXSTRLEN,ZoneName)                     &&
              Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName) &&
              TecUtilDataSetIsAvailable())
            {
              EntIndex_t NumZones;
              if (TecUtilDataSetGetInfo((char **)NULL,
                                        &NumZones,
                                        (EntIndex_t  *)NULL))
                {
                  IZoneNum = 1;
                  while ((IZoneNum <= NumZones) &&
                         !ZoneNumFound)
                    {
                      char *CurZoneName = NULL;
                      TecUtilZoneGetName(IZoneNum,&CurZoneName);
                      if (CurZoneName)
                        {
                          if (Str_ustrcmp(CurZoneName,ZoneName) == 0)
                            ZoneNumFound = TRUE;
                          TecUtilStringDealloc(&CurZoneName);
                          if (ZoneNumFound)
                            break;
                        }
                      IZoneNum++;
                    }
                }
            }
        }
      if (ZoneNumFound)
        {
          char ZoneNumString[20];
          sprintf(ZoneNumString,"%d",IZoneNum);
          IsOk = SetMacroVarValue(IntrinsicVarName,ZoneNumString,ErrMsg);
        }
      else
        {
          StuffErrMsg("No Dataset or bad parameters in call to QUERY.ZONENUMBYNAME",ErrMsg);
          IsOk = FALSE;
        }
    }
  else if (IsCommand(MacroCommandString,QUERY_VARNUMBYASSIGNMENT_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.VARNUMBYASSIGNMENT assignment VVV'
       *
       * Get the number of var by assignment and stuff into variable VVV
       *  
       *  Possible assignment values are x,y,z,u,v,w,c,s,b.
       */

      char      *CPtr;
      Boolean_t  VarNumFound = FALSE;
      char       IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];
      char       VarAssignment[MAXSTRLEN+1];
      EntIndex_t VarNum = 0;

      CPtr = &MacroCommandString[strlen(QUERY_VARNUMBYASSIGNMENT_CMD)+1];

      Str_RemoveWhiteSpace(&CPtr);
      /*
       * Note: str_gettoken can handle quoted strings or not.
       */
      if (Str_GetToken(&CPtr,MAXSTRLEN,VarAssignment)                &&
         (strlen(VarAssignment) == 1)                                &&
          Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName) &&
          TecUtilDataSetIsAvailable())
        {
          VarNum = TecUtilVarGetNumByAssignment(VarAssignment[0]);
          if (VarNum != TECUTILSETNOTMEMBER)
            VarNumFound = TRUE;
        }

      if (VarNumFound)
        {
          char VarNumString[20];
          sprintf(VarNumString,"%d",VarNum);
          IsOk = SetMacroVarValue(IntrinsicVarName,VarNumString,ErrMsg);
        }
      else
        {
          StuffErrMsg("No Dataset or bad parameters in call to QUERY.VARNUMBYASSIGNMENT",ErrMsg);
          IsOk = FALSE;
        }
    }
  else if (IsCommand(MacroCommandString,QUERY_DATASETTITLE_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.DATASETTITLE VVV'
       */

      char       IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];
      char      *CPtr;

      CPtr = &MacroCommandString[strlen(QUERY_DATASETTITLE_CMD)+1];

      Str_RemoveWhiteSpace(&CPtr);

      if (Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName))
        {
          if (TecUtilDataSetIsAvailable())
            {
              char *DataSetTitle = NULL;
              if (TecUtilDataSetGetInfo(&DataSetTitle,
                                        (EntIndex_t *)NULL,
                                        (EntIndex_t *)NULL) &&
                  (DataSetTitle != NULL))
                {
                  IsOk = SetMacroVarValue(IntrinsicVarName,DataSetTitle,ErrMsg);
                }
              else
                IsOk = SetMacroVarValue(IntrinsicVarName,"Unknown",ErrMsg);
              if (DataSetTitle)
                TecUtilStringDealloc(&DataSetTitle);
            }
          else
            IsOk = SetMacroVarValue(IntrinsicVarName,"No DataSet",ErrMsg);
        }
    }
  else if (IsCommand(MacroCommandString,QUERY_FILEEXISTS_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.FILEEXISTS "filename" VVV'
       *
       * If the file exists VVV gets "YES" otherwise VVV gets "NO"
       */
      char      *CPtr;
      Boolean_t  FileFound = FALSE;
      char       IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];      

      CPtr = &MacroCommandString[strlen(QUERY_FILEEXISTS_CMD)+1];

      Str_RemoveWhiteSpace(&CPtr);
      if (*CPtr == '"')
        {
          char  FileName[MAXSTRLEN+1];
          /*
           * Note: str_gettoken can handle quoted strings
           */
          if (Str_GetToken(&CPtr,MAXSTRLEN,FileName)   &&
              Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName))
            {
              int          ErrorCode = 0;  /* error code returned from stat() */
              struct _stat FileStatus;     /* file status/information structure */

              ErrorCode = _stat(FileName, &FileStatus);
              FileFound = (ErrorCode == 0 && !_S_ISDIR(FileStatus.st_mode));
            }
        }
      if (FileFound)
        IsOk = SetMacroVarValue(IntrinsicVarName,"YES",ErrMsg);
      else
        IsOk = SetMacroVarValue(IntrinsicVarName,"NO",ErrMsg);
    }
  else if (IsCommand(MacroCommandString,STRING_FINDPATTERN_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.FINDPATTERN StrSource StrPattern VVV'
       */
      char      *CPtr;
      char String1[MAXSTRLEN+1];
      char String2[MAXSTRLEN+1];
      char IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];

      CPtr = &MacroCommandString[strlen(STRING_FINDPATTERN_CMD)+1];
      if (Str_GetToken(&CPtr,MAXSTRLEN,String1) &&
          Str_GetToken(&CPtr,MAXSTRLEN,String2) &&
          Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName))
        {
          char *SPtr = strstr(String1,String2);

          if (SPtr == NULL)
            IsOk = SetMacroVarValue(IntrinsicVarName,"NOTFOUND",ErrMsg);
          else
            IsOk = SetMacroVarValue(IntrinsicVarName,SPtr,ErrMsg);
        }
    }
  else if (IsCommand(MacroCommandString,STRING_LENGTH_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.LENGTH StrSource VVV'
       *
       * Get the length of StrSource.  Put result in VVV
       */
      char      *CPtr;
      char String1[MAXSTRLEN+1];
      char IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];

      CPtr = &MacroCommandString[strlen(STRING_LENGTH_CMD)+1];
      if (Str_GetToken(&CPtr,MAXSTRLEN,String1) &&
          Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName))
        {
          char StrLenString[20];
          sprintf(StrLenString,"%ld",(long)strlen(String1));
          IsOk = SetMacroVarValue(IntrinsicVarName,StrLenString,ErrMsg);
        }
    }
  else if (IsCommand(MacroCommandString,STRING_SUBSTRING_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.SUBSTRING StrSource start end VVV'
       *
       * Get the substring from StrSource that starts at position start and ends
       * at position end.  Put the result in VVV.
       */
      char      *CPtr;
      char      String1[MAXSTRLEN+1];
      char      IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];
      LgIndex_t StrStart;
      LgIndex_t StrEnd;

      CPtr = &MacroCommandString[strlen(STRING_SUBSTRING_CMD)+1];
      if (Str_GetToken(&CPtr,MAXSTRLEN,String1) &&
          Str_ScanForLgIndex(&CPtr,&StrStart)   &&
          Str_ScanForLgIndex(&CPtr,&StrEnd)     &&
          Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName))
        {
          if ((StrStart > 0)              &&
              (StrEnd >= StrStart)        &&
              (StrEnd <= (LgIndex_t)strlen(String1)))
            {
              char *SubStr;
              SubStr = (char *)malloc(StrEnd-StrStart+2);

              if (SubStr)
                {
                  strncpy(SubStr,&String1[StrStart-1],StrEnd-StrStart+1);
                  SubStr[StrEnd-StrStart+1] = '\0';
                  IsOk = SetMacroVarValue(IntrinsicVarName,SubStr,ErrMsg);
                  free(SubStr);
                  SubStr = NULL;
                }
              else
                {
                  StuffErrMsg("Internal memory alloc error",ErrMsg);
                  IsOk = FALSE;
                }
            }
          else
            {
              StuffErrMsg("Invalid start and end values",ErrMsg);
              IsOk = FALSE;
            }
        }
    }
  else if (IsCommand(MacroCommandString,QUERY_ACTIVEZONES_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ACTIVEZONES VVV'
       *
       * Get the string representing the current set of active zones.
       */

      char       IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];
      char      *CPtr;
      Set_pa     ActiveZones = NULL;
      char      *ActiveZoneString = NULL;

      CPtr = &MacroCommandString[strlen(QUERY_ACTIVEZONES_CMD)+1];
      Str_RemoveWhiteSpace(&CPtr);

      if (Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName))
        {
          FrameMode_e CurFrameMode = TecUtilFrameGetMode();
          if (TecUtilDataSetIsAvailable() &&
              ((CurFrameMode == Frame_TwoD) ||
               (CurFrameMode == Frame_ThreeD)))
            {
              if (TecUtilZoneGetActive(&ActiveZones))
                {
                  Str_BuildSetString(ActiveZones,
                                     FALSE,
                                     &ActiveZoneString);
                  TecUtilSetDealloc(&ActiveZones);
                }
            }
        }
      if (ActiveZoneString)
        {
          IsOk = SetMacroVarValue(IntrinsicVarName,ActiveZoneString,ErrMsg);
          TecUtilStringDealloc(&ActiveZoneString);
        }
      else
        {
          StuffErrMsg("Bad parameters in call to GETACTIVEZONES",ErrMsg);
          IsOk = FALSE;
        }
    }
  else if (IsCommand(MacroCommandString,QUERY_ISZONEACTIVE_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ISZONEACTIVE ZZZ VVV'
       *
       * Return "YES" if zone ZZZ is active, otherwise return "NO"
       */

      char        IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];
      LgIndex_t   IZoneNum;
      char       *CPtr;
      Boolean_t   ZoneIsActive;

      CPtr = &MacroCommandString[strlen(QUERY_ISZONEACTIVE_CMD)+1];


      IZoneNum = 0;
      Str_RemoveWhiteSpace(&CPtr);

      if (Str_ScanForLgIndex(&CPtr,&IZoneNum) &&
          Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName))
        {
          FrameMode_e CurFrameMode = TecUtilFrameGetMode();
          if (TecUtilDataSetIsAvailable() &&
              ((CurFrameMode == Frame_TwoD) ||
               (CurFrameMode == Frame_ThreeD)))
            {
              ZoneIsActive = TecUtilZoneIsActive((EntIndex_t)IZoneNum);
            }
          else
            {
              IsOk = FALSE;
              StuffErrMsg("Framemode must be 2D or 3D to use QUERY.ISZONEACTIVE",ErrMsg);
            }
        }
      else
        {
          IsOk = FALSE;
          StuffErrMsg("Bad parameters in call to QUERY.ISZONEACTIVE",ErrMsg);
        }


      if (IsOk)
        {
          if (ZoneIsActive)
            IsOk = SetMacroVarValue(IntrinsicVarName,"YES",ErrMsg);
          else
            IsOk = SetMacroVarValue(IntrinsicVarName,"NO",ErrMsg);
        }
    }
  else if (IsCommand(MacroCommandString,QUERY_ISADDONLOADED_CMD))
    {
      /*
       * $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ISADDONLOADED ADDONID VVV'
       *
       * Return "YES" if Addon ADDONID is loaded, otherwise return "NO"
       */

      char        IntrinsicVarName[MAXINTRINSICVARSTRLEN+1];
      char        AddOnID[128];
      char       *CPtr;
      Boolean_t   AddOnIsLoaded;

      CPtr = &MacroCommandString[strlen(QUERY_ISZONEACTIVE_CMD)+1];


      Str_RemoveWhiteSpace(&CPtr);

      if (Str_GetToken(&CPtr,127,AddOnID) &&
          Str_GetToken(&CPtr,MAXINTRINSICVARSTRLEN,IntrinsicVarName))
        {
          AddOnIsLoaded = TecUtilAddOnGetRegisteredInfo(AddOnID,
                                                        (char **)NULL, /* Version */
                                                        (char **)NULL);/* Author */
        }
      else
        {
          IsOk = FALSE;
          StuffErrMsg("Bad parameters in call to QUERY.ISADDONLOADED",ErrMsg);
        }


      if (IsOk)
        {
          if (AddOnIsLoaded)
            IsOk = SetMacroVarValue(IntrinsicVarName,"YES",ErrMsg);
          else
            IsOk = SetMacroVarValue(IntrinsicVarName,"NO",ErrMsg);
        }
    }
  else
    {
      StuffErrMsg("Unknown Command",ErrMsg);
      IsOk = FALSE;
    }

  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/*
 * When Tecplot first loads an add-on, it makes a 
 * call to initialize the add-on. This function
 * must be named InitTecAddOn, as shown below.
 */
EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{

  TecUtilLockOn();

  /*
   * The function TecUtilAddOnRegisterInfo() is the
   * only function that is REQUIRED to be called from
   * the initialization function.
   *
   * The information you give Tecplot by calling
   * this function will show up in the Help/About Add-ons
   * dialog box.
   */

  /*
   * Note that if your add-on requires a specific version of Tecplot,
   * you would check for that here using TecUtilGetTecplotVersion()
   */

  AddOnID = TecUtilAddOnRegister(100,
                                 ADDON_NAME,
                                 "V"ADDON_VERSION"("TecVersionId") "ADDON_DATE,
                                 "Amtec");

  TecUtilMacroAddCommandCallback(ADDON_NAME,
                                 MacroCommandCallback);

  TecUtilLockOff();
}
