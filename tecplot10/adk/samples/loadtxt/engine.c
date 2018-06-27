#include "TECADDON.h"
extern AddOn_pa AddOnID;

#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include "ENGINE.h"

/* THE global state */
extern LoadTxtGlobalState_s GS;
#define MAX_LINE_SIZE   2000 /* Increase if you need longer lines */
#define MAX_VARIABLES   64   /* Max number of variables */
#define STANDARDSYNTAX  "STANDARDSYNTAX"
#define FILENAME_TOLOAD "FILENAME_TOLOAD"
#define SKIP            "SKIP"

/**
 */
static int Str_ustrncmp(const char *s1,
                        const char *s2,
                        int         Len)
{
  char *t1;
  char *t2;
  char ct1;
  char ct2;
  int  I = 0;

  if ((s1 == NULL) && (s2 == NULL))
    return (0);
  if ((s1 == NULL) || (s2 == NULL))
    return (1);

  t1 = (char*)s1;
  t2 = (char*)s2;

  while (*t1 && *t2 && (I < Len))
    {
      ct1 = (char)CAPITAL(*t1);
      ct2 = (char)CAPITAL(*t2);
      if (ct1 != ct2)
        return (1);
      t1++;
      t2++;
      I++;
    }
  if ((I == Len) ||
      ((*t1 == '\0') && (*t2 == '\0')))
    return (0);
  return (1);
}

/**
 */
static int Str_ustrcmp(const char *s1,
                       const char *s2)
{
  int L1,L2;
  if ((s1 == NULL) && (s2 == NULL))
    return (0);
  if ((s1 == NULL) || (s2 == NULL))
    return (1);

  L1 = strlen(s1);
  L2 = strlen(s2);
  if (L1 != L2)
    return (1);
  return (Str_ustrncmp(s1,s2,L1));
}

/**
 */
/* DOCSTART:gr/loadtxt_LoaderSelectedCallback.txt */
void STDCALL LoaderSelectedCallback(void)
{
  Boolean_t OkToLoad = TRUE;
  TecUtilLockStart(AddOnID);

  if (TecUtilDataSetIsAvailable())
    OkToLoad = TecUtilDialogMessageBox("The current data set will "
                                       "be replaced. Continue?",
                                       MessageBox_YesNo);

  if (OkToLoad)
    {
      BuildDialog1(MAINDIALOGID);
      TecGUIDialogLaunch(Dialog1Manager);
    }

  TecUtilLockFinish(AddOnID);
}
/* DOCEND */

/* DOCSTART:gr/loadtxt_ParseNewSyntax.txt */
/**
 * Add-on's using the standard syntax have better integration with Tecplot.
 *
 * New Syntax:
 *
 *     Name                Value          Required   Default
 *     -----------------------------------------------------
 *     STANDARDSYNTAX      1.0            Yes        N/A
 *     FILENAME_TOLOAD     "myfile.txt"   Yes        N/A
 *     SKIP                3              No         1
 */
static Boolean_t ParseNewSyntax(StringList_pa   NewInstructions,
                                char          **FileName,
                                int            *Skip)
{
  Boolean_t IsOk;
  LgIndex_t NumParams;
  Boolean_t FileNameFound = FALSE;
  Boolean_t SkipFound     = FALSE;

  REQUIRE(VALID_REF(NewInstructions));
  REQUIRE(VALID_REF(FileName) && *FileName == NULL);
  REQUIRE(VALID_REF(Skip) && *Skip == 1);

  NumParams = TecUtilStringListGetCount(NewInstructions);
  IsOk = (NumParams == 4 || NumParams == 6);
  if (IsOk)
    {
      LgIndex_t i;
      /*
       * The first name value pair is "STANDARDSYNTAX" / "1.0"; start on
       * the second name value pair at position 3.
       */
      for (i=3;i<=NumParams;i+=2)
        {
          const char *ParamStrRef = TecUtilStringListGetRawStringPtr(
                                        NewInstructions,i);
          if (Str_ustrcmp(FILENAME_TOLOAD, ParamStrRef) == 0)
            {
              IsOk = (!FileNameFound);
              if (IsOk)
                {
                  *FileName = TecUtilStringListGetString(
                                  NewInstructions,i+1);
                  FileNameFound = TRUE;
                }
              else
                TecUtilDialogErrMsg("Cannot specify the FileName_ToLoad "
                                    "text loader option twice.");
            }
          else if (Str_ustrcmp(SKIP, ParamStrRef) == 0)
            {
              IsOk = (!SkipFound);
              if (IsOk)
                {
                  const char *SkipStrRef =
                      TecUtilStringListGetRawStringPtr(NewInstructions,i+1);
                  *Skip = atoi(SkipStrRef);
                  SkipFound = TRUE;
                }
              else
                TecUtilDialogErrMsg("Cannot specify the Skip text "
                                    "loader option twice.");
            }
          else
            {
              IsOk = FALSE;
              TecUtilDialogErrMsg("Unknown text loader option.");
            }
        }
    }
  else
    TecUtilDialogErrMsg("Standard loader syntax expects "
                        "name/value pairs.");

  return IsOk;
}
/* DOCEND */

/* DOCSTART:gr/loadtxt_ParseOldSyntax.txt */
/**
 * The old syntax is here for backward compatability. Now we use the
 * Tecplot's new standard syntax. See ParseNewSyntax().
 *
 * Old Syntax:
 *
 *     Flag                Value          Required   Default
 *     -----------------------------------------------------
 *     -F                  "myfile.txt"   Yes        N/A
 *     -I                  3              No         1
 */
static Boolean_t ParseOldSyntax(StringList_pa   OldInstructions,
                                char          **FileName,
                                int            *Skip)
{
  LgIndex_t i;
  LgIndex_t NumParams;
  Boolean_t FileNameFound = FALSE;
  Boolean_t SkipFound     = FALSE;

  REQUIRE(VALID_REF(OldInstructions));
  REQUIRE(VALID_REF(FileName) && *FileName == NULL);
  REQUIRE(VALID_REF(Skip) && *Skip == 1);

  NumParams = TecUtilStringListGetCount(OldInstructions);
  for (i=1;i<=NumParams;i++)
    {
      const char *ParamStrRef = TecUtilStringListGetRawStringPtr(
                                    OldInstructions,i);
      if (ParamStrRef[0] == '-'    && 
          strlen(ParamStrRef) >= 2 &&
          i < NumParams)
        {
          /* Found a parameter */
          switch(toupper(ParamStrRef[1]))
            {
              case 'F':
                {
                  /* Found filename */
                  if (!FileNameFound)
                    {
                      *FileName = TecUtilStringListGetString(
                                      OldInstructions,++i);
                      FileNameFound = TRUE;
                    }
                } break; /* 'F' */


              case 'I':
                {
                  if (!SkipFound)
                    {
                      const char *SkipStrRef =
                          TecUtilStringListGetRawStringPtr(
                              OldInstructions,++i);
                      *Skip = atoi(SkipStrRef);
                      SkipFound = TRUE;
                    }
                } break; /* 'I' */

              default:
                {
                  /* ignore unrecognized option */
                } break;
            }
        }
    }

  return TRUE;
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/loadtxt_LoaderCallback.txt */
Boolean_t STDCALL LoaderCallback(StringList_pa Instructions)
{
  Boolean_t IsOk = TRUE;
  LgIndex_t NumParams;
  char      *FileName = NULL;
  int       Skip      = 1; /* default */

  TecUtilLockStart(AddOnID);

  NumParams = TecUtilStringListGetCount(Instructions);
  if (NumParams != 0)
    {
      const char *ParamStrRef = TecUtilStringListGetRawStringPtr(
                                    Instructions, 1);
      if (Str_ustrcmp(ParamStrRef, STANDARDSYNTAX) == 0)
        IsOk = ParseNewSyntax(Instructions, &FileName, &Skip);
      else
        IsOk = ParseOldSyntax(Instructions, &FileName, &Skip);
    }

  if (IsOk)
    {
      if (FileName != NULL && strlen(FileName) > 0)
        DoLoadDelimitedText(FileName,Skip);
      else
        TecUtilDialogErrMsg("No filename specified");

      if (FileName != NULL)
        TecUtilStringDealloc(&FileName);
    }

  TecUtilLockFinish(AddOnID);

  /*
   * Note that you do not need to dealloc the string list 'Instructions' as
   * this will be done by Tecplot or the calling function
   */

  return IsOk;
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/loadtxt_IsBlankLine.txt */
static Boolean_t IsBlankLine(const char *Line)
{
  Boolean_t Result = TRUE;
  int       Index = 0;

  REQUIRE(VALID_REF(Line));

  /* 
   * Search through the string for any character that is not:
   * " \t\r\n".
   */

  while (Result && Line[Index] != '\0')
    {
      /* These are the only characters in a blank line */
      Result = (Line[Index] == ' '  ||
                Line[Index] == '\t' ||
                Line[Index] == '\n' ||
                Line[Index] == '\r');
      Index++;
    }

  return Result;
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/loadtxt_AddDataPoints.txt */
static void AddDataPoints(LgIndex_t   PointIndex,
                          const char *Line,
                          EntIndex_t  NumVars)
{
  /* Zone is always 1 */
  FieldData_pa FD;
  EntIndex_t i;
  char buffer[MAX_LINE_SIZE];
  double  Value;
  char *strDataPoint;

  REQUIRE(PointIndex > 0);
  REQUIRE(VALID_REF(Line));
  REQUIRE(NumVars > 0);

  strcpy(buffer,Line);

  /* must have a space between data points */
  strDataPoint = strtok(buffer," \t\r\n");

  for (i=1;i<=NumVars;i++)
    {
      FD = TecUtilDataValueGetRef(1,i);
      if (strDataPoint)
        {
          Value = atof(strDataPoint);
          /* get the next one */
          strDataPoint = strtok(NULL," \t\r\n");
        }
      else
        {
          /* Default: not enough values on this line */
          Value = 0.0;
        }

      TecUtilDataValueSetByRef(FD,PointIndex,Value);
    }
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/loadtxt_GetVariableNames.txt */
static void GetVariableNames(StringList_pa  VarList,
                             char          *Line)
{
  /* NOTE: We assume Tecplot is locked */
  char *Token;

  REQUIRE(VALID_REF(Line));

  /* Be sure that the string list is initially empty */
  TecUtilStringListClear(VarList);

  /* Note that strtok() will write into Line */
  Token = strtok(Line," \t\r\n");

  while (Token)
    {
      /* Token now points to a variable name, so add it to the list */
      TecUtilStringListAppendString(VarList,Token);
      Token = strtok(NULL," \t\r\n"); /* Get the next one */
    }
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/loadtxt_Pass1.txt */
static Boolean_t Pass1(int           *LineCount,
                       StringList_pa  VarNames,
                       FILE          *F)
{
  Boolean_t IsOk = TRUE;
  char Line[MAX_LINE_SIZE];

  REQUIRE(VALID_REF(LineCount));
  REQUIRE(VALID_REF(VarNames));
  REQUIRE(VALID_REF(F));

  while (fgets(Line,MAX_LINE_SIZE,F) != NULL)
    {
      if (!IsBlankLine(Line))
        {
          /* This must be the line with the variable names */
          GetVariableNames(VarNames,Line);
          break;
        }        
    }
  /* Must be at least one variable */
  if (TecUtilStringListGetCount(VarNames) == 0)
    {
      TecUtilDialogErrMsg("No variables specified");
      IsOk = FALSE;
    }

  if (IsOk)
    {
      /* 
       * Now count all the lines.
       */

      *LineCount = 0;

      while (fgets(Line,MAX_LINE_SIZE,F) != NULL)
        {
          if (!IsBlankLine(Line))
            {
              *LineCount += 1;
            }
        }

      if (*LineCount == 0)
        {
          TecUtilDialogErrMsg("No data specified");
          IsOk = FALSE;
        }
    }

  return IsOk;
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/loadtxt_Pass2_pre.txt */
static Boolean_t Pass2(int            Skip,
                       int            LineCount,
                       StringList_pa  VarNames,
                       FILE          *F)
{
  Boolean_t IsOk = TRUE;
  char Line[MAX_LINE_SIZE];
  EntIndex_t i;
  EntIndex_t VarCount;
  EntIndex_t PointIndex;
  FieldDataType_e fd_types[MAX_VARIABLES];
  int CurrentLine;
  int IMax;

  REQUIRE(Skip >= 1);
  REQUIRE(LineCount > 0);
  REQUIRE(VALID_REF(VarNames) &&
          TecUtilStringListGetCount(VarNames) > 0);
  REQUIRE(F != NULL);

  /*
   * At this point we know that there is a least one variable and at lease
   * one line of data. So it's safe to create the dataset.
   *
   * Note that once we create the dataset, we are committed to adding the
   * correct amount of data (otherwise we will leave Tecplot in an invalid
   * state), so we must be prepared to default some datapoints to 0 if they
   * cannot be read from the file.
   */
  VarCount = TecUtilStringListGetCount(VarNames);

  /* 
   * The data type for each value we read will be double.
   */
  for (i=0;i<VarCount;i++)
    fd_types[i] = FieldDataType_Double;

  rewind(F);

  /*
   * Skip to the first non-blank line.
   * This is the line with the variables.
   */
  while (fgets(Line,MAX_LINE_SIZE,F))
    {
      if(!IsBlankLine(Line))
        break;
    }

/* DOCEND */
/* DOCSTART:gr/loadtxt_Pass2_IMaxCalc.txt */
  if (LineCount > 1)
    IMax = 2 + (div(LineCount-2,Skip)).quot;
  else
    IMax = 1;
/* DOCEND */
/* DOCSTART:gr/loadtxt_Pass2_post.txt */

  IsOk = (IsOk &&
          TecUtilDataSetCreate("Converted Text Dataset",
                               VarNames, TRUE) &&
          TecUtilDataSetAddZone("Zone 1",IMax,1,1,
                                ZoneType_Ordered, fd_types));


  if (IsOk)
    {
      CurrentLine   = 1;
      PointIndex    = 1;

      TecUtilDialogLaunchPercentDone("Importing...",TRUE);

      do
        {
          /* Get a line */
          if (fgets(Line,MAX_LINE_SIZE,F))
            {
              /* Is it a blank line */
              if (IsBlankLine(Line))
                continue; /* Skip this line */

              /* Always include the first and last points when skipping */
              if ((CurrentLine-1) % Skip == 0 ||
                  CurrentLine == 1 ||
                  CurrentLine == LineCount)
                {
                  AddDataPoints(PointIndex,Line,VarCount);
                  PointIndex += 1;    
                }
            }
          else
            {
              break; /* Done */
            }

          TecUtilDialogCheckPercentDone(MIN(100,
                                            ((int)(100.0*CurrentLine) /
                                             LineCount)));
          CurrentLine += 1;    

        } while (TRUE);

      TecUtilDialogDropPercentDone();
    } /* end if */


  return IsOk;
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/loadtxt_DoLoadDelimitedText.txt */
Boolean_t DoLoadDelimitedText(const char *FileName,
                              int         Skip)
{
  Boolean_t IsOk    = TRUE;
  FILE      *F      = NULL;
  StringList_pa VarNames;
  StringList_pa LoaderInstructions;
  int       LineCount;

  REQUIRE(VALID_REF(FileName) && strlen(FileName) > 0);
  REQUIRE(Skip >= 1);

  TecUtilLockStart(AddOnID);
  VarNames = TecUtilStringListAlloc();

  /* Try to open the file */
  F = fopen(FileName,"r");

  if (F == NULL)
    {
      TecUtilDialogErrMsg("Cannot open file for reading");
      IsOk = FALSE;
    }


  IsOk = (IsOk                         &&
          Pass1(&LineCount,VarNames,F) &&
          Pass2(Skip,LineCount,VarNames,F));

  if (F)
    fclose(F);

  if (IsOk)
    {
      char strSkip[256];

      LoaderInstructions = TecUtilStringListAlloc();
      sprintf(strSkip,"%d",Skip);

      /*
       * NOTE:
       * 
       * The string written to the layout file will look like:
       * 
       *  '"STANDARDSYNTAX" "1.0" "FILENAME_TOLOAD" "myfile.txt" "SKIP" "3"'
       *
       * This is the recommended way to encode export parameters in
       * the instruction string.
       *
       */
      TecUtilStringListAppendString(LoaderInstructions,STANDARDSYNTAX);
      TecUtilStringListAppendString(LoaderInstructions,"1.0");
      TecUtilStringListAppendString(LoaderInstructions,FILENAME_TOLOAD);
      TecUtilStringListAppendString(LoaderInstructions,FileName);
      TecUtilStringListAppendString(LoaderInstructions,SKIP);
      TecUtilStringListAppendString(LoaderInstructions,strSkip);

      TecUtilImportSetLoaderInstr(ADDON_NAME,LoaderInstructions);
      TecUtilFrameSetPlotType(PlotType_XYLine);
      TecUtilRedraw(TRUE);

      TecUtilStringListDealloc(&LoaderInstructions);
    }

  TecUtilStringListDealloc(&VarNames);
  TecUtilLockFinish(AddOnID);

  return IsOk;
}
/* DOCEND */

