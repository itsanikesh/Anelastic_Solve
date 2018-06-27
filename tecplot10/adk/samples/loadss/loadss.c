/*

   Loader Example Add-on:

   Loads a text file in the following form:

   Var 1, Var 2, ..., Var N
   data1,data2, ..., data N
   .
   .
   .
   data1,data2, ..., data N

   [Blank lines are ignored]

Notes:

1. There are no restrictions on the position
of the data, except that the variable names
must be in quotes and must be first.
*/


#include "TECADDON.h"
#include "ADDONVER.h"
#include "ADDGLBL.h"
#include "GUI.h"
#include "GUIDEFS.h"

#define MAX_VARS  1000     /* max number of variables allowed */

#ifndef ASSERT
#include <assert.h>
#define ASSERT(exp) assert(exp)
#endif


AddOn_pa AddOnID;

/*
 *
 * Exported
 *
 */
char *FileName;     /* File to load */
char *LastFileName; /* Previous file loaded */
int ISkip;          /* I Skip Value */
char Delimeter;     /* ',' or ' ' */
Boolean_t STDCALL LoadSS(StringList_pa sl);

/*
 * private to
 * this file
 */
static void STDCALL MenuCB(void);
static void STDCALL StateChangeCallback(StateChange_e WhichState,
                                        ArbParam_t NotUsed);

static Boolean_t DoLoadSS(FILE *f);


/*------------------------------------------------------------
 *
 * get_token
 *
 *------------------------------------------------------------*/
#define MAX_TOKEN_LEN 5000
static char _token[MAX_TOKEN_LEN]; /* global buffer for tokens */

static Boolean_t get_token(FILE *f)
{
  /* returns FALSE if no more tokens */

  int index = 0;
  char c;
  Boolean_t StopRightQuote;

  ASSERT(f);
  /*
   * Note that f is assumed to
   * have been opened in binary
   * mode.
   */

  /*
   * skip whitespace
   */
  while (fread(&c,sizeof(char),1,f) == 1
         && (c == ' ' || c == Delimeter || c == '\t' || c == '\n' || c == '\r'))
    {
      /* keep going */
    }

  if (!feof(f))
    {
      /*
       * now we're sitting on a non-whitespace character
       */

      StopRightQuote = (c == '"');
      if (StopRightQuote)
        {
          _token[index++] = c;
          fread(&c,sizeof(char),1,f);
        }



      do
        {
          if (index == MAX_TOKEN_LEN-1)
            break; /* ouch, we really shouldn't have lines > 5000 */

          if (feof(f))
            break;

          if (StopRightQuote)
            {
              if (c == '"')
                {
                  _token[index++] = c;
                  break;
                }
            }
          else
            {
              /* note that a space or comma may terminate the token */
              if (c == Delimeter || c == ' ' || c == '\t' || c == '\n' || c == '\r')
                break;
            }

          _token[index++] = c;
          fread(&c,sizeof(char),1,f);
        } while (1);
    }

  _token[index] = '\0';

  return (strlen(_token) > 0);
}


/*
 * Tecplot will explicitly call
 * InitTecAddOn() when the add-on
 * is first loaded. The only thing
 * we *have* to do is call
 * TecUtilAddOnRegisterInfo().
 */


EXPORTFROMADDON void STDCALL InitTecAddOn(void)
{
  /*
   * Remember: Tecplot must be locked
   * before calling any TecUtil functions...
   */
  TecUtilLockOn();
  AddOnID = TecUtilAddOnRegister(100,
                                 ADDON_NAME,
                                 ADDON_VERSION" ("TecVersionId") "ADDON_DATE,
                                 "Amtec Engineering, Inc.");

  FileName      = (char *) calloc(10000,sizeof(char));
  LastFileName  = (char *) calloc(10000,sizeof(char));

  if (TecUtilGetTecplotVersion() < MinTecplotVersionAllowed)
    {
      char buffer[256];
      sprintf(buffer,"Add-on \"%s\" requires Tecplot version %s or greater",ADDON_NAME,TecVersionId);
      TecUtilDialogErrMsg(buffer);
    }

  else
    {

      InitTGB(); /* Initialize the gui builder code */

      /*
       * Since we're a loader, 
       * we are responsible for collecting
       * loading parameters (such as skip values, filenames, etc.).
       * 
       * Note that if we were a converter, then Tecplot would
       * prompt the user for a filename and our only responsibility
       * would be to convert the data.
       */

      /*
       * Tecplot will call the menu callback
       * function MenuCB() when the user
       * selects the loader from the "import"
       * menu.
       *
       * Tecplot will call the actual loader function
       * (that is, the function which does the work
       * of loading the data) only when the user
       * loads a layout file. In that case,
       * the dialog will not be displayed.
       */

      TecUtilImportAddLoader(LoadSS,ADDON_NAME,MenuCB,NULL);

      strcpy(FileName,"");
      strcpy(LastFileName,"");
      TecUtilStateChangeAddCallback(StateChangeCallback);

      /*
       * Note that the number of
       * TecUtilLockFinish(AddOnID);    * TecUtilLockOff()'s must
       * equal the number of
       * TecUtilLockStart(AddOnID);    * TecUtilLockOn()'s
       */
    }

  TecUtilLockOff();
}

/*
 *
 * We allocate space for
 * the filename and last filename strings
 * in InitTecAddOn().
 * We free them here, when the user
 * quits tecplot
 *
 */

static void STDCALL StateChangeCallback(StateChange_e WhichState,
                                        ArbParam_t IrrelevantParam)
{
  (void) IrrelevantParam; /* not used */

  if(WhichState == StateChange_QuitTecplot) 
    {
      TecUtilLockStart(AddOnID);
      free(FileName);
      free(LastFileName);
      TecUtilLockFinish(AddOnID);
    }
}

/*
 * Menu Callback
 */

static void STDCALL MenuCB(void)
{
  Boolean_t OkToLoad = TRUE;
  TecUtilLockStart(AddOnID);

  if (TecUtilDataSetIsAvailable())
    OkToLoad = TecUtilDialogMessageBox("The current data set will be replaced.  Continue?",
                                       MessageBox_YesNo);

  if (OkToLoad)
    {
      BuildDialog1(MAINDIALOGID);
      TecGUIDialogLaunch(Dialog1Manager);
    }

  TecUtilLockFinish(AddOnID);
}

Boolean_t STDCALL LoadSS(StringList_pa sl)
{
  Boolean_t IsOk = TRUE;
  char *s = NULL;
  int i;
  FILE *f = NULL;

  TecUtilLockStart(AddOnID);
  strcpy(FileName,"");
  ISkip = 1;
  Delimeter = ',';


  /*
   * Unpack the instruction string
   */

  for (i=1;i<=TecUtilStringListGetCount(sl) && IsOk;i++)
    {
      s = TecUtilStringListGetString(sl,i);

      switch (*s)
        {
          case 'F': strcpy(FileName,s+1); break;
          case 'I': ISkip = atoi(s+1);    break;
          case 'D': Delimeter = *(s+1);   break;
          default:
            {
              TecUtilDialogErrMsg("Invalid instruction string");
              IsOk = FALSE;
            } break;
        }
      TecUtilStringDealloc(&s);
    }

  /*
   * 'S' is an alias for ' '
   * 'C' is an alias for ' '
   */

  if (toupper(Delimeter) == 'S')
    Delimeter = ' ';

  if (toupper(Delimeter) == 'C')
    Delimeter = ',';

  /*
   * Try to open the file
   */

  f = fopen(FileName,"rb");
  if (!f && IsOk)
    {
      TecUtilDialogErrMsg("Cannot open input file");
      IsOk = FALSE;
    }

  if (IsOk)
    {
      IsOk = DoLoadSS(f);
    }

  if (IsOk)
    {
      TecUtilImportSetLoaderInstr(ADDON_NAME,sl);
      TecUtilFrameSetPlotType(PlotType_XYLine);
      TecUtilRedraw(TRUE);
    }

  if (f)
    fclose(f);

  TecUtilLockFinish(AddOnID);
  return IsOk;
}

/* global variable */
static fpos_t _DataStartPos;

static void GetVars(FILE* f, StringList_pa sl)
{
  char  c;
  char  buffer[MAX_TOKEN_LEN];
  char  *Line = buffer;
  char  Var[MaxChrsVarName+1];
  int   Index = 0;

  /* read up to the first newline */

  do
    {
      if (Index >= MAX_TOKEN_LEN)
        break; /* don't overflow buffer */
      else if (fread(&c,sizeof(char),1,f) < 1)
        break;

      if (c != '\r' && c != '\n' && c != '\0')
        buffer[Index++] = c;
      else
        break;
    } while (1);

  buffer[Index] = '\0';

  /* now get the variable names */
  while (*Line)
    {
      Index = 0;
      if (*Line == '"')
        {
          /* skip to next double quote */
          Line++;
          while (*Line != '\0' && *Line != '"' && Index < MaxChrsVarName)
            Var[Index++] = *Line++;
        }
      else
        {
          /* just read to the next delimeter */
          while (*Line != '\0' && *Line != Delimeter && Index < MaxChrsVarName)
            Var[Index++] = *Line++;
        }

      Var[Index] = '\0';
      TecUtilStringListAppendString(sl,Var);

      /* now skip to the next non-delimeter char */
      while (*Line != '\0' && *Line != Delimeter)
        Line++;

      /* skip to next non-delimeter char */
      while (*Line != '\0' && (*Line == Delimeter || *Line == ' ' || *Line == '\t'))
        Line++;

      fgetpos(f,&_DataStartPos);
    }
}


static Boolean_t DoLoadSS(FILE *f)
{
  Boolean_t IsOk = TRUE;
  StringList_pa sl_var = TecUtilStringListAlloc(); /* variable list */
  long FileSize;
  int i;
  int NumValues;
  int NumVars;
  FieldDataType_e *fd_types;
  int idim;
  int imax;


  /*
   * compute the file size
   */

  fseek(f,0,SEEK_END);
  FileSize = ftell(f);
  rewind(f);

  /*
   * First,
   * we need to read
   * all of the variables,
   */

  GetVars(f,sl_var);


  /*
   * Now we have all of the variables.
   * There must be at least one
   */

  if (IsOk && TecUtilStringListGetCount(sl_var) < 1)
    {
      TecUtilDialogErrMsg("No variables defined");
      IsOk = FALSE;
    }

  /*
   * Count the number of data points
   */

  if (IsOk)
    {
      NumValues = 0;
      NumVars = TecUtilStringListGetCount(sl_var);

      TecUtilDialogLaunchPercentDone("Scanning File...",TRUE);
      while(get_token(f))
        {
          TecUtilDialogCheckPercentDone((int)(100.0*ftell(f)/FileSize));
          NumValues++;
        }
      TecUtilDialogDropPercentDone();


      fsetpos(f,&_DataStartPos); /* rewind to the start of the data */

      /*
       * compute the number of data points
       */
      idim = NumValues / NumVars;
      imax = idim;

      if (imax < 1)
        {
          TecUtilDialogErrMsg("Invalid number of datapoints");
          IsOk = FALSE;
        }

      if (IsOk && ISkip > 1)
        {
          imax = 0;
          for (i=1;i<=idim;i += ISkip)
            imax++;
          if (i - ISkip != idim)
            imax++; /* always include the last point */
        }


      if (IsOk && !TecUtilDataSetCreate("Converted Dataset",NULL,TRUE))
        {
          TecUtilDialogErrMsg("Could not create datset");
          IsOk = FALSE;
        }


      if (IsOk)
        {
          char *s;
          for (i=1;i<=NumVars && IsOk;i++)
            {
              s = TecUtilStringListGetString(sl_var,i);
              if (!TecUtilDataSetAddVar(s,NULL))
                {
                  TecUtilDialogErrMsg("Could not add variable");
                  IsOk = FALSE;
                }
              TecUtilStringDealloc(&s);
            }

          fd_types = (FieldDataType_e*) calloc(NumVars,sizeof(FieldDataType_e));
          for (i=0;i<NumVars;i++)
            fd_types[i] = FieldDataType_Double;

          if (IsOk && !TecUtilDataSetAddZone("Zone 1",imax,1,1,ZoneType_Ordered,fd_types))
            {
              TecUtilDialogErrMsg("Could not add zone");
              IsOk = FALSE;
            }

        }

      /* Now add the data (see the note above) */

      if (IsOk)
        {

          LgIndex_t PointIndex = 1;
          EntIndex_t j;
          int Skip = 0;
          double *LineValues = (double*) calloc(NumVars,sizeof(double));

          TecUtilDialogLaunchPercentDone("Adding Data...",TRUE);
          for (i=1;i<=idim;i++)
            {
              TecUtilDialogCheckPercentDone((int)(100.0*ftell(f)/FileSize));
              for (j=0;j<NumVars;j++)
                {
                  get_token(f);
                  LineValues[j] = atof(_token);
                }

              Skip += 1;

              if (i==1 || i==idim || Skip==ISkip)
                {
                  Skip = 0;
                  for (j=0;j<NumVars;j++)
                    TecUtilDataValueSetByZoneVar((EntIndex_t)1,(EntIndex_t)(j+1),PointIndex,LineValues[j]);
                  PointIndex++;
                }
            }

          free(LineValues);
          free(fd_types);
          TecUtilDialogDropPercentDone();
        }

    }



  TecUtilStringListDealloc(&sl_var);
  return IsOk;
}

