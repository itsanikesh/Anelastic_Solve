#include "TECADDON.h"
#include "ADDGLBL.h"
#include "TASSERT.h"
#include "ENGINE.h"



/* DOCSTART:gr/converter_get_token.txt */
/**
 */
#define MAX_TOKEN_LEN 5000
static char _token[MAX_TOKEN_LEN]; /* Global buffer for tokens. */

/**
 * Get the next token.
 *
 * @param f
 *     Open file handle. The file must be open for binary reading.
 * @return
 *     TRUE if more a token was fetched, FALSE otherwise.
 */
static Boolean_t get_token(FILE *f)
{
  int index = 0;
  char c;
  Boolean_t StopRightQuote;

  /* Skip white space. */
  while (fread(&c,sizeof(char),1,f) == 1 &&
         (c == ' ' || c == ',' || c == '\t' || c == '\n' || c == '\r'))
    {
      /* Keep going. */
    }

  if (!feof(f))
    {
      /* Now we're sitting on a non-white space character. */
      StopRightQuote = (c == '"');
      if (StopRightQuote)
        {
          _token[index++] = c;
          fread(&c,sizeof(char),1,f);
        }

      do 
        {
          if (index == MAX_TOKEN_LEN-1)
            break; /* Lines shouldn't be longer than 5,000 characters. */

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
              /* Note that a space or comma may terminate the token. */
              if (c == ' ' || c == ',' || c == '\t' || c == '\n' || c == '\r')
                break;
            }

          _token[index++] = c;
          fread(&c,sizeof(char),1,f);
        } while(1);
    }

  _token[index] = '\0';

  return (strlen(_token)) > 0;
}
/* DOCEND */

/* DOCSTART:gr/converter_GetVars.txt */
/**
 */
static fpos_t _DataStartPos;

/**
 * Reads a line of comman or space separated variables from the
 * top of the file to be imported. The variables may optionally
 * be enclosed in double quotes.
 *
 * @param f
 *     Open file handle. The file must be open for binary reading.
 * @return
 *     TRUE if more a token was fetched, FALSE otherwise.
 */
static void GetVars(FILE          *f,
                    StringList_pa  sl)
{
  char c;
  char buffer[5000];
  char *Line = buffer;
  char Var[100];
  int  Index = 0;
  char Delimiter = ' ';

  /* Read up to the first new line. */
  do
    {
      if (fread(&c,sizeof(char),1,f) < 1)
        break;

      if (c != '\r' && c != '\n' && c != '\0')
        buffer[Index++] = c;
      else
        break;
    } while (1);

  buffer[Index] = '\0';

  /* Now get the variable names. */
  while (*Line)
    {
      Index = 0;
      if (*Line == '"')
        {
          /* Skip to next double quote. */
          Line++;
          while (*Line && *Line != '"')
            Var[Index++] = *Line++;
        }
      else
        {
          /* Read to the next delimiter. */
          while (*Line && *Line != Delimiter)
            Var[Index++] = *Line++;
        }

      Var[Index] = '\0';
      TecUtilStringListAppendString(sl,Var);

      /* Skip to the next non-delimiter char. */
      while (*Line && *Line != Delimiter)
        Line++;

      fgetpos(f,&_DataStartPos);

      /* Skip to next non-delimiter char. */
      while (*Line && (*Line == Delimiter || *Line == ' '))
        Line++;
    }
}
/* DOCEND */

/**
 */
/* DOCSTART:gr/converter_DoConversion.txt */
static Boolean_t DoConversion(FILE  *f,
                              char  *TempFName,
                              char **MessageString)
{
  Boolean_t IsOk = TRUE;
  StringList_pa VarList = TecUtilStringListAlloc(); /* Variable list. */
  int i;
  int NumValues;
  int NumVars;
  int IMax;

  /* First, we need to read all of the variables. */
  GetVars(f,VarList);

  /* Make sure there is at least one variable. */
  if (IsOk && TecUtilStringListGetCount(VarList) < 1)
    {
      strcpy(*MessageString,"No variables defined.");
      IsOk = FALSE;
    }

  if (IsOk)
    { 
      /* Debug and VIsDouble are flags used by TecUtilTecIni(). */
      int Debug = 0;
      int VIsDouble = 1;

      /* Set JMax and KMax to 1 because we are creating an. */
      /* I-ordered data set. */

      int JMax=1,KMax=1;
      char VarNames[5000];
      char *s;

      NumValues = 0;

      /* VarList was filled by the function GetVars. */
      NumVars = TecUtilStringListGetCount(VarList);

      /* Count the number of data points. */
      while (get_token(f))
        {
          NumValues++;
        }

      /*
       * Get_token() changed where the file pointer is pointing, so
       * we must rewind to the start of the data. 
       */
      fsetpos(f,&_DataStartPos);

      /* Compute the number of data points. */
      IMax = NumValues/NumVars;

      /* FillVarNames with the variable names in VarList. */
      strcpy(VarNames,"");
      for (i=1; i<=NumVars && IsOk; i++)
        {
          s = TecUtilStringListGetString(VarList,i);
          strcat(VarNames,s);
          if (i<NumVars)
            strcat(VarNames,",");
          TecUtilStringDealloc(&s);
        }

      /*
       * Use the TecUtilTecIni() function to initialize the TempFName
       * file and fill it with the data set title and the variable name.
       */
      if (TecUtilTecIni("ConvertedDataset", VarNames, 
                        TempFName,".",&Debug,&VIsDouble) != 0)
        {
          strcpy(*MessageString,"Could not create data set.");
          IsOk = FALSE;
        }

      /*
       * Use TecUtilTecZne to add the first zone.
       * In this case, it is the only zone. 
       */
      if (IsOk && TecUtilTecZne("Zone 1",
                                &IMax,&JMax,&KMax,
                                "POINT",NULL) != 0)
        {
          strcpy(*MessageString,"Could not add zone.");
          IsOk = FALSE;
        }

      /* Now add the data. */
      if (IsOk)
        {
          LgIndex_t PointIndex = 1;
          int Skip = 0;

          /* Allocate space to temporarily store the values. */
          double *LineValues = (double*) calloc(NumValues,sizeof(double));

          /* Get the values into the array LineValues. */
          for (i=0; i<NumValues; i++)
            {
              get_token(f);
              LineValues[i] = atof(_token);
            }

          /*
           * Use the function TecUtilTecDat() to fill the 
           * temporary file with the values stored in the LineValues. 
           */
          if (TecUtilTecDat(&NumValues,(void*)LineValues,&VIsDouble) != 0)
            {
              strcpy(*MessageString,"Error loading data.");
              IsOk = FALSE;
            }

          /* Free LineValues now that we are done using it. */
          free(LineValues);
        }
    }

  /* Calling TecUtilTecEnd() closes the temporary file. */
  if (TecUtilTecEnd() != 0)
    {
      IsOk = FALSE;
      strcpy(*MessageString,"Error closing temporary file, "
                            "could not create data set.");
    }

  TecUtilStringListDealloc(&VarList);
  return IsOk;
}
/* DOCEND */

/**
 * Registered callback to convert a foreign datafile into a
 * Tecplot Binary datafile format.
 *
 * @param DataFName
 *   Name of the original foreign data file to be converted.
 *
 * @param TempBinFName
 *   Name of the temporary binary datafile that is created (by
 *   your converter).
 *
 * @param MessageString
 *   Reference to a string.  If an error occurs during conversion
 *   then allocate space for an error message and put that
 *   message in MessageString.
 *
 * @return
 *   Return TRUE if the conversion is successful. Otherwise
 *   return FALSE.  If FALSE is returned then MessageString is
 *   assumed to contain an error message.
 */
/* DOCSTART:gr/converter_ConverterCallback.txt */
Boolean_t STDCALL ConverterCallback(char  *DataFName,
                                    char  *TempBinFName,
                                    char **MessageString)
{

  Boolean_t IsOk = TRUE;
  FILE *f;

  TecUtilLockStart(AddOnID);

  /* If there is no error, remember to free MessageString. */
  *MessageString = TecUtilStringAlloc(1000,"MessageString for CNVSS");

  /* Try to open the file. */
  f = fopen(DataFName,"rb");

  /* Make sure the file was opened. */
  if (!f)
    {
      strcpy(*MessageString,"Cannot open input file.");
      IsOk = FALSE;
    }

  /* Do the conversion. */
  if (IsOk)
    IsOk = DoConversion(f,TempBinFName,MessageString);

  /* Close the file. */
  fclose(f);

  /* If there was no errors, deallocate MessageString. */
  if (IsOk)
    TecUtilStringDealloc(MessageString);
  
  TecUtilLockFinish(AddOnID);
  return IsOk;
}
/* DOCEND */

