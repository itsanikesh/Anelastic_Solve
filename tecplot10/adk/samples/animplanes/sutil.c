#include <stdio.h>
#include <ctype.h>
#include "TECADDON.h"
#include "SUTIL.h"

#include <float.h>
/***************************************************************
 * Utility functions used to parse or build character strings. *
 ***************************************************************/






int Str_ustrncmp(const char *s1,
                 const char *s2,
                 int   Len)
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

int Str_ustrcmp(const char *s1,
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


#define PLAINWHITESPACE(C) (((C) == ' ') || ((C) == '\t'))

void Str_RemoveThroughEndOfLine(char **CPtr)
{
  while (**CPtr && (**CPtr != '\n'))
    (*CPtr)++;
  if (**CPtr == '\n')
    (*CPtr)++;
}



/**
 * Copy up to 'Count' characters from the 'Source' string beginning at
 * position 'Index' to the 'Target' string. The actual number of characters
 * copied may be less than 'Count' if a '\0' was encountered in the
 * 'Source' string before 'Count' characters were copied.
 *
 * NOTE: The 'Target' and 'Source' strings may overlap.
 *
 * preconditions
 *   - 'Target' is a valid string
 *   - 'Target' string is sized to accommodate a string who's length "
 *      is at least MIN(strlen(&Source[Index]), Count) characters
 *   - 'Source' is a valid string
 *   - 0 <= Index && Index <= (LgIndex_t)strlen(Source)
 *   - Count >= 0
 *
 * postconditions
 *   - 'Target' is a valid string
 *   - strlen(Target) == Length
 */
void Str_CopySubString(char      *Target,
                       char      *Source,
                       LgIndex_t Index,
                       LgIndex_t Count)
{
  LgIndex_t Length = 0;


  Length = strlen(&Source[Index]);
  if (Count < Length)
    Length = Count;
  memmove(Target, &Source[Index], Length);
  Target[Length] = '\0';
}


void Str_RemoveWhiteSpace(char **CPtr)
{
  while (PLAINWHITESPACE(**CPtr))
    (*CPtr)++;
}

#define IsQuoteChar(C) (((C) == '"') || ((C) == '\''))

Boolean_t Str_GetToken(char **CPtr,
                       int    MaxChars,
                       char  *Token)
{
  Boolean_t IsOk = TRUE;
  int       I = 0;

  Str_RemoveWhiteSpace(CPtr);

  if (**CPtr)
    {
      Boolean_t IsQuoted  = IsQuoteChar(**CPtr);
      short     QuoteChar = '\0';
      if (IsQuoted)
        QuoteChar = *(*CPtr)++;
    
      while (**CPtr              && 
             (**CPtr != '\n')    &&
             /*!PLAINWHITESPACE(**CPtr) && */ /* (See below) */
             (I < MaxChars)      &&
             (!IsQuoted || (**CPtr != QuoteChar)))
        {
          if (**CPtr == ' ' && !IsQuoted) /* Spaces are okay, but only inside quotes */
            break;

          if (IsQuoteChar(**CPtr) && !IsQuoted)
          {
            /* Unbalanced quotes */
            IsOk = FALSE;
            break;
          }

          Token[I] = (IsQuoted ? **CPtr : (char)CAPITAL(**CPtr));
          (*CPtr)++;
          I++;
        }
    
      if (IsQuoted)
        {
          if (**CPtr == QuoteChar)
            (*CPtr)++;
          else
            IsOk = FALSE;
        }
      Token[I] = '\0';
      while (IsOk && **CPtr && PLAINWHITESPACE(**CPtr))
        (*CPtr)++;
    }
  else
    IsOk = FALSE;
  return (IsOk);
}


Boolean_t Str_ReplaceString(char      **OldString,
                            const char *NewString)
{
  Boolean_t IsOk = TRUE;
  if (*OldString)
    TecUtilStringDealloc(OldString);
  if (NewString)
    {
      *OldString = TecUtilStringAlloc(strlen(NewString)+1,"new string");
      if (*OldString)
        strcpy(*OldString,NewString);
      else
        IsOk = FALSE;
    }
  return (IsOk);
}





/***************************************************************
 *                   Low level functions                       *
 ***************************************************************/
void Str_TackOnString(char      **BaseString,
                      const char *StringToTack)
{
  char *NewString;
  int   BaseStringLen = 0;
  if (*BaseString)
    BaseStringLen = strlen(*BaseString);

  NewString = TecUtilStringAlloc(BaseStringLen+strlen(StringToTack)+1,
                                 "new string");
  if (NewString)
    {
      if (*BaseString)
        strcpy(NewString,*BaseString);
      else
        *NewString = '\0';
      strcat(NewString,StringToTack);
    }
  if (*BaseString)
    TecUtilStringDealloc(BaseString);
  *BaseString = NewString;
}






static void Str_TackOnSetTrailer(char     **Command,
                                 Boolean_t  OnARun,
                                 SetIndex_t TrailingValue)
{
  char Trailer[40];
  if (OnARun)
    sprintf(Trailer,"-%ld",TrailingValue);
  else
    sprintf(Trailer,",%ld",TrailingValue);
  Str_TackOnString(Command,Trailer);
}

void Str_BuildSetString(Set_pa    Set,
                        Boolean_t IncludeSquareBraces,
                        char    **SetString)
{
  SetIndex_t LastValue = TECUTILSETNOTMEMBER;
  SetIndex_t LastInstalledValue = TECUTILSETNOTMEMBER;
  SetIndex_t NextValue = TECUTILSETNOTMEMBER;
  Boolean_t  OnARun = FALSE;

  if (Set)
    {
      NextValue = TecUtilSetGetNextMember(Set,NextValue);
      while (NextValue != TECUTILSETNOTMEMBER)
        {
          if (LastValue == TECUTILSETNOTMEMBER)
            {
              char TString[20];
              sprintf(TString,"%ld",NextValue);
              LastInstalledValue = NextValue;
              if (IncludeSquareBraces)
                Str_TackOnString(SetString,"[");
              Str_TackOnString(SetString,TString);
            }
          else if ((NextValue-LastValue) == 1)
            OnARun = TRUE;
          else 
            {
              if (OnARun)
                Str_TackOnSetTrailer(SetString,TRUE,LastValue);
    
              Str_TackOnSetTrailer(SetString,FALSE,NextValue);
    
              LastInstalledValue = NextValue;
              OnARun = FALSE;
            }
          LastValue = NextValue;
          NextValue = TecUtilSetGetNextMember(Set,NextValue);
        }

      if (LastValue == TECUTILSETNOTMEMBER) /* Empty set. */
        {
          if (IncludeSquareBraces)
            Str_TackOnString(SetString,"[");
        }
      else if (LastValue != LastInstalledValue)
        Str_TackOnSetTrailer(SetString,OnARun,LastValue);
    
      if (IncludeSquareBraces)
        Str_TackOnString(SetString,"]");
    }
  else
    {
      if (IncludeSquareBraces)
        Str_TackOnString(SetString,"[]");
    }
}


Boolean_t Str_ScanForSymbol(char **CPtr,
                                   char   Symbol)
{
  Boolean_t SymbolFound;

  Str_RemoveWhiteSpace(CPtr);

  SymbolFound = **CPtr == Symbol;

  if ((**CPtr) && SymbolFound)
    (*CPtr)++;

  return (SymbolFound);
}



Boolean_t Str_ScanForLgIndex(char      **CPtr,
                             LgIndex_t  *Value)
{
  Boolean_t IsOk;
  double    D = 0;
  int       I = 0;
  Boolean_t IsNegative = FALSE;

  Str_RemoveWhiteSpace(CPtr);

  if (**CPtr == '-')
    {
      IsNegative = TRUE;
      (*CPtr)++;
    }

  while (**CPtr && (I < 10) && isdigit(**CPtr))
    {
      D = D*10 + **CPtr - '0';
      (*CPtr)++;
      I++;
    }

  IsOk = (I > 0) && !(I == 10 && isdigit(**CPtr));

  if (IsOk)
    *Value = (LgIndex_t)(D+0.001);
     
  if (IsNegative)
    *Value = -(*Value);

  return (IsOk);
}


Boolean_t Str_ScanForDouble(char   **CPtr,
                            double  *Value)
{
  Boolean_t IsOk = TRUE;
  int       iexp = 0;
  char     *NonBlankStart;

  Str_RemoveWhiteSpace(CPtr);

  if (**CPtr == '\0')
    IsOk = FALSE;

  NonBlankStart = *CPtr;

  if ((**CPtr == '-') || (**CPtr == '+'))
    (*CPtr)++;
  while (isdigit(**CPtr))
    (*CPtr)++;
  if (**CPtr == '.')
    (*CPtr)++;

  if (*CPtr == NonBlankStart)
    IsOk = FALSE;

  while (IsOk && isdigit(**CPtr))
    (*CPtr)++;

  if (IsOk &&
      ((**CPtr == 'E') || 
       (**CPtr == 'e') ||
       (**CPtr == 'D') ||
       (**CPtr == 'd')))
    {
      (*CPtr)++;
      if ((**CPtr == '-') || (**CPtr == '+'))
        (*CPtr)++;
      if (isdigit(**CPtr))
        {
          iexp = **CPtr - '0';
          (*CPtr)++;
        }
      else
        IsOk = FALSE;

      /* Use a while loop so that we can
         handle exponents larger than 3-digits,
         although in reality if the exponent is
         larger than 3 digits, the first digit(s)
         must be 0. */

      if ( IsOk )
        {
          while ( isdigit(**CPtr) )
            {
              iexp = iexp*10 + **CPtr - '0';
              (*CPtr)++;
            }
        }
    }
  if ((iexp > LARGESTDOUBLEEXPONENT) ||
      (iexp < SMALLESTDOUBLEEXPONENT))
    IsOk = FALSE;
     
  if (IsOk)
    {
      /* This used to be PLAINWHITESPACE (which excluded CR/LF), for some reason */
      if (**CPtr && !isspace(**CPtr))
        IsOk = FALSE;
    }
    
  if (IsOk)
    *Value = atof(NonBlankStart);

  return (IsOk);
}







/*
 *  A set must have the form: (s[,s][,s]...)
 *  where s can be one of:  n or n-m:l
 */
Boolean_t Str_GetSet(char      *IString,
                     Set_pa    *Set,
                     Boolean_t HasOuterSquareBraces)
{
  Boolean_t IsOk = TRUE;
  char     *IPtr = IString;
  LgIndex_t I1=0,I2=0,ISkip=0;

  *Set = TecUtilSetAlloc(TRUE);
  if (*Set == NULL)
    IsOk = FALSE;

  Str_RemoveWhiteSpace(&IPtr); 

  if (IsOk && HasOuterSquareBraces)
    IsOk = Str_ScanForSymbol(&IPtr,'[');

  while (IsOk && *IPtr)
    {
      if (HasOuterSquareBraces && Str_ScanForSymbol(&IPtr,']'))
        break;

      if (!Str_ScanForLgIndex(&IPtr,&I1))
        IsOk = FALSE;

      if (I1 <= 0)
        IsOk = FALSE;

      if (IsOk && Str_ScanForSymbol(&IPtr,'-'))
        {
          if (!Str_ScanForLgIndex(&IPtr,&I2))
            IsOk = FALSE;

          if ((I2 <= 0) || (I2 < I1))
            IsOk = FALSE;
          if (IsOk && Str_ScanForSymbol(&IPtr,':'))
            {
              if (!Str_ScanForLgIndex(&IPtr,&ISkip))
                IsOk = FALSE;
              if (ISkip <= 0)
                IsOk = FALSE;
            }
          else
            ISkip = 1;
        }
      else
        {
          I2    = I1;
          ISkip = 1;
        }

      if (IsOk)
        {
          LgIndex_t I;
          for (I = I1; I <= I2; I += ISkip)
            {
              if (!TecUtilSetAddMember(*Set,I,TRUE))
                IsOk = FALSE;
            }
        }
      else
        {
          IsOk = FALSE;
        }
      if (IsOk)
        {
          Str_ScanForSymbol(&IPtr,',');
          Str_RemoveWhiteSpace(&IPtr);
        }
    }
  if (!IsOk && *Set)
    TecUtilSetDealloc(Set);

  return (IsOk);
}







Boolean_t GetArgPair(char **CPtr,
                     char   CommandName[],
                     char   ValueString[],
                     int    MaxChars,
                     char **ErrMsgString)
{
  Boolean_t IsOk = TRUE;

  IsOk = (Str_GetToken(CPtr,MaxChars,CommandName) > 0);
  
  if (IsOk)
    {
      /*
       * Look for "="
       */
      Str_RemoveWhiteSpace(CPtr);
      if (**CPtr != '=')
        {
          *ErrMsgString = TecUtilStringAlloc(MaxChars+50,"err string");
          if (*ErrMsgString)
            sprintf(*ErrMsgString,"Missing \"=\" in command: %s",CommandName);
          IsOk = FALSE;
        }
      else
        {
          (*CPtr)++;
          IsOk = (Str_GetToken(CPtr,MaxChars,ValueString) > 0);
          if (!IsOk)
            {
              *ErrMsgString = TecUtilStringAlloc(MaxChars+80,"err string");
              if (*ErrMsgString)
                sprintf(*ErrMsgString,"Invalid assignment for command: %s",CommandName);
            }
        }
    }
  else
    {
      *ErrMsgString = TecUtilStringAlloc(MaxChars+50,"err string");
      if (*ErrMsgString)
        strcpy(*ErrMsgString,"Invalid command");
      IsOk = FALSE;
    }
  Str_RemoveThroughEndOfLine(CPtr);
  return (IsOk);
}

Boolean_t Macro_GetLgIndexArg(const char *Command,
                              char       *ArgString,
                              LgIndex_t   Min,
                              LgIndex_t   Max,
                              LgIndex_t  *Value,
                              char      **ErrMsg)
{
  Boolean_t IsOk = TRUE;
  char     *CPtr = ArgString;
  if (!Str_ScanForLgIndex(&CPtr,Value))
    {
      *ErrMsg = TecUtilStringAlloc(strlen(Command)+
                                   strlen(ArgString)+
                                   80,"err string");
      if (*ErrMsg)
        sprintf(*ErrMsg,
                "Invalid integer value (%s) for command: %s",
                ArgString,
               Command);
      IsOk = FALSE;
    }
  if (IsOk)
    {
      if ((*Value < Min) || (*Value > Max))
        {
          *ErrMsg = TecUtilStringAlloc(strlen(Command)+
                                       strlen(ArgString)+
                                       120,"err string");
          if (*ErrMsg)
            sprintf(*ErrMsg,
                    "Integer value for command: %s is out of range. "
                    "Value must be between %d and %d.",
                    Command,
                    Min,
                    Max);
          IsOk = FALSE;
        }
    }
  return (IsOk);
}


Boolean_t Macro_GetSetArg(const char *Command,
                          char       *ArgString,
                          Set_pa     *Value,
                          char      **ErrMsg)
{
  Boolean_t IsOk = TRUE;
  if (!Str_GetSet(ArgString,Value,TRUE))
    {
      *ErrMsg = TecUtilStringAlloc(strlen(Command)+
                                   strlen(ArgString)+
                                   80,"err string");
      if (*ErrMsg)
        sprintf(*ErrMsg,
                "Invalid set value (%s) for command: %s",
                ArgString,
                Command);
      IsOk = FALSE;
    }
  return (IsOk);
}

Boolean_t Macro_GetBooleanArg(const char *Command,
                              char       *ArgString,
                              Boolean_t  *Value,
                              char      **ErrMsg)
{
  Boolean_t IsOk = TRUE;
  if ((Str_ustrcmp(ArgString,"true") != 0)  &&
      (Str_ustrcmp(ArgString,"false") != 0) &&
      (Str_ustrcmp(ArgString,"yes") != 0)   &&
      (Str_ustrcmp(ArgString,"no") != 0)    &&
      (Str_ustrcmp(ArgString,"on") != 0)    &&
      (Str_ustrcmp(ArgString,"off") != 0))
    {
      *ErrMsg = TecUtilStringAlloc(strlen(Command)+
                                   strlen(ArgString)+
                                   80,"err string");
      if (*ErrMsg)
        sprintf(*ErrMsg,
                "Invalid Boolean value (%s) for command: %s",
                ArgString,
                Command);
      IsOk = FALSE;
    }
  else
    *Value = (Str_ustrcmp(ArgString,"true") == 0)  ||
             (Str_ustrcmp(ArgString,"on") == 0)    ||
             (Str_ustrcmp(ArgString,"yes") == 0);
  return (IsOk);
}

Boolean_t Macro_GetDoubleArg(const char *Command,
                             char       *ArgString,
                             double      Min,
                             double      Max,
                             double     *Value,
                             char      **ErrMsg)
{
  char     *CPtr = ArgString;
  Boolean_t IsOk = TRUE;
  if (!Str_ScanForDouble(&CPtr,Value))
    {
      *ErrMsg = TecUtilStringAlloc(strlen(Command)+
                                   strlen(ArgString)+
                                   80,"err string");
      if (*ErrMsg)
        sprintf(*ErrMsg,
                "Invalid double value (%s) for command: %s",
                ArgString,
                Command);
      IsOk = FALSE;
    }
  if (IsOk)
    {
      if ((*Value < Min) || (*Value > Max))
        {
          *ErrMsg = TecUtilStringAlloc(strlen(Command)+
                                       strlen(ArgString)+
                                       120,"err string");
          if (*ErrMsg)
            sprintf(*ErrMsg,
                    "Integer value for command: %s is out of range. "
                    "Value must be between %G and %G.",
                    Command,
                    Min,
                    Max);
          IsOk = FALSE;
        }
    }
  return (IsOk);
}



Boolean_t Macro_GetEnumArg(const char *Command,
                           char       *ArgString,
                           char       *EnumList,
                           int        *Value,
                           char      **ErrMsg)
{
  char     *EPtr = EnumList;
  Boolean_t IsOk = TRUE;
  int       ArgLen = strlen(ArgString);
  int       CurEnum = 0;

  *Value = -1;

  while ((*Value == -1) && *EPtr)
    {
      char NextEnum[50];
      CurEnum++;
      strncpy(NextEnum,EPtr,ArgLen);
      NextEnum[ArgLen] = '\0';
      if ((Str_ustrcmp(NextEnum,ArgString) == 0) &&
          ((EPtr[ArgLen] == '\0') || (EPtr[ArgLen] == ',')))
        {
          *Value = CurEnum;
        }
      else
        {
          EPtr = strchr(EPtr,',');
          if (*EPtr)
            EPtr++;
        }
    }

  if (*Value == -1)
    {
      *ErrMsg = TecUtilStringAlloc(strlen(Command)+
                                   strlen(ArgString)+
                                   strlen(EnumList)+
                                   120,"err string");
      if (*ErrMsg)
        sprintf(*ErrMsg,
                "Invalid enum value (%s) for command: %s\n"
                "Need to choose one of the following\n"
                "%s",
                ArgString,
                Command,
                EnumList);
      IsOk = FALSE;
    }
  return (IsOk);
}
