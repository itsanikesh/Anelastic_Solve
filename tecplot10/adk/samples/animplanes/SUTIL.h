int Str_ustrncmp(const char *s1,
                 const char *s2,
                 int   Len);
int Str_ustrcmp(const char *s1,
                const char *s2);
void Str_RemoveThroughEndOfLine(char **CPtr);

void Str_CopySubString(char      *Target,
                       char      *Source,
                       LgIndex_t Index,
                       LgIndex_t Count);
void Str_RemoveWhiteSpace(char **CPtr);

Boolean_t Str_GetToken(char **CPtr,
                       int    MaxChars,
                       char  *Token);


Boolean_t Str_ReplaceString(char      **OldString,
                            const char *NewString);




void Str_TackOnString(char      **BaseString,
                      const char *StringToTack);


static void Str_TackOnSetTrailer(char     **Command,
                                 Boolean_t  OnARun,
                                 SetIndex_t TrailingValue);

void Str_BuildSetString(Set_pa    Set,
                        Boolean_t IncludeSquareBraces,
                        char    **SetString);


Boolean_t Str_ScanForSymbol(char **CPtr,
                                   char   Symbol);



Boolean_t Str_ScanForLgIndex(char      **CPtr,
                             LgIndex_t  *Value);


Boolean_t Str_ScanForDouble(char   **CPtr,
                            double  *Value);


Boolean_t Str_GetSet(char      *IString,
                     Set_pa    *Set,
                     Boolean_t HasOuterSquareBraces);


Boolean_t GetArgPair(char **CPtr,
                     char   CommandName[],
                     char   ValueString[],
                     int    MaxChars,
                     char **ErrMsgString);

Boolean_t Macro_GetLgIndexArg(const char *Command,
                              char       *ArgString,
                              LgIndex_t   Min,
                              LgIndex_t   Max,
                              LgIndex_t  *Value,
                              char      **ErrMsg);


Boolean_t Macro_GetSetArg(const char *Command,
                          char       *ArgString,
                          Set_pa     *Value,
                          char      **ErrMsg);

Boolean_t Macro_GetBooleanArg(const char *Command,
                              char       *ArgString,
                              Boolean_t  *Value,
                              char      **ErrMsg);

Boolean_t Macro_GetDoubleArg(const char *Command,
                             char       *ArgString,
                             double      Min,
                             double      Max,
                             double     *Value,
                             char      **ErrMsg);



Boolean_t Macro_GetEnumArg(const char *Command,
                           char       *ArgString,
                           char       *EnumList,
                           int        *Value,
                           char      **ErrMsg);
