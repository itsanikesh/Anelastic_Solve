#include "TECADDON.h"
extern AddOn_pa AddOnID;

#include "ADDGLBL.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"

extern char *FileName;     /* File to load */
extern char *LastFileName; /* Previous file loaded */
extern LgIndex_t ISkip;    /* I Skip Value */
extern char Delimeter;     /* ',' or ' ' */
extern Boolean_t STDCALL LoadSS(StringList_pa sl);

/**
 */
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilHelp("tecplot/import_text_spreadsheet.htm", FALSE, 0);
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void Dialog1CancelButton_CB(void)
{
  TecGUIDialogDrop(Dialog1Manager);
/*Modal Dialogs must call TecUtilLockStart prior to coming*/
/*up and then call TecUtilLockFinish when the Ok or Cancel*/
/*button is pressed.  Only TecUtilLockFinish is supplied here.*/
  TecUtilLockFinish(AddOnID);
}




static void Dialog1OkButton_CB(void)
{
  char *buffer;
  StringList_pa sl;
  
  if (strlen(FileName) < 1)
    TecUtilDialogErrMsg("Please enter a file name");
  
  else
    {
      buffer = TecUtilStringAlloc(10000,"Temp storage");
      TecGUIDialogDrop(Dialog1Manager);
      /*Modal Dialogs must call TecUtilLockStart prior to coming*/
      /*up and then call TecUtilLockFinish when the Ok or Cancel*/
      /*button is pressed.  Only TecUtilLockFinish is supplied here.*/

      /*
       * Create a fake instruction string...
       */
      sprintf(buffer,"F%s\nI%d\nD%c",FileName,ISkip,Delimeter);
      sl = TecUtilStringListFromNLString(buffer);
      if (!LoadSS(sl))
        TecUtilDialogErrMsg("Error reading file");

      TecUtilStringListDealloc(&sl);
      TecUtilStringDealloc(&buffer);
      TecUtilLockFinish(AddOnID);
    }
}

/**
 */
static void Dialog1Init_CB(void)
{
/*Modal Dialogs must call TecUtilLockStart prior to coming*/
/*up and then call TecUtilLockFinish when the Ok or Cancel*/
/*button is pressed.*/
  TecUtilLockStart(AddOnID);
  ISkip = 1;
  Delimeter = ',';

  TecGUIRadioBoxSetToggle(Delimeter_RADIO_D1,1);
  TecGUITextFieldSetString(ISkip_TF_D1,"1");
}

/**
 */
static void Delimeter_RADIO_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  switch (*I)
    {
      case 1: Delimeter = ',';  break;
      case 2: Delimeter = ' ';  break;
      case 3: Delimeter = '\t'; break;
      default: CHECK(FALSE); break;
    }
  TecUtilLockFinish(AddOnID);
}

/**
 */
static int  ISkip_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);

  /* validate the entry if invalid replace with the original value */
  if (!(TecGUITextFieldValidateLgIndex(ISkip_TF_D1, "I-Skip value", 1, MAXINDEX, FALSE) &&
        TecGUITextFieldGetLgIndex(ISkip_TF_D1, &ISkip)))
    TecGUITextFieldSetLgIndex(ISkip_TF_D1, ISkip, FALSE);

  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static int  FileName_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  strcpy(FileName,S);
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}

/**
 */
static void SelectFile_BTN_D1_CB(void)
{
  char *fname = NULL;
  char *type = "Spreadsheet text files";
  char *filter = "*.txt";

  TecUtilLockStart(AddOnID);
  if (TecUtilDialogGetFileName(SelectFileOption_ReadSingleFile,
                               &fname,type,LastFileName,filter))
    {
      strcpy(LastFileName,fname);
      strcpy(FileName,fname);
      TecUtilStringDealloc(&fname);
      TecGUITextFieldSetString(FileName_TF_D1,FileName);
    }

  TecUtilLockFinish(AddOnID);
}

/**
 */
#include "guibld.c"
