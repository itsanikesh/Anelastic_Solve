#include "TECADDON.h"


#include "ADDGLBL.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"
#include "EXBMP.h"

static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}




static void Dialog1CancelButton_CB(void)
{
  TRACE("Cancel button callback\n");
  TecGUIDialogDrop(Dialog1Manager);
/*Modal Dialogs must call TecUtilLockStart(AddOnID) prior to coming*/
/*up and then call TecUtilLockFinish(AddOnID) when the Ok or Cancel*/
/*button is pressed.  Only TecUtilLockFinish(AddOnID) is supplied here.*/
  TecUtilLockFinish(AddOnID);
}




static void Dialog1OkButton_CB(void)
{
  /* Must collect the dialog values before dismissing 
     the dialog */
  Boolean_t         OkToExport = TRUE;
  Boolean_t         ConvertTo256Colors;
  BitDumpRegion_e   ExportRegion;
  ScreenDim_t       Width;
  char             *strWidth   = NULL;
  char             *strFileName= NULL;
  Boolean_t         Cancelled = FALSE;

  ConvertTo256Colors = (Boolean_t) (TecGUIRadioBoxGetToggle(Depth_RADIO_D1) == 1);
  switch ( TecGUIOptionMenuGet(ExportRegion_OPT_D1) )
    {
      case 1:
        {
          ExportRegion = BitDumpRegion_CurrentFrame;
        } break;

      case 2:
        {
          ExportRegion = BitDumpRegion_AllFrames;
        } break;

      case 3:
        {
          ExportRegion = BitDumpRegion_WorkArea;
        } break;

      default:
        {
          TecUtilDialogErrMsg("No export region selected");
          OkToExport = FALSE;
        } break;
    } /* end switch */

  
  strWidth = TecGUITextFieldGetString(ImageWidth_TF_D1);
  Width = (ScreenDim_t)atoi(strWidth); /* Text field callback will check this value */
  strFileName = TecGUITextFieldGetString(FileName_TF_D1);

  if (strlen(strFileName) == 0)
    {
      TecUtilDialogErrMsg("Please enter a file name");
      OkToExport = FALSE;
      Cancelled = TRUE;
    }
 
  if ( OkToExport )
    {
      /* See if the file exists */
      FILE *F = fopen(strFileName,"r");
      
      if (F)
        {
          Cancelled = !TecUtilDialogMessageBox("This file exists. Overwrite?",MessageBox_Question);
          fclose(F);
        }

      if (!Cancelled)
        {
          F = fopen(strFileName,"wb");
          if (F)
            {
              if ( !ExportBMP(F,Width,ExportRegion,ConvertTo256Colors) )
                TecUtilDialogErrMsg("Error exporting BMP file");

              fclose(F);
            }
          else
            {
              TecUtilDialogErrMsg("Unable to open output file for writing. Check disk space and permissions");
            }
        }
    }

  if (strFileName)
    TecUtilStringDealloc(&strFileName);

  if (strWidth)
    TecUtilStringDealloc(&strWidth);


  if (!Cancelled)
    {
      TecGUIDialogDrop(Dialog1Manager);
      TecUtilLockFinish(AddOnID);
    }
}




static void Dialog1Init_CB(void)
{
  char str[100];
/*Modal Dialogs must call TecUtilLockStart(AddOnID) prior to coming*/
/*up and then call TecUtilLockFinish(AddOnID) when the Ok or Cancel*/
/*button is pressed.*/
  TecUtilLockStart(AddOnID);
  sprintf(str,"%d",DEFAULT_IMAGE_WIDTH);
  TecGUITextFieldSetString(ImageWidth_TF_D1,str);
  TecGUIRadioBoxSetToggle(Depth_RADIO_D1,1);
  TecGUIOptionMenuSet(ExportRegion_OPT_D1,1);
}




static int  FileName_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  
  TecUtilLockFinish(AddOnID);
  return (IsOk);
}




static void BrowseFile_BTN_D1_CB(void)
{
  char *FileName = NULL;

  TecUtilLockStart(AddOnID);
  
  if (TecUtilDialogGetFileName(SelectFileOption_WriteFile,
                               &FileName,
                               "BMP",
                               NULL,
                               "*.bmp"))
    {
      TecGUITextFieldSetString(FileName_TF_D1,FileName);
      TecUtilStringDealloc(&FileName);
    }

  TecUtilLockFinish(AddOnID);
}




static void Depth_RADIO_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  TRACE1("RadioBox (Depth_RADIO_D1) Value Changed,  New value is: %d\n",*I);
  TecUtilLockFinish(AddOnID);
}




static int  ImageWidth_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Image Width text field callback called. New value is: [%s]\n",S);

  if (atoi(S) <= 0)
    {
      TecUtilDialogErrMsg("Please enter an integer width greater than 0");
      IsOk = 0;
    }

  TecUtilLockFinish(AddOnID);
  return (IsOk);
}



char *ExportRegion_OPT_D1_List = "Current Frame, All Frames, Work Area";




static void ExportRegion_OPT_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  TRACE1("Option Menu (ExportRegion_OPT_D1) value changed,  New value is: %d\n",*I);
  TecUtilLockFinish(AddOnID);
}





#include "guibld.c"
