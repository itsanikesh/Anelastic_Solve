#include "TECADDON.h"


#include "ADDGLBL.h"
#include "GUIDEFS.h"

/* DOCSTART:gr/animiplanes_DEFAULT_SKIP.txt */
#define DEFAULT_SKIP "1"
/* DOCEND */

/**
 */
extern void FillZoneList(void);
extern void AnimatePlanes(Set_pa ZoneSet,
                          int    Skip);

/**
 */
static void Dialog1HelpButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecUtilDialogMessageBox("On-line Help not available for this dialog.",
                          MessageBox_Information);
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void Dialog1CloseButton_CB(void)
{
  TecUtilLockStart(AddOnID);
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

/**
 */
/* DOCSTART:gr/animiplanes_Dialog1Init_CB_Pre.txt */
static void Dialog1Init_CB(void)
{
  TecUtilLockStart(AddOnID);
  /*<<< Add init code (if necessary) here>>>*/
  TecGUITextFieldSetString(Skip_TF_D1, DEFAULT_SKIP);
/* DOCEND */
/* DOCSTART:gr/animiplanes_Dialog1Init_CB_FillZoneList.txt */
  FillZoneList();
/* DOCEND */
/* DOCSTART:gr/animiplanes_Dialog1Init_CB_Post.txt */
  TecUtilLockFinish(AddOnID);
}
/* DOCEND */

/**
 */
static void ZoneList_MLST_D1_CB(const int *I)
{
  TecUtilLockStart(AddOnID);
  TRACE1("Multi selection list (ZoneList_MLST_D1) item selected,  First Item is: %d\n",*I);
  TecUtilLockFinish(AddOnID);
}

/**
 */
static int  Skip_TF_D1_CB(const char *S)
{
  int IsOk = 1;
  TecUtilLockStart(AddOnID);
  TRACE1("Text field (Skip_TF_D1) Value Changed,  New value is: %s\n",S);
  TecUtilLockFinish(AddOnID);
  return IsOk;
}

/**
 */
/* DOCSTART:gr/animiplanes_AnimPlanes_BTN_D1_CB.txt */
static void AnimPlanes_BTN_D1_CB(void)
{
  TecUtilLockStart(AddOnID);

  /* Make sure there is a dataset */
  if (TecUtilDataSetIsAvailable())
    {
      LgIndex_t  Count     = 0;
      LgIndex_t *Selection = NULL;
      Set_pa     ZoneSet   = TecUtilSetAlloc(TRUE);

      /* Get the Skip value from the text field */
      char *strSkip = TecGUITextFieldGetString(Skip_TF_D1);

      /* Get the selected zones from the ZoneList */
      TecGUIListGetSelectedItems(ZoneList_MLST_D1, &Selection, &Count);
      if (Count > 0)
        {
          LgIndex_t i;

          /* Put the selected items into ZoneSet */
          for (i = 0; i < Count; i++)
            TecUtilSetAddMember(ZoneSet, Selection[i], TRUE);

          TecUtilArrayDealloc((void **)&Selection);
        }


      /* Make sure a zone has been picked */
      if (ZoneSet != NULL) /* ...do the animation */
        AnimatePlanes(ZoneSet, atoi(strSkip));
      else
        TecUtilDialogErrMsg("No zones have been picked.");

      /* Deallocate the ZoneSet and strSkip string when we are done with them */
      if (ZoneSet != NULL)
        TecUtilSetDealloc(&ZoneSet);
      if (strSkip != NULL)
        TecUtilStringDealloc(&strSkip);
    }
  else
    TecUtilDialogErrMsg("No data set available.");

  TecUtilLockFinish(AddOnID);
}
/* DOCEND */


#include "guibld.c"
