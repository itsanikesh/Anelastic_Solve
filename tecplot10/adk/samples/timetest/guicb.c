#include "TECADDON.h"
#include "ADDGLBL.h"
#if !defined (MSWIN)
#include <unistd.h>
#endif
#include "GUIDEFS.h"
#include "TIMETEST.h"




/**
 */
extern TimerTestState_s TimerTestState;

/**
 */
static void SetSensitivities(void)
{
  LgIndex_t SelectedItem;

  SelectedItem = TecGUIListGetSelectedItem(Timers_SLST_D1);
  if (SelectedItem != -1)
    {
      Boolean_t Sensitive = TimerTestState.TimerArray[SelectedItem-1].Continue;
      TecGUISetSensitivity(CallCountLabel_LBL_D1, Sensitive);
      TecGUISetSensitivity(CallCountField_LBL_D1, Sensitive);
      TecGUISetSensitivity(LockCountLabel_LBL_D1, Sensitive);
      TecGUISetSensitivity(LockCountField_LBL_D1, Sensitive);
      TecGUISetSensitivity(LockOwnerLabel_LBL_D1, Sensitive);
      TecGUISetSensitivity(LockOwnerField_LBL_D1, Sensitive);
    }
}

/**
 */
static Boolean_t STDCALL TimerCallback(ArbParam_t Data)
{
  Boolean_t    Continue;
  char         buffer[100];
  int          TimerNum = (int)Data;
  TimerInfo_s *TimerState = &TimerTestState.TimerArray[TimerNum-1];

  Continue = TecGUIDialogIsUp(Dialog1Manager);
  if (Continue)
    {
      /*
       * IMPORTANT:
       *   All of the TecGUI and TecUtil functions called in this callback are
       *   passive as they do not change the state of Tecplot. If this callback
       *   wanted to change Tecplot's state it would have to first check to see if
       *   Tecplot is locked, then if not lock Tecplot before changing it's state.
       */
      TimerState->NumberOfTimesSent++;  

      if (TimerNum == TimerTestState.SelectedTimer)
        {
          Continue = TimerState->Continue;
          if (!Continue)
            {
              sprintf(buffer,"%d Not active",TimerNum);
              TecGUIListReplaceItem(Timers_SLST_D1,buffer,TimerNum);
              TecGUIListSetSelectedItem(Timers_SLST_D1,TimerNum);
            }

          sprintf(buffer,"%d",TimerState->NumberOfTimesSent);
          TecGUILabelSetText(CallCountField_LBL_D1,buffer);

          if (TecUtilLockIsOn())
            {
              int   LockCount = TecUtilLockGetCount();
              char *LockOwner = TecUtilLockGetCurrentOwnerName();

              /* show the number of locks */
              sprintf(buffer,"%d", LockCount);
              TecGUILabelSetText(LockCountField_LBL_D1,buffer);

              /* show the lock owner */
              if (LockOwner != NULL)
                {
                  TecGUILabelSetText(LockOwnerField_LBL_D1,LockOwner);
                  TecUtilStringDealloc(&LockOwner);
                }
              else
                TecGUILabelSetText(LockOwnerField_LBL_D1,"N/A");
            }
          else
            {
              TecGUILabelSetText(LockCountField_LBL_D1,"0");
              TecGUILabelSetText(LockOwnerField_LBL_D1,"N/A");
            }

          SetSensitivities();
        }
    }

  return Continue;
}

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
  TecGUIListDeleteAllItems(Timers_SLST_D1);
  TecGUIDialogDrop(Dialog1Manager);
  TecUtilLockFinish(AddOnID);
}

/**
 */
static void Dialog1Init_CB(void)
{
  int i;
  char buffer[100];
  TecUtilLockStart(AddOnID);  

  for (i=0;i<NUM_TIMERS;i++)
    {
      TimerTestState.TimerArray[i].Interval           = TIMER_INTERVAL;
      TimerTestState.TimerArray[i].NumberOfTimesSent  = 0;
      TimerTestState.TimerArray[i].Continue           = FALSE;
      sprintf(buffer,"%d Not active",i+1);
      TecGUIListAppendItem(Timers_SLST_D1,buffer);    
    }

  TimerTestState.SelectedTimer = 1;
  TecGUIListSetSelectedItem(Timers_SLST_D1,1);
  TecGUIToggleSet(Active_TOG_D1,FALSE);
  TecGUILabelSetText(CallCountField_LBL_D1,"0");
  TecGUILabelSetText(LockCountField_LBL_D1,"0");
  TecGUILabelSetText(LockOwnerField_LBL_D1,"N/A");
  SetSensitivities();

  TecUtilLockFinish(AddOnID);
}

/**
 */
static void Timers_SLST_D1_CB(const LgIndex_t *I)
{
  char buffer[100];
  TecUtilLockStart(AddOnID);

  TimerTestState.SelectedTimer = *I;
  TecGUIToggleSet(Active_TOG_D1,TimerTestState.TimerArray[*I-1].Continue);
  sprintf(buffer,"%d",TimerTestState.TimerArray[*I-1].NumberOfTimesSent);
  TecGUILabelSetText(CallCountField_LBL_D1,buffer);
  SetSensitivities();

  TecUtilLockFinish(AddOnID);
}

/**
 */
static void Active_TOG_D1_CB(const LgIndex_t *I)
{
  char buffer[100];
  TecUtilLockStart(AddOnID);

  TimerTestState.TimerArray[TimerTestState.SelectedTimer-1].Continue = *I ? (TRUE) : (FALSE);

  if (*I)
    {
      sprintf(buffer,"%d Active",TimerTestState.SelectedTimer);

      TecUtilTimerAddCallback(TIMER_INTERVAL,
                              (ArbParam_t)TimerTestState.SelectedTimer,
                              TimerCallback);
    }
  else
    sprintf(buffer,"Waiting for callback...");

  TecGUIListReplaceItem(Timers_SLST_D1,buffer,TimerTestState.SelectedTimer);
  TecGUIListSetSelectedItem(Timers_SLST_D1,TimerTestState.SelectedTimer);

  SetSensitivities();

  TecUtilLockFinish(AddOnID);
}

#include "guibld.c"
