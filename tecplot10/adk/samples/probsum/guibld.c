/***************************************************
 *                                                 *
 *  NOTE!  This file is automatically built by     *
 *         the Tecplot GUI Builder.  It is highly  *
 *         recommended that you never edit this    *
 *         file directly!                          *
 *                                                 *
 ***************************************************/


void BuildDialog1(int  ParentDialog)
{
  if (Dialog1Manager != BADDIALOGID)
    return;
  Dialog1Manager = TecGUIDialogCreateModeless(ParentDialog,
                                             4383,
                                             689,
                                             "PROBSUM",
                                             Dialog1Init_CB,
                                             Dialog1CloseButton_CB,
                                             Dialog1HelpButton_CB);
  StartProbe_BTN_D1 = TecGUIButtonAdd(Dialog1Manager,
                                    1450,
                                    69,
                                    1375,
                                    150,
                    "Start Probe",
                    StartProbe_BTN_D1_CB);
  SumTotal_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                                 1121,
                                 339,
                    "                                         ");
  TecGUIFrameAdd(Dialog1Manager,
             44,
             25,
             4306,
             659,
                    (char *)NULL);
}

void InitTGB(void)
{
/* Currently not used */
}
