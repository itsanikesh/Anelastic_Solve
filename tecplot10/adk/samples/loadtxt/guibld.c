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
  Dialog1Manager = TecGUIDialogCreateModal(ParentDialog,
                                          4690,
                                          607,
                                          "Delimited Text Loader ",
                                          Dialog1Init_CB,
                                          Dialog1OkButton_CB,
                                          NULL,
                                          Dialog1CancelButton_CB,
                                          Dialog1HelpButton_CB);
  Skip_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                             165,
                             139,
                    "Skip:");
  Skip_TF_D1 = TecGUITextFieldAdd(Dialog1Manager,
                                838,
                                97,
                                1240,
                                153,
                       Skip_TF_D1_CB);
  File_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                             165,
                             423,
                    "File:");
  FileName_TF_D1 = TecGUITextFieldAdd(Dialog1Manager,
                                    838,
                                    353,
                                    3131,
                                    153,
                       FileName_TF_D1_CB);
  Browse_BTN_D1 = TecGUIButtonAdd(Dialog1Manager,
                                4076,
                                353,
                                401,
                                153,
                    "...",
                    Browse_BTN_D1_CB);
}

void InitTGB(void)
{
/* Currently not used */
}
