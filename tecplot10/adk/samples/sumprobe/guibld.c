/***************************************************
 *                                                 *
 *  NOTE!  This file is automatically generated by *
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
                                             3544,
                                             636,
                                             "Add-on Dialog",
                                             Dialog1Init_CB,
                                             Dialog1CloseButton_CB,
                                             Dialog1HelpButton_CB);
  Thetotalis00_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                                     489,
                                     297,
                    "The total is:  0.0");
}

void InitTGB(void)
{
/* Currently not used */
}
