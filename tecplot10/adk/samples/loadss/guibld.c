/***************************************************
 *                                                 *
 *  NOTE!  This file is automatically generated by *
 *         the Tecplot GUI Builder.  It is highly  *
 *         recommended that you never edit this    *
 *         file directly!                          *
 *                                                 *
 ***************************************************/



/**
 */
void BuildDialog1(LgIndex_t  ParentDialog)
{
  if (Dialog1Manager != BADDIALOGID)
    return;

  Dialog1Manager = TecGUIDialogCreateModal(ParentDialog,
                                          5743,
                                          1156,
                                          "Simple Spreadsheet File Loader",
                                          Dialog1Init_CB,
                                          Dialog1OkButton_CB,
                                          NULL,
                                          Dialog1CancelButton_CB,
                                          Dialog1HelpButton_CB);
  ISkip_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                              546,
                              947,
                    "I-Skip");

  Filename_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                                 208,
                                 125,
                    "Filename");

  Delimeter_RADIO_D1 = TecGUIRadioBoxAdd(Dialog1Manager,
                                       1275,
                                       364,
                                       1249,
                                       435,
                                 "Comma",
                                 "Space",
                                 "Tab",
                                 (char *)NULL,
                                 (char *)NULL,
                                 Delimeter_RADIO_D1_CB);

  ISkip_TF_D1 = TecGUITextFieldAdd(Dialog1Manager,
                                 1249,
                                 909,
                                 1222,
                                 141,
                       ISkip_TF_D1_CB);

  FileName_TF_D1 = TecGUITextFieldAdd(Dialog1Manager,
                                    1249,
                                    87,
                                    3851,
                                    141,
                       FileName_TF_D1_CB);

  SelectFile_BTN_D1 = TecGUIButtonAdd(Dialog1Manager,
                                    5204,
                                    87,
                                    377,
                                    141,
                    "...",
                    SelectFile_BTN_D1_CB);

  Delimeter_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                                  208,
                                  386,
                    "Delimeter");

}


/**
 */
void InitTGB(void)
{
/* Currently not used */
}
