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

  Dialog1Manager = TecGUIDialogCreateModal(ParentDialog,
                                          7120,
                                          1947,
                                          "Export BMP",
                                          Dialog1Init_CB,
                                          Dialog1OkButton_CB,
                                          NULL,
                                          Dialog1CancelButton_CB,
                                          Dialog1HelpButton_CB);
  File_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                             494,
                             1639,
                    "File:");
  FileName_TF_D1 = TecGUITextFieldAdd(Dialog1Manager,
                                    1079,
                                    1579,
                                    5022,
                                    152,
                       FileName_TF_D1_CB);
  BrowseFile_BTN_D1 = TecGUIButtonAdd(Dialog1Manager,
                                    6349,
                                    1579,
                                    403,
                                    152,
                    "...",
                    BrowseFile_BTN_D1_CB);
  Depth_RADIO_D1 = TecGUIRadioBoxAdd(Dialog1Manager,
                                   507,
                                   833,
                                   1795,
                                   424,
                                 "8-bit Color",
                                 "24-bit Color",
                                 (char *)NULL,
                                 (char *)NULL,
                                 (char *)NULL,
                                 Depth_RADIO_D1_CB);
  ImageWidthin_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                                     312,
                                     212,
                    "Image Width in Pixels:");
  ImageWidth_TF_D1 = TecGUITextFieldAdd(Dialog1Manager,
                                      2732,
                                      174,
                                      1235,
                                      152,
                       ImageWidth_TF_D1_CB);
  ExportRegion_OPT_D1 = TecGUIOptionMenuAdd(Dialog1Manager,
                                          2693,
                                          468,
                                          3421,
                                          168,
                        ExportRegion_OPT_D1_List,
                        ExportRegion_OPT_D1_CB);
  ExportRegion_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                                     806,
                                     533,
                    "Export Region:");
}

void InitTGB(void)
{
/* Currently not used */
}