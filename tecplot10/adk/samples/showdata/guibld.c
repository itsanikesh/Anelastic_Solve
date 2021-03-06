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

  Dialog1Manager = TecGUIDialogCreateModeless(ParentDialog,
                                             5306,
                                             1654,
                                             "Show Field Data Sample Add-on",
                                             Dialog1Init_CB,
                                             Dialog1CloseButton_CB,
                                             Dialog1HelpButton_CB);
  TecGUIFrameAdd(Dialog1Manager,
             311,
             1027,
             4756,
             565,
                    "Data Value at Index");

  Zone_OPT_D1 = TecGUIOptionMenuAdd(Dialog1Manager,
                                  402,
                                  256,
                                  2157,
                                  143,
                        Zone_OPT_D1_List,
                        Zone_OPT_D1_CB);

  Variable_OPT_D1 = TecGUIOptionMenuAdd(Dialog1Manager,
                                      2832,
                                      256,
                                      2157,
                                      143,
                        Variable_OPT_D1_List,
                        Variable_OPT_D1_CB);

  Zone_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                             363,
                             102,
                    "Zone:");

  Variable_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                                 2806,
                                 102,
                    "Variable:");

  Numberofpoin_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                                     363,
                                     524,
                    "Number of points:");

  NumPoints_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                                  2287,
                                  539,
                    "Num Points");

  LocationLabe_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                                     363,
                                     678,
                    "Location:");

  Location_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                                 2287,
                                 683,
                    "Location");

  FieldDataTyp_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                                     363,
                                     822,
                    "Field Data Type:");

  FDType_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                               2287,
                               827,
                    "Field Data Type");

  I_OPT_D1 = TecGUIOptionMenuAdd(Dialog1Manager,
                               584,
                               1361,
                               714,
                               143,
                        I_OPT_D1_List,
                        I_OPT_D1_CB);

  I_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                          545,
                          1233,
                    "I:");

  J_OPT_D1 = TecGUIOptionMenuAdd(Dialog1Manager,
                               1533,
                               1361,
                               714,
                               143,
                        J_OPT_D1_List,
                        J_OPT_D1_CB);

  J_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                          1494,
                          1233,
                    "J:");

  K_OPT_D1 = TecGUIOptionMenuAdd(Dialog1Manager,
                               2482,
                               1361,
                               714,
                               143,
                        K_OPT_D1_List,
                        K_OPT_D1_CB);

  K_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                          2443,
                          1233,
                    "K:");

  Value_LBL_D1 = TecGUILabelAdd(Dialog1Manager,
                              2819,
                              1120,
                    "Value:");

  Value_TF_D1 = TecGUITextFieldAdd(Dialog1Manager,
                                 3625,
                                 1089,
                                 1221,
                                 143,
                       Value_TF_D1_CB);

  Set_BTN_D1 = TecGUIButtonAdd(Dialog1Manager,
                             4366,
                             1397,
                             597,
                             143,
                    "Set!",
                    Set_BTN_D1_CB);

}


/**
 */
void InitTGB(void)
{
/* Currently not used */
}
