/*
 *
 * NOTE: This file automatically generated by the Tecplot GUI Builder.
 *       Do not edit this file!
 *
 */
extern void InitTGB(void);
$$IF(AW_ISMODAL)
extern LgIndex_t  Dialog1Manager;
extern void BuildDialog1(int  ParentDialog);
$$ENDIF
$$IF(AW_ISMODELESS)
extern LgIndex_t  Dialog1Manager;
extern void BuildDialog1(int  ParentDialog);
$$ENDIF
$$IF(AW_HASSIDEBAR)
extern LgIndex_t  Sidebar1Manager;
extern void BuildSidebar1(void);
$$ENDIF