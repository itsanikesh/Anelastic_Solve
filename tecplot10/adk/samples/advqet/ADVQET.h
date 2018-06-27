#ifndef ADVQET_h
#define ADVQET_h


/* declare external linkage */
extern char *StringDup(const char *String);
extern char *StringFlushLeft(char *String);
extern char *StringFlushRight(char *String);
extern char *StringFlush(char *String);
extern void PickedObjectChangeScope(Scope_e Scope);
extern void PickedGeomChangeCoordSys(CoordSys_e CoordSys);
extern void PickedTextChangeCoordSysAndSizeUnits(CoordSys_e CoordSys,
                                                 Units_e    Units);
extern void PickedTextChangeAnchor(TextAnchor_e TextAnchor);
extern void PickedTextChangeMargin(const char *MarginString);
extern void PickedTextChangeLineSpacing(const char *LineSpacingString);
extern void PickedListChangeZoneOrMapAttachment(int Selection);
extern void UpdateMainDialog(void);
extern void UpdateAttachmentDialog(void);
extern void MainDialogSetSensitivities(void);


#endif /* ADVQET_h */
