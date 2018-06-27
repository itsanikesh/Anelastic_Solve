#ifndef ADDGLBL_H_
#define ADDGLBL_H_

#define ADDON_NAME "Delimited Text Loader"
#define ADDON_VERSION "1.1"
#define ADDON_DATE __DATE__


Boolean_t STDCALL LoadDelimitedText(StringList_pa param);
Boolean_t DoLoadDelimitedText(const char *FileName,
                              int         Skip);

/* DOCSTART:gr/loadtxt_LoadTxtGlobalState_decl.txt */
#define MAX_FILENAME 5000
typedef struct
{
  char FileName[MAX_FILENAME];
  int  Skip;
} LoadTxtGlobalState_s;
/* DOCEND */

#endif /* ADDGLBL_H_ */
