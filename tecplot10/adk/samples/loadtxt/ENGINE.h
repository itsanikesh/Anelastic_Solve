#ifndef ENGINE_H_
#define ENGINE_H_

/* DOCSTART:gr/loadtxt_generated_prototypes.txt */
extern Boolean_t STDCALL LoaderCallback(StringList_pa params);
extern void STDCALL LoaderSelectedCallback(void);
/* DOCEND */
extern Boolean_t DoLoadDelimitedText(const char *FileName,
                                     int         Skip);

#endif /* ENGINE_H_ */
