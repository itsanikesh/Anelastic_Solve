#if !defined LIST_h
#define LIST_h

/* List_pa is a public handle to lists */
typedef struct _List_s     *List_pa;


extern Boolean_t ListIsValid(List_pa List);
extern List_pa ListCreate(void);
extern void ListDestroy(List_pa *List);
extern LgIndex_t ListGetCount(List_pa List);
extern void ListSetCount(List_pa   List,
                         LgIndex_t Count);
extern void *ListGetItemRef(List_pa   List,
                            LgIndex_t ItemRefOffset);
extern void ListSetItemRef(List_pa   List,
                           LgIndex_t ItemRefOffset,
                           void      *ItemRef);
extern void ListAppendItemRef(List_pa List,
                              void    *ItemRef);

#endif /* LIST_h */
