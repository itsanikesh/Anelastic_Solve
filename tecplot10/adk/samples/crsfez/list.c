/*
*****************************************************************
*****************************************************************
*******                                                  ********
****** (C) Copyright 1989-1998  by AMTEC ENGINEERING INC.********
*******       All Rights Reserved.                       ********
*******                                                  ********
*****************************************************************
*****************************************************************
*/

#include <assert.h>
#include "TECADDON.h"
#include "LIST.h"


extern AddOn_pa AddOnID;
typedef struct _List_s
  {
    void      **Array;  /* array of items */
    LgIndex_t Count;    /* number of items in the array */
    LgIndex_t Capacity; /* maximum holding capacity of the array */
  } List_s;


/*
 * Adjust the capacity request as necessary to minimize
 * memory reallocations for large lists.
 */
static LgIndex_t AdjustCapacityRequest(LgIndex_t CurrentCapacity,
                                       LgIndex_t RequestedCapacity)
{
  LgIndex_t BLOCK_SIZE = 32; /* minimum size */
  LgIndex_t Result = 0;

  assert(0 <= CurrentCapacity && CurrentCapacity < RequestedCapacity);

  /* compute a reasonable block size adjust the request to fit within */
  BLOCK_SIZE = MAX(BLOCK_SIZE, CurrentCapacity / 2);
  Result = ((RequestedCapacity - 1) / BLOCK_SIZE + 1) * BLOCK_SIZE;

  assert(Result >= RequestedCapacity);
  return Result;
}


/*
 * Enlarge the list capacity to accommodate, at a minimum, the requested
 * capacity.
 */
static void EnlargeListCapacity(List_pa   List,
                                LgIndex_t RequestedCapacity)
{
  LgIndex_t Index = 0;
  LgIndex_t AdjustedCapacity = 0;
  void      **EnlargedList = NULL;

  assert(ListIsValid(List));
  assert(RequestedCapacity > List->Capacity);

  AdjustedCapacity = AdjustCapacityRequest(List->Capacity, RequestedCapacity);
  EnlargedList = (void **)malloc(AdjustedCapacity * sizeof(void *));
  if (EnlargedList == NULL && RequestedCapacity < AdjustedCapacity)
    {
      /* try again with minimum capacity request */
      AdjustedCapacity = RequestedCapacity;
      EnlargedList = (void **)malloc(AdjustedCapacity * sizeof(void *));
    }
  assert(EnlargedList != NULL);

  /* move the item references to the new list, release the   */
  /* old list, and record the new list in the list structure */
  for (Index = 0; Index < List->Count; Index++)
    EnlargedList[Index] = List->Array[Index];
  if (List->Array != NULL)
    free(List->Array);
  List->Array = EnlargedList;
  List->Capacity = AdjustedCapacity;

  assert(ListIsValid(List));
}


/*
 * Determine if the list handle is sane.
 */
Boolean_t ListIsValid(List_pa List)
{
  Boolean_t IsValid = FALSE;

  IsValid = (List != NULL &&
             (0 <= List->Count && List->Count <= List->Capacity));

  assert(IsValid == TRUE || IsValid == FALSE);
  return IsValid;
}


/*
 * Allocate a list handle.
 */
List_pa ListCreate(void)
{
  List_pa Result = NULL;

  Result = (List_pa)malloc(sizeof(List_s));
  assert(Result != NULL);

  Result->Array = NULL;
  Result->Count = 0;
  Result->Capacity = 0;

  assert(ListIsValid(Result));
  return Result;
}


/*
 * Destroy the list handle and set the handle to NULL.
 * NOTE: item destruction is the responsibility of the caller.
 */
void ListDestroy(List_pa *List)
{
  assert(List != NULL);
  assert(ListIsValid(*List));

  /* release the list */
  if ((*List)->Capacity != 0)
    free((*List)->Array);

  /* release the list structure itself */
  free(*List);
  *List = NULL;

  assert(*List == NULL);
}


/*
 * Return the number of item currently maintained by the list.
 */
LgIndex_t ListGetCount(List_pa List)
{
  LgIndex_t Result = 0;

  assert(ListIsValid(List));

  Result = List->Count;

  assert(Result >= 0);
  return Result;
}


/*
 * Set the number of items currently maintained by the list. If the count is
 * larger than the current list capacity it is expanded and the intervening
 * item references between the last item of the original state and the last
 * item of the new state are assigned NULL.
 */
void ListSetCount(List_pa   List,
                  LgIndex_t Count)
{
  assert(ListIsValid(List));
  assert(Count >= 0);

  if (Count > List->Capacity)
    ListSetItemRef(List, Count - 1, NULL);
  else
    List->Count = Count;

  assert(ListIsValid(List));
}


/*
 * Return a reference to the item at the specified offset in the list.
 */
void *ListGetItemRef(List_pa   List,
                     LgIndex_t ItemRefOffset)
{
  void *Result = NULL;

  assert(ListIsValid(List));
  assert(0 <= ItemRefOffset && ItemRefOffset <= List->Count - 1);

  Result = List->Array[ItemRefOffset];

  return Result;
}


/*
 * Place a reference to the specified item at the specified offset. If the
 * offset is beyond the end of the list it is sized accordingly and the
 * intervening item references between the last item of the original
 * state and the last item of the new state are assigned NULL. If an item
 * already exists at the specified location it is replaced therefore item
 * destruction is the responsibility of the caller.
 */
void ListSetItemRef(List_pa   List,
                    LgIndex_t ItemRefOffset,
                    void      *ItemRef)
{
  LgIndex_t Index = 0;

  assert(ListIsValid(List));
  assert(ItemRefOffset >= 0);

  /* if necessary enlarge the list to accommodate the request */
  if (ItemRefOffset + 1 > List->Capacity)
    EnlargeListCapacity(List, ItemRefOffset + 1);

  if (ItemRefOffset + 1 > List->Count)
    {
      /* fill intervening items between the original last item */
      /* and the new last item with NULLs; update the count    */
      for (Index = List->Count; Index < ItemRefOffset; Index++)
        List->Array[Index] = NULL;
      List->Count = ItemRefOffset + 1;
    }
  List->Array[ItemRefOffset] = ItemRef;

  assert(ListIsValid(List));
}


/*
 * Append a reference to the item to the list. The list will be expanded
 * to accommodate the additional item.
 */
void ListAppendItemRef(List_pa List,
                       void    *ItemRef)
{
  assert(ListIsValid(List));

  ListSetItemRef(List, List->Count, ItemRef);

  assert(ListIsValid(List));
}
