#if !defined _H_RAWPTR
#define _H_RAWPTR

# if defined (__cplusplus)
extern "C" {
# endif

/* release builds use macros in place of the functions */
#if defined(NDEBUG) && !defined(USE_MACROS_FOR_FUNCTIONS)
# define USE_MACROS_FOR_FUNCTIONS
#endif

/**
 */
typedef struct _RawValuePtr_s
{
  FieldData_pa              FieldData;
  void                     *RawDataPtr; /* may not always be available */
  FieldValueGetFunction_pf  GetFieldValue;
  FieldValueSetFunction_pf  SetFieldValue;
  FieldDataType_e           FieldDataType;
  ValueLocation_e           ValueLocation;
  LgIndex_t                 ValueCount;
} RawValuePtr_t;

/**
 */
typedef struct _RawNodePtr_s
{
  NodeMap_t    *NodeMapPtr;
  LgIndex_t     CellCount;
  LgIndex_t     NodesPerCell;
} RawNodePtr_t;

/**
 */
#define INVALID_RAWVALUEPTR InvalidRawValuePtr()

/**
 */
#define INVALID_RAWNODEPTR InvalidRawNodePtr()

/**
 */
#define RawValuePtrIsValid_MACRO(RawValuePtr) \
  (((VALID_REF((RawValuePtr).FieldData)  && \
     ((RawValuePtr).RawDataPtr == NULL || VALID_REF((RawValuePtr).RawDataPtr))) || \
    (VALID_REF((RawValuePtr).RawDataPtr) && (RawValuePtr).FieldData  == NULL)) && \
   VALID_ENUM((RawValuePtr).FieldDataType, FieldDataType_e) && \
   (RawValuePtr).ValueCount > 0)

/**
 */
#define RawNodePtrIsValid_MACRO(RawNodePtr) \
  (VALID_REF((RawNodePtr).NodeMapPtr) && \
   (RawNodePtr).CellCount > 0 && \
   2 <= (RawNodePtr).NodesPerCell && \
   (RawNodePtr).NodesPerCell <= 8)

/**
 */
#define RawValuePtrGetValue_MACRO(RawValuePtr, Index) \
  ((RawValuePtr).RawDataPtr != NULL \
     ? (double)(((RawValuePtr).FieldDataType == FieldDataType_Float ? ((float *)(RawValuePtr).RawDataPtr)[(Index) - 1] : \
                ((RawValuePtr).FieldDataType == FieldDataType_Double ? ((double *)(RawValuePtr).RawDataPtr)[(Index) - 1] : \
                ((RawValuePtr).FieldDataType == FieldDataType_LongInt ? ((LgIndex_t *)(RawValuePtr).RawDataPtr)[(Index) - 1] : \
                ((RawValuePtr).FieldDataType == FieldDataType_ShortInt ? ((short *)(RawValuePtr).RawDataPtr)[(Index) - 1] : \
                ((RawValuePtr).FieldDataType == FieldDataType_Byte ? ((char *)(RawValuePtr).RawDataPtr)[(Index) - 1] : \
                ((((Byte_t *)(RawValuePtr).RawDataPtr)[((Index) - 1) / 8] >> (((Index) - 1) % 8)) & (Byte_t)0x1))))))) \
     : RawValuePtr.GetFieldValue((RawValuePtr).FieldData, (Index) - 1))

/**
 */
#define RawValuePtrGetValueLoc_MACRO(RawValuePtr) ((RawValuePtr).ValueLocation)

/**
 */
#define RawValuePtrGetValueType_MACRO(RawValuePtr) ((RawValuePtr).FieldDataType)

/**
 */
#define RawValuePtrSetValue_MACRO(RawValuePtr, Index, Value) \
  do \
  { \
    if ((RawValuePtr).RawDataPtr != NULL) \
      { \
        switch((RawValuePtr).FieldDataType) \
          { \
            case FieldDataType_Float: \
              ((float *)((RawValuePtr).RawDataPtr))[(Index) - 1] = (float)(Value); \
              break; \
            case FieldDataType_Double: \
              ((double *)((RawValuePtr).RawDataPtr))[(Index) - 1] = (Value); \
              break; \
            case FieldDataType_LongInt: \
              ((LgIndex_t *)((RawValuePtr).RawDataPtr))[(Index) - 1] = (LgIndex_t)(Value); \
              break; \
            case FieldDataType_ShortInt: \
              ((short *)((RawValuePtr).RawDataPtr))[(Index) - 1] = (short)(Value); \
              break; \
            case FieldDataType_Byte: \
              ((char *)((RawValuePtr).RawDataPtr))[(Index) - 1] = (char)(Value); \
              break; \
            default: \
              CHECK(FALSE); \
          } \
      } \
    else \
      { \
        (RawValuePtr).SetFieldValue((RawValuePtr).FieldData, (Index) - 1, (double)(Value)); \
      } \
  } while(0)

/**
 */
#define RawNodePtrGetNode_MACRO(RawNodePtr, Cell, Corner) \
  ((RawNodePtr).NodeMapPtr[((Cell) - 1) * (RawNodePtr).NodesPerCell + (Corner) - 1] + 1)

/**
 */
#define RawNodePtrGetNodesPerCell_MACRO(RawNodePtr) ((RawNodePtr).NodesPerCell)

/**
 */
#define RawNodePtrSetNode_MACRO(RawNodePtr, Cell, Corner, Node) \
  do \
  { \
    (RawNodePtr).NodeMapPtr[((Cell) - 1) * (RawNodePtr).NodesPerCell + (Corner) - 1] = (Node) - 1; \
  } while(0)



/**
 * Macros are used for release mode while functions are used for debug mode
 * unless otherwise directed. See the _FUNC declarations for documentation.
 */
#if defined USE_MACROS_FOR_FUNCTIONS

#  define RawValuePtrIsValid(RawValuePtr)                   RawValuePtrIsValid_MACRO(RawValuePtr)
#  define RawValuePtrGetValue(RawValuePtr, Index)           RawValuePtrGetValue_MACRO(RawValuePtr, Index)
#  define RawValuePtrGetValueLoc(RawValuePtr)               RawValuePtrGetValueLoc_MACRO(RawValuePtr)
#  define RawValuePtrGetValueType(RawValuePtr)              RawValuePtrGetValueType_MACRO(RawValuePtr)
#  define RawValuePtrSetValue(RawValuePtr, Index, Value)    RawValuePtrSetValue_MACRO(RawValuePtr, Index, Value)
#  define RawNodePtrIsValid(RawNodePtr)                     RawNodePtrIsValid_MACRO(RawNodePtr)
#  define RawNodePtrGetNode(RawNodePtr, Cell, Corner)       RawNodePtrGetNode_MACRO(RawNodePtr, Cell, Corner)
#  define RawNodePtrGetNodesPerCell(RawNodePtr)             RawNodePtrGetNodesPerCell_MACRO(RawNodePtr)
#  define RawNodePtrSetNode(RawNodePtr, Cell, Corner, Node) RawNodePtrSetNode_MACRO(RawNodePtr, Cell, Corner, Node)

#else /* USE_MACROS_FOR_FUNCTIONS */

#  define RawValuePtrIsValid(RawValuePtr)                   RawValuePtrIsValid_FUNC(RawValuePtr)
#  define RawValuePtrGetValue(RawValuePtr, Index)           RawValuePtrGetValue_FUNC(RawValuePtr, Index)
#  define RawValuePtrGetValueLoc(RawValuePtr)               RawValuePtrGetValueLoc_FUNC(RawValuePtr)
#  define RawValuePtrGetValueType(RawValuePtr)              RawValuePtrGetValueType_FUNC(RawValuePtr)
#  define RawValuePtrSetValue(RawValuePtr, Index, Value)    RawValuePtrSetValue_FUNC(RawValuePtr, Index, Value)
#  define RawNodePtrIsValid(RawNodePtr)                     RawNodePtrIsValid_FUNC(RawNodePtr)
#  define RawNodePtrGetNode(RawNodePtr, Cell, Corner)       RawNodePtrGetNode_FUNC(RawNodePtr, Cell, Corner)
#  define RawNodePtrGetNodesPerCell(RawNodePtr)             RawNodePtrGetNodesPerCell_FUNC(RawNodePtr)
#  define RawNodePtrSetNode(RawNodePtr, Cell, Corner, Node) RawNodePtrSetNode_FUNC(RawNodePtr, Cell, Corner, Node)

/**
 * Indicates if the raw value pointer item is valid.
 *
 * @param RawValuePtr
 *     Raw value pointer item in question.
 *
 * @return
 *     TRUE if valid, FALSE otherwise.
 */
extern Boolean_t RawValuePtrIsValid_FUNC(RawValuePtr_t RawValuePtr);

/**
 * Gets the specified value from the raw value pointer item at the specified
 * index.
 *
 * @param RawValuePtr
 *     Raw value pointer item from which the value is fectched.
 * @param Index
 *     Index at which the point is desired.
 *
 * @return
 *     Value at the specified index.
 */
extern double RawValuePtrGetValue_FUNC(RawValuePtr_t  RawValuePtr,
                                       LgIndex_t      Index);

/**
 * Gets the value location from the raw value pointer item.
 *
 * @param RawValuePtr
 *     Raw value pointer item from which the value location is needed.
 *
 * @return
 *     Value location of the raw value pointer item.
 */
extern ValueLocation_e RawValuePtrGetValueLoc_FUNC(RawValuePtr_t RawValuePtr);

/**
 * Gets the field data type from the raw value pointer item.
 *
 * @param RawValuePtr
 *     Raw value pointer item from which the field data type is needed.
 *
 * @return
 *     field data type of the raw value pointer item.
 */
extern FieldDataType_e RawValuePtrGetValueType_FUNC(RawValuePtr_t RawValuePtr);

/**
 * Assigns the value to the raw value pointer item at the specified index.
 *
 * @param RawValuePtr
 *     Raw value pointer item to receive the new value.
 * @param Index
 *     Index to which the value is assigned.
 * @param Value
 *     New value to assign.
 */
extern void RawValuePtrSetValue_FUNC(RawValuePtr_t  RawValuePtr,
                                     LgIndex_t      Index,
                                     double         Value);

/**
 * Indicates if the raw node pointer item is valid.
 *
 * @param RawNodePtr
 *     Raw node pointer item in question.
 *
 * @return
 *     TRUE if valid, FALSE otherwise.
 */
extern Boolean_t RawNodePtrIsValid_FUNC(RawNodePtr_t RawNodePtr);

/**
 * Gets the specified node from the raw node pointer item at the specified
 * index.
 *
 * @param RawNodePtr
 *     Raw node pointer item from which the node is fectched.
 * @param Index
 *     Index at which the point is desired.
 *
 * @return
 *     Node at the specified index.
 */
extern NodeMap_t RawNodePtrGetNode_FUNC(RawNodePtr_t  RawNodePtr,
                                        LgIndex_t     Cell,
                                        LgIndex_t     Corner);

/**
 * Gets the number of nodes needed for each cell of the raw node map pointer
 * item.
 *
 * @param RawNodePtr
 *     Raw node pointer item from which the information is fetched.
 *
 * @return
 *     Number of nodes needed for each cell.
 */
extern LgIndex_t RawNodePtrGetNodesPerCell_FUNC(RawNodePtr_t RawNodePtr);

/**
 * Assigns the node to the raw node pointer item at the specified index.
 *
 * @param RawNodePtr
 *     Raw node pointer item to receive the new node.
 * @param Cell
 *     Cell index to which the node is assigned.
 * @param Corner
 *     Corner number of the cell index to which the node is assigned.
 * @param Node
 *     New node to assign.
 */
extern void RawNodePtrSetNode_FUNC(RawNodePtr_t  RawNodePtr,
                                   LgIndex_t     Cell,
                                   LgIndex_t     Corner,
                                   NodeMap_t     Node);
#endif /* USE_MACROS_FOR_FUNCTIONS */

/**
 * Returns an invalid raw value pointer item.
 */
extern RawValuePtr_t InvalidRawValuePtr(void);

/**
 * Returns an invalid raw node pointer item.
 */
extern RawNodePtr_t InvalidRawNodePtr(void);

/**
 * Convenience function for fetching the raw field data pointer for the
 * specified zone and variable of the current frame and wrapping it in a raw
 * value pointer item.
 *
 * @param Zone
 *     Zone for which the raw variable data is desired.
 * @param Var
 *     Variable for which the raw variable data is desired.
 *
 * @return
 *     Raw value pointer item wrapping the raw field data pointer for the
 *     specified zone and variable.
 */
extern RawValuePtr_t RawValuePtrGetPtr(EntIndex_t Zone,
                                       EntIndex_t Var);

/**
 * Allocates the data value array of a raw value pointer item with the number
 * of values of the specified type.
 *
 * @param FieldDataType
 *     Data type of the data to allocate.
 * @param ValueLocation
 *     Value location of the data to allocate.
 * @param ValueCount
 *     Number of data values to allocate.
 *
 * @return
 *     Raw value pointer item wrapping the allocated data value array.
 */
extern RawValuePtr_t RawValuePtrAllocatePtr(FieldDataType_e FieldDataType,
                                            ValueLocation_e ValueLocation,
                                            LgIndex_t       ValueCount);

/**
 * Deallocates the data value array of the raw value pointer item and assigns
 * it to an invalid item.
 *
 * @param RawValuePtr
 *     Pointer to a raw value pointer item. Upon return RawValuePtr will be
 *     assigned an invalid raw value pointer item.
 */
extern void RawValuePtrDealloc(RawValuePtr_t *RawValuePtr);

/**
 * Convenience function for fetching the raw node map pointer for the specified
 * zone of the current frame and wrapping it in a raw node pointer item.
 *
 * @param Zone
 *     Zone for which the raw node map is desired.
 *
 * @return
 *     Raw node pointer item wrapping the raw field data pointer for the
 *     specified zone and variable.
 */
extern RawNodePtr_t RawNodePtrGetPtr(EntIndex_t  Zone);

/**
 * Calculates the minimum and maximum values for the raw value pointer item.
 *
 * @param RawNodePtr
 *     Raw node pointer item to analyze.
 * @param MinVal
 *     Pointer to the resulting minimum data value.
 * @param MinVal
 *     Pointer to the resulting maximum data value.
 */
extern void RawValuePtrGetMinMax(RawValuePtr_t  RawValuePtr,
                                 double        *MinVal,
                                 double        *MaxVal);

# if defined (__cplusplus)
}
# endif

#endif /* _H_RAWPTR */
