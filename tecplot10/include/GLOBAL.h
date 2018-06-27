/* CORE SOURCE CODE REMOVED */

/*! \mainpage Tecplot ADK Reference 
 */

/*
******************************************************************
******************************************************************
*******                                                   ********
******  (C) 1988-2004 Tecplot, Inc.                        *******
*******                                                   ********
******************************************************************
******************************************************************
*/

#ifndef _GLOBAL_H
#define _GLOBAL_H

#if defined EXTERN
#undef EXTERN
#endif
#if defined Q_MAINMODULE && defined TECPLOTKERNEL
#define EXTERN
#else
#define EXTERN extern
#endif



#define EXPORT_DEPRECATED_INTERFACES_TO_ADK_ONLY
/* CORE SOURCE CODE REMOVED */


/****************************************************************
 *                                                              *
 *                          MACROS                              *
 *                                                              *
 ****************************************************************/
#if defined TRUE
#undef TRUE
#endif
#if defined FALSE
#undef FALSE
#endif
#if defined MIN
#undef MIN
#endif
#if defined MAX
#undef MAX
#endif
#if defined ROUND
#undef ROUND
#endif
#if defined ROUND2
#undef ROUND2
#endif
#if defined TRUNC
#undef TRUNC
#endif

#define TRUE                  ((Boolean_t)1)
#define FALSE                 ((Boolean_t)0)

/****************************************************************
 *                                                              *
 *                           MACROS                             *
 *                                                              *
 ****************************************************************/
#define ABS(X)                ((X) >= 0 ? (X) : -(X) )
#define MAX(X,Y)              ((X) > (Y) ? (X) : (Y) )
#define MIN(X,Y)              ((X) < (Y) ? (X) : (Y) )
#define BESTSHOWCOLOR(X)      ((X) == White_C ? Black_C : White_C)
#define ROUND_TO_BYTE(X)      ((BYTE)((X)+0.499))
#define ROUNDS(X)             ((short)((X)+0.499))
#define ROUNDL(X)             ((LgIndex_t)((X)+0.499))
#define ROUND2(X)             ((X) >= 0 ? ((int)((X)+0.499)) : ((int)((X)-0.499)))
#define TRUNC(X)              ((short) (X))
#define RAD_TO_DEG(rad)       (180.*(rad)/PI)
#define DEG_TO_RAD(deg)       (PI*(deg)/180.)
#define CAPITAL(C)            ( ('a'<=(C)&&(C)<='z') ? ((C)+('A'-'a')) : (C) )
#define ISEMPTYSTRING(S)      ( (S)[0] == '\0' )
#define ISWHITESPACE(C)       ((C == ' ') || (C == '\t') || (C == '\n'))
#define ISSEPARATOR(C)        ((C == ' ') || (C == '\t') || (C == ','))
/* clamp the input to the specified range */
#define CLAMP(value,low,high) ((value)<(low) ? (low) : (value) > (high) ? (high) : (value))
/* integer division rounds any fraction up (for example n=16,d=3 results in 6) */
#define INTEGER_DIVIDE_AND_ROUND_UP(n, d) (((int)(n)+(int)(d)-1)/(int)(d))

/**
 * Calcualtes the cell's primary corner or cell centered index from the I, J,
 * and K indices.
 *
 * Consider this IJ zone dimensioned 4 by 3:
 * @verbatim
    +-------+-------+-------+-------+
    |       |       |       |       |
    |  <8>  |  <9>  |  <10> |  <11> |  <--- ghost cells
    |       |       |       |       |
    |8      |9      |10     |11     |
    +-------+-------+-------+-------+
    |       |       |       |       |
    |  <4>  |  <5>  |  <6>  |  <7>  |
    |       |       |       |       |
    |4      |5      |6      |7      |
    +-------+-------+-------+-------+
    |       |       |       |       |
    |  <0>  |  <1>  |  <2>  |  <3>  |
    |       |       |       |       |
    |0      |1      |2      |3      |
    +-------+-------+-------+-------+
                                 .
                                /|\
                                 |
                                 |
                            ghost cells
@endverbatim
 */
#define IJKINDEX(I,J,K) ((I) + \
                         ((J)*CZData->NumIPts) + \
                         ((K)*CZData->NumIJPts))

/**
 * Calculates the I indice from the cell's primary corner or cell centered
 * index. See IJKINDEX() for a picture.
 */
#define IINDEX(N) ((N) % CZData->NumIPts)

/**
 * Calculates the J indice from the cell's primary corner or cell centered
 * index. See IJKINDEX() for a picture.
 */
#define JINDEX(N) (((N) % CZData->NumIJPts)/CZData->NumIPts)

/**
 * Calculates the K indice from the cell's primary corner or cell centered
 * index. See IJKINDEX() for a picture.
 */
#define KINDEX(N) ((N)/CZData->NumIJPts)

/* */
#define SWITCH(Type,A,B)      do {Type T = (A); (A) = (B); (B) = T;} while (FALSE)
#define SWITCH_DOUBLES(A,B)   SWITCH(double, (A), (B))
#define FPRINTFOK(x)          (Boolean_t)((x) > 0)
#define GRAPHICSARE3D(F)      ((F->PlotType == PlotType_Cartesian3D))

/* convenience macros for implication, P -> Q, and equivalence, P <-> Q. */
#define IMPLICATION(P,Q) (!(P) || (Q))
#define EQUIVALENCE(P,Q) ((P) == (Q))

/* suppress compiler warnings about unused parameters */
#ifndef UNUSED
# define UNUSED(param) (void)param
#endif 


/**
 * Converts a double into a float value
 *
 * param val
 *     double value to be converted
 */
#define CONVERT_DOUBLE_TO_FLOAT(val) \
  ( (val) > SMALLFLOAT \
    ? ( (val) < LARGEFLOAT \
        ? (float)(val) \
        : (float)LARGEFLOAT \
      ) \
    : ( (val) < -SMALLFLOAT  \
        ? ( (val) > -LARGEFLOAT \
            ? (float)(val) \
            : (float)-LARGEFLOAT \
          ) \
        : (float)0.0 \
      ) \
  )


/**
 * Clamps a double at the limits of Tecplot's precision
 *
 * param val
 *     double value to be clamped
 */
#define CLAMP_DOUBLE(val) \
  ( (val) > SMALLDOUBLE \
    ? ( (val) < LARGEDOUBLE \
        ? (double)(val) \
        : (double)LARGEDOUBLE \
      ) \
    : ( (val) < -SMALLDOUBLE  \
        ? ( (val) > -LARGEDOUBLE \
            ? (double)(val) \
            : (double)-LARGEDOUBLE \
          ) \
        : (double)0.0 \
      ) \
  )


/**
 * Converts a double into a 4-byte (signed) integer value
 *
 * param val
 *     double value to be converted
 */
#define CONVERT_DOUBLE_TO_INT32(val) \
  ( (val) > 1.0 \
    ? ( (val) < MAXINT32 \
        ? (Int32_t)(val) \
        : (Int32_t)MAXINT32 \
      ) \
    : ( (val) < -1.0  \
        ? ( (val) > (Int32_t)-MAXINT32 \
            ? (Int32_t)(val) \
            : (Int32_t)-MAXINT32 \
          ) \
        : (Int32_t)0.0 \
      ) \
  )


/**
 * Converts a double into a 2-byte (signed) integer value
 *
 * param val
 *     double value to be converted
 */
#define CONVERT_DOUBLE_TO_INT16(val) \
  ( (val) > 1.0 \
    ? ( (val) < MAXINT16 \
        ? (Int16_t)(val) \
        : (Int16_t)MAXINT16 \
      ) \
    : ( (val) < -1.0  \
        ? ( (val) > (Int16_t)-MAXINT16 \
            ? (Int16_t)(val) \
            : (Int16_t)-MAXINT16 \
          ) \
        : (Int16_t)0.0 \
      ) \
  )

/**
 * Copies two bytes from SrcBuffer to DstBuffer without causing a page
 * fault due to misaligned words.
 *
 * param DstBuffer
 *     Pointer the buffer to send the two bytes to
 * param SrcBuffer
 *     Pointer the buffer to get the two bytes from
 */
#define COPY_2_UNALIGNED_BYTES(DstBuffer, SrcBuffer) \
        do { \
          /* cannot check sizeof(SrcBuffer) or sizeof(DstBuffer) because they are */ \
          /* most likely single byte pointers into unaligned blocks of data */ \
          ((Byte_t *)(DstBuffer))[0] = ((Byte_t *)(SrcBuffer))[0]; \
          ((Byte_t *)(DstBuffer))[1] = ((Byte_t *)(SrcBuffer))[1]; \
        } while (FALSE)

/**
 * Copies two bytes from SrcBuffer to DstBuffer swapping the bytes
 * as it copies.  Will not cause a page fault due to misaligned words.
 *
 * param DstBuffer
 *     Pointer the buffer to send the two bytes to
 * param SrcBuffer
 *     Pointer the buffer to get the two bytes from
 */
#define COPY_AND_REVERSE_2_UNALIGNED_BYTES(DstBuffer, SrcBuffer) \
        do { \
          /* cannot check sizeof(SrcBuffer) or sizeof(DstBuffer) because they are */ \
          /* most likely single byte pointers into unaligned blocks of data */ \
          ((Byte_t *)(DstBuffer))[0] = ((Byte_t *)(SrcBuffer))[1]; \
          ((Byte_t *)(DstBuffer))[1] = ((Byte_t *)(SrcBuffer))[0]; \
        } while (FALSE)

/**
 * Copies four bytes from SrcBuffer to DstBuffer without causing a page
 * fault due to misaligned words.
 *
 * param DstBuffer
 *     Pointer the buffer to send the four bytes to
 * param SrcBuffer
 *     Pointer the buffer to get the four bytes from
 */
#define COPY_4_UNALIGNED_BYTES(DstBuffer, SrcBuffer) \
        do { \
          /* cannot check sizeof(SrcBuffer) or sizeof(DstBuffer) because they are */ \
          /* most likely single byte pointers into unaligned blocks of data */ \
          ((Byte_t *)(DstBuffer))[0] = ((Byte_t *)(SrcBuffer))[0]; \
          ((Byte_t *)(DstBuffer))[1] = ((Byte_t *)(SrcBuffer))[1]; \
          ((Byte_t *)(DstBuffer))[2] = ((Byte_t *)(SrcBuffer))[2]; \
          ((Byte_t *)(DstBuffer))[3] = ((Byte_t *)(SrcBuffer))[3]; \
        } while (FALSE)

/**
 * Copies four bytes from SrcBuffer to DstBuffer swapping the bytes
 * as it copies.  Will not cause a page fault due to misaligned words.
 *
 * param DstBuffer
 *     Pointer the buffer to send the four bytes to
 * param SrcBuffer
 *     Pointer the buffer to get the four bytes from
 */
#define COPY_AND_REVERSE_4_UNALIGNED_BYTES(DstBuffer, SrcBuffer) \
        do { \
          /* cannot check sizeof(SrcBuffer) or sizeof(DstBuffer) because they are */ \
          /* most likely single byte pointers into unaligned blocks of data */ \
          ((Byte_t *)(DstBuffer))[0] = ((Byte_t *)(SrcBuffer))[3]; \
          ((Byte_t *)(DstBuffer))[1] = ((Byte_t *)(SrcBuffer))[2]; \
          ((Byte_t *)(DstBuffer))[2] = ((Byte_t *)(SrcBuffer))[1]; \
          ((Byte_t *)(DstBuffer))[3] = ((Byte_t *)(SrcBuffer))[0]; \
        } while (FALSE)

/**
 * Copies four bytes from SrcBuffer to DstBuffer without causing a page
 * fault due to misaligned words.
 *
 * param DstBuffer
 *     Pointer the buffer to send the four bytes to
 * param SrcBuffer
 *     Pointer the buffer to get the four bytes from
 */
#define COPY_8_UNALIGNED_BYTES(DstBuffer, SrcBuffer) \
        do { \
          /* cannot check sizeof(SrcBuffer) or sizeof(DstBuffer) because they are */ \
          /* most likely single byte pointers into unaligned blocks of data */ \
          ((Byte_t *)(DstBuffer))[0] = ((Byte_t *)(SrcBuffer))[0]; \
          ((Byte_t *)(DstBuffer))[1] = ((Byte_t *)(SrcBuffer))[1]; \
          ((Byte_t *)(DstBuffer))[2] = ((Byte_t *)(SrcBuffer))[2]; \
          ((Byte_t *)(DstBuffer))[3] = ((Byte_t *)(SrcBuffer))[3]; \
          ((Byte_t *)(DstBuffer))[4] = ((Byte_t *)(SrcBuffer))[4]; \
          ((Byte_t *)(DstBuffer))[5] = ((Byte_t *)(SrcBuffer))[5]; \
          ((Byte_t *)(DstBuffer))[6] = ((Byte_t *)(SrcBuffer))[6]; \
          ((Byte_t *)(DstBuffer))[7] = ((Byte_t *)(SrcBuffer))[7]; \
        } while (FALSE)

/**
 * Copies eight bytes from SrcBuffer to DstBuffer swapping the bytes
 * as it copies.  Will not cause a page fault due to misaligned words.
 *
 * param DstBuffer
 *     Pointer the buffer to send the four bytes to
 * param SrcBuffer
 *     Pointer the buffer to get the four bytes from
 */
#define COPY_AND_REVERSE_8_UNALIGNED_BYTES(DstBuffer, SrcBuffer) \
        do { \
          /* cannot check sizeof(SrcBuffer) or sizeof(DstBuffer) because they are */ \
          /* most likely single byte pointers into unaligned blocks of data */ \
          ((Byte_t *)(DstBuffer))[0] = ((Byte_t *)(SrcBuffer))[7]; \
          ((Byte_t *)(DstBuffer))[1] = ((Byte_t *)(SrcBuffer))[6]; \
          ((Byte_t *)(DstBuffer))[2] = ((Byte_t *)(SrcBuffer))[5]; \
          ((Byte_t *)(DstBuffer))[3] = ((Byte_t *)(SrcBuffer))[4]; \
          ((Byte_t *)(DstBuffer))[4] = ((Byte_t *)(SrcBuffer))[3]; \
          ((Byte_t *)(DstBuffer))[5] = ((Byte_t *)(SrcBuffer))[2]; \
          ((Byte_t *)(DstBuffer))[6] = ((Byte_t *)(SrcBuffer))[1]; \
          ((Byte_t *)(DstBuffer))[7] = ((Byte_t *)(SrcBuffer))[0]; \
        } while (FALSE)

/**
 * Reverses the byte order of the specified 2 byte buffer.
 *
 * param Buffer
 *     Pointer to the 2 bytes needing byte order reversal.
 */
#define REVERSE_2_BYTES_1_AT_A_TIME(Buffer) \
          do { \
            Byte_t Byte0 = ((Byte_t *)(Buffer))[0]; \
            CHECK(sizeof(*(Buffer))==2); \
            ((Byte_t *)(Buffer))[0] = ((Byte_t *)(Buffer))[1]; \
            ((Byte_t *)(Buffer))[1] = Byte0; \
          } while (FALSE)

#define REVERSE_2_BYTES_2_AT_A_TIME(Buffer) \
          do { \
            UInt16_t data_bits = ((UInt16_t *)(Buffer))[0]; \
            CHECK(sizeof(*(Buffer))==2); \
            ((UInt16_t *)(Buffer))[0] = (((data_bits)<<8) | \
                                         ((data_bits&0xff))); \
          } while (FALSE)

/* REVERSE_2_BYTES_2_AT_A_TIME may actually be slower, needs testing. */
#define REVERSE_2_BYTES REVERSE_2_BYTES_1_AT_A_TIME

/**
 * Reverses the byte order of the specified 4 byte buffer.
 *
 * param Buffer
 *     Pointer to the 4 bytes needing byte order reversal.
 *
 * How this works:
 *
 *   ABCD
 *   D--- <<24  (1)
 *
 *   ABCD
 *   --C- &0x0000ff00
 *   -C-- <<8   (2)
 *
 *   ABCD
 *   -B-- &0x00ff0000
 *   --B- >>8   (3)
 *
 *   ABCD
 *   ---A >>24  (4)
 *
 * (1) | (2) | (3) | (4) = DCBA.
 *
 */
#define REVERSE_4_BYTES_1_AT_A_TIME(Buffer) \
          do { \
            Byte_t Byte0 = ((Byte_t *)(Buffer))[0]; \
            Byte_t Byte1 = ((Byte_t *)(Buffer))[1]; \
            CHECK(sizeof(*(Buffer))==4); \
            ((Byte_t *)(Buffer))[0] = ((Byte_t *)Buffer)[3]; \
            ((Byte_t *)(Buffer))[1] = ((Byte_t *)Buffer)[2]; \
            ((Byte_t *)(Buffer))[2] = Byte1; \
            ((Byte_t *)(Buffer))[3] = Byte0; \
          } while (FALSE)

#define REVERSE_4_BYTES_4_AT_A_TIME(Buffer) \
          do { \
            UInt32_t data_bits = *((UInt32_t *)(Buffer)); \
            CHECK(sizeof(*(Buffer))==4); \
            *((UInt32_t *)(Buffer)) = (((data_bits)<<24)            | \
                                       ((data_bits&0x0000ff00)<<8)  | \
                                       ((data_bits&0x00ff0000)>>8)  | \
                                       ((data_bits)>>24)); \
          } while (FALSE)

#if defined MSWIN
  /*
   * The DevStuido compiler seems to be the only one that can truly handle this
   * when optimization is turned on.
   */
  #define REVERSE_4_BYTES REVERSE_4_BYTES_4_AT_A_TIME
#else
  #define REVERSE_4_BYTES REVERSE_4_BYTES_1_AT_A_TIME
#endif

/**
 * Reverses the byte order of the specified 8 byte buffer.
 *
 * param Buffer
 *     Pointer to the 8 bytes needing byte order reversal.
 */
#define REVERSE_8_BYTES_1_AT_A_TIME(Buffer) \
        do { \
            Byte_t Byte0 = ((Byte_t *)(Buffer))[0]; \
            Byte_t Byte1 = ((Byte_t *)(Buffer))[1]; \
            Byte_t Byte2 = ((Byte_t *)(Buffer))[2]; \
            Byte_t Byte3 = ((Byte_t *)(Buffer))[3]; \
            CHECK(sizeof(*(Buffer))==8); \
            ((Byte_t *)(Buffer))[0] = ((Byte_t *)(Buffer))[7]; \
            ((Byte_t *)(Buffer))[1] = ((Byte_t *)(Buffer))[6]; \
            ((Byte_t *)(Buffer))[2] = ((Byte_t *)(Buffer))[5]; \
            ((Byte_t *)(Buffer))[3] = ((Byte_t *)(Buffer))[4]; \
            ((Byte_t *)(Buffer))[4] = Byte3; \
            ((Byte_t *)(Buffer))[5] = Byte2; \
            ((Byte_t *)(Buffer))[6] = Byte1; \
            ((Byte_t *)(Buffer))[7] = Byte0; \
        } while (FALSE)

#define REVERSE_8_BYTES_2_AT_A_TIME(Buffer) \
        do { \
          UInt16_t data_bits_0 = ((UInt16_t *)(Buffer))[0]; \
          UInt16_t data_bits_1 = ((UInt16_t *)(Buffer))[1]; \
          UInt16_t data_bits_2 = ((UInt16_t *)(Buffer))[2]; \
          UInt16_t data_bits_3 = ((UInt16_t *)(Buffer))[3]; \
          CHECK(sizeof(*(Buffer))==8); \
          ((UInt16_t *)(Buffer))[0] = (((data_bits_3)<<8) | \
                                       ((data_bits_3&0xff))); \
          ((UInt16_t *)(Buffer))[1] = (((data_bits_2)<<8) | \
                                       ((data_bits_2&0xff))); \
          ((UInt16_t *)(Buffer))[2] = (((data_bits_1)<<8) | \
                                       ((data_bits_1&0xff))); \
          ((UInt16_t *)(Buffer))[3] = (((data_bits_0)<<8) | \
                                       ((data_bits_0&0xff))); \
        } while (FALSE)

#define REVERSE_8_BYTES_4_AT_A_TIME(Buffer) \
        do { \
          UInt32_t data_bits_0 = ((UInt32_t *)(Buffer))[0]; \
          UInt32_t data_bits_1 = ((UInt32_t *)(Buffer))[1]; \
          CHECK(sizeof(*(Buffer))==8); \
          ((UInt32_t *)(Buffer))[0] = (((data_bits_1)<<24)           | \
                                       ((data_bits_1&0x0000ff00)<<8) | \
                                       ((data_bits_1&0x00ff0000)>>8) | \
                                       ((data_bits_1)>>24)); \
          ((UInt32_t *)(Buffer))[1] = (((data_bits_0)<<24)           | \
                                       ((data_bits_0&0x0000ff00)<<8) | \
                                       ((data_bits_0&0x00ff0000)>>8) | \
                                       ((data_bits_0)>>24)); \
        } while (FALSE)

#define REVERSE_8_BYTES_8_AT_A_TIME(Buffer) \
        do { \
          UInt64_t data_bits = *((UInt64_t *)(Buffer)); \
          CHECK(sizeof(*(Buffer))==8); \
          *((UInt64_t *)(Buffer)) = (((data_bits)<<56) | \
                                     ((data_bits&0x000000000000ff00)<<40) | \
                                     ((data_bits&0x0000000000ff0000)<<24) | \
                                     ((data_bits&0x00000000ff000000)<<8)  | \
                                     ((data_bits&0x000000ff00000000)>>8)  | \
                                     ((data_bits&0x0000ff0000000000)>>24) | \
                                     ((data_bits&0x00ff000000000000)>>40) | \
                                     ((data_bits)>>56)); \
        } while (FALSE)


#if defined MSWIN
  /*
   * The DevStuido compiler seems to be the only one that can truly handle this
   * when optimization is turned on.
   */
  #define REVERSE_8_BYTES REVERSE_8_BYTES_4_AT_A_TIME
#else
  #define REVERSE_8_BYTES REVERSE_8_BYTES_1_AT_A_TIME
#endif


/****************************************************************
 *                                                              *
 *             ADD-ON MSWIN IMPORT/EXPORT DEFINITIONS            *
 *                                                              *
 ****************************************************************/
#if defined MSWIN
#  define STDCALL __stdcall
#else
#  define STDCALL
#endif /* MSWIN */

#if defined (__cplusplus)
# define EXTERNC extern "C"
#else
# define EXTERNC
#endif /* __cplusplus */

#if defined MSWIN 
# define LINKTOADDON EXTERNC _declspec ( dllimport )
#else
# define LINKTOADDON EXTERNC
#endif /* MSWIN  */

/* Note: Add-ons should never define AMTEC_INTERNAL_MAKELIBTEC */
#if defined MSWIN && !defined TECPLOTKERNEL && defined AMTEC_INTERNAL_MAKELIBTEC
# undef LINKTOADDON
# define LINKTOADDON EXTERNC _declspec ( dllexport )
#endif

/*
 *
 * Usage:
 * EXPORTFROMADDON void STDCALL InitTecAddOn(void) { ... }
 *
 */

#if defined MSWIN
# define EXPORTFROMADDON EXTERNC _declspec ( dllexport )
#else
# define EXPORTFROMADDON EXTERNC
#endif /* MSWIN */

#define EXPORTFROMDLL EXPORTFROMADDON 

/* CORE SOURCE CODE REMOVED */


/* CORE SOURCE CODE REMOVED */


/****************************************************************
 *                                                              *
 *                       HARD CONSTANTS                         *
 *                                                              *
 ****************************************************************/
/* CORE SOURCE CODE REMOVED */

#define MAXINDEX                (LgIndex_t)2147483646   /* int */
#define MAXZONEMAP               32700                  /* int */
#define LARGEDOUBLE              1.0e+150               /* double */
#define SMALLDOUBLE              1.0e-150               /* double */
#define LARGESTEXPONENT          150                    /* int */
#define SMALLESTEXPONENT         -150                   /* int */
/* SMALLESTDOUBLE is needed for ActiveX backward compatability */
#define SMALLESTDOUBLE           SMALLDOUBLE  /* double */
#if defined VMS
#  define LARGESTDOUBLEEXPONENT  307  /* int */
#  define SMALLESTDOUBLEEXPONENT -307 /* int */
#  define LARGESTDOUBLE          1.0e+307 /* double */
#  define LARGEFLOAT             1.0e+37  /* float */
#  define SMALLFLOAT             1.0e-37  /* float */
#else
#  define LARGESTDOUBLEEXPONENT  308
#  define SMALLESTDOUBLEEXPONENT -307
#  define LARGESTDOUBLE          1.0e+308
#  define LARGEFLOAT             3.40282347E+38
#  define SMALLFLOAT             1.17549435E-38
/* Do not remove SMALLSTDOUBLE: needed for ActiveX library */
#  define SMALLSTDOUBLE          1.0e-307 /* double */
#endif
#define LARGELONG                MAXINDEX /* long */
#define LARGESHORT               32766    /* short */
#define MAXINT32                 LARGELONG
#define MAXINT16                 LARGESHORT
#define ETX                      3        /* int */
#define LN2                      0.69314718055994530942 /* double */
#define LN10                     2.30258509299404568402 /* double */
#define PIOVER2                  1.57079632679489661923 /* double */
#define TWOPI                    6.28318530717958647692 /* double */
#if defined PI
#undef PI
#endif
#define PI                       3.14159265358979323846 /* double */
#define ANGLEEPSILON             1.0e-10                /* double */
#define LARGESTANGLE             (4*PI+ANGLEEPSILON)    /* double */
#define DEGPERRADIANS            57.295779513082323     /* double */
#define CMPERINCH                2.54                   /* double */
#define POINTSPERINCH            72.0                   /* double */
#define FONTMOVEMARK             192                    /* int */
#define FONTDECISIONMARK         128                    /* int */
#define FONTLINEMARK             64                     /* int */ 
#define BAD_SET_VALUE            ((SetIndex_t)-1)       /* int */
#define MENU_POSITION_FIRST      (0)                    /* int */
#define MENU_POSITION_LAST       (-1)                   /* int */
/* BADSETVALUE is needed for ActiveX backward compatibility */
#define BADSETVALUE              BAD_SET_VALUE          /* int */
#define SOLID_TRANSLUCENCY       0                      /* int */
#define BAD_DISTANCE             (-1.0)                 /* double */
/* MIN_CIRCUMFERENTIAL_INDEX is the min J dimension for circular zones */
#define MIN_CIRCUMFERENTIAL_INDEX  4                    /* int */

/* CORE SOURCE CODE REMOVED */

/* CORE SOURCE CODE REMOVED */


#define TecplotBinaryFileVersion    102   /* NOTE: MUST also change this define symbol in preplot.c */
#define TecplotInterfaceVersion     100
#define TecplotInterfaceVersionStr  "100" /* stay in lockstep with TecplotInterfaceVersion */
#define TecplotLicenseVersion       100   /* may vary from TecplotInterfaceVersion */

#define    MAXASCIICOMMANDLENGTH     60000
#define    MaxNumZonesOrVars         MAXZONEMAP /* int */
#define    MaxXAxes                  5    /* int */
#define    MaxYAxes                  5    /* int */
#define    MaxGeoSegments            50   /* int */
#define    MaxPtsCircleOrEllipse     720  /* int */
#define    MaxFrames                 128  /* int */
#define    MaxCustomLabelSets        10   /* int */
#define    MaxCustomLabelsPerSet     5000 /* int */
#define    MaxFontMoves              20000  /* int */
#define    MaxColorMapOverrides      16     /* int */
#define    MaxValueBlankConstraints  8      /* int */
#define    MaxContourGroups          4

/*
 * If any of these values changes it's corresponding value in preplot.c must
 * change to match it so that files created by preplot and Tecplot are
 * consistent.
 */
#define    MaxChrsDatasetTitle       256    /* int */
#define    MaxChrsZnTitle            128    /* int */
#define    MaxChrsVarName            128    /* int */
#define    MaxChrsZnOrVarName        128    /* int */
/* currently limited to MaxLineIndex in preplot.c */
#define    MaxChrsAuxValueString     32000  /* int */

#define    MaxNumViews               16     /* int */
#define    MaxBasicSizes             5      /* int */
#define    MaxColorMapControlPoints  50     /* int */
#define    MaxRawColorMapEntries     800    /* int */
#define    MaxDataSetReaders         100    /* int */
#define    MaxExtendedCurveFits      100    /* int */
#define    MaxColorMapCycles         20     /* int */


/* Dimension Limits */

#define    MinPaperDimInWorkArea     0.5    /* double */
#define    MinFrameWidth             0.25   /* double */
#define    MinFrameHeight            0.25   /* double */
#define    MinAxisLength             0.1    /* double */


#define    BadEnumValue              255    /* int */


/* CORE SOURCE CODE REMOVED */

/* Tecplot Add-on Custom Products */

typedef enum
{
  AddOnProduct_None,
  AddOnProduct_RS,
  END_AddOnProduct_e,
  AddOnProduct_Invalid = BadEnumValue
} AddOnProduct_e;


/* CORE SOURCE CODE REMOVED */

#define    Black_C           ((ColorIndex_t)0)
#define    Red_C             ((ColorIndex_t)1)
#define    Green_C           ((ColorIndex_t)2)
#define    Blue_C            ((ColorIndex_t)3)
#define    Cyan_C            ((ColorIndex_t)4)
#define    Yellow_C          ((ColorIndex_t)5)
#define    Purple_C          ((ColorIndex_t)6)
#define    White_C           ((ColorIndex_t)7)

#define    Custom1_C         ((ColorIndex_t)8)
#define    Custom2_C         ((ColorIndex_t)9)
#define    Custom3_C         ((ColorIndex_t)10)
#define    Custom4_C         ((ColorIndex_t)11)
#define    Custom5_C         ((ColorIndex_t)12)
#define    Custom6_C         ((ColorIndex_t)13)
#define    Custom7_C         ((ColorIndex_t)14)
#define    Custom8_C         ((ColorIndex_t)15)
#define    Custom9_C         ((ColorIndex_t)16)

#define    Custom10_C         ((ColorIndex_t)17)
#define    Custom11_C         ((ColorIndex_t)18)
#define    Custom12_C         ((ColorIndex_t)19)
#define    Custom13_C         ((ColorIndex_t)20)
#define    Custom14_C         ((ColorIndex_t)21)
#define    Custom15_C         ((ColorIndex_t)22)
#define    Custom16_C         ((ColorIndex_t)23)
#define    Custom17_C         ((ColorIndex_t)24)
#define    Custom18_C         ((ColorIndex_t)25)
#define    Custom19_C         ((ColorIndex_t)26)

#define    Custom20_C         ((ColorIndex_t)27)
#define    Custom21_C         ((ColorIndex_t)28)
#define    Custom22_C         ((ColorIndex_t)29)
#define    Custom23_C         ((ColorIndex_t)30)
#define    Custom24_C         ((ColorIndex_t)31)
#define    Custom25_C         ((ColorIndex_t)32)
#define    Custom26_C         ((ColorIndex_t)33)
#define    Custom27_C         ((ColorIndex_t)34)
#define    Custom28_C         ((ColorIndex_t)35)
#define    Custom29_C         ((ColorIndex_t)36)

#define    Custom30_C         ((ColorIndex_t)37)
#define    Custom31_C         ((ColorIndex_t)38)
#define    Custom32_C         ((ColorIndex_t)39)
#define    Custom33_C         ((ColorIndex_t)40)
#define    Custom34_C         ((ColorIndex_t)41)
#define    Custom35_C         ((ColorIndex_t)42)
#define    Custom36_C         ((ColorIndex_t)43)
#define    Custom37_C         ((ColorIndex_t)44)
#define    Custom38_C         ((ColorIndex_t)45)
#define    Custom39_C         ((ColorIndex_t)46)

#define    Custom40_C         ((ColorIndex_t)47)
#define    Custom41_C         ((ColorIndex_t)48)
#define    Custom42_C         ((ColorIndex_t)49)
#define    Custom43_C         ((ColorIndex_t)50)
#define    Custom44_C         ((ColorIndex_t)51)
#define    Custom45_C         ((ColorIndex_t)52)
#define    Custom46_C         ((ColorIndex_t)53)
#define    Custom47_C         ((ColorIndex_t)54)
#define    Custom48_C         ((ColorIndex_t)55)
#define    Custom49_C         ((ColorIndex_t)56)

#define    Custom50_C         ((ColorIndex_t)57)
#define    Custom51_C         ((ColorIndex_t)58)
#define    Custom52_C         ((ColorIndex_t)59)
#define    Custom53_C         ((ColorIndex_t)60)
#define    Custom54_C         ((ColorIndex_t)61)
#define    Custom55_C         ((ColorIndex_t)62)
#define    Custom56_C         ((ColorIndex_t)63)

#define    MultiColor_C      ((ColorIndex_t)(-1))
#define    NoColor_C         ((ColorIndex_t)(-2))
#define    MultiColor2_C     ((ColorIndex_t)(-3))
#define    MultiColor3_C     ((ColorIndex_t)(-4))
#define    MultiColor4_C     ((ColorIndex_t)(-5))
#define    RGBColor_C        ((ColorIndex_t)(-6))
#define    InvalidColor_C    ((ColorIndex_t)(-255))

/* CORE SOURCE CODE REMOVED */

/****************************************************************
 *                                                              *
 *                          SIMPLE TYPEDEFS                     *
 *                                                              *
 ****************************************************************/



#if defined DECALPHA   || \
    defined LINUXALPHA || \
    defined LINUXI64   || \
    defined LINUX64    || \
    defined COMPAQALPHA
#define LONGIS64
#endif


/* How to define UInt64_t/Int64_t is platform specific, but they are always 8-bytes */
#if defined MSWIN
  typedef    unsigned __int64     UInt64_t;
  typedef    __int64              Int64_t;
#else
  #if defined CRAY
    typedef    unsigned int       UInt64_t;
    typedef    int                Int64_t;
  #else
    #if defined LONGIS64
      typedef unsigned long      UInt64_t;
      typedef long               Int64_t;
    #else
      typedef unsigned long long UInt64_t;
      typedef long long          Int64_t;
    #endif
  #endif
#endif

#if defined LONGIS64
  typedef    unsigned int    UInt32_t;
  typedef    int             Int32_t;
  typedef    int             LgInteger_t;
#else
  typedef    unsigned int    UInt32_t;
  typedef    int             Int32_t;
  typedef    int             LgInteger_t;
#endif

typedef    short           Int16_t;
typedef    unsigned short  UInt16_t;

typedef    int             LgIndex_t;
typedef    LgIndex_t       NodeMap_t;
typedef    LgIndex_t       ScreenDim_t;

/**
 * The following type is used for passing arbitrary integers or pointers in
 * parameters. The general rule is that this is 4 bytes on "32-bit" machines
 * and 8 bytes on "64-bit" machines.
 */
#if defined CRAY
  typedef char *ArbParam_t;
#elif defined LONGIS64
  typedef long ArbParam_t;
#elif defined MSWIN && (defined _M_IA64 || defined _M_AMD64)
   typedef INT_PTR ArbParam_t;
#else
   typedef LgIndex_t  ArbParam_t;
#endif

typedef    ArbParam_t      UniqueID_t;

/* used to hold file offset and size values */
typedef long FileOffset_t;

/**
 * 64 bit offset for memory mapped I/O.
 */
typedef UInt64_t MemMapOffset_t;

/*
 *  SmInteger must be at least a short....
 */

typedef    unsigned char    Byte_t;
typedef    short            SmInteger_t;
typedef    SmInteger_t      ColorIndex_t;
typedef    SmInteger_t      EntIndex_t;

typedef    char             Boolean_t;
typedef    char            *ZoneName_t;
typedef    char            *VarName_t;
typedef    char            *LString_t;

typedef    LgIndex_t        HeapLength_t;
typedef    LgIndex_t        SegPtsArray_t[MaxGeoSegments];
typedef    double           BasicSize_t[MaxBasicSizes];
typedef    double          *VarList_t;

typedef    long             SetIndex_t;

typedef    unsigned long    SetData_t;
typedef    SetData_t       *SetData_pt;

/* CORE SOURCE CODE REMOVED */

typedef    char             SymbolChar_t[3];




/****************************************************************
 *                                                              *
 *                     ENUMERATED TYPEDEFS                      *
 *                                                              *
 ****************************************************************/

typedef enum
{
   SidebarSizing_MaxOfAll,
   SidebarSizing_Dynamic,
   END_SidebarSizing_e,
   SidebarSizing_Invalid = BadEnumValue

} SidebarSizing_e;

typedef enum
{
   SidebarLocation_Left,
   SidebarLocation_Right,  /* Not allowed at this time */
   SidebarLocation_Top,    /* Not allowed at this time */
   SidebarLocation_Bottom, /* Not allowed at this time */
   END_SidebarLocation_e,
   SidebarLocation_Invalid = BadEnumValue

} SidebarLocation_e;

typedef enum
  {
    MenuItem_Option,
    MenuItem_Toggle,
    MenuItem_Separator,
    MenuItem_SubMenu,
    END_MenuItem_e,
    MenuItem_Invalid = BadEnumValue
  } MenuItem_e;

typedef enum
  {
    StandardMenu_File,
    StandardMenu_Edit,
    StandardMenu_View,
    StandardMenu_Plot,
    StandardMenu_Insert,
    StandardMenu_Data,
    StandardMenu_Frame,
    StandardMenu_Workspace,
    StandardMenu_Tools,
    StandardMenu_Help,
    END_StandardMenu_e,
    StandardMenu_Invalid = BadEnumValue
  } StandardMenu_e;

typedef enum
  {
    FieldProbeDialogPage_NodalValues,
    FieldProbeDialogPage_CellCenteredValues,
    FieldProbeDialogPage_ZoneCellInfo,
    FieldProbeDialogPage_FaceNeighbors,
    END_FieldProbeDialogPage_e,
    FieldProbeDialogPage_Invalid = BadEnumValue
  } FieldProbeDialogPage_e;

/* CORE SOURCE CODE REMOVED */

/**
 */
typedef enum
  {
    UndoStateCategory_FrameOrder,
    UndoStateCategory_Picked, /* picked changes, not the pick itself */
    UndoStateCategory_Text,
    UndoStateCategory_Geom,
    UndoStateCategory_View,
    UndoStateCategory_WorkspaceView,
    UndoStateCategory_Style,
    UndoStateCategory_SpecificStyle, /* meaning that specific undo style will be added by the caller */
    UndoStateCategory_Data,
    UndoStateCategory_DataAndStyle,
    END_UndoStateCategory_e,
    UndoStateCategory_Invalid = BadEnumValue
  } UndoStateCategory_e;


/*
 * Used only for Action_PropagateLinking
 */
typedef enum
{
  LinkType_WithinFrame,
  LinkType_BetweenFrames,
  END_LinkType_e,
  LinkType_Invalid = BadEnumValue
} LinkType_e;

typedef enum
{
  FrameCollection_All,
  FrameCollection_Picked,
  END_FrameCollection_e,
  FrameCollection_Invalid = BadEnumValue
} FrameCollection_e;



typedef enum 
{
  LegendProcess_DrawLegend,
  LegendProcess_EraseLegend,
  LegendProcess_GetExtents,
  END_LegendProcess_e,
  LegendProcess_Invalid = BadEnumValue
} LegendProcess_e;


typedef enum
  {
    RGBLegendOrientation_RGB,
    RGBLegendOrientation_GBR,
    RGBLegendOrientation_BRG,
    RGBLegendOrientation_RBG,
    RGBLegendOrientation_GRB,
    RGBLegendOrientation_BGR,
    END_RGBLegendOrientation_e,
    RGBLegendOrientation_Invalid = BadEnumValue
  } RGBLegendOrientation_e;



/* CORE SOURCE CODE REMOVED */



typedef enum
  {
    StateChange_VarsAltered,
    StateChange_VarsAdded,
    StateChange_ZonesDeleted,
    StateChange_ZonesAdded,
    StateChange_NodeMapsAltered,
    StateChange_FrameDeleted,
    StateChange_NewTopFrame,
    StateChange_Style,
    StateChange_DataSetReset,
    StateChange_NewLayout,
    StateChange_CompleteReset,
    StateChange_LineMapAssignment,         /* was StateChange_XYMapAssignment */
    StateChange_ContourLevels,
    StateChange_ModalDialogLaunch,
    StateChange_ModalDialogDismiss,
    StateChange_QuitTecplot,
    StateChange_ZoneName,
    StateChange_VarName,
    StateChange_LineMapName,               /* was StateChange_XYMapName */
    StateChange_LineMapAddDeleteOrReorder, /* was StateChange_XYMapAddDeleteOrReorder */
    StateChange_View,
    StateChange_ColorMap,
    StateChange_ContourVar,
    StateChange_Streamtrace,
    StateChange_NewAxisVariables,
    StateChange_MouseModeUpdate,
    StateChange_PickListCleared,
    StateChange_PickListGroupSelect,
    StateChange_PickListSingleSelect,
    StateChange_PickListStyle,
    StateChange_DataSetFileName,
    StateChange_UnsuspendInterface,        /* was StateChange_DrawGraphicsOn */
    StateChange_SuspendInterface,          /* was StateChange_DrawGraphicsOff */
    StateChange_DataSetLockOn,
    StateChange_DataSetLockOff,
    StateChange_Text,
    StateChange_Geom,
    StateChange_DataSetTitle,
    StateChange_DrawingInterrupted,
    StateChange_PrintPreviewLaunch,
    StateChange_PrintPreviewDismiss,
    StateChange_AuxDataAdded,
    StateChange_AuxDataDeleted,
    StateChange_AuxDataAltered,
    StateChange_VarsDeleted,
    END_StateChange_e,
    StateChange_Invalid = BadEnumValue,
    /* Deprecated values */
    StateChange_DrawGraphicsOn          = StateChange_UnsuspendInterface,
    StateChange_DrawGraphicsOff         = StateChange_SuspendInterface,
    StateChange_XYMapAssignment         = StateChange_LineMapAssignment,
    StateChange_XYMapName               = StateChange_LineMapName,
    StateChange_XYMapAddDeleteOrReorder = StateChange_LineMapAddDeleteOrReorder
  } StateChange_e; /*<help> "StateChange_DrawGraphicsOn is deprecated. Use StateChange_UnsuspendInterface\n"*/
                   /*<help> "StateChange_DrawGraphicsOff is deprecated. Use StateChange_SuspendInterface"*/

typedef enum
  {
    StateChangeMode_v75,
    StateChangeMode_v80,
    StateChangeMode_v100,
    END_StateChangeMode_e,
    StateChangeMode_Invalid = BadEnumValue
  } StateChangeMode_e;

typedef enum
{
  LayoutPackageObject_Image,
  LayoutPackageObject_Layout,
  LayoutPackageObject_Data,
  END_LayoutPackageObject_e,
  LayoutPackageObject_Invalid = BadEnumValue
} LayoutPackageObject_e;

typedef enum
  {
    VarLoadMode_ByName,
    VarLoadMode_ByPosition,
    END_VarLoadMode_e,
    VarLoadMode_Invalid = BadEnumValue
  } VarLoadMode_e;

typedef enum
  {
    ImageSelection_OnePerFrame,
    ImageSelection_WorkspaceOnly,
    END_ImageSelection_e,
    ImageSelection_Invalid = BadEnumValue
  } ImageSelection_e;

typedef enum
  {
    LibraryType_Foreign,
    LibraryType_V7Standard,
    LibraryType_V7ActiveX,
    END_LibraryType_e,
    LibraryType_Invalid = BadEnumValue
  } LibraryType_e; /* <help> "Add-on types" */


typedef enum
  {
    AssignOp_Equals,
    AssignOp_PlusEquals,
    AssignOp_MinusEquals,
    AssignOp_TimesEquals,
    AssignOp_DivideEquals,
    AssignOp_ConvertFromCm,
    AssignOp_ConvertFromIn,
    AssignOp_ConvertFromPt,
    AssignOp_ConvertFromPix,
    END_AssignOp_e,
    AssignOp_Invalid = BadEnumValue
  } AssignOp_e;

typedef enum
  {
    Dialog_ColorMap,
    Dialog_Equation,
    Dialog_MacroViewer,
    Dialog_ZoneMapStyle, /* was Dialog_PlotAttributes*/
    Dialog_QuickEdit,
    Dialog_QuickMacroPanel,
    Dialog_ValueBlanking,
    Dialog_Probe,          /* used for dialog positioning only */
    Dialog_ProbeAt,
    Dialog_NewLayout,
    Dialog_OpenLayout,
    Dialog_Save,
    Dialog_SaveAs,
    Dialog_LoadData,
    Dialog_WriteData,
    Dialog_Print,
    Dialog_Import,
    Dialog_Export,
    Dialog_MacroPlay,
    Dialog_MacroRecord,
    Dialog_AxisEdit,
    Dialog_SpatialVars,
    Dialog_Reset3DAxes,
    Dialog_ThreeDAxisLimits,
    Dialog_ThreeDOrientationAxis,
    Dialog_Streamtraces,
    Dialog_IsoSurfaces,
    Dialog_Slices,
    Dialog_Contour,
    Dialog_VectorLength,
    Dialog_VectorVars,
    Dialog_VectorArrowheads,
    Dialog_VectorReferenceVector,
    Dialog_ScatterSizeAndFont,
    Dialog_ScatterLegend,
    Dialog_ScatterReferenceSymbol,
    Dialog_RGBColorVarsAndRange,
    Dialog_RGBColorLegend,
    Dialog_LineMapLegend,
    Dialog_IJKBlanking,
    Dialog_DepthBlanking,
    Dialog_LightSource,
    Dialog_Advanced3DControl,
    Dialog_TwoDDrawOrder,
    Dialog_PolarDrawingOptions,
    Dialog_DataLabels,
    Dialog_StyleLinking,
    Dialog_Smooth,
    Dialog_TransformCoordinates,
    Dialog_Rotate2DData,
    Dialog_Create1DLine,
    Dialog_CreateRectangularZone,
    Dialog_CreateCircularZone,
    Dialog_DuplicateZone,
    Dialog_MirrorZone,
    Dialog_CreateZoneFromPolylines,
    Dialog_CreateZoneFromValues,
    Dialog_DeleteVariables,
    Dialog_DeleteZones,
    Dialog_ExtractContourLines,
    Dialog_ExtractFEBoundary,
    Dialog_ExtractIsoSurfaces,
    Dialog_ExtractSlices,
    Dialog_ExtractSliceFromPlane,
    Dialog_ExtractStreamtraces,
    Dialog_ExtractSubZone,
    Dialog_ExtractDiscretePoints,
    Dialog_ExtractPointsFromPolyline,
    Dialog_ExtractPointsFromGeometry,
    Dialog_LinearInterpolation,
    Dialog_InverseDistanceInterpolation,
    Dialog_KrigingInterpolation,
    Dialog_Triangulate,
    Dialog_DataInfo,
    Dialog_CurveInfo,
    Dialog_DataSpreadsheet,
    Dialog_PaperSetup,
    Dialog_OrderFrames,
    Dialog_RulerGrid,
    Dialog_ThreeDViewRotate,
    Dialog_ThreeDViewDetails,
    Dialog_TranslateMagnify,
    Dialog_PrintPreview,
    Dialog_ColorPreferences,
    Dialog_MiscPreferences,
    Dialog_SizePreferences,
    Dialog_SaveConfiguration,
    Dialog_SaveColorMap,
    Dialog_LoadColorMap,
    Dialog_HelpAboutTecplot,
    Dialog_HelpAboutAddOns,
    Dialog_Publish,
    Dialog_EditFrame,
    END_Dialog_e,
    Dialog_Invalid = BadEnumValue,
    /* Deprecated values */
    Dialog_PlotAttributes = Dialog_ZoneMapStyle
  } Dialog_e; /* <help> "Tecplot dialog types" */

typedef enum
  {
    AnchorAlignment_TopLeft,
    AnchorAlignment_TopCenter,
    AnchorAlignment_TopRight,
    AnchorAlignment_MiddleLeft,
    AnchorAlignment_MiddleCenter,
    AnchorAlignment_MiddleRight,
    AnchorAlignment_BottomLeft,
    AnchorAlignment_BottomCenter,
    AnchorAlignment_BottomRight,
    END_AnchorAlignment_e,
    AnchorAlignment_Invalid = BadEnumValue
  } AnchorAlignment_e;

/* CORE SOURCE CODE REMOVED */

/* CORE SOURCE CODE REMOVED */


#if defined EXPORT_DEPRECATED_INTERFACES_TO_ADK_ONLY
/**
 * @deprecated
 *     Please use \ref CurveInfoMode_e instead.
 */
typedef enum
  {
    ProcessXYMode_NotUsed1,         /* deprecated: do not use                     */
    ProcessXYMode_NotUsed2,         /* deprecated: do not use                     */
    ProcessXYMode_NotUsed3,         /* deprecated: do not use                     */
    ProcessXYMode_NotUsed4,         /* deprecated: do not use                     */
    ProcessXYMode_NotUsed5,         /* deprecated: do not use                     */
    ProcessXYMode_NotUsed6,         /* deprecated: do not use                     */
    ProcessXYMode_NotUsed7,         /* deprecated: do not use                     */
    ProcessXYMode_NotUsed8,         /* deprecated: do not use                     */
    ProcessXYMode_NotUsed9,         /* deprecated: do not use                     */
    ProcessXYMode_WriteCurveCoef,   /* deprecated: use CurveInfoMode_Coefficients */
    ProcessXYMode_WriteCurvePoints, /* deprecated: use CurveInfoMode_RawData      */
    END_ProcessXYMode_e,
    ProcessXYMode_Invalid = BadEnumValue
  } ProcessXYMode_e;
#endif

typedef enum
  {
    CurveInfoMode_Coefficients, /* ProcessXYMode_WriteCurveCoef   */
    CurveInfoMode_RawData,      /* ProcessXYMode_WriteCurvePoints */
    CurveInfoMode_Macro,        /* ProcessXYMode_WriteCurveCoefMacro */
    END_CurveInfoMode_e,
    CurveInfoMode_Invalid = BadEnumValue
  } CurveInfoMode_e;

/* CORE SOURCE CODE REMOVED */

typedef enum
  {
    StyleBase_Factory,
    StyleBase_Config,
    END_StyleBase_e,
    StyleBase_Invalid = BadEnumValue
  } StyleBase_e;


typedef enum
  {
    ReadDataOption_NewData,
    ReadDataOption_AppendData,
    ReadDataOption_ReplaceData,
    END_ReadDataOption_e,
    ReadDataOption_Invalid = BadEnumValue
  } ReadDataOption_e;

#if defined EXPORT_DEPRECATED_INTERFACES_TO_ADK_ONLY
/**
 * @deprecated
 *     Please use \ref LabelType_e instead.
 */
typedef enum
  {
    NodeLabel_Index,         /* deprecated: use LabelType_Index         */
    NodeLabel_VarValue,      /* deprecated: use LabelType_VarValue      */
    NodeLabel_XAndYVarValue, /* deprecated: use LabelType_XAndYVarValue */
    END_NodeLabel_e,
    NodeLabel_Invalid = BadEnumValue
  } NodeLabel_e;
#endif

typedef enum
  {
    LabelType_Index,         /* NodeLabel_Index         */
    LabelType_VarValue,      /* NodeLabel_VarValue      */
    LabelType_XAndYVarValue, /* NodeLabel_XAndYVarValue */
    END_LabelType_e,
    LabelType_Invalid = BadEnumValue
  } LabelType_e;


typedef enum
  {
    SubBoundaryEditOption_All,
    SubBoundaryEditOption_Add,
    SubBoundaryEditOption_Remove,
    SubBoundaryEditOption_AddOnly,
    END_SubBoundaryEditOption_e,
    SubBoundaryEditOption_Invalid = BadEnumValue
  } SubBoundaryEditOption_e;


typedef enum
  {
    PointerStyle_NotUsed1,
    PointerStyle_NotUsed2,
    PointerStyle_NotUsed3,
    PointerStyle_AllDirections,
    PointerStyle_NotUsed4,
    PointerStyle_NotUsed5,
    PointerStyle_NotUsed6,
    PointerStyle_UpperLeftBracket,
    PointerStyle_UpperRightBracket,
    PointerStyle_LeftBracket,
    PointerStyle_LowerLeftBracket,
    PointerStyle_LowerRightBracket,
    PointerStyle_RightBracket,
    PointerStyle_BottomBracket,
    PointerStyle_TopBracket,
    PointerStyle_UpDown,
    PointerStyle_LeftRight,
    END_PointerStyle_e,
    PointerStyle_Invalid = BadEnumValue
  } PointerStyle_e;

typedef enum
  {
    CursorStyle_Undefined,
    CursorStyle_StandardArrow,
    CursorStyle_AdjusterArrow,
    CursorStyle_AllDirections,
    CursorStyle_Rotate,
    CursorStyle_Zoom,
    CursorStyle_Locate,
    CursorStyle_UpperLeftBracket,
    CursorStyle_UpperRightBracket,
    CursorStyle_LeftBracket,
    CursorStyle_LowerLeftBracket,
    CursorStyle_LowerRightBracket,
    CursorStyle_RightBracket,
    CursorStyle_BottomBracket,
    CursorStyle_TopBracket,
    CursorStyle_UpDown,
    CursorStyle_LeftRight,
    CursorStyle_Waiting,
    END_CursorStyle_e,
    CursorStyle_Invalid = BadEnumValue
  } CursorStyle_e;


typedef enum
  {
    PickSubPosition_All,
    PickSubPosition_Top,
    PickSubPosition_Bottom,
    PickSubPosition_Left,
    PickSubPosition_Right,
    PickSubPosition_TopLeft,
    PickSubPosition_TopRight,
    PickSubPosition_BottomLeft,
    PickSubPosition_BottomRight,
    PickSubPosition_BottomAndTop,
    PickSubPosition_LeftAndRight,
    END_PickSubPosition_e,
    PickSubPosition_Invalid = BadEnumValue
  } PickSubPosition_e;


typedef enum
  {
    GetValue_Ok,
    GetValue_ResultTypeError,
    GetValue_SyntaxError,
    END_GetValueReturnCode_e,
    GetValue_Invalid = BadEnumValue
  } GetValueReturnCode_e; 

typedef enum
  {
    SetValue_Ok,
    SetValue_DuplicateValue,
    SetValue_InvalidCommandOption,
    SetValue_NoAttachedDatasetError,
    SetValue_NoAttachedFrameError,
    SetValue_NotAllowedInConfigError,
    SetValue_ValueRangeError,
    SetValue_ValueSyntaxError,
    SetValue_AssignOpError,
    SetValue_InvalidVarOrZone,
    SetValue_InternalMemoryError,
    SetValue_ContextError1,
    SetValue_ContextError2,
    SetValue_OnlyAllowedInConfigError,
    END_SetValueReturnCode_e,
/* CORE SOURCE CODE REMOVED */
    SetValue_Invalid = BadEnumValue
  } SetValueReturnCode_e; 


typedef enum
  {
    ObjectAlign_LeftJustify,
    ObjectAlign_RightJustify,
    ObjectAlign_Center,
    ObjectAlign_Top,
    ObjectAlign_Bottom,
    END_ObjectAlign_e,
    ObjectAlign_Invalid = BadEnumValue
  } ObjectAlign_e;


/*
 * For 3D axis labels only.
 */
typedef enum
  {
    LabelAlignment_ByAngle,
    LabelAlignment_AlongAxis,
    LabelAlignment_PerpendicularToAxis,
    END_LabelAlignment_e,
    LabelAlignment_Invalid = BadEnumValue
  } LabelAlignment_e; /* <help> Label alignment for 3D axis labels only" */

/*
 * View_SetMagnification added 02/24/03 so all plot types
 * can behave the same way "do a 'centered' magnifacation change".
 * Line plots will still accept View_Scale option and zoom towards
 * the corner so old macros/addons still work.
 */
typedef enum
  {
    View_Fit,
    View_DataFit,
    View_AxisFit,
    View_Scale,   /* Deprecated, Use SetMagnification */
    View_Center,
    View_Translate,
    View_Zoom,
    View_Last,
    View_Copy,
    View_Paste,
    View_Push,  /* End of V9 enums */
    View_SetMagnification,
    View_NiceFit,
    View_AxisNiceFit,
    View_MakeCurrentViewNice,
    View_AxisMakeCurrentValuesNice,
    View_AxisResetToEntireCircle,
    END_View_e,
    View_Invalid = BadEnumValue
  } View_e;



typedef enum
  {
    WorkspaceView_FitSelectedFrames,
    WorkspaceView_FitAllFrames,
    WorkspaceView_FitPaper,
    WorkspaceView_Maximize,
    WorkspaceView_LastView,
    WorkspaceView_Zoom,
    WorkspaceView_Translate,
    WorkspaceView_UnMaximize,
    END_WorkspaceView_e,
    WorkspaceView_Invalid = BadEnumValue
  } WorkspaceView_e;


typedef enum
  {
    Arrowhead_Plain,
    Arrowhead_Filled,
    Arrowhead_Hollow,
    END_ArrowheadStyle_e,
    Arrowhead_Invalid = BadEnumValue
  } ArrowheadStyle_e;


typedef enum
  {
    ArrowheadAttach_None,
    ArrowheadAttach_AtBeginning,
    ArrowheadAttach_AtEnd,
    ArrowheadAttach_AtBothEnds,
    END_ArrowheadAttachment_e,
    ArrowheadAttach_Invalid = BadEnumValue
  } ArrowheadAttachment_e;

typedef enum
  {
    Clipping_ClipToViewport,
    Clipping_ClipToFrame,
    END_Clipping_e,
    Clipping_Invalid = BadEnumValue
  } Clipping_e;

typedef enum
  {
    StatusInfo_Hover,
    StatusInfo_Identify,
    StatusInfo_Instruction,
    StatusInfo_Working,
    StatusInfo_PercentDone,
    END_StatusInfo_e,
    StatusInfo_Invalid = BadEnumValue
  } StatusInfo_e;


#if defined EXPORT_DEPRECATED_INTERFACES_TO_ADK_ONLY
/**
 * @deprecated
 *     Please use \ref PlotType_e instead.
 */
typedef enum
  {
    Frame_Empty,    /* deprecated: use PlotType_Automatic   */
    Frame_ThreeD,   /* deprecated: use PlotType_Cartesian3D */
    Frame_TwoD,     /* deprecated: use PlotType_Cartesian2D */
    Frame_XY,       /* deprecated: use PlotType_XYLine      */
    Frame_Sketch,   /* deprecated: use PlotType_Sketch      */
    END_FrameMode_e,
    Frame_Invalid = BadEnumValue
  } FrameMode_e;
#endif


typedef enum
  {
    PlotType_Automatic,   /* Frame_Empty  */
    PlotType_Cartesian3D, /* Frame_ThreeD */
    PlotType_Cartesian2D, /* Frame_TwoD   */
    PlotType_XYLine,      /* Frame_XY     */
    PlotType_Sketch,      /* Frame_Sketch */
    PlotType_PolarLine,
    END_PlotType_e,
    PlotType_Invalid = BadEnumValue
  } PlotType_e;


typedef enum
  {
    ContLineCreateMode_OneZonePerContourLevel,
    ContLineCreateMode_OneZonePerIndependentPolyline,
    END_ContLineCreateMode_e,
    ContLineCreateMode_Invalid = BadEnumValue
  } ContLineCreateMode_e;


typedef enum
  {
    PickObject_None,
    PickObject_Frame,
    PickObject_Axis,
    PickObject_3DOrientationAxis,
    PickObject_Geom,
    PickObject_Text,
    PickObject_ContourLegend,
    PickObject_ContourLabel,
    PickObject_ScatterLegend,
    PickObject_LineLegend,        /* was PickObject_XYLegend */
    PickObject_ReferenceVector,
    PickObject_ReferenceScatterSymbol,
    PickObject_StreamtracePosition,
    PickObject_StreamtraceTermLine,
    PickObject_Paper,
    PickObject_Zone,
    PickObject_XYMapping, /* deprecated: use PickObject_LineMapping */
    PickObject_StreamtraceCOB,
    PickObject_SliceCOB,
    PickObject_IsoSurfaceCOB,    
    PickObject_RGBLegend,
    PickObject_LineMapping,
    END_PickObjects_e,
    PickObject_Invalid = BadEnumValue,
    /* deprecated values */
    PickObject_XYLegend = PickObject_LineLegend
  } PickObjects_e;


/*BEGINREMOVEFROMADDON*/
typedef enum
  {
    SingleEditState_NotEditing,
    SingleEditState_ActivelyEditing,
    SingleEditState_WasEditing,
    END_SingleEditState_e,
    EditingInvalid = BadEnumValue
  } SingleEditState_e;


typedef enum
  {
    AxisSubObject_GridArea,
    AxisSubObject_AxisLine,
    AxisSubObject_Title,
    END_AxisSubObject_e,
    AxisSubObject_Invalid = BadEnumValue
  } AxisSubObject_e;

typedef enum
  {
    AxisSubPosition_GridMinBorder,
    AxisSubPosition_GridMaxBorder,
    AxisSubPosition_MainAxisLine,
    AxisSubPosition_BackAxisLine,
    AxisSubPosition_PerpAxisLine,
    AxisSubPosition_PerpBackAxisLine,
    END_AxisSubPosition_e,
    AxisSubPosition_Invalid = BadEnumValue,
    AxisSubPosition_2DStart = AxisSubPosition_GridMinBorder,
    AxisSubPosition_2DEnd = AxisSubPosition_MainAxisLine,
    AxisSubPosition_PolarStart = AxisSubPosition_GridMinBorder,
    AxisSubPosition_PolarEnd = AxisSubPosition_PerpBackAxisLine
  } AxisSubPosition_e;
/*ENDREMOVEFROMADDON*/

/*
 * NOTE: The _NoOp value is not at the top so this
 *       enumeration aligns with the old AltMouseButtonMode_e 
 *       enumeration.
 */
typedef enum
{
  MouseButtonClick_Redraw,
  MouseButtonClick_RevertToSelect,
  MouseButtonClick_NoOp,  
  END_MouseButtonClick_e,
  MouseButtonClick_Invalid = BadEnumValue
} MouseButtonClick_e;


typedef enum
{
  MouseButtonDrag_NoOp,
  MouseButtonDrag_ZoomData,
  MouseButtonDrag_ZoomPaper,
  MouseButtonDrag_TranslateData,
  MouseButtonDrag_TranslatePaper,
  MouseButtonDrag_RollerballRotate,
  MouseButtonDrag_SphericalRotate,
  MouseButtonDrag_XRotate,
  MouseButtonDrag_YRotate,
  MouseButtonDrag_ZRotate,
  MouseButtonDrag_TwistRotate,
  END_MouseButtonDrag_e,
  MouseButtonDrag_Invalid = BadEnumValue
} MouseButtonDrag_e;


/* CORE SOURCE CODE REMOVED */


typedef enum  /* Deprecated */
  {
    AltMouseButtonMode_Regen,
    AltMouseButtonMode_RevertToSelect,
    END_AltMouseButtonMode_e,
    AltMouseButtonMode_Invalid = BadEnumValue
  } AltMouseButtonMode_e;


typedef enum
  {
    Mouse_NoMode,
    Mouse_Select,
    Mouse_Adjust,
    Mouse_Zoom,
    Mouse_Translate,
    Mouse_Probe,
    Mouse_Text,
    Mouse_GeomPolyline,
    Mouse_GeomSquare,
    Mouse_GeomCircle,
    Mouse_GeomRectangle,
    Mouse_GeomEllipse,
    Mouse_GeomSpline,
    Mouse_CreateFrame,
    Mouse_RotateSpherical,
    Mouse_RotateRollerBall,
    Mouse_RotateTwist,
    Mouse_RotateXAxis,
    Mouse_RotateYAxis,
    Mouse_RotateZAxis,
    Mouse_ContourLabel,
    Mouse_ContourAdd,
    Mouse_ContourDelete,
    Mouse_StreamPoints,
    Mouse_StreamEndLine,
    Mouse_ExtractPoints,
    Mouse_ExtractLine,
    Mouse_CreateRectangularZone,
    Mouse_CreateCircularZone,
    Mouse_Slice,
    Mouse_User1,
    Mouse_User2,
    Mouse_User3,
    Mouse_User4,
    END_MouseButtonMode_e,
    Mouse_Invalid = BadEnumValue
  } MouseButtonMode_e;


typedef enum
  {
    DetailsButtonState_QuickEdit,
    DetailsButtonState_ObjectDetails,
    DetailsButtonState_ToolDetails,
    END_DetailsButtonState_e,
    DetailsButtonState_Invalid = BadEnumValue
  } DetailsButtonState_e;


typedef enum
  {
    Event_ButtonPress,
    Event_ButtonRelease,
    Event_ButtonDoublePress,
    Event_Motion,
    Event_Drag,
    Event_KeyPress,
    END_Event_e,
    Event_Invalid = BadEnumValue
  } Event_e;


typedef enum
  {
    ObjectDrawMode_DrawFirst,
    ObjectDrawMode_Move,
    ObjectDrawMode_Remove,
    ObjectDrawMode_Place,
    END_ObjectDrawMode_e,
    ObjectDrawMode_Invalid = BadEnumValue
  } ObjectDrawMode_e;


typedef enum
  {
    ThreeDViewChangeDrawLevel_Full,
    ThreeDViewChangeDrawLevel_Trace,
    END_ThreeDViewChangeDrawLevel_e,
    ThreeDViewChangeDrawLevel_Invalid = BadEnumValue
  } ThreeDViewChangeDrawLevel_e; /* <help> "ThreeDViewChangeDrawLevel is deprecated. Use PlotApproximateMode.\n"*/

typedef enum
  {
    NonCurrentFrameRedrawLevel_Full,
    NonCurrentFrameRedrawLevel_Trace,
    END_NonCurrentFrameRedrawLevel_e,
    NonCurrentFrameRedrawLevel_Invalid = BadEnumValue
  } NonCurrentFrameRedrawLevel_e; /* <help> "NonCurrentFrameRedrawLevel is deprecated. Use PlotApproximateMode.\n"*/


typedef enum
  {
    RotationMode_XYZAxis,
    RotationMode_Spherical,
    RotationMode_RollerBall,
    END_RotationMode_e,
    RotationMode_Invalid = BadEnumValue
  } RotationMode_e;




typedef enum
  {
    RotateAxis_X,
    RotateAxis_Y,
    RotateAxis_Z,
    RotateAxis_Psi,
    RotateAxis_Theta,
    RotateAxis_Alpha,
    RotateAxis_Twist,
    RotateAxis_VertRollerBall,
    RotateAxis_HorzRollerBall,
    RotateAxis_AboutVector,
/* CORE SOURCE CODE REMOVED */
    END_RotateAxis_e,
    RotateAxis_Invalid = BadEnumValue
  } RotateAxis_e;

typedef enum
  {
    RotateOriginLocation_DefinedOrigin,
    RotateOriginLocation_Viewer,
    END_RotateOriginLocation_e,
    RotateOriginLocation_Invalid = BadEnumValue
  } RotateOriginLocation_e;

/*
 * NOTE: This is only used with the $!Reset3DOrigin command.
 */
typedef enum
  {
    OriginResetLocation_DataCenter,
    OriginResetLocation_ViewCenter,
    END_OriginResetLocation_e,
    OriginResetLocation_Invalid = BadEnumValue
  } OriginResetLocation_e; 

/*
 * NOTE: This is only used with the $!CreateSliceZoneFromPlane command.
 */
typedef enum
  {
    SliceSource_SurfaceZones,
    SliceSource_VolumeZones,
    SliceSource_SurfacesOfVolumeZones,
    SliceSource_LinearZones,
    END_SliceSource_e,
    SliceSource_Invalid = BadEnumValue
  } SliceSource_e; 





typedef enum
  {
    Input_SmInteger,
    Input_Short,
    Input_Integer,
    Input_Float,
    Input_Double,
    END_Input_e,
    Input_Invalid = BadEnumValue
  } Input_e;



typedef enum
  {
    PtSelection_All,
    PtSelection_NearestN,
    PtSelection_OctantN,
    END_PtSelection_e,
    PtSelection_Invalid = BadEnumValue
  } PtSelection_e;



typedef enum
  {
    Drift_None,
    Drift_Linear,
    Drift_Quad,
    END_Drift_e,
    Drift_Invalid = BadEnumValue
  } Drift_e;



/* atpoint is simple boundary condition.
   atpointb2 is better boundary condition.
*/
typedef enum
  {
    DerivPos_atpoint,
    DerivPos_atpointb2,
    DerivPos_kphalf,
    DerivPos_jphalf,
    DerivPos_iphalf,
    END_DerivPos_e,
    DerivPos_Invalid = BadEnumValue
  } DerivPos_e; /*<help>"atpoint is the simple boundary condition\n"*/
                /*<help>"atpointb2 is a better boundary condition"*/



typedef enum
  {
    LinearInterpMode_DontChange,
    LinearInterpMode_SetToConst,
    END_LinearInterpMode_e,
    LinearInterpMode_Invalid = BadEnumValue
  } LinearInterpMode_e;



typedef enum
  {
    ConstraintOp2Mode_UseVar,
    ConstraintOp2Mode_UseConstant,
    END_ConstraintOp2Mode_e,
    ConstraintOp2Mode_Invalid = BadEnumValue
  } ConstraintOp2Mode_e;



typedef enum
  {
    ValueBlankCellMode_AllCorners,
    ValueBlankCellMode_AnyCorner,
    ValueBlankCellMode_PrimaryValue,
    END_ValueBlankCellMode_e,
    ValueBlankCellMode_Invalid = BadEnumValue,
    /* Deprecated values */
    ValueBlankCellMode_PrimaryCorner = ValueBlankCellMode_PrimaryValue 
  } ValueBlankCellMode_e;


/*
 * DEPRECATED: ValueBlankMode_e enumeration will not be supported after
 *             version 8. This API was retained for add-on developers
 *             using the TecUtilStyleSetLowLevel API.
 */
typedef enum
  {
    ValueBlankMode_AndRule,
    ValueBlankMode_OrRule,
    ValueBlankMode_CornerRule,
    END_ValueBlankMode_e,
    ValueBlankMode_Invalid = BadEnumValue
  } ValueBlankMode_e; /*<help>"DEPRECATED: ValueBlankMode_e will not be supported after version 8"*/


typedef enum
  {
    CellBlankedCond_NotBlanked,
    CellBlankedCond_PartiallyBlanked,
    CellBlankedCond_EntirelyBlanked,
    CellBlankedCond_Uncertain,
    END_CellBlankedCond_e,
    CellBlankedCond_Invalid = BadEnumValue
  } CellBlankedCond_e;


typedef enum
  {
    RelOp_LessThanOrEqual,
    RelOp_GreaterThanOrEqual,
    RelOp_LessThan,
    RelOp_GreaterThan,
    RelOp_EqualTo,
    RelOp_NotEqualTo,
    END_RelOp_e,
    RelOp_Invalid = BadEnumValue
  } RelOp_e;



typedef enum
  {
    IJKBlankMode_BlankInterior,
    IJKBlankMode_BlankExterior,
    END_IJKBlankMode_e,
    IJKBlankMode_Invalid = BadEnumValue
  } IJKBlankMode_e;


typedef enum
  {
    PlotApproximationMode_Automatic,
    PlotApproximationMode_NonCurrentAlwaysApproximated,
    PlotApproximationMode_AllFramesAlwaysApproximated,
    END_PlotApproximationMode_e,
    PlotApproximationMode_Invalid = BadEnumValue
  } PlotApproximationMode_e;

typedef enum
  {
    SphereScatterRenderQuality_Low,
    SphereScatterRenderQuality_Medium,
    SphereScatterRenderQuality_High,
    SphereScatterRenderQuality_Invalid = BadEnumValue
  } SphereScatterRenderQuality_e;

/*
 * NOTE: FillPat_e is deprecated.  It must be retained to maintain
 *       backward compatibility with the TecUtil layer however.
 *       This has been replaced by Translucency_e.
 */
typedef enum                     
  {                             
    Pattern_Solid,             
    Pattern_LowTranslucent,   
    Pattern_MedTranslucent,  
    Pattern_HighTranslucent, 
    END_FillPat_e,           
    Pattern_Invalid = BadEnumValue 
  } FillPat_e; /*<help>"DEPRECATED: Replaced by Translucency_e"*/             


typedef enum                     
  {                             
    Translucency_Solid,             
    Translucency_Low,   
    Translucency_Medium,  
    Translucency_High, 
    END_Translucency_e,           
    Translucency_Invalid = BadEnumValue 
  } Translucency_e;                    



typedef enum
  {
    SunRaster_OldFormat,
    SunRaster_Standard,
    SunRaster_ByteEncoded,
    END_SunRaster_e,
    SunRaster_Invalid = BadEnumValue
  } SunRaster_e;


typedef enum
  {
    BoundaryCondition_Fixed,
    BoundaryCondition_ZeroGradient,
    BoundaryCondition_Zero2nd,
    END_BoundaryCondition_e,
    BoundaryCondition_Invalid = BadEnumValue
  } BoundaryCondition_e;



/* Note:
 *   In 2D: AxisMode_Independent and AxisMode_XYDependent are used;
 *   in 3D: AxisMode_Independent, AxisMode_XYZDependent, and AxisMode_XYDependent are used.
 */
typedef enum
  {
    AxisMode_Independent,
    AxisMode_XYZDependent,
    AxisMode_XYDependent,
    END_AxisMode_e,
    AxisMode_Invalid = BadEnumValue
  } AxisMode_e;/*<help>"In 2D AxisMode_Independent and AxisMode_XYDependent are used\n"*/
               /*<help>"In 3D AxisMode_Independent, "*/
               /*<help>"AxisMode_XYZDependent, and AxisMode_XYDependent are used."*/

typedef enum
  {
    Quick_LineColor,
    Quick_FillColor,
    Quick_TextColor,
    END_QuickColorMode_e,
    Quick_Invalid = BadEnumValue
  } QuickColorMode_e;


typedef enum
  {
    FillMode_None,
    FillMode_UseSpecificColor,
    FillMode_UseLineColor,
    FillMode_UseBackgroundColor,
    END_FillMode_e,
    FillMode_Invalid = BadEnumValue
  } FillMode_e;


typedef enum
  {
    LinePattern_Solid,
    LinePattern_Dashed,
    LinePattern_DashDot,
    LinePattern_Dotted,
    LinePattern_LongDash,
    LinePattern_DashDotDot,
    END_LinePattern_e,
    LinePattern_Invalid = BadEnumValue
  } LinePattern_e;



typedef enum
  {
    Join_Miter,
    Join_Round,
    Join_Bevel,
    END_LineJoin_e,
    Join_Invalid = BadEnumValue
  } LineJoin_e;



typedef enum
  {
    Cap_Flat,
    Cap_Round,
    Cap_Square,
    END_LineCap_e,
    Cap_Invalid = BadEnumValue
  } LineCap_e;



typedef enum
  {
    GeomForm_LineSegs,
    GeomForm_Rectangle,
    GeomForm_Square,
    GeomForm_Circle,
    GeomForm_Ellipse,
    GeomForm_LineSegs3D, /* deprecated: use GeomForm_LineSegs with CoordSys_Grid3D */
    GeomForm_Image,
    END_GeomForm_e,
    GeomForm_Invalid = BadEnumValue,
    /* new value names */
    GeomType_LineSegs = GeomForm_LineSegs,
    GeomType_Rectangle = GeomForm_Rectangle,
    GeomType_Square = GeomForm_Square,
    GeomType_Circle = GeomForm_Circle,
    GeomType_Ellipse = GeomForm_Ellipse,
    GeomType_LineSegs3D = GeomForm_LineSegs3D, /* deprecated: use GeomType_LineSegs with CoordSys_Grid3D */
    GeomType_Image = GeomForm_Image,
    END_GeomType_e = END_GeomForm_e,
    GeomType_Invalid = GeomForm_Invalid
  } GeomForm_e;

typedef GeomForm_e GeomType_e;

typedef enum
  {
    VariableDerivationMethod_Fast,
    VariableDerivationMethod_Accurate,
    END_VariableDerivationMethod_e,
    VariableDerivationMethod_Invalid = BadEnumValue
  } VariableDerivationMethod_e;

/**
 */
typedef enum
  {
    AuxDataType_String,
    END_AuxDataType_e,
    AuxDataType_Invalid = BadEnumValue
  } AuxDataType_e;

/**
 */
typedef enum
  {
    AuxDataLocation_Zone,
    AuxDataLocation_DataSet,
    AuxDataLocation_Frame,
    AuxDataLocation_Var,
    AuxDataLocation_LineMap,
    END_AuxDataLocation_e,
    AuxDataLocation_Invalid = BadEnumValue
  } AuxDataLocation_e;


/* Note: This replaces Element_e */
typedef enum
  {
    ZoneType_Ordered,
    ZoneType_FETriangle,
    ZoneType_FEQuad,
    ZoneType_FETetra,
    ZoneType_FEBrick,
    ZoneType_FELineSeg,
    END_ZoneType_e,
    ZoneType_Invalid = BadEnumValue
  } ZoneType_e;

typedef enum
  {
    ZoneOrder_I,
    ZoneOrder_J,
    ZoneOrder_K,
    ZoneOrder_IJ,
    ZoneOrder_IK,
    ZoneOrder_JK,
    ZoneOrder_IJK,
    END_ZoneOrder_e,
    ZoneOrder_Invalid = BadEnumValue
  } ZoneOrder_e;

/* deprecated: replaced by ZoneType_e DataPacking_e */
typedef enum
  {
    DataFormat_IJKBlock,
    DataFormat_IJKPoint,
    DataFormat_FEBlock,
    DataFormat_FEPoint,
    END_DataFormat_e,
    DataFormat_Invalid = BadEnumValue
  } DataFormat_e;

typedef enum
  {
    DataPacking_Block,
    DataPacking_Point,
    END_DataPacking_e,
    DataPacking_Invalid = BadEnumValue
  } DataPacking_e;



typedef enum
  {
    PD_HPGL,
    PD_HPGL2,
    PD_PS,
    PD_LASERG, /* deprecated */
    PD_EPS,
    PD_WINDOWS, /* Windows Print Driver */
    PD_WMF, /* Windows MetaFile (used from Export only) */
    END_PrinterDriver_e,
    PD_Invalid = BadEnumValue
  } PrinterDriver_e;



typedef enum
  {
    Image_None,
    Image_TIFF,
    Image_EPSI2,
    Image_FRAME,
    END_EPSPreviewImage_e,
    Image_Invalid = BadEnumValue
  } EPSPreviewImage_e;

typedef enum
  {
    TIFFByteOrder_Intel,
    TIFFByteOrder_Motorola,
    END_TIFFByteOrder_e,
    TIFFByteOrder_Invalid = BadEnumValue
  } TIFFByteOrder_e;

typedef enum
  {
    JPEGEncoding_Standard,
    JPEGEncoding_Progressive,
    END_JPEGEncoding_e,
    JPEGEncoding_Invalid = BadEnumValue
  } JPEGEncoding_e;


typedef enum
  {
    FlashImageType_Lossless,
    FlashImageType_JPEG,
    FlashImageType_256Color,
    END_FlashImageType_e,
    FlashImageType_Invalid = BadEnumValue
  } FlashImageType_e;

typedef enum
  {
    FlashCompressionType_BestSpeed,
    FlashCompressionType_SmallestSize,
    END_FlashCompressionType_e,
    FlashCompressionType_Invalid = BadEnumValue
  } FlashCompressionType_e;


typedef enum
  {
    ExportFormat_RasterMetafile,
    ExportFormat_TIFF,
    ExportFormat_SGI,
    ExportFormat_SunRaster,
    ExportFormat_XWindows,
    ExportFormat_PSImage,       /* deprecated */
    ExportFormat_HPGL,
    ExportFormat_HPGL2,
    ExportFormat_PS,
    ExportFormat_EPS,
    ExportFormat_LaserGraphics, /* deprecated */
    ExportFormat_WindowsMetafile,
    ExportFormat_BMP,
    ExportFormat_PNG,
    ExportFormat_AVI,
    ExportFormat_Custom,  /* May be used in a future version */
    ExportFormat_JPEG,
    ExportFormat_Flash,
    END_ExportFormat_e,
    ExportFormat_Invalid = BadEnumValue
  } ExportFormat_e;

typedef enum
  {
    AnimationDest_Screen,
    AnimationDest_AVI,
    AnimationDest_RM,
    AnimationDest_Flash,
    END_AnimationDest_e,
    AnimationDest_Invalid = BadEnumValue
  } AnimationDest_e;

typedef enum
  {
    ZoneAnimationMode_StepByNumber,
    ZoneAnimationMode_GroupStepByNumber,
    ZoneAnimationMode_StepByTime,
    END_ZoneAnimationMode_e,
    ZoneAnimationMode_Invalid = BadEnumValue
  } ZoneAnimationMode_e;

#if defined EXPORT_DEPRECATED_INTERFACES_TO_ADK_ONLY
/**
 * @deprecated
 *     Please use \ref ExportRegion_e instead.
 */
typedef enum
  {
    BitDumpRegion_CurrentFrame,
    BitDumpRegion_AllFrames,
    BitDumpRegion_WorkArea,
    END_BitDumpRegion_e,
    BitDumpRegion_Invalid = BadEnumValue
  } BitDumpRegion_e;
#endif

typedef enum
  {
    ExportRegion_CurrentFrame,
    ExportRegion_AllFrames,
    ExportRegion_WorkArea,
    END_ExportRegion_e,
    ExportRegion_Invalid = BadEnumValue
  } ExportRegion_e;

typedef enum
  {
    Paper_Letter,
    Paper_Double,
    Paper_A4,
    Paper_A3,
    Paper_Custom1,
    Paper_Custom2,
    END_PaperSize_e,
    Paper_Invalid = BadEnumValue
  } PaperSize_e;



typedef enum
  {
    PaperUnitSpacing_HalfCentimeter,
    PaperUnitSpacing_OneCentimeter,
    PaperUnitSpacing_TwoCentimeters,
    PaperUnitSpacing_QuarterInch,
    PaperUnitSpacing_HalfInch,
    PaperUnitSpacing_OneInch,
    PaperUnitSpacing_TenPoints,
    PaperUnitSpacing_TwentyFourPoints,
    PaperUnitSpacing_ThirtySixPoints,
    PaperUnitSpacing_FiftyPoints,
    PaperUnitSpacing_SeventyTwoPoints,
    PaperUnitSpacing_OneTenthInch,
    PaperUnitSpacing_OneTenthCentimeter,
    END_PaperUnitSpacing_e,
    PaperUnitSpacing_Invalid = BadEnumValue
  } PaperUnitSpacing_e;


typedef enum
  {
    Palette_Monochrome,
    Palette_PenPlotter,
    Palette_Color,
    END_Palette_e,
    Palette_Invalid = BadEnumValue
  } Palette_e;


typedef enum
  {
    PrintRenderType_Vector,
    PrintRenderType_Image,
    END_PrintRenderType_e,
    PrintRenderType_Invalid = BadEnumValue
  } PrintRenderType_e;


typedef enum
  {
    Units_Grid,
    Units_Frame,
    Units_Point,
    Units_Screen,
    Units_AxisPercentage,
    END_Units_e,
    Units_Invalid = BadEnumValue
  } Units_e;


typedef enum
  {
    CoordScale_Linear,
    CoordScale_Log,
    END_CoordScale_e,
    CoordScale_Invalid = BadEnumValue,
    /* old names for the same values */
    Scale_Linear = CoordScale_Linear,
    Scale_Log = CoordScale_Log,
    Scale_Invalid = CoordScale_Invalid
  } CoordScale_e;

/* CORE SOURCE CODE REMOVED */

typedef enum
  {
    CoordSys_Grid,
    CoordSys_Frame,
    CoordSys_FrameOffset,
    CoordSys_Paper,
    CoordSys_Screen,
    CoordSys_Hardcopy,
    CoordSys_Grid3D,
    END_CoordSys_e,
    CoordSys_Invalid = BadEnumValue
  } CoordSys_e;

/*
 *  NOTE:  CoordSys_FrameOffset always is stored in inches internally.
 *         in stylesheet this may be written in other units if
 *         appropriate suffix is added.
 *
 */



typedef enum
  {
    Scope_Global,
    Scope_Local,
    END_Scope_e,
    Scope_Invalid = BadEnumValue
  } Scope_e;


typedef enum
  {
    TextAnchor_Left,
    TextAnchor_Center,
    TextAnchor_Right,
    TextAnchor_MidLeft,
    TextAnchor_MidCenter,
    TextAnchor_MidRight,
    TextAnchor_HeadLeft,
    TextAnchor_HeadCenter,
    TextAnchor_HeadRight,
    TextAnchor_OnSide,
    END_TextAnchor_e,
    TextAnchor_Invalid = BadEnumValue
  } TextAnchor_e;



typedef enum
  {
    TextBox_None,
    TextBox_Filled,
    TextBox_Hollow,
    END_TextBox_e,
    TextBox_Invalid = BadEnumValue
  } TextBox_e;



typedef enum
  {
    GeomShape_Square,
    GeomShape_Del,
    GeomShape_Grad,
    GeomShape_RTri,
    GeomShape_LTri,
    GeomShape_Diamond,
    GeomShape_Circle,
    GeomShape_Cube,
    GeomShape_Sphere,
    GeomShape_Octahedron,
    GeomShape_Point,
    END_GeomShape_e,
    GeomShape_Invalid = BadEnumValue
  } GeomShape_e;


typedef enum
  {
    BasicSize_Tiny,
    BasicSize_Small,
    BasicSize_Medium,
    BasicSize_Large,
    BasicSize_Huge,
    END_BasicSize_e,
    BasicSize_Invalid = BadEnumValue
  } BasicSize_e;



/*
 * NOTE: LineForm_e is deprecated.  It must be retained to maintain
 *       backward compatibility with the TecUtil layer however.
 *       This has been replaced by CurveType_e.
 */
typedef enum
  {
    LineForm_LineSeg,
    LineForm_CurvFit,
    LineForm_EToRFit,
    LineForm_PowerFit,
    LineForm_Spline,
    LineForm_ParaSpline,
    END_LineForm_e,
    LineForm_Invalid = BadEnumValue
  } LineForm_e;


typedef enum
  {
    CurveType_LineSeg,
    CurveType_PoylnomialFit,
    CurveType_EToRFit,
    CurveType_PowerFit,
    CurveType_Spline,
    CurveType_ParaSpline,
    CurveType_Extended,
    END_CurveType_e,
    CurveType_Invalid = BadEnumValue,
    CurveType_CurvFit = CurveType_PoylnomialFit
  } CurveType_e;

typedef enum
  {
    Script_None,
    Script_Super,
    Script_Sub,
    END_Script_e,
    Script_Invalid = BadEnumValue
  } Script_e;


typedef enum
  {
    Font_Helvetica,
    Font_HelveticaBold,
    Font_Greek,
    Font_Math,
    Font_UserDefined,
    Font_Times,
    Font_TimesItalic,
    Font_TimesBold,
    Font_TimesItalicBold,
    Font_Courier,
    Font_CourierBold,
    END_Font_e,
    Font_Invalid = BadEnumValue
  } Font_e;

typedef enum
  {
    TwoDDrawOrder_ByZone,
    TwoDDrawOrder_ByLayer,
    END_TwoDDrawOrder_e,
    TwoDDrawOrder_Invalid = BadEnumValue
  } TwoDDrawOrder_e;

typedef enum
  {
    DrawOrder_AfterData,
    DrawOrder_BeforeData,
    END_DrawOrder_e,
    DrawOrder_Invalid = BadEnumValue
  } DrawOrder_e;

/*
 *
 * NOTE: Streamtrace_TwoDLine is new.  All 2D
 *       streamtraces are assigned this value.
 */
typedef enum
  {
    Streamtrace_SurfaceLine,
    Streamtrace_SurfaceRibbon,
    Streamtrace_VolumeLine,
    Streamtrace_VolumeRibbon,
    Streamtrace_VolumeRod,
    Streamtrace_TwoDLine,
    END_Streamtrace_e,
    Streamtrace_Invalid = BadEnumValue
  } Streamtrace_e;



typedef enum
  {
    StreamDir_Forward,
    StreamDir_Reverse,
    StreamDir_Both,
    END_StreamDir_e,
    StreamDir_Invalid = BadEnumValue
  } StreamDir_e;

typedef enum
  {
    IsoSurfaceSelection_AllContourLevels,
    IsoSurfaceSelection_OneSpecificValue,
    IsoSurfaceSelection_TwoSpecificValues,
    IsoSurfaceSelection_ThreeSpecificValues,
    END_IsoSurfaceSelection_e,
    IsoSurfaceSelection_Invalid = BadEnumValue
  } IsoSurfaceSelection_e;


typedef enum
  {
    ValueLocation_CellCentered,
    ValueLocation_Nodal,
    END_ValueLocation_e,
    ValueLocation_Invalid = BadEnumValue
  } ValueLocation_e;

typedef enum
  {
    FieldDataType_Reserved, /* never use */
    FieldDataType_Float,
    FieldDataType_Double,
    FieldDataType_Int32,
    FieldDataType_Int16,
    FieldDataType_Byte,
    FieldDataType_Bit,
    END_FieldDataType_e,
    FieldDataType_IJKFunction,   /* Not used yet */
    FieldDataType_Int64, /* Not used yet */
#if defined EXPORT_DEPRECATED_INTERFACES_TO_ADK_ONLY
    FieldDataType_LongInt = FieldDataType_Int32,
    FieldDataType_ShortInt = FieldDataType_Int16,
#endif
    FieldDataType_Invalid = BadEnumValue
  } FieldDataType_e;

#define VALID_FIELD_DATA_TYPE(FieldDataType) (VALID_ENUM((FieldDataType),FieldDataType_e) && \
                                              (FieldDataType)!=FieldDataType_Reserved)

#if defined EXPORT_DEPRECATED_INTERFACES_TO_ADK_ONLY
/**
 * @deprecated
 *     Please use \ref MeshType_e instead.
 */
typedef enum
  {
    Mesh_Wireframe,  /* deprecated: use MeshType_Wireframe  */
    Mesh_Overlay,    /* deprecated: use MeshType_Overlay    */
    Mesh_HiddenLine, /* deprecated: use MeshType_HiddenLine */
    END_MeshPlotType_e,
    Mesh_Invalid = BadEnumValue
  } MeshPlotType_e;
#endif

typedef enum
  {
    MeshType_Wireframe,  /* Mesh_Wireframe  */
    MeshType_Overlay,    /* Mesh_Overlay    */
    MeshType_HiddenLine, /* Mesh_HiddenLine */
    END_MeshType_e,
    MeshType_Invalid = BadEnumValue
  } MeshType_e;




#if defined EXPORT_DEPRECATED_INTERFACES_TO_ADK_ONLY
/**
 * @deprecated
 *     Please use \ref ContourType_e instead.
 */
typedef enum
  {
    Contour_Lines,       /* deprecated: use ContourType_Lines        */
    Contour_Flood,       /* deprecated: use ContourType_Flood        */
    Contour_Overlay,     /* deprecated: use ContourType_Overlay      */
    Contour_AverageCell, /* deprecated: use ContourType_AverageCell  */
    Contour_CornerCell,  /* deprecated: use ContourType_PrimaryValue */
    END_ContourPlotType_e,
    Contour_Invalid = BadEnumValue
  } ContourPlotType_e;
#endif


typedef enum
  {
    ContourType_Lines,         /* Contour_Lines       */
    ContourType_Flood,         /* Contour_Flood       */
    ContourType_Overlay,       /* Contour_Overlay     */
    ContourType_AverageCell,   /* Contour_AverageCell */
    ContourType_PrimaryValue,  /* Contour_CornerCell  */
    END_ContourType_e,
    ContourType_Invalid = BadEnumValue
  } ContourType_e;

typedef enum
  {
    ContourColoring_RGB,
    ContourColoring_Group1,
    ContourColoring_Group2,
    ContourColoring_Group3,
    ContourColoring_Group4,
    END_ContourColoring_e,
    ContourColoring_Invalid = BadEnumValue
  } ContourColoring_e;

#if defined EXPORT_DEPRECATED_INTERFACES_TO_ADK_ONLY
/**
 * @deprecated
 *     Please use \ref VectorType_e instead.
 */
typedef enum
  {
    Vector_TailAtPoint, /* deprecated: use VectorType_TailAtPoint */
    Vector_HeadAtPoint, /* deprecated: use VectorType_HeadAtPoint */
    Vector_MidAtPoint,  /* deprecated: use VectorType_MidAtPoint  */
    Vector_HeadOnly,    /* deprecated: use VectorType_HeadOnly    */
    END_VectorPlotType_e,
    Vector_Invalid = BadEnumValue
  } VectorPlotType_e;
#endif


typedef enum
  {
    VectorType_TailAtPoint, /* Vector_TailAtPoint */
    VectorType_HeadAtPoint, /* Vector_HeadAtPoint */
    VectorType_MidAtPoint,  /* Vector_MidAtPoint  */
    VectorType_HeadOnly,    /* Vector_HeadOnly    */
    END_VectorType_e,
    VectorType_Invalid = BadEnumValue
  } VectorType_e;


/*
 * NOTE: ShadePlotType_e is deprecated.  It must be retained to maintain
 *       backward compatibility with the TecUtil layer however.
 *       This has been replaced by LightingEffect_e.
 */
typedef enum
  {
    Shade_SolidColor,
    Shade_Paneled,
    Shade_Gouraud,
    Shade_ColoredPaneled,
    Shade_ColoredGouraud,
    END_ShadePlotType_e,
    Shade_Invalid = BadEnumValue
  } ShadePlotType_e;

/*
 * NOTE: LightingEffect_None is Deprecated.  It must remain
 *       in the list to allow macro processing of older 
 *       (i.e. early v9) macros.
 */
typedef enum
  {
    LightingEffect_Paneled,
    LightingEffect_Gouraud,
    LightingEffect_None,
    END_LightingEffect_e,
    LightingEffect_Invalid = BadEnumValue
  } LightingEffect_e;

typedef enum
  {
    Lines_I,
    Lines_J,
    Lines_K,
    END_IJKLines_e,
    Lines_Invalid = BadEnumValue
  } IJKLines_e;

typedef enum
  {
    IJKCellType_Planes,
    IJKCellType_FacePlanes,
    IJKCellType_Volume,
    END_IJKCellType_e,
    IJKCellType_Invalid = BadEnumValue
  } IJKCellType_e;


/*
 *  Ver 6 used PlaneSet.  Ver 7 uses CellType and Planes variables.
 *
 *   "PlaneSet" in version 6    vs.  IJKPlanes in v7:
 *
 *   'A' = AllPlanes                 CellType = IJKCellType_Volume
 *   'd','e','f','C' = ComboPlanes   CellType = IJKCellType_Planes, IJKPlanes = depends on defC
 *   'F' = Faces Planes Only         CellType = IJKCellType_FacePlanes
 *   'I' = I-Planes                  CellType = IJKCellType_Planes, IJKPlanes = Planes_I
 *   'J' = J-Planes                  CellType = IJKCellType_Planes, IJKPlanes = Planes_J
 *   'K' = K-Planes                  CellType = IJKCellType_Planes, IJKPlanes = Planes_K
 *
 *
 * NOTE: IJKPlanes_e is still used internally in tecplot (and in the TecUtil layer).
 *       it has been relagated to communicating which planes of an IJK zone are in
 *       use.  
 *
 */

typedef enum
  {
    Planes_I,
    Planes_J,
    Planes_K,
    Planes_IJ,   /* deprecated */
    Planes_JK,   /* deprecated */
    Planes_IK,   /* deprecated */
    Planes_IJK,  /* deprecated */
    Planes_Face, /* used on the panel heap */
    Planes_Volume,
    Planes_Unused,
    END_IJKPlanes_e,
    Planes_Invalid = BadEnumValue
  } IJKPlanes_e;



typedef enum
  {
    SurfacesToPlot_BoundaryFaces,
    SurfacesToPlot_ExposedCellFaces,
    SurfacesToPlot_IPlanes,
    SurfacesToPlot_JPlanes,
    SurfacesToPlot_KPlanes,
    SurfacesToPlot_IJPlanes,
    SurfacesToPlot_JKPlanes,
    SurfacesToPlot_IKPlanes,
    SurfacesToPlot_IJKPlanes,
    SurfacesToPlot_All,
    END_SurfacesToPlot_e,
    SurfacesToPlot_Invalid = BadEnumValue
  } SurfacesToPlot_e;

typedef enum
  {
    PointsToPlot_SurfaceNodes,  /* was _SurfacesOnly */
    PointsToPlot_AllNodes,      /* was _All          */
    PointsToPlot_SurfaceCellCenters,
    PointsToPlot_AllCellCenters,
    PointsToPlot_AllConnected,
#if defined EXPORT_DEPRECATED_INTERFACES_TO_ADK_ONLY
    PointsToPlot_SurfacesOnly = PointsToPlot_SurfaceNodes, /* deprecated */
    PointsToPlot_All          = PointsToPlot_AllNodes,     /* deprecated */
#endif
    END_PointsToPlot_e,
    PointsToPlot_Invalid = BadEnumValue
  } PointsToPlot_e;


typedef enum
{
  SliceSurface_XPlanes,
  SliceSurface_YPlanes,
  SliceSurface_ZPlanes,
  SliceSurface_IPlanes,
  SliceSurface_JPlanes,
  SliceSurface_KPlanes,
  END_SliceSurface_e,
  SliceSurface_Invalid = BadEnumValue
} SliceSurface_e;


typedef enum
  {
    Skip_ByIndex,
    Skip_ByFrameUnits,
    END_SkipMode_e,
    Skip_Invalid = BadEnumValue
  } SkipMode_e;



#if defined EXPORT_DEPRECATED_INTERFACES_TO_ADK_ONLY
/**
 * @deprecated
 *     Please use \ref BoundaryType_e instead.
 */
typedef enum
  {
    Boundary_None, /* deprecated: use BoundaryType_None */
    Boundary_Min,  /* deprecated: use BoundaryType_Min  */
    Boundary_Max,  /* deprecated: use BoundaryType_Max  */
    Boundary_Both, /* deprecated: use BoundaryType_Both */
    END_BoundPlotType_e,
    Boundary_Invalid = BadEnumValue
  } BoundPlotType_e;
#endif

typedef enum
  {
    BoundaryType_None, /* Boundary_None */
    BoundaryType_Min,  /* Boundary_Min  */
    BoundaryType_Max,  /* Boundary_Max  */
    BoundaryType_Both, /* Boundary_Both */
    END_BoundaryType_e,
    BoundaryType_Invalid = BadEnumValue
  } BoundaryType_e;



typedef enum
  {
    ColorMap_SmRainbow,
    ColorMap_LgRainbow,
    ColorMap_Modern,
    ColorMap_GrayScale,
    ColorMap_Wild,
    ColorMap_UserDef,
    ColorMap_TwoColor,
    ColorMap_RawUserDef,
    END_ContourColorMap_e,
    ColorMap_Invalid = BadEnumValue
  } ContourColorMap_e;



typedef enum
  {
    ErrorBar_Up,
    ErrorBar_Down,
    ErrorBar_Left,
    ErrorBar_Right,
    ErrorBar_Horz,
    ErrorBar_Vert,
    ErrorBar_Cross,
    END_ErrorBar_e,
    ErrorBar_Invalid = BadEnumValue
  } ErrorBar_e;



typedef enum
  {
    ContourLineMode_UseZoneLineType,
    ContourLineMode_SkipToSolid,
    ContourLineMode_DashNegative,
    END_ContourLineMode_e,
    ContourLineMode_Invalid = BadEnumValue
  } ContourLineMode_e;


/* CORE SOURCE CODE REMOVED */


typedef enum
  {
    MessageBox_Error,
    MessageBox_Warning,
    MessageBox_Information,
    MessageBox_Question,   /* Ok, Cancel buttons */
    MessageBox_YesNo,
    MessageBox_YesNoCancel,
    MessageBox_WarningOkCancel,
    END_MessageBoxType_e,
    MessageBox_Invalid = BadEnumValue
  } MessageBoxType_e;


/* CORE SOURCE CODE REMOVED */

typedef enum
  {
    NumberFormat_Integer,
    NumberFormat_FixedFloat,
    NumberFormat_Exponential,
    NumberFormat_BestFloat,
    NumberFormat_SuperScript,
    NumberFormat_CustomLabel,
    NumberFormat_LogSuperScript,
    NumberFormat_RangeBestFloat,
    END_NumberFormat_e,
    NumberFormat_Invalid = BadEnumValue
  } NumberFormat_e;

/* For backward compatibility with v9- */
typedef NumberFormat_e ValueFormat_e;


typedef enum
  {
    BackingStoreMode_QuickAndDirty,
    BackingStoreMode_RealTimeUpdate,
    BackingStoreMode_PeriodicUpdate,
    END_BackingStoreMode_e,
    BackingStoreMode_Invalid = BadEnumValue
  } BackingStoreMode_e;


typedef enum
  {
    TickDirection_In,
    TickDirection_Out,
    TickDirection_Centered,
    END_TickDirection_e,
    TickDirection_Invalid = BadEnumValue
  } TickDirection_e;

/* This enumerated type is no longer used as of Tecplot V10. */
typedef enum
  {
    AxisTitlePosition_Left,
    AxisTitlePosition_Center,
    AxisTitlePosition_Right,
    END_AxisTitlePosition_e,
    AxisTitlePosition_Invalid = BadEnumValue
  } AxisTitlePosition_e;

typedef enum
  {
    AxisTitleMode_NoTitle,
    AxisTitleMode_UseVarName,
    AxisTitleMode_UseText,
    END_AxisTitleMode_e,
    AxisTitleMode_Invalid = BadEnumValue
  } AxisTitleMode_e;

typedef enum
  {
    AxisAlignment_WithViewport,
    AxisAlignment_WithOpposingAxisValue,
    AxisAlignment_WithGridMin,
    AxisAlignment_WithGridMax,
    AxisAlignment_WithSpecificAngle,
    AxisAlignment_WithGridAreaTop,
    AxisAlignment_WithGridAreaBottom,
    AxisAlignment_WithGridAreaLeft,
    AxisAlignment_WithGridAreaRight,
    END_AxisAlignment_e,
    AxisAlignment_Invalid = BadEnumValue
  } AxisAlignment_e;

typedef enum
  {
    FunctionDependency_XIndependent,
    FunctionDependency_YIndependent,
    END_FunctionDependency_e,
    FunctionDependency_Invalid = BadEnumValue,
    FunctionDependency_ThetaIndependent = FunctionDependency_XIndependent,
    FunctionDependency_RIndependent = FunctionDependency_YIndependent
  } FunctionDependency_e;

typedef enum
  {
    LegendShow_Yes,
    LegendShow_No,
    LegendShow_Auto,
    END_LegendShow_e,
    LegendShow_Invalid = BadEnumValue
  } LegendShow_e;

typedef enum
  {
    LineMapSort_None,
    LineMapSort_IndependentVar,
    LineMapSort_DependentVar,
    LineMapSort_SpecificVar,
    END_LineMapSort_e,
    LineMapSort_Invalid = BadEnumValue
  } LineMapSort_e;

typedef enum
  {
    ContLegendLabelLocation_ContourLevels,
    ContLegendLabelLocation_Increment,
    ContLegendLabelLocation_ColorMapDivisions,
    END_ContLegendLabelLocation_e,
    ContLegendLabelLocation_Invalid = BadEnumValue
  } ContLegendLabelLocation_e;

typedef enum
  {
    ThetaMode_Degrees,
    ThetaMode_Radians,
    ThetaMode_Arbitrary,
    END_ThetaMode_e,
    ThetaMode_Invalid = BadEnumValue
  } ThetaMode_e;

typedef enum
  {
    Transform_PolarToRect,
    Transform_SphericalToRect,
    Transform_RectToPolar,
    Transform_RectToSpherical,
    END_Transform_e,
    Transform_Invalid = BadEnumValue
  } Transform_e;
  
typedef enum
  {
    LaunchDialogMode_ModalSync,
    LaunchDialogMode_Modeless,
    LaunchDialogMode_ModalAsync,
    END_LaunchDialogMode_e,
    LaunchDialogMode_Invalid = BadEnumValue
  } LaunchDialogMode_e;


typedef enum
  {
    SelectFileOption_ReadSingleFile,
    SelectFileOption_ReadMultiFile,
    SelectFileOption_AllowMultiFileRead,
    SelectFileOption_WriteFile,
    SelectFileOption_SelectDirectory,
    END_SelectFileOption_e,
    SelectFileOption_Invalid = BadEnumValue
  } SelectFileOption_e;


/*   CURRENTLY NOT USED .... */
typedef enum
  {
    ViewActionDrawMode_NoDraw,
    ViewActionDrawMode_DrawTrace,
    ViewActionDrawMode_DrawFull,
    END_ViewActionDrawMode_e,
    ViewActionDrawMode_Invalid = BadEnumValue
  } ViewActionDrawMode_e;


typedef enum
  {
    FrameAction_PushTop,
    FrameAction_Pop,
    FrameAction_PopAtPosition,
    FrameAction_DeleteTop,
    FrameAction_FitAllToPaper,
    FrameAction_PushByName,
    FrameAction_PopByName,
    FrameAction_Push,
    END_FrameAction_e,
    FrameAction_Invalid = BadEnumValue
  } FrameAction_e;

typedef enum
  {
    DoubleBufferAction_On,
    DoubleBufferAction_Off,
    DoubleBufferAction_Swap,
    END_DoubleBufferAction_e,
    DoubleBufferAction_Invalid = BadEnumValue
  } DoubleBufferAction_e;

typedef enum
  {
    PickAction_CheckToAdd,
    PickAction_AddAll,
    PickAction_AddAllInRegion,
    PickAction_Edit,
    PickAction_Cut,
    PickAction_Copy,
    PickAction_Clear,
    PickAction_Paste,
    PickAction_PasteAtPosition,
    PickAction_Shift,
    PickAction_Magnify,
    PickAction_Push,
    PickAction_Pop,
    PickAction_SetMouseMode,
    PickAction_DeselectAll,
    PickAction_AddZones,
    PickAction_AddXYMaps, /* deprecated: use PickAction_AddLineMaps */
    PickAction_AddLineMaps,
    END_PickAction_e,
    PickAction_Invalid = BadEnumValue
  } PickAction_e;


typedef enum
  {
    ContourLevelAction_Add,
    ContourLevelAction_New,
    ContourLevelAction_DeleteRange,
    ContourLevelAction_Reset,
    ContourLevelAction_ResetToNice,
    ContourLevelAction_DeleteNearest,
    END_ContourLevelAction_e,
    ContourLevelAction_Invalid = BadEnumValue
  } ContourLevelAction_e;

typedef enum
  {
    ContourLabelAction_Add,
    ContourLabelAction_DeleteAll,
    END_ContourLabelAction_e,
    ContourLabelAction_Invalid = BadEnumValue
  } ContourLabelAction_e;

typedef enum
  {
    StreamtraceAction_Add,
    StreamtraceAction_DeleteAll,
    StreamtraceAction_DeleteRange,
    StreamtraceAction_SetTerminationLine,
    StreamtraceAction_ResetDeltaTime,
    END_StreamtraceAction_e,
    StreamtraceAction_Invalid = BadEnumValue
  } StreamtraceAction_e;

typedef enum
  {
    ColorMapControlAction_RedistributeControlPoints,
    ColorMapControlAction_CopyCannedColorMap,
    ColorMapControlAction_ResetToFactoryDefaults,
    END_ColorMapControlAction_e,
    ColorMapControlAction_Invalid = BadEnumValue
  } ColorMapControlAction_e;

typedef enum
  {
    ColorMapDistribution_Continuous,
    ColorMapDistribution_Banded,
    END_ColorMapDistribution_e,
    ColorMapDistribution_Invalid = BadEnumValue
  } ColorMapDistribution_e;

typedef enum
  {
    RGBMode_SpecifyRGB,
    RGBMode_SpecifyRG,
    RGBMode_SpecifyRB,
    RGBMode_SpecifyGB,
    END_RGBMode_e,
    RGBMode_Invalid = BadEnumValue
  } RGBMode_e;

typedef enum
  {
    TecUtilErr_None,
    TecUtilErr_Undetermined,
    END_TecUtilErr_e,
    TecUtilErr_Invalid = BadEnumValue
  } TecUtilErr_e;

/* CORE SOURCE CODE REMOVED */

typedef enum /* Custom exporter error message */
{
  ExportCustReturnCode_Ok,
  ExportCustReturnCode_Failed,
  ExportCustReturnCode_TecplotLocked,
  ExportCustReturnCode_ExporterNotLoaded,     
  ExportCustReturnCode_ExportCallbackFailed,          
  ExportCustReturnCode_NotAnImageExporter,    
  ExportCustReturnCode_NotAFieldDataExporter, 
  END_ExportCustReturnCode_e,
  ExportCustReturnCode_Invalid = BadEnumValue
} ExportCustReturnCode_e;

/**
 * COB/Zone types.
 */
typedef enum
  {
    CZType_FieldDataZone,
    CZType_FEBoundaryCOB,
    CZType_IsoSurfaceCOB,
    CZType_SliceCOB,
    CZType_StreamtraceCOB,
    CZType_StreamtraceMarkerCOB,
    CZType_StreamtraceArrowheadCOB,
    END_CZType_e,
    CZType_Invalid = BadEnumValue
  } CZType_e;

/**
 */
typedef enum
  {
    FaceNeighborMode_LocalOneToOne,
    FaceNeighborMode_LocalOneToMany,
    FaceNeighborMode_GlobalOneToOne,
    FaceNeighborMode_GlobalOneToMany,
    END_FaceNeighborMode_e,
    FaceNeighborMode_Invalid = BadEnumValue
  } FaceNeighborMode_e;

/* CORE SOURCE CODE REMOVED */




/****************************************************************
 *                                                              *
 *                     STRUCTURE TYPEDEFS                       *
 *                                                              *
 ****************************************************************/

typedef struct _StringList_s *StringList_pa;
typedef struct _Menu_s       *Menu_pa;
/* CORE SOURCE CODE REMOVED */

typedef enum
  {
    ImageResizeFilter_Texture,
    ImageResizeFilter_Box,
    ImageResizeFilter_Lanczos2,
    ImageResizeFilter_Lanczos3,
    ImageResizeFilter_Triangle,
    ImageResizeFilter_Bell,
    ImageResizeFilter_BSpline,
    ImageResizeFilter_Cubic,
    ImageResizeFilter_Mitchell,
    ImageResizeFilter_Gaussian,
    END_ImageResizeFilter_e,
    ImageResizeFilter_Invalid = BadEnumValue
  } ImageResizeFilter_e;

/* CORE SOURCE CODE REMOVED */

typedef struct _Set_a *Set_pa;

/* CORE SOURCE CODE REMOVED */

typedef struct _AddOnList_a *AddOn_pa;

typedef struct _NodeMap_a *NodeMap_pa;

/* CORE SOURCE CODE REMOVED */


/* used to indicate that no neighboring element or zone exists */
#define NO_NEIGHBORING_ELEMENT (-1)
#define NO_NEIGHBORING_ZONE    (-1)

typedef struct _FaceNeighbor_a *FaceNeighbor_pa;


/* CORE SOURCE CODE REMOVED */

typedef struct _FieldData_a *FieldData_pa;

/**
 */
typedef struct _AuxData_s  *AuxData_pa;


/* CORE SOURCE CODE REMOVED */


/**
 * This function is called when the user activates a menu item
 * added via TecUtilMenuInsertOption or TecUtilMenuInsertToggle.
 *
 * @param ClientData
 *   Arbitrary client data.
 *
 * <FortranSyntax>
 *    SUBROUTINE MenuActivateCallback(
 *   &                     ClientDataPtr)
 *    POINTER (ClientDataPtr,DummyClientData)
 * </FortranSyntax>
 */
typedef void (STDCALL *MenuActivateCallback_pf)(ArbParam_t ClientData);

/**
 * This function is called when the a menu is deleted.
 *
 * @param ClientData
 *   Arbitrary client data.
 *
 * <FortranSyntax>
 *    SUBROUTINE MenuDeleteCallback(
 *   &                     ClientDataPtr)
 *    POINTER (ClientDataPtr,DummyClientData)
 * </FortranSyntax>
 */
typedef void (STDCALL *MenuDeleteCallback_pf)(ArbParam_t ClientData);

/**
 * This function is called to determine the sensitivity for a menu item (option,
 * toggle or submenu).
 *
 * @param ClientData
 *   Arbitrary client data.
 *
 * @return
 *   Return TRUE if the menu item should be sensitive to user input,
 *   or FALSE if it should be insensitive to user input (gray).
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION MenuGetSensitivityCallback(
 *   &                     ClientDataPtr)
 *    POINTER (ClientDataPtr,DummyClientData)
 * </FortranSyntax>
 */
typedef Boolean_t (STDCALL *MenuGetSensitivityCallback_pf)(ArbParam_t ClientData);

/**
 * This function is called to determine the checked state for a toggle menu item.
 *
 * @param ClientData
 *   Arbitrary client data.
 *
 * @return
 *   Return TRUE if the toggle should be checked,
 *   or FALSE if it should be unchecked.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION MenuGetToggleStateCallback(
 *   &                     ClientDataPtr)
 *    POINTER (ClientDataPtr,DummyClientData)
 * </FortranSyntax>
 */
typedef Boolean_t (STDCALL *MenuGetToggleStateCallback_pf)(ArbParam_t ClientData);


/**
 * This function is called when the user performs a probe event.
 *
 * @param IsNearestPoint
 *   This is TRUE if the previous probe event was a nearest point probe.
 *   This is FALSE if it was an interpolated probe.
 *
 * <FortranSyntax>
 *   SUBROUTINE MyProbeDestinationCallback(
 *              IsNearestPoint)
 *   INTEGER*4 IsNearestPoint
 * </FortranSyntax>
 */
typedef void (STDCALL *ProbeDestination_pf)(Boolean_t IsNearestPoint);


/**
 *  DynamicMenu Functions are called upon a user selecting
 *  a menu item added via TecUtilMenuAddOption.
 *
 * <FortranSyntax>
 *   SUBROUTINE MyDynamicMenuCallback()
 * </FortranSyntax>
 */
typedef void (STDCALL *DynamicMenuCallback_pf)(void);


/**
 * Compares two strings from a list string. Note that either string may be
 * NULL as StringLists allow for NULL elements.
 *
 * @param String1
 *     String to compare against String2.
 * @param String2
 *     String to compare against String1.
 * @param ClientData
 *     Contextual information that was passed to the 'StringListSort' function.
 *
 * @return
 *     - A value less than zero if String1 is less than String2.
 *     - A value of zero if String1 is equal to String2.
 *     - A value greater than zero if String1 is greater than String2.
 */
typedef int (*StringListStringComparator_pf)(const char *String1,
                                             const char *String2,
                                             ArbParam_t  ClientData);

/**
 * Gets a value from a field data handle.
 *
 * @param FD
 *     Field data handle for which to set the value.  This FieldValueGetFunction_pf
 *     must have been retrieved from this field data handle via TecUtilDataValueRefGetGetFunc.
 *
 * @param pt
 *     Zero-based index into the field data.
 *
 * @return
 *     Value for that index, always passed as a double precision floating-point
 *     value regardless of the data type of the field data handle.
 */
typedef double (STDCALL *FieldValueGetFunction_pf)(const FieldData_pa FD,
                                                   LgIndex_t          pt);

/**
 * Sets a value in a field data handle.
 *
 * @param FD
 *     Field data handle for which to set the value.  This FieldValueSetFunction_pf
 *     must have been retrieved from this field data handle via TecUtilDataValueRefGetSetFunc.
 *
 * @param pt
 *     Zero-based index into the field data.
 *
 * @param val
 *     New value for that index, always passed as a double precision floating-point
 *     value regardless of the data type of the field data handle.
 *
 * @return
 *     None.
 */
typedef void (STDCALL *FieldValueSetFunction_pf)(FieldData_pa FD,
                                                 LgIndex_t    pt,
                                                 double       val);

/**
 *  ExtractDestination functions are called upon successful
 *  completion of an extract polyline or extract discrete points
 *  operation.
 *
 * @param NumPts
 *    Number of points extracted.
 *
 * @param XValues
 *    Double precision array of X-Coordinates in the extracted polyline.
 *
 * @param YValues
 *    Double precision array of Y-Coordinates in the extracted polyline.
 *
 * <FortranSyntax>
 *   INTEGER*4 FUNCTION MyExtractDestinationCallback(
 *  &                   NumPts,
 *  &                   XValues,
 *  &                   YValues)
 *   INTEGER*4 NumPts
 *   REAL*8    XValues
 *   REAL*8    YValues
 * </FortranSyntax>
 */
typedef void (STDCALL *ExtractDestination_pf)(LgIndex_t NumPts,
                                              double   *XValues,
                                              double   *YValues);



/**
 * SelectFileOptionsCallback Functions are called when the
 * "Options" button is pressed in the modal file selection
 * dialog.
 *
 * <FortranSyntax>
 *   SUBROUTINE MySelectFileOptionsCallback()
 * </FortranSyntax>
 */
typedef void (STDCALL *SelectFileOptionsCallback_pf)(void);




/**
 * Post data load instruction callback for "Converter-Plus" addons.
 *
 * @param PreviousInstructions
 *   The previous set of instructions used by the converter.
 *
 * @param PreviousRawData
 *   The previous raw data associated with the instructions.
 *
 * @param PreviousZones
 *   Set of zones loaded with the previous instructions.
 *
 * <FortranSyntax>
 *    SUBROUTINE MyConverterPostReadCallback(
 *   &                   PreviousInstructions,
 *   &                   PreviousRawData,
 *   &                   PreviousZones)
 *    CHARACTER*(*)   CommandString
 *    CHARACTER*(*)   ErrMsgString
 *    POINTER         (PreviousZones,DummyPreviousZonesData)
 * </FortranSyntax>
 *
 */
typedef void (STDCALL *ConverterPostReadCallback_pf)(const char   *PreviousInstructions,
                                                     const char   *PreviousRawData,
                                                     const Set_pa PreviousZones);


/**
 * Callback registered by your addon to convert a foreign datafile into a 
 * Tecplot Binary datafile format.
 *
 * @return
 *   Return TRUE if the conversion is successful. Otherwise return FALSE.
 *   If FALSE is returned then MessageString is assumed to contain an error 
 *   message.
 *
 * @param DataFName
 *   Name of the original foreign data file to be converted.
 *
 * @param TempBinFName
 *   Name of the temporary binary datafile that is created (by your converter).
 *
 * @param MessageString
 *   Reference to a string.  If an error occurs during conversion then allocate 
 *   space for an error message and put that message in MessageString.
 *
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION MyDataSetConverterCallback(
 *   &                   DataFName,
 *   &                   TempBinFName,
 *   &                   MessageString)
 *    CHARACTER*(*)   DataFName
 *    CHARACTER*(*)   TempBinFName
 *    CHARACTER*(*)   MessageString
 * </FortranSyntax>
 *
 */
typedef Boolean_t (STDCALL *DataSetConverter_pf)(char  *DataFName,
                                                 char  *TempBinFName,
                                                 char **MessageString);







/**
 * Callback registered by your addon to process foreign loader instructions.  
 * When called, it must parse the supplied instructions and load the data into Tecplot.
 *
 * @return
 *   Return TRUE if the data is loaded successfully. Otherwise, FALSE.
 *
 * @param Instructions
 *   This contains all of the instructions needed to load the data.
 *
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION MyDataSetLoaderCallback(
 *   &                   Instructions)
 *    POINTER        (Instructions,DummyInstructionsData)
 * </FortranSyntax>
 */
typedef Boolean_t (STDCALL *DataSetLoader_pf)(StringList_pa Instructions);





/**
 * Callback used to provide the ability to override data loader instructions
 * while processing a layout.
 *
 * @return
 *   Return TRUE if the instructions are successfully replaced or left alone. 
 *   Return FALSE if the user cancels the operation.
 *
 * @param Instructions
 *   The original instructions needed to load the data.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION MyDataSetLoaderInstOverCallback(
 *   &                   Instructions)
 *    POINTER        (Instructions,DummyInstructionsData)
 * </FortranSyntax>
 *
 */
typedef Boolean_t (STDCALL *DataSetLoaderInstructionOverride_pf)(StringList_pa  Instructions);



/**
 *  Callback used to assign extended curve settings.  
 *  This is called when the user presses the "Curve Settings" 
 *  button in the mapping style dialog.
 *
 *  @param LineMapSet
 *    Set of line maps currently selected.
 *
 *  @param
 *    A string list of the curve settings for the Line-maps that are selected in the 
 *    Line mappings dialog.
 *
 * <FortranSyntax>
 *   SUBROUTINE MyGetCurveSettingsCallback(
 *  &                LineMapSet,
 *  &                SelectedLineMapSettings)
 *    POINTER    (LineMapSet,DummyLineMapData)
 *    POINTER    (SelectedLineMapSettings,DummyLineMapSettings)
 * </FortranSyntax>
 */
typedef void (STDCALL *GetCurveSettingsCallback_pf) (Set_pa        LineMapSet,
                                                     StringList_pa SelectedLineMapSettings);




/**
 * Callback function that returns an abbreviated version of the curve settings 
 * for a particular Line Map for display in the Line Mappings dialog.
 *
 * @param LineMap
 *   The map number that is currently being operated on.
 * @param CurveSettings
 *   The string that Tecplot maintains which contains the extended curve fit settings
 *   for the current Line-map.
 * @param AbbreviatedSettings
 *   The short form of the CurveSettings that are passed into your function by
 *   Tecplot. This must be allocated by the addon using TecUtilStringAlloc().
 *
 * <FortranSyntax>
 *   SUBROUTINE MyGetAbrevSettingsStringCallback(
 *  &                LineMap,
 *  &                CurveSettings,
 *  &                AbbreviatedSettings),
 *    INTEGER*4  LineMap
 *    CHARACTER*(*) CurveSettings
 *    CHARACTER*(*) AbbreviatedSettings
 * </FortranSyntax>
 */
typedef void (STDCALL *GetAbbreviatedSettingsStringCallback_pf) (EntIndex_t  LineMap,
                                                                 char       *CurveSettings,
                                                                 char      **AbbreviatedSettings);




/**
 * This function returns a string (CurveInfoString) for Tecplot to display 
 * information about a particular curve in the curve info dialog.
 *
 * @return
 *   Return TRUE if the curve info string can be generated, otherwise FALSE.
 *
 * @param RawIndV
 *   The handle to the raw field data of the independent variable.
 * @param RawDepV
 *   The handle to the raw field data of the dependent variable.
 * @param IndVCoordScale
 *   An enumerated variable whose values are Scale_linear when the independent variable
 *   axis has a linear scale and Scale_log when it has a log scale.
 * @param DepVCoordScale
 *   An enumerated variable whose values are Scale_linear when the dependent variable axis
 *   has a linear scale and Scale_log when it has a log scale.
 * @param NumRawPts
 *   number of raw field data values.
 * @param LineMap
 *   The map number that is currently being operated on.
 * @param CurveSettings
 *   The curve settings string for the current Line-map.
 * @param CurveInfoString
 *   The string that is to be presented in the Data/XY-Plot Curve Info dialog. 
 *   The CurveInfoString must be allocated by the addon using
 *   TecUtilStringAlloc().
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION MyGetCurveInfoStringCallback(
 *   &                   RawIndV,
 *   &                   RawDepV,
 *   &                   IndVCoordScale,
 *   &                   DepVCoordScale,
 *   &                   NumRawPts,
 *   &                   LineMap,
 *   &                   CurveSettings,
 *   &                   CurveInfoString)
 *    POINTER       (RawIndV,DummyRawIndVData)
 *    POINTER       (RawDepV,DummyRawDepVData)
 *    INTEGER*4     IndVCoordScale
 *    INTEGER*4     DepVCoordScale
 *    INTEGER*4     NumRawPts
 *    INTEGER*4     LineMap
 *    CHARACTER*(*) CurveSettings
 *    CHARACTER*(*) CurveInfoString
 * </FortranSyntax>
 */
typedef Boolean_t (STDCALL *GetCurveInfoStringCallback_pf) (FieldData_pa RawIndV,
                                                            FieldData_pa RawDepV,
                                                            CoordScale_e IndVCoordScale,
                                                            CoordScale_e DepVCoordScale,
                                                            LgIndex_t    NumRawPts,
                                                            EntIndex_t   LineMap,
                                                            char        *CurveSettings,
                                                            char       **CurveInfoString);

/**
 * Callback function used to calculate data points for an extended curve fit.
 *
 * @return
 *   Return TRUE if the curve can be calculated, otherwise FALSE.
 *
 * @param RawIndV
 *   The handle to the raw field data of the independent variable.
 * @param RawDepV
 *   The handle to the raw field data of the dependent variable.
 * @param IndVCoordScale
 *   An enumerated variable whose values are Scale_linear when the independent variable
 *   axis has a linear scale and Scale_log when it has a log scale.
 * @param DepVCoordScale
 *   An enumerated variable whose values are Scale_linear when the dependent variable axis
 *   has a linear scale and Scale_log when it has a log scale.
 * @param NumRawPts
 *   number of raw field data values.
 * @param NumCurvePts
 *   The number of points that will construct the curve fit.
 * @param LineMap
 *   The line map to operated on.
 * @param CurveSettings
 *   The curve settings string for the current Line-map.
 * @param IndCurveValues
 *   A pre-allocated array of size NumCurvePts which the addon will populate with 
 *   the independent values for the curve fit
 * @param DepCurveValues.
 *   A pre-allocated array of size NumCurvePts which the add-on will populate 
 *   with the dependent values for the curve fit.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION MyGetLinePlotDataPointsCallback(
 *   &                   RawIndV,
 *   &                   RawDepV,
 *   &                   IndVCoordScale,
 *   &                   DepVCoordScale,
 *   &                   NumRawPts,
 *   &                   NumCurvePts,
 *   &                   LineMap,
 *   &                   CurveSettings,
 *   &                   IndCurveValues,
 *   &                   DepCurveValues)
 *    POINTER       (RawIndV,DummyRawIndVData)
 *    POINTER       (RawDepV,DummyRawDepVData)
 *    INTEGER*4     IndVCoordScale
 *    INTEGER*4     DepVCoordScale
 *    INTEGER*4     NumRawPts
 *    INTEGER*4     NumCurvePts
 *    INTEGER*4     LineMap
 *    CHARACTER*(*) CurveSettings
 *    REAL*8        IndCurveValues()
 *    REAL*8        DepCurveValues()
 * </FortranSyntax>
 */
typedef Boolean_t (STDCALL *GetLinePlotDataPointsCallback_pf) (FieldData_pa RawIndV,
                                                               FieldData_pa RawDepV,
                                                               CoordScale_e IndVCoordScale,
                                                               CoordScale_e DepVCoordScale,
                                                               LgIndex_t    NumRawPts,
                                                               LgIndex_t    NumCurvePts,
                                                               EntIndex_t   LineMap,
                                                               char        *CurveSettings,
                                                               double      *IndCurveValues,
                                                               double      *DepCurveValues);
#if defined EXPORT_DEPRECATED_INTERFACES_TO_ADK_ONLY
/**
 * @deprecated
 *     Please use \ref GetLinePlotDataPointsCallback_pf instead.
 */
typedef GetLinePlotDataPointsCallback_pf GetXYDataPointsCallback_pf;
#endif




/**
 * A Callback function used to obtain an interpolated dependent value for an 
 * extended curve fit given an independent value.
 *
 * @return
 *   Return TRUE if it is possible to obtain the interpolated value, otherwise FALSE.
 *
 * @param RawIndV
 *   handle to the raw field data of the independent variable.
 * @param RawDepV
 *   The handle to the raw field data of the dependent variable.
 * @param IndVCoordScale
 *   An enumerated variable whose values are Scale_linear when the independent variable
 *   axis has a linear scale and Scale_log when it has a log scale.
 * @param DepVCoordScale
 *   An enumerated variable whose values are Scale_linear when the dependent variable axis
 *   has a linear scale and Scale_log when it has a log scale.
 * @param NumRawPts
 *   The number of field data values.
 * @param NumCurvePts
 *   The number of points used to construct the curve fit.
 * @param LineMapNum
 *   The line map number currently being operated on.
 * @param CurveSettings
 *   The curve settings string for the current Line-map.
 * @param ProbeIndValue
 *   The independent value location of the probe (supplied).
 * @param ProbeDepValue
 *   Reference to the the calculated dependent value location of the probe.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION MyGetProbeValueCallback(
 *   &                   RawIndV,
 *   &                   RawDepV,
 *   &                   IndVCoordScale,
 *   &                   DepVCoordScale,
 *   &                   NumRawPts,
 *   &                   NumCurvePts,
 *   &                   LineMapNum,
 *   &                   CurveSettings,
 *   &                   CurveInfoString,
 *   &                   ProbeIndValue,
 *   &                   ProbeDepValue)
 *    POINTER       (RawIndV,DummyRawIndVData)
 *    POINTER       (RawDepV,DummyRawDepVData)
 *    INTEGER*4     IndVCoordScale
 *    INTEGER*4     DepVCoordScale
 *    INTEGER*4     NumRawPts
 *    INTEGER*4     NumCurvePts
 *    INTEGER*4     LineMapNum
 *    CHARACTER*(*) CurveSettings
 *    REAL*8        ProbeIndValue
 *    REAL*8        ProbeDepValue
 * </FortranSyntax>
 *
 */
typedef Boolean_t (STDCALL *GetProbeValueCallback_pf) (FieldData_pa RawIndV,
                                                       FieldData_pa RawDepV,
                                                       CoordScale_e IndVCoordScale,
                                                       CoordScale_e DepVCoordScale,
                                                       LgIndex_t    NumRawPts,
                                                       LgIndex_t    NumCurvePts,
                                                       EntIndex_t   LineMapNum,
                                                       char        *CurveSettings,
                                                       double       ProbeIndValue,
                                                       double      *ProbeDepValue);



#if defined MSWIN
typedef Boolean_t (STDCALL *PreTranslateMessage_pf)(MSG *pMsg);
#endif

/*********************************************************
 * Add-on Timers
 *********************************************************/
/**
 * This is called when a registered timer fires.
 *
 * @par Limitation:
 *   Unix and Linux versions of Tecplot currently do not fire timer events when
 *   Tecplot is running in batch mode (with the -b flag). This behavior
 *   limitation is subject to change.
 *
 * @param ClientData
 *   Arbitrary client data.
 *
 * @return
 *   Return TRUE if the timer should be reinstated.   Return FALSE
 *   to stop subsequent callbacks.
 *
 * 
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION MyAddOnTimerCallback(
 *   &                     ClientDataPtr)
 *    POINTER (ClientDataPtr,DummyClientData)
 * </FortranSyntax>
 */
typedef Boolean_t (STDCALL *AddOnTimerCallback_pf) (ArbParam_t ClientData);

/**
 * This is called when Tecplot is idle.
 *
 * @par Note:
 *   Tecplot is never idle when running in batch mode (with the -b flag).
 *
 * @param ClientData
 *   Arbitrary client data.
 *
 * <FortranSyntax>
 *    INTEGER*4 FUNCTION MyOnIdleCallback(
 *   &                     ClientDataPtr)
 *    POINTER (ClientDataPtr,DummyClientData)
 * </FortranSyntax>
 *
 */
typedef void (STDCALL *OnIdleCallback_pf)(ArbParam_t ClientData);

/* CORE SOURCE CODE REMOVED */

#endif /* _GLOBAL_H */
