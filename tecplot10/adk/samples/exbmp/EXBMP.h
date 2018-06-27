/* EXBMP.h */

#ifndef _EXBMP_H_
#define _EXBMP_H_


/* Since BMP is a native Windows file format, we use the Windows data types */

#ifndef BYTE
# define BYTE Byte_t
#endif

#ifndef WORD
# define WORD UInt16_t
#endif

#ifndef DWORD
# define DWORD UInt32_t
#endif

#define DEFAULT_IMAGE_WIDTH 512

Boolean_t ExportBMP(FILE *OutputFile,
                    ScreenDim_t Width,
                    BitDumpRegion_e ExportRegion,
                    Boolean_t ConvertTo256Colors);

#endif /* _EXBMP_H_ */



