#include "TECADDON.h"
#include "TASSERT.h"
#include "ADDGLBL.h"
#include "GUIDEFS.h"
#include "ADDONVER.h"
#include "EXBMP.h"


/*  Since BMP is a native Windows format, everything
    must be written out in Intel order */
#if !defined(MSWIN) /* Change to match your operating system */
# define SWAP_BYTES /* swap if not Intel */
#endif


static void Export1ByteInt(FILE *F,
                           BYTE n)
{
  REQUIRE(F != NULL);
  REQUIRE("n is any byte");

  fwrite(&n,1,sizeof(BYTE),F);
}

static void Export2ByteInt(FILE *F,
                           WORD w)
{
  REQUIRE(F != NULL);
  REQUIRE("w is any WORD value");

#ifdef SWAP_BYTES
  {
    BYTE *pByte = (BYTE*) &w;
    BYTE t = pByte[0];

    pByte[0] = pByte[1];
    pByte[1] = t;
  }
#endif /* SWAP_BYTES */
  
  fwrite(&w,1,sizeof(WORD),F);
}

static void Export4ByteInt(FILE *F,
                           DWORD dw)
{
  REQUIRE(F != NULL);
  REQUIRE("dw is any DWORD value");

#ifdef SWAP_BYTES
    {
      BYTE buffer[4];
      BYTE *pN = (BYTE*)&dw;
      buffer[3] = pN[0];
      buffer[2] = pN[1];
      buffer[1] = pN[2];
      buffer[0] = pN[3];

      fwrite(buffer,1,sizeof(DWORD),F);
    }
#endif /* SWAP_BYTES */

  fwrite(&dw,1,sizeof(DWORD),F);
}


static Boolean_t WriteBMP(FILE *F,
                          ScreenDim_t Width,
                          Boolean_t ConvertTo256Colors)
{
  Boolean_t   IsOk = TRUE;
  short       ImageHeight;
  ScreenDim_t Height;
  ScreenDim_t jj;
  ScreenDim_t ii;
  BYTE PadBytes[3]  = { 0,0,0 };   /* up to 3 pad bytes at the end of each line */
  /* pad each line to a 4 byte boundary.
     the line length is either Width, or Width * 3 for true color images */
  int NumPadBytes = (ConvertTo256Colors ? Width : Width * 3) & ( (1<<0) || (1<<1) );
  if ( NumPadBytes != 0 )
    NumPadBytes = 4 - NumPadBytes;

  REQUIRE(F != NULL);
  REQUIRE(Width > 0);
  REQUIRE(VALID_BOOLEAN(ConvertTo256Colors));

  TecUtilImageGetDimensions(NULL,&ImageHeight);
  Height = (ScreenDim_t) ImageHeight;

  TecUtilDialogLaunchPercentDone("Exporting BMP...",TRUE);
  
  /* BMP 2.x header */
  Export2ByteInt(F,0x4D42); /* magic number */
  Export4ByteInt(F,0);      /* File size (0 since we're not compressing) */
  Export2ByteInt(F,0);      /* Reserved 1 */
  Export2ByteInt(F,0);      /* Reserved 2 */

  /* bitmap offset */
  Export4ByteInt(F,   14 + /* sizeof 2.x header */
                      40 + /* sizeof 3.x header */
                      (ConvertTo256Colors ? 256*4 : 0)); /* sizeof colortable (0 if none) */

  Export4ByteInt(F,40); /* sizeof 3.x header  */
  Export4ByteInt(F,(DWORD)Width);   /* width  */
  Export4ByteInt(F,(DWORD)Height);  /* height */
  Export2ByteInt(F,1); /* # of color planes (always 1) */
  Export2ByteInt(F,(WORD) (ConvertTo256Colors ? 8 : 24)); /* bits per pixel */

  Export4ByteInt(F,0); /* no compression */
  Export4ByteInt(F,0); /* bitmap size (0 since there is no compression) */
  Export4ByteInt(F,0); /* resolution */
  Export4ByteInt(F,0);
  Export4ByteInt(F,0); /* colors used (calculated) */
  Export4ByteInt(F,0); /* colors important (all) */

  /* palette */
  if (ConvertTo256Colors)
    {
      /* palette */
      Byte_t PalRed[256];
      Byte_t PalGreen[256];
      Byte_t PalBlue[256];

      short *IndexScanLine  = NULL; /* if convert to 256 colors */


      BYTE RGB[4]       = { 0,0,0,0 }; /* B,G,R,Reserved */
     
      IndexScanLine = (short*) malloc(Width * sizeof(short)); 
      /* NOTE: A real application would check the return value of malloc() */
      CHECK(VALID_REF(IndexScanLine));

      TecUtilImageGetColorTable(PalRed,PalGreen,PalBlue);
      for (ii=0;ii<256;ii++)
        {
          RGB[0] = PalBlue[ii];
          RGB[1] = PalGreen[ii];
          RGB[2] = PalRed[ii];

          fwrite(RGB,4,sizeof(char),F);
        }      
      
      /* BMPs must be written "Upside Down" */
      for ( jj = Height; jj>=1; jj-- )
        {
          TecUtilImageIndexedGetScanLine((short)jj,IndexScanLine);
          for (ii=0;ii<Width;ii++)
            Export1ByteInt(F,(BYTE)IndexScanLine[ii]);

          if (NumPadBytes > 0)
            fwrite(PadBytes,NumPadBytes,sizeof(BYTE),F);

          if (jj % 32 == 0)
            TecUtilDialogCheckPercentDone( ((Height-jj) * 100) / Height );
        }

      
      free(IndexScanLine);
    }

  else  /* true color */
    {
      short *RedScanLine    = NULL; /* if true color */
      short *GreenScanLine  = NULL; /* "" */
      short *BlueScanLine   = NULL; /* "" */
      /* Note: A real application would check the return value of malloc() */

      RedScanLine     = (short*) malloc(Width * sizeof(short));
      GreenScanLine   = (short*) malloc(Width * sizeof(short));
      BlueScanLine    = (short*) malloc(Width * sizeof(short));

      CHECK(VALID_REF(RedScanLine));
      CHECK(VALID_REF(GreenScanLine));
      CHECK(VALID_REF(BlueScanLine));

      for ( jj = Height; jj>=1; jj-- )
        {
          TecUtilImageRGBGetScanLine((short)jj,RedScanLine,GreenScanLine,BlueScanLine);

          for (ii=0;ii<Width;ii++)
            {
              Export1ByteInt(F,(BYTE)BlueScanLine[ii]);
              Export1ByteInt(F,(BYTE)GreenScanLine[ii]);
              Export1ByteInt(F,(BYTE)RedScanLine[ii]);
            }

          if (NumPadBytes > 0)
            fwrite(PadBytes,NumPadBytes,sizeof(BYTE),F);

          if (jj % 32 == 0)
            TecUtilDialogCheckPercentDone( ((Height-jj) * 100) / Height );
        }

      free(RedScanLine);
      free(GreenScanLine);
      free(BlueScanLine);
    }
  



  TecUtilDialogDropPercentDone();

  ENSURE(VALID_BOOLEAN(IsOk));
  return IsOk;
}


Boolean_t ExportBMP(FILE *OutputFile,
                    ScreenDim_t Width,
                    BitDumpRegion_e ExportRegion,
                    Boolean_t ConvertTo256Colors)
{
  Boolean_t Result = FALSE;
  ArgList_pa ArgList;

  REQUIRE(OutputFile != NULL);
  REQUIRE(Width > 0);
  /* REQUIRE(VALID_ENUM(ExportRegion)); */
  REQUIRE(VALID_BOOLEAN(ConvertTo256Colors));
  
  ArgList = TecUtilArgListAlloc();

  if ( ArgList )
    {
      TecUtilArgListAppendInt(ArgList,SV_CONVERTTO256COLORS,ConvertTo256Colors);
      TecUtilArgListAppendInt(ArgList,SV_IMAGEWIDTH,(LgIndex_t)Width);
      TecUtilArgListAppendInt(ArgList,SV_BITDUMPREGION,(LgIndex_t)ExportRegion);

      if ( TecUtilImageBitmapCreateX(ArgList) )
        {
          Result = WriteBMP(OutputFile,Width,ConvertTo256Colors);
          TecUtilImageBitmapDestroy();
        }
     
      TecUtilArgListDealloc(&ArgList);
    }
  



  ENSURE(VALID_BOOLEAN(Result));
  return Result;
}


