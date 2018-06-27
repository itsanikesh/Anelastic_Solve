/*
 * gltt graphics library
 * Copyright (C) 1998-1999 Stephane Rehel
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * Modified 11/22/2000 by Amtec Engineering, Inc., Bellevue, WA. To include
 * changes, #define AMTEC.
 */

#include <string.h>
#if defined AMTEC
#include <math.h>
#endif /* AMTEC */

#include "FTGlyphPixmap.h"
#include "FTGlyph.h"
#if defined AMTEC
#include "FTEngine.h"
#endif /* AMTEC */

#include "freetype.h"

/////////////////////////////////////////////////////////////////////////////

#if defined AMTEC
FTGlyphPixmap::FTGlyphPixmap( FTGlyph* _glyph, double _angle /* = 0.0 */ )
#else /* AMTEC */
FTGlyphPixmap::FTGlyphPixmap( FTGlyph* _glyph )
#endif /* AMTEC */
{
  glyph= _glyph;
  width= height= 0;
  cols= 0;
  bitmap= 0;

  pixmap= 0;
  r= g= b= 0;
  a= 255;

  advance= 0;
  delta_x= delta_y= 0;
#if defined AMTEC
  angle = _angle;
#endif /* AMTEC */
}

/////////////////////////////////////////////////////////////////////////////

FTGlyphPixmap::~FTGlyphPixmap()
{
  destroy();

  glyph= 0;
}

/////////////////////////////////////////////////////////////////////////////

void FTGlyphPixmap::destroy()
{
  delete[] bitmap;
  bitmap= 0;

  delete[] pixmap;
  pixmap= 0;

  width= height= 0;
  cols= 0;

  advance= 0;
  delta_x= delta_y= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTGlyphPixmap::create()
{
  destroy();

  if( glyph == 0 )
    return GLTT_FALSE;
  if( glyph->getGlyph() == 0 )
    return GLTT_FALSE;

  TT_Glyph_Metrics metrics;
  TT_Error err= TT_Get_Glyph_Metrics( *glyph->getGlyph(), &metrics );

  if( err )
    return GLTT_FALSE;

  TT_BBox& bbox= metrics.bbox; // glyph bounding box

  advance= metrics.advance;
  delta_x= bbox.xMin;
  delta_y= bbox.yMin;

#if defined AMTEC
  TT_Outline outline;

  /* get the outline */
  TT_Get_Glyph_Outline( *glyph->getGlyph(), &outline );

  if(angle != 0.0)
    {
      TT_Matrix  matrix;

      /* Now rotate it. */
#define PI_OVER_180 (3.14159265359 / 180.0)
      matrix.xx = (TT_Fixed)(cos(angle * PI_OVER_180) * (double)(0x10000));
      matrix.xy = (TT_Fixed)(-sin(angle * PI_OVER_180) * (double)(0x10000));
      matrix.yx = -matrix.xy;
      matrix.yy = matrix.xx;
      TT_Transform_Outline(&outline,
                           &matrix );
      
      /* compute its extent */
      TT_Get_Outline_BBox( &outline, &bbox );
      delta_x= bbox.xMin;
      delta_y= bbox.yMin;
    }
#endif /* AMTEC */

  #define  FLOOR(x)    ((x) & -64)
  #define  CEILING(x)  (((x)+63) & -64)
  bbox.xMin= FLOOR(bbox.xMin);
  bbox.yMin= FLOOR(bbox.yMin);
  bbox.xMax= CEILING(bbox.xMax);
  bbox.yMax= CEILING(bbox.yMax);
  #undef CEILING
  #undef FLOOR

  width = (bbox.xMax - bbox.xMin)/64;
  height= (bbox.yMax - bbox.yMin)/64;

  cols= (width+3) & -4;
  int size= cols * height;

  if( size <= 0 )
    return GLTT_TRUE;

  bitmap= new unsigned char [ size ];

  memset( (void*) bitmap, 0, size );

  TT_Raster_Map map;

  map.width = width;
  map.cols  = cols;
  map.rows  = height;
  map.flow  = TT_Flow_Up;
  map.size  = size;
  map.bitmap= (void*) bitmap;

#if defined AMTEC
  {
    TT_Engine engine = *FTEngine::getStaticEngine()->getEngine();
    
    TT_Translate_Outline( &outline, -bbox.xMin, -bbox.yMin );
    err = TT_Get_Outline_Pixmap( engine, &outline, &map );
    TT_Translate_Outline( &outline, bbox.xMin, bbox.yMin );
  }
#else /* AMTEC */
  err= TT_Get_Glyph_Pixmap( *glyph->getGlyph(),
                            &map,
                            -bbox.xMin,
                            -bbox.yMin );
#endif /* AMTEC */
  if( err )
    {
    delete bitmap;
    bitmap= 0;
    return GLTT_FALSE;
    }

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

unsigned char* FTGlyphPixmap::getPixmap( unsigned char _r,
                                         unsigned char _g,
                                         unsigned char _b,
                                         unsigned char _a )
{
  if( bitmap == 0 )
    return 0;

  if( pixmap != 0 && r == _r && g == _g && b == _b && a == _a )
    return pixmap;

  // This is another color! We need to recolor our pixmap from the
  // grayscale bitmap

  if( pixmap == 0 )
    pixmap= new unsigned char [ cols * height * 4 ];

  unsigned char palette[5][4];

  int j;

  for( j= 0; j < 5; ++j )
    {
    palette[j][0]= (unsigned char)(int(_r) * j / 4);
    palette[j][1]= (unsigned char)(int(_g) * j / 4);
    palette[j][2]= (unsigned char)(int(_b) * j / 4);
    palette[j][3]= (unsigned char)(int(_a) * j / 4);
    }

  register unsigned char* bline= bitmap;
  register unsigned char* pline= pixmap;
  if( sizeof(int) == 4 )
    {
    for( j= 0; j < height; ++j )
      {
      for( register int i= 0; i < width; ++i )
        {
        register unsigned char k= bline[i];
        // assuming sizeof(int) == 4 ...
        *(int*)(pline + i*4)= *(int*) &(palette[k][0]);
        }
      bline += cols;
      pline += cols * 4;
      }
    }
   else
    {
    for( j= 0; j < height; ++j )
      {
      for( register int i= 0; i < width; ++i )
        {
        register unsigned char k= bline[i];
        (pline + i*4)[0]= palette[k][0];
        (pline + i*4)[1]= palette[k][1];
        (pline + i*4)[2]= palette[k][2];
        (pline + i*4)[3]= palette[k][3];
        }
      bline += cols;
      pline += cols * 4;
      }
    }

  r= _r;
  g= _g;
  b= _b;
  a= _a;

  return pixmap;
}

/////////////////////////////////////////////////////////////////////////////
