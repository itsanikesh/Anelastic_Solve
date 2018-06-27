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

#ifndef __FTBitmapFont_h
#define __FTBitmapFont_h

#ifndef __FTFont_h
#include "FTFont.h"
#endif

class FTGlyphBitmap;

/////////////////////////////////////////////////////////////////////////////

#ifndef DLLDECL
#define DLLDECL __declspec( dllimport )
#endif

class DLLDECL FTBitmapFont: public FTFont
{
protected:
  FTGlyphBitmap** bitmaps;
#if defined AMTEC
  double angle;
#else /* AMTEC */
  GLTTboolean* loaded;
#endif /* AMTEC */

public:
#if defined AMTEC
  FTBitmapFont( FTInstance* _instance, double _angle = 0.0 );
#else /* AMTEC */
  FTBitmapFont( FTInstance* _instance );
#endif /* AMTEC */

  virtual ~FTBitmapFont();

  void destroy();

  virtual GLTTboolean create();

#if defined AMTEC
  virtual GLTTboolean loadGlyph( int i );
#else /* AMTEC */
  GLTTboolean loadGlyph( int i );
#endif /* AMTEC */
  void load( int from = 0, int to = 255 );

  FTGlyphBitmap* getBitmap( int ascii_code )
    {
    if( bitmaps == 0 || loaded == 0 )
      return 0;
    if( ascii_code < 0 || ascii_code > 255 )
      return 0;

    if( ! loaded[ascii_code] )
      loadGlyph(ascii_code);

    return bitmaps[ascii_code];
    }

  int getWidth( const char* text );
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __FTBitmapFont_h
