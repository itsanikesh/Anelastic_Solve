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

#ifndef __FTPixmapFont_h
#define __FTPixmapFont_h

#ifndef __FTFont_h
#include "FTFont.h"
#endif

class FTGlyphPixmap;

/////////////////////////////////////////////////////////////////////////////

#ifndef DLLDECL
#define DLLDECL __declspec( dllimport )
#endif

class DLLDECL FTPixmapFont: public FTFont
{
protected:
  FTGlyphPixmap** pixmaps;
#if defined AMTEC
  double angle;
#else /* AMTEC */
  GLTTboolean* loaded;
#endif /* AMTEC */

public:
#if defined AMTEC
  FTPixmapFont( FTInstance* _instance, double _angle = 0.0 );
#else /* AMTEC */
  FTPixmapFont( FTInstance* _instance );
#endif /* AMTEC */

  virtual ~FTPixmapFont();

  void destroy();

  virtual GLTTboolean create();

#if defined AMTEC
  virtual GLTTboolean loadGlyph( int i );
#else /* AMTEC */
  GLTTboolean loadGlyph( int i );
#endif /* AMTEC */
  void load( int from = 0, int to = 255 );

  FTGlyphPixmap* getPixmap( int ascii_code )
    {
    if( pixmaps == 0 || loaded == 0 )
      return 0;
    if( ascii_code < 0 || ascii_code > 255 )
      return 0;

    if( ! loaded[ascii_code] )
      loadGlyph(ascii_code);

    return pixmaps[ascii_code];
    }

  int getWidth( const char* text );
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __FTPixmapFont_h
