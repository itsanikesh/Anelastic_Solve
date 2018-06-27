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

#ifndef __FTGlyphPixmap_h
#define __FTGlyphPixmap_h

#ifndef __GLTTboolean_h
#include "GLTTboolean.h"
#endif

class FTGlyph;

/////////////////////////////////////////////////////////////////////////////

#ifndef DLLDECL
#define DLLDECL __declspec( dllimport )
#endif

class DLLDECL FTGlyphPixmap
{
protected:
  FTGlyph* glyph;

  int width, height;
  int cols;
  unsigned char* bitmap;

  unsigned char* pixmap;
  unsigned char r, g, b, a; // associated r,g,b,a for this pixmap

  int advance; // 26.6
  int delta_x, delta_y; // 26.6

#if defined AMTEC
  double angle;
#endif /* AMTEC */

public:
#if defined AMTEC
  FTGlyphPixmap( FTGlyph* _glyph, double _angle = 0.0 );
#else /* AMTEC */
  FTGlyphPixmap( FTGlyph* _glyph );
#endif /* AMTEC */

  virtual ~FTGlyphPixmap();

  void destroy();

  GLTTboolean create();

  FTGlyph* getGlyph()
    {
    return glyph;
    }

  int getPixmapAllocatedWidth() const
    {
    return cols;
    }
  int getPixmapWidth() const
    {
    return width;
    }
  int getPixmapHeight() const
    {
    return height;
    }

  unsigned char* getPixmap( unsigned char _r,
                            unsigned char _g,
                            unsigned char _b,
                            unsigned char _a );

  // 26.6
  int getDeltaX() const
    {
    return delta_x;
    }

  // 26.6
  int getDeltaY() const
    {
    return delta_y;
    }

  // 26.6
  int getAdvance() const
    {
    return advance;
    }
#if defined AMTEC
  double getAngle()
    {
    return angle;
    }
#endif /* AMTEC */
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __FTGlyphPixmap_h
