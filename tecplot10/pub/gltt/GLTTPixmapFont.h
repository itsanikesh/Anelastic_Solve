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

#ifndef __GLTTPixmapFont_h
#define __GLTTPixmapFont_h

#ifndef __GLTTboolean_h
#include "GLTTboolean.h"
#endif

class FTFace;
class FTInstance;
class FTPixmapFont;

/////////////////////////////////////////////////////////////////////////////

#ifndef DLLDECL
#define DLLDECL __declspec( dllimport )
#endif

class DLLDECL GLTTPixmapFont
{
protected:
  FTFace* face;

  FTInstance* instance;

  FTPixmapFont* pixmaps;

public:
  GLTTPixmapFont( FTFace* _face );

  virtual ~GLTTPixmapFont();

  void destroy();

#if defined AMTEC
  GLTTboolean create( int point_size, int resolution = 96, double angle = 0.0 );
  void load( int from = 0, int to = 255 );
#else /* AMTEC */
  GLTTboolean create( int point_size );
#endif /* AMTEC */

  FTPixmapFont* getPixmapFont() const
    {
    return pixmaps;
    }

  void output( int x, int y, const char* text );
  void output( const char* text );

  int getWidth( const char* text );
  int getHeight() const;
  int getDescender() const;
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __GLTTPixmapFont_h
