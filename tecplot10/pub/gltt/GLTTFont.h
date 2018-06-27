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

#ifndef __GLTTFont_h
#define __GLTTFont_h

#ifndef __GLTTboolean_h
#include "GLTTboolean.h"
#endif

#if defined AMTEC
#ifndef __GLTTGlyphTriangles_h
#include "GLTTGlyphTriangles.h"
#endif
#endif /* AMTEC */

class FTFace;
class FTInstance;
class FTFont;

/////////////////////////////////////////////////////////////////////////////

#ifndef DLLDECL
#define DLLDECL __declspec( dllimport )
#endif

#if defined AMTEC
  typedef void (_stdcall *VertexFunction)(double xx, double yy);
#endif /* AMTEC */

class DLLDECL GLTTFont
{
protected:
  FTFace* face;

  FTInstance* instance;

  FTFont* font;

  GLTTboolean* loaded;
#if defined AMTEC
  GLTTGlyphTriangles** triangles;
  static VertexFunction vertex_function;
#else /* AMTEC */
  int list_base;
#endif /* AMTEC */

  double precision;


public:
  GLTTFont( FTFace* _face );

  virtual ~GLTTFont();

  void destroy();

  void setPrecision( double _precision );

#if defined AMTEC
  GLTTboolean create( int point_size, int resolution = 96 );
  static void setVertexFunction( VertexFunction vf ) { vertex_function = vf; }
#else /* AMTEC */
  GLTTboolean create( int point_size );
#endif /* AMTEC */

  GLTTboolean loadGlyph( int i );
  void load( int from = 0, int to = 255 );
  void load( const char* text );

  FTFont* getFont() const
    {
    return font;
    }

  void output( const char* text );

  int getWidth( const char* text );
  int getHeight() const;
  int getDescender() const;

  void getBBox( const char* text,
                int& llx, int& lly, int& urx, int& ury ) const;
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __GLTTFont_h
