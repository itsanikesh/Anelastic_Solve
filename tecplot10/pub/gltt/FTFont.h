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
 */

#ifndef __FTFont_h
#define __FTFont_h

#ifndef __GLTTboolean_h
#include "GLTTboolean.h"
#endif

class FTInstance;
class FTGlyph;

/////////////////////////////////////////////////////////////////////////////

#ifndef DLLDECL
#define DLLDECL __declspec( dllimport )
#endif

class DLLDECL FTFont
{
protected:
  FTInstance* instance;

  FTGlyph** glyphs;
#if defined AMTEC
  GLTTboolean* loaded;
#endif /* !AMTEC */

public:
  FTFont( FTInstance* _instance );

  virtual ~FTFont();

  void destroy();

  virtual GLTTboolean create();

#if defined AMTEC
  virtual GLTTboolean loadGlyph( int i );
#endif /* AMTEC */

#if defined AMTEC
  FTGlyph* getGlyph( int i )
#else /* AMTEC */
  FTGlyph* getGlyph( int i ) const
#endif /* AMTEC */
    {
    if( glyphs == 0 )
      return 0;
    if( i < 0 || i > 255 )
      return 0;

#if defined AMTEC
    if( !loadGlyph(i) )
      return GLTT_FALSE;
#endif /* AMTEC */

    return glyphs[i];
    }

  int getHeight() const;

  int getDescender() const;

  int getWidth( const char* text );

  void getBBox( const char* text,
                int& llx, int& lly, int& urx, int& ury ) const;
};

/////////////////////////////////////////////////////////////////////////////

#endif // ifndef __FTFont_h

