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

#include "FTPixmapFont.h"
#include "FTGlyph.h"
#include "FTGlyphPixmap.h"

/////////////////////////////////////////////////////////////////////////////

#if defined AMTEC
FTPixmapFont::FTPixmapFont( FTInstance* _instance, double _angle /*= 0.0*/): FTFont(_instance)
#else /* AMTEC */
FTPixmapFont::FTPixmapFont( FTInstance* _instance ): FTFont(_instance)
#endif /* AMTEC */
{
  pixmaps= 0;
#if defined AMTEC
  angle = _angle;
#else /* AMTEC */
  loaded= 0;
#endif /* !AMTEC */
}

/////////////////////////////////////////////////////////////////////////////

FTPixmapFont::~FTPixmapFont()
{
  destroy();
}

/////////////////////////////////////////////////////////////////////////////

void FTPixmapFont::destroy()
{
  if( pixmaps != 0 )
    {
    for( int i= 0; i < 256; ++i )
      delete pixmaps[i];

    delete[] pixmaps;
    pixmaps= 0;
    }

#if !defined AMTEC
  delete[] loaded;
  loaded= 0;
#endif /* !AMTEC */
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTPixmapFont::create()
{
  destroy();

  if( ! FTFont::create() )
    return GLTT_FALSE;

  int i;

  pixmaps= new FTGlyphPixmap* [ 256 ];
#if !defined AMTEC
  loaded= new GLTTboolean [ 256 ];
#endif /* !AMTEC */

  for( i= 0; i < 256; ++i )
    {
    pixmaps[i]= 0;
#if !defined AMTEC
    loaded[i]= GLTT_FALSE;
#endif /* !AMTEC */
    }

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean FTPixmapFont::loadGlyph( int ascii_code )
{
  if( ascii_code < 0 || ascii_code > 255 || pixmaps == 0 || loaded == 0 )
    return GLTT_FALSE;

  if( loaded[ascii_code] )
    return GLTT_TRUE;

#if defined AMTEC
  if(!FTFont::loadGlyph(ascii_code))
    return GLTT_FALSE;
#else /* AMTEC */
  loaded[ascii_code]= GLTT_TRUE;
#endif /* AMTEC */

  FTGlyph* glyph= FTFont::glyphs[ascii_code];
  if( glyph == 0 )
    return GLTT_FALSE;

#if defined AMTEC
  FTGlyphPixmap* gpixmap= new FTGlyphPixmap(glyph, angle);
#else /* AMTEC */
  FTGlyphPixmap* gpixmap= new FTGlyphPixmap(glyph);
#endif /* AMTEC */
  if( ! gpixmap->create() )
    {
    delete gpixmap;
    return GLTT_FALSE;
    }

  pixmaps[ascii_code]= gpixmap;

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

void FTPixmapFont::load( int from /* = 0 */, int to /* = 255 */ )
{
  for( int i= from; i <= to; ++i )
    loadGlyph(i);
}

/////////////////////////////////////////////////////////////////////////////

int FTPixmapFont::getWidth( const char* text )
{
  if( text == 0 )
    return 0;

  int w= 0;
  for(;;)
    {
    int ch= (unsigned char) *(text++);
    if( ch == 0 )
      break;

    loadGlyph(ch);
    if( pixmaps[ch] == 0 )
      continue;

    w+= pixmaps[ch]->getAdvance();
    }

  return w / 64;
}

/////////////////////////////////////////////////////////////////////////////
