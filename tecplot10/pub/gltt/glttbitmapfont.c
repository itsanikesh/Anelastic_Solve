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

#if defined AMTEC
#include <math.h>
#endif /* AMTEC */

#include "GLTTBitmapFont.h"

#include "FTBitmapFont.h"
#include "FTInstance.h"
#include "FTGlyph.h"
#include "FTGlyphBitmap.h"

#ifdef WIN32
#include <windows.h>
#endif

#ifdef LOCAL_GL_HEADER
  #include "local_gl.h"
#else
  #include <GL/gl.h>
#endif

/////////////////////////////////////////////////////////////////////////////

GLTTBitmapFont::GLTTBitmapFont( FTFace* _face )
{
  face= _face;

  instance= 0;

  bitmaps= 0;
}

/////////////////////////////////////////////////////////////////////////////

GLTTBitmapFont::~GLTTBitmapFont()
{
  destroy();

  face= 0;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTBitmapFont::destroy()
{
  delete bitmaps;
  bitmaps= 0;

  delete instance;
  instance= 0;
}

/////////////////////////////////////////////////////////////////////////////

#if defined AMTEC
GLTTboolean GLTTBitmapFont::create( int point_size,
                                   int resolution /* = 96 */,
                                   double angle /* = 0.0 */)
#else /* AMTEC */
GLTTboolean GLTTBitmapFont::create( int point_size )
#endif /* AMTEC */
{
  destroy();

  if( point_size < 1 )
    point_size= 1;

  instance= new FTInstance(face);

  if( ! instance->create() )
    return GLTT_FALSE;

#if defined AMTEC
  if( ! instance->setResolutions(resolution,resolution) )
#else /* AMTEC */
  if( ! instance->setResolutions(96,96) )
#endif /* AMTEC */
    return GLTT_FALSE;

  if( ! instance->setPointSize(point_size) )
    return GLTT_FALSE;

#if defined AMTEC
  bitmaps= new FTBitmapFont(instance, angle);
#else /* AMTEC */
  bitmaps= new FTBitmapFont(instance);
#endif /* AMTEC */

  if( ! bitmaps->create() )
    return GLTT_FALSE;

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTBitmapFont::output( int x, int y, const char* text )
{
  if( text == 0 || bitmaps == 0 )
    return;

  glRasterPos2i( x, y );

  GLboolean position_valid;
  glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &position_valid);
  if( !position_valid )
    {
    glRasterPos2i(0,0);

    glGetBooleanv(GL_CURRENT_RASTER_POSITION_VALID, &position_valid);
    if( !position_valid )
      return;

    glBitmap( 0, 0, 0, 0, GLfloat(x), GLfloat(y), (const GLubyte *)0 );
    }

  output(text);
}

/////////////////////////////////////////////////////////////////////////////

void GLTTBitmapFont::output( const char* text )
{
  if( text == 0 || bitmaps == 0 )
    return;

  GLint swapbytes, lsbfirst, rowlength;
  GLint skiprows, skippixels, alignment;

  // Save the current packing mode for bitmaps.
  glGetIntegerv( GL_UNPACK_SWAP_BYTES, &swapbytes );
  glGetIntegerv( GL_UNPACK_LSB_FIRST, &lsbfirst );
  glGetIntegerv( GL_UNPACK_ROW_LENGTH, &rowlength );
  glGetIntegerv( GL_UNPACK_SKIP_ROWS, &skiprows );
  glGetIntegerv( GL_UNPACK_SKIP_PIXELS, &skippixels );
  glGetIntegerv( GL_UNPACK_ALIGNMENT, &alignment );

  // Enforce a standard packing mode
  glPixelStorei( GL_UNPACK_SWAP_BYTES, GL_FALSE );
  glPixelStorei( GL_UNPACK_LSB_FIRST, GL_FALSE );
  glPixelStorei( GL_UNPACK_SKIP_ROWS, 0 );
  glPixelStorei( GL_UNPACK_SKIP_PIXELS, 0 );
  glPixelStorei( GL_UNPACK_ROW_LENGTH, 0 );
  glPixelStorei( GL_UNPACK_ALIGNMENT, 1 );

  for(;;)
    {
    int ch= (unsigned char)*text;
    if( ch == 0 )
      break;
    ++text;

    FTGlyphBitmap* gbitmap= bitmaps->getBitmap(ch);
    if( gbitmap == 0 )
      continue;

    if( gbitmap->getBitmap() != 0 )
      {
      glBitmap( gbitmap->getBitmapWidth(),
                gbitmap->getBitmapHeight(),
                GLfloat(double(gbitmap->getDeltaX())/-64.0), // x orig
                GLfloat(double(gbitmap->getDeltaY())/-64.0), // y orig
#if defined AMTEC
#define PI_OVER_180 (3.14159265359 / 180.0)
                GLfloat((double(gbitmap->getAdvance())/64.0)*cos(gbitmap->getAngle() * PI_OVER_180)), // x move
                GLfloat((double(gbitmap->getAdvance())/64.0)*sin(gbitmap->getAngle() * PI_OVER_180)), // y move
#else /* AMTEC */
                GLfloat(double(gbitmap->getAdvance())/64.0), // x move
                0.0, // y move
#endif /* AMTEC */
                (const GLubyte*) gbitmap->getBitmap() );
      }
     else
      {
      glBitmap( 0, 0,
                0.0, 0.0,
#if defined AMTEC
                GLfloat(double(gbitmap->getAdvance())/64.0*cos(gbitmap->getAngle() * PI_OVER_180)), // x move
                GLfloat(double(gbitmap->getAdvance())/64.0*sin(gbitmap->getAngle() * PI_OVER_180)), // y move
#else /* AMTEC */
                GLfloat(double(gbitmap->getAdvance())/64.), 0.0,
#endif /* AMTEC */
                (const GLubyte *)0 );
      }
    }

  // Restore saved packing modes.
  glPixelStorei( GL_UNPACK_SWAP_BYTES, swapbytes );
  glPixelStorei( GL_UNPACK_LSB_FIRST, lsbfirst );
  glPixelStorei( GL_UNPACK_ROW_LENGTH, rowlength );
  glPixelStorei( GL_UNPACK_SKIP_ROWS, skiprows );
  glPixelStorei( GL_UNPACK_SKIP_PIXELS, skippixels );
  glPixelStorei( GL_UNPACK_ALIGNMENT, alignment );
}

/////////////////////////////////////////////////////////////////////////////

int GLTTBitmapFont::getWidth( const char* text )
{
  if( bitmaps == 0 )
    return 0;

  return bitmaps->getWidth(text);
}

/////////////////////////////////////////////////////////////////////////////

int GLTTBitmapFont::getHeight() const
{
  if( instance == 0 )
    return 0;

  return instance->getHeight();
}


/////////////////////////////////////////////////////////////////////////////

int GLTTBitmapFont::getDescender() const
{
  if( instance == 0 )
    return 0;

  return instance->getDescender();
}

/////////////////////////////////////////////////////////////////////////////

#if defined AMTEC
void GLTTBitmapFont::load( int from /* = 0 */, int to /* = 255 */ )
{
  if(bitmaps)
    {
    bitmaps->load(from, to);
    }
}
#endif /* AMTEC */
