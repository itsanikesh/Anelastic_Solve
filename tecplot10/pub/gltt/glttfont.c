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

#include <stdio.h>

#include "GLTTFont.h"

#include "FTFont.h"
#include "FTInstance.h"
#include "FTGlyph.h"
#include "FTGlyphVectorizer.h"

#include "GLTTGlyphPolygonizer.h"

#if defined AMTEC
#include "GLTTGlyphTriangles.h"
#endif /* AMTEC */

#ifdef WIN32
#include <windows.h>
#endif

#ifdef LOCAL_GL_HEADER
  #include "local_gl.h"
  #include "local_glu.h"
#else
  #include <GL/gl.h>
  #include <GL/glu.h>
#endif


/////////////////////////////////////////////////////////////////////////////

#if defined AMTEC
  VertexFunction GLTTFont::vertex_function = glVertex2d;
#endif /* AMTEC */


GLTTFont::GLTTFont( FTFace* _face )
{
  face= _face;

  instance= 0;
  font= 0;

  loaded= 0;
#if defined AMTEC
  triangles = 0;
#else /* AMTEC */
  list_base= 0;
#endif /* AMTEC */

  precision= 4.;
}

/////////////////////////////////////////////////////////////////////////////

GLTTFont::~GLTTFont()
{
  destroy();

  face= 0;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTFont::destroy()
{
  delete[] loaded;
  loaded= 0;

#if defined AMTEC
  if (triangles != 0)
    {
    for(int i = 0; i < 256; i++)
      {
      if (triangles[i] != 0) delete triangles[i];
      }
    delete[] triangles;
    }
#else /* AMTEC */
  if( list_base != 0 )
    {
    glDeleteLists( list_base, 256 );
    list_base= 0;
    }
#endif /* AMTEC */

  delete font;
  font= 0;

  delete instance;
  instance= 0;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTFont::setPrecision( double _precision )
{
  precision= _precision;
}

/////////////////////////////////////////////////////////////////////////////

#if defined AMTEC
GLTTboolean GLTTFont::create( int point_size, int resolution /* = 96 */)
#else /* AMTEC */
GLTTboolean GLTTFont::create( int point_size )
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

  font= new FTFont(instance);

  if( ! font->create() )
    return GLTT_FALSE;

#if defined AMTEC
  triangles = new GLTTGlyphTriangles* [ 256 ];
  if( triangles == 0 )
    return GLTT_FALSE;
  else
    for(int j = 0; j < 256; j++)
      triangles[j] = 0;
#else /* AMTEC */
  list_base= glGenLists(256);
  if( list_base == 0 )
    return GLTT_FALSE;
#endif /* AMTEC */

  loaded= new GLTTboolean [ 256 ];
  for( int i= 0; i < 256; ++i )
    loaded[i]= GLTT_FALSE;


  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

GLTTboolean GLTTFont::loadGlyph( int i )
{
  if( i < 0 || i > 256 )
    return GLTT_FALSE;

#if defined AMTEC
  if( triangles == 0 || loaded == 0 )
#else /* AMTEC */
  if( list_base == 0 || loaded == 0 )
#endif /* AMTEC */
    return GLTT_FALSE;

  if( loaded[i] )
    return GLTT_TRUE;

  loaded[i]= GLTT_TRUE;

#if defined AMTEC
  triangles[i - 1]      = new GLTTGlyphTriangles();

  GLTTGlyphTriangles *t = triangles[i - 1];

  t->setPrecision(precision);
#else /* AMTEC */
  GLTTGlyphPolygonizer polygonizer;

  polygonizer.setPrecision(precision);

  int list= list_base + i;
#endif /* AMTEC */
  FTGlyph* glyph= font->getGlyph(i);

  if( glyph == 0 )
    {
    err:
#if !defined AMTEC
    glNewList(list,GL_COMPILE);
    glEndList();
#endif /* !AMTEC */
    return GLTT_TRUE;
    }

#if defined AMTEC
  if ( ! t->init(glyph) )
    goto err;

  t->triangulate();
  t->alloc();
  
  t->nTriangles= 0;
  t->count_them= GLTT_FALSE;
  t->triangulate();
#else /* AMTEC */
  if( ! polygonizer.init(glyph) )
    goto err;

  glNewList(list,GL_COMPILE);
  polygonizer.polygonize();
  glTranslatef( GLfloat(polygonizer.getAdvance()), 0., 0. );

  glEndList();
#endif /* AMTEC */

  return GLTT_TRUE;
}

/////////////////////////////////////////////////////////////////////////////

void GLTTFont::load( int from /* = 0 */, int to /* = 255 */ )
{
  for( int i= from; i <= to; ++i )
    loadGlyph(i);
}

/////////////////////////////////////////////////////////////////////////////

void GLTTFont::load( const char* text )
{
#if defined AMTEC
  if( text == 0 || triangles == 0 )
#else /* AMTEC */
  if( text == 0 || list_base == 0 )
#endif /* AMTEC */
    return;

  for(;;)
    {
    int ch= (unsigned char)*text;
    if( ch == 0 )
      break;
    ++text;

    if( ! loaded[ch] )
      loadGlyph(ch);
    }
}

/////////////////////////////////////////////////////////////////////////////

void GLTTFont::output( const char* text )
{
#if defined AMTEC
  if( text == 0 || triangles == 0 )
#else /* AMTEC */
  if( text == 0 || list_base == 0 )
#endif /* AMTEC */
    return;

  glPushMatrix();

  for(;;)
    {
    int ch= (unsigned char)*text;
    if( ch == 0 )
      break;
    ++text;

    if( ! loaded[ch] )
      loadGlyph(ch);

#if defined AMTEC
    GLTTGlyphTriangles *t = triangles[ch - 1];
    for(int i = 0; i < t->nTriangles; i++)
      {
        glBegin(GL_POLYGON);
          vertex_function(t->triangles[i].p1->x, t->triangles[i].p1->y);
          vertex_function(t->triangles[i].p2->x, t->triangles[i].p2->y);
          vertex_function(t->triangles[i].p3->x, t->triangles[i].p3->y);
        glEnd();
      }
    glTranslatef( GLfloat(t->getPolygonizer()->getAdvance()), 0., 0. );
#else /* AMTEC */
    glCallList( list_base + ch );
#endif /* AMTEC */
    }

//  glPushAttrib( GL_LIST_BIT );
//  glListBase(list_base);
//  glCallLists( strlen(text), GL_UNSIGNED_BYTE, (GLubyte*)text );
//  glPopAttrib();

  glPopMatrix();
}

/////////////////////////////////////////////////////////////////////////////

int GLTTFont::getHeight() const
{
  if( font == 0 )
    return 0;

  return font->getHeight();
}

/////////////////////////////////////////////////////////////////////////////

int GLTTFont::getDescender() const
{
  if( font == 0 )
    return 0;

  return font->getDescender();
}

/////////////////////////////////////////////////////////////////////////////

int GLTTFont::getWidth( const char* text )
{
  if( font == 0 )
    return 0;

  return font->getWidth(text);
}

/////////////////////////////////////////////////////////////////////////////

void GLTTFont::getBBox( const char* text,
                        int& llx, int& lly, int& urx, int& ury ) const
{
  llx= lly= urx= ury= 0;

  if( font == 0 )
    return;

  font->getBBox( text, llx, lly, urx, ury );
}

/////////////////////////////////////////////////////////////////////////////

