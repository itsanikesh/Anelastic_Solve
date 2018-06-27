#ifndef __GLTTGlyphTriangles_h
#define __GLTTGlyphTriangles_h

#ifndef __FTGlyphVectorizer_h
#include "FTGlyphVectorizer.h"
#endif

#ifndef __GLTTGlyphTriangulator_h
#include "GLTTGlyphTriangulator.h"
#endif

class GLTTGlyphTriangles: public GLTTGlyphTriangulator
{
public:
  struct Triangle
    {
    FTGlyphVectorizer::POINT* p1;
    FTGlyphVectorizer::POINT* p2;
    FTGlyphVectorizer::POINT* p3;
    };

  Triangle* triangles;
  int nTriangles;

  GLTTboolean count_them;

  GLTTGlyphTriangles( FTGlyphVectorizer* vectorizer = 0 ):
    GLTTGlyphTriangulator(vectorizer)
    {
    triangles= 0;
    nTriangles= 0;
    count_them= GLTT_TRUE;
    }
  virtual ~GLTTGlyphTriangles()
    {
    delete[] triangles;
    triangles= 0;
    }
  void alloc()
    {
    delete[] triangles;
    triangles= new Triangle [ nTriangles + 1 ];
    }
  virtual void triangle( FTGlyphVectorizer::POINT* p1,
                         FTGlyphVectorizer::POINT* p2,
                         FTGlyphVectorizer::POINT* p3 )
    {
    if( count_them )
      {
      ++nTriangles;
      return;
      }
    triangles[nTriangles].p1= p1;
    triangles[nTriangles].p2= p2;
    triangles[nTriangles].p3= p3;
    ++nTriangles;
    }
  GLTTGlyphPolygonizer* getPolygonizer()
    {
    return polygonizer;
    }
  };

#endif /* ifndef __GLTTGlyphTriangles_h */
