/* file: polygrid.c 
 *
 * This program will take as input a set of arbituary shaped 3d or 2d polylines.
 * The ouput will be a set of triangles that will form the panels
 * to completely connect each set of adjacent polylines.  This output
 * is in the form required by preplot. 
 *
 * The input is of the form:
 *
 * IConnect           | 1=Connect first and last points, 0=Dont.
 * NDim               | Number of dimensions (2 or 3)
 * NADDV              | Number of additional variables (excl. X,Y [and Z])
 * N1                 | Number of points in the first polyline 
 * X1 Y1 Z1 V11 V12.. | Coordinate and field data for first point       
 * X2 Y2 Z2 V21 V22.. | Coordinate and field data for second point      
 * .                  | .
 * .                  | .
 * .                  | .
 * XN1 YN1 ZN1 VN1..  | Coordinate and field data for last point
 * N2                 | Number of points in second polyline.
 * X1 Y1 Z1 V11 V12.. | Coordinate and field data for first point       
 * X2 Y2 Z2 V21 V22.. | Coordinate and field data for second point      
 * .                  | .
 * .                  | .
 * .                  | .
 * XN2 YN2 ZN2 VN1..  | Coordinate and field data for last point of 
 * .                    the second polyline
 * .
 * .
 * as many polylines as you want.
 *
 *
 * To run polygrid type:
 *
 *    polygrid [maxptsperline] < myfile.pol > myfile.dat
 *
 * To compile polygrid type:
 *
 *    cc -o polygrid polygrid.c
 *
 * To make a special version of polygrid which creates FEBRICK
 * cells instead of FETRIANGLE cells include -DMAKEBRICKS
 */


typedef long LgIndex;

#include <stdio.h>
#include <math.h>
#if defined DOS
#include <malloc.h>
#endif
#include <stdlib.h>

LgIndex MaxPts;
double *X1;
double *Y1;
double *Z1;
double *V1;
double *X2;
double *Y2;
double *Z2;
double *V2;
int     NumAddVars;
int     NumDim;
LgIndex NumPts1;
LgIndex NumPts2;
LgIndex StartI1;
LgIndex StartI2;
LgIndex IConnect;
FILE   *TriFile;
FILE   *DataFile;




int ReadInCircle2()
{
  LgIndex I,J;
   if (scanf("%ld",&NumPts2) > 0)
     {
       if (NumPts2 > MaxPts-1)
         {
           fprintf(stderr,"Err: Polyline has %ld points,  limit is %ld\n",
                          NumPts2,MaxPts-1);
           exit(-1);                           
         }
       for (I=0; I<NumPts2; I++)
         {
           if (NumDim == 3)
             {
               if (scanf("%lf %lf %lf",&X2[I],&Y2[I],&Z2[I]) < 3)
                 return(0);
             }
           else
             {
               if (scanf("%lf %lf",&X2[I],&Y2[I]) < 2)
                 return(0);
             }
           for (J=0; J < NumAddVars; J++)
             {
               if (scanf("%lf",&V2[I*NumAddVars+J]) < 1)
                 return (0);
             }
         }
       if (IConnect)
         {
           X2[NumPts2] = X2[0];
           Y2[NumPts2] = Y2[0];
           if (NumDim == 3)
             Z2[NumPts2] = Z2[0];
           NumPts2++;
         }
       return (1);
     }
   return (0);
}

double GetDist(I1,I2)
       LgIndex I1,I2;
{
  double Dx;
  double Dy;
  double Dz = 0.0;
  if ((I1 >= NumPts1) ||
      (I2 >= NumPts2))
    return (1.0E300);

  Dx = X2[I2]-X1[I1];
  Dy = Y2[I2]-Y1[I1];
  if (NumDim == 3)
    Dz = Z2[I2]-Z1[I1];
  return(Dx*Dx+Dy*Dy+Dz*Dz);
}



main (argc,argv)
int   argc;
char *argv[];
{
  LgIndex I2,I3;
  LgIndex NextI1,NextI2;
  double  NextD1,NextD2;
  LgIndex TriInd[3];
  int     IsStart=1;
  short   C;
  int     NumTri=0;

#if defined (DOS)
  MaxPts = 500;
#else
  MaxPts = 2000;
#endif

  if (argc > 1)
    {
      if (sscanf(argv[1],"%ld",&MaxPts) != 1)
        {
          printf("Err: Bad value for MaxPts\n");
          exit (-1);
        }
    }

  StartI1 = 0;
  StartI2 = 0;

  TriFile  = fopen("qfile","w");
  DataFile = fopen("dfile","w");


  if ((scanf("%ld",&IConnect) <= 0) ||
      (IConnect < 0)               ||
      (IConnect > 1))
    {
      printf("Err: IConnect value bad");
      exit (0);
    }

  if ((scanf("%d",&NumDim) <= 0) ||
      (NumDim < 2)               ||
      (NumDim > 3))
    {
      printf("Err: NDim value must be 2 or 3");
      exit (0);
    }

  if ((scanf("%d",&NumAddVars) <= 0)  ||
      (NumAddVars < 0))
    {
      printf("Err: Bad number of additional values");
      exit (0);
    }

  X1 = (double *)malloc((MaxPts+1)*sizeof(double));
  Y1 = (double *)malloc((MaxPts+1)*sizeof(double));
  if (NumDim == 3)
    Z1 = (double *)malloc((MaxPts+1)*sizeof(double));
  X2 = (double *)malloc((MaxPts+1)*sizeof(double));
  Y2 = (double *)malloc((MaxPts+1)*sizeof(double));
  if (NumDim == 3)
    Z2 = (double *)malloc((MaxPts+1)*sizeof(double));

  if (NumAddVars > 0)
    {
      V1 = (double *)malloc((MaxPts+1)*NumAddVars*sizeof(double));
      V2 = (double *)malloc((MaxPts+1)*NumAddVars*sizeof(double));
    }

  if ((X1 == NULL) || 
      (Y1 == NULL) ||
      (X2 == NULL) || 
      (Y2 == NULL) ||
      ((NumDim == 3) && ((Z1 == NULL) || (Z2 == NULL))) ||
      ((NumAddVars > 0) && ((V1 == NULL) || (V2 == NULL))))
    {
      fprintf(stderr,"Err: Cannot allocate space for temp arrays.\n");
      exit (-1);
    }



  if (!ReadInCircle2())
    {
      printf("Error at start of input file\n");
      exit (0);
    }
  do
    {
      if (!IsStart)
        {
          /*
           * "Connect" the first two points.
           */
          NextI1 = 1;
          NextI2 = 1;
          /*
           *
           */
          while ((NextI1 < NumPts1) || (NextI2 < NumPts2))
            {
              NextD1 = GetDist(NextI1,NextI2-1);
              NextD2 = GetDist(NextI1-1,NextI2);
              if (NextD1 < NextD2)
                {
                  /*
                   * Triangle comes from connecting next point on 
                   * P1 with last connected point in P2.
                   */
                  TriInd[0] = StartI1+NextI1-1;
                  TriInd[1] = StartI1+NextI1;
                  TriInd[2] = StartI2+NextI2-1;
                  NextI1++;
                }
              else
                {
                  /*
                   * Triangle comes from connecting next point on 
                   * P2 with last connected point in P1.
                   */
                  TriInd[0] = StartI2+NextI2-1;
                  TriInd[1] = StartI2+NextI2;
                  TriInd[2] = StartI1+NextI1-1;
                  NextI2++;
                }
              NumTri++;
              fprintf(TriFile,"%ld %ld %ld\n",TriInd[0]+1,
                                              TriInd[1]+1,
                                              TriInd[2]+1);
#if defined MAKEBRICKS
              fprintf(TriFile,"%ld %ld %ld %ld %ld\n",TriInd[0]+1,
                                                      TriInd[0]+1,
                                                      TriInd[1]+1,
                                                      TriInd[2]+1,
                                                      TriInd[0]+1);
#endif
            }
        }

      /* Output another row of points and copy circle 2 to circle 1*/

      for (I2=0;I2<NumPts2;I2++)
        {
          if (NumDim == 3)
            fprintf(DataFile,"%16.12e %16.12e %16.12e",X2[I2],Y2[I2],Z2[I2]);
          else
            fprintf(DataFile,"%16.12e %16.12e",X2[I2],Y2[I2]);
          for (I3=0; I3 < NumAddVars; I3++)
            {
              fprintf(DataFile," %16.12e",V2[I2*NumAddVars+I3]);
              V1[I2*NumAddVars+I3] = V2[I2*NumAddVars+I3];
            }
          fputc('\n',DataFile);
          X1[I2] = X2[I2];
          Y1[I2] = Y2[I2];
          if (NumDim == 3)
            Z1[I2] = Z2[I2];
        }
      NumPts1 = NumPts2;
      StartI1 = StartI2;
      StartI2 += NumPts2;
      IsStart=0;
    }
  while (ReadInCircle2());

  fclose(TriFile );
  fclose(DataFile);
  TriFile  = fopen("qfile","r");
  DataFile = fopen("dfile","r");

  if (NumDim == 3)
    printf("Variables = X,Y,Z");
  else
    printf("Variables = X,Y");
  for (I2 = 0; I2 < NumAddVars; I2++)
    printf(",V%ld",I2+1);
  putchar('\n');
#if defined MAKEBRICKS
  printf("Zone F=FePoint I=%ld J=%ld ET=Brick\n",StartI2,NumTri);
#else
  printf("Zone F=FePoint I=%ld J=%ld ET=Triangle\n",StartI2,NumTri);
#endif

  while ((C=fgetc(DataFile)) != EOF)
    putchar(C);
  while ((C=fgetc(TriFile )) != EOF)
    putchar(C);
  fclose(DataFile);
  fclose(TriFile );
#ifndef VMS
  unlink("qfile");
  unlink("dfile");
#else
  remove("qfile");
  remove("dfile");
#endif
  return 0;
}
