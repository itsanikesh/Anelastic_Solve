C
C Simple example fortran program to write a
C binary datafile for tecplot.  This example
C does the following:
C
C   1.  Open a datafile called "t.plt"
C   2.  Assign values for X,Y, and P
C   3.  Write out a zone dimensioned 4x5
C   4.  Close the datafile.
C
C
      program test

      INCLUDE 'tecio.inc'

      character*1 NULLCHR
      Integer*4   Debug,III,NPts,NElm

      Dimension X(4,5), Y(4,5), P(4,5)
      Integer*4 TecIni,TecDat,TecZne,TecNod,TecFil
      Integer*4 VIsDouble

      NULLCHR = CHAR(0)
      Debug   = 1
      VIsDouble = 0
      IMax    = 4
      JMax    = 5
      KMax    = 1
C
C... Open the file and write the tecplot datafile 
C... header information.
C
      I = TecIni('SIMPLE DATASET'//NULLCHR,
     &           'X Y P'//NULLCHR,
     &           't.plt'//NULLCHR,
     &           '.'//NULLCHR,
     &           Debug,
     &           VIsDouble)

      Do 10 I = 1,4
      Do 10 J = 1,5
        X(I,J) = I
        Y(I,J) = J
        P(I,J) = I*J
   10 Continue
C
C... Write the zone header information.
C
      I = TecZne('Simple Zone'//NULLCHR,
     &           IMax,
     &           JMax,
     &           KMax,
     &           'BLOCK'//NULLCHR,
     &           NULLCHR)
C
C... Write out the field data.
C
      III = IMax*JMax
      I   = TecDat(III,X,0)
      I   = TecDat(III,Y,0)
      I   = TecDat(III,P,0)

      I = TecEnd()
      Stop
      End
