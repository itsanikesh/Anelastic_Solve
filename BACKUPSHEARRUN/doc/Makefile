###########################################################################
#                         DOCUMENTATION MAKE FILE                         #
#                     v.0 04/22/2009 Kyle A. Brucker                      #
###########################################################################
.SUFFIXES : .o .f90 .a .F90 .c
FF	=ftn -DPARALLEL 
DEBUG	=
CC	=cc
AR	=ar
FFLAGS	=
CFLAGS	=
OTHER	=
INCDIRS =-I../
DSBOBJ	=dsbdoc.o
	
dsbdoc:   ${DSBOBJ}
	${CC} ${DSBOBJ} -o $@.x

.c.o:
	${CC} ${OPT} ${DEBUG} ${INCDIRS} -c ${CFLAGS} $< ${OTHER}

clean:
	-/bin/rm -f *.o *.mod *.oo *.x *.a 

proper: clean
	-/bin/rm -f *~ 

