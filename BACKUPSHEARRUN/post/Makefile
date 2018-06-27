###########################################################################
#                 BINARY PLANE POST PROCESSING MAKEFILE                   #
#                     v.0 07/24/2008 Kyle A. Brucker                      #
###########################################################################
.SUFFIXES : .o .f90 .a
FF	=ifort
IOFLAG	=
DEBUG	=-check
OPT	=-O3 
RUNDIR	=.
LIBDIRS =-L/p/home/anikeshp/fftw-3.2.1/lib
INCDIRS =-I/p/home/anikeshp/fftw-3.2.1/include
LIBS    =/p/home/anikeshp/tecplot10/lib/tecio64.a -lstdc++ -lfftw3
EXECTIM	=time.x
EXEC3D	=c3d.x
EXEC2D	=stats2d.x
EXECPLN =Pln.x
EXECKX  =kxspec.x
TIME    =gtime.o
PLN     =PostPlane.o
D2	=PostPlaneStats.o
D3	=Combine2dplanes_3d.o 
KXSPEC  =kxspec.o
OBJECTS	=tecBinary.o 

Stat2d: ${OBJECTS} ${D2}
	${FF} ${OPT} ${DEBUG} ${OBJECTS} ${D2} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${EXEC2D}

Stat3d: ${OBJECTS} ${D3}
	${FF} ${OPT} ${DEBUG} ${OBJECTS} ${D3} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${EXEC3D}

time:	${TIME}
	${FF} ${OPT} ${DEBUG} ${TIME} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${EXECTIM}

Pln: ${OBJECTS} ${PLN}
	${FF} ${OPT} ${DEBUG} ${OBJECTS} ${PLN} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${EXECPLN}

kxspec: ${OBJECTS} ${KXSPEC}
	${FF} ${OPT} ${DEBUG} ${OBJECTS} ${KXSPEC} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${EXECKX}

.f90.o:
	${FF} ${OPT} ${DEBUG} ${INCDIRS} ${IOFLAG} -c ${FLAGS} $< ${OTHER}

clean:
	-/bin/rm -f *.o *.mod *.oo *.x 

proper: clean
	-/bin/rm -f *~

