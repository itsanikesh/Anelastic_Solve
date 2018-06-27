###########################################################################
#                         GRID WEIGHTING FACTORS                          #
#                     v.0 02/03/200 Kyle A. Brucker                       #
###########################################################################
.SUFFIXES : .o .f90 .a
FF	=ifort
IOFLAG	=
DEBUG	=-check -fp-stack-check
OPT	=-O 
RUNDIR	=.
LIBDIRS =
INCDIRS =
LIBS  	=/opt/tecplot360/lib/tecio64.a -lstdc++ 
SGF	=sgf.o
EXECSGF =sgf.x
TECOBJ	=tecBinary.o
VTKOBJ	=vtkBinary.o 

sgf:	${TECOBJ} ${VTKOBJ} ${SGF}
	${FF} ${OPT} ${DEBUG} ${TECOBJ} ${VTKOBJ} ${SGF} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${EXECSGF}

.f90.o:
	${FF} ${OPT} ${DEBUG} ${INCDIRS} ${IOFLAG} -c ${FLAGS} $< ${OTHER}

clean:
	-/bin/rm -f *.o *.mod *.oo *.x 

proper: clean
	-/bin/rm -f *~

