.SUFFIXES : .o .a .c .f90 .F90 .f .F 
########################################################################
#                      MAKEFILE FOR USE WITH SF2S                      #
#----------------------------------------------------------------------#
# NOTES AND RESTRICTIONS:                                              #
#         SET PLATFORM DEPENDENT VARIABLES in                          #
#           arch.in                                                    #
#                                                                      # 
#         This makefile works by building the relevant object          #
#         files in their sub-directory and linking them to make an     #
#         executable in the current  directory                         #
#----------------------------------------------------------------------#
# CURRENT STATUS:                                                      #
#         Builds dns code, pressure solver test program interpolation  #
#         programs and Grid program.                                  #
#         Does not build postprocessing or documentation               #   
#         A rule should be implemented so that that if modules.o       #
#         changes a make clean will automatically be occur             # 
#----------------------------------------------------------------------#
# AUTHOR: Matt de Stadler                                              # 
#         University of California San Diego                           #
#         Any suggestions to improve this code are welcome.            #
#         Send mail to sf2s@gmail.com                                  # 
#----------------------------------------------------------------------#
# LEGAL:  This code can be used freely EXCEPT for                      #
#         commercial purposes.  COMMERCIAL USE IS NOT PERMITTED        # 
#         UNLESS WRITTEN CONSENT IS PROVIDED BY THE AUTHOR.            #
#         There is no guarantee that this code is bug-free.            #
#----------------------------------------------------------------------#
# ACKNOWLEDGEMENTS:                                                    # 
#         This code has been developed by students in the              #
#         Computational Fluid Dynamics Lab at the University of        #
#         California San Diego, under the direction of Prof.           #
#         Sutanu Sarkar.                                               #
#----------------------------------------------------------------------# 
# MAINTENANCE HISTORY:                                                 #
# 29-Apr-09: Original Version                                          # 
# 06-May-09: Added header and saving of preprocessed .F90 files        # 
########################################################################

########################################################################
########## USUALLY THERE IS NO NEED TO EDIT BELOW THIS LINE  ###########
########## VARIABLES BELOW ARE NOT INTENDED TO BE CHANGED    ###########
########## UNLESS THE FILES THAT MAKE UP A MODULE CHANGE OR  ###########
##########             NEW MODULES ARE ADDED                 ###########
########################################################################

include ./arch.in

EXEC    = sturb.x
PTEST	= ptest.x
GRID    = grid.x
INTERP1 = interpX1.x
INTERP2 = interpX2.x
INTERP3 = interpX3.x
OUTLIBi	= libmg.a

FFLAGS  = $(OBUILD) $(PARALLEL) $(MODULE) $(OPT) 
CFLAGS  = $(OBUILD) $(PARALLEL) $(MODULE) $(OPT)

MODULEdir  = ./include
MODULESobj = $(MODULEdir)/modules.o \
	$(MODULEdir)/mg_modules.o

DECOMPdir = ./decomp
DECOMPobj = $(DECOMPdir)/dist1dC.o \
	$(DECOMPdir)/dist1d.o \
	$(DECOMPdir)/dist2dC.o \
	$(DECOMPdir)/dist2d.o \
	$(DECOMPdir)/dist3d.o \
	$(DECOMPdir)/gather1dC.o \
	$(DECOMPdir)/gather1d.o \
	$(DECOMPdir)/gather2dC.o \
	$(DECOMPdir)/gather2d.o \
	$(DECOMPdir)/gather3d.o \
	$(DECOMPdir)/Gpencil.o \
	$(DECOMPdir)/Gplane.o \
	$(DECOMPdir)/ghost.o\
	$(DECOMPdir)/mpi_setup.o

DNSdir = ./dns
DNSobj =$(DNSdir)/Average.o \
	$(DNSdir)/grid_setup.o \
	$(DNSdir)/BC.o \
	$(DNSdir)/IC.o \
	$(DNSdir)/initialize.o \
	$(DNSdir)/input.o \
	$(DNSdir)/isoSpec.o \
	$(DNSdir)/memManage.o \
	$(DNSdir)/NSsolver.o \
	$(DNSdir)/output.o \
	$(DNSdir)/Restart.o \
	$(DNSdir)/Routines.o \
	$(DNSdir)/Sponge.o \
	$(DNSdir)/statistics.o \
	$(DNSdir)/StatTools.o \
	$(DNSdir)/Thomas_z.o \
	$(DNSdir)/Sturb.o 


LESdir = ./les
LESobj =$(LESdir)/filter.o \
	$(LESdir)/straincal.o \
	$(LESdir)/SSM.o \
        $(LESdir)/DSMrho.o \
        $(LESdir)/DSMscal1.o \
	$(LESdir)/DSM.o \
	$(LESdir)/DMM.o 
 
#	$(DNSdir)/stats_temp.o \
# THE STATISTICS OBJECTS SHOULD BE CLEANED UP ONCE THINGS ARE FINISHED


GRIDdir	 = ./grid/grid
GRID1obj = $(GRIDdir)/Grid.o \
	$(UTILSdir)/readINI.o 

INTERPdir = ./interp
INTERPobj = $(INTERPdir)/Neville.o \
	$(UTILSdir)/string.o \
	$(INTERPdir)/write_vtk.o 

PSOLVEdir = ./mgMK
PSOLVEobj = $(PSOLVEdir)/mg_allocate.o \
	$(PSOLVEdir)/mg_bdry.o \
	$(PSOLVEdir)/mg_check_size.o \
	$(PSOLVEdir)/mg_coeff.o \
	$(PSOLVEdir)/mg_cor.o \
	$(PSOLVEdir)/mg_cycle.o \
	$(PSOLVEdir)/mg_end.o \
	$(PSOLVEdir)/mg_grids.o \
	$(PSOLVEdir)/mg_hydro.o \
	$(PSOLVEdir)/mg_init.o \
	$(PSOLVEdir)/mg_read_params.o \
	$(PSOLVEdir)/mg_relax.o \
	$(PSOLVEdir)/mg_residual.o \
	$(PSOLVEdir)/mg_restr.o \
	$(PSOLVEdir)/mg_set.o \
	$(PSOLVEdir)/mg_set_params.o \
	$(PSOLVEdir)/mg_solver.o \
	$(PSOLVEdir)/mg_sor_sorch.o \
	$(PSOLVEdir)/mg_trans.o \
        $(PSOLVEdir)/mg_phpde.o \
        $(PSOLVEdir)/mg_pfpde.o 

PSOLVETESTdir = ./mgMK/test
PSOLVETESTobj = $(PSOLVETESTdir)/test.o \
	$(PSOLVETESTdir)/inputptest.o \
	$(PSOLVETESTdir)/startup.o \
	$(PSOLVETESTdir)/write_solution.o  

UTILSdir = ./utils
UTILSobj =$(UTILSdir)/loadGrid.o \
	$(UTILSdir)/PenPln.o \
	$(UTILSdir)/readINI.o \
	$(UTILSdir)/string.o 

# The main DNS solver
ifeq "$(PARALLEL)" "-DPARALLEL"
DNSSOLVERobj = $(MODULESobj) $(DECOMPobj) $(DNSobj) $(LESobj) $(PSOLVEobj) $(UTILSobj)
else
DNSSOLVERobj = $(MODULESobj) $(DNSobj) $(LESobj) $(PSOLVEobj) $(UTILSobj)
endif
# The test program for the pressure solver
ifeq "$(PARALLEL)" "-DPARALLEL"
PSOLVEDRIVERobj = $(MODULESobj) $(PSOLVEobj) $(DECOMPobj) $(PSOLVETESTobj) $(UTILSobj)
else
PSOLVEDRIVERobj = $(MODULESobj) $(PSOLVEobj) $(PSOLVETESTobj) $(UTILSobj)
endif

# The interpolation stand alone programs
INTERP1obj = $(INTERPobj) $(INTERPdir)/InterpX1.o 
INTERP2obj = $(INTERPobj) $(INTERPdir)/InterpX2.o 
INTERP3obj = $(INTERPobj) $(INTERPdir)/InterpX3.o 

# Grid program
GRIDobj = $(MODULESobj) $(GRID1obj) 

# DNS solver: 
all: ${DNSSOLVERobj} 
	${FF} ${OPT} ${DEBUG} ${DNSSOLVERobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${EXEC}

# Test program for the pressure solver:
ptest: ${PSOLVEDRIVERobj} 
	${FF} ${OPT} ${DEBUG} ${PSOLVEDRIVERobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${PTEST}

# Interpolation only
interp1: ${INTERP1obj}
	${FF} ${OPT} ${DEBUG} ${INTERP1obj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${INTERP1}

interp2: ${INTERP2obj}
	${FF} ${OPT} ${DEBUG} ${INTERP2obj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${INTERP2}

interp3: ${INTERP3obj}
	${FF} ${OPT} ${DEBUG} ${INTERP3obj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${INTERP3}

# Grid only
gridp: $(GRIDobj)
	${FF} ${OPT} ${DEBUG} ${GRIDobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${GRID}

# Post-processing only
# post: 

# Create a library file for the pressure solver
mglib: $(PSOLVEobj)
	$(AR) rusv $(OUTLIBi) $(PSOLVEobj)

# This forces the below object files to be updated if the modules are updated
#${PSOLVEDRIVERobj} : $(MODULEdir)
#${DNSSOLVERobj} : $(MODULEdir)
#${GRIDobj} : $(MODULEdir)

clean: 
	-/bin/rm -f *.o *.mod *.oo *.x lib*.a *.ipa 

cleanall: 
	-/bin/rm -f $(MODULEdir)/*.o $(MODULEdir)/*.mod $(MODULEdir)/*.oo $(MODULEdir)/*.x $(MODULEdir)/lib*.a 
	-/bin/rm -f $(DECOMPdir)/*.o $(DECOMPdir)/*.mod $(DECOMPdir)/*.oo $(DECOMPdir)/*.x $(DECOMPdir)/lib*.a 
	-/bin/rm -f $(DNSdir)/*.o $(DNSdir)/*.mod $(DNSdir)/*.oo $(DNSdir)/*.x $(DNSdir)/lib*.a 
	-/bin/rm -f $(LESdir)/*.o $(LESdir)/*.mod $(LESdir)/*.oo $(LESdir)/*.x $(LESdir)/lib*.a 
	-/bin/rm -f $(GRIDdir)/*.o $(GRIDdir)/*.mod $(GRIDdir)/*.oo $(GRIDdir)/*.x $(GRIDdir)/lib*.a $(GRIDdir)/*.in 
	-/bin/rm -f $(INTERPdir)/*.o $(INTERPdir)/*.mod $(INTERPdir)/*.oo $(INTERPdir)/*.x $(INTERPdir)/lib*.a 
	-/bin/rm -f $(PSOLVEdir)/*.o $(PSOLVEdir)/*.mod $(PSOLVEdir)/*.oo $(PSOLVEdir)/*.x $(PSOLVEdir)/lib*.a 
	-/bin/rm -f $(PSOLVETESTdir)/*.o $(PSOLVETESTdir)/*.mod $(PSOLVETESTdir)/*.oo $(PSOLVETESTdir)/*.x $(PSOLVETESTdir)/lib*.a 
	-/bin/rm -f $(UTILSdir)/*.o $(UTILSdir)/*.mod $(UTILSdir)/*.oo $(UTILSdir)/*.x $(UTILSdir)/lib*.a
	-/bin/rm -rf  $(MODULEdir)/source $(DECOMPdir)/source $(DNSdir)/source $(GRIDdir)/source $(INTERPdir)/source 
	-/bin/rm -rf $(PSOLVEdir)/source $(PSOLVETESTdir)/source $(UTILSdir)/source $(LESdir)/source
	cd ./doc/; ./cleanup; cd ..


#########################################################################
#         Suffixes for compiling most normal F90/f90 files     	        #
#########################################################################
.F90.o :
	mkdir -p ${@D}/source 2> /dev/null
	${CPP} $(CPPFLAGS) $< > ${@D}/source/${*F}.f90
	$(FF) $(FFLAGS) $(DEBUG) $(INCDIRS) -c $< -o $(@D)/$(@F)

.f90.o :
	$(FF) $(FFLAGS) $(DEBUG) $(INCDIRS) -c $< -o $(@D)/$(@F)

.c.o :
	$(CC) $(CFLAGS) $(DEBUG) $(INCDIRS) -c $< -o $(@D)/$(@F)

.f.o :
	$(FF) $(FFLAGS) $(DEBUG) $(INCDIRS) -c $< -o $(@D)/$(@F)
