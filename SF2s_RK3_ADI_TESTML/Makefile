#@c                                                                     
########################################################################
#                        MAKEFILE FOR USE WITH SF2S                    #
#----------------------------------------------------------------------#
# NOTES AND RESTRICTIONS:                                              #
#         SET PLATFORM DEPENDENT VARIABLES in                          #
#           arch.in                                                    #
#                                                                      # 
#         This makefile works by building the relevant object          #
#         files in their sub-directory and linking them to make an     #
#         executable in the current  directory.                        #
#----------------------------------------------------------------------#
# CURRENT STATUS:                                                      #
#         Builds dns code, pressure solver test program, interpolation #
#         programs, grid program, post-processing tools and            #
#         documentation.                                               #
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
#         http://cfdlab.ucsd.edu                                       #
#----------------------------------------------------------------------# 
# MAINTENANCE HISTORY:                                                 #
# 29-Apr-09: Original Version                                          # 
# 06-May-09: Added header and saving of preprocessed .F90 files        # 
# 08-July-09: Ensured automatic generation of grid program             # 
########################################################################
#@q                                                                     

########################################################################
########## THERE IS NO NEED TO EDIT BELOW THIS LINE          ###########
##########                                                   ###########
########## VARIABLES BELOW ARE NOT INTENDED TO BE CHANGED    ###########
########## UNLESS THE FILES THAT MAKE UP A MODULE CHANGE OR  ###########
##########             NEW MODULES ARE ADDED                 ###########
########################################################################

.SUFFIXES : .o .a .c .f90 .F90 .f .F 

include ./arch.in

EXEC      = sturb.x
PTEST	  = ptest.x
GRID      = grid.x
IBGEOM    = geometry_IB.x
INTERP1   = interpX1.x
INTERP2   = interpX2.x
INTERP3   = interpX3.x
REST      = restWake.x
STATS2D   = stats2d.x
STATS3D   = stats3d.x
COMBINE3D = combine3DvizFiles.x
PLN       = Pln.x
KXSPEC    = kxspec.x
KXCOSPEC  = kxcospec.x
OMG3EXEC  = omg3_3d.x
OUTLIBi	  = libmg.a

FFLAGS  = $(OBUILD) $(PARALLEL) $(USETEC) $(MODULE) $(OPT) $(IBM_FLAG) $(MMS_TEST)
CFLAGS  = $(OBUILD) $(PARALLEL) $(USETEC) $(MODULE) $(OPT) $(IBM_FLAG) $(MMS_TEST)

#########################################################################
#       Objects for later use compiling executables     	        #
#########################################################################

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

GRIDdir	 = ./grid
GRID1obj = $(GRIDdir)/Grid.o \
	$(UTILSdir)/readINI.o 

IBGEOMobj = $(GRIDdir)/IB_geometry.o \
        $(GRIDdir)/search_ibm27.o
#        $(GRIDdir)/search_ibm21.o

INTERPdir = ./interp
INTERPobj = $(INTERPdir)/Neville.o \
	$(UTILSdir)/string.o \
	$(INTERPdir)/Thomas.o \
	$(INTERPdir)/write_vtk.o \
	$(UTILSdir)/readINI.o 

OMG3obj     = $(INTERPdir)/omg3_3D.o \
	$(UTILSdir)/string.o \

POSTdir	    = ./post
RESTobj     = $(POSTdir)/restart_Wake_Stats.o \
	       $(UTILSdir)/string.o 
Combine3Dobj = $(POSTdir)/combine3Dviz.o 
KXSPECobj   = $(POSTdir)/kxspec.o 
KXCOSPECobj = $(POSTdir)/kxcospectra.o 
STATS2Dobj  = $(POSTdir)/PostPlaneStats_tke_mke_ke_PENCIL.o 
STATS3Dobj  = $(POSTdir)/Temp_test.o 
#STATS2Dobj  = $(POSTdir)/PostPlaneStats.o 
PLNobj      = $(POSTdir)/PostPlane.o 
TECobj      = $(POSTdir)/tecBinary.o 

PSOLVEdir = ./mgMK
PSOLVEobj = $(PSOLVEdir)/mg_allocate.o \
	$(PSOLVEdir)/mg_relax.o \
	$(PSOLVEdir)/mg_bdry.o \
	$(PSOLVEdir)/mg_check_size.o \
	$(PSOLVEdir)/mg_coeff.o \
	$(PSOLVEdir)/mg_cor.o \
	$(PSOLVEdir)/mg_cycle.o \
	$(PSOLVEdir)/mg_grids.o \
	$(PSOLVEdir)/mg_init.o \
	$(PSOLVEdir)/mg_read_params.o \
	$(PSOLVEdir)/mg_residual.o \
	$(PSOLVEdir)/mg_restr.o \
	$(PSOLVEdir)/mg_set.o \
	$(PSOLVEdir)/mg_solver.o \
	$(PSOLVEdir)/mg_trans_pack.o 

PSOLVETESTdir = ./mgMK/test
PSOLVETESTobj = $(PSOLVETESTdir)/test.o \
	$(PSOLVETESTdir)/inputptest.o \
	$(PSOLVETESTdir)/mg_test_cases.o \
	$(PSOLVETESTdir)/check_deriv_coeffs.o 

UTILSdir = ./utils
UTILSobj =$(UTILSdir)/loadGrid.o \
	$(UTILSdir)/PenPln.o \
	$(UTILSdir)/readINI.o \
	$(UTILSdir)/string.o \
	$(UTILSdir)/write_one_var.o  

DNSdir = ./dns
DNSobj =$(DNSdir)/Average.o \
	$(DNSdir)/avgtime.o \
        $(DNSdir)/grid_setup.o \
        $(DNSdir)/BC.o \
        $(DNSdir)/IC.o \
        $(DNSdir)/initialize.o \
        $(DNSdir)/input.o \
        $(DNSdir)/isoSpec.o \
        $(DNSdir)/memManage.o \
        $(DNSdir)/NSsolver_explicit_RK3.o \
        $(DNSdir)/output.o \
        $(DNSdir)/Restart.o \
        $(DNSdir)/Routines.o \
        $(DNSdir)/Sponge.o \
	$(DNSdir)/spat_wake_ics.o \
        $(DNSdir)/statistics.o \
        $(DNSdir)/statistics_cylwake.o \
        $(DNSdir)/StatTools.o \
        $(DNSdir)/Sturb.o \
        $(DNSdir)/IBsource.o \
        $(DNSdir)/ReadIBdata.o \
        $(DNSdir)/ADI_subroutines_colocated.o \
        $(DNSdir)/NSsolver_collocated.o \
        $(DNSdir)/Thomas_serial.o \
        $(DNSdir)/Thomas_x.o \
        $(DNSdir)/Thomas_y.o \
        $(DNSdir)/Thomas_z.o \
        $(DNSdir)/test_thom_periodic.o 

#       $(DNSdir)/ADI_subroutines.o \
        $(DNSdirN)/BC_subroutines_colocated.o \
        $(DNSdir)/BC_subroutines.o 

##################################################
#######     Start creating executables     #######
##################################################

# The main DNS solver
ifeq "$(PARALLEL)" "-DPARALLEL"
DNSSOLVERobj = $(MODULESobj) $(DECOMPobj) $(DNSobj) $(PSOLVEobj) $(UTILSobj)
else
DNSSOLVERobj = $(MODULESobj) $(DNSobj) $(PSOLVEobj) $(UTILSobj)
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
dns: ${DNSSOLVERobj} 
	${FF} ${OPT} ${DEBUG} ${DNSSOLVERobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${EXEC}

# Test program for the pressure solver:
ptest: ${PSOLVEDRIVERobj} 
	${FF} ${OPT} ${DEBUG} ${PSOLVEDRIVERobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${PTEST}

# Interpolation only, make all three programs at once
interp: interp1 interp2 interp3

interp1: ${INTERP1obj}
	${FF} ${OPT} ${DEBUG} ${INTERP1obj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${INTERP1}

interp2: ${INTERP2obj}
	${FF} ${OPT} ${DEBUG} ${INTERP2obj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${INTERP2}

interp3: ${INTERP3obj}
	${FF} ${OPT} ${DEBUG} ${INTERP3obj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${INTERP3}
#	${FF} ${OPT} $(OPENMPFLAG) ${DEBUG} ${INTERP3obj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${INTERP3}
#	-/bin/rm -f ${UTILSdir}/*.o

# Grid only, delete serial version of intermediate files used to create grid.x 
gridp: $(GRIDobj)
	${FF} ${OPT} ${DEBUG} ${GRIDobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${GRID}
	-/bin/rm -f ${UTILSdir}/readINI.o
	-/bin/rm -f ${MODULEdir}/*.mod
	-/bin/rm -f ${MODULEdir}/*.o

ibgeom: $(IBGEOMobj)
	${FF} ${OPT} ${DEBUG} ${IBGEOMobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${IBGEOM}

# make all post-processing programs
post: kxspec kxcospec rest stats2d pln stats3d

# 1D spectra in x1
kxspec: $(KXSPECobj)
	${FF} ${OPT} ${DEBUG} ${KXSPECobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${KXSPEC}

# 1D cospectra in x1
kxcospec: $(KXCOSPECobj)
	${FF} ${OPT} ${DEBUG} ${KXCOSPECobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${KXCOSPEC}

# wake statistics from restart file
rest: $(RESTobj)
	${FF} ${OPT} ${DEBUG} ${RESTobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${REST}
# wake statistics from restart file
omg3: $(OMG3obj)
	${FF} ${OPT} ${DEBUG} ${OMG3obj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${OMG3EXEC}

# 2D wake statistics
ifeq "$(USETEC)" "-DUSETEC"
stats2d: $(STATS2Dobj) $(TECobj)
	${FF} ${OPT} ${DEBUG} ${TECobj} ${STATS2Dobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${TECLIBS} ${LIBS} -o ${RUNDIR}/${STATS2D}
stats3d: $(STATS3Dobj) $(TECobj)
	${FF} ${OPT} ${DEBUG} ${TECobj} ${STATS3Dobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${TECLIBS} ${LIBS} -o ${RUNDIR}/${STATS3D}
else
stats2d: $(STATS2Dobj)
	${FF} ${OPT} ${DEBUG} ${STATS2Dobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${STATS2D}
stats3d: $(STATS3Dobj)
	${FF} ${OPT} ${DEBUG} ${STATS3Dobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${STATS3D}
endif

# 2D planes
ifeq "$(USETEC)" "-DUSETEC"
viz3d: $(Combine3Dobj) $(TECobj)
	${FF} ${OPT} ${DEBUG} $(TECobj) ${Combine3Dobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${TECLIBS} ${LIBS} -o ${RUNDIR}/${COMBINE3D}
pln: $(PLNobj) $(TECobj)
	${FF} ${OPT} ${DEBUG} ${TECobj} ${PLNobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${TECLIBS} ${LIBS} -o ${RUNDIR}/${PLN}
else
viz3d: $(Combine3Dobj)
	${FF} ${OPT} ${DEBUG} ${Combine3Dobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${COMBINE3D}
pln: $(PLNobj)
	${FF} ${OPT} ${DEBUG} ${PLNobj} ${IOFLAG} ${LIBDIRS} ${INCDIRS} ${LIBS} -o ${RUNDIR}/${PLN}
endif

# This redefines FFLAGS so that only a serial version of the grid program will be built
gridp: FFLAGS = $(OBUILD) $(MODULE)
# -mcmodel=medium or -mcmodel=large are supposed to allow for compilation using lots of memory (>2GB) did not work for me on july 19 2011
#ibgeom: FFLAGS = $(OBUILD) -mcmodel=large -shared-intel
ibgeom: FFLAGS = $(OBUILD) 

# This redefines FFLAGS so that a threaded with openmp version of interpX3 is built
#interp3: FFLAGS = $(OPT) $(OBUILD) $(MODULE) $(OPENMPFLAG)
#interp1: FFLAGS = $(OBUILD) $(MODULE)
#interp2: FFLAGS = $(OBUILD) $(MODULE)
#interp3: FFLAGS = $(OBUILD) $(MODULE)

# Code documentation
doc: 
	cd docs/; make; ./createdoc; cp manual.pdf ..; cd ..

# Create a library file for the pressure solver
mglib: $(PSOLVEobj)
	$(AR) rusv $(OUTLIBi) $(PSOLVEobj)

# This forces the below object files to be updated if the modules are updated
#${PSOLVEDRIVERobj} : $(MODULEdir)
#${DNSSOLVERobj} : $(MODULEdir)
#${GRIDobj} : $(MODULEdir)

clean: 
	-/bin/rm -f *.o *.mod *.oo *.x lib*.a *.pdf

cleanall: 
	-/bin/rm -f $(MODULEdir)/*.o $(MODULEdir)/*.mod $(MODULEdir)/*.oo $(MODULEdir)/*_mod.f90 $(MODULEdir)/lib*.a $(MODULEdir)/*.ipa 
	-/bin/rm -f $(DECOMPdir)/*.o $(DECOMPdir)/*.ipa $(DECOMPdir)/*.oo $(DECOMPdir)/*.x $(DECOMPdir)/lib*.a 
	-/bin/rm -f $(DNSdir)/*.o $(DNSdir)/*.ipa $(DNSdir)/*.oo $(DNSdir)/*.x $(DNSdir)/lib*.a 
	-/bin/rm -f $(GRIDdir)/*.o $(GRIDdir)/*.ipa $(GRIDdir)/*.oo $(GRIDdir)/*.x $(GRIDdir)/*.in 
	-/bin/rm -f $(INTERPdir)/*.o $(INTERPdir)/*.ipa $(INTERPdir)/*.oo $(INTERPdir)/*.x $(INTERPdir)/lib*.a 
	-/bin/rm -f $(POSTdir)/*.o $(POSTdir)/*.ipa $(POSTdir)/*.oo $(POSTdir)/*.x $(POSTdir)/lib*.a  $(POSTdir)/*.mod
	-/bin/rm -f $(PSOLVEdir)/*.o $(PSOLVEdir)/*.ipa $(PSOLVEdir)/*.oo $(PSOLVEdir)/*.x $(PSOLVEdir)/lib*.a 
	-/bin/rm -f $(PSOLVETESTdir)/*.o $(PSOLVETESTdir)/*.ipa $(PSOLVETESTdir)/*.oo $(PSOLVETESTdir)/*.x $(PSOLVETESTdir)/lib*.a 
	-/bin/rm -f $(UTILSdir)/*.o $(UTILSdir)/*.ipa $(UTILSdir)/*.oo $(UTILSdir)/*.x $(UTILSdir)/lib*.a
	-/bin/rm -rf  $(MODULEdir)/source $(DECOMPdir)/source $(DNSdir)/source $(GRIDdir)/source $(INTERPdir)/source 
	-/bin/rm -rf $(PSOLVEdir)/source $(PSOLVETESTdir)/source $(UTILSdir)/source $(POSTdir)/source
	-/bin/rm -f *.f90 *.mod
	cd ./docs/; ./cleanup; cd ..

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
