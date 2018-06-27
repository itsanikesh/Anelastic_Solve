SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

shlib: $(LIB)

$$IF(!AW_FORTRANLANGUAGE)
#
# list of obj files....
#
$$IF(AW_HASENGINE)
$$IF(AW_USETGB)
OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/engine.o \
     $(OBJDIR)/guicb.o \
     $(OBJDIR)/guidefs.o
$$ENDIF
$$IF(!AW_USETGB)
OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/engine.o
$$ENDIF
$$ENDIF
$$IF(!AW_HASENGINE)
$$IF(AW_USETGB)
OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/guicb.o \
     $(OBJDIR)/guidefs.o
$$ENDIF
$$IF(!AW_USETGB)
OBJS=$(OBJDIR)/main.o
$$ENDIF
$$ENDIF

EHEADER=
GHEADER=
$$IF(AW_HASENGINE)
EHEADER=ENGINE.h
$$ENDIF
$$IF(AW_USETGB)
GHEADER=GUIDEFS.h
$$ENDIF
$$ENDIF


$$IF(AW_CLANGUAGE)
#
# Main link command:
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o -o $(LIB)

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.c ADDGLBL.h $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/main.c 
$$IF(AW_USETGB)
$(OBJDIR)/guidefs.o : guidefs.c guibld.c $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/guidefs.c 
$(OBJDIR)/guicb.o : guicb.c guibld.c ADDGLBL.h $(GHEADER)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/guicb.c
$$ENDIF
$$IF(AW_HASENGINE)
$(OBJDIR)/engine.o : engine.c ADDGLBL.h ENGINE.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/engine.c
$$ENDIF
$$ENDIF

$$IF(AW_CPPLANGUAGE)
#
# Main link command:
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(CPPLD) $(CPPLDFLAGS) *.o -o $(LIB)

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.cpp $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CPPCOMP) $(CPPFLAGS) $(SOURCEDIR)/main.cpp
$$IF(AW_USETGB)
$(OBJDIR)/guidefs.o : guidefs.cpp guibld.cpp $(GHEADER)
	cd $(OBJDIR); $(CPPCOMP) $(CPPFLAGS) $(SOURCEDIR)/guidefs.cpp
$(OBJDIR)/guicb.o : guicb.cpp guibld.cpp $(GHEADER)
	cd $(OBJDIR); $(CPPCOMP) $(CPPFLAGS) $(SOURCEDIR)/guicb.cpp
$$ENDIF
$$IF(AW_HASENGINE)
$(OBJDIR)/engine.o : engine.cpp ENGINE.h
	cd $(OBJDIR); $(CPPCOMP) $(CPPFLAGS) $(SOURCEDIR)/engine.cpp
$$ENDIF
$$ENDIF

$$IF(AW_FORTRANLANGUAGE)

GLUELIB=$(EXTLIBDIR)/libfglue.a

#
# list of obj files....
#
$$IF(AW_HASENGINE)
$$IF(AW_USETGB)
OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/engine.o \
     $(OBJDIR)/guicb.o
$$ENDIF
$$IF(!AW_USETGB)
OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/engine.o
$$ENDIF
$$ENDIF

$$IF(!AW_HASENGINE)
$$IF(AW_USETGB)
OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/guicb.o
$$ENDIF
$$IF(!AW_USETGB)
OBJS=$(OBJDIR)/main.o
$$ENDIF
$$ENDIF




#
# Main link command:
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(GLUELIB) $(FLIBS) -o $(LIB)

EINC=
GINC=
$$IF(AW_HASENGINE)
EINC=ENGINE.INC
$$ENDIF
$$IF(AW_USETGB)
GINC=GUIDEFS.INC GUICB.INC
$$ENDIF
 
#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.F ADDGLBL.INC $(EINC) $(GINC)
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) $(SOURCEDIR)/main.F 
$$IF(AW_USETGB)
$(OBJDIR)/guicb.o : guicb.F guibld.F ADDGLBL.INC $(GINC) 
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) $(SOURCEDIR)/guicb.F
$$ENDIF
$$IF(AW_HASENGINE)
$(OBJDIR)/engine.o : ADDGLBL.INC $(EINC)
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) $(SOURCEDIR)/engine.F
$$ENDIF
$$ENDIF
 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

