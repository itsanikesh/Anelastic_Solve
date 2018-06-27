SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

IOUTILLIB=$(EXTLIBDIR)/libioutil.a
GLUELIB=$(EXTLIBDIR)/libfglue.a
GUILIB=$(EXTLIBDIR)/libgui.a

shlib: $(LIB)










OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/util.o \
     $(OBJDIR)/guicb.o
EXTMAINHEADERS=GUIDEFS.INC GUICB.INC

 
#
# The next dependency probably can remain unchanged...
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(GUILIB) $(GLUELIB) $(FLIBS) -o $(LIB)
 
#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.F $(EXTMAINHEADERS) CELLCTR.INC
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) $(SOURCEDIR)/main.F 
$(OBJDIR)/util.o : util.F $(EXTMAINHEADERS) CELLCTR.INC
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) $(SOURCEDIR)/util.F 
$(OBJDIR)/guicb.o : guicb.F guibld.F GUIDEFS.INC GUICB.INC CELLCTR.INC
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) $(SOURCEDIR)/guicb.F
 







 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

