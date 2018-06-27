SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
STATLIB=$(ADDONLIBDIR)/lib$(LIBBASENAME).a
OBJDIR=obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)
IOUTILLIB=$(EXTLIBDIR)/libioutil.a
GLUELIB=$(EXTLIBDIR)/libfglue.a
GUILIB=$(EXTLIBDIR)/libgui.a
 
shlib: $(LIB)
statlib: $(STATLIB)
#
# list of obj files....
#
OBJS=$(OBJDIR)/sinewave.o \
		 $(OBJDIR)/guicb.o \
		 $(OBJDIR)/util.o
 
#
# The next two dependencies probably can remain unchanged...
#
 
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(GLUELIB) $(GUILIB) $(FLIBS) -o $(LIB)
 
$(STATLIB) : $(OBJS)
	cd $(OBJDIR); $(AR) q lib$(LIBBASENAME).a *.o $(GUILIB); mv lib$(LIBBASENAME).a  $(STATLIB)
 
#
# Duplicate the next line for additional source files...
#
 
$(OBJDIR)/sinewave.o : sinewave.F GUIDEFS.INC GUICB.INC
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) ../../../sinewave.F 
$(OBJDIR)/util.o : util.F GUIDEFS.INC GUICB.INC SINEWAVE.INC
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) ../../../util.F 
$(OBJDIR)/guicb.o : guicb.F guibld.F
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) ../../../guicb.F 
 
 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1
