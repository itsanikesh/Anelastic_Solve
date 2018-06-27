SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

IOUTILLIB=$(EXTLIBDIR)/libioutil.a
GLUELIB=$(EXTLIBDIR)/libfglue.a

shlib: $(LIB)







OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/guicb.o \
     $(OBJDIR)/engine.o
EXTMAINHEADERS=GUIDEFS.INC GUICB.INC



 
#
# The next dependency probably can remain unchanged...
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(GLUELIB) $(FLIBS) -o $(LIB)
 
#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.F $(EXTMAINHEADERS)
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) ../../../main.F 

$(OBJDIR)/guicb.o : guicb.F guibld.F GUIDEFS.INC GUICB.INC
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) ../../../guicb.F
 
$(OBJDIR)/engine.o : engine.F
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) ../../../engine.F






 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

