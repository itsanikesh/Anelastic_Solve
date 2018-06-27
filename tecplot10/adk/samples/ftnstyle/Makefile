SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

IOUTILLIB=$(EXTLIBDIR)/libioutil.a

shlib: $(LIB)






GLUELIB=$(EXTLIBDIR)/libfglue.a

#
# list of obj files....
#

OBJS=$(OBJDIR)/main.o




#
# Main link command:
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(GLUELIB) $(FLIBS) -o $(LIB)

EINC=
GINC=
 
#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.F $(EINC) $(GINC)
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) $(SOURCEDIR)/main.F 
 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

