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

OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/util.o




#
# Main link command:
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(GLUELIB) $(FLIBS) -o $(LIB)

EINC=
 
#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.F $(EINC) 
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) $(SOURCEDIR)/main.F 
$(OBJDIR)/util.o : util.F $(EINC) 
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) $(SOURCEDIR)/util.F 
 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

