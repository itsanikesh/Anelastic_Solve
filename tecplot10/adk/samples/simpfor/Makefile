SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
STATLIB=$(ADDONLIBDIR)/lib$(LIBBASENAME).a
OBJDIR=obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)
IOUTILLIB=$(EXTLIBDIR)/libioutil.a
GLUELIB=$(EXTLIBDIR)/libfglue.a
GUILIB=
GUIHEADERS=
 
shlib: $(LIB)
statlib: $(STATLIB)
#
# list of obj files....
#
OBJS=$(OBJDIR)/simpfor.o
 
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
 
$(OBJDIR)/simpfor.o : simpfor.F 
	cd $(OBJDIR); $(FCOMP) $(FFLAGS) ../../../simpfor.F 
 
 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1
