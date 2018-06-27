SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
STATLIB=$(ADDONLIBDIR)/lib$(LIBBASENAME).a
OBJDIR=obj/$(PLATFORM)/$(OBJSUBDIR)
IOUTILLIB=$(EXTBASEDIR)/$(PLATFORM)/libioutil.a
GUILIB=$(EXTBASEDIR)/$(PLATFORM)/libgui.a
 
shlib: $(LIB)
statlib: $(STATLIB)
#
# list of obj files....
#
OBJS=$(OBJDIR)/doexport.o \
     $(OBJDIR)/exbmp.o \
     $(OBJDIR)/guicb.o \
     $(OBJDIR)/guidefs.o
 
#
# The next two dependencies probably can remain unchanged...
#
 
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(GUILIB) -o $(LIB)
 
$(STATLIB) : $(OBJS)
	cd $(OBJDIR); $(AR) q lib$(LIBBASENAME).a *.o $(GUILIB); mv lib$(LIBBASENAME).a  $(STATLIB)
 
#
# Duplicate the next line for additional source files...
#
 
$(OBJDIR)/doexport.o : doexport.c EXBMP.h GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../doexport.c 
$(OBJDIR)/exbmp.o : exbmp.c EXBMP.h GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../exbmp.c 
$(OBJDIR)/guicb.o : guicb.c guibld.c EXBMP.h GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../guicb.c 
$(OBJDIR)/guidefs.o : guidefs.c EXBMP.h GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../guidefs.c 
 
 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1
