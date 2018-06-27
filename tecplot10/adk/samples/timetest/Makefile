SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
STATLIB=$(ADDONLIBDIR)/lib$(LIBBASENAME).a
OBJDIR=obj/$(PLATFORM)/$(OBJSUBDIR)
IOUTILLIB=$(EXTBASEDIR)/$(PLATFORM)/libioutil.a
 
shlib: $(LIB)
statlib: $(STATLIB)
#
# list of obj files....
#
OBJS=$(OBJDIR)/timetest.o $(OBJDIR)/guidefs.o $(OBJDIR)/guicb.o
 
#
# The next two dependencies probably can remain unchanged...
#
 
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o -o $(LIB) >/dev/null 2>&1
 
$(STATLIB) : $(OBJS)
	cd $(OBJDIR); $(AR) q lib$(LIBBASENAME).a *.o; mv lib$(LIBBASENAME).a  $(STATLIB)
 
#
# Duplicate the next line for additional source files...
#
 
$(OBJDIR)/timetest.o : timetest.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../timetest.c 
$(OBJDIR)/guidefs.o : guidefs.c guibld.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../guidefs.c 
$(OBJDIR)/guicb.o : guicb.c guibld.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../guicb.c 
 
 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1
