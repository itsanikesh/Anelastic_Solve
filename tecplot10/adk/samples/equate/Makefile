LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
STATLIB=$(TECLIBDIR)/lib$(LIBBASENAME).a
OBJDIR=obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)
IOUTILLIB=$(EXTLIBDIR)/libioutil.a
 
shlib: $(LIB)
statlib: $(STATLIB)
#
# list of obj files....
#
OBJS=$(OBJDIR)/main.o $(OBJDIR)/guicb.o $(OBJDIR)/guidefs.o
 
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

$(OBJDIR)/guidefs.o : guidefs.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../guidefs.c
$(OBJDIR)/main.o : main.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../main.c 
$(OBJDIR)/guicb.o : guicb.c guibld.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../guicb.c 
 
 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1
