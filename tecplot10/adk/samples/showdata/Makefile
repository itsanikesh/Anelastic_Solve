SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

IOUTILLIB=$(EXTLIBDIR)/libioutil.a
# GUILIB=$(EXTLIBDIR)/libgui.a
GUILIB=

shlib: $(LIB)



#
# list of obj files....
#


OBJS=$(OBJDIR)/showdata.o \
     $(OBJDIR)/guidefs.o \
     $(OBJDIR)/guicb.o
EXTMAINHEADERS=GUIDEFS.h


$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(GUILIB) -o $(LIB) >/dev/null 2>&1

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/showdata.o : showdata.c $(EXTMAINHEADERS)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../showdata.c 

$(OBJDIR)/guidefs.o : guidefs.c guibld.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../guidefs.c 
$(OBJDIR)/guicb.o : guicb.c guibld.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../guicb.c
 







 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

