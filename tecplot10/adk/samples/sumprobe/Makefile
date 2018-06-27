SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

IOUTILLIB=$(EXTLIBDIR)/libioutil.a
GUILIB=$(EXTLIBDIR)/libgui.a

shlib: $(LIB)



#
# list of obj files....
#


OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/guidefs.o \
     $(OBJDIR)/guicb.o
EXTMAINHEADERS=GUIDEFS.h


$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(GUILIB) -o $(LIB)

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.c $(EXTMAINHEADERS)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../main.c 

$(OBJDIR)/guidefs.o : guidefs.c guibld.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../guidefs.c 
$(OBJDIR)/guicb.o : guicb.c guibld.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../guicb.c
 







 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

