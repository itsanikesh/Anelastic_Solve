SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

IOUTILLIB=$(EXTLIBDIR)/libioutil.a

shlib: $(LIB)



#
# list of obj files....
#


OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/engine.o
EXTMAINHEADERS=ENGINE.h



$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o -o $(LIB)

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.c $(EXTMAINHEADERS)
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../main.c 

 
$(OBJDIR)/engine.o : engine.c ENGINE.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) ../../../engine.c







 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

