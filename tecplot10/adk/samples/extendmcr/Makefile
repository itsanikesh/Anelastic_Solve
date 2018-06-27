SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)
ADKUTILLIB=$(EXTLIBDIR)/libadkutil.a
IOUTILLIB=$(EXTLIBDIR)/libioutil.a

shlib: $(LIB)



#
# list of obj files....
#



OBJS=$(OBJDIR)/main.o


$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(ADKUTILLIB) -o $(LIB) >/dev/null 2>&1

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.c 
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/main.c 


 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

