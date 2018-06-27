SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)

IOUTILLIB=$(EXTLIBDIR)/libioutil.a

shlib: $(LIB)

#
# list of obj files....
#
OBJS=$(OBJDIR)/main.o \
     $(OBJDIR)/guicb.o \
     $(OBJDIR)/guidefs.o

EHEADER=
GHEADER=
GHEADER=GUIDEFS.h



#
# Main link command:
#
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(CPPLD) $(CPPLDFLAGS) *.o -o $(LIB) >/dev/null 2>&1

#
# Duplicate the next line for additional source files...
#
$(OBJDIR)/main.o : main.cpp $(EHEADER) $(GHEADER)
	cd $(OBJDIR); $(CPPCOMP) $(CPPFLAGS) $(SOURCEDIR)/main.cpp
$(OBJDIR)/guidefs.o : guidefs.cpp guibld.cpp $(GHEADER)
	cd $(OBJDIR); $(CPPCOMP) $(CPPFLAGS) $(SOURCEDIR)/guidefs.cpp
$(OBJDIR)/guicb.o : guicb.cpp guibld.cpp $(GHEADER)
	cd $(OBJDIR); $(CPPCOMP) $(CPPFLAGS) $(SOURCEDIR)/guicb.cpp

 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1

