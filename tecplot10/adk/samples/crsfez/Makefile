SHELL=/bin/sh
LIB=$(TECADDONDEVDIR)/lib/$(PLATFORM)/$(OBJSUBDIR)/lib$(LIBBASENAME).$(LIBEXT)
STATLIB=$(ADDONLIBDIR)/lib$(LIBBASENAME).a
OBJDIR=$(OBJBUILDDIR)/obj/$(PLATFORM)/$(OBJSUBDIR)
EXTLIBDIR=$(EXTBASEDIR)/$(PLATFORM)
ADKUTILLIB=$(EXTLIBDIR)/libadkutil.a
IOUTILLIB=$(EXTLIBDIR)/libioutil.a
 
shlib: $(LIB)
statlib: $(STATLIB)
#
# list of obj files....
#
OBJS=$(OBJDIR)/crsfez.o \
     $(OBJDIR)/guidefs.o \
     $(OBJDIR)/guicb.o \
     $(OBJDIR)/list.o
 
#
# The next two dependencies probably can remain unchanged...
#
 
$(LIB) : $(OBJS)
	cd $(OBJDIR); $(LD) $(LDFLAGS) *.o $(ADKUTILLIB) -o $(LIB) >/dev/null 2>&1
 
$(STATLIB) : $(OBJS)
	cd $(OBJDIR); $(AR) q lib$(LIBBASENAME).a *.o; mv lib$(LIBBASENAME).a  $(STATLIB)
 
#
# Duplicate the next line for additional source files...
#
 
$(OBJDIR)/crsfez.o : crsfez.c GUIDEFS.h ADDONVER.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/crsfez.c
$(OBJDIR)/guidefs.o : guidefs.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/guidefs.c 
$(OBJDIR)/guicb.o : guicb.c guibld.c GUIDEFS.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/guicb.c 
$(OBJDIR)/list.o : list.c LIST.h
	cd $(OBJDIR); $(CCOMP) $(CFLAGS) $(SOURCEDIR)/list.c 
 
 
clean:
	rm obj/*/*/*.o > /dev/null 2>&1
