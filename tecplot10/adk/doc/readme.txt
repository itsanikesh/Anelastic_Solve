Tecplot Version 10.0 Add-on Developers Kit README File        08/29/2003

---------------------------------------------
Notes About the Tecplot Add-on Developers Kit
---------------------------------------------

All of our example code was created and tested with Developer Studio 5.
If you are not using Developer Studio 5 or later, you will have to make
changes to the example code for your specific developer environment.

Other files in this directory are PDF files (Acrobat format) which you
can print out or view for documentation. These manuals are occasionally
updated and are available for download from the support section of
www.tecplot.com. The files are:

   adkum.pdf  -  Tecplot ADK Users Manual (Acrobat format)
   adkgr.pdf  -  Tecplot ADK Getting Results (Acrobat format)
   adkrm      -  Sub-directory containing the Tecplot ADK Reference 
                 Manual (html format).

These manuals are occasionally updated and are available for download 
from the support section of www.tecplot.com. To see if the on-line 
manual is newer, compare the dates on the title pages.

---------------------------------------------------------------------
Notes Specific to the Tecplot Add-on Developers Kit ActiveX Component
---------------------------------------------------------------------

The following routines have not yet been implemented for the
Tecplot Add-on Developers Kit ActiveX Component:

TecUtilDataValueGetRawPtr
TecUtilDataNodeGetRawPtr
TecUtilReadBinaryData

---------------------------------------------------------------------

The following routines may not be implemented at all for the
Tecplot Add-on Developers Kit ActiveX Component:

TecUtilInterfaceGetMotifHandles
TecUtilStringAlloc
TecUtilStringDealloc

It is not necessary to call TecUtilStringDealloc on any string
returned from the ADK when using the ActiveX Component.

---------------------------------------------------------------------

Error messages

When Tecplot has a problem with an ActiveX add-on, it attempts
to provide an error message which will help to determine the
cause of the failure.  Some of the error messages are returned
from non-Tecplot sources and can be somewhat cryptic.

- "Wrong number of arguments or invalid property assignment" means that
  there is currently a different version of the LibTecAX DLL in the
  registry than that which was there when the add-on was created
  (compiled).  We will endeavor to make sure that this error message
  never appears.  However, given that this is a "beta" release version,
  we reserve the right to change the LibTecAX DLL.  Once the add-on
  developers kit is released, the LibTecAX DLL will be frozen, and
  will conform to ActiveX versioning standards.

- "A device attached to the system is not functioning" means that
  the files that are required for the operation are not in sync.
  In other words, the ActiveX add-on, and the TECPLOT.EXE, LIBTEC.DLL,
  LIBTEC.LIB, LIBTECAX.DLL, and/or LIBTECAX.TLB files are not all
  running the same version.  If you get this message, you may need
  to reinstall the latest version of the ADK and remake your add-on.

- "Run-time error '91' Object variable or With block variable not set"
  comes from a Visual Basic add-on and means that you have not
  initialized the Tecplot application variable before using it.
  For example, the following line declares 'tec' as a Tecplot.Application
  variable:

    Private tec as Tecplot.Application

  And the following line is required before accessing any TecUtil functions
  from 'tec':

    Set tec = new Tecplot.Application

- Error code 0x0000007e, "Module not found", can mean that MSVBVM50.DLL
  is not found on the system.  (Visual Basic add-ons only.)

- "First-chance exception in tecplot.exe (KERNEL32.DLL): 0xC000008F:
  Float Inexact Result."  can mean any number of things.  One possibility
  is that a Visual Basic add-on is using a variable which has not been
  declared as the Tecplot Add-on Developers Kit ActiveX object.  For
  example, declare a variable "tec" as the Tecplot.Application object:

    Public tec as New Tecplot.Application

  And use a variable called "mytec" by accident:
  
    mytec.TecUtilRedraw True

  To avoid this problem, always use the "Option Explicit" statement at the
  beginning of your module.

---------------------------------------------------------------------
