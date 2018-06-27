This directory contains example source code for
writing out Tecplot binary data files directly
from your application.

In order to write out Tecplot binary data files
directly, you will need to link your source code
with the tecio library included in the Tecplot
distribution (tecio.a for UNIX and tecio.lib
for Windows).  You may also need to reference
the tecio include file (tecio.h) which defines
the tecio routines.

There are currently 4 example files in this directory:

  simtest.c   Simple C/C++ test file of tecio.dll
  simtest.f   Simple Fortran test file of tecio.dll
  comtest.c   Complex C/C++ test file of tecio.dll
  comtest.f   Complex Fortran test file of tecio.dll


To make the test examples do the following:

UNIX:

   1.  Make sure the tecio.a library
       exists.  It should be located in 
       the lib directory below the Tecplot
       home directory.

   2.  Set your TEC80HOME environment variable
       to the Tecplot home directory.

   3.  Run Make.  (Capital M)

Windows:

   1. In Developer Studio, create a new project of type
      "Win32 Console Application".

   2. Add either "simtest.c" or "comtest.c" (but not both) to
      the project.

   3. Be sure that the Tec80\Include directory is in the compiler
      include file path (so the compiler will be able to find TECIO.h).

   4. Add "tecio.lib" to the list of libraries to link with (located
      in the Tec80\bin directory).

   5. Press F7 (or click the "Build" button) to build the example.


Notes for Windows Programmers using Fortran:

We have not included extensive support for Fortran compilers to
use the tecio library under Windows.  Any Fortran compiler should
be able to link with the tecio.lib file.  We have included a sample
Fortran include file (tecio.for) which Fortran users may need to
convert into an include file that is compatible with their specific
Fortran compiler.  (The tecio functions use "stdcall" calling
convention and require all parameters to be passed by reference.
Fortran strings must be NULL terminated and passed without a
length argument.)

Notes for Windows Programmers using Visual Basic:

We have not included extensive support for Visual Basic programmers
to use the tecio library under Windows.  Visual Basic should be able
to access the tecio.dll library as it would any other Windows DLL as
specified in the Visual Basic 5.0 documentation : Visual Basic
Component Tools Guide, Part 4, "Accessing DLLs and the Windows API".

If you are interested in an ActiveX enabled version of the tecio
library, please make a suggestion to our technical support group
at support@amtec.com.


