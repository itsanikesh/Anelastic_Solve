# Microsoft Developer Studio Project File - Name="exbmp" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=exbmp - Win32 IA64Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "exbmp.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "exbmp.mak" CFG="exbmp - Win32 IA64Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "exbmp - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "exbmp - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "exbmp - Win32 IA64Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "exbmp - Win32 IA64Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""$/Tecplot 10.0/Addons/Windows/Sample/exbmp", PHBCAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "exbmp - Win32 Release"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /include:"Release/"
# ADD F90 /include:"Release/"
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\..\..\build\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_USRDLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 wingui.lib tecplot.lib /nologo /base:"0x20fd0000" /subsystem:windows /dll /machine:I386 /libpath:"..\..\..\build/Release"
# SUBTRACT LINK32 /pdb:none
# Begin Custom Build - Updating Binary Files...
TargetDir=.\Release
TargetPath=.\Release\exbmp.dll
InputPath=.\Release\exbmp.dll
SOURCE="$(InputPath)"

"..\..\..\build\$(TargetPath)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	xcopy /d /q $(TargetPath) "..\..\..\build\$(TargetDir)" 
	touch "..\..\..\build\$(TargetPath)" 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "exbmp - Win32 Debug"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /include:"Debug/"
# ADD F90 /include:"Debug/"
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /YX /FD /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\build\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_USRDLL" /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 wingui.lib tecplot.lib /nologo /base:"0x20fd0000" /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"..\..\..\build/Debug"
# SUBTRACT LINK32 /pdb:none
# Begin Custom Build - Updating Binary Files...
TargetDir=.\Debug
TargetPath=.\Debug\exbmp.dll
InputPath=.\Debug\exbmp.dll
SOURCE="$(InputPath)"

"..\..\..\build\$(TargetPath)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	xcopy /d /q $(InputPath) "..\..\..\build\$(TargetDir)" 
	touch "..\..\..\build\$(TargetPath)" 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "exbmp - Win32 IA64Debug"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "exbmp___Win32_IA64Debug"
# PROP BASE Intermediate_Dir "exbmp___Win32_IA64Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "IA64Debug"
# PROP Intermediate_Dir "IA64Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /include:"Debug/"
# ADD F90 /include:"Debug/"
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\build\Include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_USRDLL" /YX /FD /c
# ADD CPP /nologo /MDd /Gm /GX /Zi /Od /I "..\..\..\build\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_USRDLL" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 "..\..\..\build\Debug\wingui.lib" "..\..\..\build\Debug\tecplot.lib" kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /base:"0x20fd0000" /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 wingui.lib tecplot.lib /nologo /base:"0x20fd0000" /subsystem:windows /dll /debug /machine:IX86 /libpath:"..\..\..\build/IA64Debug" /machine:IA64
# SUBTRACT LINK32 /pdb:none
# Begin Custom Build - Updating Binary Files...
TargetDir=.\IA64Debug
TargetPath=.\IA64Debug\exbmp.dll
InputPath=.\IA64Debug\exbmp.dll
SOURCE="$(InputPath)"

"..\..\..\build\$(TargetPath)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	xcopy /d /q $(InputPath) "..\..\..\build\$(TargetDir)" 
	touch "..\..\..\build\$(TargetPath)" 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "exbmp - Win32 IA64Release"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "exbmp___Win32_IA64Release"
# PROP BASE Intermediate_Dir "exbmp___Win32_IA64Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "IA64Release"
# PROP Intermediate_Dir "IA64Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /include:"Release/"
# ADD F90 /include:"Release/"
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /I "..\..\..\build\Include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_USRDLL" /YX /FD /c
# ADD CPP /nologo /MD /GX /O2 /I "..\..\..\build\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_USRDLL" /YX /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 "..\..\..\build\Release\wingui.lib" "..\..\..\build\Release\tecplot.lib" kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /base:"0x20fd0000" /subsystem:windows /dll /machine:I386
# ADD LINK32 wingui.lib tecplot.lib /nologo /base:"0x20fd0000" /subsystem:windows /dll /machine:IX86 /libpath:"..\..\..\build/IA64Release" /machine:IA64
# SUBTRACT LINK32 /pdb:none
# Begin Custom Build - Updating Binary Files...
TargetDir=.\IA64Release
TargetPath=.\IA64Release\exbmp.dll
InputPath=.\IA64Release\exbmp.dll
SOURCE="$(InputPath)"

"..\..\..\build\$(TargetPath)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	xcopy /d /q $(TargetPath) "..\..\..\build\$(TargetDir)" 
	touch "..\..\..\build\$(TargetPath)" 
	
# End Custom Build

!ENDIF 

# Begin Target

# Name "exbmp - Win32 Release"
# Name "exbmp - Win32 Debug"
# Name "exbmp - Win32 IA64Debug"
# Name "exbmp - Win32 IA64Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\doexport.c
# End Source File
# Begin Source File

SOURCE=.\exbmp.c
# End Source File
# Begin Source File

SOURCE=.\guibld.c
# PROP Exclude_From_Scan -1
# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\guicb.c
# End Source File
# Begin Source File

SOURCE=.\guidefs.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\ADDGLBL.h
# End Source File
# Begin Source File

SOURCE=.\EXBMP.h
# End Source File
# Begin Source File

SOURCE=.\GUIDEFS.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=.\ADDONVER.h
# End Source File
# Begin Source File

SOURCE=.\gui.lay
# End Source File
# Begin Source File

SOURCE=.\prebuild.txt

!IF  "$(CFG)" == "exbmp - Win32 Release"

# PROP Ignore_Default_Tool 1
# Begin Custom Build - Creating Version Resource...
InputPath=.\prebuild.txt

"_VERSION.RC" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	makerc ADDONVER.h exbmp "..\..\..\build\Include\VERSION.H"

# End Custom Build

!ELSEIF  "$(CFG)" == "exbmp - Win32 Debug"

# PROP Ignore_Default_Tool 1
# Begin Custom Build - Creating Version Resource...
InputPath=.\prebuild.txt

"_VERSION.RC" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	makerc ADDONVER.h exbmp "..\..\..\build\Include\VERSION.H"

# End Custom Build

!ELSEIF  "$(CFG)" == "exbmp - Win32 IA64Debug"

# PROP BASE Ignore_Default_Tool 1
# PROP Ignore_Default_Tool 1
# Begin Custom Build - Creating Version Resource...
InputPath=.\prebuild.txt

"_VERSION.RC" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	makerc ADDONVER.h exbmp "..\..\..\build\Include\VERSION.H"

# End Custom Build

!ELSEIF  "$(CFG)" == "exbmp - Win32 IA64Release"

# PROP BASE Ignore_Default_Tool 1
# PROP Ignore_Default_Tool 1
# Begin Custom Build - Creating Version Resource...
InputPath=.\prebuild.txt

"_VERSION.RC" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	makerc ADDONVER.h exbmp "..\..\..\build\Include\VERSION.H"

# End Custom Build

!ENDIF 

# End Source File
# End Target
# End Project
