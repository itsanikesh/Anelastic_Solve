# Microsoft Developer Studio Project File - Name="crsfez" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=crsfez - Win32 IA64Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "crsfez.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "crsfez.mak" CFG="crsfez - Win32 IA64Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "crsfez - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "crsfez - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "crsfez - Win32 IA64Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "crsfez - Win32 IA64Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName "crsfez"
# PROP Scc_LocalPath "."
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "crsfez - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MD /GX /O2 /I "..\..\..\build\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 wingui.lib tecplot.lib adkutil.lib /nologo /base:"0x1ed50000" /subsystem:windows /dll /machine:I386 /libpath:"..\..\..\build/Release"
# SUBTRACT LINK32 /pdb:none
# Begin Custom Build - Updating Binary Files...
TargetDir=.\Release
TargetPath=.\Release\crsfez.dll
InputPath=.\Release\crsfez.dll
SOURCE="$(InputPath)"

"..\..\..\build\$(TargetPath)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	xcopy /d /q $(TargetPath) "..\..\..\build\$(TargetDir)" 
	touch "..\..\..\build\$(TargetPath)" 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "crsfez - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD F90 /browser
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MDd /Gm /GX /ZI /Od /I "..\..\..\build\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 wingui.lib tecplot.lib adkutil.lib /nologo /base:"0x1ed50000" /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept /libpath:"..\..\..\build/Debug"
# SUBTRACT LINK32 /pdb:none
# Begin Custom Build - Updating Binary Files...
TargetDir=.\Debug
TargetPath=.\Debug\crsfez.dll
InputPath=.\Debug\crsfez.dll
SOURCE="$(InputPath)"

"..\..\..\build\$(TargetPath)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	xcopy /d /q $(TargetPath) "..\..\..\build\$(TargetDir)" 
	touch "..\..\..\build\$(TargetPath)" 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "crsfez - Win32 IA64Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "crsfez___Win32_IA64Debug"
# PROP BASE Intermediate_Dir "crsfez___Win32_IA64Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "IA64Debug"
# PROP Intermediate_Dir "IA64Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /browser
# ADD F90 /browser
# ADD BASE CPP /nologo /MDd /Gm /GX /ZI /Od /I "..\..\..\build\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /FD /c
# ADD CPP /nologo /MDd /Gm /GX /Zi /Od /I "..\..\..\build\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 "..\..\..\build\debug\wingui.lib" "..\..\..\build\debug\libtec.lib" kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /base:"0x1ed50000" /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 wingui.lib tecplot.lib adkutil.lib /nologo /base:"0x1ed50000" /subsystem:windows /dll /debug /machine:IX86 /libpath:"..\..\..\build/IA64Debug" /machine:IA64
# SUBTRACT LINK32 /pdb:none
# Begin Custom Build - Updating Binary Files...
TargetDir=.\IA64Debug
TargetPath=.\IA64Debug\crsfez.dll
InputPath=.\IA64Debug\crsfez.dll
SOURCE="$(InputPath)"

"..\..\..\build\$(TargetPath)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	xcopy /d /q $(TargetPath) "..\..\..\build\$(TargetDir)" 
	touch "..\..\..\build\$(TargetPath)" 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "crsfez - Win32 IA64Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "crsfez___Win32_IA64Release"
# PROP BASE Intermediate_Dir "crsfez___Win32_IA64Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "IA64Release"
# PROP Intermediate_Dir "IA64Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /GX /O2 /I "..\..\..\build\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MD /GX /O2 /I "..\..\..\build\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 "..\..\..\build\release\wingui.lib" "..\..\..\build\release\libtec.lib" kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /base:"0x1ed50000" /subsystem:windows /dll /machine:I386
# ADD LINK32 wingui.lib tecplot.lib adkutil.lib /nologo /base:"0x1ed50000" /subsystem:windows /dll /machine:IX86 /libpath:"..\..\..\build/IA64Release" /machine:IA64
# SUBTRACT LINK32 /pdb:none
# Begin Custom Build - Updating Binary Files...
TargetDir=.\IA64Release
TargetPath=.\IA64Release\crsfez.dll
InputPath=.\IA64Release\crsfez.dll
SOURCE="$(InputPath)"

"..\..\..\build\$(TargetPath)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	xcopy /d /q $(TargetPath) "..\..\..\build\$(TargetDir)" 
	touch "..\..\..\build\$(TargetPath)" 
	
# End Custom Build

!ENDIF 

# Begin Target

# Name "crsfez - Win32 Release"
# Name "crsfez - Win32 Debug"
# Name "crsfez - Win32 IA64Debug"
# Name "crsfez - Win32 IA64Release"
# Begin Group "TGB Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\ADDGLBL.h
# End Source File
# Begin Source File

SOURCE=.\gui.lay
# End Source File
# Begin Source File

SOURCE=.\guibld.c

!IF  "$(CFG)" == "crsfez - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "crsfez - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "crsfez - Win32 IA64Debug"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "crsfez - Win32 IA64Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\guicb.c
# End Source File
# Begin Source File

SOURCE=.\guidefs.c
# End Source File
# Begin Source File

SOURCE=.\GUIDEFS.h
# End Source File
# End Group
# Begin Group "Generic Source Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\crsfez.c
# End Source File
# Begin Source File

SOURCE=.\list.c
# End Source File
# End Group
# Begin Group "Generic Header Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\CRSFEZ.h
# End Source File
# Begin Source File

SOURCE=.\LIST.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\crsfez.rc
# End Source File
# Begin Source File

SOURCE=.\resource.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\ADDONVER.h
# End Source File
# Begin Source File

SOURCE=.\prebuild.txt

!IF  "$(CFG)" == "crsfez - Win32 Release"

# PROP Ignore_Default_Tool 1
USERDEP__PREBU="ADDONVER.h"	
# Begin Custom Build - Creating version resource...
InputPath=.\prebuild.txt

"_VERSION.RC" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	makerc ADDONVER.h crsfez "..\..\..\build\Include\VERSION.H"

# End Custom Build

!ELSEIF  "$(CFG)" == "crsfez - Win32 Debug"

# PROP Ignore_Default_Tool 1
USERDEP__PREBU="ADDONVER.h"	
# Begin Custom Build - Creating version resource...
InputPath=.\prebuild.txt

"_VERSION.RC" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	makerc ADDONVER.h crsfez "..\..\..\build\Include\VERSION.H"

# End Custom Build

!ELSEIF  "$(CFG)" == "crsfez - Win32 IA64Debug"

# PROP BASE Ignore_Default_Tool 1
# PROP Ignore_Default_Tool 1
USERDEP__PREBU="ADDONVER.h"	
# Begin Custom Build - Creating version resource...
InputPath=.\prebuild.txt

"_VERSION.RC" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	makerc ADDONVER.h crsfez "..\..\..\build\Include\VERSION.H"

# End Custom Build

!ELSEIF  "$(CFG)" == "crsfez - Win32 IA64Release"

# PROP BASE Ignore_Default_Tool 1
# PROP Ignore_Default_Tool 1
USERDEP__PREBU="ADDONVER.h"	
# Begin Custom Build - Creating version resource...
InputPath=.\prebuild.txt

"_VERSION.RC" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	makerc ADDONVER.h crsfez "..\..\..\build\Include\VERSION.H"

# End Custom Build

!ENDIF 

# End Source File
# End Target
# End Project
