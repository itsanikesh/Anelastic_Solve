# Microsoft Developer Studio Project File - Name="LoadSS" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=LoadSS - Win32 IA64Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "LoadSS.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "LoadSS.mak" CFG="LoadSS - Win32 IA64Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "LoadSS - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "LoadSS - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "LoadSS - Win32 IA64Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "LoadSS - Win32 IA64Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""$/Tecplot 10.0/Addons/Windows/CoreProduct/LoadSS", EOACAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "LoadSS - Win32 Release"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "LoadSSW0"
# PROP BASE Intermediate_Dir "LoadSSW0"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE CPP /nologo /MD /W3 /GX /Od /Ob2 /Gf /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /D "_WINDLL" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /Ob2 /I "..\..\..\build\Include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /FD /c
# SUBTRACT CPP /YX /Yc /Yu
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /dll /machine:IX86
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 wingui.lib tecplot.lib /nologo /base:"0x1f950000" /subsystem:windows /dll /machine:IX86 /libpath:"..\..\..\build/Release"
# SUBTRACT LINK32 /pdb:none
# Begin Custom Build - Updating Binary Files...
TargetDir=.\Release
TargetPath=.\Release\LoadSS.dll
InputPath=.\Release\LoadSS.dll
SOURCE="$(InputPath)"

"..\..\..\build\$(TargetPath)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	xcopy /d /q $(TargetPath) "..\..\..\build\$(TargetDir)" 
	touch "..\..\..\build\$(TargetPath)" 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "LoadSS - Win32 Debug"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "LoadSSWi"
# PROP BASE Intermediate_Dir "LoadSSWi"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /D "_WINDLL" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\build\Include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /FR /FD /c
# SUBTRACT CPP /YX /Yc /Yu
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /fo"Debug/LoadSS.res" /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 libtec.lib /nologo /subsystem:windows /dll /debug /machine:IX86 /pdbtype:sept
# SUBTRACT BASE LINK32 /pdb:none /incremental:no
# ADD LINK32 wingui.lib tecplot.lib /nologo /base:"0x1f950000" /subsystem:windows /dll /debug /machine:IX86 /pdbtype:sept /libpath:"..\..\..\build/Debug"
# SUBTRACT LINK32 /pdb:none
# Begin Custom Build - Updating Binary Files...
TargetDir=.\Debug
TargetPath=.\Debug\LoadSS.dll
InputPath=.\Debug\LoadSS.dll
SOURCE="$(InputPath)"

"..\..\..\build\$(TargetPath)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	xcopy /d /q $(TargetPath) "..\..\..\build\$(TargetDir)" 
	touch "..\..\..\build\$(TargetPath)" 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "LoadSS - Win32 IA64Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "LoadSS___Win32_IA64Debug"
# PROP BASE Intermediate_Dir "LoadSS___Win32_IA64Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "IA64Debug"
# PROP Intermediate_Dir "IA64Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\build\Include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /FR /FD /c
# SUBTRACT BASE CPP /YX /Yc /Yu
# ADD CPP /nologo /MDd /Gm /GX /Zi /Od /I "..\..\..\build\Include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /FR /FD /c
# SUBTRACT CPP /YX /Yc /Yu
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 "..\..\..\build\Debug\wingui.lib" "..\..\..\build\Debug\libtec.lib" kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /base:"0x1f950000" /subsystem:windows /dll /debug /machine:IX86 /pdbtype:sept
# SUBTRACT BASE LINK32 /pdb:none /incremental:no
# ADD LINK32 wingui.lib tecplot.lib /nologo /base:"0x1f950000" /subsystem:windows /dll /debug /machine:IX86 /libpath:"..\..\..\build/IA64Debug" /machine:IA64
# SUBTRACT LINK32 /pdb:none
# Begin Custom Build - Updating Binary Files...
TargetDir=.\IA64Debug
TargetPath=.\IA64Debug\LoadSS.dll
InputPath=.\IA64Debug\LoadSS.dll
SOURCE="$(InputPath)"

"..\..\..\build\$(TargetPath)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	xcopy /d /q $(TargetPath) "..\..\..\build\$(TargetDir)" 
	touch "..\..\..\build\$(TargetPath)" 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "LoadSS - Win32 IA64Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "LoadSS___Win32_IA64Release"
# PROP BASE Intermediate_Dir "LoadSS___Win32_IA64Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "IA64Release"
# PROP Intermediate_Dir "IA64Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /Ob2 /I "..\..\..\build\Include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /FD /c
# SUBTRACT BASE CPP /YX /Yc /Yu
# ADD CPP /nologo /MD /GX /O2 /Ob2 /I "..\..\..\build\Include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /FD /c
# SUBTRACT CPP /Fr /YX /Yc /Yu
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 "..\..\..\build\Release\wingui.lib" "..\..\..\build\Release\libtec.lib" kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /base:"0x1f950000" /subsystem:windows /dll /machine:IX86 /libpath:"..\..\..\build\Include"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 wingui.lib tecplot.lib /nologo /base:"0x1f950000" /subsystem:windows /dll /machine:IX86 /libpath:"..\..\..\build/IA64Release" /machine:IA64
# SUBTRACT LINK32 /pdb:none
# Begin Custom Build - Updating Binary Files...
TargetDir=.\IA64Release
TargetPath=.\IA64Release\LoadSS.dll
InputPath=.\IA64Release\LoadSS.dll
SOURCE="$(InputPath)"

"..\..\..\build\$(TargetPath)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	xcopy /d /q $(TargetPath) "..\..\..\build\$(TargetDir)" 
	touch "..\..\..\build\$(TargetPath)" 
	
# End Custom Build

!ENDIF 

# Begin Target

# Name "LoadSS - Win32 Release"
# Name "LoadSS - Win32 Debug"
# Name "LoadSS - Win32 IA64Debug"
# Name "LoadSS - Win32 IA64Release"
# Begin Group "Generic Source Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\loadss.c
# End Source File
# End Group
# Begin Group "Generic Header Files"

# PROP Default_Filter ""
# End Group
# Begin Group "TGB Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\ADDGLBL.h
# End Source File
# Begin Source File

SOURCE=.\dialog.lay
# End Source File
# Begin Source File

SOURCE=.\guibld.c

!IF  "$(CFG)" == "LoadSS - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "LoadSS - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "LoadSS - Win32 IA64Debug"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "LoadSS - Win32 IA64Release"

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
# Begin Group "Resource Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\LoadSS.rc
# End Source File
# Begin Source File

SOURCE=.\Resource.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\ADDONVER.h
# End Source File
# Begin Source File

SOURCE=.\prebuild.txt

!IF  "$(CFG)" == "LoadSS - Win32 Release"

USERDEP__PREBU="loadss.c"	
# Begin Custom Build - Creating version resource...
InputPath=.\prebuild.txt

"_VERSION.RC" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	makerc loadss.c loadss ..\..\..\build\INCLUDE\VERSION.H

# End Custom Build

!ELSEIF  "$(CFG)" == "LoadSS - Win32 Debug"

USERDEP__PREBU="ADDONVER.h"	
# Begin Custom Build - Creating version resource...
InputPath=.\prebuild.txt

"_VERSION.RC" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	makerc ADDONVER.h loadss "..\..\..\build\INCLUDE\VERSION.H"

# End Custom Build

!ELSEIF  "$(CFG)" == "LoadSS - Win32 IA64Debug"

USERDEP__PREBU="ADDONVER.h"	
# Begin Custom Build - Creating version resource...
InputPath=.\prebuild.txt

"_VERSION.RC" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	makerc ADDONVER.h loadss "..\..\..\build\INCLUDE\VERSION.H"

# End Custom Build

!ELSEIF  "$(CFG)" == "LoadSS - Win32 IA64Release"

USERDEP__PREBU="loadss.c"	
# Begin Custom Build - Creating version resource...
InputPath=.\prebuild.txt

"_VERSION.RC" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	makerc loadss.c loadss ..\..\..\build\INCLUDE\VERSION.H

# End Custom Build

!ENDIF 

# End Source File
# End Target
# End Project
