/*
****************************************************************
****************** BEGIN DEVELOPMENT NOTES *********************
****************************************************************

D ****************************************************************
D *                 Build 1.0 9-04-98                            *
D ****************************************************************

B 12/04/2000 (STI)
B Fixed error in Makefile.

B 12/07/2000 (STI)
B Fixed error in matching of zone attributes when the master 
B (text or geometry) object is unattached to a zone. It was 
B picking all all objects that were either unattached to a 
B zone or attached to zone 1.  Added calls to 
B TecUtilTextIsAttached and TecUtilGeomIsAttached to detect
B objects unattached to a zone. Now picks other objects
B that are unattached to a zone when the master object is 
B unattached to a zone.

C 12/07/2000 (STI)
C Modified dialog label and Select button text to be more
C descriptive. Also Changed the tools menu name from advpick
C to Advanced Pick.



****************************************************************
****************** END DEVELOPMENT NOTES ***********************
****************************************************************
****************************************************************
*  D in column 1 marks date information.                       *
*  C in column 1 marks notes on new changes.                   *
*  B in column 1 marks notes on bug fixes.                     *
****************************************************************

*/

#define ADDON_NAME "AdvPick Sample"
#define ADDON_VERSION "1.1"
#ifdef __DATE__
# define ADDON_DATE __DATE__
#else
# define ADDON_DATE ""
#endif /* __DATE__ */
#define MinTecplotVersionAllowed 740400
