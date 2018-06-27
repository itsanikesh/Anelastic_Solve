#ifndef ADDONVER_H_
#define ADDONVER_H_

/*
****************************************************************
****************** BEGIN DEVELOPMENT NOTES *********************
****************************************************************
D ****************************************************************
D *  Initial Release  1.0                                        *
D ****************************************************************





****************************************************************
****************** END DEVELOPMENT NOTES ***********************
****************************************************************
****************************************************************
*  V in column 1 marks date information.                       *
*  C in column 1 marks notes on new changes.                   *
*  B in column 1 marks notes on bug fixes.                     *
****************************************************************
*/

#define MinTecplotVersionAllowed 750000 /* Minimum tecplot version we require */

#define ADDON_NAME "XY Sum"
#define ADDON_VERSION "1.0"
#ifdef __DATE__
# define ADDON_DATE __DATE__
#else
#  ifdef DATE
#    define ADDON_DATE DATE
#  else
#    define ADDON_DATE ""
#  endif  /* DATE  */
#endif /* __DATE__ */

#endif /* ADDONVER_H_ */
