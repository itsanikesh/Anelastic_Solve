The extendmcr addon "extends" the tecplot macro language with 
the following commands:

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.MAPNAMEBYNUM nnn VVV'
  Get the string for mapping nnn and stuff into variable VVV


$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ZONENAMEBYNUM nnn VVV'
  Get the string for zone nnn and stuff into variable VVV


$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.VARNAMEBYNUM nnn VVV'
  Get the string for var nnn and stuff into variable VVV


$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ZONENUMBYNAME "zonename" VVV'
  Get the number of zone named zonename and stuff into variable VVV


$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.VARNUMBYASSIGNMENT assignment VVV'
  Get the number of var by assignment and stuff into variable VVV
  assignment can be one of x,y,z,u,v,w,c,s,b (See TecUtilVarGetNumByAssignment).


$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.DATASETTITLE VVV'
  Get the dataset title


$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.LENGTH StrSource VVV'
  Get the length of string StrSource.


$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.FINDPATTERN StrSource StrPattern VVV'
  Get the substring from StrSource that starts at pattern StrPattern and
  goes to the end of StrSource.  If not found returns "NOTFOUND".


$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.SUBSTRING StrSource start end VVV'
  Get the substring from StrSource that starts at position start and ends
  at position end.  Put the result in VVV.


$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ACTIVEZONES VVV'
  Get the set of active zones and put the result in VVV.  
  NOTE: Set string does not include []'s

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ISZONEACTIVE ZZZ VVV'
  Test to see if zone ZZZ is currently active.  If so, VVV is set
  to "YES" otherwise it is set to "NO"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.FILEEXISTS FILENAME VVV'
  Test to see if the file FILENAME exists.  If so, VVV is set to "YES" otherwise "NO"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ISADDONLOADED ADDONID VVV'
  Test to see if the addon ADDONID is loaded.  If so, VVV is set to "YES" otherwise "NO"

