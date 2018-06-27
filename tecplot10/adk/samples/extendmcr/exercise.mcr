#!MC 900
$!NEWLAYOUT 
$!PICK SETMOUSEMODE
  MOUSEMODE = SELECT
$!CREATERECTANGULARZONE 
  IMAX = 10
  JMAX = 10
  KMAX = 1
  X1 = 0
  Y1 = 0
  Z1 = 0
  X2 = 1
  Y2 = 1
  Z2 = 1
$!PICK SETMOUSEMODE
  MOUSEMODE = SELECT
$!CREATECIRCULARZONE 
  IMAX = 10
  JMAX = 25
  KMAX = 1
  X = 0.5
  Y = 0.5
  Z1 = 0
  Z2 = 0
  RADIUS = 0.707106781187
$!PICK SETMOUSEMODE
  MOUSEMODE = SELECT

$!addoncommand addonid='extendmcr' command="QUERY.DATASETTITLE vvv"
$!pause "Dataset Title = |vvv|"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ZONENAMEBYNUM 2 ZZZ'
$!PAUSE "name of zone 2 is: |ZZZ|"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ZONENUMBYNAME "|ZZZ|" VVV2'
$!PAUSE "number of zone |ZZZ| is: |VVV2|"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.VARNUMBYASSIGNMENT y VVV2'
$!PAUSE "Var num for y is: |VVV2|"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.VARNAMEBYNUM 2 VVV'
$!PAUSE "Name of variable 2 is: |VVV|"

#
# If zone two is named "circular zone" this will work.....
#

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.FINDPATTERN "|ZZZ|" "cular" IVVV'
$!PAUSE "strstr of |ZZZ| with cular is: |IVVV|"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.FINDPATTERN "|ZZZ|" "cccc" IVVV'
$!PAUSE "strstr of |ZZZ| with cccc is: |IVVV|"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.LENGTH "|IVVV|" IVVV2'
$!PAUSE "String length of |IVVV| is |IVVV2|"

# $!ADDONCOMMAND ADDONID='extendmcr' COMMAND='SUBSTR "|VVV|" 2 4 IVVV'
# $!PAUSE "|IVVV|"

#
# Turn on all zones with "Apple" in the zone name.
#

#
# First see if there is at least one....
#

#
#
#
$!Pause "Next section will attempt to turn on only the zones with Circ in the name"

$!VarSet |ZonesToActivate| = "["
$!Loop |NumZones|
$!ADDONCOMMAND ADDONID='extendmcr' COMMAND="QUERY.ZONENAMEBYNUM |Loop| VVV"
$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.FINDPATTERN "|VVV|" "Circ" CIRCSTRING'
$!If "|CIRCSTRING|" != "NOTFOUND"
  $!If "|ZonesToActivate|" != "["
    $!VarSet |ZonesToActivate| = "|ZonesToActivate|,|Loop|"
  $!Endif
  $!If "|ZonesToActivate|" == "["
    $!VarSet |ZonesToActivate| = "[|Loop|"
  $!Endif
$!ENDIF
$!EndLoop

#
# Only do the activation if any zones with "apple" were found.
#
$!If "|ZonesToActivate|" != "["
  $!VarSet |ZonesToActivate| = "|ZonesToActivate|]"
  $!ActiveFieldZones = |ZonesToActivate|
$!Endif


$!ADDONCOMMAND ADDONID='extendmcr' COMMAND="QUERY.ACTIVEZONES AAA"
$!Pause "active zones = |AAA|"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND="QUERY.ISZONEACTIVE 1 BBB"
$!Pause "is zone 1 active: |BBB|"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND="QUERY.ISZONEACTIVE 3 BBB"
$!Pause "is zone 3 active: |BBB|"
