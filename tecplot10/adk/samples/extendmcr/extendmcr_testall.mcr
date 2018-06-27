#!MC 900
$!VarSet |MFBD| = 'C:\Tecplot\Tec904'

$!NEWLAYOUT
$!CREATERECTANGULARZONE 
  IMAX = 10
  JMAX = 10
  KMAX = 10
  X1 = -1
  Y1 = -1
  Z1 = -1
  X2 = 1
  Y2 = 1
  Z2 = 1

$!ALTERDATA 
  EQUATION = '{Temperature} = i*y + j*x' 
$!ALTERDATA 
  EQUATION = '{Pressure} = sin(x) + cos (y) ' 
$!BLANKING VALUE{INCLUDE = YES}
$!BLANKING VALUE{CONSTRAINT 1 {INCLUDE = YES}}
$!BLANKING VALUE{CONSTRAINT 1 {VARA = 5}}

$!GLOBALCONTOUR VAR = 4
$!FIELDLAYERS SHOWCONTOUR = YES

$!FIELDLAYERS SHOWMESH = NO
$!FIELDLAYERS SHOWSCATTER = YES
$!GLOBALSCATTER VAR = 2
$!FIELD [1]  SCATTER{SIZEBYVARIABLE = YES}
$!FIELD [1]  SCATTER{SYMBOLSHAPE{ISASCII = NO}}
$!FIELD [1]  SCATTER{SYMBOLSHAPE{GEOMSHAPE = DIAMOND}}
$!FIELD [1]  SCATTER{ISFILLED = YES}
$!FIELD [1]  SCATTER{FILLCOLOR = CUSTOM40}
$!GLOBALTHREED SYMBOLLIFTFRACTION = 1.6
$!ALTERDATA 
  EQUATION = '{x1} = -x*2' 
$!ALTERDATA 
  EQUATION = '{y1} = -y * 3' 
$!ALTERDATA 
  EQUATION = '{z1} = -z * 1.5' 
$!THREEDAXIS XVAR = 6
$!THREEDAXIS YVAR = 7
$!THREEDAXIS ZVAR = 8

$!GLOBALTHREEDVECTOR UVAR = 3
$!GLOBALTHREEDVECTOR VVAR = 2
$!GLOBALTHREEDVECTOR WVAR = 1
$!RESETVECTORLENGTH 
$!FIELDLAYERS SHOWVECTOR = YES
$!FIELD [1]  VECTOR{COLOR = CUSTOM22}
$!VIEW TRANSLATE
  X = -10
  Y = 0
$!VIEW TRANSLATE
  X = 0
  Y = -20
$!THREEDVIEW 
  PSIANGLE = 60
  THETAANGLE = -58.5296
  ALPHAANGLE = 0
  VIEWERPOSITION
    {
    X = 40.5939518369
    Y = -23.4141015267
    Z = 29.5559496308
    }
$!THREEDAXIS FRAMEAXIS{SHOW = NO}


$!SETDATASETTITLE "Extendmcr test zone"
$!VARSET |ZNUM| = "blank"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.DATASETTITLE ZNUM'

$!ATTACHTEXT 
  XYPOS
    {
    X = 5
    Y = 90
    }
  TEXT = "Title is: |ZNUM|" 

$!VARSET |VNAME| = "X"

$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNAMEBYNUM 2 VNAME' 

$!ATTACHTEXT 
  XYPOS
    {
    X = 5
    Y = 85
    }
  TEXT = "Var 2 is: |VNAME|" 

$!VARSET |ZNAME| = "HELLO"

$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.ZONENAMEBYNUM 1 ZNAME' 

$!ATTACHTEXT 
  XYPOS
    {
    X = 5
    Y = 80
    }
  TEXT = "Zone is:  |ZNAME|" 

$!VARSET |ZNUM1| = "x"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ZONENUMBYNAME "|ZNAME|" ZNUM1'

$!ATTACHTEXT 
  XYPOS
    {
    X = 5
    Y = 75
    }
  TEXT = "Zonenum is:  |ZNUM1|" 

$!VARSET |ZNUMS| = "blank"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ACTIVEZONES ZNUMS'

$!ATTACHTEXT 
  XYPOS
    {
    X = 5
    Y = 70
    }
  TEXT = "Active zones are: |ZNUMS|" 

$!VARSET |PORTION| = "blank"

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.SUBSTRING "|ZNUM|" 4 8 PORTION'

$!ATTACHTEXT 
  XYPOS
    {
    X = 35
    Y = 90
    }
  TEXT = "Substring 4 to 8 is: |PORTION|" 

$!VARSET |SLEN| = 0
$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.LENGTH "|ZNUM|" SLEN'

$!ATTACHTEXT 
  XYPOS
    {
    X = 35
    Y = 85
    }
  TEXT = "Title length is: |SLEN|" 

$!VARSET |PORTION1| = 0

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='STRING.FINDPATTERN "|ZNUM|" "|PORTION|" PORTION1'

$!ATTACHTEXT 
  XYPOS
    {
    X = 35
    Y = 80
    }
  TEXT = "After |PORTION| is:  |PORTION1|" 

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ISZONEACTIVE |ZNUM1| ISACT'

$!ATTACHTEXT 
  XYPOS
    {
    X = 35
    Y = 75
    }
  TEXT = "Is zone |ZNUM1| active?  |ISACT|" 

$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNUMBYASSIGNMENT C VNUM' 
$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNAMEBYNUM |VNUM| VNAME' 

$!ATTACHTEXT 
  XYPOS
    {
    X = 65
    Y = 90
    }
  TEXT = "Contour var is: |VNAME|" 

$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNUMBYASSIGNMENT B VNUM' 
$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNAMEBYNUM |VNUM| VNAME' 

$!ATTACHTEXT 
  XYPOS
    {
    X = 65
    Y = 85
    }
  TEXT = "Blanking var is: |VNAME|" 

$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNUMBYASSIGNMENT S VNUM' 
$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNAMEBYNUM |VNUM| VNAME' 

$!ATTACHTEXT 
  XYPOS
    {
    X = 65
    Y = 75
    }
  TEXT = "Scatter-sizing var is: |VNAME|" 

$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNUMBYASSIGNMENT x VNUM' 
$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNAMEBYNUM |VNUM| VNAME' 

$!ATTACHTEXT 
  XYPOS
    {
    X = 65
    Y = 70
    }
  TEXT = "Z-Axis var is: |VNAME|" 

$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNUMBYASSIGNMENT y VNUM' 
$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNAMEBYNUM |VNUM| VNAME' 

$!ATTACHTEXT 
  XYPOS
    {
    X = 65
    Y = 65
    }
  TEXT = "Y-axis var is: |VNAME|" 

$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNUMBYASSIGNMENT z VNUM' 
$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNAMEBYNUM |VNUM| VNAME' 

$!ATTACHTEXT 
  XYPOS
    {
    X = 65
    Y = 60
    }
  TEXT = "Z-axis var is: |VNAME|" 

$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNUMBYASSIGNMENT u VNUM' 
$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNAMEBYNUM |VNUM| VNAME' 

$!ATTACHTEXT 
  XYPOS
    {
    X = 65
    Y = 55
    }
  TEXT = "U vector var is: |VNAME|" 

$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNUMBYASSIGNMENT v VNUM' 
$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNAMEBYNUM |VNUM| VNAME' 

$!ATTACHTEXT 
  XYPOS
    {
    X = 65
    Y = 50
    }
  TEXT = "V vector var is: |VNAME|" 

$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNUMBYASSIGNMENT w VNUM' 
$!ADDONCOMMAND ADDONID = 'extendmcr' COMMAND='QUERY.VARNAMEBYNUM |VNUM| VNAME' 

$!ATTACHTEXT 
  XYPOS
    {
    X = 65
    Y = 45
    }
  TEXT = "W vector var is: |VNAME|" 

$!PLOTTYPE = XYLINE

$!VarSet |MAPNUM| = 3

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.MAPNAMEBYNUM |MAPNUM| MAPNAME'

$!ATTACHTEXT 
  XYPOS
    {
    X = 65
    Y = 30
    }
  TEXT = "Map 1 name  is:  |MAPNAME|" 


$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ISADDONLOADED "extendmcr" VVV'
$!ATTACHTEXT 
  XYPOS
    {
    X = 5
    Y = 45
    }
  TEXT = "ADDON extendmcr is loaded = |VVV|" 

$!ADDONCOMMAND ADDONID='extendmcr' COMMAND='QUERY.ISADDONLOADED "bananaaddon" VVV'
$!ATTACHTEXT 
  XYPOS
    {
    X = 5
    Y = 42 
    }
  TEXT = "ADDON bananaaddon is loaded = |VVV|" 



$!RemoveVar |MFBD|
